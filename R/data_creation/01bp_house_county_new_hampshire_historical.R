## New Hampshire, pre-2012 U.S. House by county, from the NH Secretary of State's own archived
## results (sos.nh.gov). Complements 01an (OpenElections, 2012/2014 only; OE's 2000 dir has
## primaries only).
##
## SOURCE (found by re-locating the prior fork's lead): the old sos.nh.gov site had one page per
## year, `http://sos.nh.gov/<YEAR>GenElectResults.aspx` -> `<YEAR>ConGen.aspx` ("Representative
## In Congress"), which offers per-district TOWN-level results as PDF and (2000, 2006, 2010) as
## Excel. sos.nh.gov itself now 403/503s automated requests, so everything here was fetched
## through the Wayback Machine (`web.archive.org/web/2016id_/...DownloadAsset.aspx?id=N`).
## The results are TOWN-level (NH reports by town/ward), NOT the "one page with all counties"
## the earlier fork described (that was probably a misread of the two per-district town tables);
## rolled up to county here with the town->county crosswalk from 01an plus an explicit
## unincorporated-places table (below).
##
## AVAILABILITY (Wayback captured only some assets; the rest are a genuine gap, not a bug):
##   2000  Excel (id 3178, both districts)   -> USED
##   2006  Excel (id 2221, both districts)   -> USED
##   2010  Excel (ids 555/556, one per dist) -> USED
##   1996  scanned image-only PDFs (ids 37807/37808) -> USED, hand-transcribed (see below)
##   1998, 2002, 2004, 2008: the district-2 file was never archived (DownloadAsset ids 3376,
##         2861, 2611, 1792/1794 all return Wayback's "not archived" page; live site blocked).
##         Only the district-1 PDF (embedded iframe asset) survives for 1998/2002/2008 -- NOT
##         used: a county roll-up from one district's towns would silently drop the CD2 share of
##         every split county (Rockingham, Hillsborough, Belknap, ...), so those years stay a gap.
##   1990-1994: the site has no House results before 1996.
##
## Each district sheet prints a "Totals" row; the parser checks its own column sums against it
## for every sheet and stops if they differ.
##
## Party: candidate headers carry the party suffix (", r" / ", d" / ", lib" / ", ind" ...), so
## no Wikipedia lookup was needed. Scatter (write-ins) and third parties fall to OTHER, i.e.
## into totalvote only, consistent with the rest of the House-county build.

source(file.path("R", "00_setup.R"))
library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "new_hampshire")
ASSET_DIR <- file.path(RAW_DIR, "assets")

county_fips_crosswalk <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))
nh_fips <- county_fips_crosswalk %>% filter(state == "NEW HAMPSHIRE") %>% select(county_name, county_fips)

## ---- town -> county ----
norm_key <- function(x) {
  x <- toupper(x)
  x <- str_replace_all(x, "\\*", "")                      # footnote asterisks ("Derry*", "Landaff*")
  x <- str_replace_all(x, "\\s+(WARD|WD)\\s*\\d+\\s*$", "") # "Dover Wd1", "Berlin ward 4", "Laconia Ward 2"
  x <- str_replace_all(x, "[^A-Z0-9&]", "")               # drop spaces/apostrophes/periods
  x
}
nh_town_county <- read_csv(file.path(RAW_DIR, "nh_town_county_crosswalk.csv"), show_col_types = FALSE) %>%
  mutate(key = norm_key(town), county = toupper(trimws(county))) %>% select(key, county)

## Unincorporated places/grants/purchases/locations missing from the OpenElections crosswalk
## (which only carries the spellings that appear in its own 2012+ files). All are Coos County
## except Hale's Location and Hart's Location (Carroll). Spelling variants across years
## ("Bean'sPurchase", "Sargents Purchase", "Thompson & Meserve's Pur.") are absorbed by norm_key
## plus the extra keys below.
unincorporated <- tribble(
  ~town, ~county,
  "Hale's Location", "CARROLL", "Hart's Location", "CARROLL", "Kilkenny", "COOS",
  "Atkinson & Gilmanton Academy Grant", "COOS", "Bean's Grant", "COOS", "Bean's Purchase", "COOS",
  "Chandler's Purchase", "COOS", "Crawford's Purchase", "COOS", "Cutt's Grant", "COOS",
  "Dix's Grant", "COOS", "Erving's Location", "COOS", "Green's Grant", "COOS",
  "Hadley's Purchase", "COOS", "Low & Burbank's Grant", "COOS", "Martin's Location", "COOS",
  "Pinkham's Grant", "COOS", "Sargent's Purchase", "COOS", "Thompson & Meserve's Purchase", "COOS",
  "Thompson & Meserve's Pur.", "COOS", "Wentworth's Location", "COOS"
) %>% mutate(key = norm_key(town)) %>% select(key, county)
town_lookup <- bind_rows(nh_town_county, unincorporated) %>% distinct(key, .keep_all = TRUE)

to_party <- function(cand) {
  suffix <- tolower(str_trim(str_extract(cand, "(?<=,)\\s*[A-Za-z]+\\s*$")))
  case_when(suffix == "r" ~ "REP", suffix == "d" ~ "DEM", TRUE ~ "OTHER")
}

## ---- Excel parser: one district sheet -> tidy town x candidate votes, self-checked ----
parse_sheet <- function(f, sheet) {
  x <- read_excel(f, sheet = sheet, col_names = FALSE, col_types = "text", .name_repair = "minimal")
  names(x) <- paste0("V", seq_len(ncol(x)))
  hdr <- which(apply(x, 1, function(r) any(grepl(",\\s*(r|d|l|lib|ind|ci|i|u|c)\\s*$", r, ignore.case = TRUE))))[1]
  h <- as.character(unlist(x[hdr, ]))
  tot <- which(grepl("^totals?$", trimws(x$V1), ignore.case = TRUE))[1]
  stopifnot(!is.na(hdr), !is.na(tot))
  cols <- which(!is.na(h) & seq_along(h) > 1)
  body <- x[(hdr + 1):(tot - 1), ]
  ## Drop page-break repeat rows (2006 sheets embed "39028" / "Page 2 of 3" rows) -- a real town
  ## row has a non-numeric name in V1 and numeric-or-blank vote cells.
  body <- body %>% filter(!is.na(V1), !grepl("^[0-9.]+$", trimws(V1)), !grepl("^Page \\d+ of \\d+$", trimws(V1)))
  ## "--"/"-" = zero votes (2006 prints this for unincorporated places with no voters)
  body <- body %>% mutate(across(all_of(paste0("V", cols)), ~ if_else(grepl("^\\s*[-\u2013\u2014]+\\s*$", .x), "0", .x)))
  votes_raw <- body[, paste0("V", cols), drop = FALSE]
  is_bad <- apply(votes_raw, 1, function(r) any(!is.na(r) & is.na(suppressWarnings(as.numeric(gsub(",", "", r))))))
  body <- body[!is_bad, ]    # repeated header/label rows inside the body
  long <- body %>%
    pivot_longer(all_of(paste0("V", cols)), names_to = "col", values_to = "v") %>%
    mutate(cand = h[as.integer(sub("V", "", col))],
           votes = coalesce(suppressWarnings(as.numeric(gsub(",", "", v))), 0)) %>%
    transmute(town_raw = trimws(V1), cand, votes)
  ## self-check against the sheet's own printed Totals row
  printed <- setNames(suppressWarnings(as.numeric(gsub(",", "", unlist(x[tot, cols])))), h[cols])
  mine <- long %>% group_by(cand) %>% summarise(s = sum(votes), .groups = "drop")
  chk <- mine %>% mutate(printed = printed[cand]) %>% filter(!is.na(printed))
  if (any(chk$s != chk$printed)) {
    print(chk); stop("Column sums do not match printed Totals row: ", f, " / ", sheet)
  }
  message(sprintf("  %s [%s]: %d rows, %d candidates, column sums == printed Totals",
                  basename(f), trimws(sheet), n_distinct(long$town_raw), nrow(chk)))
  long
}

specs <- tribble(
  ~year, ~district, ~file,             ~sheet,
  2000,  1, "2000_cd12.xls", "congress1",
  2000,  2, "2000_cd12.xls", "congress2",
  2006,  1, "2006_cd12.xls", "rcongress1",
  2006,  2, "2006_cd12.xls", "rcongress2",
  2010,  1, "2010_cd1.xls",  "congress1",
  2010,  2, "2010_cd2.xls",  " congress2"     # sheet name really has a leading space
)

message("Parsing NH SOS Excel files:")
town_votes <- pmap_dfr(specs, function(year, district, file, sheet) {
  parse_sheet(file.path(ASSET_DIR, file), sheet) %>% mutate(year = year, district = district)
})

## 1996: the two district PDFs are fax-quality image scans (pdftotext returns nothing). OCR
## (tesseract, even after grid-line removal / cell-by-cell) was ~5% wrong on digits, so the
## 7 pages (assets/1996_cd1.pdf, 1996_cd2.pdf; 300dpi renders in ocr1996/) were transcribed by
## reading the rendered images directly -> nh_1996_town_votes.csv (115 CD1 + 207 CD2 town rows),
## then VERIFIED: every candidate column's sum equals the sheet's own printed TOTALS row
## (CD1 123,939/115,462/8,176/159; CD2 123,001/105,867/3,727/10,757/235), exactly.
## Kendel's party label is printed "i.a." (independent alliance) and Lamirande "i" -> both OTHER.
f1996 <- file.path(RAW_DIR, "nh_1996_town_votes.csv")
t96 <- read_csv(f1996, show_col_types = FALSE)
chk96 <- t96 %>% group_by(district, cand) %>% summarise(s = sum(votes), .groups = "drop")
printed96 <- tribble(~district, ~cand, ~printed,
  1, "Sununu, r", 123939, 1, "Keefe, d", 115462, 1, "Flanders, l", 8176, 1, "Scatter", 159,
  2, "Bass, r", 123001, 2, "Arnesen, d", 105867, 2, "Kendel, ia", 3727, 2, "Lamirande, i", 10757, 2, "Scatter", 235)
stopifnot(nrow(chk96) == nrow(printed96), all(inner_join(chk96, printed96, by = c("district", "cand"))$s ==
                                                inner_join(chk96, printed96, by = c("district", "cand"))$printed))
message("  1996 transcription: column sums == printed TOTALS rows (both districts)")
town_votes <- bind_rows(town_votes, t96 %>% transmute(town_raw, cand, votes, year, district))

## ---- town -> county, with an explicit no-unmatched check ----
town_votes <- town_votes %>% mutate(key = norm_key(town_raw)) %>% left_join(town_lookup, by = "key")
if (any(is.na(town_votes$county))) {
  print(town_votes %>% filter(is.na(county)) %>% distinct(year, town_raw))
  stop("Unmatched towns -- extend `unincorporated` / crosswalk")
}
town_votes <- town_votes %>% mutate(party = to_party(cand))

nh_by_county <- town_votes %>%
  group_by(year, county) %>%
  summarise(totalvote = sum(votes),
            demovote_n = sum(votes[party == "DEM"]),
            repuvote_n = sum(votes[party == "REP"]), .groups = "drop")

elect_he_cty_nh_historical <- nh_by_county %>%
  left_join(nh_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(state = "NEW HAMPSHIRE", year, cty_fips = county_fips, sample = "HE",
            demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote) %>%
  arrange(year, cty_fips) %>%
  save_step("elect_he_cty_nh_historical")

message("NH historical House county rows: ", nrow(elect_he_cty_nh_historical))
print(table(elect_he_cty_nh_historical$year))
stopifnot(all(table(elect_he_cty_nh_historical$year) == 10))

sanity <- elect_he_cty_nh_historical$repuvote + elect_he_cty_nh_historical$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")

## Statewide two-party reality check: weighted D/R shares per year should track NH's known
## delegation history (2000 R holds both seats; 2006 D sweeps both; 2010 R sweeps both).
print(elect_he_cty_nh_historical %>% group_by(year) %>%
        summarise(state_dem = weighted.mean(demovote, totalvote),
                  state_rep = weighted.mean(repuvote, totalvote), total = sum(totalvote)))
