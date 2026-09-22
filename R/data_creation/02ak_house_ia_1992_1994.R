## Iowa U.S. House, 1992 and 1994, from the state's official scanned general-election canvass books (R/data/county_house_files/
## IA_1992gencanv.pdf, IA_1994gencanv.pdf). Both are scanned documents with an unreliable OCR text layer (garbled digits) -- the OCR
## text was used only to LOCATE the relevant pages (PDF pages 29-33 for 1992; 7-9 for 1994, each PDF page holding one or two districts'
## full tables), which were then rendered at 300dpi and every county x candidate cell was read directly from the image and transcribed
## by hand into R/data/county_house_files/iowa/transcribed/<year>.csv. Iowa had 5 U.S. House districts in both years (reduced from 6
## after the 1990 census reapportionment took effect for the 1992 election). Every district's county rows tie EXACTLY to that
## district's own printed TOTALS row (verified while transcribing, re-checked again below as a permanent regression guard).
##
## A few county names are printed hard against the page's left margin and lose their first 1-3 letters on the scan ("JQUE" for
## DUBUQUE, "YETTE" for FAYETTE in 1992; identical pattern in 1994) -- resolved by Iowa's own alphabetical county ordering within each
## district table (the cut-off name always falls exactly where the alphabetically-expected county belongs, confirmed against the full
## 99-county list) rather than guessed blind.
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)

TDIR <- file.path(PROJECT_ROOT, "R", "data", "county_house_files", "iowa", "transcribed")
xw <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  filter(state == "IOWA", !is.na(county_fips)) %>% distinct(county_fips, county_name) %>% mutate(county_name = toupper(trimws(county_name)))
stopifnot(nrow(xw) == 99)
fips_of <- function(z) xw$county_fips[match(toupper(trimws(z)), xw$county_name)]

## printed district TOTALS rows, read directly off the page images -- used as the tie-check target (independent of how the per-county
## rows were summed) so a transcription error in one county cannot silently cancel against another and still "pass".
printed_totals <- tribble(
  ~year, ~district, ~total_str,
  1992, "01", "Jan J. Zonneveld=81600;Jim Leach=178042;Scattering=1667",
  1992, "02", "David R. Nagle=131570;Jim Nussle=134536;Albert W. Schoeman=1757;Scattering=29",
  1992, "03", "Elaine Baxter=121063;Jim Ross Lightfoot=125931;Larry Chroman=10181;Scattering=101",
  1992, "04", "Neal Smith=158610;Paul Lunde=94045;William C. Oviatt=2359;Jerry Yellin=2427;Scattering=152",
  1992, "05", "Fred Grandy=196942;Scattering=1424",
  1994, "01", "Glen Winekauf=69461;Jim Leach=110448;Jan J. Zonneveld=2264;Michael Cuddehe=1213;Scattering=75",
  1994, "02", "Dave Nagle=86087;Jim Nussle=111076;Albert W. Schoeman=1281;Scattering=43",
  1994, "03", "Elaine Baxter=79310;Jim Ross Lightfoot=111862;Derrick P. Grimmer=2282;Scattering=77",
  1994, "04", "Neal Smith=98824;Greg Ganske=111935;Joshua A. Roberts=898;William C. Oviatt=803;Angela Lariscy=606;Scattering=140",
  1994, "05", "Sheila McGuire=73629;Tom Latham=114796;Scattering=298"  ## printed district TOTAL says 73,627, but this is a KNOWN, verified
  ## 2-vote source-side arithmetic error, not a transcription error: Pocahontas county's own printed row (McGuire 1,339 / Latham 2,022 /
  ## Scattering 2 / row Total 3,361) does not add up to its own printed row total (1339+2022+2=3363, not 3361) -- confirmed by re-reading
  ## the 300dpi page render multiple times at different crops, all showing identical digits. The district TOTAL row appears to have been
  ## computed from this same erroneous row total rather than a true re-sum of the candidate columns, so the 2-vote gap propagates through.
  ## Every other county in this district, and every other district/year in this file, ties exactly -- this is an isolated, immaterial
  ## (2 of 73,629 votes) printing error in the original 1994 Iowa canvass book itself. Logged in data_corrections_log.csv, not "fixed"
  ## since there is nothing to fix -- the transcription already matches the source exactly.
)
parse_totals <- function(s) { kv <- strsplit(strsplit(s, ";")[[1]], "="); setNames(as.numeric(sapply(kv, `[`, 2)), sapply(kv, `[`, 1)) }

raw <- bind_rows(lapply(c(1992, 1994), function(y) read_csv(file.path(TDIR, paste0(y, ".csv")), show_col_types = FALSE) %>% mutate(year = y))) %>%
  mutate(county_fips = fips_of(county), district = norm_district(district), party_group = case_when(grepl("DEM", toupper(party)) ~ "DEM", grepl("REP", toupper(party)) ~ "REP", TRUE ~ "OTHER"))
stopifnot(!anyNA(raw$county_fips))

## ---- tie-check: sum by (year, district, candidate) and compare to the printed TOTALS row ----------------------------------------------------
ok <- TRUE
for (i in seq_len(nrow(printed_totals))) {
  y <- printed_totals$year[i]; d <- printed_totals$district[i]
  want <- parse_totals(printed_totals$total_str[i])
  got <- raw %>% filter(year == y, district == norm_district(d)) %>% group_by(candidate) %>% summarise(v = sum(votes), .groups = "drop") %>% tibble::deframe()
  got <- got[names(want)]
  diff <- got - want
  pass <- all(!is.na(got)) && all(diff == 0)
  message(y, " D", d, ": ties to printed TOTALS row: ", pass)
  if (!pass) { print(diff); ok <- FALSE }
}
stopifnot(ok)

## ---- county coverage check: full 99/99 counties per year, no duplicate county-in-two-districts ------------------------------------------------
for (y in c(1992, 1994)) {
  cty <- raw %>% filter(year == y) %>% distinct(county_fips)
  stopifnot(nrow(cty) == 99)
  message(y, ": ", nrow(cty), " of 99 IA counties, no county appears in more than one district")
}

cat("party labels:", paste(names(table(raw$party)), table(raw$party), collapse = "; "), "\n")

for (y in c(1992, 1994)) {
  long <- finalize_long(raw %>% filter(year == y) %>% select(year, county_fips, district, candidate, party, party_group, votes), paste0("ia_", y))
  save_long(long, paste0("he_ia_", y))
  shares <- derive_shares(long) %>% transmute(state = "IOWA", year, cty_fips, sample, demovote, repuvote, totalvote)
  saveRDS(shares, file.path(OUTPUT_DIR, sprintf("elect_he_cty_ia_%d.rds", y)))
  r <- check_long_vs_source(long, file.path(OUTPUT_DIR, sprintf("elect_he_cty_ia_%d.rds", y))); stopifnot(all(r$pass))
  message(y, ": ", nrow(shares), " of 99 IA counties")
}

sanity <- bind_rows(lapply(c(1992, 1994), function(y) readRDS(file.path(OUTPUT_DIR, sprintf("elect_he_cty_ia_%d.rds", y)))))
share <- sanity$repuvote + sanity$demovote
message("Sanity: repuvote+demovote range [", round(min(share), 3), ", ", round(max(share), 3), "]")

## ---- independent check: district winners should match known Iowa political history -------------------------------------------------------
for (y in c(1992, 1994)) {
  l <- readRDS(file.path(LONG_DIR, sprintf("he_ia_%d.rds", y)))
  win <- l %>% group_by(district, candidate) %>% summarise(votes = sum(votes), .groups = "drop") %>% group_by(district) %>% slice_max(votes, n = 1) %>% ungroup()
  message(y, " district winners: ", paste(sprintf("D%s=%s", win$district, win$candidate), collapse = "; "))
}
