## Arizona county-level U.S. House results, 1996 general election (pre-OpenElections gap; 01u starts at 2000).
##
## Source, and why the production numbers come from the CANVASS rather than the precinct files:
## AZ SOS "Historical Election Results" page (Wayback), 1996 section:
##   * Official Canvass PDF   https://azsos.gov/sites/default/files/canvass1996ge.pdf      (image-only scan)
##   * Precinct-level results https://apps.azsos.gov/results/1996general/counties/<County>/  (Wayback)
## The canvass prints, for each of the 6 districts, every candidate's vote in every county that touches
## that district plus a printed TOTAL column -- i.e. exactly the county x district table we need, for all
## 15 counties. The precinct directory, in contrast, is a per-county grab-bag of incompatible formats:
## tidy tab exports (Apache, Graham, Pima), one wide sheet (Mohave), a county "official canvass" print
## (Maricopa), ballot-image report blocks with repeating per-precinct/cumulative sections (La Paz, Gila,
## Coconino, Santa Cruz), WordPerfect binaries (Greenlee, Pinal), scanned GIFs only (Navajo, Yavapai), and
## two files that are not archived at any Wayback snapshot (Cochise, Yuma -> 404). So the precinct files
## are used as an INDEPENDENT CROSS-CHECK on every county where a total can be extracted, not as the source.
##
## Canvass transcription: the PDF is image-only (pdftotext returns ~nothing; ocrmypdf digits are wrong,
## e.g. 88,214 read as "88,21¢"), so PDF pages 4-5 were rendered at 200dpi and every number read by eye.
## Each candidate row's county entries are asserted below to sum EXACTLY to that row's printed TOTAL
## (all 16 candidate rows tie), so a mis-read digit anywhere in a row cannot survive.
##
## totalvote = sum of every candidate listed on the House lines (REP/DEM/LBT/RPA; the canvass lists no
## House write-ins), blanks/overvotes excluded. Counties split across districts (Maricopa 1/2/3/4/6, Pima
## 2/5, Pinal 2/5/6, Coconino 3/6, Navajo 3/6, Graham 5/6) are summed by county.
## Does NOT touch elect_cty_final.rds or the coverage tracker (serialized fold-in is done separately).

source(file.path("R", "00_setup.R"))
library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "arizona_historical", "1996")

az_fips <- read_delim(file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
                      delim = "\t", show_col_types = FALSE) %>%
  filter(state == "ARIZONA", !is.na(county_fips)) %>%
  distinct(county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))

## ---- Canvass transcription: candidate rows with (county = votes) entries and the printed TOTAL --------
## Counties absent from a row are printed as "---" in the canvass (district does not touch that county).
cand <- function(district, name, party, total, ...) {
  v <- c(...)
  tibble(district = district, candidate = name, party = party, county = toupper(names(v)),
         votes = as.numeric(v), printed_total = total)
}
canvass <- bind_rows(
  ## District 1 (Maricopa only)
  cand(1, "SALMON",      "REP", 135634, MARICOPA = 135634),
  cand(1, "COX",         "DEM",  89738, MARICOPA =  89738),
  ## District 2
  cand(2, "BUSTER",      "REP",  38786, MARICOPA = 10726, PIMA =  9640, PINAL =   6, `SANTA CRUZ` = 2076, YUMA = 16338),
  cand(2, "PASTOR",      "DEM",  81982, MARICOPA = 30678, PIMA = 34519, PINAL = 105, `SANTA CRUZ` = 5670, YUMA = 11010),
  cand(2, "BANGLE",      "LBT",   5333, MARICOPA =  1978, PIMA =  2362, PINAL =   4, `SANTA CRUZ` =  278, YUMA =   711),
  ## District 3
  cand(3, "STUMP",       "REP", 175231, COCONINO = 7770, `LA PAZ` = 2883, MARICOPA = 99644, MOHAVE = 25525, NAVAJO = 204, YAVAPAI = 39205),
  cand(3, "SCHNEIDER",   "DEM",  88214, COCONINO = 4395, `LA PAZ` = 1552, MARICOPA = 49989, MOHAVE = 14049, NAVAJO = 485, YAVAPAI = 17744),
  ## District 4 (Maricopa only)
  cand(4, "SHADEGG",     "REP", 150486, MARICOPA = 150486),
  cand(4, "MILTON",      "DEM",  74857, MARICOPA =  74857),
  ## District 5
  cand(5, "KOLBE",       "REP", 179349, COCHISE = 19941, GRAHAM = 4476, PIMA = 149186, PINAL = 5746),
  cand(5, "NELSON",      "DEM",  67597, COCHISE =  9803, GRAHAM = 3419, PIMA =  51562, PINAL = 2813),
  cand(5, "ZAJAC",       "LBT",   7322, COCHISE =  1035, GRAHAM =  304, PIMA =   5761, PINAL =  222),
  cand(5, "FINKELSTEIN", "RPA",   6630, COCHISE =  1013, GRAHAM =  397, PIMA =   4964, PINAL =  256),
  ## District 6
  cand(6, "HAYWORTH",    "REP", 121431, APACHE =  5779, COCONINO =  8833, GILA = 7615, GRAHAM = 118, GREENLEE = 1447, MARICOPA = 76630, NAVAJO = 11066, PINAL =  9943),
  cand(6, "OWENS",       "DEM", 118957, APACHE = 12155, COCONINO = 15244, GILA = 8706, GRAHAM = 472, GREENLEE = 1719, MARICOPA = 53063, NAVAJO = 12576, PINAL = 15022),
  cand(6, "ANDERSON",    "LBT",  14899, APACHE =   977, COCONINO =  1771, GILA = 1128, GRAHAM =  14, GREENLEE =  196, MARICOPA =  7731, NAVAJO =  1260, PINAL =  1822)
)

## ---- Verification 1: every candidate row's county entries sum exactly to its printed TOTAL -------------
row_check <- canvass %>% group_by(district, candidate) %>%
  summarise(s = sum(votes), printed = first(printed_total), .groups = "drop")
stopifnot(nrow(row_check) == 16, all(row_check$s == row_check$printed))
message("Canvass transcription: all ", nrow(row_check), " candidate rows sum exactly to their printed TOTAL")

## Guard against pseudo-rows / stray labels sneaking in as candidates or counties
stopifnot(!any(grepl("TOTAL|BLANK|OVER|UNDER|WRITE", canvass$candidate)), all(canvass$county %in% az_fips$county_name))

## ---- Aggregate to county ------------------------------------------------------------------------------
to_party <- function(x) case_when(x == "DEM" ~ "DEM", x == "REP" ~ "REP", TRUE ~ "OTHER")   # LBT/RPA -> OTHER
az96 <- canvass %>%
  mutate(pty = to_party(party)) %>%
  group_by(county) %>%
  summarise(totalvote = sum(votes), demovote_n = sum(votes[pty == "DEM"]), repuvote_n = sum(votes[pty == "REP"]), .groups = "drop") %>%
  left_join(az_fips, by = c("county" = "county_name"))
stopifnot(!anyNA(az96$county_fips), nrow(az96) == 15, sum(az96$totalvote) == sum(canvass$votes))

elect_he_cty_az_1996 <- az96 %>%
  transmute(state = "ARIZONA", year = 1996, cty_fips = county_fips, sample = "HE",
            demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote) %>%
  arrange(cty_fips) %>%
  save_step("elect_he_cty_az_1996")

## ---- Verification 2: independent cross-check against the archived PRECINCT-level county files -----------
## Only counties whose file yields a clean county total are compared; the rest are listed as not verifiable.
canv_c <- function(d, nm) canvass %>% filter(district == d, candidate == nm) %>% select(county, votes)
expect_eq <- function(label, got, want) {
  ok <- length(got) > 0 && length(got) == length(want) && !anyNA(got) && isTRUE(all(got == want))   # empty/NA parse must FAIL, not pass
  message(sprintf("  %-34s %s  (file: %s | canvass: %s)", label, if (ok) "MATCH" else "**MISMATCH**",
                  paste(got, collapse = "/"), paste(want, collapse = "/")))
  ok
}
cv <- function(county, d, nm) canvass$votes[canvass$county == county & canvass$district == d & canvass$candidate == nm]
rd <- function(county, f) { p <- file.path(RAW_DIR, county, f)
  if (file.exists(p)) iconv(readLines(p, warn = FALSE, skipNul = TRUE, encoding = "latin1"), "latin1", "ASCII", sub = " ") else NULL }
res <- c()

## Apache & Graham: tidy tab exports; the row with precinct == "TOTAL" is the county total per candidate
tab_total <- function(county, f, name_rx) {
  x <- rd(county, f); if (is.null(x)) return(NULL)
  p <- strsplit(sub("\r$", "", x), "\t"); p <- p[lengths(p) >= 6]
  v <- vapply(p, function(z) if (z[5] == "TOTAL" && grepl(name_rx, z[2], ignore.case = TRUE)) as.numeric(z[6]) else NA_real_, 0)
  v[!is.na(v)]
}
res["Apache D6"]  <- expect_eq("Apache D6 (Owens/Hayworth/Anderson)",
  c(tab_total("Apache", "Apache1996GE.tab.txt", "^OWENS, STEVE")[1], tab_total("Apache", "Apache1996GE.tab.txt", "^HAYWORTH")[1], tab_total("Apache", "Apache1996GE.tab.txt", "^ANDERSON, ROBERT")[1]),
  c(cv("APACHE", 6, "OWENS"), cv("APACHE", 6, "HAYWORTH"), cv("APACHE", 6, "ANDERSON")))
gr <- function(rx) tab_total("Graham", "Graham1996GE.tab.txt", rx)[1]
res["Graham D5"] <- expect_eq("Graham D5 (Kolbe/Nelson/Zajac/Finkelstein)",
  c(gr("^Kolbe"), gr("^Nelson, Mort"), gr("^Zajac"), gr("^Finkelstein")),
  c(cv("GRAHAM", 5, "KOLBE"), cv("GRAHAM", 5, "NELSON"), cv("GRAHAM", 5, "ZAJAC"), cv("GRAHAM", 5, "FINKELSTEIN")))
res["Graham D6"] <- expect_eq("Graham D6 (Hayworth/Owens/Anderson)",
  c(gr("^Hayworth"), gr("^Owens, Steve"), gr("^Anderson, Robert")),
  c(cv("GRAHAM", 6, "HAYWORTH"), cv("GRAHAM", 6, "OWENS"), cv("GRAHAM", 6, "ANDERSON")))

## Pima: after each district's candidate-header line, the next line starting "TOTAL" has each candidate's
## county total in tab fields 4, 9, 14, 19 (polls + absentee = total, then pct, then denominator)
pima <- rd("Pima", "Pima1996GE.tab.txt")
if (!is.null(pima)) {
  pima_row <- function(hdr_rx) { i <- grep(hdr_rx, pima)[1]; j <- i + which(grepl("^TOTAL\t", pima[(i + 1):length(pima)]))[1]
                                 z <- strsplit(pima[j], "\t")[[1]]; as.numeric(gsub(",", "", z[c(4, 9, 14, 19)])) }
  res["Pima D2"] <- expect_eq("Pima D2 (Buster/Pastor/Bangle)", pima_row("BUSTER, JIM")[1:3], c(cv("PIMA", 2, "BUSTER"), cv("PIMA", 2, "PASTOR"), cv("PIMA", 2, "BANGLE")))
  res["Pima D5"] <- expect_eq("Pima D5 (Kolbe/Nelson/Zajac/Finkel.)", pima_row("KOLBE, JIM"), c(cv("PIMA", 5, "KOLBE"), cv("PIMA", 5, "NELSON"), cv("PIMA", 5, "ZAJAC"), cv("PIMA", 5, "FINKELSTEIN")))
}

## Mohave: one wide sheet, the "TOTAL" row: Stump / Schneider are fields 8 / 9
moh <- rd("Mohave", "Mohave1996GE.txt")
if (!is.null(moh)) {
  z <- strsplit(moh[grepl("^TOTAL\t", moh)][1], "\t")[[1]]
  res["Mohave D3"] <- expect_eq("Mohave D3 (Stump/Schneider)", as.numeric(z[8:9]), c(cv("MOHAVE", 3, "STUMP"), cv("MOHAVE", 3, "SCHNEIDER")))
}

## Maricopa: county print has a "COUNTY TOTAL" row on each contest's page; take the last 2-3 numbers
mar <- rd("Maricopa", "Maricopa1996GE.txt")
if (!is.null(mar)) {
  pages <- strsplit(paste(mar, collapse = "\n"), "\f")[[1]]
  mar_tot <- function(d) {
    p <- pages[grepl(paste0("REPRESENTATIVE IN CONGRESS ", d, "\\b"), pages) & grepl("COUNTY TOTAL", pages)][1]
    l <- strsplit(p, "\n")[[1]]; l <- l[grepl("^\\s*COUNTY TOTAL", l)][1]
    n <- as.numeric(strsplit(trimws(sub("COUNTY TOTAL", "", l)), "\\s+")[[1]])
    n[-(1:3)]                                    # drop registered / ballots cast / turnout %
  }
  for (d in c(1, 2, 3, 4, 6)) {
    nm <- unique(canvass$candidate[canvass$district == d])
    res[paste("Maricopa D", d)] <- expect_eq(paste("Maricopa D", d), mar_tot(d), vapply(nm, function(n) cv("MARICOPA", d, n), 0))
  }
}

## La Paz: ballot-image report; county total = the final "CUMULATIVE REPORT - 10 PRECINCTS PROCESSED" block
lp <- rd("LaPaz", "LaPaz1996GE.txt")
if (!is.null(lp)) {
  idx <- grep("CUMULATIVE REPORT -\\s+10 PRECINCTS PROCESSED\\s+PAGE\\s+1\\b", lp); last <- max(idx)
  blk <- lp[last:min(last + 60, length(lp))]
  g <- function(nm) as.numeric(sub("^\\s*\\d{3}\\s+(\\d+).*", "\\1", blk[grepl(nm, blk)][1]))
  res["La Paz D3"] <- expect_eq("La Paz D3 (Stump/Schneider)", c(g("STUMP, BOB"), g("SCHNEIDER")), c(cv("LA PAZ", 3, "STUMP"), cv("LA PAZ", 3, "SCHNEIDER")))
}

## Greenlee (WordPerfect binary, strings extracted by hand): Hayworth 1,447 / Owens 1,719 / Anderson 196 -- read from
## Greenlee1996GE.wpd.txt via `strings`; checked by eye, matches canvass.
res["Greenlee D6"] <- expect_eq("Greenlee D6 (by eye from wpd strings)", c(1447, 1719, 196), c(cv("GREENLEE", 6, "HAYWORTH"), cv("GREENLEE", 6, "OWENS"), cv("GREENLEE", 6, "ANDERSON")))

message("\nCross-check summary: ", sum(res), " of ", length(res), " comparisons match the canvass exactly.")
message("Not verifiable from precinct files (canvass only): Cochise & Yuma (files 404 at every Wayback snapshot), Navajo & Yavapai (scanned GIFs only),",
        " Pinal (WordPerfect binary, unreadable here), Santa Cruz (file has only 23 of ~36 precincts), and Coconino D3 / Gila / Santa Cruz (see notes).")
message("Known small discrepancies vs canvass in precinct files: Gila's last cumulative report is ~1.4% below the official canvass (Owens 8,579 vs 8,706;",
        " Hayworth 7,507 vs 7,615; Anderson 1,110 vs 1,128) -- a pre-canvass snapshot; the canvass is the official figure and is used.")
stopifnot(all(res))

## ---- Sanity checks --------------------------------------------------------------------------------------
message("\n== rows: ", nrow(elect_he_cty_az_1996), " (expect 15); two-party sum range: ",
        paste(round(range(elect_he_cty_az_1996$demovote + elect_he_cty_az_1996$repuvote), 3), collapse = " - "))
message("statewide: DEM ", round(sum(az96$demovote_n) / sum(az96$totalvote), 4), "  REP ", round(sum(az96$repuvote_n) / sum(az96$totalvote), 4))
print(elect_he_cty_az_1996)
