## Arizona county-level U.S. House results, 1992 general election (extends 01u's OpenElections-based
## 2000-2014 build back one cycle).
##
## Source: the State of Arizona OFFICIAL CANVASS, General Election, November 3, 1992 (Secretary of
## State), page 3 -- https://azsos.gov/sites/default/files/canvass1992ge.pdf, via the Wayback
## Machine copy linked from azsos.gov's "Historical Election Results & Information" page. It is an
## image-only scan and prints county columns x candidate rows, for each of AZ's 6 districts, with a
## per-candidate TOTAL column. Local copy: R/data/raw_house_county_open_states/arizona_historical/
## canvass1992ge.pdf (OCR'd copy `ocr_canvass1992ge.pdf`).
##
## Why the canvass and not the archived precinct-level directory
## (apps.azsos.gov/results/1992general/counties/): that directory is a patchwork -- Cochise, Graham,
## La Paz and Maricopa folders are EMPTY in the archive, Gila's file 404s, Yavapai/Yuma are WordPerfect
## (.wpd), Coconino/Pinal are hundreds of per-precinct text files, Pima is a numeric-coded dump --
## i.e. no single complete precinct source exists. The canvass has all 15 counties on one page.
##
## Transcription method (same as kentucky_2010 in 01i): OCR digits were NOT trusted; the House
## section was rendered at 260 dpi and every county/candidate cell read off the image by hand, then
## checked by requiring every candidate's county cells to sum EXACTLY to that candidate's printed
## TOTAL (hard stopifnot below, all 20 candidates). Independent cross-check against the archive's
## own precinct files where they survive: Apache1992GE.txt (District 6: English 11,340 / Wead 5,728 /
## Stannard 700) and Greenlee1992GE.tab.txt (District 6: English 2,097 / Wead 1,530 / Stannard 122) match
## the transcription exactly.
##
## Counties split across districts are summed by county (Maricopa 6 districts; Pima D2+D5; Pinal
## D2+D5+D6; Coconino D3+D6; Graham D5+D6; Navajo D3+D6). Dashes ("----") in the source mean the
## district does not touch that county and are simply omitted. totalvote = every listed candidate
## incl. write-ins (Robert Brown, D2); blanks/overvotes are not printed in the canvass. Party:
## (D) -> DEM, (R) -> REP, (L)/(NL)/(I) -> OTHER.
##
## Outputs R/output/elect_he_cty_az_1992.rds. Does NOT touch elect_cty_final.rds / the tracker.

source(file.path("R", "00_setup.R"))
library(readr)

az_fips <- read_delim(file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
                      delim = "\t", show_col_types = FALSE) %>%
  filter(state == "ARIZONA", !is.na(county_fips)) %>%
  distinct(county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))

## ---- hand transcription: district, candidate, party, printed TOTAL, county -> votes ----------
## cells = c(COUNTY = votes, ...) listing only the counties that carry votes (others are "----").
cand <- function(district, name, party, total, ...) {
  v <- c(...)
  tibble(district = district, candidate = name, party = party, printed_total = total,
         county = names(v), votes = as.numeric(v))
}

canvass <- bind_rows(
  ## District 1 -- Maricopa only
  cand(1, "Sam Coppersmith",        "DEM",   130715, MARICOPA = 130715),
  cand(1, "John J. Rhodes, III",    "REP",   113613, MARICOPA = 113613),
  cand(1, "Ted Goldstein (NL)",     "OTHER",  10461, MARICOPA = 10461),
  ## District 2 -- Maricopa, Pima, Pinal, Santa Cruz, Yuma
  cand(2, "Ed Pastor",              "DEM",    90693, MARICOPA = 37605, PIMA = 35026, PINAL = 87, `SANTA CRUZ` = 5294, YUMA = 12681),
  cand(2, "Don Shooter",            "REP",    41257, MARICOPA = 14858, PIMA = 10844, PINAL = 10, `SANTA CRUZ` = 2056, YUMA = 13489),
  cand(2, "Dan Detaranto (L)",      "OTHER",   5423, MARICOPA = 2294,  PIMA = 2272,  PINAL = 2,  `SANTA CRUZ` = 219,  YUMA = 636),
  cand(2, "Robert Brown (Write-in)","OTHER",      5, MARICOPA = 0,     PIMA = 1,     PINAL = 0,  `SANTA CRUZ` = 1,    YUMA = 3),
  ## District 3 -- Coconino, La Paz, Maricopa, Mohave, Navajo, Yavapai
  cand(3, "Roger Hartstone",        "DEM",    88830, COCONINO = 4680, `LA PAZ` = 1567, MARICOPA = 50867, MOHAVE = 13375, NAVAJO = 432, YAVAPAI = 17909),
  cand(3, "Bob Stump",              "REP",   158906, COCONINO = 7329, `LA PAZ` = 3023, MARICOPA = 90997, MOHAVE = 22662, NAVAJO = 262, YAVAPAI = 34633),
  cand(3, "Pamela Volponi (NL)",    "OTHER",  10767, COCONINO = 549,  `LA PAZ` = 142,  MARICOPA = 5751,  MOHAVE = 1706,  NAVAJO = 35,  YAVAPAI = 2584),
  ## District 4 -- Maricopa only
  cand(4, "Walter R. Mybeck, II",   "DEM",    70572, MARICOPA = 70572),
  cand(4, "Jon Kyl",                "REP",   156330, MARICOPA = 156330),
  cand(4, "Tim McDermott (L)",      "OTHER",  11611, MARICOPA = 11611),
  cand(4, "Debbie Collings (I)",    "OTHER",  25553, MARICOPA = 25553),
  ## District 5 -- Cochise, Graham, Pima, Pinal
  cand(5, "Jim Toevs",              "DEM",    77256, COCHISE = 9288,  GRAHAM = 2540, PIMA = 62946,  PINAL = 2482),
  cand(5, "Jim Kolbe",              "REP",   172867, COCHISE = 21261, GRAHAM = 5751, PIMA = 140497, PINAL = 5358),
  cand(5, "Perry Willis (L)",       "OTHER",   9690, COCHISE = 1345,  GRAHAM = 529,  PIMA = 7365,   PINAL = 451),
  ## District 6 -- Apache, Coconino, Gila, Graham, Greenlee, Maricopa, Navajo, Pinal
  cand(6, "Karan English",          "DEM",   124251, APACHE = 11340, COCONINO = 17001, GILA = 9253, GRAHAM = 397, GREENLEE = 2097, MARICOPA = 57308, NAVAJO = 12009, PINAL = 14846),
  cand(6, "Doug Wead",              "REP",    97074, APACHE = 5728,  COCONINO = 9714,  GILA = 7340, GRAHAM = 44,  GREENLEE = 1530, MARICOPA = 52391, NAVAJO = 9907,  PINAL = 10420),
  cand(6, "Sarah Stannard (I)",     "OTHER",  13047, APACHE = 700,   COCONINO = 1506,  GILA = 1095, GRAHAM = 20,  GREENLEE = 122,  MARICOPA = 6938,  NAVAJO = 1107,  PINAL = 1559)
)

## ---- Verification 1: each candidate's county cells sum exactly to the canvass's printed TOTAL ----
chk <- canvass %>% group_by(district, candidate, printed_total) %>%
  summarise(sum_counties = sum(votes), .groups = "drop")
message("Candidates transcribed: ", nrow(chk), " (expect 20); max |county sum - printed TOTAL| = ",
        max(abs(chk$sum_counties - chk$printed_total)))
stopifnot(nrow(chk) == 20, all(chk$sum_counties == chk$printed_total))
stopifnot(!anyNA(canvass$votes), all(canvass$county %in% az_fips$county_name))

## ---- Verification 2: independent precinct-file totals (Apache, Greenlee; District 6) ----
indep <- tribble(~county, ~candidate, ~votes_precinct_file,
                 "APACHE",   "Karan English",      11340, "APACHE",   "Doug Wead",  5728, "APACHE",   "Sarah Stannard (I)",  700,
                 "GREENLEE", "Karan English",       2097, "GREENLEE", "Doug Wead",  1530, "GREENLEE", "Sarah Stannard (I)",  122)
ind_cmp <- indep %>% inner_join(canvass, by = c("county", "candidate"))
stopifnot(nrow(ind_cmp) == nrow(indep), all(ind_cmp$votes == ind_cmp$votes_precinct_file))
message("Independent precinct-file cross-check (Apache, Greenlee, District 6): all ", nrow(ind_cmp), " cells match")

## ---- Aggregate to county (sum across districts) ------------------------------------------------
elect_he_cty_az_1992 <- canvass %>%
  group_by(county) %>%
  summarise(totalvote = sum(votes), demovote_n = sum(votes[party == "DEM"]),
            repuvote_n = sum(votes[party == "REP"]), .groups = "drop") %>%
  left_join(az_fips, by = c("county" = "county_name")) %>%
  filter(totalvote > 0) %>%
  transmute(state = "ARIZONA", year = 1992, cty_fips = county_fips, sample = "HE",
            demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote) %>%
  arrange(cty_fips) %>%
  save_step("elect_he_cty_az_1992")

## ---- Sanity checks ---------------------------------------------------------------------------------
stopifnot(nrow(elect_he_cty_az_1992) == 15, !anyNA(elect_he_cty_az_1992))
two <- elect_he_cty_az_1992$demovote + elect_he_cty_az_1992$repuvote
message("Rows: ", nrow(elect_he_cty_az_1992), " (expect 15); two-party sum range: ",
        paste(round(range(two), 3), collapse = " - "))
message("Statewide House total = ", sum(elect_he_cty_az_1992$totalvote),
        "; weighted D share = ", round(with(elect_he_cty_az_1992, sum(demovote * totalvote) / sum(totalvote)), 3),
        ", R share = ", round(with(elect_he_cty_az_1992, sum(repuvote * totalvote) / sum(totalvote)), 3))
print(elect_he_cty_az_1992)
