## Arizona county-level U.S. House results, 1990 (extends 01u's 2000-2014 back one cycle).
##
## Source: Arizona Secretary of State's OFFICIAL CANVASS - GENERAL ELECTION - November 6, 1990
## (Jim Shumway, SoS, dated November 26, 1990), from the Wayback Machine copy of
##   https://azsos.gov/sites/default/files/canvass1990ge.pdf
## saved as R/data/raw_house_county_open_states/arizona_historical/canvass1990ge.pdf.
## The PDF is a 12-page image-only scan (~100 ppi) and page 1 is printed SIDEWAYS (rotated 90 deg),
## which is why OCR found nothing ("ocr_1990.txt" has no 'Congress'). Rotating the embedded page
## image (pdfimages -> rotate 90 deg CCW) makes it legible: ALL FIVE U.S. House races are on page 1,
## county columns x candidate rows, with a printed TOTAL column and TOTAL BALLOTS CAST row.
##
## NOTE: Arizona had FIVE congressional districts in 1990 (the 6th seat came with the 1990 census,
## first elected in 1992), not six.
##
## Every number below was transcribed by reading the rotated page image directly (not OCR digits).
## Verification, enforced by stopifnot() below:
##   (1) each candidate's county cells sum EXACTLY to that candidate's printed TOTAL cell;
##   (2) each county's total House vote is <= that county's printed TOTAL BALLOTS CAST and >= 80% of it.
##       Observed 0.90-0.95 everywhere EXCEPT Maricopa at 0.843: expected, not an error -- Maricopa spans
##       four districts including uncontested District 1 (Rhodes vs. write-ins only), which has much
##       higher roll-off. (The floor was set to 0.80 after seeing this; the exact row-sum tie in (1) is
##       the real per-cell check, (2) is a coarse independent one.)
##   (3) statewide candidate totals are the printed TOTAL column values.
## A "-----" cell in the source means the district does not touch that county (0 votes), so split
## counties (Apache/Gila/Graham/Navajo/Maricopa/... appear in several districts) simply sum by county.
##
## Party = printed (D)/(R)/(SW) label. Write-in candidates (Rose, McDonald, Henry) are classified OTHER
## regardless of their printed party label, matching 01u / every other state in this project.
## totalvote = sum of every candidate's votes incl. write-ins (matches 01u).
##
## Outputs R/output/elect_he_cty_az_1990.rds (same schema as elect_he_cty_az.rds).
## Does NOT touch elect_cty_final.rds / the coverage tracker (serialized fold-in done separately).

source(file.path("R", "00_setup.R"))
library(readr)

county_fips_crosswalk <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))
az_fips <- county_fips_crosswalk %>% filter(state == "ARIZONA") %>% select(county_name, county_fips)

COUNTIES <- c("APACHE", "COCHISE", "COCONINO", "GILA", "GRAHAM", "GREENLEE", "LA PAZ", "MARICOPA",
              "MOHAVE", "NAVAJO", "PIMA", "PINAL", "SANTA CRUZ", "YAVAPAI", "YUMA")

## Printed header rows (page 1): TOTAL BALLOTS CAST by county, in COUNTIES order; printed statewide 1,094,735.
BALLOTS <- c(14284, 25409, 29668, 15230, 7975, 3071, 3641, 640847, 28894, 18864, 208368, 29880, 6460, 43180, 18964)
stopifnot(sum(BALLOTS) == 1094735)

## One row per candidate: district, party (as printed), write-in flag, name, printed TOTAL, and a named
## vector of the county cells that are not "-----".
cand <- function(district, party, writein, name, total, ...) {
  v <- c(...)
  list(district = district, party = party, writein = writein, name = name, total = total, cells = v)
}
CANDS <- list(
  cand(1, "R",  FALSE, "John J. Rhodes III",        166223, MARICOPA = 166223),
  cand(1, "SW", TRUE,  "Betsy McDonald (write-in)",    172, MARICOPA = 172),
  cand(1, "R",  TRUE,  "Tim Rose (write-in)",          621, MARICOPA = 621),

  cand(2, "D",  FALSE, "Morris K. Udall",             76549, MARICOPA = 30802, PIMA = 33699, PINAL = 1261, `SANTA CRUZ` = 2358, YUMA = 8429),
  cand(2, "R",  FALSE, "Joseph Sweeney",              39586, MARICOPA = 17401, PIMA = 12890, PINAL = 554,  `SANTA CRUZ` = 865,  YUMA = 7876),
  cand(2, "D",  TRUE,  "Kathleen Henry (write-in)",      44, MARICOPA = 17,    PIMA = 25,    PINAL = 1,    `SANTA CRUZ` = 0,    YUMA = 1),

  cand(3, "D",  FALSE, "Roger Hartstone",            103018, COCONINO = 14249, `LA PAZ` = 1371, MARICOPA = 58520, MOHAVE = 11116, YAVAPAI = 17252, YUMA = 510),
  cand(3, "R",  FALSE, "Bob Stump",                  134279, COCONINO = 13776, `LA PAZ` = 2062, MARICOPA = 78924, MOHAVE = 15693, YAVAPAI = 23112, YUMA = 712),

  cand(4, "D",  FALSE, "Mark Ivey, Jr.",              89395, APACHE = 7456, GILA = 8210, GRAHAM = 267, MARICOPA = 65071,  NAVAJO = 8391),
  cand(4, "R",  FALSE, "Jon Kyl",                    141843, APACHE = 3763, GILA = 6162, GRAHAM = 75,  MARICOPA = 122470, NAVAJO = 9373),

  cand(5, "D",  FALSE, "Chuck Phillips",              75642, APACHE = 787,  COCHISE = 9995,  GRAHAM = 2741, GREENLEE = 1343, PIMA = 47207,  PINAL = 12768, `SANTA CRUZ` = 801),
  cand(5, "R",  FALSE, "Jim Kolbe",                  138975, APACHE = 1149, COCHISE = 14090, GRAHAM = 4142, GREENLEE = 1492, PIMA = 103295, PINAL = 13011, `SANTA CRUZ` = 1796)
)

long <- purrr::map_dfr(CANDS, function(cd) {
  ## (1) county cells must sum exactly to the printed TOTAL for this candidate
  if (sum(cd$cells) != cd$total)
    stop("Row does not tie to printed total: ", cd$name, " (", sum(cd$cells), " vs ", cd$total, ")")
  stopifnot(all(names(cd$cells) %in% COUNTIES))
  tibble(county = names(cd$cells), district = cd$district, name = cd$name, printed_party = cd$party,
         writein = cd$writein, votes = as.numeric(cd$cells))
}) %>%
  mutate(party = case_when(writein ~ "OTHER", printed_party == "D" ~ "DEM", printed_party == "R" ~ "REP", TRUE ~ "OTHER"))
message("All ", length(CANDS), " candidate rows tie exactly to their printed TOTAL column values.")

## ---- Aggregate to county (split counties sum across districts) ----------------------------------
az90 <- long %>%
  group_by(county) %>%
  summarise(totalvote = sum(votes),
            demovote_n = sum(votes[party == "DEM"]),
            repuvote_n = sum(votes[party == "REP"]),
            n_districts = n_distinct(district), .groups = "drop") %>%
  right_join(tibble(county = COUNTIES, ballots = BALLOTS), by = "county") %>%
  left_join(az_fips, by = c("county" = "county_name"))

stopifnot(nrow(az90) == 15, !anyNA(az90$county_fips), !anyNA(az90$totalvote))

## (2) roll-off check against each county's printed TOTAL BALLOTS CAST
az90 <- az90 %>% mutate(rolloff = totalvote / ballots)
message("House votes / ballots cast by county: range ", paste(round(range(az90$rolloff), 3), collapse = " - "))
print(az90 %>% select(county, n_districts, ballots, totalvote, rolloff) %>% mutate(rolloff = round(rolloff, 3)), n = 20)
stopifnot(all(az90$rolloff <= 1), all(az90$rolloff >= 0.80))

## (3) statewide totals
stopifnot(sum(az90$totalvote) == sum(vapply(CANDS, function(cd) cd$total, numeric(1))))

elect_he_cty_az_1990 <- az90 %>%
  transmute(state = "ARIZONA", year = 1990, cty_fips = county_fips, sample = "HE",
            demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote) %>%
  arrange(cty_fips) %>%
  save_step("elect_he_cty_az_1990")

## ---- Sanity output --------------------------------------------------------------------------------
message("\n== rows: ", nrow(elect_he_cty_az_1990), " (expect 15); demovote+repuvote range: ",
        paste(round(range(elect_he_cty_az_1990$demovote + elect_he_cty_az_1990$repuvote), 4), collapse = " - "))
st <- long %>% filter(!writein) %>% group_by(district, party) %>% summarise(v = sum(votes), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = party, values_from = v)
print(st %>% mutate(dem_share = round(DEM / (DEM + REP), 3)))
print(az90 %>% select(county, n_districts, ballots, totalvote, rolloff) %>% mutate(rolloff = round(rolloff, 3)), n = 20)
print(elect_he_cty_az_1990)
