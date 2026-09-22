## Arizona county-level U.S. House results, 1994 general election (pre-2000 gap; 01u only reaches 2000).
##
## SOURCE: the official State of Arizona Canvass (Nov 8, 1994 general; compiled by the Secretary of
## State Nov 28, 1994), PDF page 1, archived by the Wayback Machine from
##   https://azsos.gov/sites/default/files/canvass1994ge.pdf
##   (linked from azsos.gov/elections/voter-registration-historical-election-data/historical-election-results-information)
## saved as R/data/raw_house_county_open_states/arizona_historical/canvass1994ge.pdf. The PDF is
## image-only. Page 1 prints, for every one of the 6 congressional districts, a county-column x
## candidate-row table (16 columns: 15 counties + TOTAL). OCR digits were NOT trusted (this canvass
## family's OCR shows dropped/garbled digits, and its column alignment is unreliable); instead the
## rendered page image (220 dpi) was read directly and every
## non-empty county cell transcribed into `canvass` below, then verified two independent ways:
##   (1) each district's column sum == that district's own printed TOTAL column (asserted below;
##       all 18 district x party totals tie exactly), so a misread digit anywhere would fail;
##   (2) two counties' own archived official files agree exactly (asserted below when the files exist):
##       Maricopa (Maricopa1994GE.prn.txt, "OFFICIAL CANVASS" TOTAL rows for CD1-4 and CD6) and
##       Santa Cruz (SantaCruz1994GE.tab.txt, county TOTAL rows for CD2).
##
## WHY NOT THE PRECINCT-LEVEL ARCHIVE the directive named
## (apps.azsos.gov/results/1994general/counties/): the Wayback copy is badly incomplete. Per the CDX
## index, Apache, Graham, Greenlee, Mohave, Navajo and Pima have EMPTY folders (no files archived at
## all); Coconino has ~80 individual precinct files with gaps in the numbering; Pinal has 54 precinct
## files; Cochise/Gila/La Paz files are UNOFFICIAL precinct dumps with duplicated re-run reports (same
## precinct repeated, different counts for the same precinct), which cannot be summed to a certified
## county total (naive sums came out ~8x the canvass). Only Maricopa + Santa Cruz are usable
## (used for cross-check above), so the certified statewide canvass is the only complete source.
##
## Definitions: the canvass prints no House write-in rows in 1994, so totalvote = D + R + Libertarian
## (blank/over/undervotes are not in the table). Libertarian -> OTHER (non-DEM/REP), as elsewhere.
## Split counties (Maricopa 5 districts, Pima/Pinal/Coconino/Navajo/Graham 2-3) are summed by county.
## Output: R/output/elect_he_cty_az_1994.rds. Does NOT touch elect_cty_final.rds / the tracker.

source(file.path("R", "00_setup.R"))
library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "arizona_historical")

az_fips <- read_delim(file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
                      delim = "\t", show_col_types = FALSE) %>%
  filter(state == "ARIZONA", !is.na(county_fips)) %>%
  distinct(county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))

## district, county, D, R, L  -- transcribed from canvass page 1 (only counties in each district's table)
canvass <- tribble(
  ~district, ~county,      ~dem,  ~rep,   ~lib,
  1, "MARICOPA",          70627, 101350,  8890,
  2, "MARICOPA",          23351,  10559,  2185,
  2, "PIMA",              26111,   8981,  1814,
  2, "PINAL",                69,     10,     1,
  2, "SANTA CRUZ",         4529,   1951,   204,
  2, "YUMA",               8529,  11296,   856,
  3, "COCONINO",           2858,   5997,     0,
  3, "LA PAZ",             1026,   2569,     0,
  3, "MARICOPA",          34546,  81231,     0,
  3, "MOHAVE",             9488,  22149,     0,
  3, "NAVAJO",              398,    235,     0,
  3, "YAVAPAI",           13623,  33215,     0,
  4, "MARICOPA",          69760, 116714,  7428,
  5, "COCHISE",            8926,  16352,  1091,
  5, "GRAHAM",             2192,   4881,   239,
  5, "PIMA",              50254, 123212,  6172,
  5, "PINAL",              2064,   5069,   319,
  6, "APACHE",            11228,   5031,   543,
  6, "COCONINO",          11228,   8928,  1136,
  6, "GILA",               5905,   8347,   782,
  6, "GRAHAM",              282,     37,     7,
  6, "GREENLEE",           1304,   1433,   132,
  6, "MARICOPA",          30573,  62417,  3329,
  6, "NAVAJO",            10121,  10513,   768,
  6, "PINAL",             10680,  10354,   990
)

## District TOTAL column as printed on the canvass (verification target).
printed <- tribble(
  ~district, ~dem,  ~rep,   ~lib,
  1, 70627, 101350,  8890,
  2, 62589,  32797,  5060,
  3, 61939, 145396,     0,
  4, 69760, 116714,  7428,
  5, 63436, 149514,  7821,
  6, 81321, 107060,  7687
)

chk <- canvass %>% group_by(district) %>% summarise(across(c(dem, rep, lib), sum), .groups = "drop")
stopifnot(isTRUE(all.equal(as.data.frame(chk), as.data.frame(printed), check.attributes = FALSE)))
message("Verification 1: all 6 districts x 3 parties tie EXACTLY to the canvass's printed TOTAL column.")
stopifnot(setequal(unique(canvass$county), az_fips$county_name), length(unique(canvass$county)) == 15)

## ---- Verification 2: independent county files, when the archived raw files are present ---------
mar <- file.path(RAW_DIR, "1994", "maricopa", "Maricopa1994GE.prn.txt")
if (file.exists(mar)) {
  tot <- grep("^TOTAL", readLines(mar, warn = FALSE), value = TRUE)
  nums <- function(l) as.numeric(gsub(",", "", strsplit(trimws(sub("^TOTAL", "", l)), "\\s+")[[1]]))
  a <- nums(tot[1]); b <- nums(tot[2])   # tot[1]: reg, ballots, Sen R/D/L, CD1 R/D/L, CD2 R/D/L, CD3 R/D ; tot[2]: reg, ballots, CD4 R/D/L, CD6 R/D/L
  mp <- canvass %>% filter(county == "MARICOPA") %>% arrange(district)
  ok <- c(a[6:8] == c(mp$rep[1], mp$dem[1], mp$lib[1]),      # CD1
          a[9:11] == c(mp$rep[2], mp$dem[2], mp$lib[2]),     # CD2
          a[12:13] == c(mp$rep[3], mp$dem[3]),                # CD3
          b[3:5] == c(mp$rep[4], mp$dem[4], mp$lib[4]),      # CD4
          b[6:8] == c(mp$rep[5], mp$dem[5], mp$lib[5]))      # CD6
  stopifnot(all(ok))
  message("Verification 2a: Maricopa's own OFFICIAL CANVASS file matches the statewide canvass for CD1-4 and CD6 (", sum(ok), " values, 0 diffs).")
}
sc <- file.path(RAW_DIR, "1994", "santa_cruz", "SantaCruz1994GE.tab.txt")
if (file.exists(sc)) {
  x <- read_tsv(sc, col_names = FALSE, show_col_types = FALSE, col_types = cols(.default = "c"))
  ## NB this file also labels the GOVERNOR candidates (Basha/Symington/Buttrick) with the office string
  ## "US REPRESENTATIVE DIST. 2" -- filtering on office alone would mix in governor totals, so match the
  ## three actual District 2 candidates by name.
  x <- x %>% filter(grepl("^US REPRESENTATIVE", X1), X5 == "TOTAL", X2 %in% c("PASTOR, ED", "MACDONALD, ROBERT", "BERTRAND, JAMES")) %>%
    transmute(party = X3, v = as.numeric(X6))
  stopifnot(nrow(x) == 3)
  s <- canvass %>% filter(county == "SANTA CRUZ")
  stopifnot(x$v[x$party == "DEM"] == s$dem, x$v[x$party == "REP"] == s$rep, x$v[x$party == "LBT"] == s$lib)
  message("Verification 2b: Santa Cruz's own county file TOTAL rows match the canvass (D ", s$dem, ", R ", s$rep, ", L ", s$lib, ").")
}

## ---- Aggregate to county ------------------------------------------------------------------------
elect_he_cty_az_1994 <- canvass %>%
  group_by(county) %>%
  summarise(demovote_n = sum(dem), repuvote_n = sum(rep), totalvote = sum(dem + rep + lib), .groups = "drop") %>%
  left_join(az_fips, by = c("county" = "county_name")) %>%
  transmute(state = "ARIZONA", year = 1994, cty_fips = county_fips, sample = "HE",
            demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote) %>%
  arrange(cty_fips)
stopifnot(nrow(elect_he_cty_az_1994) == 15, !anyNA(elect_he_cty_az_1994))
save_step(elect_he_cty_az_1994, "elect_he_cty_az_1994")

sanity <- elect_he_cty_az_1994$demovote + elect_he_cty_az_1994$repuvote
message("rows: ", nrow(elect_he_cty_az_1994), "; repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")
message("Statewide House votes (sum of counties): ", format(sum(elect_he_cty_az_1994$totalvote), big.mark = ","),
        " ; D share ", round(sum(elect_he_cty_az_1994$demovote * elect_he_cty_az_1994$totalvote) / sum(elect_he_cty_az_1994$totalvote), 4))
print(elect_he_cty_az_1994)
