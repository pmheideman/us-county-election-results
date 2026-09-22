## Maine county-level U.S. House results, 2018 (fills MEDSL's 6-of-16-county gap).
##
## Why: MEDSL's 2018 Maine House rows cover only 6 counties (Cumberland, Kennebec, Knox, Lincoln,
## Sagadahoc, York) and only their District 1 portions -- District 2 (Golden/Poliquin, the state's
## first ranked-choice congressional race) is missing entirely. OpenElections' 2018 file has the
## same District-1-only gap. The Maine Secretary of State's own 2018 results page has both districts:
##   https://www.maine.gov/sos/elections-voting/election-results-data/election-results-2018
##   CD1: rep-cd1-11-6.xlsx           (town level, county code column, "County Totals" rows)
##   CD2: Rep-congress-results.xlsx   (ward/precinct level, county code column, "Results Reported by Towns")
## (both under /sos/sites/maine.gov.sos/files/content/assets/)
##
## CD2 vote definition: FIRST-ROUND (first-choice) votes only, as reported by town. Ranked-choice
## transfers are a statewide-tabulation artifact that has no county breakdown and is not what the
## rest of the panel measures. The town file predates the final certified tabulation slightly:
## its statewide first-round candidate total is 289,332 vs 289,624 certified (-0.10%; Poliquin
## 134,061 vs 134,184, Golden 131,954 vs 132,013), so county totals here are ~0.1% low in CD2
## counties. Two-party shares are unaffected to the 3rd decimal (checked below).
##
## Pseudo-rows handled: "County Totals"/"District Totals"/blank rows (CTY code NA -> dropped by
## the county-code filter), "STATE UOCAVA" (no county, dropped, ~0.4% of votes, same as 01bo),
## CD2's helper column "TBC by Town" (a per-town aggregate repeated on the ABS row -- NOT used;
## would double-count). BLANK ballots are excluded from totalvote (01bo/01af convention).
## Maine has no split towns; Kennebec/Knox/Lincoln/Sagadahoc/Waldo-type counties split across the
## two districts, so county totals are summed across both files.
##
## Output: R/output/elect_he_cty_me_2018.rds. Does NOT touch elect_cty_final.rds (serialized
## fold-in is done separately: replaces MEDSL's partial 2018 Maine rows with these 16).

source(file.path("R", "00_setup.R"))
library(readxl)
library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "maine")
BASE <- "https://www.maine.gov/sos/sites/maine.gov.sos/files/content/assets/"
fetch <- function(remote, local) {
  dest <- file.path(RAW_DIR, local)
  if (!file.exists(dest) || file.size(dest) == 0)
    system2("curl", c("-sL", "-A", shQuote("Mozilla/5.0"), "-o", shQuote(dest), shQuote(paste0(BASE, remote))))
  dest
}
fetch("rep-cd1-11-6.xlsx", "2018_house_cd1.xlsx")
fetch("Rep-congress-results.xlsx", "2018_house_cd2.xlsx")

me_fips <- read_delim(file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
                      delim = "\t", show_col_types = FALSE) %>%
  filter(state == "MAINE", !is.na(county_fips)) %>%
  distinct(county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))

CTY_CODE <- c(AND = "ANDROSCOGGIN", ARO = "AROOSTOOK", CUM = "CUMBERLAND", FRA = "FRANKLIN",
              HAN = "HANCOCK", KEN = "KENNEBEC", KNO = "KNOX", LIN = "LINCOLN", OXF = "OXFORD",
              PEN = "PENOBSCOT", PIS = "PISCATAQUIS", SAG = "SAGADAHOC", SOM = "SOMERSET",
              WAL = "WALDO", WAS = "WASHINGTON", YOR = "YORK")
num <- function(x) suppressWarnings(as.numeric(gsub(",", "", trimws(as.character(x)))))
rd <- function(f, sheet = 1) suppressMessages(read_excel(file.path(RAW_DIR, f), sheet = sheet, col_names = FALSE, col_types = "text"))

## ---- CD1: cols DIST, CTY, TOWN, Grohman(IND), Holbrook(REP), Pingree(DEM), BLANK, TOTAL VOTES CAST ----
c1 <- rd("2018_house_cd1.xlsx")
stopifnot(grepl("Grohman", c1[[4]][1]), grepl("Holbrook", c1[[5]][1]), grepl("Pingree", c1[[6]][1]),
          grepl("Independent", c1[[4]][3]), grepl("Republican", c1[[5]][3]), grepl("Democratic", c1[[6]][3]))
d1 <- c1[toupper(trimws(c1[[2]])) %in% names(CTY_CODE), ] %>%
  transmute(county = CTY_CODE[toupper(trimws(...2))], district = "1", oth = num(...4), rep = num(...5), dem = num(...6))
## (renamed access below because tibble from read_excel has ...N names)

## ---- CD2: cols TYP, CTY, Municipality, W-P, Bond(IND), Golden(DEM), Hoar(IND), Poliquin(REP), BLK, TBC ----
c2 <- rd("2018_house_cd2.xlsx")
stopifnot(grepl("Bond", c2[[5]][1]), grepl("Golden", c2[[6]][1]), grepl("Hoar", c2[[7]][1]), grepl("Poliquin", c2[[8]][1]),
          grepl("Dem", c2[[6]][3]), grepl("Rep", c2[[8]][3]))
d2 <- c2[toupper(trimws(c2[[2]])) %in% names(CTY_CODE), ] %>%
  transmute(county = CTY_CODE[toupper(trimws(...2))], district = "2",
            oth = num(...5) + num(...7), rep = num(...8), dem = num(...6))

if (anyNA(d1) || anyNA(d2)) stop("unparsed numeric cells in CD1/CD2")

## ---- Verify against each file's own printed totals BEFORE aggregating ---------------------------
tot1 <- c1[grepl("District Totals", c1[[3]]), ]
uoc1 <- c1[grepl("UOCAVA", c1[[3]]), ]
message("   (CD1 UOCAVA row: D ", num(uoc1[[6]]), " R ", num(uoc1[[5]]), " O ", num(uoc1[[4]]), ")")
## Printed District Totals include the STATE UOCAVA row, so add it back before comparing:
stopifnot(sum(d1$dem) + num(uoc1[[6]]) == num(tot1[[6]]),
          sum(d1$rep) + num(uoc1[[5]]) == num(tot1[[5]]),
          sum(d1$oth) + num(uoc1[[4]]) == num(tot1[[4]]))

tot2 <- c2[nrow(c2), ]                                                # unlabeled grand-total row (last row)
uoc2 <- c2[grepl("UOCAVA", c2[[3]]), ]
stopifnot(is.na(tot2[[2]]), is.na(tot2[[3]]), !is.na(tot2[[5]]), nrow(uoc2) == 1)
stopifnot(sum(d2$dem) + num(uoc2[[6]]) == num(tot2[[6]]),
          sum(d2$rep) + num(uoc2[[8]]) == num(tot2[[8]]),
          sum(d2$oth) + num(uoc2[[5]]) + num(uoc2[[7]]) == num(tot2[[5]]) + num(tot2[[7]]))
message("CD2: ward sums + UOCAVA tie out exactly to the file's printed grand-total row")

## ---- Aggregate to county ------------------------------------------------------------------------
me18 <- bind_rows(d1, d2) %>%
  group_by(county) %>%
  summarise(demovote_n = sum(dem), repuvote_n = sum(rep), totalvote = sum(dem + rep + oth), .groups = "drop") %>%
  left_join(me_fips, by = c("county" = "county_name"))
stopifnot(!any(is.na(me18$county_fips)), nrow(me18) == 16)

elect_he_cty_me_2018 <- me18 %>%
  transmute(state = "MAINE", year = 2018, cty_fips = county_fips, sample = "HE",
            demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote) %>%
  arrange(cty_fips) %>%
  save_step("elect_he_cty_me_2018")

## ---- Sanity checks --------------------------------------------------------------------------------
message("\n== rows: ", nrow(elect_he_cty_me_2018), " (expect 16); two-party sum range: ",
        paste(round(range(elect_he_cty_me_2018$demovote + elect_he_cty_me_2018$repuvote), 3), collapse = " - "))
stat <- bind_rows(d1, d2) %>% group_by(district) %>%
  summarise(dem = sum(dem), rep = sum(rep), oth = sum(oth), .groups = "drop")
print(stat)

## vs certified first-round (SOS RCV summary report): CD2 Golden 132,013 / Poliquin 134,184 / Bond 16,552 / Hoar 6,875
cert <- c(golden = 132013, poliquin = 134184, others = 16552 + 6875)
mine <- with(stat[stat$district == "2", ], c(golden = dem, poliquin = rep, others = oth))
message("CD2 vs certified round 1 (pct diff, excl. UOCAVA ~1,020 votes):")
print(round(100 * (mine / cert - 1), 2))

## vs OpenElections 2018 (District 1 only) for the 6 counties MEDSL/OE share: CD1 sums must match OE exactly
oe <- read_csv("https://raw.githubusercontent.com/openelections/openelections-data-me/master/2018/20181106__me__general__town.csv",
               show_col_types = FALSE) %>% filter(office == "U.S. House", candidate != "Blanks", county != "") %>%
  group_by(county = toupper(county)) %>% summarise(oe_dem = sum(votes[party == "DEM"]), oe_tot = sum(votes), .groups = "drop")
## (tot is computed before dem: summarise() would otherwise see the already-summed `dem`)
cmp <- d1 %>% group_by(county) %>% summarise(tot = sum(dem + rep + oth), dem = sum(dem), .groups = "drop") %>%
  inner_join(oe, by = "county")
message("CD1 vs OpenElections (6 counties): max |dem diff| = ", max(abs(cmp$dem - cmp$oe_dem)),
        ", max |total diff| = ", max(abs(cmp$tot - cmp$oe_tot)))
print(elect_he_cty_me_2018)
