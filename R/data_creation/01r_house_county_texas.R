## Texas: found via OpenElections (github.com/openelections/openelections-data-tx), the last
## state in the large-states push (CA done/TX this file/NY done/OH done/MI done/NJ done/IL done).
##
## TX's repo has a general-election county-level file for every even year 2000-2016 EXCEPT that
## the exact path/office-label pair changes once, mid-span:
##   2000/2002/2004/2006/2008/2010/2012/2016: `<date>__tx__general__county.csv` at the year's
##       top level, office labelled "U. S. Representative" (note the unusual spacing -- a known
##       gotcha flagged before this file was even opened).
##   2014: NO top-level `__county.csv` file exists for the general election (confirmed 404) --
##       the equivalent clean county-level aggregate lives one directory deeper, at
##       `2014/counties/20141104__tx__general__county.csv`, with a DIFFERENT office label,
##       "U.S. House" (no internal spacing at all, unlike every other year's "U. S.
##       Representative"). Found only by checking the `counties/` subdirectory rather than
##       assuming 2014 needed the precinct-level files also present there (204 separate
##       `<county>__precinct.csv` files, which would have needed summing like NC/VA/Kansas-2014
##       -- the clean aggregate makes that unnecessary here).
## Every year's file has an explicit `candidate == "Total"` pseudo-row per (county, district) --
## same class of bug as Kansas/Georgia/New York's pseudo-total rows, filtered out explicitly.
## No repeated mid-file header rows found in any of these county-level files (that gotcha, per
## the project's earlier OpenElections survey, evidently applies to a different/precinct-level
## TX file, not these).
##
## Party column is a clean small code set in every year (DEM/REP/LIB/GRN/IND/WI/""), no
## write-in-suffix quirk like California -- exact-match alias table is fine here.
##
## 2006 is a genuine partial year: only 205 of 254 counties have ANY "U. S. Representative" row
## in the source at all (confirmed: the other 49 counties have full down-ballot results in the
## same file, e.g. Atascosa has 15 other offices present, just no U.S. House row) -- a real
## upstream gap, not a parsing bug, kept as partial coverage rather than excluded outright.
##
## 2016 overlaps MEDSL (which the coverage tracker already flags as only 97.2% for Texas that
## year) -- built here anyway, both as the cross-check year and because it's a source of nearly
## full (254/254) coverage that could someday supplement MEDSL's gap. Per this project's
## established convention (see 01d/01n/01o/01q), the fold-in below still only keeps year < 2016
## and leaves 2016+ to MEDSL, even though this source is more complete for TX 2016 specifically
## -- flagged in the project memory as a possible future follow-up rather than changed here.

source(file.path("R", "00_setup.R"))
library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "texas")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

county_fips_crosswalk <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))

tx_fips <- county_fips_crosswalk %>% filter(state == "TEXAS") %>% select(county_name, county_fips)

download_oe <- function(remote_path, local_name) {
  dest <- file.path(RAW_DIR, local_name)
  if (!file.exists(dest) || file.size(dest) == 0) {
    url <- paste0("https://raw.githubusercontent.com/openelections/openelections-data-tx/master/", remote_path)
    system2("curl", c("-sL", "-A", "'Mozilla/5.0'", "-o", shQuote(dest), shQuote(url)))
  }
  dest
}

to_party <- function(x) {
  case_when(x == "DEM" ~ "DEM", x == "REP" ~ "REP", TRUE ~ "OTHER")
}

## Years sharing the top-level file + "U. S. Representative" label (unusual double-space spacing)
TX_FILES_STD <- tribble(
  ~year, ~remote_path,
  2000,  "2000/20001107__tx__general__county.csv",
  2002,  "2002/20021105__tx__general__county.csv",
  2004,  "2004/20041102__tx__general__county.csv",
  2006,  "2006/20061107__tx__general__county.csv",
  2008,  "2008/20081104__tx__general__county.csv",
  2010,  "2010/20101102__tx__general__county.csv",
  2012,  "2012/20121106__tx__general__county.csv",
  2016,  "2016/20161108__tx__general__county.csv"
)

read_tx_year_std <- function(year, remote_path) {
  path <- download_oe(remote_path, paste0(year, "_general_county.csv"))
  read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    filter(office == "U. S. Representative", candidate != "Total") %>%
    mutate(county = toupper(trimws(county)), party = to_party(party), votes = as.numeric(votes)) %>%
    filter(!is.na(votes)) %>%
    group_by(county, party) %>% summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
    mutate(year = year)
}

## 2014: different path (one level deeper, in counties/) and different office label ("U.S.
## House", no internal spacing) -- see header note. Same "Total" pseudo-row and party codes as
## every other year otherwise.
read_tx_2014 <- function() {
  path <- download_oe("2014/counties/20141104__tx__general__county.csv", "2014_general_county.csv")
  read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    filter(office == "U.S. House", candidate != "Total") %>%
    mutate(county = toupper(trimws(county)), party = to_party(party), votes = as.numeric(votes)) %>%
    filter(!is.na(votes)) %>%
    group_by(county, party) %>% summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
    mutate(year = 2014)
}

tx_by_county <- bind_rows(
  pmap_dfr(TX_FILES_STD, read_tx_year_std),
  read_tx_2014()
)

elect_he_cty_tx <- tx_by_county %>%
  group_by(year, county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(county = ifelse(county == "LASALLE", "LA SALLE", county)) %>%  # crosswalk spells it "LA SALLE"
  left_join(tx_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(
    state = "TEXAS", year, cty_fips = county_fips, sample = "HE",
    demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
  ) %>%
  save_step("elect_he_cty_tx")

message("TX House county-level rows built: ", nrow(elect_he_cty_tx), " (of possible ", 9 * 254, ")")
print(table(elect_he_cty_tx$year))

sanity <- elect_he_cty_tx$repuvote + elect_he_cty_tx$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")
stopifnot(all(sanity >= 0 & sanity <= 1.001))

low_share <- elect_he_cty_tx %>% filter(demovote + repuvote < 0.5) %>% arrange(year)
message(nrow(low_share), " county-years with demovote+repuvote < 0.5:")
print(low_share)

## Cross-check against MEDSL for the one overlapping year (2016) -- same pattern as every other
## state in this push.
medsl_he <- readRDS(file.path(OUTPUT_DIR, "elect_he_cty_medsl.rds"))
tx_check <- elect_he_cty_tx %>%
  filter(year == 2016) %>%
  inner_join(medsl_he %>% filter(year == 2016), by = c("cty_fips", "year"), suffix = c("_tx", "_medsl")) %>%
  mutate(repuvote_diff = abs(repuvote_tx - repuvote_medsl))

message("TX vs MEDSL 2016 cross-check: ", nrow(tx_check), " counties matched, max repuvote diff = ",
        round(max(tx_check$repuvote_diff, na.rm = TRUE), 4), ", mean diff = ",
        round(mean(tx_check$repuvote_diff, na.rm = TRUE), 5))

n_counties_by_year <- elect_he_cty_tx %>% count(year, name = "n_counties")
print(n_counties_by_year)

## ---- Fold into the master panel (year < 2016 only -- 2016+ stays with MEDSL, per convention) ----
elect_cty_final <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")) %>%
  bind_rows(elect_he_cty_tx %>% filter(year < 2016) %>% select(-state)) %>%
  distinct(cty_fips, year, sample, .keep_all = TRUE) %>%
  arrange(cty_fips, year, sample) %>%
  save_step("elect_cty_final")

message("elect_cty_final.rds updated with TX 2000-2014. Total rows now: ", nrow(elect_cty_final))
