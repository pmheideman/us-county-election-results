## Pennsylvania: found via OpenElections (github.com/openelections/openelections-data-pa), a
## nonpartisan volunteer archive of official county-canvass-sourced results, surveyed across the
## whole OpenElections org as a followup to the Kansas work (01j) to see what else it could fill.
## PA turned out to be one of the easiest states in that survey.
##
## Two distinct formats across this span, both handled here:
## 1. 2000, 2002, 2004, 2006, 2008, 2010, 2014: OpenElections' own cleaned, already-county-level
##    file (one row per county x office x district x party x candidate, columns
##    `county,office,district,party,candidate,votes`) -- same shape as Kansas's 2012 file, no
##    pseudo-total row this time (checked and confirmed absent), office consistently labeled
##    "U.S. House" and party as clean 3-4 letter codes (DEM/REP plus various minor-party codes)
##    across every one of these 7 years.
## 2. 2012: no cleaned file exists yet in the repo (only the raw statewide precinct-level export),
##    but that raw format is fully documented in the repo's own README.md -- a fixed 33-column
##    layout from PA's SURE elections system, comma-separated (real CSV quoting, e.g. a candidate
##    name field like "CASEY, JR" -- naive comma-splitting misparses these rows, but readr's
##    read_csv handles real CSV quoting correctly). Column 9 is a clean 3-letter office code
##    ("USC" = "Representative in Congress" = US House) and column 28 is literally the county's
##    3-digit FIPS suffix directly (verified: county code 51 = Philadelphia in the documented
##    county-code table has fips_code "101", and Philadelphia County PA's real FIPS is 42101) --
##    no name-based crosswalk join needed for this year at all, just `42000 + as.integer(fips_code)`
##    (fips_code appears as both "003" and "3" for the same county in a handful of rows --
##    as.integer() normalizes this fine).
##
## PA's own delegation size shrank across this span (21 districts in 2000, 19 in 2002-2010, 18
## from 2012 on, after the standard post-census redistricting) -- expected, not a bug; confirmed
## every year still has all 67 counties represented in the U.S. House rows regardless.
##
## Also expected: repuvote+demovote == 0 for a cluster of ~10 rural counties in 2002 specifically
## (Bradford, Montour, Northumberland, Pike, Snyder, Sullivan, Susquehanna, Union, Wayne, Wyoming --
## all PA-10). PA used to allow candidates to cross-file and appear on both major parties' ballot
## lines; in 2002 Don Sherwood won PA-10 essentially unopposed and the source file records his
## county totals there under party code "DEMREP" rather than "REP" -- a single fused vote total
## that can't be honestly split between the two parties, so (consistent with every other minor-
## party/write-in code already excluded from demovote/repuvote elsewhere in this project) it's
## left out of both, giving these counties demovote=repuvote=0 for that one year. totalvote is
## unaffected (it sums every party code, fusion included) -- only the two-party SHARE columns are
## incomplete for this small, understood, one-year, one-district cluster.

source(file.path("R", "00_setup.R"))
library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "pennsylvania")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

county_fips_crosswalk <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))

pa_fips <- county_fips_crosswalk %>% filter(state == "PENNSYLVANIA") %>% select(county_name, county_fips)

download_oe <- function(year, remote_name, local_name) {
  dest <- file.path(RAW_DIR, local_name)
  if (!file.exists(dest) || file.size(dest) == 0) {
    url <- paste0("https://raw.githubusercontent.com/openelections/openelections-data-pa/master/",
                   year, "/", remote_name)
    system2("curl", c("-sL", "-A", "'Mozilla/5.0'", "-o", shQuote(dest), shQuote(url)))
  }
  dest
}

## ---- 2000-2010 + 2014: already-cleaned county-level files ----
PA_CLEAN_YEARS <- tribble(
  ~year, ~date_str,
  2000,  "20001107",
  2002,  "20021105",
  2004,  "20041102",
  2006,  "20061107",
  2008,  "20081104",
  2010,  "20101102",
  2014,  "20141104"
)

read_pa_clean_year <- function(year, date_str) {
  path <- download_oe(year, paste0(date_str, "__pa__general__county.csv"), paste0(year, "_county.csv"))
  read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    filter(office == "U.S. House") %>%
    mutate(county = toupper(trimws(county)), party = trimws(party), votes = as.numeric(votes)) %>%
    filter(!is.na(votes)) %>%
    group_by(county, party) %>% summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
    mutate(year = year)
}

pa_clean_by_county <- pmap_dfr(PA_CLEAN_YEARS, read_pa_clean_year)

elect_he_cty_pa_clean <- pa_clean_by_county %>%
  group_by(year, county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(pa_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(
    year, cty_fips = county_fips, sample = "HE",
    demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
  )

message("PA clean-format years built: ", nrow(elect_he_cty_pa_clean), " (of possible ", 7 * 67, ")")

## ---- 2012: raw SURE-system precinct export, documented 33-column layout ----
PA_2012_COLS <- c("year_f","election_type","county_code","precinct_code","office_rank","district",
                   "party_rank","ballot_position","office_code","party_code","candidate_number",
                   "last_name","first_name","middle_name","suffix","votes","us_cd","state_sd",
                   "state_hd","muni_type","muni_name","muni_bd_code1","muni_bd_name1",
                   "muni_bd_code2","muni_bd_name2","bicounty_code","mcd_code","fips_code","vtd_code",
                   "prev_precinct","prev_us_cd","prev_state_sd","prev_state_hd")

pa_2012_path <- download_oe(2012, "20121106__pa__general__precinct.csv", "2012_precinct.csv")
pa_2012_raw <- read_csv(pa_2012_path, col_names = PA_2012_COLS, col_types = cols(.default = "c"),
                         show_col_types = FALSE) %>%
  filter(office_code == "USC") %>%
  mutate(votes = as.numeric(votes), cty_fips = 42000 + as.integer(fips_code)) %>%
  filter(!is.na(votes))

elect_he_cty_pa_2012 <- pa_2012_raw %>%
  group_by(cty_fips, party_code) %>% summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
  group_by(cty_fips) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party_code == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party_code == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(totalvote > 0) %>%
  transmute(year = 2012, cty_fips, sample = "HE",
            demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote)

message("PA 2012 built: ", nrow(elect_he_cty_pa_2012), " (of possible 67)")

elect_he_cty_pa <- bind_rows(elect_he_cty_pa_clean, elect_he_cty_pa_2012) %>%
  mutate(state = "PENNSYLVANIA") %>%
  save_step("elect_he_cty_pa")

message("PA House county-level rows built: ", nrow(elect_he_cty_pa), " across years: ",
        paste(sort(unique(elect_he_cty_pa$year)), collapse = ", "))
print(table(elect_he_cty_pa$year))

## ---- Sanity checks ----
sanity <- elect_he_cty_pa$repuvote + elect_he_cty_pa$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")

## Known 2012 PA-12 result (Rothfus R defeated incumbent Critz D) as an external spot check --
## Allegheny + Beaver + a handful of other counties split this district, so check the statewide
## aggregate two-party lean direction is at least plausible instead of an exact county match.
pa_2012_by_district <- pa_2012_raw %>% group_by(district, party_code) %>%
  summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop")
message("PA 2012 district totals (spot check a few known races):")
print(pa_2012_by_district %>% filter(district %in% c("12", "4"), party_code %in% c("DEM", "REP")))

## ---- Fold into the master panel ----
elect_cty_final <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")) %>%
  bind_rows(elect_he_cty_pa %>% select(-state)) %>%
  distinct(cty_fips, year, sample, .keep_all = TRUE) %>%
  arrange(cty_fips, year, sample) %>%
  save_step("elect_cty_final")

message("elect_cty_final.rds updated with PA 2000-2014 (odd redistricting years and 2012 raw ",
        "precinct format handled separately). Total rows now: ", nrow(elect_cty_final))
