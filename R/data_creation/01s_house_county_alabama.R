## *** 2012 rows removed from elect_he_cty_al.rds 2026-09-21 (R/assemble_panel.R's from-scratch reproducibility check): this OpenElections build's
## 2012 candidate totals differ from elect_he_cty_al_historical.rds's 2012 totals in 55 of 61 counties. The historical build is the one the FEC
## reconciliation confirms (all 7 districts within 0.5% of the certified totals) and the one the panel actually uses; this file's 2012 rows were pure,
## silently-conflicting duplicates that made a from-scratch rebuild ambiguous. The 2014 rows (not covered by al_historical) are untouched.
## Alabama: found via OpenElections (github.com/openelections/openelections-data-al). First state
## in the post-large-states sweep through the remaining ~35 states.
##
## AL's repo only has pre-2016 data for 2012 and 2014 (no 1990s/2000s coverage at all -- the repo
## starts there, a genuine archive limit, not a search failure). Both years are PRECINCT-level
## only (no top-level county-aggregated file exists for either year, unlike CA/TX) -- summed by
## county the same way as NC/VA/2014-Kansas.
##
## 2012's file has NO party column at all (confirmed: entirely blank). Only 15 distinct
## (district, candidate) pairs across the whole state that year -- small enough to hand-map to
## party directly from the file's own district/candidate list, rather than building a full
## Wikipedia-infobox lookup (the Kentucky/Ohio/Michigan-style fix for this same problem). Verified
## against known 2012 AL House delegation: R held 6 of 7 seats (AL-7/Sewell was the lone D seat),
## which matches the district-by-district winners here.
## Two literal-string variants of the same AL-4 and AL-6 candidates appear in the raw file
## ("DANIEL H. BOMAN" vs "DANIEL H. BOWMAN"; two different quote-casings of PENNY "Colonel"
## BAILEY) -- both variants mapped to the same party in the lookup table so they collapse
## correctly regardless of which spelling a given precinct row used.
##
## 2014's file DOES have a real party column, using BOTH long codes (DEM/REP) and short codes
## (D/R) interchangeably across different precinct sub-blocks for the same race (e.g. AL-2 has
## both "REP,Martha Roby" and "R,Roby" rows) -- both map to the same party bucket, no lookup
## needed this year.
##
## 2016 has a clean, already county-level top-level file (`20161108__al__general.csv`) with a
## `county == "Total"` pseudo-row (statewide total, same class of bug as every other state's
## pseudo-total row) -- built here ONLY as a cross-check against MEDSL, not folded into the panel
## (MEDSL already covers 2016+, per project convention).
##
## ** MEDSL cross-check for AL 2016 does NOT validate cleanly (mean diff 0.061, max 0.28) -- but
## the discrepancy traces to MEDSL's own AL numbers looking inflated, not to this script. AL's
## statewide House total from this OpenElections file (~1.89M votes, D+R+write-in) is plausible
## against AL's actual ~2.1M 2016 presidential turnout; MEDSL's AL House sample sums to ~3.04M
## statewide (e.g. Jefferson County alone shows 481K MEDSL vs a real ~305K presidential-year
## turnout) -- higher than presidential turnout, which is not plausible for a down-ballot race.
## Treated as a known MEDSL-AL-2016 data-quality issue, flagged for project memory, NOT fixed
## here (out of scope: this script's job is 2012/2014, and 2016 stays with MEDSL either way per
## convention -- this is a validation-only finding, not a blocker). **



source(file.path("R", "00_setup.R"))
library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "alabama")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

county_fips_crosswalk <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))

al_fips <- county_fips_crosswalk %>% filter(state == "ALABAMA") %>% select(county_name, county_fips)

download_oe <- function(remote_path, local_name) {
  dest <- file.path(RAW_DIR, local_name)
  if (!file.exists(dest) || file.size(dest) == 0) {
    url <- paste0("https://raw.githubusercontent.com/openelections/openelections-data-al/master/", remote_path)
    system2("curl", c("-sL", "-A", "'Mozilla/5.0'", "-o", shQuote(dest), shQuote(url)))
  }
  dest
}

## ---- 2012: precinct-level, no party column -- hand-mapped from the file's own 15 candidates ----
AL_2012_PARTY <- tribble(
  ~candidate,                  ~party,
  "JO BONNER",                 "REP",
  "MARTHA ROBY",                "REP",
  "THERESE FORD",               "DEM",
  "MIKE ROGERS",                "REP",
  "JOHN ANDREW HARRIS",         "DEM",
  "ROBERT ADERHOLT",            "REP",
  "DANIEL H. BOMAN",            "DEM",
  "DANIEL H. BOWMAN",           "DEM",
  "MO BROOKS",                  "REP",
  "CHARLIE L. HOLLEY",          "DEM",
  "SPENCER BACHUS",             "REP",
  "PENNY \"Colonel\" BAILEY",   "DEM",
  "PENNY \"COLONEL\" BAILEY",   "DEM",
  "TERRI A. SEWELL",            "DEM",
  "DON CHAMBERLAIN",            "REP"
)

read_al_2012 <- function() {
  path <- download_oe("2012/20121106__al__general__precinct.csv", "2012_general_precinct.csv")
  read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    filter(office == "U.S. House") %>%
    ## Every (county, district) block carries its own precinct literally named "Total" whose
    ## votes exactly equal the sum of that block's real precincts (confirmed: Jefferson
    ## district 6 -- Bailey 51217, Bachus 113933, both match the real-precinct sum exactly).
    ## Left in, this silently doubled totalvote for 61 of AL's 67 counties -- caught via a
    ## cross-check against a separately-sourced historical spreadsheet, whose 2012 totals ran
    ## almost exactly half of this file's uncorrected totals for the same counties.
    filter(!grepl("total|registered", precinct, ignore.case = TRUE)) %>%
    mutate(county = toupper(trimws(county)),
           ## Two literal typos in AL's own 2012 source data (confirmed: not a CSV-parsing
           ## artifact, the raw county field really is misspelled/truncated) -- recovers 2
           ## counties that would otherwise silently fail the crosswalk join below.
           county = case_when(county == "RADOLPH" ~ "RANDOLPH", county == "ST" ~ "ST. CLAIR", TRUE ~ county),
           votes = as.numeric(votes)) %>%
    filter(!is.na(votes)) %>%
    select(-party) %>%
    left_join(AL_2012_PARTY, by = "candidate") %>%
    mutate(party = ifelse(is.na(party), "OTHER", party)) %>%
    group_by(county, party) %>% summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
    mutate(year = 2012)
}

## ---- 2014: precinct-level, real party column (mixed long/short codes) ----
to_party_2014 <- function(x) {
  case_when(x %in% c("DEM", "D") ~ "DEM", x %in% c("REP", "R") ~ "REP", TRUE ~ "OTHER")
}

read_al_2014 <- function() {
  path <- download_oe("2014/20141104__al__general__precinct.csv", "2014_general_precinct.csv")
  read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    filter(office == "U.S. House") %>%
    ## Same pseudo-total-precinct issue as 2012, worse here: 15 counties carry a "Total",
    ## "Calculated totals", or "Reported totals" precinct row alongside their real precincts
    ## (Butler County has BOTH "Calculated totals" and "Reported totals" for the same district --
    ## would triple-count if neither were excluded). Confirmed real precinct rows exist
    ## independently for all 15 affected counties, so excluding these rows doesn't zero out any
    ## county's data.
    filter(!grepl("total|registered", precinct, ignore.case = TRUE)) %>%
    mutate(county = toupper(trimws(county)), party = to_party_2014(party), votes = as.numeric(votes)) %>%
    filter(!is.na(votes)) %>%
    group_by(county, party) %>% summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
    mutate(year = 2014)
}

al_by_county <- bind_rows(read_al_2012(), read_al_2014())

elect_he_cty_al <- al_by_county %>%
  group_by(year, county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(al_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(
    state = "ALABAMA", year, cty_fips = county_fips, sample = "HE",
    demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
  ) %>%
  save_step("elect_he_cty_al")

message("AL House county-level rows built: ", nrow(elect_he_cty_al), " (of possible ", 2 * 67, ")")
print(table(elect_he_cty_al$year))

sanity <- elect_he_cty_al$repuvote + elect_he_cty_al$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")
stopifnot(all(sanity >= 0 & sanity <= 1.001))

low_share <- elect_he_cty_al %>% filter(demovote + repuvote < 0.9) %>% arrange(year)
message(nrow(low_share), " county-years with demovote+repuvote < 0.9:")
print(low_share)

## ---- 2016 cross-check only (NOT folded into the panel -- MEDSL already covers 2016+) ----
read_al_2016_check <- function() {
  path <- download_oe("2016/20161108__al__general.csv", "2016_general_county.csv")
  read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    filter(office == "U.S. House", county != "Total") %>%
    mutate(county = toupper(trimws(county)),
           party = case_when(party == "D" ~ "DEM", party == "R" ~ "REP", TRUE ~ "OTHER"),
           votes = as.numeric(votes)) %>%
    filter(!is.na(votes)) %>%
    group_by(county, party) %>% summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
    group_by(county) %>%
    summarise(
      totalvote = sum(votes, na.rm = TRUE),
      demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
      repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(al_fips, by = c("county" = "county_name")) %>%
    filter(!is.na(county_fips), totalvote > 0) %>%
    transmute(cty_fips = county_fips, year = 2016,
              demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote)
}

al_2016_check <- read_al_2016_check()
medsl_he <- readRDS(file.path(OUTPUT_DIR, "elect_he_cty_medsl.rds"))
al_check <- al_2016_check %>%
  inner_join(medsl_he %>% filter(year == 2016), by = c("cty_fips", "year"), suffix = c("_al", "_medsl")) %>%
  mutate(repuvote_diff = abs(repuvote_al - repuvote_medsl))

message("AL 2016 cross-check (validation only, not folded in): ", nrow(al_check), " counties matched, max diff = ",
        round(max(al_check$repuvote_diff, na.rm = TRUE), 4), ", mean diff = ",
        round(mean(al_check$repuvote_diff, na.rm = TRUE), 5))

n_counties_by_year <- elect_he_cty_al %>% count(year, name = "n_counties")
print(n_counties_by_year)

## ---- Fold into the master panel (2012/2014 only) ----
## Filter out any prior AL/HE/2012-2014 rows before re-binding, same as every other state script
## -- distinct()'s keep-first behavior would otherwise silently keep stale rows on a re-run (this
## bit Colorado once already; here it would have kept the pre-fix, ~2x-inflated totalvote rows).
elect_cty_final <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")) %>%
  filter(!(cty_fips %in% al_fips$county_fips & sample == "HE" & year %in% elect_he_cty_al$year)) %>%
  bind_rows(elect_he_cty_al %>% select(-state)) %>%
  distinct(cty_fips, year, sample, .keep_all = TRUE) %>%
  arrange(cty_fips, year, sample) %>%
  save_step("elect_cty_final")

message("elect_cty_final.rds updated with AL 2012/2014. Total rows now: ", nrow(elect_cty_final))
