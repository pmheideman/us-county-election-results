## Kansas: sos.ks.gov's own "General Election Official Vote Totals" PDFs (checked back to 2004,
## the earliest posted) give ONLY statewide-per-district totals -- no county breakdown at all,
## confirmed by direct inspection (e.g. the 2008 PDF's "United States House of Representatives
## 001" section is 4 rows, one per candidate, nothing else). This matches the user's own
## assessment that the SOS site is useless for county-level historical results. Unlike Florida's
## "obvious county endpoint is a trap" gotcha, there wasn't a hidden better endpoint here -- these
## really are the only vote totals SOS publishes online for these years.
##
## OpenElections (github.com/openelections/openelections-data-ks) fills 2012 and 2014 with real,
## county-level, office/district/party/votes rows sourced from the official county canvasses --
## no precinct aggregation needed for 2012 (already a per-county file), light aggregation for 2014
## (a statewide precinct-level file with a `county` column, county = sum across its precincts,
## same pattern as NC/VA in 01d). The OE repo starts at 2012 and 2016+ is already covered by
## MEDSL, so this closes exactly the two addressable pre-MEDSL years; 1992-2010 remains an open
## gap (SOS has nothing county-level online for those years either, and OpenElections doesn't
## reach back that far) -- flagged in the coverage tracker rather than attempted here.
##
## Two gotchas hit and fixed:
## 1. The 2012 county.csv carries an extra `county == "TOTALS"` pseudo-row alongside the 105 real
##    counties (same idea as Georgia's "Total Votes" row in 01d) -- naively summing every row
##    exactly DOUBLES every district's vote total (confirmed: got 2x the real number, e.g.
##    district 1's Republican total came out to 422,674 instead of the correct 211,337, until
##    this row was excluded). Always exclude it before grouping.
## 2. The 2014 statewide precinct file mixes county-name casing (both "WASHINGTON" and
##    "Washington" appear as literal distinct strings) and has a handful of corrupted rows where
##    an embedded repeated header block reads county="COUNTY", office="Secretary of State" or
##    "State Treasurer", votes="Write-ins"/some name (non-numeric) -- harmless here since these
##    happen to fall under offices other than "U.S. House", but toupper() the county column
##    regardless and coerce votes with as.numeric() (which will just NA out any further such junk)
##    rather than assume no such row could ever land on the U.S. House rows in some other year.

source(file.path("R", "00_setup.R"))
library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "kansas")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

county_fips_crosswalk <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))

ks_fips <- county_fips_crosswalk %>% filter(state == "KANSAS") %>% select(county_name, county_fips)

download_oe <- function(year, remote_name, local_name) {
  dest <- file.path(RAW_DIR, local_name)
  if (!file.exists(dest)) {
    url <- paste0("https://raw.githubusercontent.com/openelections/openelections-data-ks/master/",
                   year, "/", remote_name)
    system2("curl", c("-sL", "-A", "'Mozilla/5.0'", "-o", shQuote(dest), shQuote(url)))
  }
  dest
}

## ---- 2012: already a single county-level file (one row per county x office x district x party x candidate) ----
ks_2012_path <- download_oe(2012, "20121106__ks__general__county.csv", "2012_county.csv")
ks_2012_raw <- read_csv(ks_2012_path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
  filter(county != "TOTALS", office == "U.S. House") %>%
  mutate(county = toupper(trimws(county)), party = trimws(party), votes = as.numeric(votes)) %>%
  filter(!is.na(votes))

## ---- 2014: statewide precinct-level file with a `county` column -- sum across precincts ----
ks_2014_path <- download_oe(2014, "20141104__ks__general__precinct.csv", "2014_precinct.csv")
ks_2014_raw <- read_csv(ks_2014_path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
  filter(office == "U.S. House") %>%
  mutate(county = toupper(trimws(county)), party = trimws(party), votes = as.numeric(votes)) %>%
  filter(!is.na(votes)) %>%
  group_by(county, party) %>% summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop")

ks_2012_by_county <- ks_2012_raw %>% group_by(county, party) %>%
  summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>% mutate(year = 2012)
ks_2014_by_county <- ks_2014_raw %>% mutate(year = 2014)

ks_by_county <- bind_rows(ks_2012_by_county, ks_2014_by_county)

elect_he_cty_ks <- ks_by_county %>%
  group_by(year, county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "Democratic"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "Republican"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(ks_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(
    state = "KANSAS", year, cty_fips = county_fips, sample = "HE",
    demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
  ) %>%
  save_step("elect_he_cty_ks")

message("KS House county-level rows built: ", nrow(elect_he_cty_ks), " (of possible ", 2 * 105, ")")
print(table(elect_he_cty_ks$year))

## ---- Sanity check: no county's own two-party share should exceed 1, and check against the ----
## 2012 file's own printed "TOTALS" pseudo-row as an independent internal cross-check (not MEDSL,
## which doesn't reach back this far) -- confirms the TOTALS-row-exclusion fix above is correct.
sanity <- elect_he_cty_ks$repuvote + elect_he_cty_ks$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")

totals_check <- ks_2012_raw %>% filter(county == "TOTALS") ## should be empty -- already excluded above
stopifnot(nrow(totals_check) == 0)

official_totals_2012 <- read_csv(ks_2012_path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
  filter(county == "TOTALS", office == "U.S. House") %>%
  mutate(votes = as.numeric(votes)) %>%
  group_by(party) %>% summarise(votes = sum(votes), .groups = "drop")
computed_totals_2012 <- ks_2012_raw %>% group_by(party) %>% summarise(votes = sum(votes), .groups = "drop")
check_2012 <- official_totals_2012 %>% inner_join(computed_totals_2012, by = "party", suffix = c("_official", "_computed"))
message("2012 computed-from-counties vs official TOTALS row: max diff = ",
        max(abs(check_2012$votes_official - check_2012$votes_computed)))

## ---- Fold into the master panel ----
elect_cty_final <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")) %>%
  bind_rows(elect_he_cty_ks %>% select(-state)) %>%
  distinct(cty_fips, year, sample, .keep_all = TRUE) %>%
  arrange(cty_fips, year, sample) %>%
  save_step("elect_cty_final")

message("elect_cty_final.rds updated with KS 2012/2014. Total rows now: ", nrow(elect_cty_final))
message("Kansas 1992-2010 remains an open gap: SOS's own PDFs (checked back to 2004) give only ",
        "statewide per-district totals, no county breakdown; OpenElections doesn't reach back ",
        "before 2012. Not attempted further here -- would need the Kansas Historical Society's ",
        "microfilmed county canvass books or a similar offline source.")
