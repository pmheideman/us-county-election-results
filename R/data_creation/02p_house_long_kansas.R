## Candidate-level LONG table for Kansas (source he_cty_ks: OpenElections 2012 county file + 2014 precinct file; see 01j).
## Same filters and party grouping as 01j (party "Democratic" -> DEM, "Republican" -> REP, else OTHER; 2012 TOTALS pseudo-row dropped;
## counties matched by upper-cased name to the crosswalk, unmatched dropped). Acceptance: shares derived from this table == elect_he_cty_ks.rds.
source(file.path("R", "00_setup.R")); library(readr); source(file.path("R", "long_helpers.R"))
save_step <- function(df, name) invisible(df)            # SAFETY: this script writes only under R/output/long/

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "kansas")
xw <- read_delim(file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))
ks_fips <- xw %>% filter(state == "KANSAS") %>% select(county_name, county_fips)

rd <- function(f, year) read_csv(file.path(RAW_DIR, f), show_col_types = FALSE, col_types = cols(.default = "c")) %>%
  filter(county != "TOTALS", office == "U.S. House") %>%
  mutate(county = toupper(trimws(county)), party = trimws(party), votes = as.numeric(votes), year = year) %>% filter(!is.na(votes))
raw <- bind_rows(rd("2012_county.csv", 2012), rd("2014_precinct.csv", 2014)) %>%
  group_by(year, county, district, candidate, party) %>% summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
  left_join(ks_fips, by = c("county" = "county_name")) %>% filter(!is.na(county_fips)) %>%
  mutate(party_group = case_when(party == "Democratic" ~ "DEM", party == "Republican" ~ "REP", TRUE ~ "OTHER"))
long <- finalize_long(raw, "ks"); save_long(long, "he_ks")
message("KS long rows: ", nrow(long)); check_long_vs_source(long, file.path(OUTPUT_DIR, "elect_he_cty_ks.rds"))
