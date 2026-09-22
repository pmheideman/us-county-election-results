## Candidate-level LONG table for the Indiana House build (shares file elect_he_cty_in.rds, script 01ac). Parsing, county aliases (SAINT JOSEPH, LAPORT)
## and the DEMOCRATIC/REPUBLICAN grouping copied from 01ac; candidate/district kept. Output: R/output/long/he_in.rds
## NOTE: for 2002-2010 the source has 26 counties with votes = 0 (structural gap); those rows carry candidate names but 0 votes and are kept (0-vote rows
## do not create county-year keys because derive_shares drops totalvote == 0, exactly as the build did).
source(file.path("R", "00_setup.R")); library(readr); source(file.path("R", "long_helpers.R"))
save_step <- function(df, name) invisible(df)
RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "indiana")
in_fips <- read_delim(file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name))) %>% filter(state == "INDIANA") %>% select(county_name, county_fips)
PARTY_ALIAS <- c(DEMOCRATIC = "DEM", REPUBLICAN = "REP"); to_party <- function(x) coalesce(PARTY_ALIAS[toupper(trimws(x))], "OTHER")
COUNTY_ALIAS <- c("SAINT JOSEPH" = "ST. JOSEPH", "LAPORT" = "LAPORTE"); to_county <- function(x) coalesce(COUNTY_ALIAS[x], x)
read_in_year <- function(year) {
  read_csv(file.path(RAW_DIR, paste0(year, "_county.csv")), show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    filter(office == "U.S. House") %>%
    mutate(county = to_county(toupper(trimws(county))), party_group = unname(to_party(party)), votes = as.numeric(votes)) %>%
    filter(!is.na(votes)) %>%
    group_by(county, district, candidate, party, party_group) %>% summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>% mutate(year = year)
}
raw <- purrr::map_dfr(c(2002, 2004, 2006, 2008, 2010, 2012, 2014), read_in_year) %>% left_join(in_fips, by = c("county" = "county_name")) %>% filter(!is.na(county_fips))
long <- finalize_long(raw, "in"); save_long(long, "he_in")
res <- check_long_vs_source(long, file.path(OUTPUT_DIR, "elect_he_cty_in.rds")); print(res)
message("rows ", nrow(long), "; zero-vote rows: ", sum(long$votes == 0), "; candidates ", nrow(distinct(long, year, district, candidate)), "; blank-party raw rows: ", sum(is.na(raw$party) | raw$party == ""))
