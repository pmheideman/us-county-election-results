## Candidate-level LONG table for the Ohio House build (shares file elect_he_cty_oh.rds, script 01l). Parsing and party grouping are
## copied from 01l unchanged; only the candidate/district columns are kept instead of collapsing to county x party.
## Output: R/output/long/he_oh.rds. Acceptance test: check_long_vs_source against elect_he_cty_oh.rds.
source(file.path("R", "00_setup.R")); library(readr); source(file.path("R", "long_helpers.R"))
save_step <- function(df, name) invisible(df)          # nothing else may be written

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "ohio")
oh_fips <- read_delim(file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name))) %>% filter(state == "OHIO") %>% select(county_name, county_fips)
PARTY_ALIAS <- c(R = "REP", REP = "REP", Republican = "REP", D = "DEM", DEM = "DEM", Democratic = "DEM")
to_party <- function(x) coalesce(PARTY_ALIAS[trimws(x)], "OTHER")

read_oh_year <- function(year) {
  raw <- read_csv(file.path(RAW_DIR, paste0(year, "_raw.csv")), show_col_types = FALSE, col_types = cols(.default = "c"))
  raw %>% filter(grepl("^U\\.?S\\.? ?(House|Representative)", office, ignore.case = TRUE)) %>%
    mutate(county = toupper(trimws(county)), party_group = unname(to_party(party)), votes = as.numeric(votes)) %>%
    filter(!is.na(votes)) %>%
    group_by(county, district, candidate, party, party_group) %>% summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
    mutate(year = year)
}
raw <- purrr::map_dfr(c(2000, 2002, 2006, 2008, 2012, 2014), read_oh_year) %>%
  left_join(oh_fips, by = c("county" = "county_name")) %>% filter(!is.na(county_fips))
long <- finalize_long(raw, "oh"); save_long(long, "he_oh")
res <- check_long_vs_source(long, file.path(OUTPUT_DIR, "elect_he_cty_oh.rds")); print(res)
message("rows ", nrow(long), "; candidates ", nrow(distinct(long, year, district, candidate)), "; blank-party rows in raw: ", sum(is.na(raw$party) | raw$party == ""))
