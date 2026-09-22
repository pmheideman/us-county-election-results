## Candidate-level LONG table for the Illinois House build (shares file elect_he_cty_il.rds, script 01p). Parsing and grouping (DEM/REP exact, else OTHER)
## copied from 01p; candidate/district kept. Output: R/output/long/he_il.rds
source(file.path("R", "00_setup.R")); library(readr); source(file.path("R", "long_helpers.R"))
save_step <- function(df, name) invisible(df)
RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "illinois")
il_fips <- read_delim(file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name))) %>% filter(state == "ILLINOIS") %>% select(county_name, county_fips)
PARTY_ALIAS <- c(DEM = "DEM", REP = "REP"); to_party <- function(x) coalesce(PARTY_ALIAS[trimws(x)], "OTHER")
read_il_year <- function(year) {
  read_csv(file.path(RAW_DIR, paste0(year, "_county.csv")), show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    filter(office == "U.S. House") %>%
    mutate(county = toupper(trimws(county)), party_group = unname(to_party(party)), votes = as.numeric(votes)) %>%
    filter(!is.na(votes)) %>%
    group_by(county, district, candidate, party, party_group) %>% summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>% mutate(year = year)
}
raw <- purrr::map_dfr(c(2008, 2010, 2012, 2014), read_il_year) %>% left_join(il_fips, by = c("county" = "county_name")) %>% filter(!is.na(county_fips))
long <- finalize_long(raw, "il"); save_long(long, "he_il")
res <- check_long_vs_source(long, file.path(OUTPUT_DIR, "elect_he_cty_il.rds")); print(res)
message("rows ", nrow(long), "; candidates ", nrow(distinct(long, year, district, candidate)), "; blank-party raw rows: ", sum(is.na(raw$party) | raw$party == ""))
