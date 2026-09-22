## Candidate-level LONG table for the Michigan House build (shares file elect_he_cty_mi.rds, script 01m). Parsing, the GD. TRAVERSE alias and the
## exact-match DEM/REP grouping are copied from 01m; candidate/district are kept. Output: R/output/long/he_mi.rds
source(file.path("R", "00_setup.R")); library(readr); source(file.path("R", "long_helpers.R"))
save_step <- function(df, name) invisible(df)
RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "michigan")
mi_fips <- read_delim(file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name))) %>% filter(state == "MICHIGAN") %>% select(county_name, county_fips)
COUNTY_ALIASES <- c("GD. TRAVERSE" = "GRAND TRAVERSE")
read_mi_year <- function(year) {
  read_csv(file.path(RAW_DIR, paste0(year, "_precinct.csv")), show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    filter(office == "U.S. House") %>%
    mutate(county = toupper(trimws(county)), county = coalesce(COUNTY_ALIASES[county], county), party = trimws(party), votes = as.numeric(votes)) %>%
    filter(!is.na(votes)) %>%
    mutate(party_group = case_when(party == "DEM" ~ "DEM", party == "REP" ~ "REP", TRUE ~ "OTHER")) %>%
    group_by(county, district, candidate, party, party_group) %>% summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>% mutate(year = year)
}
raw <- purrr::map_dfr(c(2008, 2010, 2012, 2014), read_mi_year) %>% left_join(mi_fips, by = c("county" = "county_name")) %>% filter(!is.na(county_fips))
long <- finalize_long(raw, "mi"); save_long(long, "he_mi")
res <- check_long_vs_source(long, file.path(OUTPUT_DIR, "elect_he_cty_mi.rds")); print(res)
message("rows ", nrow(long), "; candidates ", nrow(distinct(long, year, district, candidate)), "; blank-party raw rows: ", sum(is.na(raw$party) | raw$party == ""))
