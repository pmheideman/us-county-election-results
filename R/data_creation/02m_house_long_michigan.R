## Candidate-level LONG table for the Michigan House build (shares file elect_he_cty_mi.rds, script 01m). Parsing, the GD. TRAVERSE alias and the
## exact-match DEM/REP grouping are copied from 01m; candidate/district are kept. Output: R/output/long/he_mi.rds
## 2026-09-22: 1998/2000/2002/2004/2006 added, same as 01m -- party for these years comes from the
## Clerk-of-the-House lookup (by year/district/last-name), not the source file's own (blank) party column.
source(file.path("R", "00_setup.R")); library(readr); source(file.path("R", "long_helpers.R"))
save_step <- function(df, name) invisible(df)
RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "michigan")
mi_fips <- read_delim(file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name))) %>% filter(state == "MICHIGAN") %>% select(county_name, county_fips)
COUNTY_ALIASES <- c("GD. TRAVERSE" = "GRAND TRAVERSE")
last_name_of <- function(x) {
  toks <- strsplit(trimws(x), "\\s+")
  toupper(gsub("[^A-Za-z]", "", vapply(toks, function(t) if (length(t) == 0) "" else tail(t, 1), character(1))))
}
clerk_lookup <- read_csv(file.path(PROJECT_ROOT, "R", "data", "clerk_house_stats", "clerk_house_party_lookup.csv"), show_col_types = FALSE) %>%
  filter(state == "MICHIGAN") %>% transmute(year, district = as.integer(district), last_name = last_name_of(candidate), party)
read_mi_year <- function(year) {
  read_csv(file.path(RAW_DIR, paste0(year, "_precinct.csv")), show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    filter(office == "U.S. House") %>%
    mutate(county = toupper(trimws(county)), county = coalesce(COUNTY_ALIASES[county], county), party = trimws(party), votes = as.numeric(votes)) %>%
    filter(!is.na(votes)) %>%
    mutate(party_group = case_when(party == "DEM" ~ "DEM", party == "REP" ~ "REP", TRUE ~ "OTHER")) %>%
    group_by(county, district, candidate, party, party_group) %>% summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>% mutate(year = year)
}
read_mi_year_no_party <- function(year) {
  read_csv(file.path(RAW_DIR, paste0(year, "_precinct.csv")), show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    filter(office == "U.S. House") %>%
    select(-party) %>%
    mutate(county = toupper(trimws(county)), county = coalesce(COUNTY_ALIASES[county], county),
           district = as.integer(district), last_name = last_name_of(candidate), votes = as.numeric(votes), year = year) %>%
    filter(!is.na(votes)) %>%
    left_join(clerk_lookup, by = c("year", "district", "last_name")) %>%
    mutate(party = coalesce(party, "OTHER"), party_group = party, district = as.character(district)) %>%
    group_by(county, district, candidate, party, party_group) %>% summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>% mutate(year = year)
}
raw <- bind_rows(
  purrr::map_dfr(c(2008, 2010, 2012, 2014), read_mi_year),
  purrr::map_dfr(c(1998, 2000, 2002, 2004, 2006), read_mi_year_no_party)
) %>% left_join(mi_fips, by = c("county" = "county_name")) %>% filter(!is.na(county_fips))
long <- finalize_long(raw, "mi"); save_long(long, "he_mi")
res <- check_long_vs_source(long, file.path(OUTPUT_DIR, "elect_he_cty_mi.rds")); print(res)
message("rows ", nrow(long), "; candidates ", nrow(distinct(long, year, district, candidate)), "; blank-party raw rows: ", sum(is.na(raw$party) | raw$party == ""))
