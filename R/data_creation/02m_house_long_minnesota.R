## Candidate-level LONG table for the Minnesota House build (shares file elect_he_cty_mn.rds, script 01ah). County cleaning (periods removed), the
## DFL -> DEM / R -> REP exact grouping and the 87-county crosswalk (SAINT LOUIS excluded) copied from 01ah; candidate/district kept.
## Output: R/output/long/he_mn.rds
source(file.path("R", "00_setup.R")); library(readr); source(file.path("R", "long_helpers.R"))
save_step <- function(df, name) invisible(df)
RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "minnesota")
mn_fips <- read_delim(file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name))) %>% filter(state == "MINNESOTA", county_name != "SAINT LOUIS") %>% select(county_name, county_fips)
stopifnot(nrow(mn_fips) == 87)
clean_county <- function(x) gsub("\\.", "", toupper(trimws(x)))
to_party <- function(x) { x <- toupper(trimws(x)); case_when(x == "DFL" ~ "DEM", x == "R" ~ "REP", TRUE ~ "OTHER") }
read_mn <- function(year) {
  raw <- read_csv(file.path(RAW_DIR, paste0(year, "_general_county.csv")), show_col_types = FALSE, col_types = cols(.default = "c"))
  if (!"district" %in% names(raw)) raw$district <- NA_character_
  raw %>% filter(toupper(trimws(office)) == "U.S. HOUSE") %>%
    transmute(county = clean_county(county), district, candidate = trimws(candidate), party_raw = party, party_group = to_party(party), votes = as.numeric(votes), year = year)
}
raw <- bind_rows(read_mn(2012), read_mn(2014)) %>% filter(!is.na(votes)) %>%
  group_by(year, county, district, candidate, party_raw, party_group) %>% summarise(votes = sum(votes), .groups = "drop") %>% rename(party = party_raw) %>%
  left_join(mn_fips, by = c("county" = "county_name")) %>% filter(!is.na(county_fips))
long <- finalize_long(raw, "mn"); save_long(long, "he_mn")
res <- check_long_vs_source(long, file.path(OUTPUT_DIR, "elect_he_cty_mn.rds")); print(res)
message("rows ", nrow(long), "; candidates ", nrow(distinct(long, year, district, candidate)), "; blank-party raw rows: ", sum(is.na(raw$party) | raw$party == ""))
