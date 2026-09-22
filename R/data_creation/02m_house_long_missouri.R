## Candidate-level LONG table for the Missouri House build (shares file elect_he_cty_mo.rds, script 01aj). File list (2002 is a precinct file), the
## ST. LOUIS -> ST LOUIS COUNTY normalization, the Kansas City exclusion, the 115-county crosswalk (Kansas City fips dropped) and the DEM/REP prefix
## grouping copied from 01aj; candidate/district kept. Output: R/output/long/he_mo.rds
source(file.path("R", "00_setup.R")); library(readr); source(file.path("R", "long_helpers.R"))
save_step <- function(df, name) invisible(df)
RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "missouri")
mo_fips <- read_delim(file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name))) %>%
  filter(state == "MISSOURI", !county_fips %in% c(36000, 2938000)) %>% select(county_name, county_fips) %>% distinct()
stopifnot(length(unique(mo_fips$county_fips)) == 115)
to_party <- function(x) { x <- toupper(trimws(x)); case_when(grepl("^DEM", x) ~ "DEM", grepl("^REP", x) ~ "REP", TRUE ~ "OTHER") }
normalize_county <- function(x) gsub("^ST\\.?\\s+LOUIS$", "ST LOUIS COUNTY", toupper(trimws(x)))
read_year <- function(year, local) {
  raw <- read_csv(file.path(RAW_DIR, local), show_col_types = FALSE, col_types = cols(.default = "c"))
  if (!"district" %in% names(raw)) raw$district <- NA_character_
  raw %>% filter(office == "U.S. House") %>%
    transmute(county = normalize_county(county), district, candidate = trimws(candidate), party_raw = party, party_group = to_party(party), votes = as.numeric(votes), year = year)
}
all_rows <- bind_rows(read_year(2000, "2000_general.csv"), read_year(2002, "2002_general_precinct.csv"), read_year(2004, "2004_general.csv"),
                      read_year(2006, "2006_general.csv"), read_year(2008, "2008_general.csv"), read_year(2010, "2010_general.csv"),
                      read_year(2012, "2012_general.csv"), read_year(2014, "2014_general.csv")) %>%
  filter(!is.na(votes), toupper(trimws(county)) != "KANSAS CITY")
raw <- all_rows %>% group_by(year, county, district, candidate, party_raw, party_group) %>% summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
  rename(party = party_raw) %>% left_join(mo_fips, by = c("county" = "county_name")) %>% filter(!is.na(county_fips))
long <- finalize_long(raw, "mo"); save_long(long, "he_mo")
res <- check_long_vs_source(long, file.path(OUTPUT_DIR, "elect_he_cty_mo.rds")); print(res)
message("rows ", nrow(long), "; candidates ", nrow(distinct(long, year, district, candidate)), "; blank-party raw rows: ", sum(is.na(raw$party) | raw$party == ""), "; districts present: ", sum(!is.na(long$district)), " of ", nrow(long), " rows")
