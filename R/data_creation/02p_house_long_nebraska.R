## Candidate-level LONG table for Nebraska (he_cty_ne; OpenElections 2008-2014 precinct files; logic of 01al: pseudo/write-in/over/under rows
## dropped, party_group from the raw party code via to_party). Acceptance: derived shares == elect_he_cty_ne.rds.
source(file.path("R", "00_setup.R")); library(readr); source(file.path("R", "long_helpers.R"))
save_step <- function(df, name) invisible(df)
RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "nebraska")
xw <- read_delim(file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))
ne_fips <- xw %>% filter(state == "NEBRASKA") %>% select(county_name, county_fips)
to_party <- function(x) { x <- toupper(trimws(x)); case_when(grepl("^DEM", x) ~ "DEM", grepl("^REP", x) ~ "REP", TRUE ~ "OTHER") }
is_pseudo_row <- function(candidate) { c <- toupper(trimws(candidate)); grepl("^TOTAL", c) | grepl("^WRITE-IN", c) | grepl("^SCATTERING", c) | grepl("^OVER VOTES", c) | grepl("^UNDER VOTES", c) }
read_year <- function(year, office_target)
  read_csv(file.path(RAW_DIR, paste0(year, "_general_precinct.csv")), show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    filter(toupper(trimws(office)) == toupper(office_target), !is_pseudo_row(candidate)) %>%
    transmute(county = toupper(trimws(county)), district, candidate = trimws(candidate), party_raw = trimws(party), party_group = to_party(party),
              votes = as.numeric(votes), year = year)
raw <- bind_rows(read_year(2008, "U.S. House"), read_year(2010, "Representative"), read_year(2012, "Representative"), read_year(2014, "Representative")) %>%
  filter(!is.na(votes)) %>% group_by(year, county, district, candidate, party_raw, party_group) %>% summarise(votes = sum(votes), .groups = "drop") %>%
  left_join(ne_fips, by = c("county" = "county_name")) %>% filter(!is.na(county_fips)) %>% rename(party = party_raw)
long <- finalize_long(raw, "ne"); save_long(long, "he_ne")
message("NE long rows: ", nrow(long)); check_long_vs_source(long, file.path(OUTPUT_DIR, "elect_he_cty_ne.rds"))
