## Candidate-level LONG table for North Dakota (he_cty_nd; OpenElections 2000-2014 county files, 2000 party by candidate name; logic of 01ap).
## Acceptance: derived shares == elect_he_cty_nd.rds (2016 in 01ap is a MEDSL cross-check only, not part of the shares file).
source(file.path("R", "00_setup.R")); library(readr); source(file.path("R", "long_helpers.R"))
save_step <- function(df, name) invisible(df)
RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "north_dakota")
xw <- read_delim(file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))
nd_fips <- xw %>% filter(state == "NORTH DAKOTA") %>% select(county_name, county_fips)
to_party <- function(x) { x <- toupper(trimws(x)); case_when(grepl("^DEM", x) ~ "DEM", grepl("^REP", x) ~ "REP", TRUE ~ "OTHER") }
to_party_2000 <- function(candidate) case_when(candidate == "Earl Pomeroy" ~ "DEM", candidate == "John Dorso" ~ "REP", TRUE ~ "OTHER")
read_year <- function(y) {
  out <- read_csv(file.path(RAW_DIR, paste0(y, "_general_county.csv")), show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    filter(trimws(office) == "U.S. House") %>%
    transmute(county = toupper(trimws(county)), district, candidate = trimws(candidate), party_raw = trimws(party), votes = as.numeric(votes), year = as.numeric(y))
  out %>% mutate(party_group = if (y == "2000") to_party_2000(candidate) else to_party(party_raw))
}
raw <- bind_rows(lapply(as.character(c(2000, 2002, 2004, 2006, 2008, 2010, 2012, 2014)), read_year)) %>% filter(!is.na(votes)) %>%
  group_by(year, county, district, candidate, party_raw, party_group) %>% summarise(votes = sum(votes), .groups = "drop") %>%
  left_join(nd_fips, by = c("county" = "county_name")) %>% filter(!is.na(county_fips)) %>% rename(party = party_raw)
long <- finalize_long(raw, "nd"); save_long(long, "he_nd")
message("ND long rows: ", nrow(long)); check_long_vs_source(long, file.path(OUTPUT_DIR, "elect_he_cty_nd.rds"))
