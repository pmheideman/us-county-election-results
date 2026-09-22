## Candidate-level LONG table for Arkansas (he_cty_ar; OpenElections 2002-2014, logic of 01v: county-level rows only in 2002, precinct rows summed
## otherwise, party "Democrat"/"DEM" -> DEM, "Republican"/"REP" -> REP). Acceptance: derived shares == elect_he_cty_ar.rds.
source(file.path("R", "00_setup.R")); library(readr); library(stringr); source(file.path("R", "long_helpers.R"))
save_step <- function(df, name) invisible(df)
RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "arkansas")
xw <- read_delim(file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))
ar_fips <- xw %>% filter(state == "ARKANSAS") %>% select(county_name, county_fips)
strip_county_suffix <- function(x) toupper(trimws(sub(" COUNTY$", "", toupper(trimws(x)))))
to_party <- function(x) case_when(x %in% c("Democrat", "DEM") ~ "DEM", x %in% c("Republican", "REP") ~ "REP", TRUE ~ "OTHER")
fin <- function(d, year) d %>% filter(!is.na(votes)) %>% mutate(year = year)
r2002 <- read_csv(file.path(RAW_DIR, "2002_general.csv"), show_col_types = FALSE, col_types = cols(.default = "c")) %>%
  filter(str_starts(office, "U.S. Congress"), reporting_level == "county", candidate != "") %>%
  transmute(county = strip_county_suffix(jurisdiction), district = str_extract(office, "[0-9]+"), candidate = trimws(candidate), party_raw = party, party_group = to_party(party), votes = as.numeric(votes)) %>% fin(2002)
r2008 <- read_csv(file.path(RAW_DIR, "2008_general_precinct.csv"), show_col_types = FALSE, col_types = cols(.default = "c")) %>% filter(office == "U.S. House") %>%
  transmute(county = strip_county_suffix(county), district, candidate = trimws(candidate), party_raw = party, party_group = to_party(party), votes = as.numeric(votes)) %>% fin(2008)
r2010 <- { txt <- gsub("\r", "\n", readr::read_file(file.path(RAW_DIR, "2010_general.csv")), fixed = TRUE)
  read_csv(I(txt), show_col_types = FALSE, col_types = cols(.default = "c")) %>% filter(office == "U.S. House") %>%
  transmute(county = strip_county_suffix(county), district, candidate = trimws(candidate), party_raw = party, party_group = to_party(party), votes = as.numeric(votes)) %>% fin(2010) }
rp <- function(year) read_csv(file.path(RAW_DIR, paste0(year, "_general_precinct.csv")), show_col_types = FALSE, col_types = cols(.default = "c")) %>% filter(office == "U.S. House") %>%
  transmute(county = strip_county_suffix(county), district, candidate = trimws(candidate), party_raw = party, party_group = to_party(party), votes = as.numeric(votes)) %>% fin(year)
raw <- bind_rows(r2002, r2008, r2010, rp(2012), rp(2014)) %>%
  group_by(year, county, district, candidate, party_raw, party_group) %>% summarise(votes = sum(votes), .groups = "drop") %>%
  left_join(ar_fips, by = c("county" = "county_name")) %>% filter(!is.na(county_fips)) %>% rename(party = party_raw)
long <- finalize_long(raw, "ar"); save_long(long, "he_ar")
message("AR long rows: ", nrow(long)); check_long_vs_source(long, file.path(OUTPUT_DIR, "elect_he_cty_ar.rds"))
