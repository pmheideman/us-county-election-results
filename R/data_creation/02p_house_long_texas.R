## Candidate-level LONG table for Texas (he_cty_tx; OpenElections 2000-2016 county files, logic of 01r: per-(county,district) "Total"
## pseudo-row dropped, party "DEM"/"REP" -> groups, else OTHER; 2014 file is under counties/ with office "U.S. House").
## Acceptance: derived shares == elect_he_cty_tx.rds.
source(file.path("R", "00_setup.R")); library(readr); source(file.path("R", "long_helpers.R"))
save_step <- function(df, name) invisible(df)
RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "texas")
xw <- read_delim(file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))
tx_fips <- xw %>% filter(state == "TEXAS") %>% select(county_name, county_fips)
to_party <- function(x) case_when(x == "DEM" ~ "DEM", x == "REP" ~ "REP", TRUE ~ "OTHER")
rd <- function(year, office_val) read_csv(file.path(RAW_DIR, paste0(year, "_general_county.csv")), show_col_types = FALSE, col_types = cols(.default = "c")) %>%
  filter(office == office_val, candidate != "Total") %>%
  transmute(county = toupper(trimws(county)), district, candidate = trimws(candidate), party_raw = party, party_group = to_party(party), votes = as.numeric(votes), year = year) %>%
  filter(!is.na(votes))
raw <- bind_rows(lapply(c(2000, 2002, 2004, 2006, 2008, 2010, 2012, 2016), rd, office_val = "U. S. Representative"), rd(2014, "U.S. House")) %>%
  group_by(year, county, district, candidate, party_raw, party_group) %>% summarise(votes = sum(votes), .groups = "drop") %>%
  mutate(county = ifelse(county == "LASALLE", "LA SALLE", county)) %>%   # same alias as 01r (crosswalk spells it "LA SALLE")
  left_join(tx_fips, by = c("county" = "county_name")) %>% filter(!is.na(county_fips)) %>% rename(party = party_raw)
long <- finalize_long(raw, "tx"); save_long(long, "he_tx")
message("TX long rows: ", nrow(long)); check_long_vs_source(long, file.path(OUTPUT_DIR, "elect_he_cty_tx.rds"))
