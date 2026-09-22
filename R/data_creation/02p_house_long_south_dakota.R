## Candidate-level LONG tables for South Dakota: he_cty_sd (OpenElections 2014, logic of 01au) and he_cty_sd_sos (SOS archive 1990-2012,
## from sd_sos_house_long.rds built by 01cd; party "R"/"D"/"O" -> group). Acceptance: shares derived == each shares file.
source(file.path("R", "00_setup.R")); library(readr); source(file.path("R", "long_helpers.R"))
save_step <- function(df, name) invisible(df)
xw <- read_delim(file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))
sd_fips <- xw %>% filter(state == "SOUTH DAKOTA") %>% select(county_name, county_fips)
to_party <- function(x) { x <- toupper(trimws(x)); case_when(grepl("^DEM|^D$", x) ~ "DEM", grepl("^REP|^R$", x) ~ "REP", TRUE ~ "OTHER") }

## --- he_cty_sd: OpenElections 2014
raw <- read_csv(file.path(PROJECT_ROOT, "R/data/raw_house_county_open_states/south_dakota/2014_general_county.csv"), show_col_types = FALSE, col_types = cols(.default = "c")) %>%
  filter(trimws(office) == "U.S. House") %>%
  transmute(county = toupper(trimws(county)), district, candidate = trimws(candidate), party_raw = trimws(party), party_group = to_party(party), votes = as.numeric(votes), year = 2014) %>%
  filter(!is.na(votes)) %>% group_by(year, county, district, candidate, party_raw, party_group) %>% summarise(votes = sum(votes), .groups = "drop") %>%
  left_join(sd_fips, by = c("county" = "county_name")) %>% filter(!is.na(county_fips)) %>% rename(party = party_raw)
long_a <- finalize_long(raw, "sd"); save_long(long_a, "he_sd")
message("SD (OE 2014) long rows: ", nrow(long_a)); res_a <- check_long_vs_source(long_a, file.path(OUTPUT_DIR, "elect_he_cty_sd.rds"))

## --- he_cty_sd_sos: SOS archive 1990-2012 (at-large seat -> district "00")
s <- readRDS(file.path(OUTPUT_DIR, "sd_sos_house_long.rds")) %>%
  mutate(party_group = unname(c(R = "REP", D = "DEM", O = "OTHER")[party]), party = unname(c(R = "Republican", D = "Democratic", O = "Other")[party]),
         votes = as.numeric(votes)) %>%
  transmute(year, county_fips, district = "00", candidate, party, party_group, votes)
long_b <- finalize_long(s, "sd_sos"); save_long(long_b, "he_sd_sos")
message("SD (SOS) long rows: ", nrow(long_b)); res_b <- check_long_vs_source(long_b, file.path(OUTPUT_DIR, "elect_he_cty_sd_sos.rds"))
