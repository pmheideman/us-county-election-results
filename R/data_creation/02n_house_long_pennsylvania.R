## Candidate-level LONG table for Pennsylvania U.S. House (source: elect_he_cty_pa.rds, built by 01k). Same parsing and grouping as 01k:
## clean county files 2000-2010 + 2014 (party codes exactly "DEM"/"REP" count as major parties; fused rows such as DEMREP stay OTHER),
## 2012 from the raw SURE precinct export (office USC, county = 42000 + fips_code).
source(file.path("R", "00_setup.R")); library(readr); source(file.path("R", "long_helpers.R"))
save_step <- function(df, name) invisible(df)
RAW <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "pennsylvania")
pa_fips <- read_delim(file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name))) %>% filter(state == "PENNSYLVANIA") %>% select(county_name, county_fips)
grp <- function(p) ifelse(p == "DEM", "DEM", ifelse(p == "REP", "REP", "OTHER"))

yrs <- c(2000, 2002, 2004, 2006, 2008, 2010, 2014)
clean <- purrr::map_dfr(yrs, function(y) read_csv(file.path(RAW, paste0(y, "_county.csv")), show_col_types = FALSE, col_types = cols(.default = "c")) %>%
  filter(office == "U.S. House") %>% mutate(county = toupper(trimws(county)), party = trimws(party), votes = as.numeric(votes)) %>%
  filter(!is.na(votes)) %>% transmute(year = y, county, district, candidate, party, votes)) %>%
  inner_join(pa_fips, by = c("county" = "county_name"))

raw12 <- read_csv(file.path(RAW, "2012_precinct.csv"), col_names = c("year_f","election_type","county_code","precinct_code","office_rank","district",
  "party_rank","ballot_position","office_code","party_code","candidate_number","last_name","first_name","middle_name","suffix","votes","us_cd","state_sd",
  "state_hd","muni_type","muni_name","muni_bd_code1","muni_bd_name1","muni_bd_code2","muni_bd_name2","bicounty_code","mcd_code","fips_code","vtd_code",
  "prev_precinct","prev_us_cd","prev_state_sd","prev_state_hd"), col_types = cols(.default = "c"), show_col_types = FALSE) %>%
  filter(office_code == "USC") %>% mutate(votes = as.numeric(votes), county_fips = 42000 + as.integer(fips_code)) %>% filter(!is.na(votes)) %>%
  transmute(year = 2012, county_fips, district, candidate = trimws(paste(first_name, ifelse(is.na(middle_name), "", middle_name), last_name, ifelse(is.na(suffix), "", suffix))),
            party = party_code, votes)

df <- bind_rows(clean %>% select(year, county_fips, district, candidate, party, votes), raw12) %>% mutate(party_group = grp(party))
long <- finalize_long(df, "pa"); save_long(long, "he_pa")
res <- check_long_vs_source(long, file.path(OUTPUT_DIR, "elect_he_cty_pa.rds")); print(res)
