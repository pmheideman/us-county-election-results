## Oklahoma U.S. House 1998, county level, from the State Election Board's results page as archived by the Wayback Machine (parsed by 01fj_oklahoma_1998_parse.py into
## R/data/raw_house_county_open_states/oklahoma_archive/us_house_1998_county_district.csv): per district, every candidate's votes in every county of the district; counties split between districts
## (Tulsa, Wagoner, Osage, Oklahoma, Canadian, Cleveland, ...) appear once per district and are summed. Every district table ties to its STATE TOTAL line, and every county row to its total.
## Party: D -> DEM, R -> REP, everything else (I) OTHER. Outputs: R/output/long/he_okarc_1998.rds and R/output/elect_he_cty_okarc_1998.rds (folded into the panel by 01fl_oklahoma_1998_apply.R).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
d <- read_csv(file.path(PROJECT_ROOT, "R/data/raw_house_county_open_states/oklahoma_archive/us_house_1998_county_district.csv"), show_col_types = FALSE, col_types = cols(.default = "c")) %>%
  mutate(year = as.integer(year), votes = as.numeric(votes), district = sprintf("%02d", as.integer(district)), ckey = gsub("[^A-Z]", "", toupper(county)))
ok <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>% filter(state == "OKLAHOMA", !is.na(county_fips), county_fips > 40000, county_fips < 41000) %>%
  transmute(county_fips, ckey = gsub("[^A-Z]", "", toupper(county_name))) %>% distinct(ckey, .keep_all = TRUE)
stopifnot(n_distinct(ok$county_fips) == 77, all(d$ckey %in% ok$ckey), n_distinct(d$ckey) == 77)
raw <- d %>% inner_join(ok, by = "ckey") %>% mutate(party_group = case_when(party_code == "D" ~ "DEM", party_code == "R" ~ "REP", TRUE ~ "OTHER"), party = case_when(party_code == "D" ~ "Democratic", party_code == "R" ~ "Republican", party_code == "I" ~ "Independent", TRUE ~ party_code)) %>%
  group_by(year, county_fips, district, candidate, party, party_group) %>% summarise(votes = sum(votes), .groups = "drop") %>% filter(votes > 0)
long <- finalize_long(raw, "okarc_1998"); save_long(long, "he_okarc_1998")
shares <- derive_shares(long) %>% transmute(state = "OKLAHOMA", year, cty_fips, sample, demovote, repuvote, totalvote); stopifnot(nrow(shares) == 77)
saveRDS(shares, file.path(OUTPUT_DIR, "elect_he_cty_okarc_1998.rds")); r <- check_long_vs_source(long, file.path(OUTPUT_DIR, "elect_he_cty_okarc_1998.rds")); stopifnot(all(r$pass))
message("1998: ", nrow(shares), " counties, ", nrow(long), " long rows, votes ", format(sum(long$votes), big.mark = ","))
