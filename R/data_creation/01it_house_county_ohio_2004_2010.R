## Ohio U.S. House, county level, 2004 and 2010, from the Ohio Secretary of State's results pages archived by the Wayback Machine (parsed and checked by
## 01is_ohio_2004_2010_parse.py -> R/data/raw_house_county_open_states/ohio_sos_archive/us_house_county_district_2004_2010.csv: every column ties to its printed
## Total and every D/R candidate equals the FEC). Split counties summed over districts. Party: D -> DEM, R -> REP, else OTHER (write-ins kept as OTHER).
## Outputs per year: he_ohsosweb_<year> long and elect_he_cty_ohsosweb_<year> (folded in by 01iu_ohio_2004_2010_apply.R).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
oh <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>% filter(state == "OHIO", !is.na(county_fips), county_fips > 39000, county_fips < 40000) %>%
  transmute(county_fips, ckey = gsub("[^A-Z]", "", toupper(county_name))) %>% distinct(ckey, .keep_all = TRUE)
stopifnot(nrow(oh) == 88)
d_all <- read_csv(file.path(PROJECT_ROOT, "R/data/raw_house_county_open_states/ohio_sos_archive/us_house_county_district_2004_2010.csv"), show_col_types = FALSE, col_types = cols(.default = "c")) %>%
  mutate(year = as.integer(year), votes = as.numeric(votes), district = sprintf("%02d", as.integer(district)), ckey = gsub("[^A-Z]", "", toupper(county)))
stopifnot(all(d_all$ckey %in% oh$ckey))
for (yr in c(2004L, 2010L)) {
  raw <- d_all %>% filter(year == yr) %>% inner_join(oh, by = "ckey") %>% transmute(year, county_fips, district, candidate, party = case_when(party_code == "D" ~ "Democratic", party_code == "R" ~ "Republican", TRUE ~ "Other"),
    party_group = case_when(party_code == "D" ~ "DEM", party_code == "R" ~ "REP", TRUE ~ "OTHER"), votes) %>% group_by(year, county_fips, district, candidate, party, party_group) %>% summarise(votes = sum(votes), .groups = "drop") %>% filter(votes > 0)
  lab <- paste0("ohsosweb_", yr)
  long <- finalize_long(raw, lab); save_long(long, paste0("he_", lab))
  shares <- derive_shares(long) %>% transmute(state = "OHIO", year, cty_fips, sample, demovote, repuvote, totalvote); stopifnot(nrow(shares) == 88, median(shares$demovote + shares$repuvote) > 0.2)
  f <- file.path(OUTPUT_DIR, paste0("elect_he_cty_", lab, ".rds")); saveRDS(shares, f); r <- check_long_vs_source(long, f); stopifnot(all(r$pass))
  message(yr, ": ", nrow(shares), " counties, ", nrow(long), " long rows, votes ", format(sum(long$votes), big.mark = ","))
}
