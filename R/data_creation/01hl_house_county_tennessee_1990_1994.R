## Tennessee U.S. House, county level, 1990, 1992, 1994, from the Tennessee Blue Books (1991-1994, 1995-1996) checked against the Secretary of State's certification on
## SRI microfiche (01hk_tennessee_1990_1994.py -> R/data/county_house_files/tennessee/tennessee_house_county_1990_1994.csv; every candidate column ties to the certified
## district total, which equals the FEC; four Blue Book cells replaced by the certification). Split counties are summed. Party: D -> DEM, R -> REP, else OTHER. Write-ins dropped.
## Outputs per year: he_tnbb_<year> long and elect_he_cty_tnbb_<year> (folded in by 01hm_tennessee_1990_1994_apply.R).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
d_all <- read_csv(file.path(PROJECT_ROOT, "R/data/county_house_files/tennessee/tennessee_house_county_1990_1994.csv"), show_col_types = FALSE, col_types = cols(.default = "c")) %>%
  mutate(year = as.integer(year), votes = as.numeric(votes), district = sprintf("%02d", as.integer(district)), ckey = gsub("[^A-Z]", "", toupper(county)))
tn <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>% filter(state == "TENNESSEE", !is.na(county_fips), county_fips > 47000, county_fips < 48000) %>% transmute(county_fips, ckey = gsub("[^A-Z]", "", toupper(county_name))) %>% distinct(ckey, .keep_all = TRUE)
stopifnot(nrow(tn) == 95, all(d_all$ckey %in% tn$ckey))
for (yr in c(1990L, 1992L, 1994L)) {
  d <- d_all %>% filter(year == yr); stopifnot(n_distinct(d$ckey) == 95)
  raw <- d %>% inner_join(tn, by = "ckey") %>% transmute(year, county_fips, district, candidate, party = case_when(party_code == "D" ~ "Democratic", party_code == "R" ~ "Republican", TRUE ~ "Independent"),
    party_group = case_when(party_code == "D" ~ "DEM", party_code == "R" ~ "REP", TRUE ~ "OTHER"), votes) %>% group_by(year, county_fips, district, candidate, party, party_group) %>% summarise(votes = sum(votes), .groups = "drop") %>% filter(votes > 0)
  lab <- paste0("tnbb_", yr)
  long <- finalize_long(raw, lab); save_long(long, paste0("he_", lab))
  shares <- derive_shares(long) %>% transmute(state = "TENNESSEE", year, cty_fips, sample, demovote, repuvote, totalvote); stopifnot(nrow(shares) == 95, median(shares$demovote + shares$repuvote) > 0.2)
  f <- file.path(OUTPUT_DIR, paste0("elect_he_cty_", lab, ".rds")); saveRDS(shares, f); r <- check_long_vs_source(long, f); stopifnot(all(r$pass))
  message(yr, ": ", nrow(shares), " counties, ", nrow(long), " long rows, votes ", format(sum(long$votes), big.mark = ","))
}
