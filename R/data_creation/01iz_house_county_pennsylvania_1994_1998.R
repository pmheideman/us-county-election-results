## Pennsylvania U.S. House, county level, 1994, 1996, 1998, from the Department of State's precinct election returns (2005 extract), summed to counties and
## checked against the Clerk of the House by 01iy_pennsylvania_1992_1998_parse.py (every major candidate within 1% of the certified total; 1994 D7, 1996 D11 and
## 1998 D3 within 3%, documented; party codes from the Clerk for cross-filed candidates). 1992 is not built (see 01iy). Split counties summed over districts.
## Party: D -> DEM, R -> REP, else OTHER. Outputs per year: he_padosprec_<year> long and elect_he_cty_padosprec_<year> (folded in by 01ja_pennsylvania_1994_1998_apply.R).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
pa <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>% filter(state == "PENNSYLVANIA", !is.na(county_fips), county_fips > 42000, county_fips < 43000) %>%
  transmute(county_fips, ckey = gsub("[^A-Z]", "", toupper(county_name))) %>% distinct(ckey, .keep_all = TRUE)
stopifnot(nrow(pa) == 67)
for (yr in c(1994L, 1996L, 1998L)) {
  d <- read_csv(file.path(PROJECT_ROOT, sprintf("R/data/county_house_files/pennsylvania/pennsylvania_house_county_%d.csv", yr)), show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    mutate(year = as.integer(year), votes = as.numeric(votes), district = sprintf("%02d", as.integer(district)), ckey = gsub("[^A-Z]", "", toupper(county)))
  stopifnot(all(d$ckey %in% pa$ckey), n_distinct(d$ckey) == 67)
  raw <- d %>% inner_join(pa, by = "ckey") %>% transmute(year, county_fips, district, candidate, party = case_when(party_code == "D" ~ "Democratic", party_code == "R" ~ "Republican", TRUE ~ "Other"),
    party_group = case_when(party_code == "D" ~ "DEM", party_code == "R" ~ "REP", TRUE ~ "OTHER"), votes) %>% group_by(year, county_fips, district, candidate, party, party_group) %>% summarise(votes = sum(votes), .groups = "drop") %>% filter(votes > 0)
  lab <- paste0("padosprec_", yr)
  long <- finalize_long(raw, lab); save_long(long, paste0("he_", lab))
  shares <- derive_shares(long) %>% transmute(state = "PENNSYLVANIA", year, cty_fips, sample, demovote, repuvote, totalvote); stopifnot(nrow(shares) == 67, median(shares$demovote + shares$repuvote) > 0.2)
  f <- file.path(OUTPUT_DIR, paste0("elect_he_cty_", lab, ".rds")); saveRDS(shares, f); r <- check_long_vs_source(long, f); stopifnot(all(r$pass))
  message(yr, ": ", nrow(shares), " counties, ", nrow(long), " long rows, votes ", format(sum(long$votes), big.mark = ","))
}
