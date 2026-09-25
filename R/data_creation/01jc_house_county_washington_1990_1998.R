## Washington U.S. House, county level, 1990-1998, from the Secretary of State's 'Election Search' results database county breakdowns, reconciled to the
## FEC (1990) / Clerk of the House (1992-1998) by 01jb_washington_1990_1998_reconcile.py (every candidate equals the official total; party codes from them).
## Split counties summed over districts. Party: D -> DEM, R -> REP, else OTHER. Outputs per year: he_wasosdb_<year> long and elect_he_cty_wasosdb_<year>
## (folded in by 01jd_washington_1990_1998_apply.R).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
wa <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>% filter(state == "WASHINGTON", !is.na(county_fips), county_fips > 53000, county_fips < 54000) %>%
  transmute(county_fips, ckey = gsub("[^A-Z]", "", toupper(county_name))) %>% distinct(ckey, .keep_all = TRUE)
stopifnot(nrow(wa) == 39)
for (yr in c(1990L, 1992L, 1994L, 1996L, 1998L)) {
  d <- read_csv(file.path(PROJECT_ROOT, sprintf("R/data/county_house_files/washington/washington_house_county_%d.csv", yr)), show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    mutate(year = as.integer(year), votes = as.numeric(votes), district = sprintf("%02d", as.integer(district)), ckey = gsub("[^A-Z]", "", toupper(county)))
  stopifnot(all(d$ckey %in% wa$ckey), n_distinct(d$ckey) == 39)
  raw <- d %>% inner_join(wa, by = "ckey") %>% transmute(year, county_fips, district, candidate, party = case_when(party_code == "D" ~ "Democratic", party_code == "R" ~ "Republican", TRUE ~ "Other"),
    party_group = case_when(party_code == "D" ~ "DEM", party_code == "R" ~ "REP", TRUE ~ "OTHER"), votes) %>% group_by(year, county_fips, district, candidate, party, party_group) %>% summarise(votes = sum(votes), .groups = "drop") %>% filter(votes > 0)
  lab <- paste0("wasosdb_", yr)
  long <- finalize_long(raw, lab); save_long(long, paste0("he_", lab))
  shares <- derive_shares(long) %>% transmute(state = "WASHINGTON", year, cty_fips, sample, demovote, repuvote, totalvote); stopifnot(nrow(shares) == 39, median(shares$demovote + shares$repuvote) > 0.2)
  f <- file.path(OUTPUT_DIR, paste0("elect_he_cty_", lab, ".rds")); saveRDS(shares, f); r <- check_long_vs_source(long, f); stopifnot(all(r$pass))
  message(yr, ": ", nrow(shares), " counties, ", nrow(long), " long rows, votes ", format(sum(long$votes), big.mark = ","))
}
