## Delaware U.S. House (at-large) 1990, 1992, 1994, 1996, county level, transcribed from the Department of Elections' scanned statewide-offices tables (01ge_delaware_1990_1996_transcribe.py ->
## R/data/county_house_files/delaware/delaware_house_county_1990_1996.csv; New Castle = City of Wilmington + Rural New Castle; every candidate and column ties to the printed totals).
## Party: D -> DEM, R -> REP, K (A Delaware), L, V (Natural Law), P (U.S. Taxpayers) -> OTHER. Outputs: he_dede_<year> long and elect_he_cty_dede_<year> (folded in by 01gg_delaware_1990_1996_apply.R).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
d <- read_csv(file.path(PROJECT_ROOT, "R/data/county_house_files/delaware/delaware_house_county_1990_1996.csv"), show_col_types = FALSE) %>% mutate(votes = as.numeric(votes))
de <- read.delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab")) %>% filter(state == "DELAWARE", !is.na(county_fips)) %>% transmute(county_fips, county = toupper(trimws(county_name))) %>% distinct(county, .keep_all = TRUE)
stopifnot(all(unique(d$county) %in% de$county))
pname <- c(D = "Democratic", R = "Republican", K = "A Delaware Party", L = "Libertarian", V = "Natural Law", P = "U.S. Taxpayers")
raw <- d %>% inner_join(de, by = "county") %>% transmute(year, county_fips, district = "00", candidate, party = unname(pname[party_code]), party_group = case_when(party_code == "D" ~ "DEM", party_code == "R" ~ "REP", TRUE ~ "OTHER"), votes)
stopifnot(!anyNA(raw$party))
for (y in c(1990, 1992, 1994, 1996)) {
  long <- finalize_long(raw %>% filter(year == y), paste0("dede_", y)); save_long(long, paste0("he_dede_", y))
  shares <- derive_shares(long) %>% transmute(state = "DELAWARE", year, cty_fips, sample, demovote, repuvote, totalvote); stopifnot(nrow(shares) == 3)
  f <- file.path(OUTPUT_DIR, sprintf("elect_he_cty_dede_%d.rds", y)); saveRDS(shares, f); r <- check_long_vs_source(long, f); stopifnot(all(r$pass))
  message(y, ": ", nrow(shares), " counties, ", nrow(long), " long rows, votes ", format(sum(long$votes), big.mark = ","))
}
