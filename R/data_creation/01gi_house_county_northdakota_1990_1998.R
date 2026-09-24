## North Dakota U.S. House (at-large) 1990-1998, county level, from the Secretary of State's "Official Abstract of Votes Cast" scans (01gh_northdakota_1990_1998_parse.py ->
## R/data/county_house_files/north_dakota/nd_house_county_1990_1998.csv; 53 counties per year, every candidate column ties to the printed statewide total, which equals the FEC's official total
## for 1990, 1992, 1996 and 1998). 1990: Dorgan (D) and Schafer (R) only (6 write-ins statewide are not reported by county). Party: D -> DEM, R -> REP, everything else OTHER.
## Outputs: he_ndsos_<year> long and elect_he_cty_ndsos_<year> (folded in by 01gj_northdakota_apply.R).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
d <- read_csv(file.path(PROJECT_ROOT, "R/data/county_house_files/north_dakota/nd_house_county_1990_1998.csv"), show_col_types = FALSE) %>% mutate(votes = as.numeric(votes), ckey = gsub("[^A-Z]", "", toupper(county)))
nd <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>% filter(state == "NORTH DAKOTA", !is.na(county_fips)) %>% transmute(county_fips, ckey = gsub("[^A-Z]", "", toupper(county_name))) %>% distinct(ckey, .keep_all = TRUE)
stopifnot(nrow(nd) == 53, all(d$ckey %in% nd$ckey))
pname <- c(D = "Democratic-NPL", R = "Republican", I = "Independent")
raw <- d %>% inner_join(nd, by = "ckey") %>% transmute(year, county_fips, district = "00", candidate, party = unname(pname[party_code]), party_group = case_when(party_code == "D" ~ "DEM", party_code == "R" ~ "REP", TRUE ~ "OTHER"), votes) %>% filter(votes > 0)
for (y in seq(1990, 1998, 2)) {
  long <- finalize_long(raw %>% filter(year == y), paste0("ndsos_", y)); save_long(long, paste0("he_ndsos_", y))
  shares <- derive_shares(long) %>% transmute(state = "NORTH DAKOTA", year, cty_fips, sample, demovote, repuvote, totalvote); stopifnot(nrow(shares) == 53)
  f <- file.path(OUTPUT_DIR, sprintf("elect_he_cty_ndsos_%d.rds", y)); saveRDS(shares, f); r <- check_long_vs_source(long, f); stopifnot(all(r$pass))
  message(y, ": ", nrow(shares), " counties, ", nrow(long), " long rows, votes ", format(sum(long$votes), big.mark = ","))
}
