## Arkansas House 2004 (SOS certification report) and Delaware House 1998 (Department of Elections results by election district, summed to counties),
## parsed and checked by 01jk_arkansas_2004_delaware_1998_parse.py. Arkansas District 4 was unopposed (not on the ballot): its 29 counties have no rows.
## Party: D -> DEM, R -> REP, else OTHER. Outputs: he_arsoscert_2004 / elect_he_cty_arsoscert_2004 and he_deedsum_1998 / elect_he_cty_deedsum_1998
## (folded in by 01jm_ar2004_de1998_apply.R).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
cp <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>% filter(!is.na(county_fips)) %>%
  transmute(state_po, county_fips, ckey = gsub("[^A-Z]", "", toupper(county_name))) %>% distinct()
for (x in list(list("AR", 2004L, "arkansas/arkansas_house_county_2004.csv", "arsoscert", "ARKANSAS", 46), list("DE", 1998L, "delaware/delaware_house_county_1998.csv", "deedsum", "DELAWARE", 3))) {
  xw <- cp %>% filter(state_po == x[[1]]) %>% distinct(ckey, .keep_all = TRUE)
  d <- read_csv(file.path(PROJECT_ROOT, "R/data/county_house_files", x[[3]]), show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    mutate(year = as.integer(year), votes = as.numeric(votes), district = sprintf("%02d", as.integer(district)), ckey = gsub("[^A-Z]", "", toupper(county)))
  stopifnot(all(d$ckey %in% xw$ckey), n_distinct(d$ckey) == x[[6]])
  raw <- d %>% inner_join(xw %>% select(ckey, county_fips), by = "ckey") %>% transmute(year, county_fips, district, candidate, party = case_when(party_code == "D" ~ "Democratic", party_code == "R" ~ "Republican", TRUE ~ "Other"),
    party_group = case_when(party_code == "D" ~ "DEM", party_code == "R" ~ "REP", TRUE ~ "OTHER"), votes) %>% group_by(year, county_fips, district, candidate, party, party_group) %>% summarise(votes = sum(votes), .groups = "drop") %>% filter(votes > 0)
  lab <- paste0(x[[4]], "_", x[[2]])
  long <- finalize_long(raw, lab); save_long(long, paste0("he_", lab))
  shares <- derive_shares(long) %>% transmute(state = x[[5]], year, cty_fips, sample, demovote, repuvote, totalvote); stopifnot(nrow(shares) == x[[6]])
  f <- file.path(OUTPUT_DIR, paste0("elect_he_cty_", lab, ".rds")); saveRDS(shares, f); r <- check_long_vs_source(long, f); stopifnot(all(r$pass))
  message(x[[5]], " ", x[[2]], ": ", nrow(shares), " counties, ", nrow(long), " long rows, votes ", format(sum(long$votes), big.mark = ","))
}
