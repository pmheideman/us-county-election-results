## North Carolina U.S. House, county level, 1992 and 1994, from the State Board of Elections' typed "Abstract of Votes Cast for Member of Congress" as scanned in the State Library's digital
## collection (OCR'd by 01gk_northcarolina_1992_1994_parse.py into R/data/county_house_files/north_carolina/nc_house_county_1992_1994.csv; every district's county rows tie to the printed TOTAL row
## except 1992 District 10, which is 34 (Ballenger) and 2 (Brown) votes short of its printed total, as printed). A county split between districts appears in each and is summed.
## Party: D -> DEM, R -> REP, everything else (L, WI) OTHER. Outputs: R/output/long/he_ncdcr_<year>.rds and R/output/elect_he_cty_ncdcr_<year>.rds (folded in by 01gm_northcarolina_1992_1994_apply.R).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
d <- read_csv(file.path(PROJECT_ROOT, "R/data/county_house_files/north_carolina/nc_house_county_1992_1994.csv"), show_col_types = FALSE, col_types = cols(.default = "c")) %>%
  mutate(year = as.integer(year), votes = as.numeric(votes), district = sprintf("%02d", as.integer(district)), ckey = gsub("[^A-Z]", "", toupper(county)))
nc <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>% filter(state == "NORTH CAROLINA", !is.na(county_fips), county_fips > 37000, county_fips < 38000) %>% transmute(county_fips, ckey = gsub("[^A-Z]", "", toupper(county_name))) %>% distinct(ckey, .keep_all = TRUE)
stopifnot(n_distinct(nc$county_fips) == 100, all(d$ckey %in% nc$ckey))
raw <- d %>% inner_join(nc, by = "ckey") %>% transmute(year, county_fips, district, candidate, party = case_when(party_code == "D" ~ "Democratic", party_code == "R" ~ "Republican", party_code == "L" ~ "Libertarian", party_code == "WI" ~ "Write-In", TRUE ~ party_code),
  party_group = case_when(party_code == "D" ~ "DEM", party_code == "R" ~ "REP", TRUE ~ "OTHER"), votes) %>% group_by(year, county_fips, district, candidate, party, party_group) %>% summarise(votes = sum(votes), .groups = "drop") %>% filter(votes > 0)
for (y in sort(unique(raw$year))) {
  long <- finalize_long(raw %>% filter(year == y), paste0("ncdcr_", y)); save_long(long, paste0("he_ncdcr_", y))
  shares <- derive_shares(long) %>% transmute(state = "NORTH CAROLINA", year, cty_fips, sample, demovote, repuvote, totalvote); stopifnot(nrow(shares) == 100)
  f <- file.path(OUTPUT_DIR, sprintf("elect_he_cty_ncdcr_%d.rds", y)); saveRDS(shares, f); r <- check_long_vs_source(long, f); stopifnot(all(r$pass))
  message(y, ": ", nrow(shares), " counties, ", nrow(long), " long rows, votes ", format(sum(long$votes), big.mark = ","))
}
