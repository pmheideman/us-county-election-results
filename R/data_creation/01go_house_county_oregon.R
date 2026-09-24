## Oregon U.S. House, county level, 1990-1998, 2000, 2002, 2004 and 2012, from the Secretary of State's Official Abstract of Votes / Statistical Summary documents in the State Library of Oregon's digital
## collection (01gn_oregon_parse.py -> R/data/county_house_files/oregon/or_house_county.csv; all 36 counties, every district's candidate columns tie to the printed TOTAL rows; 1990 and 1992 also equal the FEC).
## A county split between districts appears in each and is summed. Party: D -> DEM, R -> REP, everything else OTHER. Outputs: R/output/long/he_orsl_<year>.rds and R/output/elect_he_cty_orsl_<year>.rds
## (folded in by 01gp_oregon_apply.R; 2000, 2002, 2004 and 2012 replace earlier partial rows).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
d <- read_csv(file.path(PROJECT_ROOT, "R/data/county_house_files/oregon/or_house_county.csv"), show_col_types = FALSE, col_types = cols(.default = "c")) %>%
  mutate(year = as.integer(year), votes = as.numeric(votes), district = sprintf("%02d", as.integer(district)), ckey = gsub("[^A-Z]", "", toupper(county)))
orc <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>% filter(state == "OREGON", !is.na(county_fips)) %>% transmute(county_fips, ckey = gsub("[^A-Z]", "", toupper(county_name))) %>% distinct(ckey, .keep_all = TRUE)
stopifnot(nrow(orc) == 36, all(d$ckey %in% orc$ckey))
pname <- c(D = "Democratic", R = "Republican", L = "Libertarian", I = "Independent", S = "Socialist", C = "Constitution", P = "Progressive", PG = "Pacific Green", NL = "Natural Law", RF = "Reform", A = "American")
raw <- d %>% inner_join(orc, by = "ckey") %>% transmute(year, county_fips, district, candidate, party = ifelse(party_code %in% names(pname), unname(pname[party_code]), party_code),
  party_group = case_when(party_code == "D" ~ "DEM", party_code == "R" ~ "REP", TRUE ~ "OTHER"), votes) %>% group_by(year, county_fips, district, candidate, party, party_group) %>% summarise(votes = sum(votes), .groups = "drop") %>% filter(votes > 0)
for (y in sort(unique(raw$year))) {
  long <- finalize_long(raw %>% filter(year == y), paste0("orsl_", y)); save_long(long, paste0("he_orsl_", y))
  shares <- derive_shares(long) %>% transmute(state = "OREGON", year, cty_fips, sample, demovote, repuvote, totalvote); stopifnot(nrow(shares) == 36)
  f <- file.path(OUTPUT_DIR, sprintf("elect_he_cty_orsl_%d.rds", y)); saveRDS(shares, f); r <- check_long_vs_source(long, f); stopifnot(all(r$pass))
  message(y, ": ", nrow(shares), " counties, ", nrow(long), " long rows, votes ", format(sum(long$votes), big.mark = ","))
}
