## Nevada U.S. House, county level, 1990-1998, from the Secretary of State's official general-election abstracts (transcribed by 01fd_nevada_1990_1998_transcribe.py into
## R/data/county_house_files/nevada/nevada_house_county_1990_1998.csv; every row's 17 county cells add up to the printed total, and the statewide candidate totals equal the FEC's official results).
## District 1 is entirely Clark County; district 2 covers the other 16 counties plus part of Clark (and Washoe): a county's total is the sum over the districts.
## Party: the abbreviation printed after each candidate: D -> DEM, R -> REP; everything else (L, IA, NL, POP, ...) OTHER.
## Outputs: R/output/long/he_nvsos_<year>.rds and R/output/elect_he_cty_nvsos_<year>.rds for 1990, 1992, 1994, 1996, 1998 (folded into the panel by 01ff_nevada_apply.R).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
d <- read_csv(file.path(PROJECT_ROOT, "R/data/county_house_files/nevada/nevada_house_county_1990_1998.csv"), show_col_types = FALSE, col_types = cols(.default = "c")) %>% mutate(year = as.integer(year), votes = as.numeric(votes), district = sprintf("%02d", as.integer(district)), county = toupper(trimws(county)))
nv <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>% filter(state == "NEVADA", !is.na(county_fips)) %>% distinct(county_name, county_fips) %>% mutate(county = toupper(trimws(county_name)))
stopifnot(nrow(nv) == 17, all(d$county %in% nv$county))
raw <- d %>% inner_join(nv %>% select(county, county_fips), by = "county") %>% mutate(party_group = case_when(party == "D" ~ "DEM", party == "R" ~ "REP", TRUE ~ "OTHER"),
  party_name = case_when(party == "D" ~ "Democratic", party == "R" ~ "Republican", party == "L" ~ "Libertarian", party == "IA" ~ "Independent American", party == "NL" ~ "Natural Law", party == "POP" ~ "Populist", TRUE ~ party)) %>%
  transmute(year, county_fips, district, candidate, party = party_name, party_group, votes)
for (y in sort(unique(raw$year))) {
  long <- finalize_long(raw %>% filter(year == y), paste0("nvsos_", y)); save_long(long, paste0("he_nvsos_", y))
  shares <- derive_shares(long) %>% transmute(state = "NEVADA", year, cty_fips, sample, demovote, repuvote, totalvote); stopifnot(nrow(shares) == 17)
  saveRDS(shares, file.path(OUTPUT_DIR, sprintf("elect_he_cty_nvsos_%d.rds", y))); r <- check_long_vs_source(long, file.path(OUTPUT_DIR, sprintf("elect_he_cty_nvsos_%d.rds", y))); stopifnot(all(r$pass))
  message(y, ": ", nrow(shares), " counties, ", nrow(long), " long rows, votes ", format(sum(long$votes), big.mark = ","))
}
