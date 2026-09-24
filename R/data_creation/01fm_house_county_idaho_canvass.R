## Idaho U.S. House, county level, 1990, 1992 and 2022, from the Idaho Secretary of State's Elections Database (https://canvass.sos.idaho.gov/, the PD43+ ElectionStats platform, the same one as
## Massachusetts). Plain curl works. The county-level U.S. Representative export (contests/search/year_from:<y1>/year_to:<y2>/office_id:3/show_granularity_dt_id:3/.csv) was saved as
## R/data/raw_house_county_open_states/idaho_canvass/us_house_county_1990_1992.csv and ..._2022_2022.csv (all primaries and generals; only "General" rows are used).
## Layout: one row per contest x candidate x county, plus per-county pseudo-candidates "Total Ballots Cast" and "Total Votes Cast" (is_pseudocandidate = 1). County names carry a trailing space in places ("Ada ").
## Ada County is in both districts and is summed. Check (stop on failure): for every county and district the candidate votes add up to that county's "Total Votes Cast".
## Party: literal (Democratic / Republican / anything else OTHER). Outputs: R/output/long/he_idcv_<year>.rds and R/output/elect_he_cty_idcv_<year>.rds (1990, 1992, 2022; folded in by 01fn_idaho_apply.R).
## 2026-09-24: also 1994, 1996, 1998, 2000 and 2016 from ..._1994_2020.csv, replacing flawed rows (folded in by 01hn_idaho_canvass_replace_apply.R): the OpenElections files for 1994-1998 have
## no rows at all for Helen Chenoweth (R, District 1 winner each year), 2000 lacks Donovan Bramwell (L, District 2), and MEDSL 2016 undercounts one county (Labrador -6,406).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
D <- file.path(PROJECT_ROOT, "R/data/raw_house_county_open_states/idaho_canvass")
d <- bind_rows(read_csv(file.path(D, "us_house_county_1990_1992.csv"), show_col_types = FALSE, col_types = cols(.default = "c")), read_csv(file.path(D, "us_house_county_2022_2022.csv"), show_col_types = FALSE, col_types = cols(.default = "c")),
  read_csv(file.path(D, "us_house_county_1994_2020.csv"), show_col_types = FALSE, col_types = cols(.default = "c")) %>% filter(substr(election_date, 1, 4) %in% c("1994", "1996", "1998", "2000", "2016"))) %>%
  filter(election_type == "General", office_name == "United States Representative") %>%
  mutate(year = as.integer(substr(election_date, 1, 4)), county = toupper(trimws(granular_division_name)), district = sprintf("%02d", as.integer(district_name)), votes = as.numeric(votes))
tv <- d %>% filter(candidate_name == "Total Votes Cast") %>% select(year, district, county, tv = votes)
cd <- d %>% filter(is_pseudocandidate == "0")
chk <- cd %>% group_by(year, district, county) %>% summarise(s = sum(votes), .groups = "drop") %>% inner_join(tv, by = c("year", "district", "county")); stopifnot(nrow(chk) == nrow(tv), all(chk$s == chk$tv))
cat("county x district rows:", nrow(chk), "| candidate votes equal 'Total Votes Cast' in all\n")
id <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>% filter(state == "IDAHO", !is.na(county_fips), county_fips > 16000, county_fips < 17000) %>% transmute(county_fips, county = toupper(trimws(county_name))) %>% distinct(county, .keep_all = TRUE)
stopifnot(n_distinct(id$county_fips) == 44, all(cd$county %in% id$county))
raw <- cd %>% inner_join(id, by = "county") %>% transmute(year, county_fips, district, candidate = trimws(candidate_name), party = ifelse(is.na(candidate_party) | candidate_party == "", "Other", candidate_party),
  party_group = case_when(candidate_party == "Democratic" ~ "DEM", candidate_party == "Republican" ~ "REP", TRUE ~ "OTHER"), votes) %>% group_by(year, county_fips, district, candidate, party, party_group) %>% summarise(votes = sum(votes), .groups = "drop") %>% filter(votes > 0)
for (y in sort(unique(raw$year))) {
  long <- finalize_long(raw %>% filter(year == y), paste0("idcv_", y)); save_long(long, paste0("he_idcv_", y))
  shares <- derive_shares(long) %>% transmute(state = "IDAHO", year, cty_fips, sample, demovote, repuvote, totalvote); stopifnot(nrow(shares) == 44)
  saveRDS(shares, file.path(OUTPUT_DIR, sprintf("elect_he_cty_idcv_%d.rds", y))); r <- check_long_vs_source(long, file.path(OUTPUT_DIR, sprintf("elect_he_cty_idcv_%d.rds", y))); stopifnot(all(r$pass))
  message(y, ": ", nrow(shares), " counties, ", nrow(long), " long rows, votes ", format(sum(long$votes), big.mark = ","))
}
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")) %>% filter(sample == "HE", cty_fips %/% 1000 == 16, year == 2022)
sh <- derive_shares(raw %>% filter(year == 2022) %>% transmute(year, county_fips, votes, party_group, office = "house")) %>% inner_join(panel %>% select(cty_fips, pd = demovote, pr = repuvote, pt = totalvote), by = "cty_fips")
cat("2022: counties in panel ", nrow(panel), "; identical: ", sum(abs(sh$demovote - sh$pd) < 1e-9 & abs(sh$repuvote - sh$pr) < 1e-9 & abs(sh$totalvote / sh$pt - 1) < 1e-6), "; within 1% of total: ", sum(abs(sh$totalvote / sh$pt - 1) < 0.01), "\n", sep = "")
