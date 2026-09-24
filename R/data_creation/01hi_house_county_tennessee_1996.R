## Tennessee U.S. House, county level, 1996, from the Tennessee Blue Book 1997-1998 (typeset tables, two independent keys - image transcription and the PDF text layer - checked by
## 01hh_tennessee_1996_bluebook.py -> R/data/county_house_files/tennessee/tennessee_house_county_1996.csv; 95 counties, every column ties to the printed TOTAL, totals equal the FEC). A county split
## between districts (Bradley, Knox, Davidson, Robertson, Shelby) appears in each and is summed. Party: D -> DEM, R -> REP, everything else OTHER (independents). Write-ins dropped, as for 1998.
## Outputs: he_tnbb_1996 long and elect_he_cty_tnbb_1996 (folded in by 01hj_tennessee_1996_apply.R).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
d <- read_csv(file.path(PROJECT_ROOT, "R/data/county_house_files/tennessee/tennessee_house_county_1996.csv"), show_col_types = FALSE, col_types = cols(.default = "c")) %>%
  mutate(year = as.integer(year), votes = as.numeric(votes), district = sprintf("%02d", as.integer(district)), ckey = gsub("[^A-Z]", "", toupper(county)))
tn <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>% filter(state == "TENNESSEE", !is.na(county_fips), county_fips > 47000, county_fips < 48000) %>% transmute(county_fips, ckey = gsub("[^A-Z]", "", toupper(county_name))) %>% distinct(ckey, .keep_all = TRUE)
stopifnot(nrow(tn) == 95, all(d$ckey %in% tn$ckey), n_distinct(d$ckey) == 95)
raw <- d %>% inner_join(tn, by = "ckey") %>% transmute(year, county_fips, district, candidate, party = case_when(party_code == "D" ~ "Democratic", party_code == "R" ~ "Republican", TRUE ~ "Independent"),
  party_group = case_when(party_code == "D" ~ "DEM", party_code == "R" ~ "REP", TRUE ~ "OTHER"), votes) %>% group_by(year, county_fips, district, candidate, party, party_group) %>% summarise(votes = sum(votes), .groups = "drop") %>% filter(votes > 0)
long <- finalize_long(raw, "tnbb_1996"); save_long(long, "he_tnbb_1996")
shares <- derive_shares(long) %>% transmute(state = "TENNESSEE", year, cty_fips, sample, demovote, repuvote, totalvote); stopifnot(nrow(shares) == 95)
f <- file.path(OUTPUT_DIR, "elect_he_cty_tnbb_1996.rds"); saveRDS(shares, f); r <- check_long_vs_source(long, f); stopifnot(all(r$pass))
message("1996: ", nrow(shares), " counties, ", nrow(long), " long rows, votes ", format(sum(long$votes), big.mark = ","))
