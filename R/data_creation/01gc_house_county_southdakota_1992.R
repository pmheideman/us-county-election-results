## South Dakota U.S. House (at-large) 1992, county level, transcribed from the Secretary of State's scanned 1992 general-election returns (01gb_southdakota_1992_transcribe.py ->
## R/data/county_house_files/south_dakota/south_dakota_house_county_1992.csv; the 66 county rows tie to the printed totals of all five candidates). Shannon County = FIPS 46113 as in the rest of the panel.
## Party: D -> DEM, R -> REP, L and I -> OTHER. Outputs: he_sdsos_1992 long and elect_he_cty_sdsos_1992 (folded in by 01gd_southdakota_apply.R).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
d <- read_csv(file.path(PROJECT_ROOT, "R/data/county_house_files/south_dakota/south_dakota_house_county_1992.csv"), show_col_types = FALSE, col_types = cols(.default = "c")) %>% mutate(year = as.integer(year), votes = as.numeric(votes), ckey = gsub("[^A-Z]", "", toupper(county)))
sd <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>% filter(state == "SOUTH DAKOTA", !is.na(county_fips), county_fips > 46000, county_fips < 47000, county_fips != 46102) %>% transmute(county_fips, ckey = gsub("[^A-Z]", "", toupper(county_name))) %>% filter(ckey != "OGLALALAKOTA") %>% distinct(ckey, .keep_all = TRUE)
stopifnot(n_distinct(sd$county_fips) == 66, all(d$ckey %in% sd$ckey))
raw <- d %>% inner_join(sd, by = "ckey") %>% transmute(year, county_fips, district = "00", candidate, party = case_when(party_code == "D" ~ "Democratic", party_code == "R" ~ "Republican", party_code == "L" ~ "Libertarian", party_code == "I" ~ "Independent", TRUE ~ party_code),
  party_group = case_when(party_code == "D" ~ "DEM", party_code == "R" ~ "REP", TRUE ~ "OTHER"), votes) %>% filter(votes > 0)
long <- finalize_long(raw, "sdsos_1992"); save_long(long, "he_sdsos_1992")
shares <- derive_shares(long) %>% transmute(state = "SOUTH DAKOTA", year, cty_fips, sample, demovote, repuvote, totalvote); stopifnot(nrow(shares) == 66)
saveRDS(shares, file.path(OUTPUT_DIR, "elect_he_cty_sdsos_1992.rds")); r <- check_long_vs_source(long, file.path(OUTPUT_DIR, "elect_he_cty_sdsos_1992.rds")); stopifnot(all(r$pass))
message("1992: ", nrow(shares), " counties, ", nrow(long), " long rows, votes ", format(sum(long$votes), big.mark = ","))
