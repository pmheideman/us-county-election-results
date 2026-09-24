## Wyoming U.S. House (at-large), county level, 1990, 1992 and 1994, hand-transcribed from the Secretary of State's Official Directory and Election Returns volumes as scanned in the Internet Archive's
## Statistical Reference Index microfiche collection (01gv_wyoming_1990_1994_transcribe.py -> R/data/county_house_files/wyoming/wyoming_house_county_1990_1994.csv; every candidate's 23 county rows tie to
## the printed Official Totals). Party: D -> DEM, R -> REP, L -> OTHER. Outputs: R/output/long/he_wyia_<year>.rds and R/output/elect_he_cty_wyia_<year>.rds (folded in by 01gx_wyoming_1990_1994_apply.R).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
d <- read_csv(file.path(PROJECT_ROOT, "R/data/county_house_files/wyoming/wyoming_house_county_1990_1994.csv"), show_col_types = FALSE, col_types = cols(.default = "c")) %>% mutate(year = as.integer(year), votes = as.numeric(votes), ckey = gsub("[^A-Z]", "", toupper(county)))
wy <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>% filter(state == "WYOMING", !is.na(county_fips), county_fips > 56000, county_fips < 57000) %>% transmute(county_fips, ckey = gsub("[^A-Z]", "", toupper(county_name))) %>% distinct(ckey, .keep_all = TRUE)
stopifnot(n_distinct(wy$county_fips) == 23, all(d$ckey %in% wy$ckey))
raw <- d %>% inner_join(wy, by = "ckey") %>% transmute(year, county_fips, district = "00", candidate, party = case_when(party_code == "D" ~ "Democratic", party_code == "R" ~ "Republican", party_code == "L" ~ "Libertarian", TRUE ~ party_code),
  party_group = case_when(party_code == "D" ~ "DEM", party_code == "R" ~ "REP", TRUE ~ "OTHER"), votes) %>% filter(votes > 0)
for (y in sort(unique(raw$year))) {
  long <- finalize_long(raw %>% filter(year == y), paste0("wyia_", y)); save_long(long, paste0("he_wyia_", y))
  shares <- derive_shares(long) %>% transmute(state = "WYOMING", year, cty_fips, sample, demovote, repuvote, totalvote); stopifnot(nrow(shares) == 23)
  saveRDS(shares, file.path(OUTPUT_DIR, sprintf("elect_he_cty_wyia_%d.rds", y))); r <- check_long_vs_source(long, file.path(OUTPUT_DIR, sprintf("elect_he_cty_wyia_%d.rds", y))); stopifnot(all(r$pass))
  message(y, ": ", nrow(shares), " counties, ", nrow(long), " long rows, votes ", format(sum(long$votes), big.mark = ","))
}
