## Montana U.S. House (at-large), county level, 1994, from the Secretary of State's 1994 Statewide General Canvass Lotus worksheet (Wayback Machine copy; parsed by 01gq_montana_1994_parse.py into
## R/data/county_house_files/montana/montana_house_county_1994.csv; 56 counties, the county rows tie to the printed TOTAL row).
## Party: D -> DEM, R -> REP, everything else (L, NL, Reform) OTHER. Outputs: he_mtsos_<year> long and elect_he_cty_mtsos_<year> (folded in by 01gs_montana_1994_apply.R).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
d <- read_csv(file.path(PROJECT_ROOT, "R/data/county_house_files/montana/montana_house_county_1994.csv"), show_col_types = FALSE, col_types = cols(.default = "c")) %>% mutate(year = as.integer(year), votes = as.numeric(votes), ckey = gsub("[^A-Z]", "", toupper(gsub("&", "AND", county))))
mt <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>% filter(state == "MONTANA", !is.na(county_fips), county_fips > 30000, county_fips < 31000) %>% transmute(county_fips, ckey = gsub("[^A-Z]", "", toupper(gsub("&", "AND", county_name)))) %>% distinct(ckey, .keep_all = TRUE)
stopifnot(n_distinct(mt$county_fips) == 56, all(d$ckey %in% mt$ckey))
raw <- d %>% inner_join(mt, by = "ckey") %>% transmute(year, county_fips, district = "00", candidate, party = case_when(party_code == "D" ~ "Democratic", party_code == "R" ~ "Republican", party_code == "L" ~ "Libertarian", party_code == "NL" ~ "Natural Law", party_code == "I" ~ "Independent", TRUE ~ party_code),
  party_group = case_when(party_code == "D" ~ "DEM", party_code == "R" ~ "REP", TRUE ~ "OTHER"), votes) %>% filter(votes > 0)
for (y in sort(unique(raw$year))) {
  long <- finalize_long(raw %>% filter(year == y), paste0("mtsos_", y)); save_long(long, paste0("he_mtsos_", y))
  shares <- derive_shares(long) %>% transmute(state = "MONTANA", year, cty_fips, sample, demovote, repuvote, totalvote); stopifnot(nrow(shares) == 56)
  saveRDS(shares, file.path(OUTPUT_DIR, sprintf("elect_he_cty_mtsos_%d.rds", y))); r <- check_long_vs_source(long, file.path(OUTPUT_DIR, sprintf("elect_he_cty_mtsos_%d.rds", y))); stopifnot(all(r$pass))
  message(y, ": ", nrow(shares), " counties, ", nrow(long), " long rows, votes ", format(sum(long$votes), big.mark = ","))
}
