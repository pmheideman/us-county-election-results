## Montana U.S. House 1990 (two districts: Western 01, Eastern 02), county level, transcribed from the Secretary of State's scanned official canvass sheet (01gt_montana_1990_transcribe.py ->
## R/data/county_house_files/montana/montana_house_county_1990.csv; 56 counties, the four candidate columns tie to the printed TOTALS row). Party: D -> DEM, R -> REP.
## Outputs: he_mtsos_1990 long and elect_he_cty_mtsos_1990 (folded in by 01gs_montana_1994_apply.R together with 1994).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
d <- read_csv(file.path(PROJECT_ROOT, "R/data/county_house_files/montana/montana_house_county_1990.csv"), show_col_types = FALSE, col_types = cols(.default = "c")) %>% mutate(year = as.integer(year), votes = as.numeric(votes), district = sprintf("%02d", as.integer(district)), ckey = gsub("[^A-Z]", "", toupper(gsub("&", "AND", county))))
mt <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>% filter(state == "MONTANA", !is.na(county_fips), county_fips > 30000, county_fips < 31000) %>% transmute(county_fips, ckey = gsub("[^A-Z]", "", toupper(gsub("&", "AND", county_name)))) %>% distinct(ckey, .keep_all = TRUE)
stopifnot(n_distinct(mt$county_fips) == 56, all(d$ckey %in% mt$ckey), n_distinct(d$ckey) == 56)
raw <- d %>% inner_join(mt, by = "ckey") %>% transmute(year, county_fips, district, candidate, party = ifelse(party_code == "D", "Democratic", "Republican"), party_group = ifelse(party_code == "D", "DEM", "REP"), votes)
long <- finalize_long(raw, "mtsos_1990"); save_long(long, "he_mtsos_1990")
shares <- derive_shares(long) %>% transmute(state = "MONTANA", year, cty_fips, sample, demovote, repuvote, totalvote); stopifnot(nrow(shares) == 56)
f <- file.path(OUTPUT_DIR, "elect_he_cty_mtsos_1990.rds"); saveRDS(shares, f); r <- check_long_vs_source(long, f); stopifnot(all(r$pass))
message("1990: ", nrow(shares), " counties, ", nrow(long), " long rows, votes ", format(sum(long$votes), big.mark = ","))
