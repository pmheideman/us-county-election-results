## Oklahoma U.S. House, county level, 1990, 1992, 1996, from the State Election Board's 'State of Oklahoma Election Results and Statistics' (1990 and 1992: SRI microfiche on
## the Internet Archive, items micro_IA40706939_0247 / micro_IA40706946_0152, transcribed by 01ib_oklahoma_1990_1992_transcribe.py; 1996: Oklahoma Department of Libraries
## Digital Prairie item stgovpub/13878, text layer parsed by 01ic_oklahoma_1996_parse.py). Both stop unless candidate columns tie to the printed totals and the FEC.
## Split counties (* in the source) are summed over districts. Party: D -> DEM, R -> REP, else OTHER.
## Outputs per year: he_okseb_<year> long and elect_he_cty_okseb_<year> (folded in by 01ie_oklahoma_1990_1996_apply.R).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
ok <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>% filter(state == "OKLAHOMA", !is.na(county_fips), county_fips > 40000, county_fips < 41000) %>%
  transmute(county_fips, ckey = gsub("[^A-Z]", "", toupper(county_name))) %>% distinct(ckey, .keep_all = TRUE)
stopifnot(nrow(ok) == 77)
d_all <- bind_rows(
  read_csv(file.path(PROJECT_ROOT, "R/data/county_house_files/oklahoma_microfiche/oklahoma_house_county_1990_1992.csv"), show_col_types = FALSE, col_types = cols(.default = "c")),
  read_csv(file.path(PROJECT_ROOT, "R/data/county_house_files/oklahoma_house_county_1996.csv"), show_col_types = FALSE, col_types = cols(.default = "c"))) %>%
  mutate(year = as.integer(year), votes = as.numeric(votes), district = sprintf("%02d", as.integer(district)), ckey = gsub("[^A-Z]", "", toupper(county)))
stopifnot(all(d_all$ckey %in% ok$ckey))
for (yr in c(1990L, 1992L, 1996L)) {
  d <- d_all %>% filter(year == yr); stopifnot(n_distinct(d$ckey) == 77)
  raw <- d %>% inner_join(ok, by = "ckey") %>% transmute(year, county_fips, district, candidate, party = case_when(party_code == "D" ~ "Democratic", party_code == "R" ~ "Republican", TRUE ~ "Independent"),
    party_group = case_when(party_code == "D" ~ "DEM", party_code == "R" ~ "REP", TRUE ~ "OTHER"), votes) %>% group_by(year, county_fips, district, candidate, party, party_group) %>% summarise(votes = sum(votes), .groups = "drop") %>% filter(votes > 0)
  lab <- paste0("okseb_", yr)
  long <- finalize_long(raw, lab); save_long(long, paste0("he_", lab))
  shares <- derive_shares(long) %>% transmute(state = "OKLAHOMA", year, cty_fips, sample, demovote, repuvote, totalvote); stopifnot(nrow(shares) == 77, median(shares$demovote + shares$repuvote) > 0.2)
  f <- file.path(OUTPUT_DIR, paste0("elect_he_cty_", lab, ".rds")); saveRDS(shares, f); r <- check_long_vs_source(long, f); stopifnot(all(r$pass))
  message(yr, ": ", nrow(shares), " counties, ", nrow(long), " long rows, votes ", format(sum(long$votes), big.mark = ","))
}
