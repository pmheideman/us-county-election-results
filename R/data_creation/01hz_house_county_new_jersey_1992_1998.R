## New Jersey U.S. House, county level, 1992, 1994, 1996, 1998, from the NJ Division of Elections' 'Candidates for the Office of House of Representatives' official
## returns (image-only typescript PDFs from nj.gov/state/elections, R/data/county_house_files/new_jersey/). Hand-transcribed by 01hu/01hv/01hw/01hx_new_jersey_<year>_transcribe.py,
## which stop unless every candidate's county votes add up to the printed Total and every candidate Total equals the official district results (Clerk of the House;
## FEC book for 1996). Split counties are summed over districts. Party: D -> DEM, R -> REP, else OTHER.
## Outputs per year: he_njdoe_<year> long and elect_he_cty_njdoe_<year> (folded in by 01ia_new_jersey_1992_1998_apply.R).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
nj <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>% filter(state == "NEW JERSEY", !is.na(county_fips), county_fips > 34000, county_fips < 35000) %>%
  transmute(county_fips, ckey = gsub("[^A-Z]", "", toupper(county_name))) %>% distinct(ckey, .keep_all = TRUE)
stopifnot(nrow(nj) == 21)
for (yr in c(1992L, 1994L, 1996L, 1998L)) {
  d <- read_csv(file.path(PROJECT_ROOT, sprintf("R/data/county_house_files/new_jersey/new_jersey_house_county_%d.csv", yr)), show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    mutate(year = as.integer(year), votes = as.numeric(votes), district = sprintf("%02d", as.integer(district)), ckey = gsub("[^A-Z]", "", toupper(county)))
  stopifnot(all(d$ckey %in% nj$ckey), n_distinct(d$ckey) == 21)
  raw <- d %>% inner_join(nj, by = "ckey") %>% transmute(year, county_fips, district, candidate, party = case_when(party_code == "D" ~ "Democratic", party_code == "R" ~ "Republican", TRUE ~ "Independent"),
    party_group = case_when(party_code == "D" ~ "DEM", party_code == "R" ~ "REP", TRUE ~ "OTHER"), votes) %>% group_by(year, county_fips, district, candidate, party, party_group) %>% summarise(votes = sum(votes), .groups = "drop") %>% filter(votes > 0)
  lab <- paste0("njdoe_", yr)
  long <- finalize_long(raw, lab); save_long(long, paste0("he_", lab))
  shares <- derive_shares(long) %>% transmute(state = "NEW JERSEY", year, cty_fips, sample, demovote, repuvote, totalvote); stopifnot(nrow(shares) == 21, median(shares$demovote + shares$repuvote) > 0.2)
  f <- file.path(OUTPUT_DIR, paste0("elect_he_cty_", lab, ".rds")); saveRDS(shares, f); r <- check_long_vs_source(long, f); stopifnot(all(r$pass))
  message(yr, ": ", nrow(shares), " counties, ", nrow(long), " long rows, votes ", format(sum(long$votes), big.mark = ","))
}
