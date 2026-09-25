## North Carolina U.S. House, county level, 1990, from the North Carolina Manual 1991-1992 (pp. 930-933), hand-transcribed and checked by
## 01je_north_carolina_1990_transcribe.py (columns tie to printed totals and the FEC; no minor candidates in any district).
## Split counties summed over districts. Party: D -> DEM, R -> REP, else OTHER. Outputs per year: he_ncmanual_1990 long and elect_he_cty_ncmanual_1990
## (folded in by 01jg_north_carolina_1990_apply.R).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
wa <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>% filter(state == "NORTH CAROLINA", !is.na(county_fips), county_fips > 37000, county_fips < 38000) %>%
  transmute(county_fips, ckey = gsub("[^A-Z]", "", toupper(county_name))) %>% distinct(ckey, .keep_all = TRUE)
stopifnot(nrow(wa) == 100)
for (yr in c(1990L)) {
  d <- read_csv(file.path(PROJECT_ROOT, sprintf("R/data/county_house_files/north_carolina/north_carolina_house_county_%d.csv", yr)), show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    mutate(year = as.integer(year), votes = as.numeric(votes), district = sprintf("%02d", as.integer(district)), ckey = gsub("[^A-Z]", "", toupper(county)))
  stopifnot(all(d$ckey %in% wa$ckey), n_distinct(d$ckey) == 100)
  raw <- d %>% inner_join(wa, by = "ckey") %>% transmute(year, county_fips, district, candidate, party = case_when(party_code == "D" ~ "Democratic", party_code == "R" ~ "Republican", TRUE ~ "Other"),
    party_group = case_when(party_code == "D" ~ "DEM", party_code == "R" ~ "REP", TRUE ~ "OTHER"), votes) %>% group_by(year, county_fips, district, candidate, party, party_group) %>% summarise(votes = sum(votes), .groups = "drop") %>% filter(votes > 0)
  lab <- paste0("ncmanual_", yr)
  long <- finalize_long(raw, lab); save_long(long, paste0("he_", lab))
  shares <- derive_shares(long) %>% transmute(state = "NORTH CAROLINA", year, cty_fips, sample, demovote, repuvote, totalvote); stopifnot(nrow(shares) == 100, median(shares$demovote + shares$repuvote) > 0.2)
  f <- file.path(OUTPUT_DIR, paste0("elect_he_cty_", lab, ".rds")); saveRDS(shares, f); r <- check_long_vs_source(long, f); stopifnot(all(r$pass))
  message(yr, ": ", nrow(shares), " counties, ", nrow(long), " long rows, votes ", format(sum(long$votes), big.mark = ","))
}
