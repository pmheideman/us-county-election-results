## Michigan U.S. House, county level, 1990 and 1992, from the Michigan Manual 'Official Canvass of Votes, General Election' (1991-92 Manual pp. 886-890 for 1990,
## screenshots supplied by the project lead; 1993-94 Manual pp. 854-857 for 1992, scanned PDF). Hand-transcribed by 01hq_michigan_1990_transcribe.py and
## 01hr_michigan_1992_transcribe.py, which stop unless every county row adds up to its printed Total by County, every column to the printed Totals, and every
## district to the FEC. Split counties (Wayne, Oakland, Macomb, Genesee, ...) are summed over districts. Party: D -> DEM, R -> REP, else OTHER. Write-ins dropped.
## Outputs per year: he_mimanual_<year> long and elect_he_cty_mimanual_<year> (folded in by 01ht_michigan_1990_1992_apply.R).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
mi <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>% filter(state == "MICHIGAN", !is.na(county_fips), county_fips > 26000, county_fips < 27000) %>%
  transmute(county_fips, ckey = gsub("[^A-Z]", "", toupper(county_name))) %>% distinct(ckey, .keep_all = TRUE)
stopifnot(nrow(mi) == 83)
for (yr in c(1990L, 1992L)) {
  d <- read_csv(file.path(PROJECT_ROOT, sprintf("R/data/county_house_files/michigan/michigan_house_county_%d.csv", yr)), show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    mutate(year = as.integer(year), votes = as.numeric(votes), district = sprintf("%02d", as.integer(district)), ckey = gsub("[^A-Z]", "", toupper(county)))
  stopifnot(all(d$ckey %in% mi$ckey), n_distinct(d$ckey) == 83)
  raw <- d %>% inner_join(mi, by = "ckey") %>% transmute(year, county_fips, district, candidate, party = case_when(party_code == "D" ~ "Democratic", party_code == "R" ~ "Republican", TRUE ~ "Independent"),
    party_group = case_when(party_code == "D" ~ "DEM", party_code == "R" ~ "REP", TRUE ~ "OTHER"), votes) %>% group_by(year, county_fips, district, candidate, party, party_group) %>% summarise(votes = sum(votes), .groups = "drop") %>% filter(votes > 0)
  lab <- paste0("mimanual_", yr)
  long <- finalize_long(raw, lab); save_long(long, paste0("he_", lab))
  shares <- derive_shares(long) %>% transmute(state = "MICHIGAN", year, cty_fips, sample, demovote, repuvote, totalvote); stopifnot(nrow(shares) == 83, median(shares$demovote + shares$repuvote) > 0.2)
  f <- file.path(OUTPUT_DIR, paste0("elect_he_cty_", lab, ".rds")); saveRDS(shares, f); r <- check_long_vs_source(long, f); stopifnot(all(r$pass))
  message(yr, ": ", nrow(shares), " counties, ", nrow(long), " long rows, votes ", format(sum(long$votes), big.mark = ","))
}
