## North Carolina U.S. House, county level, 1996 and 1998, from the State Board of Elections' archived result PDFs (parsed by 01fr_northcarolina_1996_1998_parse.py into
## R/data/county_house_files/north_carolina/nc_house_county_1996_1998.csv; every district's county rows tie to the printed Total / GRAND TOTALS). A county split between districts appears in each and is summed.
## 1998 names are printed "Last, First" and are turned into "First Last". Party: D -> DEM, R -> REP, everything else (L, NL, WI) OTHER.
## Outputs: R/output/long/he_ncsbe_<year>.rds and R/output/elect_he_cty_ncsbe_<year>.rds (folded in by 01ft_northcarolina_apply.R).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
d <- read_csv(file.path(PROJECT_ROOT, "R/data/county_house_files/north_carolina/nc_house_county_1996_1998.csv"), show_col_types = FALSE, col_types = cols(.default = "c")) %>%
  mutate(year = as.integer(year), votes = as.numeric(votes), district = sprintf("%02d", as.integer(district)), ckey = gsub("[^A-Z]", "", toupper(county)),
         candidate = ifelse(grepl(",", candidate), gsub("\\s+", " ", sub("^(.*?),\\s*(.*)$", "\\2 \\1", candidate)), candidate))
nc <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>% filter(state == "NORTH CAROLINA", !is.na(county_fips), county_fips > 37000, county_fips < 38000) %>% transmute(county_fips, ckey = gsub("[^A-Z]", "", toupper(county_name))) %>% distinct(ckey, .keep_all = TRUE)
stopifnot(n_distinct(nc$county_fips) == 100, all(d$ckey %in% nc$ckey))
raw <- d %>% inner_join(nc, by = "ckey") %>% transmute(year, county_fips, district, candidate, party = case_when(party_code == "D" ~ "Democratic", party_code == "R" ~ "Republican", party_code == "L" ~ "Libertarian", party_code == "NL" ~ "Natural Law", party_code == "WI" ~ "Write-In", TRUE ~ party_code),
  party_group = case_when(party_code == "D" ~ "DEM", party_code == "R" ~ "REP", TRUE ~ "OTHER"), votes) %>% group_by(year, county_fips, district, candidate, party, party_group) %>% summarise(votes = sum(votes), .groups = "drop") %>% filter(votes > 0)
for (y in sort(unique(raw$year))) {
  long <- finalize_long(raw %>% filter(year == y), paste0("ncsbe_", y)); save_long(long, paste0("he_ncsbe_", y))
  shares <- derive_shares(long) %>% transmute(state = "NORTH CAROLINA", year, cty_fips, sample, demovote, repuvote, totalvote); stopifnot(nrow(shares) == 100)
  saveRDS(shares, file.path(OUTPUT_DIR, sprintf("elect_he_cty_ncsbe_%d.rds", y))); r <- check_long_vs_source(long, file.path(OUTPUT_DIR, sprintf("elect_he_cty_ncsbe_%d.rds", y))); stopifnot(all(r$pass))
  message(y, ": ", nrow(shares), " counties, ", nrow(long), " long rows, votes ", format(sum(long$votes), big.mark = ","))
}
