## New Hampshire U.S. House, county level, 1994, 1998, 2002, 2004, 2008, from the NH Department of State's Manual for the General Court (1995, 1999, 2003,
## 2005, 2009 editions: 'U.S. House' tables by town, found by a source search, R/data/county_house_files/source_search/NH_1994_2008.md). Towns transcribed
## and summed to counties by 01jh (1994, 1998), 01ji (2002, 2004), 01jj (2008) *_transcribe.py, which stop unless every candidate's town sum equals the
## printed Totals and the official results. Split counties summed over districts. Party: D -> DEM, R -> REP, else OTHER. Builds every year whose CSV exists.
## Outputs per year: he_nhmanual_<year> long and elect_he_cty_nhmanual_<year> (folded in by 01jo_new_hampshire_1994_2008_apply.R).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
nh <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>% filter(state == "NEW HAMPSHIRE", !is.na(county_fips), county_fips > 33000, county_fips < 34000) %>%
  transmute(county_fips, ckey = gsub("[^A-Z]", "", toupper(county_name))) %>% distinct(ckey, .keep_all = TRUE)
stopifnot(nrow(nh) == 10)
for (yr in c(1994L, 1998L, 2002L, 2004L, 2008L)) {
  f_in <- file.path(PROJECT_ROOT, sprintf("R/data/county_house_files/new_hampshire/new_hampshire_house_county_%d.csv", yr))
  if (!file.exists(f_in)) { message(yr, ": no CSV yet, skipped"); next }
  d <- read_csv(f_in, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    mutate(year = as.integer(year), votes = as.numeric(votes), district = sprintf("%02d", as.integer(district)), ckey = gsub("[^A-Z]", "", toupper(county)))
  stopifnot(all(d$ckey %in% nh$ckey), n_distinct(d$ckey) == 10)
  raw <- d %>% inner_join(nh, by = "ckey") %>% transmute(year, county_fips, district, candidate, party = case_when(party_code == "D" ~ "Democratic", party_code == "R" ~ "Republican", TRUE ~ "Other"),
    party_group = case_when(party_code == "D" ~ "DEM", party_code == "R" ~ "REP", TRUE ~ "OTHER"), votes) %>% group_by(year, county_fips, district, candidate, party, party_group) %>% summarise(votes = sum(votes), .groups = "drop") %>% filter(votes > 0)
  lab <- paste0("nhmanual_", yr)
  long <- finalize_long(raw, lab); save_long(long, paste0("he_", lab))
  shares <- derive_shares(long) %>% transmute(state = "NEW HAMPSHIRE", year, cty_fips, sample, demovote, repuvote, totalvote); stopifnot(nrow(shares) == 10, median(shares$demovote + shares$repuvote) > 0.2)
  f <- file.path(OUTPUT_DIR, paste0("elect_he_cty_", lab, ".rds")); saveRDS(shares, f); r <- check_long_vs_source(long, f); stopifnot(all(r$pass))
  message(yr, ": ", nrow(shares), " counties, ", nrow(long), " long rows, votes ", format(sum(long$votes), big.mark = ","))
}
