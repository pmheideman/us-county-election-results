## South Carolina U.S. House 2010, county level, from the State Election Commission's election-night-reporting archive for the 2010 general election
## (http://www.enr-scvotes.org/SC/19077/39934/, "detailxls.zip" -> detail.xls: a SpreadsheetML workbook with one sheet per contest; sheets "U.S. House of Representatives District 1..6": counties x candidates,
## "Total Votes" columns, a Totals row). Saved under R/data/raw_house_county_open_states/south_carolina_2010/ and read into us_house_2010_county_district.csv (sums tie to each sheet's Totals row).
## The file has no party column: Democratic and Republican nominees are identified from the FEC's official 2010 results (all six district D and R totals equal the FEC's): R = Tim Scott, Joe Wilson, Jeff Duncan,
## Trey Gowdy, Mick Mulvaney, Jim Pratt; D = Ben Frasier, Rob Miller, Jane Ballard Dyer (her two ballot lines are both counted DEM, the FEC combines them), Paul Corden, John Spratt, James E. (Jim) Clyburn;
## every other candidate (and write-ins) is OTHER. A county split between districts appears in each district's sheet and is summed.
## Outputs: R/output/long/he_scenr_2010.rds and R/output/elect_he_cty_scenr_2010.rds (folded in by 01fv_southcarolina_apply.R).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
d <- read_csv(file.path(PROJECT_ROOT, "R/data/raw_house_county_open_states/south_carolina_2010/us_house_2010_county_district.csv"), show_col_types = FALSE, col_types = cols(.default = "c")) %>%
  mutate(year = as.integer(year), votes = as.numeric(votes), district = sprintf("%02d", as.integer(district)), ckey = gsub("[^A-Z]", "", toupper(county)),
         candidate = ifelse(toupper(candidate) == "WRITE-IN", "Write-In", candidate))
REP <- c("Tim Scott", "Joe Wilson", "Jeff Duncan", "Trey Gowdy", "Mick Mulvaney", "Jim Pratt"); DEM <- c("Ben Frasier", "Rob Miller", "Jane Ballard Dyer", "Paul Corden", "John Spratt", "James E Jim Clyburn")
sc <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>% filter(state == "SOUTH CAROLINA", !is.na(county_fips), county_fips > 45000, county_fips < 46000) %>% transmute(county_fips, ckey = gsub("[^A-Z]", "", toupper(county_name))) %>% distinct(ckey, .keep_all = TRUE)
stopifnot(n_distinct(sc$county_fips) == 46, all(d$ckey %in% sc$ckey), all(c(REP, DEM) %in% d$candidate))
raw <- d %>% inner_join(sc, by = "ckey") %>% mutate(party_group = case_when(candidate %in% DEM ~ "DEM", candidate %in% REP ~ "REP", TRUE ~ "OTHER"), party = case_when(party_group == "DEM" ~ "Democratic", party_group == "REP" ~ "Republican", TRUE ~ "Other"),
  candidate = ifelse(candidate == "James E Jim Clyburn", "James E. (Jim) Clyburn", candidate)) %>% group_by(year, county_fips, district, candidate, party, party_group) %>% summarise(votes = sum(votes), .groups = "drop") %>% filter(votes > 0)
long <- finalize_long(raw, "scenr_2010"); save_long(long, "he_scenr_2010")
shares <- derive_shares(long) %>% transmute(state = "SOUTH CAROLINA", year, cty_fips, sample, demovote, repuvote, totalvote); stopifnot(nrow(shares) == 46)
saveRDS(shares, file.path(OUTPUT_DIR, "elect_he_cty_scenr_2010.rds")); r <- check_long_vs_source(long, file.path(OUTPUT_DIR, "elect_he_cty_scenr_2010.rds")); stopifnot(all(r$pass))
message("2010: ", nrow(shares), " counties, ", nrow(long), " long rows, votes ", format(sum(long$votes), big.mark = ","))
