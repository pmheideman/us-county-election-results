## Arkansas 1990 and 2006 U.S. House, county level, from the official Arkansas Secretary of State election books (scanned images, no text layer):
##   R/data/county_house_files/AR_1990_Election_results.pdf  (pp. 50, 52, 54, 56: "1990 ARKANSAS GENERAL ELECTION - U.S. REPRESENTATIVE nTH DISTRICT")
##   R/data/county_house_files/AR_2006_PP__GE_Election_Results.pdf (pp. 47-49: "U.S. Congress Candidates", November 7 2006)
## The page images were READ BY HAND (OCR only located the pages) into R/data/raw_house_county_open_states/arkansas_official/ar_{1990,2006}_transcription.csv.
## Every district was checked against the PRINTED totals: for each county dem + rep == printed total votes, and the county sums, the county count
## and (1990) the printed statewide district totals are reproduced exactly (see the checks below). One candidate per party in each district; no third-party candidates
## and no split counties in either year (counties lie wholly in one district).
## Outputs: R/output/long/he_ar_1990.rds, he_ar_2006.rds and R/output/elect_he_cty_ar_1990.rds, elect_he_cty_ar_2006.rds (NEW files).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "arkansas_official")
xw <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  filter(state == "ARKANSAS", !is.na(county_fips)) %>% distinct(county_name, county_fips)
norm <- function(z) gsub("[^A-Z]", "", toupper(z)); xw <- xw %>% mutate(key = norm(county_name)) %>% distinct(key, county_fips); stopifnot(nrow(xw) == 75)

cands <- list(
  `1990` = tribble(~district, ~dem, ~rep,
    "01", "Bill Alexander", "Terry Hayes", "02", "Ray Thornton", "Jim Keet",
    "03", "Dan Ivy", "John Paul Hammerschmidt", "04", "Beryl Franklin Anthony Jr.", "Roy Rood"),
  `2006` = tribble(~district, ~dem, ~rep,
    "01", "Marion Berry", "Mickey Stumbaugh", "02", "Vic Snyder", "Andy Mayberry",
    "03", "Woodrow Anderson", "John Boozman", "04", "Mike Ross", "Joe Ross"))
## printed district totals: dem, rep, total, counties
printed <- list(
  `1990` = list(`01` = c(101026, 56071, 157097, 24), `02` = c(103471, 67800, 171271, 8), `03` = c(54332, 129876, 184208, 20), `04` = c(110365, 42130, 152495, 23)),
  `2006` = list(`01` = c(127577, 56611, 184188, 26), `02` = c(124871, 81432, 206303, 8), `03` = c(75885, 125039, 200924, 12), `04` = c(128236, 43360, 171596, 29)))

for (yr in c(1990L, 2006L)) {
  t <- read_csv(file.path(DIR, sprintf("ar_%d_transcription.csv", yr)), col_types = cols(district = "c", county = "c", .default = "i")) %>%
    mutate(district = sprintf("%02d", as.integer(district)), county_fips = xw$county_fips[match(norm(county), xw$key)])
  stopifnot(!anyNA(t$county_fips), n_distinct(t$county_fips) == 75, nrow(t) == 75, all(t$dem_votes + t$rep_votes == t$total_votes))
  for (d in names(printed[[as.character(yr)]])) {
    x <- t %>% filter(district == d); p <- printed[[as.character(yr)]][[d]]
    stopifnot(sum(x$dem_votes) == p[1], sum(x$rep_votes) == p[2], sum(x$total_votes) == p[3], nrow(x) == p[4])
  }
  cat(yr, ": all 4 districts tie exactly to the printed totals (dem, rep, total, county count); every row dem + rep == printed total\n")
  cn <- cands[[as.character(yr)]]
  raw <- bind_rows(
    t %>% left_join(cn, by = "district") %>% transmute(year = yr, county_fips, district, candidate = dem, party = "Democratic", party_group = "DEM", votes = dem_votes),
    t %>% left_join(cn, by = "district") %>% transmute(year = yr, county_fips, district, candidate = rep, party = "Republican", party_group = "REP", votes = rep_votes))
  long <- finalize_long(raw, paste0("ar_", yr))
  save_long(long, paste0("he_ar_", yr))
  shares <- derive_shares(long) %>% transmute(state = "ARKANSAS", year, cty_fips, sample, demovote, repuvote, totalvote)
  saveRDS(shares, file.path(OUTPUT_DIR, sprintf("elect_he_cty_ar_%d.rds", yr)))
  print(check_long_vs_source(long, file.path(OUTPUT_DIR, sprintf("elect_he_cty_ar_%d.rds", yr))))
  pe <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")) %>% filter(sample == "PE", year == ifelse(yr == 1990, 1992, 2004)) %>% select(cty_fips, pe = totalvote)
  r <- shares %>% left_join(pe, by = "cty_fips") %>% mutate(ratio = totalvote / pe)
  cat("House", yr, "/ nearest presidential total: min", round(min(r$ratio, na.rm = TRUE), 2), "median", round(median(r$ratio, na.rm = TRUE), 2), "max", round(max(r$ratio, na.rm = TRUE), 2), "\n")
}
