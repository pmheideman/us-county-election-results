## Candidate-level LONG table for New York U.S. House (source: elect_he_cty_ny.rds, built by 01o). Same fusion handling as 01o: sum ALL of a candidate's
## ballot lines within (county, district) first, then classify the candidate DEM/REP if that word appears among their party labels. One long row per
## county x district x candidate (the lines are already consolidated, as in the shares build).
## KNOWN DEFECT IN THE ORIGINAL BUILD (found here): Edolphus Towns, Kings County 2004 (NY-10) has labels "REP DEM WOR", so 01o counted his 147,212 votes in BOTH the
## Democratic and the Republican numerators (repuvote 0.2974 vs the true 0.0852 for that key). The long table classifies him DEM only, so exactly ONE key of the
## build (Kings 2004) fails check_long_vs_source(); the long table is the correct one. Display party: "Democratic"/"Republican" for D/R
## candidates, otherwise the candidate's first raw party label.
source(file.path("R", "00_setup.R")); library(readr); source(file.path("R", "long_helpers.R"))
save_step <- function(df, name) invisible(df)
RAW <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "new_york")
ny_fips <- read_delim(file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name))) %>% filter(state == "NEW YORK") %>% select(county_name, county_fips)
NY_COUNTY_ALIAS <- c(GENESSEE = "GENESEE")
clean_county <- function(x) { x <- toupper(trimws(x)); coalesce(NY_COUNTY_ALIAS[x], x) }
collapse_fusion <- function(df) {
  df %>% mutate(county = clean_county(county), votes = as.numeric(votes)) %>%
    filter(!is.na(votes), county != "", county != "TOTAL", !toupper(trimws(candidate)) %in% c("TOTAL", "BALLOTS CAST", "OVER VOTES", "UNDER VOTES")) %>%
    group_by(county, district, candidate) %>%
    summarise(votes = sum(votes, na.rm = TRUE), party_labels = paste(party, collapse = " "), first_label = first(party), .groups = "drop") %>%
    mutate(is_dem = grepl("\\bDEM\\b|Democratic", party_labels, ignore.case = TRUE), is_rep = grepl("\\bREP\\b|Republican", party_labels, ignore.case = TRUE))
}
files <- list(`2000` = "2000_general.csv", `2004` = "2004_general.csv", `2006` = "2006_general.csv", `2014` = "2014_general.csv", `2012` = "2012_precinct.csv")
ny <- purrr::imap_dfr(files, function(f, y) collapse_fusion(read_csv(file.path(RAW, f), show_col_types = FALSE, col_types = cols(.default = "c")) %>% filter(office == "U.S. House")) %>% mutate(year = as.integer(y))) %>%
  inner_join(ny_fips, by = c("county" = "county_name"))
message("candidates flagged BOTH dem and rep (would break single party_group): ", sum(ny$is_dem & ny$is_rep))
print(as.data.frame(ny %>% filter(is_dem & is_rep) %>% select(year, county, district, candidate, votes, party_labels)))
df <- ny %>% transmute(year, county_fips, district, candidate, votes,
                       party_group = ifelse(is_dem, "DEM", ifelse(is_rep, "REP", "OTHER")),   # a candidate flagged BOTH counts as DEM here (see note below)
                       party = ifelse(is_dem, "Democratic", ifelse(is_rep, "Republican", first_label)))
long <- finalize_long(df, "ny"); save_long(long, "he_ny")
res <- check_long_vs_source(long, file.path(OUTPUT_DIR, "elect_he_cty_ny.rds")); print(res)
message("NA district rows: ", sum(is.na(long$district)), " of ", nrow(long), "; candidates: ", n_distinct(long$year, long$district, long$candidate))
