## Candidate-level LONG table for Louisiana (he_cty_la_sos 1990-2014 and he_cty_la_sos_2024), from la_sos_house_parish_long.rds (all rounds,
## built by 01bx). Only the DECISIVE-round rows the shares used are kept, by the same rule as 01bx: drop closed party primaries
## (2008-10-04, 2010-08-28, 2010-10-02), then per (year, district) take the latest-dated race. stage = "runoff" when that district-year had an
## earlier non-closed round (the decisive race was a later runoff), else "general" (decided outright in the open/general round).
## party_group as in 01bx: tag DEM / REP -> groups, all else OTHER. Acceptance: derived shares == elect_he_cty_la_sos.rds (years <= 2014)
## and == elect_he_cty_la_sos_2024.rds (2024).
source(file.path("R", "00_setup.R")); library(readr); source(file.path("R", "long_helpers.R"))
save_step <- function(df, name) invisible(df)
l <- readRDS(file.path(OUTPUT_DIR, "la_sos_house_parish_long.rds"))
## the saved file predates 01bx's parish -> FIPS join, so repeat it (letters-only name key against the crosswalk, as 01bx does)
norm <- function(x) gsub("[^A-Z]", "", toupper(x))
la_fips <- read_delim(file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  filter(state == "LOUISIANA", !is.na(county_fips)) %>% distinct(county_name, county_fips) %>% mutate(key = norm(county_name)) %>% distinct(key, county_fips)
stopifnot(nrow(la_fips) == 64, !anyDuplicated(la_fips$key))
l <- l %>% mutate(key = norm(parish)) %>% left_join(la_fips, by = "key"); stopifnot(!anyNA(l$county_fips))
CLOSED <- c("20081004", "20100828", "20101002")
races <- l %>% group_by(date, race_id, year, district) %>% summarise(.groups = "drop") %>% filter(!date %in% CLOSED)
decisive <- races %>% group_by(year, district) %>% mutate(n_rounds = n(), first_date = min(date)) %>% filter(date == max(date)) %>% ungroup() %>%
  mutate(stage = ifelse(n_rounds > 1, "runoff", "general"))
stopifnot(!anyDuplicated(decisive[, c("year", "district")]))
lab <- c(DEM = "Democratic", REP = "Republican", OTH = "Other", IND = "Independent", LBT = "Libertarian", GRN = "Green", NOPTY = "Nonpartisan")
raw <- l %>% inner_join(decisive %>% select(race_id, stage), by = "race_id") %>%
  transmute(year, county_fips = as.integer(county_fips), district, candidate, party_group = case_when(party == "DEM" ~ "DEM", party == "REP" ~ "REP", TRUE ~ "OTHER"),
            party = unname(lab[party]), votes = as.numeric(votes), stage)
long <- finalize_long(raw, "la_sos"); save_long(long, "he_la_sos")
message("LA long rows: ", nrow(long), " | stage counts:"); print(table(long$stage))
check_long_vs_source(long %>% filter(year <= 2014), file.path(OUTPUT_DIR, "elect_he_cty_la_sos.rds"))
check_long_vs_source(long %>% filter(year == 2024), file.path(OUTPUT_DIR, "elect_he_cty_la_sos_2024.rds"))
