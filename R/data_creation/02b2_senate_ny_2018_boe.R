## New York 2018 U.S. Senate, counties missing from MEDSL (Jefferson 36045, Niagara 36063, Ulster 36111), from the New York State Board of Elections' Elections Database
## (https://results.elections.ny.gov/, an Elstats platform), contest 596 "United States Senator", General Election 2018-11-06. The county table was fetched through the site's own
## download endpoint (GET /api/download_contest/596_table.csv?split_party=false, X-Elstats-Tenant: ny) from inside a browser session (the site returns HTTP 403 to curl) and saved as
## R/data/raw_house_county_open_states/ny_elections/contest_596_senate_2018.csv (all 62 counties). (Until 2026-09-23 these three counties came from America Votes 33, CQ Press;
## the official table agrees with the book in all 62 counties, so the numbers did not change, only the source.)
## The table gives per county: Gillibrand (Democratic), Farley (Republican), Scattering, Blank, Void and Total Votes. Candidate totals combine ALL party lines (e.g. Gillibrand: Democratic +
## Working Families + Independence + Women's Equality), whereas MEDSL-based rows count the Democratic and Republican LINES only, so these three counties' Democratic/Republican shares are
## slightly higher than a line-based count would give. "Other" = Scattering (blank and void ballots are not candidate votes and are left out).
## Checks: the 62 county rows add up to the printed statewide row (D 4,056,931 / R 1,998,220 / scattering 3,872 / blank 190,794 / void 1,069 / total 6,250,886).
## Outputs: R/output/long/se_nyboe_2018.rds, R/output/elect_se_cty_nyboe_2018.rds (NEW files).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
f <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "ny_elections", "contest_596_senate_2018.csv")
raw_t <- read.csv(f, header = FALSE, skip = 2, stringsAsFactors = FALSE, col.names = c("level", "county", "dem", "rep", "other", "blank", "void", "total"))
st <- raw_t[raw_t$level == "State", ]; av <- raw_t[raw_t$level == "County", ] %>% mutate(county = trimws(county))
stopifnot(nrow(av) == 62, sum(av$rep) == st$rep, sum(av$dem) == st$dem, sum(av$other) == st$other, sum(av$total) == st$total, st$rep == 1998220, st$dem == 4056931, st$other == 3872)
xw <- read.delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab")) %>% filter(state_po == "NY", year == 2020, !is.na(county_fips)) %>% distinct(county_fips, county_name) %>% mutate(county = gsub("[^A-Z ]", "", toupper(county_name)))
av <- av %>% mutate(county = gsub("[^A-Z ]", "", toupper(county))) %>% left_join(xw, by = "county"); stopifnot(!anyNA(av$county_fips))
sel <- av %>% filter(county_fips %in% c(36045, 36063, 36111))
raw <- bind_rows(sel %>% transmute(year = 2018L, county_fips, district = NA_character_, candidate = "Chele Chiavacci Farley", party = "Republican", party_group = "REP", votes = rep),
                 sel %>% transmute(year = 2018L, county_fips, district = NA_character_, candidate = "Kirsten E. Gillibrand", party = "Democratic", party_group = "DEM", votes = dem),
                 sel %>% transmute(year = 2018L, county_fips, district = NA_character_, candidate = "Other candidates", party = "Other", party_group = "OTHER", votes = other))
long <- finalize_long(raw, "nyboe_2018", office = "senate"); long$candidate[long$candidate == "Chele Chiavacci Farley"] <- "Chele Chiavacci Farley"; save_long(long, "se_nyboe_2018")
sh <- derive_shares(long) %>% transmute(state = "NEW YORK", year, cty_fips, sample, demovote, repuvote, totalvote); saveRDS(sh, file.path(OUTPUT_DIR, "elect_se_cty_nyboe_2018.rds")); print(as.data.frame(sh))
stopifnot(all(check_long_vs_source(long, file.path(OUTPUT_DIR, "elect_se_cty_nyboe_2018.rds"))$pass))
## comparison with MEDSL for the other counties (after the FEDERAL VOTES fix)
m <- readRDS(file.path(LONG_DIR, "se_medsl.rds")) %>% filter(year == 2018, state_fips == 36) %>% group_by(county_fips) %>% summarise(m_tot = sum(votes))
cmp <- av %>% mutate(a_tot = rep + dem + other) %>% inner_join(m, by = "county_fips") %>% mutate(r = m_tot / a_tot)
cat("MEDSL vs NY BOE county totals: ", nrow(cmp), "counties; identical or within 0.1%:", sum(abs(cmp$r - 1) < 0.001), "; larger differences:\n"); print(as.data.frame(cmp %>% filter(abs(r - 1) >= 0.001) %>% select(county, a_tot, m_tot, r)))
