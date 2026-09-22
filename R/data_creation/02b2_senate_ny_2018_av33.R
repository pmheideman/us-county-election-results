## New York 2018 U.S. Senate, counties missing from MEDSL (Jefferson 36045, Niagara 36063, Ulster 36111), from the county table "SENATOR 2018" of America Votes 33 (2017-2018; CQ Press; PDF
## R/data/county_house_files/america_votes_2018.pdf, book pp. 268-269 = PDF pp. 290-291; transcription R/data/raw_house_county_open_states/america_votes/ny_senate_2018_county.csv, all 62 counties).
## The book gives per county: total vote, Republican (Farley), Democratic (Gillibrand) and Other. Candidate totals combine ALL party lines (e.g. Gillibrand: Democratic + Working Families + Independence +
## Women's Equality), whereas MEDSL-based rows count the Democratic and Republican LINES only, so these three counties' Democratic/Republican shares are slightly higher than a line-based count would give.
## Checks: the 62 rows add up to the printed statewide TOTAL (R 1,998,220 / D 4,056,931 / other 3,872 / total 6,059,023); the 59 counties present in MEDSL agree on the county total (except the two problems found below).
## Ulster's total is blank in the OCR; it is R + D + Other (77,618) and matches the printed percentages (35.3% / 64.6%).
## Outputs: R/output/long/se_av33_2018.rds, R/output/elect_se_cty_av33_2018.rds (NEW files).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
D <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "america_votes"); dir.create(D, showWarnings = FALSE, recursive = TRUE)
f <- file.path(D, "ny_senate_2018_county.csv")
if (!file.exists(f)) file.copy("/tmp/claude-1000/-home-paul-Stats-substack-projects-immigration/831fbe11-26b0-4ba2-9f03-261a9fbdf053/scratchpad/av/ny_av.csv", f)
av <- read.csv(f, stringsAsFactors = FALSE)
stopifnot(nrow(av) == 62, sum(av$rep) == 1998220, sum(av$dem) == 4056931, sum(av$other) == 3872)
xw <- read.delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab")) %>% filter(state_po == "NY", year == 2020, !is.na(county_fips)) %>% distinct(county_fips, county_name) %>% mutate(county = gsub("[^A-Z ]", "", toupper(county_name)))
av <- av %>% mutate(county = gsub("[^A-Z ]", "", toupper(county))) %>% left_join(xw, by = "county"); stopifnot(!anyNA(av$county_fips))
sel <- av %>% filter(county_fips %in% c(36045, 36063, 36111))
raw <- bind_rows(sel %>% transmute(year = 2018L, county_fips, district = NA_character_, candidate = "Chele Chiavacci Farley", party = "Republican", party_group = "REP", votes = rep),
                 sel %>% transmute(year = 2018L, county_fips, district = NA_character_, candidate = "Kirsten E. Gillibrand", party = "Democratic", party_group = "DEM", votes = dem),
                 sel %>% transmute(year = 2018L, county_fips, district = NA_character_, candidate = "Other candidates", party = "Other", party_group = "OTHER", votes = other))
long <- finalize_long(raw, "av33_2018", office = "senate"); long$candidate[long$candidate == "Chele Chiavacci Farley"] <- "Chele Chiavacci Farley"; save_long(long, "se_av33_2018")
sh <- derive_shares(long) %>% transmute(state = "NEW YORK", year, cty_fips, sample, demovote, repuvote, totalvote); saveRDS(sh, file.path(OUTPUT_DIR, "elect_se_cty_av33_2018.rds")); print(as.data.frame(sh))
stopifnot(all(check_long_vs_source(long, file.path(OUTPUT_DIR, "elect_se_cty_av33_2018.rds"))$pass))
## comparison with MEDSL for the other counties (after the FEDERAL VOTES fix)
m <- readRDS(file.path(LONG_DIR, "se_medsl.rds")) %>% filter(year == 2018, state_fips == 36) %>% group_by(county_fips) %>% summarise(m_tot = sum(votes))
cmp <- av %>% mutate(a_tot = rep + dem + other) %>% inner_join(m, by = "county_fips") %>% mutate(r = m_tot / a_tot)
cat("MEDSL vs America Votes county totals: ", nrow(cmp), "counties; identical or within 0.1%:", sum(abs(cmp$r - 1) < 0.001), "; larger differences:\n"); print(as.data.frame(cmp %>% filter(abs(r - 1) >= 0.001) %>% select(county, a_tot, m_tot, r)))
