## First House release files (v0.1.0-house): candidate-level long CSV, county summary CSV, gaps CSV -> release/v0.1.0-house/.
## Inputs: R/output/long/house_long_all.rds (assembled, master-checked against the panel), house_county_summary.rds, house_results_coverage.csv.
## Scope (docs/DECISIONS.md): U.S. House, 1990+, 48 states (no AK, HI, DC), regular general elections, final results.
## Files keep the generic names decided for the full release (office column = "house" only, for now).
##
## quality_flag values (semicolon-separated, blank = none known):
##   placeholder_name        candidate name is a generic placeholder ("Democratic Candidate", "Other Candidate 1", "Unnamed ...") because the source printed no name
##   surname_only            the source prints surnames only (Maine 1990/92/2000, NH historical, AZ 1996, KY 2010): shown, but the name is incomplete
##   initials_only           the first name is abbreviated to initials ("H. H. Bateman", Virginia 1990s-2000s)
##   totals_possibly_inflated NJ Bergen 2024 total is 1.86x the presidential total (raw rows look duplicated);
##                            -- shares are probably fine, `votes` totals are not (see data_corrections_log.csv)
##   one_party_race          (summary file) no Democratic or no Republican votes in the county-year
## gap reasons: unopposed_no_ballot (known structural: LA, OK), source_not_found (no county-level source yet; may include some unopposed seats),
##   partial (some counties or districts missing).

source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R")); source(file.path("R", "source_tokens.R"))
library(readr)
REL <- file.path(PROJECT_ROOT, "release", "v0.1.0-house"); dir.create(REL, showWarnings = FALSE, recursive = TRUE)

long <- readRDS(file.path(LONG_DIR, "house_long_all.rds"))
summ <- readRDS(file.path(LONG_DIR, "house_county_summary.rds"))

## ---- names: county and state ---------------------------------------------------------------------------------------------------------------
xw <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  filter(!is.na(county_fips)) %>% distinct(county_fips, county_name, state, state_po)
cty <- xw %>% group_by(county_fips) %>% slice(1) %>% ungroup() %>%
  mutate(county_name = tools::toTitleCase(tolower(county_name)), state = tools::toTitleCase(tolower(state)))
## Virginia: independent cities are listed as "City" (the MEDSL table names some of them without it, e.g. 51515 "Bedford" next to the county 51019 "Bedford"); the two cities that no longer exist are added
cty <- cty %>% mutate(county_name = ifelse(state_po == "VA" & county_fips >= 51510 & !grepl("City$", county_name), paste(county_name, "City"), county_name))
cty <- bind_rows(cty, tibble(county_fips = c(51560, 51780), county_name = c("Clifton Forge City", "South Boston City"), state = "Virginia", state_po = "VA"))
stopifnot(!anyDuplicated(cty$county_fips))
## Non-county buckets in the MEDSL files have no real county FIPS and cannot be mapped: Maine 23000/23099 (federal/UOCAVA ballots), New York
## 36122 (9,540 votes) and Missouri 29380 (Kansas City: 127,894 votes in 2016 that MEDSL assigns to a made-up FIPS instead of Jackson/Clay/Platte,
## so those three counties are UNDERCOUNTED in 2016). They are excluded from the release files and flagged in data_corrections_log.csv.
drop_k <- setdiff(unique(long$county_fips), cty$county_fips)
message("non-county buckets excluded from the release: ", paste(drop_k, collapse = ", "), " (", format(sum(long$votes[long$county_fips %in% drop_k]), big.mark = ","), " votes)")
long <- long %>% filter(county_fips %in% cty$county_fips)
summ <- summ %>% filter(county_fips %in% cty$county_fips)

is_writein <- grepl("^\\[?write[- ]?ins?\\]?\\**$|scatter|^misc|^blank|^void|^over ?votes?|^under ?votes?", long$candidate, ignore.case = TRUE)
## genuine placeholders: generic labels because the source printed no candidate name (CA 1990-96 transcriptions, AZ 1994, KY 2010 pooled columns, unnamed rows)
flag_placeholder <- grepl("(Democratic|Republican|Other|Minor) Candidate( [0-9]+)?$|^Unnamed|Other candidates", long$candidate, ignore.case = TRUE)
## surname-only: the source prints surnames only (Maine 1990/92/2000, NH historical, AZ 1996, ...): shown, but the name is incomplete
flag_surname <- !flag_placeholder & !is_writein & !grepl(" ", long$candidate)
## initials-only first names ("H. H. Bateman"): Virginia 1990s-2000s prints them this way; the name is usable but abbreviated
## (every token before the surname is an initial: "H. H. Bateman" yes, "J. Gresham Barrett" no)
flag_initials <- !flag_placeholder & !is_writein & vapply(strsplit(sub(",? (Jr|Sr|II|III|IV)\\.?$", "", long$candidate), " "), function(t) length(t) >= 2 && all(grepl("^[A-Z]\\.?$", t[-length(t)])), NA)
## (AL/SC/AR/IN 2016 were fixed 2026-09-20: straight-party and "unopposed candidates" rows had been counted as House votes.) Still flagged: Maine 2024, NJ Bergen 2024.
## (Maine 2024 was also flagged until 2026-09-20: its 'Total Ballots Cast' pseudo-row was the cause and is now excluded.) Still flagged: NJ Bergen 2024.
infl <- long$source == "medsl" & long$county_fips == 34003 & long$year == 2024
long <- long %>% mutate(quality_flag = paste0(ifelse(flag_placeholder, "placeholder_name;", ""), ifelse(flag_surname, "surname_only;", ""), ifelse(flag_initials, "initials_only;", ""), ifelse(infl, "totals_possibly_inflated;", "")),
                        quality_flag = sub(";$", "", quality_flag))

out_long <- long %>% left_join(cty %>% select(county_fips, county_name, state_name = state, state_po), by = "county_fips") %>%
  transmute(year, office, state_fips = sprintf("%02d", state_fips), state = state_name, state_po, county_fips = sprintf("%05d", county_fips), county_name,
            district, stage, candidate, party, party_group, votes, source_internal = source, quality_flag) %>%
  mutate(source = release_source_token(source_internal, state_po, year)) %>% select(-source_internal, everything(), source_internal) %>%
  select(year, office, state_fips, state, state_po, county_fips, county_name, district, stage, candidate, party, party_group, votes, source, quality_flag, source_internal) %>%
  arrange(year, county_fips, district, desc(votes))
## SOURCES.csv: one row per release source token (state x origin x year), with the registry's full description, license and the corrections-log entries that touch that state-year
src_tab <- build_sources_table(out_long %>% mutate(source_internal = source_internal), file.path(PROJECT_ROOT, "R", "output", "data_corrections_log.csv")) %>%
  select(source, office, state_po, year, origin, publisher, document, locator, format, obtained, transcription, license, license_status, n_rows, n_counties, n_districts, corrections_log_entries, notes)
write_csv(src_tab, file.path(REL, "SOURCES.csv"), na = "")
write_csv(out_long %>% select(-source_internal), file.path(REL, "us_county_results_long.csv"), na = "")

## ---- summary: cross-district county totals + flags ------------------------------------------------------------------------------------------------
infl_k <- long %>% filter(infl) %>% distinct(year, county_fips) %>% mutate(infl = TRUE)
ph_k <- long %>% filter(flag_placeholder) %>% distinct(year, county_fips) %>% mutate(ph = TRUE)
sn_k <- long %>% filter(flag_surname) %>% distinct(year, county_fips) %>% mutate(sn = TRUE)
out_sum <- summ %>% left_join(cty %>% select(county_fips, county_name, state_name = state, state_po), by = c("county_fips")) %>%
  left_join(infl_k, by = c("year", "county_fips")) %>% left_join(ph_k, by = c("year", "county_fips")) %>% left_join(sn_k, by = c("year", "county_fips")) %>%
  mutate(office = "house",
         quality_flag = sub(";$", "", paste0(ifelse(!is.na(ph), "placeholder_name;", ""), ifelse(!is.na(sn), "surname_only;", ""), ifelse(!is.na(infl), "totals_possibly_inflated;", ""),
                                            ifelse(dem_votes == 0 | rep_votes == 0, "one_party_race;", "")))) %>%
  transmute(year, office, state_fips = sprintf("%02d", state_fips), state = state_name, state_po, county_fips = sprintf("%05d", county_fips), county_name,
            n_districts, dem_votes, rep_votes, other_votes, total_votes, dem_two_party_share = round(dem_two_party_share, 6),
            rep_share_of_total = round(rep_share_of_total, 6), status = "covered", quality_flag) %>% arrange(year, county_fips)
write_csv(out_sum, file.path(REL, "us_county_results_summary.csv"), na = "")

## ---- gaps: state-year coverage with a reason ------------------------------------------------------------------------------------------------------------------
cov <- read.csv(file.path(PROJECT_ROOT, "house_results_coverage.csv"), stringsAsFactors = FALSE) %>%
  filter(year >= 1990, !state %in% c("ALASKA", "HAWAII"), county_status != "full")
unopposed <- tribble(~state, ~year, ~note,
  "LOUISIANA", 1990, "district 6 unopposed", "LOUISIANA", 1996, "districts 1,2,3 unopposed", "LOUISIANA", 1998, "districts 1,3,4,5,7 unopposed",
  "LOUISIANA", 2000, "district 2 unopposed", "LOUISIANA", 2004, "district 4 unopposed", "LOUISIANA", 2008, "districts 3,5 unopposed",
  "LOUISIANA", 2010, "district 7 unopposed", "LOUISIANA", 2022, "district 4 unopposed",
  "ARKANSAS", 1998, "district 1 unopposed (25 counties)", "ARKANSAS", 2000, "district 3 unopposed (16 counties)", "OKLAHOMA", 2010, "district 4 unopposed", "OKLAHOMA", 2014, "district 1 unopposed", "OKLAHOMA", 2016, "district 1 unopposed", "OKLAHOMA", 2024, "district 3 unopposed")
n_expected <- xw %>% filter(!county_fips %in% c(36000, 2938000)) %>% group_by(state) %>% summarise(counties_expected = n_distinct(county_fips), .groups = "drop") %>% mutate(state = toupper(state))
covered_n <- out_sum %>% group_by(state = toupper(state), year) %>% summarise(counties_covered = n_distinct(county_fips), .groups = "drop")
gaps <- cov %>% left_join(unopposed, by = c("state", "year")) %>% left_join(n_expected, by = "state") %>% left_join(covered_n, by = c("state", "year")) %>%
  mutate(counties_covered = ifelse(is.na(counties_covered), 0L, counties_covered),
         gap_reason = case_when(!is.na(note) ~ "unopposed_no_ballot", county_status == "partial" ~ "partial", TRUE ~ "source_not_found"),
         note = ifelse(is.na(note), ifelse(gap_reason == "source_not_found", "no county-level source found yet; may include unopposed seats", ""), note)) %>%
  transmute(office = "house", state, year, counties_covered, counties_expected, coverage_pct = round(county_coverage_pct, 3), status = county_status, gap_reason, note) %>%
  arrange(state, year)
write_csv(gaps, file.path(REL, "us_county_results_gaps.csv"), na = "")

message("long rows: ", nrow(out_long), " | summary rows: ", nrow(out_sum), " | gaps rows: ", nrow(gaps))
message("long flags:"); print(as.data.frame(sort(table(ifelse(out_long$quality_flag == "", "(none)", out_long$quality_flag)), decreasing = TRUE)))
message("gap reasons:"); print(as.data.frame(table(gaps$gap_reason)))
message("years: ", paste(range(out_long$year), collapse = "-"), " | states: ", n_distinct(out_long$state_fips), " | counties: ", n_distinct(out_long$county_fips))
fl <- list.files(REL, full.names = TRUE); message("file sizes (MB): ", paste(basename(fl), round(file.size(fl) / 1e6, 1), collapse = "; "))
