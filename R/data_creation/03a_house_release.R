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
##   excludes_unopposed_seat (summary file) part of the county lies in a district whose unopposed winner was not on the ballot (FL/LA/OK) or not
##                           tabulated (AR); the county's totals cover only its contested districts (see us_county_results_no_ballot.csv)
## gap reasons: unopposed_no_ballot (every blank county lies in seats whose unopposed winner was not on the ballot; built by 03c_house_no_ballot.R), source_not_found (no county-level source yet; may include some unopposed seats),
##   partial (some counties or districts missing).

source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R")); source(file.path("R", "source_tokens.R"))
library(readr)
REL <- file.path(PROJECT_ROOT, "release", "v0.1.0-house"); dir.create(REL, showWarnings = FALSE, recursive = TRUE)

long <- readRDS(file.path(LONG_DIR, "house_long_all.rds"))
summ <- readRDS(file.path(LONG_DIR, "house_county_summary.rds"))

## ---- names: county and state ---------------------------------------------------------------------------------------------------------------
xw <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  filter(!is.na(county_fips)) %>% distinct(county_fips, county_name, state, state_po)
## MEDSL's own presidential crosswalk carries two more Kansas City, MO pseudo-county rows (36000 used in 2024, 2938000 in 2000-2020) -- like the
## House bucket 29380 below, these aren't real counties. Drop them here (before cty is built from xw) so they don't get treated as a legitimate
## county downstream; previously only excluded from the n_expected count (see corrections log 2026-09-22), which let them leak into the released
## summary/long files as a phantom "Kansas City" county.
xw <- xw %>% filter(!county_fips %in% c(36000, 2938000))
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
## Mississippi 1990/1992 (added 2026-09-22): the county-level sum for one specific candidate in three district-years exceeds the correct
## (FEC-certified) total by a suspiciously round amount (1990 D2 Espy +4,000; 1992 D2 Espy +2,002; 1992 D5 Taylor +2,000) that survived two
## independent transcription passes plus a dedicated FEC-reconciliation pass without locating a specific bad county cell -- see
## data_corrections_log.csv. Every county's row for that candidate/year is flagged, not just one, since which specific county holds the
## excess is unknown.
infl <- (long$source == "medsl" & long$county_fips == 34003 & long$year == 2024) |
  (long$source == "ms_1990" & long$year == 1990 & long$candidate == "Mike Espy") |
  (long$source == "ms_1992" & long$year == 1992 & long$candidate %in% c("Mike Espy", "Gene Taylor"))
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
## no-ballot seats (unopposed, not on the ballot / not tabulated) and their counties: built by 03c_house_no_ballot.R, which must run first
nbc <- read_csv(file.path(OUTPUT_DIR, "house_no_ballot_counties.csv"), col_types = cols(county_fips = col_double(), whole_county = col_logical(), .default = col_character())) %>% mutate(year = as.numeric(year))
## county with returns that ALSO lies partly in a no-ballot seat: its totals leave out that seat (usually a safe seat, so shares lean against its party)
nb_k <- nbc %>% filter(!whole_county) %>% distinct(year, county_fips) %>% mutate(nbs = TRUE)
out_sum <- summ %>% left_join(cty %>% select(county_fips, county_name, state_name = state, state_po), by = c("county_fips")) %>%
  left_join(infl_k, by = c("year", "county_fips")) %>% left_join(ph_k, by = c("year", "county_fips")) %>% left_join(sn_k, by = c("year", "county_fips")) %>% left_join(nb_k, by = c("year", "county_fips")) %>%
  mutate(office = "house",
         quality_flag = sub(";$", "", paste0(ifelse(!is.na(ph), "placeholder_name;", ""), ifelse(!is.na(sn), "surname_only;", ""), ifelse(!is.na(infl), "totals_possibly_inflated;", ""),
                                            ifelse(dem_votes == 0 | rep_votes == 0, "one_party_race;", ""), ifelse(!is.na(nbs), "excludes_unopposed_seat;", "")))) %>%
  transmute(year, office, state_fips = sprintf("%02d", state_fips), state = state_name, state_po, county_fips = sprintf("%05d", county_fips), county_name,
            n_districts, dem_votes, rep_votes, other_votes, total_votes, dem_two_party_share = round(dem_two_party_share, 6),
            rep_share_of_total = round(rep_share_of_total, 6), status = "covered", quality_flag) %>% arrange(year, county_fips)
write_csv(out_sum, file.path(REL, "us_county_results_summary.csv"), na = "")

## ---- gaps: state-year coverage with a reason ------------------------------------------------------------------------------------------------------------------
cov <- read.csv(file.path(PROJECT_ROOT, "house_results_coverage.csv"), stringsAsFactors = FALSE) %>%
  filter(year >= 1990, !state %in% c("ALASKA", "HAWAII"), county_status != "full")
nb_seats <- nbc %>% distinct(year, state_po, district, candidate) %>% arrange(year, state_po, district) %>% group_by(year, state_po) %>%
  summarise(seats = paste0(ifelse(n() > 1, "districts ", "district "), paste0(as.integer(district), " (", candidate, ")", collapse = ", "), " unopposed, ",
                                ifelse(state_po[1] == "AR", "votes not tabulated", "not on the ballot")), .groups = "drop") %>%
  left_join(cty %>% distinct(state_po, state = toupper(state)), by = "state_po")
nb_blank <- nbc %>% filter(whole_county) %>% distinct(year, county_fips)
n_expected <- xw %>% group_by(state) %>% summarise(counties_expected = n_distinct(county_fips), .groups = "drop") %>% mutate(state = toupper(state))
covered_n <- out_sum %>% group_by(state = toupper(state), year) %>% summarise(counties_covered = n_distinct(county_fips), .groups = "drop")
## a state-year is unopposed_no_ballot when every county without returns lies wholly in no-ballot seats; when only some do, it stays partial/none and the note says both
blank_n <- cov %>% distinct(state, year) %>% left_join(xw %>% distinct(state = toupper(state), county_fips), by = "state", relationship = "many-to-many") %>%
  anti_join(out_sum %>% distinct(year = as.numeric(year), county_fips = as.numeric(county_fips)), by = c("year", "county_fips")) %>% mutate(nb = paste(year, county_fips) %in% paste(nb_blank$year, nb_blank$county_fips)) %>%
  group_by(state, year) %>% summarise(n_blank = n(), n_nb = sum(nb), .groups = "drop")
gaps <- cov %>% left_join(nb_seats %>% select(state, year, seats), by = c("state", "year")) %>% left_join(blank_n, by = c("state", "year")) %>% left_join(n_expected, by = "state") %>% left_join(covered_n, by = c("state", "year")) %>%
  mutate(counties_covered = ifelse(is.na(counties_covered), 0L, counties_covered), n_nb = ifelse(is.na(n_nb), 0L, n_nb),
         gap_reason = case_when(n_nb > 0 & n_nb == n_blank ~ "unopposed_no_ballot", county_status == "partial" ~ "partial", TRUE ~ "source_not_found"),
         note = case_when(gap_reason == "unopposed_no_ballot" ~ seats,
                          n_nb > 0 ~ paste0(seats, " (", n_nb, ifelse(n_nb == 1, " county", " counties"), "); ", n_blank - n_nb, ifelse(n_blank - n_nb == 1, " other county", " other counties"), " missing data"),
                          gap_reason == "source_not_found" ~ "no county-level source found yet; may include unopposed seats", TRUE ~ "")) %>%
  transmute(office = "house", state, year, counties_covered, counties_expected, coverage_pct = round(county_coverage_pct, 3), status = county_status, gap_reason, note) %>%
  arrange(state, year)
write_csv(gaps, file.path(REL, "us_county_results_gaps.csv"), na = "")

message("long rows: ", nrow(out_long), " | summary rows: ", nrow(out_sum), " | gaps rows: ", nrow(gaps))
message("long flags:"); print(as.data.frame(sort(table(ifelse(out_long$quality_flag == "", "(none)", out_long$quality_flag)), decreasing = TRUE)))
message("gap reasons:"); print(as.data.frame(table(gaps$gap_reason)))
message("years: ", paste(range(out_long$year), collapse = "-"), " | states: ", n_distinct(out_long$state_fips), " | counties: ", n_distinct(out_long$county_fips))
fl <- list.files(REL, full.names = TRUE); message("file sizes (MB): ", paste(basename(fl), round(file.size(fl) / 1e6, 1), collapse = "; "))
