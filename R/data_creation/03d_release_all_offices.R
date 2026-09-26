## Release (v1.0.0; was v0.2.0): U.S. House (as in release/v0.1.0-house, built by 03a) + PRESIDENT + SENATE county results, 1990-2024, 48 states (no AK, HI, DC), regular general elections, final results.
## Inputs: release/v0.1.0-house/* (House files, already release-ready), R/output/long/pe_long_all.rds and se_long_all.rds (02zb_pe_se_long_assemble.R, master-checked against the panel).
## Output: release/v<VERSION>/{us_county_results_long.csv (+ .parquet), us_county_results_summary.csv, us_county_results_gaps.csv, us_county_results_no_ballot.csv, SOURCES.csv, data_corrections_log.csv, DATA_DICTIONARY.md, VERSION}
## President and Senate rows: district is blank (statewide offices). quality_flag values added: other_candidates_aggregated (the source itemizes only the two major-party nominees; all other candidates are one row, see SOURCES.csv).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R")); source(file.path("R", "source_tokens.R")); library(readr)
H <- file.path(PROJECT_ROOT, "release", "v0.1.0-house"); VERSION <- "1.0.0"; REL <- file.path(PROJECT_ROOT, "release", paste0("v", VERSION)); dir.create(REL, showWarnings = FALSE, recursive = TRUE)
cc <- cols(.default = col_character())
hl <- read_csv(file.path(H, "us_county_results_long.csv"), col_types = cc, na = character()); hs <- read_csv(file.path(H, "us_county_results_summary.csv"), col_types = cc, na = character())
hg <- read_csv(file.path(H, "us_county_results_gaps.csv"), col_types = cc, na = character()); hsrc <- read_csv(file.path(H, "SOURCES.csv"), col_types = cc, na = character())

## ---- county / state names (same crosswalk as 03a) ---------------------------------------------------------------------------------------------
xw <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>% filter(!is.na(county_fips)) %>% distinct(county_fips, county_name, state, state_po)
## Kansas City, MO pseudo-county rows (36000 in 2024, 2938000 in 2000-2020; see 03a and corrections log 2026-09-22) -- not real counties, drop before cty is built
xw <- xw %>% filter(!county_fips %in% c(36000, 2938000))
cty <- xw %>% group_by(county_fips) %>% slice(1) %>% ungroup() %>% mutate(county_name = tools::toTitleCase(tolower(county_name)), state = tools::toTitleCase(tolower(state)))
cty <- cty %>% mutate(county_name = ifelse(state_po == "VA" & county_fips >= 51510 & !grepl("City$", county_name), paste(county_name, "City"), county_name))
cty <- bind_rows(cty, tibble(county_fips = c(51560, 51780), county_name = c("Clifton Forge City", "South Boston City"), state = "Virginia", state_po = "VA"))
stopifnot(!anyDuplicated(cty$county_fips))

## ---- President / Senate long rows ------------------------------------------------------------------------------------------------------------------
NAMEFIX <- c("Joseph R Biden Jr" = "Joseph R. Biden Jr.", "Donald J Trump" = "Donald J. Trump", "Donald Trump" = "Donald J. Trump", "Kamala D Harris" = "Kamala D. Harris", "George W. Bush" = "George W. Bush")
mk <- function(f, office) { l <- readRDS(file.path(LONG_DIR, f)); nb <- setdiff(unique(l$county_fips), cty$county_fips)
  nb <- setdiff(nb, 12025L); message(office, ": non-county buckets excluded: ", paste(nb, collapse = ", "), " (", format(sum(l$votes[l$county_fips %in% nb]), big.mark = ","), " votes)")
  ## the historical (CQ / ICPSR) rows and some MEDSL rows use Dade County's old FIPS 12025; Miami-Dade has been 12086 since 1997 (year-specific rows never hold both)
  l$county_fips[l$county_fips == 12025L] <- 12086L; l$state_fips <- l$county_fips %/% 1000L
  l <- l %>% group_by(across(-votes)) %>% summarise(votes = sum(votes), .groups = "drop")
  ## zero-vote artefact rows (a candidate line with 0 votes labelled OTHER next to the real DEM/REP line of the same candidate, e.g. Jefferson KY 2016, Sapraicone NY 2024): keep the row with the votes
  l <- l %>% group_by(year, county_fips, candidate, party) %>% arrange(desc(votes), .by_group = TRUE) %>% filter(n() == 1 | row_number() == 1 | votes > 0) %>% ungroup()
  l <- l %>% group_by(year, county_fips, candidate, party, party_group) %>% summarise(across(everything(), first), votes = sum(votes), .groups = "drop") %>% relocate(names(readRDS(file.path(LONG_DIR, f))))
  l <- l %>% filter(county_fips %in% cty$county_fips); l$candidate <- ifelse(l$candidate %in% names(NAMEFIX), NAMEFIX[l$candidate], l$candidate)
  l$candidate[grepl("^all other candidates", l$candidate, ignore.case = TRUE)] <- "All other candidates (not itemized in the source)"; l$candidate[grepl("^other candidates$", l$candidate, ignore.case = TRUE)] <- "Other candidates"
  l %>% left_join(cty %>% select(county_fips, county_name, state_name = state, state_po), by = "county_fips") %>% mutate(source_internal = source, source = release_source_token(source_internal, state_po, year, office = office),
    quality_flag = ifelse(candidate %in% c("All other candidates (not itemized in the source)", "Other candidates"), "other_candidates_aggregated", "")) }
pl <- mk("pe_long_all.rds", "president"); sl <- mk("se_long_all.rds", "senate")
ps <- bind_rows(pl, sl)
out_ps <- ps %>% transmute(year = as.character(year), office, state_fips = sprintf("%02d", state_fips), state = state_name, state_po, county_fips = sprintf("%05d", county_fips), county_name, district = "", stage,
                            candidate, party, party_group, votes = as.character(votes), source, quality_flag) %>% arrange(office, year, county_fips, desc(as.numeric(votes)))
long_all <- bind_rows(hl, out_ps) %>% arrange(factor(office, c("house", "senate", "president")), as.integer(year), county_fips, district)
write_csv(long_all, file.path(REL, "us_county_results_long.csv"), na = "")
## Parquet copy of the long file: same rows and columns, with year and votes stored as integers (FIPS codes stay character, so leading zeros survive)
nanoparquet::write_parquet(long_all %>% mutate(year = as.integer(year), votes = as.integer(votes)), file.path(REL, "us_county_results_long.parquet"))

## ---- SOURCES.csv ----------------------------------------------------------------------------------------------------------------------------------
psrc <- build_sources_table(ps %>% transmute(year, office, state_po, county_fips, district = NA_character_, source, source_internal), file.path(PROJECT_ROOT, "R", "output", "data_corrections_log.csv")) %>%
  select(source, office, state_po, year, origin, publisher, document, locator, format, obtained, transcription, license, license_status, n_rows, n_counties, n_districts, corrections_log_entries, notes) %>% mutate(across(everything(), as.character))
src_all <- bind_rows(hsrc %>% mutate(across(everything(), as.character)), psrc); write_csv(src_all, file.path(REL, "SOURCES.csv"), na = "")

## ---- summary ---------------------------------------------------------------------------------------------------------------------------------------
sm <- ps %>% group_by(office, year, state_fips, county_fips, county_name, state_name, state_po) %>%
  summarise(dem_votes = sum(votes[party_group == "DEM"]), rep_votes = sum(votes[party_group == "REP"]), other_votes = sum(votes[party_group == "OTHER"]), total_votes = sum(votes), agg = any(quality_flag != ""), .groups = "drop") %>%
  transmute(year = as.character(year), office, state_fips = sprintf("%02d", state_fips), state = state_name, state_po, county_fips = sprintf("%05d", county_fips), county_name, n_districts = "", dem_votes = as.character(dem_votes), rep_votes = as.character(rep_votes),
            other_votes = as.character(other_votes), total_votes = as.character(total_votes), dem_two_party_share = as.character(round(as.numeric(dem_votes) / (as.numeric(dem_votes) + as.numeric(rep_votes)), 6)),
            rep_share_of_total = as.character(round(as.numeric(rep_votes) / as.numeric(total_votes), 6)), status = "covered",
            quality_flag = sub(";$", "", paste0(ifelse(agg, "other_candidates_aggregated;", ""), ifelse(as.numeric(dem_votes) == 0 | as.numeric(rep_votes) == 0, "one_party_race;", ""))))
sum_all <- bind_rows(hs, sm) %>% arrange(factor(office, c("house", "senate", "president")), as.integer(year), county_fips); write_csv(sum_all, file.path(REL, "us_county_results_summary.csv"), na = "")

## ---- gaps for President and Senate ---------------------------------------------------------------------------------------------------------------
n_exp <- ps %>% distinct(office, year, state_po, county_fips) %>% count(office, state_po, year) %>% group_by(office, state_po) %>% summarise(expected = as.integer(names(sort(table(n), decreasing = TRUE))[1]), .groups = "drop")   # the usual county count of the state
states48 <- sort(setdiff(unique(cty$state_po), c("AK", "HI", "DC")))
cover <- ps %>% distinct(office, year, state_po, county_fips) %>% count(office, year, state_po, name = "covered")
pres_grid <- expand.grid(office = "president", year = seq(1992, 2024, 4), state_po = states48, stringsAsFactors = FALSE)
## Senate: state-years with a race in ANY source (general or special), so special-only state-years appear as gaps with their reason
e <- new.env(); load(file.path(PROJECT_ROOT, "R/data/raw_election_historical/us_senate_county_returns_1908_2020.Rdata"), envir = e); sh <- get(setdiff(ls(e), character()), e)
sh <- get(ls(e)[1], e); sen_any <- sh %>% filter(election_year >= 1990, election_year %% 2 == 0, election_year <= 2014) %>% mutate(state_po = as.character(state)) %>% group_by(year = election_year, state_po) %>% summarise(only_special = all(election_type == "S"), .groups = "drop")
sen_grid <- bind_rows(sen_any %>% select(year, state_po, only_special), ps %>% filter(office == "senate", year >= 2016) %>% distinct(year, state_po) %>% mutate(only_special = FALSE)) %>% distinct(year, state_po, .keep_all = TRUE) %>% mutate(office = "senate")
## Regular Senate races follow the three-class schedule (class by year: 1990 mod 6 -> class 2, 1992 mod 6 -> class 3, 1994 mod 6 -> class 1); a scheduled race that no source covers is listed as source_not_found
## (found 2026-09-21: California 2022 is in none of MEDSL's 2022 files).
C1 <- c("AZ","CA","CT","DE","FL","HI","IN","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NJ","NM","NY","ND","OH","PA","RI","TN","TX","UT","VT","VA","WA","WV","WI","WY")
C2 <- c("AL","AK","AR","CO","DE","GA","ID","IL","IA","KS","KY","LA","ME","MA","MI","MN","MS","MT","NE","NH","NJ","NM","NC","OK","OR","RI","SC","SD","TN","TX","VA","WV","WY")
C3 <- c("AL","AK","AZ","AR","CA","CO","CT","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","MD","MO","NV","NH","NY","NC","ND","OH","OK","OR","PA","SC","SD","UT","VT","WA","WI")
stopifnot(length(C1) == 33, length(C2) == 33, length(C3) == 34)
sched <- bind_rows(lapply(seq(1990, 2024, 2), function(y) data.frame(year = y, state_po = switch(as.character((y - 1990) %% 6), "0" = C2, "2" = C3, "4" = C1), only_special = FALSE)))
sen_grid <- bind_rows(sen_grid %>% select(year, state_po, only_special), sched) %>% group_by(year, state_po) %>% summarise(only_special = all(only_special), .groups = "drop") %>% mutate(office = "senate")
sen_grid <- bind_rows(sen_grid %>% filter(!(year == 2020 & state_po == "AZ")), data.frame(year = 2020, state_po = "AZ", only_special = TRUE, office = "senate"))       # Arizona 2020: the only Senate race was a special election (01a exclude_special)
grid <- bind_rows(pres_grid %>% mutate(only_special = FALSE), sen_grid %>% select(office, year, state_po, only_special)) %>% filter(state_po %in% states48)
gp <- grid %>% left_join(cover, by = c("office", "year", "state_po")) %>% left_join(n_exp, by = c("office", "state_po")) %>% mutate(covered = ifelse(is.na(covered), 0L, covered)) %>%
  mutate(status = ifelse(covered == 0, "none", ifelse(covered < expected * 0.98, "partial", "full"))) %>% filter(status != "full") %>%
  mutate(gap_reason = case_when(only_special & covered == 0 ~ "special_election_only", covered == 0 ~ "source_not_found", TRUE ~ "partial"),
         note = case_when(gap_reason == "special_election_only" ~ "the only Senate race that year was a special election (excluded from v1)", gap_reason == "source_not_found" ~ "no county-level source found yet", TRUE ~ "")) %>%
  left_join(cty %>% distinct(state_po, state), by = "state_po") %>%
  transmute(office, state = toupper(state), year = as.character(year), counties_covered = as.character(covered), counties_expected = as.character(expected), coverage_pct = as.character(round(covered / expected, 3)), status, gap_reason, note)
gaps_all <- bind_rows(hg, gp) %>% arrange(factor(office, c("house", "senate", "president")), state, as.integer(year)); write_csv(gaps_all, file.path(REL, "us_county_results_gaps.csv"), na = "")

## ---- House seats with no ballot (unopposed winner not on the ballot, or not tabulated), by county: from 03c_house_no_ballot.R --------------------
nbc <- read_csv(file.path(PROJECT_ROOT, "R", "output", "house_no_ballot_counties.csv"), col_types = cc, na = character())
nbc <- nbc %>% mutate(cf = as.integer(county_fips)) %>% left_join(cty %>% transmute(cf = county_fips, county_name, state), by = "cf") %>% { stopifnot(!anyNA(.$county_name)); . } %>%
  transmute(year, office = "house", state_fips = substr(county_fips, 1, 2), state, state_po, county_fips, county_name, district, candidate, party_group = party,
            whole_county = ifelse(whole_county == "TRUE", "yes", "no"), reason = "unopposed_no_ballot", state_rule, source, county_method)
write_csv(nbc, file.path(REL, "us_county_results_no_ballot.csv"), na = "")

file.copy(file.path(PROJECT_ROOT, "R", "output", "data_corrections_log.csv"), file.path(REL, "data_corrections_log.csv"), overwrite = TRUE)
writeLines(VERSION, file.path(REL, "VERSION"))

## ---- packaging for the GitHub Release: changelog copy, SHA-256 checksums, one zip of everything (DATA_DICTIONARY.md is hand-maintained in REL) ------
file.copy(file.path(PROJECT_ROOT, "CHANGELOG.md"), file.path(REL, "CHANGELOG.md"), overwrite = TRUE)
zipname <- paste0("us_county_election_results_v", VERSION, ".zip")
pkg <- setdiff(sort(list.files(REL)), c("SHA256SUMS.txt", zipname)); stopifnot("DATA_DICTIONARY.md" %in% pkg)
old_wd <- setwd(REL)
system2("sha256sum", pkg, stdout = "SHA256SUMS.txt")
unlink(zipname); utils::zip(zipname, c(pkg, "SHA256SUMS.txt"), flags = "-9Xq")
setwd(old_wd)
message("long rows: ", nrow(long_all), " (house ", nrow(hl), ", president ", sum(out_ps$office == "president"), ", senate ", sum(out_ps$office == "senate"), ") | summary rows: ", nrow(sum_all), " | gaps rows: ", nrow(gaps_all), " | sources: ", nrow(src_all))
print(as.data.frame(gp %>% count(office, gap_reason)))
