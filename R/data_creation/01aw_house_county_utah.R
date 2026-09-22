## SUPERSEDED 2026-09-22 by 02af_house_ut_2012_2014.R (the state's own official canvass workbooks, full 29/29 counties both years) --
## this OpenElections-sourced build's partial coverage (15/29 in 2012, 25/29 in 2014) is a genuine repo gap, not a parsing bug, but it is
## fully replaced in the panel now; its output files were removed (R/output/long/he_ut.rds, R/output/elect_he_cty_ut.rds) and its
## source_registry.csv row was deleted to avoid a base_token collision with the new source. Kept here only as a record of the approach.
##
## Utah: found via OpenElections (github.com/openelections/openelections-data-ut). Part of the
## post-large-states push through the remaining state list. Repo only starts at 2012 (checked the
## full directory listing via a codeload.github.com tarball -- GitHub's listing API was
## rate-limited) -- no pre-2012 general-election data exists there at all, so 2012/2014 is the
## whole usable pre-MEDSL window (the 1990 project goal is aspirational per state, not achievable
## here; Utah's own state elections site was not additionally checked this pass).
##
## Both years are per-county precinct-level files (one CSV per county, no statewide file, no
## top-level county-aggregated file) -- summed by county the same way as NC/VA/2014-Kansas.
## Neither year has full county coverage: 2012 has 15 of 29 counties, 2014 has 22 of 29 (both
## genuine repo gaps -- the missing counties simply have no file in the repo for that year at all,
## not a parsing failure).
##
## Office label is NOT just "different between the two years" -- it varies WILDLY county-by-county
## WITHIN each year too (each county file apparently transcribed its own header by hand): seen across
## the 44 county-files checked: "U.S. House of Representatives", "U. S. HOUSE OF REPRESENTATIVE"
## (singular, irregular spacing), "U.S.House" (no space), "US HOUSE OF REPRESENTATIVES", "US HOUSE",
## "US CONGRESS D2" (district embedded in the label itself), "US CONGRESS", "US Congressional",
## "United States House of Representatives". A first pass hand-picking one fixed string per year
## silently dropped 8-16 counties per year that had real files but a different label spelling --
## caught by comparing "files found" against "counties surviving to output," not by a row-count
## check alone. Fixed with a single regex applied uniformly regardless of year:
## `^(U\.?\s*S\.?|UNITED\s+STATES)\s*(HOUSE|CONGRESS)` on the uppercased, trimmed label -- verified
## against the full unique-label list from every county file in both years before trusting it.
## **The one real trap this regex has to actively avoid**: Duchesne's 2014 file labels its STATE
## house race just "Utah House" (no literal word "State" at all) -- an exclude-if-contains-STATE
## rule would have wrongly kept it. The anchor requires the label to *start with* a US/United-States
## marker instead, which "Utah House" doesn't, so it's correctly excluded without needing a
## STATE-keyword denylist at all.
##
## Party field: two more format layers than first appeared from a single-county spot check. 2012 is
## uniformly short codes (CON/DEM/LIB/NP/REP/UNA). **2014 mixes short codes AND full words in
## DIFFERENT county files** (DEM/REP/CON/LIB/NP/UNA/IAP/1AP/IAM in some counties, but
## DEMOCRATIC/REPUBLICAN/CONSTITUTION/LIBERTARIAN/"INDEPENDENT AMERICAN" spelled out in others) --
## an exact "DEM"/"REP" match (which worked fine against the one county checked first) would have
## silently zeroed real Democratic/Republican votes in every full-word county. Fixed with a
## `grepl("^DEM", x)`/`grepl("^REP", x)` prefix classifier instead (safe here: no other party code
## or full name in the observed set starts with those 3 letters).
##
## Two pseudo-candidate rows found and excluded (checked the full candidate list across every
## county file in both years, not just one): `"Registered Voters"` (both years) and
## `"Ballots Cast"` (2012 only, a county not covered by the single-county spot check that first
## missed it) -- both are administrative stats that leaked into the vote-count export, same class
## of bug as Arizona's "Registered Voters"/"Times Counted" rows, not real candidates.
##
## Within a (county, candidate) pair, 2014 files repeat the same candidate multiple times (1, 3, or
## 5 rows observed) with no mode/method column distinguishing them -- summed all of them per
## (county, district, party), since there's no field marking them as duplicates and the schema has
## no "TOTAL" row to prefer instead; this reproduces plausible real district totals when checked
## against a known race (Chaffetz's Salt Lake County-only 2014 UT-3 total, ~58K, is a sane partial
## share of his real ~179K district-wide result).
##
## All matched county names (after stripping " COUNTY" and uppercasing) hit the crosswalk exactly
## -- zero aliases needed.

source(file.path("R", "00_setup.R"))
library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "utah")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

county_fips_crosswalk <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))

ut_fips <- county_fips_crosswalk %>% filter(state == "UTAH") %>% select(county_name, county_fips)
stopifnot(nrow(ut_fips) == 29)

REPO_TARBALL <- "https://codeload.github.com/openelections/openelections-data-ut/tar.gz/refs/heads/master"
TARBALL_DEST <- file.path(RAW_DIR, "repo.tar.gz")
EXTRACT_DIR  <- file.path(RAW_DIR, "extracted")

if (!dir.exists(EXTRACT_DIR)) {
  if (!file.exists(TARBALL_DEST) || file.size(TARBALL_DEST) == 0) {
    system2("curl", c("-sL", "-o", shQuote(TARBALL_DEST), shQuote(REPO_TARBALL)))
  }
  dir.create(EXTRACT_DIR, showWarnings = FALSE, recursive = TRUE)
  system2("tar", c("xzf", shQuote(TARBALL_DEST), "-C", shQuote(EXTRACT_DIR)))
}
REPO_ROOT <- list.dirs(EXTRACT_DIR, recursive = FALSE)[1]

to_party <- function(x) {
  x <- toupper(trimws(x))
  case_when(grepl("^DEM", x) ~ "DEM", grepl("^REP", x) ~ "REP", TRUE ~ "OTHER")
}

is_us_house_office <- function(x) {
  x <- toupper(trimws(x))
  grepl("^(U\\.?\\s*S\\.?|UNITED\\s+STATES)\\s*(HOUSE|CONGRESS)", x)
}

PSEUDO_CANDIDATES <- c("REGISTERED VOTERS", "BALLOTS CAST")

read_year <- function(year_chr) {
  files <- list.files(file.path(REPO_ROOT, year_chr), pattern = "precinct\\.csv$", full.names = TRUE)
  message(year_chr, ": ", length(files), " county files found")
  raw <- bind_rows(lapply(files, read_csv, show_col_types = FALSE, col_types = cols(.default = "c")))
  raw %>%
    filter(is_us_house_office(office), !(toupper(trimws(candidate)) %in% PSEUDO_CANDIDATES)) %>%
    transmute(
      county = toupper(trimws(gsub("\\s*COUNTY\\s*$", "", county, ignore.case = TRUE))),
      district = trimws(district),
      ## Davis's candidate strings use a literal non-breaking space (U+00A0) between first/last
      ## name instead of a regular space -- invisible in print, byte-different from every other
      ## county's plain-space names, and NOT stripped by trimws(). Left unfixed, this silently
      ## broke the candidate->party fallback lookup below for that one county only (every other
      ## county's names matched fine). Normalize all whitespace, not just leading/trailing, before
      ## using candidate as a join key.
      candidate_key = toupper(str_squish(gsub("[ \\s]+", " ", candidate))),
      party_raw = party,
      votes = as.numeric(votes),
      year = as.numeric(year_chr)
    ) %>%
    filter(!is.na(votes))
}

message("Fetching Utah House data, 2 years...")
all_rows <- bind_rows(read_year("2012"), read_year("2014")) %>%
  mutate(party = to_party(party_raw))

## Fallback for a real blank-`party`-column problem (2014 only: Davis/Duchesne/San Juan/Washington
## have NA for every House row that year, unlike every other 2014 county) -- these counties' own
## candidates (Chris Stewart, Rob Bishop, Jason Chaffetz, Luz Robles, Donna McAleer, Brian
## Wonnacott, ...) all appear WITH a resolvable party elsewhere in the same year's well-labeled
## counties, so build a candidate->party lookup from those instead of an external source (same
## technique used for Massachusetts's 2012 blank-party rows).
candidate_party_lookup <- all_rows %>%
  filter(party %in% c("DEM", "REP")) %>%
  count(candidate_key, party) %>%
  group_by(candidate_key) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(candidate_key, party_lookup = party)

n_before_fallback <- sum(all_rows$party == "OTHER" & is.na(all_rows$party_raw))
all_rows <- all_rows %>%
  left_join(candidate_party_lookup, by = "candidate_key") %>%
  mutate(party = if_else(party == "OTHER" & is.na(party_raw) & !is.na(party_lookup), party_lookup, party)) %>%
  select(-party_lookup)
message(n_before_fallback, " blank-party rows found; resolved via candidate lookup where possible")

message("Raw rows fetched: ", nrow(all_rows))
print(table(all_rows$year))

elect_he_cty_ut <- all_rows %>%
  group_by(year, county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(ut_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(
    state = "UTAH", year, cty_fips = county_fips, sample = "HE",
    demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
  ) %>%
  save_step("elect_he_cty_ut")

message("UT House county-level rows built: ", nrow(elect_he_cty_ut), " (of possible ", 29 * 2, ")")
print(table(elect_he_cty_ut$year))

sanity <- elect_he_cty_ut$repuvote + elect_he_cty_ut$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")
stopifnot(all(sanity >= 0 & sanity <= 1.001))

low_share <- elect_he_cty_ut %>% filter(demovote + repuvote < 0.5) %>% arrange(year)
message(nrow(low_share), " county-years with demovote+repuvote < 0.5:")
print(low_share)

for (yr in sort(unique(elect_he_cty_ut$year))) {
  present <- elect_he_cty_ut %>% filter(year == yr) %>% pull(cty_fips)
  missing <- ut_fips %>% filter(!county_fips %in% present)
  message(yr, ": ", length(present), "/29 counties present. Missing: ", paste(missing$county_name, collapse = ", "))
}

## ---- Cross-check against MEDSL for any overlap year (none exists -- both years are pre-2016) ----
medsl_final <- readRDS(file.path(PROJECT_ROOT, "R", "output", "elect_cty_final.rds"))
medsl_ut <- medsl_final %>% filter(sample == "HE", year %in% c(2012, 2014), cty_fips %in% ut_fips$county_fips)
message("MEDSL UT HE rows for 2012/2014 (expect none, MEDSL only covers 2016+): ", nrow(medsl_ut))

## Utah is heavily Republican statewide (Salt Lake/Summit are the only competitive counties) --
## the observed sanity range and the low-share list (if any) below were checked against this prior
## rather than assumed to be bugs.

## NOTE: deliberately NOT folded into elect_cty_final.rds and house_results_coverage.csv NOT
## re-run here -- multiple states are being built in parallel by concurrent agents, and the
## fold-in / tracker-refresh step is being done centrally afterward to avoid a lost-update race
## on those shared files.
