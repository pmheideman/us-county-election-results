## Oregon: found via OpenElections (github.com/openelections/openelections-data-or). Filling the
## remaining gap flagged after the state-by-state sweep closed out (Oregon/Alaska/Maryland were the
## 3 states with no county-level House script at all; MD's OE repo is empty, AK not yet checked).
##
## Two eras, same as many earlier states:
## - 2006/2008/2010/2014: already county-level aggregated statewide file
##   (`county,office,district,party,candidate,votes`), clean, consistent `office=="U.S. House"`
##   label every year, full 36/36 Oregon counties, no pseudo-total row (checked the full candidate
##   list per year -- a "Misc." write-in-style catch-all bucket is real, not a pseudo-total, kept as
##   OTHER same as any other minor candidate). 2012 is the same shape but a genuine 34/36 (Wheeler
##   and Yamhill absent from the whole file, not just House -- confirmed, not a name mismatch).
## - 2000/2002/2004: only per-county PRECINCT-level files exist, and only a SUBSET of Oregon's 36
##   counties have a file at all that year (21/36, 23/36, 27/36 respectively) -- a real gap in
##   OpenElections' own archive (most other counties' files were apparently never transcribed for
##   this state/era), not a parsing limit. These files need the standard sum-by-county treatment and
##   have a `candidate=="Total"` pseudo-row per (county,precinct,office) that must be filtered
##   (same class of bug as Nebraska/Kansas/many others).
##
## One filename oddity, confirmed harmless: Deschutes' 2002 GENERAL file is misnamed
## `..._deschutes__primary.csv` (contains real November general-election data, e.g. U.S. Senate
## rows for Lon Mabon/CON) -- a repo-side typo in the filename only, not a wrong file.
##
## Party codes vary but D/R are stable across years (full words in 2000-2002 "DEM"/"REP", short
## codes elsewhere) -- handled with the standard ^DEM/^REP prefix rule used throughout this project.

source(file.path("R", "00_setup.R"))
library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "oregon")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

county_fips_crosswalk <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))

or_fips <- county_fips_crosswalk %>% filter(state == "OREGON") %>% select(county_name, county_fips)
stopifnot(nrow(or_fips) == 36)

download_oe <- function(remote_path, local_name) {
  dest <- file.path(RAW_DIR, local_name)
  if (!file.exists(dest) || file.size(dest) == 0) {
    url <- paste0("https://raw.githubusercontent.com/openelections/openelections-data-or/master/", remote_path)
    system2("curl", c("-sL", "-A", "'Mozilla/5.0'", "-o", shQuote(dest), shQuote(url)))
  }
  dest
}

## 2000-2004 precinct files use full codes ("DEM"/"REP"); 2006-2014's already-aggregated county
## files use bare single letters ("D"/"R") -- exact match on both forms, NOT a prefix match (a
## prefix match on "D"/"R" alone would be far too permissive against the other party codes present,
## e.g. Oregon's early-2000s Independent code happens to also start with letters no party overlaps
## with here, but exact-matching the known code set is the safer, explicit choice regardless).
to_party <- function(x) {
  x <- toupper(trimws(x))
  case_when(
    x %in% c("D", "DEM", "DEMOCRAT", "DEMOCRATIC") ~ "DEM",
    x %in% c("R", "REP", "REPUBLICAN") ~ "REP",
    TRUE ~ "OTHER"
  )
}

## OVER/UNDER VOTES matched with an optional space (some counties write "Overvotes"/"Undervotes"
## with no space, e.g. Gilliam 2004 -- confirmed via check_long_vs_source: the shares panel had been
## counting 48 such votes as real ones for that county-year) and BLANKS added (a real pseudo-vote
## row seen in several counties' 2002 files, e.g. Clackamas -- same PSEUDO_NAME_RE class used by
## R/long_helpers.R for every other state, this state script just hadn't picked up those two yet).
is_pseudo_row <- function(candidate) {
  c <- toupper(trimws(candidate))
  grepl("^TOTAL", c) | grepl("^WRITE-IN", c) | grepl("^SCATTERING", c) |
    grepl("^OVER ?VOTES", c) | grepl("^UNDER ?VOTES", c) | grepl("^BLANKS?$", c)
}

## 2000/2002/2004 precinct files: some counties carry an EXTRA row per (office,district,
## party,candidate) with precinct=="Total" -- a county-wide rollup disguised as if it were
## just another precinct, with a REAL candidate name (not caught by is_pseudo_row, which only
## looks at the candidate field). Confirmed by direct inspection: Multnomah 2002 CD-3,
## Blumenauer's precinct=="Total" row is exactly 133,810, the sum of his ~110 real precinct
## rows -- this is the cause of the Oregon House 2000s doubling flagged against the FEC
## (qa_state_reconcile_house_1990s.R): CD-3 is entirely inside Multnomah, so it came out ~2x;
## other districts span counties with and without the bug, so they were inflated 25-41% instead.
## Not every county's file has this row (12 of 20 in 2000, 8 of 12 in 2002, 9 of 10 in 2004) --
## drop it wherever present rather than assuming its absence.
is_total_precinct <- function(precinct) toupper(trimws(precinct)) == "TOTAL"

## ---- 2000/2002/2004: per-county precinct files, subset of counties only ----
## Exact filenames as listed by the GitHub contents API for each year's directory (NOT
## reconstructed from a county list + one guessed date -- an earlier draft of this script did
## that and wrongly included Josephine for 2000, which 404'd: 2000's directory has no Josephine
## file at all, and 2002's Yamhill file is dated 20021118, ten days after every other county's
## 20021105 -- both only caught by checking the real directory listing).
precinct_files <- list(
  "2000" = c(
    benton = "20001107__or__general__benton__precinct.csv",
    clackamas = "20001107__or__general__clackamas__precinct.csv",
    clatsop = "20001107__or__general__clatsop__precinct.csv",
    curry = "20001107__or__general__curry__precinct.csv",
    deschutes = "20001107__or__general__deschutes__precinct.csv",
    douglas = "20001107__or__general__douglas__precinct.csv",
    gilliam = "20001107__or__general__gilliam__precinct.csv",
    harney = "20001107__or__general__harney__precinct.csv",
    hood_river = "20001107__or__general__hood_river__precinct.csv",
    klamath = "20001107__or__general__klamath__precinct.csv",
    lane = "20001107__or__general__lane__precinct.csv",
    lincoln = "20001107__or__general__lincoln__precinct.csv",
    malheur = "20001107__or__general__malheur__precinct.csv",
    marion = "20001107__or__general__marion__precinct.csv",
    multnomah = "20001107__or__general__multnomah__precinct.csv",
    polk = "20001107__or__general__polk__precinct.csv",
    umatilla = "20001107__or__general__umatilla__precinct.csv",
    wasco = "20001107__or__general__wasco__precinct.csv",
    washington = "20001107__or__general__washington__precinct.csv",
    yamhill = "20001107__or__general__yamhill__precinct.csv"
  ),
  "2002" = c(
    benton = "20021105__or__general__benton__precinct.csv",
    clackamas = "20021105__or__general__clackamas__precinct.csv",
    clatsop = "20021105__or__general__clatsop__precinct.csv",
    coos = "20021105__or__general__coos__precinct.csv",
    curry = "20021105__or__general__curry__precinct.csv",
    ## Real general-election data despite the "primary" filename -- confirmed by content.
    deschutes = "20021105__or__general__deschutes__primary.csv",
    douglas = "20021105__or__general__douglas__precinct.csv",
    gilliam = "20021105__or__general__gilliam__precinct.csv",
    harney = "20021105__or__general__harney__precinct.csv",
    hood_river = "20021105__or__general__hood_river__precinct.csv",
    klamath = "20021105__or__general__klamath__precinct.csv",
    lane = "20021105__or__general__lane__precinct.csv",
    lincoln = "20021105__or__general__lincoln__precinct.csv",
    linn = "20021105__or__general__linn__precinct.csv",
    malheur = "20021105__or__general__malheur__precinct.csv",
    marion = "20021105__or__general__marion__precinct.csv",
    multnomah = "20021105__or__general__multnomah__precinct.csv",
    polk = "20021105__or__general__polk__precinct.csv",
    sherman = "20021105__or__general__sherman__precinct.csv",
    umatilla = "20021105__or__general__umatilla__precinct.csv",
    wasco = "20021105__or__general__wasco__precinct.csv",
    washington = "20021105__or__general__washington__precinct.csv",
    yamhill = "20021118__or__general__yamhill__precinct.csv"
  ),
  "2004" = c(
    baker = "20041102__or__general__baker__precinct.csv",
    benton = "20041102__or__general__benton__precinct.csv",
    clackamas = "20041102__or__general__clackamas__precinct.csv",
    clatsop = "20041102__or__general__clatsop__precinct.csv",
    columbia = "20041102__or__general__columbia__precinct.csv",
    coos = "20041102__or__general__coos__precinct.csv",
    curry = "20041102__or__general__curry__precinct.csv",
    deschutes = "20041102__or__general__deschutes__precinct.csv",
    douglas = "20041102__or__general__douglas__precinct.csv",
    gilliam = "20041102__or__general__gilliam__precinct.csv",
    harney = "20041102__or__general__harney__precinct.csv",
    hood_river = "20041102__or__general__hood_river__precinct.csv",
    jefferson = "20041102__or__general__jefferson__precinct.csv",
    klamath = "20041102__or__general__klamath__precinct.csv",
    lane = "20041102__or__general__lane__precinct.csv",
    lincoln = "20041102__or__general__lincoln__precinct.csv",
    linn = "20041102__or__general__linn__precinct.csv",
    malheur = "20041102__or__general__malheur__precinct.csv",
    marion = "20041102__or__general__marion__precinct.csv",
    multnomah = "20041102__or__general__multnomah__precinct.csv",
    polk = "20041102__or__general__polk__precinct.csv",
    sherman = "20041102__or__general__sherman__precinct.csv",
    umatilla = "20041102__or__general__umatilla__precinct.csv",
    wasco = "20041102__or__general__wasco__precinct.csv",
    washington = "20041102__or__general__washington__precinct.csv",
    wheeler = "20041102__or__general__wheeler__precinct.csv",
    yamhill = "20041102__or__general__yamhill__precinct.csv"
  )
)

read_precinct_county <- function(year, county) {
  yr <- as.character(year)
  fname <- precinct_files[[yr]][[county]]
  path <- download_oe(paste0(yr, "/", fname), paste0(yr, "_", county, "_precinct.csv"))
  raw <- read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c"))
  raw %>%
    filter(toupper(trimws(office)) == "U.S. HOUSE", !is_pseudo_row(candidate),
           !is_total_precinct(precinct)) %>%
    transmute(county = toupper(trimws(county)), party = to_party(party),
              votes = as.numeric(votes), year = as.integer(year))
}

message("Fetching Oregon 2000/2002/2004 precinct files...")
precinct_rows <- bind_rows(lapply(names(precinct_files), function(yr) {
  bind_rows(lapply(names(precinct_files[[yr]]), function(cty) read_precinct_county(yr, cty)))
}))

## ---- 2006/2008/2010/2012/2014: already county-level statewide files ----
county_files <- c(
  "2006" = "20061107__or__general.csv",
  "2008" = "20081104__or__general.csv",
  "2010" = "20101102__or__general.csv",
  "2012" = "20121106__or__general.csv",
  "2014" = "20141104__or__general__county.csv"
)

read_county_year <- function(year) {
  yr <- as.character(year)
  path <- download_oe(paste0(yr, "/", county_files[[yr]]), paste0(yr, "_general_county.csv"))
  raw <- read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c"))
  raw %>%
    filter(toupper(trimws(office)) == "U.S. HOUSE", !is_pseudo_row(candidate)) %>%
    transmute(county = toupper(trimws(county)), party = to_party(party),
              votes = as.numeric(votes), year = as.integer(yr))
}

message("Fetching Oregon 2006-2014 county-level files...")
county_rows <- bind_rows(lapply(names(county_files), read_county_year))

all_rows <- bind_rows(precinct_rows, county_rows) %>% filter(!is.na(votes))
message("Raw rows fetched: ", nrow(all_rows))
print(table(all_rows$year))

elect_he_cty_or <- all_rows %>%
  group_by(year, county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(or_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(
    state = "OREGON", year, cty_fips = county_fips, sample = "HE",
    demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
  ) %>%
  save_step("elect_he_cty_or")

message("OR House county-level rows built: ", nrow(elect_he_cty_or), " (of possible ", 36 * 8, ")")
print(table(elect_he_cty_or$year))

sanity <- elect_he_cty_or$repuvote + elect_he_cty_or$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")
stopifnot(all(sanity >= 0 & sanity <= 1.001))

low_share <- elect_he_cty_or %>% filter(demovote + repuvote < 0.5) %>% arrange(year)
message(nrow(low_share), " county-years with demovote+repuvote < 0.5:")
print(low_share)

## Which counties are missing per year, for the coverage record
for (yr in sort(unique(elect_he_cty_or$year))) {
  present <- elect_he_cty_or %>% filter(year == yr) %>% pull(cty_fips)
  missing <- or_fips %>% filter(!county_fips %in% present)
  if (nrow(missing) > 0) {
    message(yr, ": missing ", nrow(missing), " counties: ", paste(missing$county_name, collapse = ", "))
  }
}

## ---- 2016 cross-check against MEDSL (validation only, not folded in) ----
path_2016 <- download_oe("2016/20161108__or__general.csv", "2016_general_county.csv")
raw_2016 <- read_csv(path_2016, show_col_types = FALSE, col_types = cols(.default = "c"))

or_2016 <- raw_2016 %>%
  filter(toupper(trimws(office)) == "U.S. HOUSE", !is_pseudo_row(candidate)) %>%
  transmute(county = toupper(trimws(county)), party = to_party(party), votes = as.numeric(votes)) %>%
  filter(!is.na(votes)) %>%
  group_by(county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(or_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(cty_fips = county_fips, demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote)

medsl_final <- readRDS(file.path(PROJECT_ROOT, "R", "output", "elect_cty_final.rds"))
medsl_or_2016 <- medsl_final %>% filter(sample == "HE", year == 2016, cty_fips %in% or_fips$county_fips)

if (nrow(medsl_or_2016) > 0) {
  cmp <- or_2016 %>%
    inner_join(medsl_or_2016, by = "cty_fips", suffix = c("_oe", "_medsl")) %>%
    mutate(diff_dem = abs(demovote_oe - demovote_medsl), diff_rep = abs(repuvote_oe - repuvote_medsl))
  message("2016 cross-check vs MEDSL: ", nrow(cmp), " counties matched. ",
          "mean diff_dem=", round(mean(cmp$diff_dem), 5), " max diff_dem=", round(max(cmp$diff_dem), 5),
          " mean diff_rep=", round(mean(cmp$diff_rep), 5), " max diff_rep=", round(max(cmp$diff_rep), 5))
} else {
  message("No MEDSL OR 2016 HE rows found to cross-check against.")
}

message("Done. NOTE: deliberately NOT folded into elect_cty_final.rds and house_results_coverage.csv")
message("NOT re-run here -- fold-in / tracker-refresh done separately by the coordinating session.")
