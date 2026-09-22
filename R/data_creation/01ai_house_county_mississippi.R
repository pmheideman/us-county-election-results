## Mississippi: found via OpenElections (github.com/openelections/openelections-data-ms). Part of
## the post-large-states push through the remaining state list. Repo starts 2003 but no general
## U.S. House file exists before 2006 (2003/2004 are primary-only files). Real usable House years:
## 2006, 2008, 2010, 2012, 2014 (2016+ already covered by MEDSL). GitHub's listing API was
## rate-limited when checked, so the whole repo was fetched as a codeload.github.com tarball
## instead (not subject to the same limit) -- a reusable technique for this recurring problem.
##
## All 5 years share one already-county-level, single-file-per-year shape (no precinct summing,
## no per-county fetching) -- the cleanest state in this pass. Only real wrinkle: 2008's column
## order is `candidate,office,district,party,county,votes` while every other year is
## `county,office,district,party,candidate,votes` -- harmless since columns are read by name, not
## position, but worth noting since a positional read would have silently swapped county/candidate.
##
## Office label is uniformly exactly "U.S. House" every year, all 4 districts present every year,
## no per-year alias table needed. Party labels vary DEM/REP/IND/REF-style codes (2006 only) vs.
## full words (2008/2010/2012/2014) -- handled with one startsWith("DEM")/startsWith("REP")
## classifier, no conflicts (nothing like "Democratic-Republican" to worry about here).
##
## One pseudo-row, one shape, one county value: 2006/2010/2014 each carry a `county == "Total"`
## row (confirmed absent from 2008/2012, which have exactly the real 82-county count) -- filtered
## by county name before the crosswalk join. No pseudo-CANDIDATE rows (no "Write-In"/"Total
## Votes"/etc. in the candidate field) in any of the 5 years -- checked the full candidate list per
## year directly, cleanest state yet for this class of bug.
##
## No MEDSL overlap year built (2016+ already fully covered) -- verified via internal consistency
## instead: share in [0,1], totalvote > 0, low-two-party-share county-years checked directly.

source(file.path("R", "00_setup.R"))
library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "mississippi")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

county_fips_crosswalk <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))

ms_fips <- county_fips_crosswalk %>% filter(state == "MISSISSIPPI") %>% select(county_name, county_fips)
stopifnot(nrow(ms_fips) == 82)

## The repo tarball was already fetched by hand into RAW_DIR/ms_repo.tar.gz during investigation;
## re-fetch here only if missing, so the script is runnable standalone.
tarball <- file.path(RAW_DIR, "ms_repo.tar.gz")
if (!file.exists(tarball) || file.size(tarball) == 0) {
  system2("curl", c("-sL", "-o", shQuote(tarball),
                     shQuote("https://codeload.github.com/openelections/openelections-data-ms/tar.gz/refs/heads/master")))
}

extract_if_needed <- function(member) {
  dest <- file.path(RAW_DIR, "openelections-data-ms-master", member)
  if (!file.exists(dest)) {
    system2("tar", c("-xzf", shQuote(tarball), "-C", shQuote(RAW_DIR),
                      shQuote(paste0("openelections-data-ms-master/", member))))
  }
  dest
}

to_party <- function(x) {
  x <- toupper(trimws(x))
  case_when(
    grepl("^DEM", x) ~ "DEM",
    grepl("^REP", x) ~ "REP",
    TRUE ~ "OTHER"
  )
}

read_year <- function(year, member) {
  path <- extract_if_needed(member)
  raw <- read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c"))
  raw %>%
    filter(toupper(trimws(office)) == "U.S. HOUSE",
           toupper(trimws(county)) != "TOTAL") %>%
    transmute(
      county = toupper(trimws(county)), candidate = trimws(candidate),
      party = to_party(party), votes = as.numeric(votes), year = year
    )
}

message("Fetching Mississippi House data, 5 years...")
all_rows <- bind_rows(
  read_year(2006, "2006/20061107__ms__general.csv"),
  read_year(2008, "2008/20081104__ms__general.csv"),
  read_year(2010, "2010/20101102__ms__general.csv"),
  read_year(2012, "2012/20121106__ms__general.csv"),
  read_year(2014, "2014/20141104__ms__general.csv")
) %>%
  filter(!is.na(votes))

message("Raw rows fetched: ", nrow(all_rows))
print(table(all_rows$year))

elect_he_cty_ms <- all_rows %>%
  group_by(year, county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(ms_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(
    state = "MISSISSIPPI", year, cty_fips = county_fips, sample = "HE",
    demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
  ) %>%
  save_step("elect_he_cty_ms")

message("MS House county-level rows built: ", nrow(elect_he_cty_ms), " (of possible ", 82 * 5, ")")
print(table(elect_he_cty_ms$year))

sanity <- elect_he_cty_ms$repuvote + elect_he_cty_ms$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")
stopifnot(all(sanity >= 0 & sanity <= 1.001))

low_share <- elect_he_cty_ms %>% filter(demovote + repuvote < 0.5) %>% arrange(year)
message(nrow(low_share), " county-years with demovote+repuvote < 0.5:")
print(low_share)

## Which counties are missing per year, for the coverage record
for (yr in sort(unique(elect_he_cty_ms$year))) {
  present <- elect_he_cty_ms %>% filter(year == yr) %>% pull(cty_fips)
  missing <- ms_fips %>% filter(!county_fips %in% present)
  if (nrow(missing) > 0) {
    message(yr, ": missing ", nrow(missing), " counties: ", paste(missing$county_name, collapse = ", "))
  }
}

## NOTE: deliberately NOT folded into elect_cty_final.rds and house_results_coverage.csv NOT
## re-run here -- multiple states are being built in parallel by concurrent agents, and the
## fold-in / tracker-refresh step is being done centrally afterward to avoid a lost-update race
## on those shared files.
