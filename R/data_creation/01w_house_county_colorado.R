## Colorado: found via OpenElections (github.com/openelections/openelections-data-ca -- sic,
## github.com/openelections/openelections-data-co). Part of the post-large-states push through
## the remaining state list. Repo spans 2002-2024.
##
## Two source shapes across the pre-MEDSL span (2002-2014):
##   2002: already a single county-level file (one row per county x office x district x party x
##         candidate), WITH a `county == "TOTALS"` pseudo-row (same class of bug as Kansas 2012 /
##         Georgia -- excluded explicitly, confirmed 65 distinct "county" values for U.S. House
##         before exclusion, 64 (the real county count) after).
##   2004/2006/2008/2010/2012: statewide precinct-level files with a `county` column -- summed
##         across precincts per county, same technique as NC/VA (01d) and Kansas 2014. No
##         pseudo-total county value found in any of these five years (checked explicitly, per
##         the New York lesson that more than one such convention can exist within a state).
##   2014: already a single county-level file again, no pseudo-total row this time (64 distinct
##         counties directly, confirmed).
## 64/64 Colorado counties present in every one of these 6 usable years, all pre-filter checked by
## direct inspection before writing this script (not just discovered after running it).
##
## Party-label format changes YEAR TO YEAR (not just state to state, as seen elsewhere in this
## project): abbreviated codes ("REP"/"DEM") in 2002/2010/2014, full words ("Republican"/
## "Democratic"/"Democrat") in 2004/2006/2008, and a "<Party Name> Party" suffix form
## ("Republican Party"/"Democratic Party") in 2012 specifically. A single normalization handles
## all of them at once without a per-year alias table: toupper(party) then startsWith("REP") /
## startsWith("DEM") -- "REPUBLICAN PARTY", "REPUBLICAN", and "REP" all satisfy startsWith("REP"),
## same for the DEM side. Same startsWith() technique as California's write-in-suffix handling in
## 01q, applied here for a different reason (year-varying full-word/abbreviation forms rather than
## write-in suffixes).
##
## Office label also varies: "U.S. House" every year except 2014, where OpenElections' own source
## file labels it "Representatives To The 114th United States Congress" (matched via a broad
## regex rather than hardcoding that exact string, in case a future year phrases it differently
## again).
##
## No MEDSL overlap year is built here (2016+ already fully covered by MEDSL, and this script only
## reaches back to 2002 with no need to duplicate 2016) -- same situation as California and
## Georgia 2012: verified via internal consistency (share in [0,1], totalvote > 0, checked low-
## two-party-share county-years directly) rather than a direct MEDSL diff.

source(file.path("R", "00_setup.R"))
library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "colorado")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

county_fips_crosswalk <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))

co_fips <- county_fips_crosswalk %>% filter(state == "COLORADO") %>% select(county_name, county_fips)

download_oe <- function(year, remote_name, local_name) {
  dest <- file.path(RAW_DIR, local_name)
  if (!file.exists(dest) || file.size(dest) == 0) {
    url <- paste0("https://raw.githubusercontent.com/openelections/openelections-data-co/master/",
                   year, "/", remote_name)
    system2("curl", c("-sL", "-A", "'Mozilla/5.0'", "-o", shQuote(dest), shQuote(url)))
  }
  dest
}

## Handles "REP"/"DEM", "Republican"/"Democratic"/"Democrat", and "Republican Party"/
## "Democratic Party" all at once -- see header note. Anything else (LIB, GRN, ACN, UNA, Unity,
## Colorado Reform, write-ins, etc.) falls to OTHER, same treatment as every other state script.
to_party <- function(x) {
  x <- toupper(trimws(x))
  case_when(
    startsWith(x, "DEM") ~ "DEM",
    startsWith(x, "REP") ~ "REP",
    TRUE ~ "OTHER"
  )
}

is_us_house <- function(office) {
  office <- trimws(office)
  !is.na(office) & (office == "U.S. House" | grepl("United States Congress", office, fixed = TRUE))
}

## ---- 2002: already county-level, with TWO pseudo-total conventions to exclude ----
## (1) a whole-county `county == "TOTALS"` row (same class as Kansas 2012), AND, found only by
## checking a specific county's raw rows after an initial run came back with suspiciously low
## two-party shares (Adams County 2002 D+R summed to ~0.48 rather than the expected ~0.9+): (2) a
## PER-DISTRICT pseudo-total row with `party == "" & candidate == ""` whose `votes` value exactly
## equals the sum of that district's real candidate rows (confirmed: district 2's blank row =
## 58350 = 33448 DEM + 23072 REP + 453 ACP + 973 LIB + 404 NLP). Left unfiltered, this exactly
## DOUBLES totalvote per district (true total + the blank row's duplicate of that same total),
## silently halving every computed vote share without affecting `totalvote > 0`, so it wasn't
## caught by the county-count check alone. Excluded via `candidate != ""` (trimmed). Same general
## lesson carried from NY/KS/GA: a single state -- here even a single YEAR -- can have more than
## one pseudo-total convention; check for a second one before trusting a single fix.
co_2002_path <- download_oe(2002, "20021105__co__general.csv", "2002_general.csv")
co_2002_raw <- read_csv(co_2002_path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
  filter(is_us_house(office), county != "TOTALS", trimws(candidate) != "") %>%
  mutate(county = toupper(trimws(county)), party = to_party(party), votes = as.numeric(votes)) %>%
  filter(!is.na(votes))
stopifnot(n_distinct(co_2002_raw$county) == 64)

## ---- 2004/2006/2008/2010/2012: statewide precinct-level files, sum across precincts by county ----
CO_PRECINCT_FILES <- tribble(
  ~year, ~remote_name,
  2004,  "20041106__co__general__precinct.csv",
  2006,  "20061107__co__general__precinct.csv",
  2008,  "20081104__co__general__precinct.csv",
  2010,  "20101102__co__general__precinct.csv",
  2012,  "20121106__co__general__precinct.csv"
)

read_co_precinct_year <- function(year, remote_name) {
  path <- download_oe(year, remote_name, paste0(year, "_precinct.csv"))
  df <- read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    filter(is_us_house(office)) %>%
    mutate(county = toupper(trimws(county)), party = to_party(party), votes = as.numeric(votes)) %>%
    filter(!is.na(votes))
  stopifnot(n_distinct(df$county) == 64)
  df %>% mutate(year = year)
}

co_precinct_by_county <- pmap_dfr(CO_PRECINCT_FILES, read_co_precinct_year)

## ---- 2014: already county-level again, SAME per-district blank-candidate pseudo-total row as ----
## 2002 (confirmed directly: Adams County district 7's blank row = 93055 = 41255 REP + 51800 DEM).
## No county-level "TOTALS" row this year, but the per-district blank-row convention persists.
co_2014_path <- download_oe(2014, "20141104__co__general.csv", "2014_general.csv")
co_2014_raw <- read_csv(co_2014_path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
  filter(is_us_house(office), trimws(candidate) != "") %>%
  mutate(county = toupper(trimws(county)), party = to_party(party), votes = as.numeric(votes)) %>%
  filter(!is.na(votes))
stopifnot(n_distinct(co_2014_raw$county) == 64)

co_by_county <- bind_rows(
  co_2002_raw %>% mutate(year = 2002) %>% select(year, county, party, votes),
  co_precinct_by_county %>% select(year, county, party, votes),
  co_2014_raw %>% mutate(year = 2014) %>% select(year, county, party, votes)
)

elect_he_cty_co <- co_by_county %>%
  group_by(year, county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(co_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(
    state = "COLORADO", year, cty_fips = county_fips, sample = "HE",
    demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
  ) %>%
  save_step("elect_he_cty_co")

message("CO House county-level rows built: ", nrow(elect_he_cty_co), " (of possible ", 7 * 64, ")")
print(table(elect_he_cty_co$year))

sanity <- elect_he_cty_co$repuvote + elect_he_cty_co$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")
stopifnot(all(sanity >= 0 & sanity <= 1.001))

low_share <- elect_he_cty_co %>% filter(demovote + repuvote < 0.5) %>% arrange(year)
message(nrow(low_share), " county-years with demovote+repuvote < 0.5:")
print(low_share)

## ---- Fold into the master panel ----
## Explicitly drop any prior CO HE rows for these years before re-appending (rather than relying
## on distinct()'s keep-first behavior), so this script stays safe to re-run after a fix -- an
## earlier version of this script had the per-district blank-total-row bug described above, whose
## output was already folded into elect_cty_final.rds once before being caught; distinct() alone
## would have silently kept those stale buggy rows since they appear earlier in bind_rows() order.
elect_cty_final <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")) %>%
  filter(!(cty_fips %in% co_fips$county_fips & sample == "HE" & year %in% elect_he_cty_co$year)) %>%
  bind_rows(elect_he_cty_co %>% select(-state)) %>%
  distinct(cty_fips, year, sample, .keep_all = TRUE) %>%
  arrange(cty_fips, year, sample) %>%
  save_step("elect_cty_final")

message("elect_cty_final.rds updated with CO 2002-2014. Total rows now: ", nrow(elect_cty_final))
