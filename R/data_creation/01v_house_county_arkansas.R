## Arkansas: found via OpenElections (github.com/openelections/openelections-data-ar). Part of
## the state-by-state OpenElections sweep following the large-states push (CA/TX/NY/OH/MI/NJ/IL).
##
## Five usable years, each with its own format quirk -- no single schema across the whole span:
##   2002: a combined statewide "general.csv" (all offices/all levels), office labelled
##       "U.S. Congress District NN" (note: different wording from every later year's plain
##       "U.S. House"), and the COUNTY name lives in the `jurisdiction` column, not `county`
##       (that column is blank for these rows). `reporting_level == "county"` isolates the
##       county-level rows from the file's precinct-level ones. Two pseudo-rows per (county,
##       office), "Over"/"Under" (over/under-votes), both have candidate == "" -- filtered by
##       requiring a non-empty candidate, no explicit string match needed.
##   2006: NO general-election U.S. House file exists in the repo at all for this year (only a
##       primary-runoff precinct file) -- a genuine upstream gap, excluded entirely (same class
##       of gap as Texas 2004 in the OE OH repo, or NJ 2010).
##   2008: precinct-level file, office == "U.S. House", party as full words (Democrat/
##       Republican/Green/Write-In), and -- unique to this one year -- the `county` column
##       itself carries a literal " County" suffix ("Newton County"), unlike every other AR
##       year's bare name. Stripped explicitly for this year only.
##   2010: precinct-level file BUT the raw file uses bare CR (\r) line terminators only, not
##       \r\n or \n -- readr::read_csv() silently returns a single one-line, unparseable blob
##       if fed this directly (confirmed: naive read gave 0 rows). Fixed by reading raw text and
##       converting \r -> \n before parsing. Party is full words here too (Democrat/Republican/
##       Green/Independent).
##   2012, 2014: precinct-level files, office == "U.S. House", party as short codes (DEM/REP/
##       LIB/GRN/...), bare county names (no suffix). 2012's repo also has a `counties/`
##       subdirectory, checked and confirmed to hold only PRIMARY per-county precinct files
##       (irrelevant to the general election) -- unlike Texas 2014, there is no cleaner
##       already-aggregated county file hiding there, so the top-level precinct file is used
##       directly for both years.
## No "Total"/other pseudo-total row found in 2008/2010/2012/2014 (checked explicitly, per the
## project's standing rule that more than one such string can exist across a state's years).
##
## Per-year county coverage, all gaps checked and confirmed genuine (source-level), not bugs:
##   2002: 74/75 -- Boone County IS present with real candidate rows (Boozman R, a write-in),
##       but both are recorded with votes == 0 in the source itself -- a data artifact of the
##       same class as Texas 2014's zero-vote Delta County row, correctly dropped by the
##       standard `totalvote > 0` filter, not a join/parsing failure.
##   2008: 49/75 -- the missing 26 counties are EXACTLY AR's 1st congressional district (Marion
##       Berry, D, ran unopposed that year) -- confirmed by comparing against the district's own
##       county list derived from the 2002 file. The precinct-level source simply has no U.S.
##       House row at all for any AR-1 county in 2008, a genuine upstream gap tied to the
##       uncontested race, not a parsing bug.
##   2010: 75/75, full coverage.
##   2012: 72/75 -- Columbia, Ouachita, and Union counties are missing from the ENTIRE source
##       file (checked: zero rows for these counties under any office, not just U.S. House) --
##       a genuine file-level gap in OpenElections' collection that year, not a filtering bug.
##   2014: 74/75 -- Arkansas County (the county sharing the state's name) is likewise missing
##       from the entire source file, same class of gap as 2012's three counties.

source(file.path("R", "00_setup.R"))
library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "arkansas")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

county_fips_crosswalk <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))

ar_fips <- county_fips_crosswalk %>% filter(state == "ARKANSAS") %>% select(county_name, county_fips)

download_oe <- function(remote_path, local_name) {
  dest <- file.path(RAW_DIR, local_name)
  if (!file.exists(dest) || file.size(dest) == 0) {
    url <- paste0("https://raw.githubusercontent.com/openelections/openelections-data-ar/master/", remote_path)
    system2("curl", c("-sL", "-A", "'Mozilla/5.0'", "-o", shQuote(dest), shQuote(url)))
  }
  dest
}

strip_county_suffix <- function(x) toupper(trimws(sub(" COUNTY$", "", toupper(trimws(x)))))

to_party <- function(x) {
  case_when(
    x %in% c("Democrat", "DEM") ~ "DEM",
    x %in% c("Republican", "REP") ~ "REP",
    TRUE ~ "OTHER"
  )
}

## ---- 2002: statewide combined file, county-level rows via reporting_level, county name in
## `jurisdiction`, office = "U.S. Congress District NN" ----
read_ar_2002 <- function() {
  path <- download_oe("2002/20021105__ar__general.csv", "2002_general.csv")
  read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    filter(str_starts(office, "U.S. Congress"), reporting_level == "county", candidate != "") %>%
    mutate(county = strip_county_suffix(jurisdiction), party = to_party(party), votes = as.numeric(votes)) %>%
    filter(!is.na(votes)) %>%
    group_by(county, party) %>% summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
    mutate(year = 2002)
}

## ---- 2008: precinct-level, office = "U.S. House", county carries " County" suffix this year ----
read_ar_2008 <- function() {
  path <- download_oe("2008/20081104__ar__general__precinct.csv", "2008_general_precinct.csv")
  read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    filter(office == "U.S. House") %>%
    mutate(county = strip_county_suffix(county), party = to_party(party), votes = as.numeric(votes)) %>%
    filter(!is.na(votes)) %>%
    group_by(county, party) %>% summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
    mutate(year = 2008)
}

## ---- 2010: precinct-level, but bare-CR line endings -- convert before parsing ----
read_ar_2010 <- function() {
  path <- download_oe("2010/20101102__ar__general.csv", "2010_general.csv")
  raw_text <- readr::read_file(path)
  fixed_text <- gsub("\r", "\n", raw_text, fixed = TRUE)
  read_csv(I(fixed_text), show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    filter(office == "U.S. House") %>%
    mutate(county = strip_county_suffix(county), party = to_party(party), votes = as.numeric(votes)) %>%
    filter(!is.na(votes)) %>%
    group_by(county, party) %>% summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
    mutate(year = 2010)
}

## ---- 2012, 2014: precinct-level, office = "U.S. House", short party codes, bare county names ----
read_ar_precinct_std <- function(year, remote_path) {
  path <- download_oe(remote_path, paste0(year, "_general_precinct.csv"))
  read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    filter(office == "U.S. House") %>%
    mutate(county = strip_county_suffix(county), party = to_party(party), votes = as.numeric(votes)) %>%
    filter(!is.na(votes)) %>%
    group_by(county, party) %>% summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
    mutate(year = year)
}

ar_by_county <- bind_rows(
  read_ar_2002(),
  read_ar_2008(),
  read_ar_2010(),
  read_ar_precinct_std(2012, "2012/20121106__ar__general__precinct.csv"),
  read_ar_precinct_std(2014, "2014/20141104__ar__general__precinct.csv")
)

elect_he_cty_ar <- ar_by_county %>%
  group_by(year, county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(ar_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(
    state = "ARKANSAS", year, cty_fips = county_fips, sample = "HE",
    demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
  ) %>%
  save_step("elect_he_cty_ar")

message("AR House county-level rows built: ", nrow(elect_he_cty_ar), " (of possible ", 5 * 75, ")")
print(table(elect_he_cty_ar$year))

sanity <- elect_he_cty_ar$repuvote + elect_he_cty_ar$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")
stopifnot(all(sanity >= 0 & sanity <= 1.001))

low_share <- elect_he_cty_ar %>% filter(demovote + repuvote < 0.5) %>% arrange(year)
message(nrow(low_share), " county-years with demovote+repuvote < 0.5:")
print(low_share)

n_counties_by_year <- elect_he_cty_ar %>% count(year, name = "n_counties")
print(n_counties_by_year)

## No MEDSL overlap year is built here (all 5 years are < 2016) -- verify instead against each
## year's own statewide U.S. House winner shares (a coarser but still meaningful internal check,
## same fallback used for California/Georgia when no MEDSL overlap exists).
message("Statewide weighted D/R shares by year (internal-consistency check, no MEDSL overlap):")
print(
  elect_he_cty_ar %>%
    group_by(year) %>%
    summarise(
      repu_share = weighted.mean(repuvote, totalvote),
      demo_share = weighted.mean(demovote, totalvote)
    )
)

## ---- Fold into the master panel (year < 2016 only, per convention -- MEDSL covers 2016+) ----
elect_cty_final <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")) %>%
  bind_rows(elect_he_cty_ar %>% filter(year < 2016) %>% select(-state)) %>%
  distinct(cty_fips, year, sample, .keep_all = TRUE) %>%
  arrange(cty_fips, year, sample) %>%
  save_step("elect_cty_final")

message("elect_cty_final.rds updated with AR 2002-2014. Total rows now: ", nrow(elect_cty_final))
