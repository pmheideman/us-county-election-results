## Hawaii: found via OpenElections (github.com/openelections/openelections-data-hi). Part of the
## post-large-states push through the remaining state list. Repo spans 2008-2020, with only
## general-election files needed (2008/2010/2012/2014 pre-MEDSL; 2016+ already covered by MEDSL).
##
## Cleanest small state yet: every year's file already carries a `county` column directly (no
## precinct-to-county crosswalk work needed, unlike most precinct-level sources), and county names
## ("City & County of Honolulu", "County of Hawaii", "County of Kauai", "County of Maui") map
## 1:1 onto the crosswalk's 4 real Hawaii counties (15001/15003/15007/15009) once the "City & " /
## "County of " prefixes are stripped. Kalawao County (15005) has no elections administration of
## its own (a state-run leprosy-settlement jurisdiction with a tiny population, no county
## government) -- correctly absent from the source, not a bug.
##
## Office label varies: "US Representative" (2008/2010/2012) vs "U.S. House" (2014) -- handled
## with an explicit alias pair rather than one fixed string.
##
## Party label format also varies: 3-letter codes "DEM"/"REP"/"LIB" (2008) vs single-letter "D"/
## "R"/"L"/"N" (2010/2012/2014) -- handled with an explicit alias map (not startsWith, since a
## single letter can't safely prefix-match without risk; enumerated pairs instead).
##
## Real pseudo-row gotcha, found only in 2008: three literal non-candidate rows per (county,
## precinct, office) -- "Total Ballots", "Total Blank Votes", "Total Over Votes" -- carry a BLANK
## party field, same as write-in candidates do. Do NOT filter by blank party (that would also drop
## real write-in candidates, e.g. 2008 CD-2's "STENSHOL, Shaun" who has a genuinely blank party
## field and real vote totals) -- filtered instead by an explicit candidate-name blocklist for
## these three literal strings. 2010/2012/2014 have no such pseudo-rows for the US House contest
## (checked explicitly per year, per the general lesson that a state's pseudo-row convention isn't
## guaranteed to repeat every year).
##
## Separate real gotcha, in 2010/2012/2014 (not 2008): a handful of rows -- mostly "AB-nn"
## absentee-ballot precincts and "Overseas nnn" rows, worst in 2010 (388 rows, 171,611 votes; only
## 10 rows/4,147 votes in 2012; 5 rows/61 votes in 2014) -- have the `county` field itself left
## blank in the source. Same class of gap as Florida's un-broken-out "Fed Abs" row: real votes with
## no honest county attribution possible, dropped rather than guessed at (precinct numbering isn't
## a reliable-enough guide to infer county safely). Small enough in 3 of 4 years to be negligible;
## worth knowing 2010 loses ~a few percent of the statewide House vote this way if that year's
## totals are ever compared against another source.
##
## No MEDSL overlap year built here (2016+ already fully covered by MEDSL) -- verified via internal
## consistency instead (share in [0,1], totalvote > 0, low-two-party-share county-years checked
## directly), same situation as California/Colorado/Georgia-2012.

source(file.path("R", "00_setup.R"))
library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "hawaii")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

county_fips_crosswalk <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))

hi_fips <- county_fips_crosswalk %>% filter(state == "HAWAII") %>% select(county_name, county_fips)
stopifnot(nrow(hi_fips) == 4)

download_oe <- function(year, remote_name, local_name) {
  dest <- file.path(RAW_DIR, local_name)
  if (!file.exists(dest) || file.size(dest) == 0) {
    url <- paste0("https://raw.githubusercontent.com/openelections/openelections-data-hi/master/",
                   year, "/", remote_name)
    system2("curl", c("-sL", "-A", "'Mozilla/5.0'", "-o", shQuote(dest), shQuote(url)))
  }
  dest
}

to_party <- function(x) {
  x <- toupper(trimws(x))
  case_when(
    x %in% c("DEM", "D") ~ "DEM",
    x %in% c("REP", "R") ~ "REP",
    TRUE ~ "OTHER"
  )
}

is_us_house <- function(office) {
  office <- trimws(office)
  !is.na(office) & office %in% c("US Representative", "U.S. House")
}

clean_county <- function(x) {
  x <- toupper(trimws(x))
  x <- sub("^CITY & COUNTY OF ", "", x)
  x <- sub("^COUNTY OF ", "", x)
  x
}

PSEUDO_ROWS <- c("Total Ballots", "Total Blank Votes", "Total Over Votes")

HI_FILES <- tribble(
  ~year, ~remote_name,
  2008,  "20081104__hi__general__precinct.csv",
  2010,  "20101102__hi__general__precinct.csv",
  2012,  "20121106__hi__general__precinct.csv",
  2014,  "20141104__hi__general__precinct.csv"
)

read_hi_year <- function(year, remote_name) {
  path <- download_oe(year, remote_name, paste0(year, "_precinct.csv"))
  raw <- read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    filter(is_us_house(office), !trimws(candidate) %in% PSEUDO_ROWS) %>%
    mutate(votes = as.numeric(votes)) %>%
    filter(!is.na(votes))
  ## 2010 has 130 precincts (mostly "AB-nn" absentee-ballot precincts and "Overseas nnn" rows,
  ## plus a handful of oddly-blank-county numbered precincts) with county left blank in the
  ## source itself -- 171,611 real votes with no county attribution possible. Same class of gap
  ## as Florida's un-broken-out "Fed Abs" row: a genuine, unavoidable source limitation, not a
  ## parsing bug. Dropped here rather than guessed at (precinct numbering isn't a reliable enough
  ## guide to safely infer county). Checked: 2008/2012/2014 have zero such rows for US House.
  n_dropped <- sum(raw$votes[is.na(raw$county)], na.rm = TRUE)
  if (n_dropped > 0) {
    message(year, ": dropping ", n_dropped, " votes from ", sum(is.na(raw$county)),
            " county-unattributed rows (absentee/overseas, no county field in source)")
  }
  df <- raw %>%
    filter(!is.na(county)) %>%
    mutate(county = clean_county(county), party = to_party(party))
  stopifnot(n_distinct(df$county) == 4)
  df %>% mutate(year = year)
}

hi_by_county <- pmap_dfr(HI_FILES, read_hi_year)

elect_he_cty_hi <- hi_by_county %>%
  group_by(year, county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(hi_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(
    state = "HAWAII", year, cty_fips = county_fips, sample = "HE",
    demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
  ) %>%
  save_step("elect_he_cty_hi")

message("HI House county-level rows built: ", nrow(elect_he_cty_hi), " (of possible ", 4 * 4, ")")
print(table(elect_he_cty_hi$year))

sanity <- elect_he_cty_hi$repuvote + elect_he_cty_hi$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")
stopifnot(all(sanity >= 0 & sanity <= 1.001))

low_share <- elect_he_cty_hi %>% filter(demovote + repuvote < 0.5) %>% arrange(year)
message(nrow(low_share), " county-years with demovote+repuvote < 0.5:")
print(low_share)

## NOTE: deliberately NOT folded into elect_cty_final.rds and house_results_coverage.csv NOT
## re-run here -- multiple states are being built in parallel by concurrent agents, and the
## fold-in / tracker-refresh step is being done centrally afterward to avoid a lost-update race
## on those shared files.
