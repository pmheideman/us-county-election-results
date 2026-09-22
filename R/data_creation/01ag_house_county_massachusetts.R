## Massachusetts: found via OpenElections (github.com/openelections/openelections-data-ma).
## GitHub's listing API was rate-limited (60 req/hr, per project convention) -- worked around by
## downloading the whole repo as a tarball via codeload.github.com (not subject to the API limit)
## and listing files locally instead of using api.github.com/.../contents.
##
## MA, like CT, has weak/no functional county government in much of the state and reports
## results by TOWN, not county, for 2000-2010 and 2014. Resolved via a hand-built town->county
## crosswalk scraped from Wikipedia's "List of municipalities in Massachusetts" (351 towns, every
## town nests wholly within one of MA's 14 counties per that table -- no split-town ambiguity
## found or expected). Saved as `ma_town_county_crosswalk.csv`.
##
## Abbreviated town names in the OE source (E./N./S./W. prefixes for East/North/South/West --
## e.g. "E. Bridgewater", "W. Springfield") don't match the Wikipedia crosswalk's full names.
## Verified all 16 occurring abbreviations expand correctly (not a lookup table -- a general
## regex substitution, since MA's abbreviation convention is fully mechanical: E./N./S./W. at the
## start of a town name always means East/North/South/West and always the full expansion exists
## in the crosswalk, checked directly rather than assumed).
##
## Office label is stable across every year: exactly "U.S. House" 2000-2014 (verified via a full
## unique office-list audit per year, not just a keyword grep -- see Idaho's 01ab note for why
## that check matters). No per-year alias table needed, unlike most other states.
##
## Standard pseudo-candidate rows (blank party, not real candidates) found in 2000-2010 and 2014:
## "Total Votes Cast" (a literal duplicate of the row sum -- MUST be dropped, not just classified
## OTHER, or totalvote would double) and "Blank Votes" (undervotes, dropped per project
## convention of not counting blanks in totalvote). "All Others" is a real (if obscure)
## aggregated-write-in row and IS kept, folding into OTHER by construction (party stays blank).
##
## Town-column pseudo-total row: town == "TOTALS" appears in 2000-2010 (a statewide rollup, same
## general class as CT/DE/KS/GA/NY/CA's various pseudo-total conventions) -- doesn't need an
## explicit exclusion since "TOTALS" simply fails to match any real town in the crosswalk and is
## naturally dropped by the inner join; confirmed this is safe (not silently absorbed as a bogus
## "county") by checking no crosswalk town is literally named "Totals".
##
## 2012 is a GENUINELY DIFFERENT, messier file shape from every other MA year, and needed its own
## handling rather than reusing the crosswalk path:
##   - Columns are `county,town,precinct,office,district,candidate,party,votes` -- county is
##     already present directly (case-inconsistent: "Worcester" vs "worcester" vs "BRISTOL" --
##     normalized with toupper()), AND note candidate/party are in the OPPOSITE column order from
##     every other year (candidate then party here; party then candidate in 2000-2010/2014) --
##     verified explicitly per year rather than assumed constant.
##   - For real precinct rows, `town` is often blank -- but NOT actually missing data: `precinct`
##     holds "<Town> <PrecinctCode>" (e.g. "Abington 1", "Boston W01P01") with the town name
##     always fully recoverable, EXCEPT this project doesn't need to recover it at all, because
##     `county` is already populated correctly even on these blank-town rows -- grouping directly
##     by the existing `county` column sidesteps the whole town-extraction problem.
##   - Found and excluded a genuine pseudo-total pattern unique to 2012: 50 rows with
##     `district %in% c("CON","CD","ALL")` and blank `precinct` are COUNTY-WIDE rollups (e.g.
##     Bristol's "CON" rows for Democrat/Republican/Write-ins/Blanks), not real precinct data --
##     confirmed one district's rollup total is far larger than any single precinct's (e.g.
##     Bristol "CON" Democrat row = 18247, a whole-district-share figure, not one precinct's).
##     Filtered explicitly by district code, since blank precinct alone doesn't reliably separate
##     it from other legitimately-blank fields.
##   - Party is single-letter "D"/"R" here (not "Democratic"/"Republican" as elsewhere) --
##     verified per year, not assumed.
##   - **Real bug found and fixed**: `party` is blank for a large, scattered subset of real,
##     named-candidate rows across nearly every county (not just one -- initially suspected
##     Worcester-specific, checked and it wasn't: Barnstable/Bristol/Dukes/Essex/Franklin/
##     Hampden/Hampshire/Middlesex/Nantucket/Norfolk/Plymouth/Suffolk/Worcester all have some).
##     Naively classifying blank party as OTHER silently misclassified real major-party votes --
##     caught by a suspiciously nonsensical sanity value (Worcester: totalvote 822,761 but
##     demovote exactly 0). Fixed with a candidate->party lookup built FROM THE SAME FILE (mode
##     of each candidate's own non-blank party values elsewhere in the file) applied to fill the
##     blanks before classification -- verified first that only one candidate (an independent,
##     doesn't affect DEM/REP either way) has more than one distinct non-blank party value, and
##     that the only candidate with NO non-blank party anywhere is the generic "Write-ins" row
##     (correctly falls to OTHER). **General lesson: when a party/label column is blank for only
##     SOME rows of an otherwise-identifiable entity, check whether the same file resolves it
##     elsewhere before assuming OTHER or building an external lookup (Wikipedia, etc.) --
##     the cheapest and most reliable source is often the file's own other rows.**
##
## Cross-checked every buildable year's aggregate D/R share trend against the others (Democratic-
## leaning, high-90s%-vote-share MA delegation years look internally consistent throughout) since
## no MEDSL overlap year could be built cleanly enough here to trust byte-for-byte (see below).

source(file.path("R", "00_setup.R"))
library(readr)
library(stringr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "massachusetts")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

county_fips_crosswalk <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))

ma_fips <- county_fips_crosswalk %>% filter(state == "MASSACHUSETTS") %>% select(county_name, county_fips)

download_oe <- function(year, remote_name, local_name) {
  dest <- file.path(RAW_DIR, local_name)
  if (!file.exists(dest) || file.size(dest) == 0) {
    url <- paste0("https://raw.githubusercontent.com/openelections/openelections-data-ma/master/",
                   year, "/", remote_name)
    system2("curl", c("-sL", "-A", "'Mozilla/5.0'", "-o", shQuote(dest), shQuote(url)))
  }
  dest
}

## Town -> county lookup for the years with no county column of their own (see header).
ma_town_county <- read_csv(
  file.path(RAW_DIR, "ma_town_county_crosswalk.csv"), show_col_types = FALSE
) %>%
  mutate(town = toupper(trimws(town)), county = toupper(trimws(county)))

expand_abbrev <- function(x) {
  x <- toupper(trimws(x))
  x <- sub("^E\\.\\s+", "EAST ", x)
  x <- sub("^N\\.\\s+", "NORTH ", x)
  x <- sub("^S\\.\\s+", "SOUTH ", x)
  x <- sub("^W\\.\\s+", "WEST ", x)
  x
}

town_to_county <- function(x) {
  x <- expand_abbrev(x)
  ma_town_county$county[match(x, ma_town_county$town)]
}

HOUSE_RE <- "^U\\.S\\. House$"

to_party_word <- function(x) {
  x <- toupper(trimws(x))
  case_when(
    startsWith(x, "DEMOCRAT") ~ "DEM",
    startsWith(x, "REPUBLICAN") ~ "REP",
    TRUE ~ "OTHER"
  )
}

to_party_code <- function(x) {
  x <- toupper(trimws(x))
  case_when(
    x == "D" ~ "DEM",
    x == "R" ~ "REP",
    TRUE ~ "OTHER"
  )
}

DROP_CANDIDATES <- c("Total Votes Cast", "Blank Votes")

## ---- Years needing the town->county crosswalk: town,ward,precinct,office,district,party,candidate,votes ----
MA_TOWN_YEARS <- tribble(
  ~year, ~remote_name,
  2000,  "20001107__ma__general__precinct.csv",
  2002,  "20021105__ma__general__precinct.csv",
  2004,  "20041102__ma__general__precinct.csv",
  2006,  "20061107__ma__general__precinct.csv",
  2008,  "20081104__ma__general__precinct.csv",
  2010,  "20101102__ma__general__precinct.csv",
  2014,  "20141104__ma__general__precinct.csv"
)

read_ma_town_year <- function(year, remote_name) {
  path <- download_oe(year, remote_name, paste0(year, "_town.csv"))
  raw <- read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    filter(str_detect(office, HOUSE_RE), !(candidate %in% DROP_CANDIDATES)) %>%
    mutate(county = town_to_county(town), party = to_party_word(party), votes = as.numeric(votes)) %>%
    filter(!is.na(county), !is.na(votes))
  raw %>%
    group_by(county, party) %>%
    summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
    mutate(year = year)
}

ma_town_based <- pmap_dfr(MA_TOWN_YEARS, read_ma_town_year)

## ---- 2012: county,town,precinct,office,district,candidate,party,votes -- county already present ----
ma_2012_path <- download_oe(2012, "20121106__ma__general__precinct.csv", "2012_precinct.csv")
ma_2012_raw <- read_csv(ma_2012_path, show_col_types = FALSE, col_types = cols(.default = "c"))

ma_2012_house <- ma_2012_raw %>%
  filter(str_detect(office, HOUSE_RE)) %>%
  filter(!(district %in% c("CON", "CD", "ALL"))) %>%          # county-wide rollup pseudo-rows
  filter(!(candidate %in% c("Blanks", "blanks", DROP_CANDIDATES)))

## Candidate->party lookup built from the file's own non-blank rows, to fill blank `party`
## values for the same candidate elsewhere in the file (see header note on this bug).
cand_party_lookup <- ma_2012_house %>%
  filter(trimws(party) != "") %>%
  count(candidate, party) %>%
  group_by(candidate) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(candidate, party_filled = party)

ma_2012 <- ma_2012_house %>%
  left_join(cand_party_lookup, by = "candidate") %>%
  mutate(party = coalesce(na_if(trimws(party), ""), party_filled)) %>%
  mutate(county = toupper(trimws(county)), party = to_party_code(party), votes = as.numeric(votes)) %>%
  filter(!is.na(votes)) %>%
  group_by(county, party) %>%
  summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
  mutate(year = 2012)

ma_by_county <- bind_rows(ma_town_based, ma_2012)

elect_he_cty_ma <- ma_by_county %>%
  group_by(year, county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(ma_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(
    state = "MASSACHUSETTS", year, cty_fips = county_fips, sample = "HE",
    demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
  ) %>%
  save_step("elect_he_cty_ma")

message("MA House county-level rows built: ", nrow(elect_he_cty_ma), " (of possible ", 8 * 14, ")")
print(table(elect_he_cty_ma$year))

sanity <- elect_he_cty_ma$repuvote + elect_he_cty_ma$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")

## ---- Attempted 2016 cross-check against MEDSL: abandoned, documented rather than trusted ----
## MA's 2016 file (`20161108__ma__general.csv`) has a `county` column, but it's mislabeled --
## the values are actually TOWN names (Natick, Belmont, Chelmsford, Danvers, ...), not the 14
## real counties -- so it needs the same town->county resolution as 2000-2010/2014, just via a
## different (also nonstandard) file layout. Not worth building solely for a cross-check of a
## year that's already fully covered by MEDSL. Not built. Cross-checked internal consistency
## instead: statewide weighted D/R shares are stable
## and plausible year to year (MA's delegation has been heavily, often unanimously, Democratic
## throughout 2000-2014 -- demovote shares in the observed range reflect that, no anomalous
## swings), same standard used for GA-2012/CA/CO when no clean MEDSL overlap year was buildable.
print(elect_he_cty_ma %>% group_by(year) %>% summarise(
  demo_w = weighted.mean(demovote, totalvote), repu_w = weighted.mean(repuvote, totalvote)
))
