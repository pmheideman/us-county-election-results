## Connecticut: found via OpenElections (github.com/openelections/openelections-data-ct).
##
## CT has no county government (abolished 1960s) -- election results are reported by TOWN, not
## county. Handled two ways depending on the year's file shape:
##   2000/2002/2006/2008/2010: the file itself carries a `county` column directly alongside
##     `town` -- no crosswalk needed at all, just group by the county field already provided.
##   2012/2016 (2016 used only for the MEDSL cross-check, not folded into output): file has only
##     a `town` column, no county -- resolved via a hand-built town->county lookup scraped from
##     Wikipedia's "List of municipalities in Connecticut" (169 towns, every one nests wholly
##     within exactly one of CT's 8 counties, no split-town ambiguity). Saved as
##     `ct_town_county_crosswalk.csv`. A handful of town-name spelling/spacing mismatches per
##     year handled via an explicit alias table (e.g. "BEACONFALLS" vs "BEACON FALLS",
##     "MERIDIEN" vs "MERIDEN", "NEW MILLFORD" vs "NEW MILFORD") -- same pattern as MI's
##     "GD. TRAVERSE"/NY's "GENESSEE" alias fixes elsewhere in this project.
## 2004: no directory at all in the OE repo -- genuine archive gap, same as OH 2004/NJ 2010.
## 2014: only scattered per-town files exist (no single statewide file) -- not worth assembling
##   for one year given the fragmentation (same "diminishing returns" call made for e.g. OH
##   2010's missing party column).
##
## CONNECTICUT IS A FUSION-VOTING STATE (confirmed directly: Rob Simmons's 2000 US House rows
## show up as BOTH "Rep" and "Ind" for the same town/district, e.g. Ashford: 810 Rep + 28 Ind;
## also John Larson 2016 Hartford: 1574 "Democratic Party" + 40 "Working Families Party" votes
## for the same precinct). IMPORTANT, and a genuine deviation from the New York script (01o):
## tried the NY-style approach FIRST (consolidate a fusion candidate's votes across every
## ballot line into their endorsing major party) and it does NOT match MEDSL for CT -- the 2016
## cross-check came back with every county's demovote inflated by exactly the Working Families
## line's share (mean diff 0.062, up to 0.085) while repuvote matched almost exactly (CT's minor
## fusion lines here are essentially all WFP-endorses-the-Democrat, no equivalent on the R side).
## Switched to the LITERAL party-line only (no cross-line consolidation at all -- just classify
## each row's own `party` field and sum), which reproduces MEDSL's county-level 2016 numbers
## EXACTLY (8/8 counties, diff = 0 to floating-point precision). **General lesson: don't assume
## a fusion-handling technique validated for one state (NY) transfers to another (CT) just
## because both are fusion-voting states -- MEDSL's own convention for what counts as a
## candidate's "party vote" can differ, and the only way to know is to cross-check a real
## overlap year, which is exactly what caught this.**
##
## Office label varies by year -- verified per year rather than assumed:
##   "US House" (2000/2002/2008/2010, district in its own column)
##   "US House 1".."US House 5" (2006 ONLY -- district number is embedded in the office string
##     itself, its own district column is blank that year; harmless since county-level output
##     never needs the district)
##   "U.S. House" (2012/2016, with periods)
## Matched with one broad regex `^U\\.?S\\.?\\s*House` covering all of the above.
##
## TWO DIFFERENT pseudo-total-row strings found in the `county` column across years (general
## lesson repeated from NY/KS/GA/CA: a state can have more than one) -- neither is a town at
## all: a literal `county == "CONNECTICUT"` statewide rollup row in 2000/2002/2006, and a
## literal `county == "TOTAL"` row in 2008. Both explicitly excluded. Also hit a genuine
## delimiter artifact: 2000/2002/2006 spell "New Haven"/"New London" with a literal PERIOD
## instead of a space in some rows ("NEW.HAVEN", "NEW.LONDON") alongside the correctly
## space-delimited spelling in others of the SAME file -- normalized by replacing "." with " "
## before matching against the FIPS crosswalk (this, not a missing county, was why an early
## version of this script only found 7/8 counties in 2000 and 2002).
##
## 2012's precinct file is a genuine, real partial-coverage gap: it contains ONLY U.S. House
## District 1 precincts (confirmed: every row's `district` value is "1", covering only
## Hartford-area towns) -- not a parsing bug, the file itself never had the other 4 districts.
## Kept as an honest partial year (3-4 of 8 counties, whichever District 1 actually touches)
## rather than excluded outright, same treatment as NY's partial 2012 in 01o.
##
## Write-in rows (candidate "write-in"/"Write-In", blank party field) are real and harmless --
## checked, always carry 0 votes in every year inspected -- fall to OTHER by construction.

source(file.path("R", "00_setup.R"))
library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "connecticut")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

county_fips_crosswalk <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))

ct_fips <- county_fips_crosswalk %>% filter(state == "CONNECTICUT") %>% select(county_name, county_fips)

download_oe <- function(year, remote_name, local_name) {
  dest <- file.path(RAW_DIR, local_name)
  if (!file.exists(dest) || file.size(dest) == 0) {
    url <- paste0("https://raw.githubusercontent.com/openelections/openelections-data-ct/master/",
                   year, "/", remote_name)
    system2("curl", c("-sL", "-A", "'Mozilla/5.0'", "-o", shQuote(dest), shQuote(url)))
  }
  dest
}

## Town -> county lookup for the two years with no county column of their own. Scraped from
## Wikipedia's "List of municipalities in Connecticut" table (169 towns, matches CT's known town
## count exactly -- every town nests wholly within one county, no split-town ambiguity).
TOWN_ALIAS <- c(
  BEACONFALLS = "BEACON FALLS", DEEPRIVER = "DEEP RIVER", EASTHADDAM = "EAST HADDAM",
  EASTHAMPTON = "EAST HAMPTON", MERIDIEN = "MERIDEN", NORTHCANAAN = "NORTH CANAAN",
  NORTHSTONINGTON = "NORTH STONINGTON", OLDLYME = "OLD LYME", `NEW MILLFORD` = "NEW MILFORD"
)
ct_town_county <- read_csv(
  file.path(RAW_DIR, "ct_town_county_crosswalk.csv"), show_col_types = FALSE
) %>%
  mutate(
    town = toupper(trimws(town)),
    county = toupper(trimws(sub(" County$", "", county)))
  )

town_to_county <- function(x) {
  x <- toupper(trimws(x))
  x <- coalesce(TOWN_ALIAS[x], x)
  ct_town_county$county[match(x, ct_town_county$town)]
}

## "New.Haven"/"New.London" delimiter-artifact fix + the two pseudo-total strings, see header.
clean_county <- function(x) {
  x <- toupper(trimws(x))
  x <- gsub("\\.", " ", x)
  x[x %in% c("CONNECTICUT", "TOTAL")] <- NA
  x
}

HOUSE_RE <- "^U\\.?S\\.?\\s*House"

## Classify by the LITERAL party-line value only -- no fusion consolidation (see header note on
## why the NY-style approach was tried and rejected for CT). Handles abbreviated codes used in
## 2000-2012 ("Dem"/"Rep"/"DEM"/"REP") and full names used in 2016 ("Democratic Party"/
## "Republican Party") with one case-insensitive prefix check.
to_party <- function(x) {
  x <- toupper(trimws(x))
  case_when(
    x == "DEM" | startsWith(x, "DEMOCRAT") ~ "DEM",
    x == "REP" | startsWith(x, "REPUBLICAN") ~ "REP",
    TRUE ~ "OTHER"
  )
}

## ---- Years with a direct `county` column already in the source file ----
CT_DIRECT_YEARS <- tribble(
  ~year, ~remote_name,
  2000,  "20001107__ct__general__town.csv",
  2002,  "20021105__ct__general__town.csv",
  2006,  "20061107__ct__general__town.csv",
  2008,  "20081104__ct__general__town.csv",
  2010,  "20101102__ct__general__precinct.csv"
)

read_ct_direct_year <- function(year, remote_name) {
  path <- download_oe(year, remote_name, paste0(year, "_direct.csv"))
  raw <- read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    filter(grepl(HOUSE_RE, office)) %>%
    mutate(county = clean_county(county), party = to_party(party), votes = as.numeric(votes)) %>%
    filter(!is.na(county), !is.na(votes))
  raw %>%
    group_by(county, party) %>%
    summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
    mutate(year = year)
}

ct_direct <- pmap_dfr(CT_DIRECT_YEARS, read_ct_direct_year)

## ---- 2012: town-only file, no county column -- resolve via the town->county crosswalk.
## Real, partial coverage: this file only ever contained District 1 (see header note). ----
ct_2012_path <- download_oe(2012, "20121106__ct__general__precinct.csv", "2012_precinct.csv")
ct_2012 <- read_csv(ct_2012_path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
  filter(grepl(HOUSE_RE, office)) %>%
  mutate(county = town_to_county(town), party = to_party(party), votes = as.numeric(votes)) %>%
  filter(!is.na(county), !is.na(votes)) %>%
  group_by(county, party) %>%
  summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
  mutate(year = 2012)

ct_by_county <- bind_rows(ct_direct, ct_2012)

elect_he_cty_ct <- ct_by_county %>%
  group_by(year, county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(ct_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(
    state = "CONNECTICUT", year, cty_fips = county_fips, sample = "HE",
    demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
  ) %>%
  save_step("elect_he_cty_ct")

message("CT House county-level rows built: ", nrow(elect_he_cty_ct), " (of possible ", 5 * 8, " + partial 2012)")
print(table(elect_he_cty_ct$year))

sanity <- elect_he_cty_ct$repuvote + elect_he_cty_ct$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")

## ---- 2016 cross-check against MEDSL (built here for validation only, NOT folded into output) ----
ct_2016_path <- download_oe(2016, "20161108__ct__general__precinct.csv", "2016_precinct.csv")
ct_2016 <- read_csv(ct_2016_path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
  filter(grepl(HOUSE_RE, office)) %>%
  mutate(county = town_to_county(town), party = to_party(party), votes = as.numeric(votes)) %>%
  filter(!is.na(county), !is.na(votes)) %>%
  group_by(county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(ct_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(cty_fips = county_fips, demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote)

medsl_final <- readRDS(file.path(PROJECT_ROOT, "R", "output", "elect_cty_final.rds"))
medsl_ct_2016 <- medsl_final %>%
  filter(sample == "HE", year == 2016, cty_fips %in% ct_fips$county_fips) %>%
  select(cty_fips, demovote_medsl = demovote, repuvote_medsl = repuvote)

cross_check <- ct_2016 %>%
  inner_join(medsl_ct_2016, by = "cty_fips") %>%
  mutate(diff = abs(demovote - demovote_medsl) + abs(repuvote - repuvote_medsl))

message("2016 cross-check vs MEDSL: ", nrow(cross_check), " of 8 counties matched, mean diff = ",
        round(mean(cross_check$diff), 6), ", max diff = ", round(max(cross_check$diff), 6))
print(cross_check %>% arrange(desc(diff)))
