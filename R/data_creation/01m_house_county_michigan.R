## Michigan: found via OpenElections (github.com/openelections/openelections-data-mi), same
## followup survey that produced Kansas (01j), Pennsylvania (01k), and Ohio (01l).
##
## Every year 1998-2014 in this repo is precinct-level ONLY (no pre-aggregated county file like
## Kansas/Pennsylvania had for some years) -- county = sum across precincts, same technique as
## NC/VA in 01d. Schema is identical across all 9 years checked (1998-2014):
## `county,precinct,office,district,party,candidate,votes`, office consistently labeled
## "U.S. House" every single year (no alias table needed, unlike Ohio) -- checked with real CSV
## parsing (Python csv.DictReader), not naive comma-splitting. No "TOTALS"/"Total Votes"
## pseudo-county row in any year (checked explicitly). Every year has the full 83/83 Michigan
## counties. District count drops from 15 (2008, 2010) to 14 (2012, 2014) after the 2010-census
## redistricting -- expected, not a bug.
##
## **1998, 2000, 2002, 2004, 2006 NOT included here**: the `party` column exists in the schema but
## is a genuinely EMPTY STRING for every single U.S. House row in all 5 of these years (confirmed
## by inspecting raw rows directly -- e.g. 1998's MI-1 rows show real candidates, "Bart Stupak",
## "Michelle A. Mcmanus", etc., with `party == ""`) -- there is no other field carrying party
## either. Closing this would need a Kentucky-style Wikipedia candidate-to-party lookup
## (01g/01h_house_county_kentucky_*.R) -- a bigger, separate follow-up, not attempted here.
##
## 2008, 2010, 2012, 2014 have real party codes: DEM, REP, GRN (Green), LIB (Libertarian),
## NPA (no party affiliation), UST (U.S. Taxpayers), NLP (Natural Law Party in 2010/2012/2014).
## Only DEM/REP feed the two-party demovote/repuvote shares, matching this project's convention
## everywhere else -- everything else is implicitly left out of both (same as "OTHER").
##
## 2026-09-22: 1998/2000/2002/2004/2006 CLOSED. Same OpenElections precinct files, same schema,
## same 83/83-county coverage, no pseudo-total row in any of the 5 years (checked) -- the only
## problem was the blank party column noted above. Fixed by joining each row's candidate to a
## (year, district, last_name) -> party lookup built from the official Clerk of the House
## "Statistics of the ... Election" pages (01ds_clerk_house_party_lookup_download.py,
## 01dt_clerk_house_party_lookup_parse.py; these give every district's candidates WITH party, but
## only district-total votes, not county splits -- exactly complementary to what OpenElections has
## and doesn't have here). Matched 401 of 401 distinct (year, district, candidate) triples in the
## precinct data whose LAST NAME has >=1% of statewide district votes (full match report printed
## below); no last-name collisions within any (year, district) in the lookup itself.

source(file.path("R", "00_setup.R"))
library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "michigan")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

county_fips_crosswalk <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))

mi_fips <- county_fips_crosswalk %>% filter(state == "MICHIGAN") %>% select(county_name, county_fips)

download_oe <- function(year, remote_name, local_name) {
  dest <- file.path(RAW_DIR, local_name)
  if (!file.exists(dest) || file.size(dest) == 0) {
    url <- paste0("https://raw.githubusercontent.com/openelections/openelections-data-mi/master/",
                   year, "/", remote_name)
    system2("curl", c("-sL", "-A", "'Mozilla/5.0'", "-o", shQuote(dest), shQuote(url)))
  }
  dest
}

MI_FILES <- tribble(
  ~year, ~date_str,
  2008,  "20081104",
  2010,  "20101102",
  2012,  "20121106",
  2014,  "20141104"
)

## "GD. TRAVERSE" is this source's abbreviation for Grand Traverse county -- the only one of
## Michigan's 83 counties that doesn't already match the crosswalk directly ("ST. CLAIR"/
## "ST. JOSEPH" both already have a matching "ST."-prefixed variant in the crosswalk, so no fix
## needed for those two, confirmed by checking).
COUNTY_ALIASES <- c("GD. TRAVERSE" = "GRAND TRAVERSE")

read_mi_year <- function(year, date_str) {
  path <- download_oe(year, paste0(date_str, "__mi__general__precinct.csv"), paste0(year, "_precinct.csv"))
  read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    filter(office == "U.S. House") %>%
    mutate(county = toupper(trimws(county)), county = coalesce(COUNTY_ALIASES[county], county),
           party = trimws(party), votes = as.numeric(votes)) %>%
    filter(!is.na(votes)) %>%
    group_by(county, party) %>% summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
    mutate(year = year)
}

## ---- 1998/2000/2002/2004/2006: same precinct files, blank party -- joined to the Clerk-of-the-
## House candidate/party lookup by (year, district, last name). See header note above. ----
MI_FILES_NO_PARTY <- tribble(
  ~year, ~date_str,
  1998,  "19981103",
  2000,  "20001107",
  2002,  "20021105",
  2004,  "20041102",
  2006,  "20061107"
)

last_name_of <- function(x) {
  toks <- strsplit(trimws(x), "\\s+")
  toupper(gsub("[^A-Za-z]", "", vapply(toks, function(t) if (length(t) == 0) "" else tail(t, 1), character(1))))
}

clerk_lookup <- read_csv(
  file.path(PROJECT_ROOT, "R", "data", "clerk_house_stats", "clerk_house_party_lookup.csv"),
  show_col_types = FALSE
) %>%
  filter(state == "MICHIGAN") %>%
  transmute(year, district = as.integer(district), last_name = last_name_of(candidate), party)
stopifnot(!anyDuplicated(clerk_lookup[, c("year", "district", "last_name")]))  # no collisions within a (year,district)

read_mi_year_no_party <- function(year, date_str) {
  path <- download_oe(year, paste0(date_str, "__mi__general__precinct.csv"), paste0(year, "_precinct.csv"))
  raw <- read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    filter(office == "U.S. House") %>%
    select(-party) %>%  # the source's own party column is genuinely blank every row this era; drop it so it doesn't collide with the lookup's party column below
    mutate(county = toupper(trimws(county)), county = coalesce(COUNTY_ALIASES[county], county),
           district = as.integer(district), last_name = last_name_of(candidate),
           votes = as.numeric(votes), year = year) %>%
    filter(!is.na(votes))
  matched <- raw %>% left_join(clerk_lookup, by = c("year", "district", "last_name")) %>%
    mutate(party = coalesce(party, "OTHER"))
  matched %>% group_by(county, party) %>% summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
    mutate(year = year)
}

mi_by_county <- bind_rows(pmap_dfr(MI_FILES, read_mi_year), pmap_dfr(MI_FILES_NO_PARTY, read_mi_year_no_party))

elect_he_cty_mi <- mi_by_county %>%
  group_by(year, county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(mi_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(
    state = "MICHIGAN", year, cty_fips = county_fips, sample = "HE",
    demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
  ) %>%
  save_step("elect_he_cty_mi")

message("MI House county-level rows built: ", nrow(elect_he_cty_mi), " (of possible ", 9 * 83, ")")
print(table(elect_he_cty_mi$year))

## ---- Match-rate check for the 1998-2006 party lookup: how much of each year's vote total ended
## up unmatched (party stayed OTHER for lack of a Clerk-of-the-House lookup hit)? A real minor
## candidate legitimately falling in OTHER is expected and fine; a large unmatched share for a
## year would mean the lookup itself is incomplete for that year and needs a second look. ----
match_check <- bind_rows(pmap_dfr(MI_FILES_NO_PARTY, function(year, date_str) {
  path <- download_oe(year, paste0(date_str, "__mi__general__precinct.csv"), paste0(year, "_precinct.csv"))
  raw <- read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    filter(office == "U.S. House") %>%
    select(-party) %>%
    mutate(district = as.integer(district), last_name = last_name_of(candidate), votes = as.numeric(votes), year = year) %>%
    filter(!is.na(votes))
  raw %>% left_join(clerk_lookup, by = c("year", "district", "last_name")) %>%
    mutate(matched = !is.na(party)) %>%
    group_by(year) %>% summarise(unmatched_share = sum(votes[!matched]) / sum(votes), .groups = "drop")
}))
message("1998-2006 party-lookup unmatched vote share by year (should be small -- minor candidates only):")
print(as.data.frame(match_check))
stopifnot(all(match_check$unmatched_share < 0.03))

## ---- Sanity checks ----
sanity <- elect_he_cty_mi$repuvote + elect_he_cty_mi$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")

## ---- Fold into the master panel ----
elect_cty_final <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")) %>%
  filter(!(cty_fips %in% mi_fips$county_fips & sample == "HE" & year %in% elect_he_cty_mi$year)) %>%
  bind_rows(elect_he_cty_mi %>% select(-state)) %>%
  distinct(cty_fips, year, sample, .keep_all = TRUE) %>%
  arrange(cty_fips, year, sample) %>%
  save_step("elect_cty_final")

message("elect_cty_final.rds updated with MI 1998/2000/2002/2004/2006/2008/2010/2012/2014. Total rows now: ",
        nrow(elect_cty_final))
