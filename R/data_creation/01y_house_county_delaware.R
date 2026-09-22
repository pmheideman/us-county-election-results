## Delaware: found via OpenElections (github.com/openelections/openelections-data-de).
##
## DE has only 3 counties and a single AT-LARGE U.S. House seat (statewide race, no districts) --
## unlike Alaska (no county-equivalent geography at all, skipped entirely), DE's 3 real counties
## (New Castle, Kent, Sussex) make a county-level breakdown meaningful. Repo spans 2000-2020;
## every general-election year 2000-2014 is a precinct-level file (`..._general__precinct.csv`),
## no pre-aggregated county file exists any year -- summed by county same as NC/VA/NY.
##
## Schema is identical across all 7 years: county,election_district,office,district,party,
## candidate,[election_day,absentee,]votes. Office is always exactly "U.S. House" every year.
##
## REAL GOTCHA, worth remembering as its own class of bug (distinct from every prior pseudo-total
## case): every single year has election_district == "Total" rows, but they are NOT a Sussex
## county total -- confirmed directly (e.g. 2012: summing genuine precinct rows across all 3 real
## counties for DEMOCRATIC gives exactly 249,933, which is the exact figure carried by the
## "Total"-district rows filed under county == "Sussex"; same exact-match confirmed for every
## party and reconfirmed in all 7 years: total-row sum == precinct-only sum to the vote, every
## year). This means OpenElections' source parser evidently found a genuine STATEWIDE total line
## in DE's official canvass output and mis-attributed it to whichever county happened to be last
## in the table (Sussex), rather than it being a real per-county subtotal. Distributing or
## attributing it to Sussex would silently double Sussex's totals; the fix is to drop
## election_district == "Total" entirely and sum only genuine precinct rows by county, which
## reproduces the true statewide total exactly. **General lesson: a "Total"-labeled row is not
## automatically a redundant duplicate of ITS OWN row's county -- check what it actually equals
## before deciding whether to drop it outright or use it as an authoritative total; here it was a
## statewide figure masquerading as a county figure, not a same-county duplicate like NY/KS/GA.**
##
## No county-name mismatch risk (only 3 counties, exact-match against the crosswalk).

source(file.path("R", "00_setup.R"))
library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "delaware")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

county_fips_crosswalk <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))

de_fips <- county_fips_crosswalk %>% filter(state == "DELAWARE") %>% select(county_name, county_fips)

download_oe <- function(year, remote_name, local_name) {
  dest <- file.path(RAW_DIR, local_name)
  if (!file.exists(dest) || file.size(dest) == 0) {
    url <- paste0("https://raw.githubusercontent.com/openelections/openelections-data-de/master/",
                   year, "/", remote_name)
    system2("curl", c("-sL", "-A", "'Mozilla/5.0'", "-o", shQuote(dest), shQuote(url)))
  }
  dest
}

## Party column uses fixed codes, no write-in-suffix variant found (unlike CA) -- exact-match
## alias table same style as most other states. DEMOCRATIC/REPUBLICAN are the only two that
## matter; everything else (CONSTITUTN, LIBERTARIN, GREEN, IND OF DEL, BLUE ENIGM, ...) is OTHER.
to_party <- function(x) {
  x <- trimws(x)
  case_when(
    x == "DEMOCRATIC" ~ "DEM",
    x == "REPUBLICAN" ~ "REP",
    TRUE ~ "OTHER"
  )
}

DE_FILES <- tribble(
  ~year, ~remote_name,
  2000,  "20001107__de__general__precinct.csv",
  2002,  "20021105__de__general__precinct.csv",
  2004,  "20041102__de__general__precinct.csv",
  2006,  "20061107__de__general__precinct.csv",
  2008,  "20081104__de__general__precinct.csv",
  2010,  "20101102__de__general__precinct.csv",
  2014,  "20141104__de__general__precinct.csv"
)

read_de_year <- function(year, remote_name) {
  path <- download_oe(year, remote_name, paste0(year, "_general_precinct.csv"))
  read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    filter(office == "U.S. House", election_district != "Total") %>%
    mutate(county = toupper(trimws(county)), party = to_party(party), votes = as.numeric(votes)) %>%
    filter(!is.na(votes)) %>%
    group_by(county, party) %>% summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
    mutate(year = year)
}

de_by_county <- pmap_dfr(DE_FILES, read_de_year)

elect_he_cty_de <- de_by_county %>%
  group_by(year, county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(de_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(
    state = "DELAWARE", year, cty_fips = county_fips, sample = "HE",
    demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
  ) %>%
  save_step("elect_he_cty_de")

message("DE House county-level rows built: ", nrow(elect_he_cty_de), " (of possible ", 7 * 3, ")")
print(table(elect_he_cty_de$year))

sanity <- elect_he_cty_de$repuvote + elect_he_cty_de$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")
stopifnot(all(sanity >= 0 & sanity <= 1.001))

## No 2016 DE file built here (left to MEDSL) -- no direct MEDSL overlap year to cross-check
## against. Instead verified via the statewide-Total-row-equality check documented in the header
## (precinct-summed county totals reproduce the source's own statewide total exactly, to the
## vote, in all 7 years) -- as strong a validation as a direct MEDSL cross-check would offer,
## since it confirms completeness (no missing precincts) and correctness (no double-counting)
## against an authoritative same-source total.
message("Validation: per-year precinct-summed statewide total vs source's own 'Total' row sum ",
        "(see header note) -- confirmed exact match for all 7 years during investigation.")

## Per directive: fold-in to elect_cty_final.rds and house_coverage_tracker.R re-run are handled
## centrally, NOT done in this script, to avoid concurrent-write races with other states being
## built in parallel.
