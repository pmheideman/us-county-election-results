## Indiana: found via OpenElections (github.com/openelections/openelections-data-in), continuing
## the state-by-state sweep after the large-states push and the AL/AZ/AR/CO/CT/DE/GA/HI/ID batch.
##
## IN's repo has year directories from 2002 through 2024. Within the pre-MEDSL span (2002-2014),
## every single year already has a clean, already county-level general-election file
## (`county,office,district,candidate,party,votes`). Office is consistently labeled exactly
## "U.S. House" in all 7 years (confirmed via a full unique-office-label audit per year, not just
## a keyword grep -- the "state can truncate a label for just one district" lesson from Idaho
## didn't apply here, every year's label set was identical in shape). Party is given as clean full
## words (Democratic/Republican/Libertarian/...) -- no write-in suffix, no single-letter codes.
## No pseudo-total row of any kind found in any year (checked county name AND candidate name for
## anything total/over/under/write-in-looking).
##
## Result: 514/644 (92 counties x 7 years). 2012 and 2014 are full 92/92. **2002-2010 are all a
## genuine partial 66/92 (71.7%) -- and it's the SAME 26 counties, every single year 2002-2010,
## confirmed by intersecting the five years' zero-vote-county sets exactly (Adams, Benton,
## Blackford, Carroll, Cass, Dubois, Hancock, Huntington, Jasper, Jay, Jefferson, Jennings,
## Johnson, Lawrence, Madison, Miami, Newton, Ohio, Perry, Putnam, Ripley, Spencer, Switzerland,
## Wabash, Warren, Wells).** Checked the raw source directly: these 26 counties' U.S. House rows
## exist with real candidate names but literally every candidate's `votes` field is 0 (confirmed
## e.g. Adams County 2002 district 3: Rigdon/Donlan/Souder all recorded as 0 votes) -- while the
## SAME counties report normal nonzero totals for other offices in the same file, and the other
## 66 counties report normal nonzero U.S. House totals. This is a genuine, structural digitization
## gap in OpenElections' own IN archive for this specific office across this specific span, not a
## parsing bug -- correctly excluded by the standard `totalvote > 0` filter already used
## project-wide. **General lesson: when a "low coverage" result turns out to be the exact same set
## of counties across every year in a span (not a random subset each year), that's a strong signal
## of a structural source-side gap for that specific office, not a per-year parsing issue -- worth
## explicitly checking for a stable county set before assuming the gap is unexplained noise.**
##
## One crosswalk gotcha worth noting: a naive text grep for `"INDIANA"` against the shared
## countypres crosswalk also matches Pennsylvania's "Indiana County" (its county_name field is
## literally "INDIANA") -- the production join below filters on state == "INDIANA" explicitly,
## not a text search, so this never affects the actual data, just flagging it as a reminder to
## always join on state + county name together, never county name alone.
##
## Two real county-name mismatches found via a 2016 ad-hoc cross-check against MEDSL (that 2016
## file itself isn't part of the production 2002-2014 build, MEDSL already covers 2016+, but
## fetching it for free as a validation harness surfaced these): the source spells St. Joseph
## County as "Saint Joseph" in every single year (crosswalk has "ST. JOSEPH") -- would have
## silently dropped this county in ALL 7 years without the alias. And LaPorte County is spelled
## "LaPorte" (matches crosswalk fine) in 2002-2010 but "LaPort" -- missing the trailing e -- in
## 2012 and 2014 only, a genuine mid-series spelling drift, not a one-off typo. Both fixed with an
## explicit alias step before the crosswalk join.

source(file.path("R", "00_setup.R"))
library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "indiana")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

county_fips_crosswalk <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))

in_fips <- county_fips_crosswalk %>% filter(state == "INDIANA") %>% select(county_name, county_fips)

download_oe <- function(year, remote_name, local_name) {
  dest <- file.path(RAW_DIR, local_name)
  if (!file.exists(dest) || file.size(dest) == 0) {
    url <- paste0("https://raw.githubusercontent.com/openelections/openelections-data-in/master/",
                   year, "/", remote_name)
    system2("curl", c("-sL", "-A", "'Mozilla/5.0'", "-o", shQuote(dest), shQuote(url)))
  }
  dest
}

PARTY_ALIAS <- c(DEMOCRATIC = "DEM", REPUBLICAN = "REP")
to_party <- function(x) coalesce(PARTY_ALIAS[toupper(trimws(x))], "OTHER")

COUNTY_ALIAS <- c("SAINT JOSEPH" = "ST. JOSEPH", "LAPORT" = "LAPORTE")
to_county <- function(x) coalesce(COUNTY_ALIAS[x], x)

IN_FILES <- tribble(
  ~year, ~remote_name,
  2002,  "20021105__in__general__county.csv",
  2004,  "20041102__in__general__county.csv",
  2006,  "20061106__in__general__county.csv",
  2008,  "20081104__in__general__county.csv",
  2010,  "20101102__in__general__county.csv",
  2012,  "20121106__in__general__county.csv",
  2014,  "20141104__in__general__county.csv"
)

read_in_year <- function(year, remote_name) {
  path <- download_oe(year, remote_name, paste0(year, "_county.csv"))
  read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    filter(office == "U.S. House") %>%
    mutate(county = to_county(toupper(trimws(county))), party = to_party(party), votes = as.numeric(votes)) %>%
    filter(!is.na(votes)) %>%
    group_by(county, party) %>% summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
    mutate(year = year)
}

in_by_county <- pmap_dfr(IN_FILES, read_in_year)

elect_he_cty_in <- in_by_county %>%
  group_by(year, county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(in_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(
    state = "INDIANA", year, cty_fips = county_fips, sample = "HE",
    demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
  ) %>%
  save_step("elect_he_cty_in")

message("IN House county-level rows built: ", nrow(elect_he_cty_in), " (of possible ", 7 * 92, ")")
print(table(elect_he_cty_in$year))

sanity <- elect_he_cty_in$repuvote + elect_he_cty_in$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")

## ---- Cross-check: MEDSL 2016 (built independently in 01a) ----
## NOT folded into elect_cty_final.rds here -- multiple states being built in parallel this batch,
## fold-in is done centrally afterward to avoid a concurrent read-modify-write race (see AL/AZ/AR/CO
## notes: this exact race happened in an earlier batch and was caught).
medsl_2016 <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")) %>%
  filter(sample == "HE", year == 2016, cty_fips %in% in_fips$county_fips)
message("MEDSL 2016 IN HE rows available for reference (not used to build/validate this script's ",
        "2002-2014 output directly, just confirming coverage exists for a future overlap check): ",
        nrow(medsl_2016))
