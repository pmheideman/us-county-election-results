## Nevada: found via OpenElections (github.com/openelections/openelections-data-nv). Part of the
## post-large-states push through the remaining state list. GitHub's listing API was rate-limited
## on first check -- worked around via the codeload.github.com tarball fetch (established project
## convention) to enumerate file paths, then downloaded individual files via
## raw.githubusercontent.com (not rate-limited) as usual.
##
## Two distinct file shapes across the 8 usable years (2000-2014; 2016+ already covered by MEDSL):
##
## 2000/2002/2004/2006/2008/2010: one already county-aggregated file PER COUNTY (17 files/year,
## filename token e.g. "white_pine" -> crosswalk's "WHITE PINE", "carson_city" -> "CARSON CITY" --
## both matched exactly after underscore->space + uppercase, no alias table needed). Office label
## for the U.S. House race drifts across these 6 years but always contains "REPRESENTATIVE IN
## CONGRESS" (with varying comma/casing: "Representative in Congress, District N" in 2000,
## "REPRESENTATIVE IN CONGRESS, DISTRICT N" 2002/2008/2010, "REPRESENTATIVE IN CONGRESS DISTRICT N"
## 2004 with no comma, "U.S. Representative in Congress, District N" 2006/2008+ with a "U.S."
## prefix) -- pulled the full unique office list per year before trusting a single regex; a plain
## `grepl("REPRESENTATIVE IN CONGRESS", toupper(office))` covers every year with no false positives
## (no "State Representative"-style office exists in Nevada to collide with it, checked). Party
## already provided in this era as a clean column (3-letter codes DEM/REP/IAP/LIB/CIT/GRN/NAT
## 2000-2006, full words DEMOCRAT/REPUBLICAN/... 2008-2010) -- one `startsWith("DEM")`/
## `startsWith("REP")` classifier (after toupper) covers both. Votes are comma-formatted strings in
## 2000-2006 ("1,962") vs plain digits in 2008-2010 -- stripped commas unconditionally before
## `as.numeric()`. One real pseudo-row pair present every year: "Under Votes"/"Over Votes" candidate
## rows (party always NA) -- excluded by candidate-name pattern, same class of pseudo-row as other
## states' write-in/total rows just under a different name.
##
## 2012/2014: the per-county aggregated file disappeared from the repo this era -- only a single
## statewide PRECINCT-level file exists per year (`..._general__precinct.csv`), with `county` and
## `district` columns already present (no crosswalk-via-filename needed, unlike the earlier years)
## but CRUCIALLY NO PARTY COLUMN AT ALL for this file. Office label is a clean, exact "U.S. House"
## both years -- but a first-pass filter using `grepl("HOUSE", office)` wrongly pulled in "State
## House" rows too (Nevada's state legislature's lower chamber really is called "State House",
## unlike Nebraska's unicameral system which has no such collision) -- caught by district numbers
## running up to 40+ instead of the real 4 U.S. House districts; fixed with an exact `office ==
## "U.S. House"` match. **General lesson, a new instance of the same class of bug as Iowa's
## `grepl("STATE",...)` substring trap: an office-label keyword filter needs to be checked against
## every office string that contains the same keyword, not just assumed unique, even when the
## keyword itself (here "House") seems specific -- "State House" is a real, different office in
## some states and a keyword match alone won't distinguish it from "U.S. House".**
##
## With no party column, built an explicit candidate->party lookup by hand for the two years' known
## major-party nominees (a small, closed set -- exactly 4 districts x 2 major-party candidates each
## year, all real, nationally-covered Nevada races): 2012 NV-1 Titus(D)/Edwards(R), NV-2
## Amodei(R)/Koepnick(D), NV-3 Heck(R)/Oceguera(D), NV-4 Horsford(D)/Tarkanian(R); 2014 NV-1
## Titus(D)/Teijeiro(R), NV-2 Amodei(R)/Spees(D), NV-3 Heck(R)/Bilbray(D), NV-4
## Hardy(R)/Horsford(D) -- Horsford's 2014 loss to Hardy is a well-documented upset, useful as a
## direct plausibility check on the mapping (his county totals should show him LOSING that year,
## unlike 2012). All other candidates (independents, Libertarians, "None of These Candidates" --
## a real, standard Nevada ballot option, not a data artifact) fall to OTHER, same as everywhere
## else in this project. No MEDSL overlap year exists for 2012/2014 directly, so this mapping is
## checked instead via the Horsford win/loss plausibility signal and the standard sanity-range
## check below.

source(file.path("R", "00_setup.R"))
library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "nevada")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

county_fips_crosswalk <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))

nv_fips <- county_fips_crosswalk %>% filter(state == "NEVADA") %>% select(county_name, county_fips)
stopifnot(nrow(nv_fips) == 17)

nv_counties <- c("carson_city", "churchill", "clark", "douglas", "elko", "esmeralda", "eureka",
                  "humboldt", "lander", "lincoln", "lyon", "mineral", "nye", "pershing", "storey",
                  "washoe", "white_pine")

download_oe <- function(remote_path, local_name) {
  dest <- file.path(RAW_DIR, local_name)
  if (!file.exists(dest) || file.size(dest) == 0) {
    url <- paste0("https://raw.githubusercontent.com/openelections/openelections-data-nv/master/", remote_path)
    system2("curl", c("-sL", "-A", "'Mozilla/5.0'", "-o", shQuote(dest), shQuote(url)))
  }
  dest
}

to_party <- function(x) {
  x <- toupper(trimws(x))
  case_when(
    startsWith(x, "DEM") ~ "DEM",
    startsWith(x, "REP") ~ "REP",
    TRUE ~ "OTHER"
  )
}

is_pseudo_row <- function(candidate) {
  c <- toupper(trimws(candidate))
  grepl("OVER VOTES", c) | grepl("UNDER VOTES", c)
}

## ---- 2000-2010: per-county aggregated files ----
read_county_year <- function(year, date_str) {
  bind_rows(lapply(nv_counties, function(cty) {
    remote <- sprintf("%d/%s__nv__general__%s.csv", year, date_str, cty)
    local <- sprintf("%d_%s.csv", year, cty)
    path <- download_oe(remote, local)
    raw <- read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c"))
    h <- raw %>% filter(grepl("REPRESENTATIVE IN CONGRESS", toupper(office)))
    if (nrow(h) == 0) return(NULL)
    h %>%
      filter(!is_pseudo_row(candidate)) %>%
      transmute(
        county = toupper(gsub("_", " ", cty)),
        candidate = trimws(candidate),
        party = to_party(party),
        votes = as.numeric(gsub(",", "", votes)),
        year = year
      )
  }))
}

message("Fetching Nevada House data, 2000-2010 (per-county files)...")
rows_2000s <- bind_rows(
  read_county_year(2000, "20001107"),
  read_county_year(2002, "20021105"),
  read_county_year(2004, "20041102"),
  read_county_year(2006, "20061107"),
  read_county_year(2008, "20081104"),
  read_county_year(2010, "20101102")
)
message("2000-2010 raw rows: ", nrow(rows_2000s))
print(table(rows_2000s$year))

## ---- 2012/2014: single statewide precinct file, no party column, hand-mapped from known
## major-party nominees ----
party_map <- tribble(
  ~year, ~candidate,           ~party,
  2012,  "Dina Titus",         "DEM",
  2012,  "Chris Edwards",      "REP",
  2012,  "Mark Amodei",        "REP",
  2012,  "Samuel Koepnick",    "DEM",
  2012,  "Joe Heck",           "REP",
  2012,  "John Oceguera",      "DEM",
  2012,  "Steven Horsford",    "DEM",
  2012,  "Danny Tarkanian",    "REP",
  2014,  "Dina Titus",         "DEM",
  2014,  "Annette Teijeiro",   "REP",
  2014,  "Mark Amodei",        "REP",
  2014,  "Kristen Spees",      "DEM",
  2014,  "Joe Heck",           "REP",
  2014,  "Erin Bilbray",       "DEM",
  2014,  "Cresent Hardy",      "REP",
  2014,  "Steven Horsford",    "DEM"
)

read_precinct_year <- function(year, date_str) {
  remote <- sprintf("%d/%s__nv__general__precinct.csv", year, date_str)
  local <- sprintf("%d_statewide_precinct.csv", year)
  path <- download_oe(remote, local)
  raw <- read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c"))
  raw %>%
    filter(office == "U.S. House") %>%
    left_join(party_map %>% filter(year == !!year) %>% select(-year), by = "candidate") %>%
    transmute(
      county = toupper(trimws(county)),
      candidate = trimws(candidate),
      party = coalesce(party, "OTHER"),
      votes = as.numeric(votes),
      year = year
    )
}

message("Fetching Nevada House data, 2012/2014 (statewide precinct files)...")
rows_2010s <- bind_rows(
  read_precinct_year(2012, "20121106"),
  read_precinct_year(2014, "20141104")
)
message("2012/2014 raw rows: ", nrow(rows_2010s))
print(table(rows_2010s$year))

all_rows <- bind_rows(rows_2000s, rows_2010s) %>% filter(!is.na(votes))

elect_he_cty_nv <- all_rows %>%
  group_by(year, county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(nv_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(
    state = "NEVADA", year, cty_fips = county_fips, sample = "HE",
    demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
  ) %>%
  save_step("elect_he_cty_nv")

message("NV House county-level rows built: ", nrow(elect_he_cty_nv), " (of possible ", 17 * 8, ")")
print(table(elect_he_cty_nv$year))

sanity <- elect_he_cty_nv$repuvote + elect_he_cty_nv$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")
stopifnot(all(sanity >= 0 & sanity <= 1.001))

low_share <- elect_he_cty_nv %>% filter(demovote + repuvote < 0.5) %>% arrange(year)
message(nrow(low_share), " county-years with demovote+repuvote < 0.5:")
print(low_share)

## Which counties are missing per year, for the coverage record
for (yr in sort(unique(elect_he_cty_nv$year))) {
  present <- elect_he_cty_nv %>% filter(year == yr) %>% pull(cty_fips)
  missing <- nv_fips %>% filter(!county_fips %in% present)
  if (nrow(missing) > 0) {
    message(yr, ": missing ", nrow(missing), " counties: ", paste(missing$county_name, collapse = ", "))
  }
}

## Plausibility check on the hand-built 2012/2014 party map: Horsford (D) won NV-4 in 2012 but
## LOST it to Hardy (R) in 2014 -- a well-documented result, used here as a direct sanity check
## that candidate names weren't accidentally swapped between parties.
horsford_check <- rows_2010s %>%
  filter(candidate %in% c("Steven Horsford", "Danny Tarkanian", "Cresent Hardy")) %>%
  group_by(year, candidate) %>%
  summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop")
message("Horsford plausibility check (raw candidate vote totals by year):")
print(horsford_check)
horsford_2012 <- horsford_check$votes[horsford_check$year == 2012 & horsford_check$candidate == "Steven Horsford"]
tarkanian_2012 <- horsford_check$votes[horsford_check$year == 2012 & horsford_check$candidate == "Danny Tarkanian"]
horsford_2014 <- horsford_check$votes[horsford_check$year == 2014 & horsford_check$candidate == "Steven Horsford"]
hardy_2014 <- horsford_check$votes[horsford_check$year == 2014 & horsford_check$candidate == "Cresent Hardy"]
stopifnot(horsford_2012 > tarkanian_2012)
stopifnot(hardy_2014 > horsford_2014)
message("Horsford 2012 win / 2014 loss pattern confirmed -- party mapping is not swapped.")

## NOTE: deliberately NOT folded into elect_cty_final.rds and house_results_coverage.csv NOT
## re-run here -- multiple states are being built in parallel by concurrent agents, and the
## fold-in / tracker-refresh step is being done centrally afterward to avoid a lost-update race
## on those shared files.
