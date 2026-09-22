## Iowa: found via OpenElections (github.com/openelections/openelections-data-ia). Part of the
## post-large-states push through the remaining state list. Repo goes back to 1980, but 1980-1996
## only have president/senate files -- no U.S. House file exists for any of those years (checked
## the full directory listing). Real usable House years: 2000, 2002, 2004, 2006, 2008, 2010, 2012,
## 2014 (2016+ already covered by MEDSL).
##
## Four different file shapes across these 8 years, each needing its own handling:
##  - 2000: dedicated, already county-level `us_house_of_representatives__county.csv`, clean party
##    column, office = "U.S. House of Representatives".
##  - 2002/2004/2006/2008: combined all-office `..._general__county.csv` (2008's variant uses a
##    `county` column directly; 2002/2004/2006 use `jurisdiction` instead -- both handled), office
##    label is "United States Representative" this era (distinct from 2000's wording and from
##    "State Representative", which also contains the word "Representative" -- filtered by
##    excluding any label containing "STATE").
##  - 2010: dedicated `us_house_of_representatives__county.csv` again, but with a BLANK party
##    column for every single row (a source limitation, not a parsing bug) -- resolved with an
##    explicit candidate->party hand-map built from Iowa's known 2010 U.S. House delegation (5
##    seats). Also has a real district-label gap: district 2's rows have `district == ""` instead
##    of "2" (same class of bug as Idaho's truncated-district-label issue) -- harmless here since
##    counties are summed regardless of district, but worth knowing if anyone ever needs district
##    breakouts from this file.
##  - 2012: no county-level general file exists at all for U.S. House -- only 99 per-county
##    precinct files. Fetched all 99 (built from the project's own IA county list, not GitHub's
##    directory-listing API, to avoid the documented listing rate limit -- raw.githubusercontent.com
##    file fetches are NOT rate-limited). Clean party column this year. County names in the
##    precinct-file naming convention are lowercase/underscored with apostrophes stripped
##    (O'Brien -> "obrien").
##  - 2014: single statewide precinct file (`..._general__precinct.csv`, ~98k rows) with a `county`
##    column already present -- no per-county fetching needed. Office label is "U.S. House" (yet a
##    fourth distinct wording).
##
## Pseudo-rows encountered ("Total"/"Totals"/"Over Votes"/"Under Votes"/"Write-in"/"Write-In
## Votes"/"Scattering") ARE the standard vote-doubling kind (same class as Kansas/Georgia/NY/
## Colorado): first version matched these by an EXACT string list and silently let several through
## because the literal text varies by year/file ("Totals" not caught by a list containing only
## "Total"; "Write-In Votes" not caught by a list containing only "Write-In") -- this doubled
## totalvote and pulled repuvote+demovote as low as ~0.45-0.5 uniformly across affected
## county-years, caught by that suspiciously-flat sanity floor rather than the county-count check
## (same general lesson as Colorado's blank pseudo-row). Fixed by switching to a prefix-pattern
## match (`is_pseudo_row()`) instead of an exact-string list. **General lesson, worth repeating
## again: a pseudo-row exclusion list built from ONE file's exact wording will not survive contact
## with a second file that phrases the same housekeeping row slightly differently -- match on a
## stable prefix/pattern, not the literal full string, once more than one source file is involved.**
##
## Also found in 2002 only: District 5's real Republican candidate (Steve King) appears with a
## BLANK party field under the bare surname "King" in every District-5 county, a source-file
## mangling (not a write-in or minor candidate) -- confirmed by vote magnitude (comparable to the
## Democratic candidate's totals, not write-in-sized) and fixed with an explicit one-candidate
## override rather than a general rule.
##
## No MEDSL overlap year built (2016+ already fully covered) -- verified via internal consistency
## instead: share in [0,1], totalvote > 0, low-two-party-share county-years checked directly.

source(file.path("R", "00_setup.R"))
library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "iowa")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

county_fips_crosswalk <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))

ia_fips <- county_fips_crosswalk %>% filter(state == "IOWA") %>% select(county_name, county_fips)
stopifnot(nrow(ia_fips) == 99)

download_oe <- function(remote_path, local_name) {
  dest <- file.path(RAW_DIR, local_name)
  if (!file.exists(dest) || file.size(dest) == 0) {
    url <- paste0("https://raw.githubusercontent.com/openelections/openelections-data-ia/master/", remote_path)
    system2("curl", c("-sL", "-A", "'Mozilla/5.0'", "-o", shQuote(dest), shQuote(url)))
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

## Matched by pattern, not exact string -- the literal candidate text for these varies by year/file
## ("Write-In" vs "Write-In Votes", "Total" vs "Totals"), and in every Iowa year checked, every row
## carrying one of these labels is a generic aggregate/housekeeping line, never a specific named
## write-in candidate (unlike e.g. Hawaii's 2008 file, which had one genuine write-in candidate to
## protect against dropping).
is_pseudo_row <- function(candidate) {
  c <- toupper(trimws(candidate))
  grepl("^TOTAL", c) | grepl("^WRITE-IN", c) | grepl("^SCATTERING", c) |
    grepl("^OVER VOTES", c) | grepl("^UNDER VOTES", c)
}

## ---- 2000: dedicated clean county file ----
read_2000 <- function() {
  path <- download_oe("2000/20001107__ia__general__us_house_of_representatives__county.csv", "2000_house.csv")
  read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    filter(!is_pseudo_row(candidate)) %>%
    transmute(county = toupper(trimws(jurisdiction)), candidate = trimws(candidate),
              party = to_party(party), votes = as.numeric(votes), year = 2000)
}

## ---- 2002/2004/2006/2008: combined general file, filter to US House ----
is_us_house_combined <- function(office) {
  ## NOTE: "United States Representative" contains the substring "STATE" (inside "STATES"), so
  ## excluding on grepl("STATE", office) wrongly dropped every real row too -- anchor to "^STATE"
  ## (matches "State Representative"/"State Senator") instead of a bare substring match.
  office <- toupper(trimws(office))
  grepl("REPRESENTATIVE", office) & !grepl("^STATE", office)
}

## 2002 District 5 only: the source file itself mangles "Steve King" (the real Republican
## candidate) into a bare "King" row with a BLANK party field, present identically across every
## District-5 county (confirmed: "King"'s vote counts are large and consistent with a real major
## candidate, not a write-in) -- hand-mapped here rather than left to fall into OTHER. That row's
## neighboring "Write-In Votes" line (large, roughly Shomshor+King in magnitude) and "Totals" line
## (blank) look like a similar one-row label shift in the source, but since "Write-In Votes" is
## already excluded by is_pseudo_row() this doesn't need separate handling.
IA_2002_PARTY_OVERRIDE <- c("King" = "REP")

read_combined <- function(year, remote_name) {
  path <- download_oe(paste0(year, "/", remote_name), paste0(year, "_general.csv"))
  raw <- read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c"))
  county_col <- if ("county" %in% names(raw)) "county" else "jurisdiction"
  raw %>%
    filter(is_us_house_combined(office), !is_pseudo_row(candidate)) %>%
    transmute(
      county = toupper(trimws(.data[[county_col]])), candidate = trimws(candidate),
      party = if (year == 2002) {
        case_when(candidate %in% names(IA_2002_PARTY_OVERRIDE) ~ IA_2002_PARTY_OVERRIDE[candidate],
                  TRUE ~ to_party(party))
      } else {
        to_party(party)
      },
      votes = as.numeric(votes), year = year
    )
}

## ---- 2010: dedicated file, no party column -- hand-mapped from known 2010 IA delegation ----
IA_2010_PARTY <- c(
  "Bruce Braley" = "DEM", "Benjamin M. Lange" = "REP",
  "Dave Loebsack" = "DEM", "Mariannette Miller-Meeks" = "REP",
  "Leonard L. Boswell" = "DEM", "Brad Zaun" = "REP",
  "Tom Latham" = "REP", "Bill Maske" = "DEM",
  "Steve King" = "REP", "Matthew Campbell" = "DEM"
)

read_2010 <- function() {
  path <- download_oe("2010/20101102__ia__general__us_house_of_representatives__county.csv", "2010_house.csv")
  raw <- read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c"))
  raw %>%
    filter(!is_pseudo_row(candidate)) %>%
    transmute(
      county = toupper(trimws(jurisdiction)), candidate = trimws(candidate),
      party = case_when(candidate %in% names(IA_2010_PARTY) ~ IA_2010_PARTY[candidate], TRUE ~ "OTHER"),
      votes = as.numeric(votes), year = 2010
    )
}

## ---- 2012: 99 per-county precinct files, no county-level file exists ----
slugify_county <- function(x) {
  x <- tolower(x)
  x <- gsub("'", "", x)
  x <- gsub(" ", "_", x)
  x
}

read_2012_county <- function(county_name) {
  slug <- slugify_county(county_name)
  remote <- paste0("2012/20121106__ia__general__", slug, "__precinct.csv")
  dest <- file.path(RAW_DIR, paste0("2012_", slug, ".csv"))
  path <- download_oe(remote, paste0("2012_", slug, ".csv"))
  if (!file.exists(path) || file.size(path) == 0) return(NULL)
  raw <- read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c"))
  raw %>%
    filter(toupper(trimws(office)) == "U.S. HOUSE OF REPRESENTATIVES",
           !is_pseudo_row(candidate)) %>%
    transmute(county = toupper(county_name), candidate = trimws(candidate),
              party = to_party(party), votes = as.numeric(votes), year = 2012)
}

read_2012 <- function() {
  map_dfr(ia_fips$county_name, read_2012_county)
}

## ---- 2014: single statewide precinct file, county column already present ----
read_2014 <- function() {
  path <- download_oe("2014/20141104__ia__general__precinct.csv", "2014_general_precinct.csv")
  raw <- read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c"))
  ## the file adds ONE extra "precinct" per district whose `county` field is the literal string "Statewide": its per-candidate votes equal the
  ## sum of that district's real counties exactly (checked against the FEC's official district totals), so including it doubled every 2014 IA House
  ## county-year (confirmed by the FEC reconciliation, R/qa_state_reconcile_congress.R: our totals were 1.995-2.0x the certified district totals).
  raw %>%
    filter(toupper(trimws(office)) == "U.S. HOUSE",
           !is_pseudo_row(candidate), toupper(trimws(county)) != "STATEWIDE") %>%
    transmute(county = toupper(trimws(county)), candidate = trimws(candidate),
              party = to_party(party), votes = as.numeric(votes), year = 2014)
}

message("Fetching Iowa House data, 8 years...")
all_rows <- bind_rows(
  read_2000(),
  read_combined(2002, "20021105__ia__general__county.csv"),
  read_combined(2004, "20041102__ia__general__county.csv"),
  read_combined(2006, "20061107__ia__general__county.csv"),
  read_combined(2008, "20081104__ia__general__county.csv"),
  read_2010(),
  read_2012(),
  read_2014()
) %>%
  filter(!is.na(votes))

message("Raw rows fetched: ", nrow(all_rows))
print(table(all_rows$year))

elect_he_cty_ia <- all_rows %>%
  group_by(year, county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(ia_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(
    state = "IOWA", year, cty_fips = county_fips, sample = "HE",
    demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
  ) %>%
  save_step("elect_he_cty_ia")

message("IA House county-level rows built: ", nrow(elect_he_cty_ia), " (of possible ", 99 * 8, ")")
print(table(elect_he_cty_ia$year))

sanity <- elect_he_cty_ia$repuvote + elect_he_cty_ia$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")
stopifnot(all(sanity >= 0 & sanity <= 1.001))

low_share <- elect_he_cty_ia %>% filter(demovote + repuvote < 0.5) %>% arrange(year)
message(nrow(low_share), " county-years with demovote+repuvote < 0.5:")
print(low_share)

## Which counties are missing per year, for the coverage record
for (yr in sort(unique(elect_he_cty_ia$year))) {
  present <- elect_he_cty_ia %>% filter(year == yr) %>% pull(cty_fips)
  missing <- ia_fips %>% filter(!county_fips %in% present)
  if (nrow(missing) > 0) {
    message(yr, ": missing ", nrow(missing), " counties: ", paste(missing$county_name, collapse = ", "))
  }
}

## NOTE: deliberately NOT folded into elect_cty_final.rds and house_results_coverage.csv NOT
## re-run here -- multiple states are being built in parallel by concurrent agents, and the
## fold-in / tracker-refresh step is being done centrally afterward to avoid a lost-update race
## on those shared files.
