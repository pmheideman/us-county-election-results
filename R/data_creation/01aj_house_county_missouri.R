## Missouri: found via OpenElections (github.com/openelections/openelections-data-mo). Part of the
## post-large-states push through the remaining state list. GitHub's listing API was rate-limited
## when checked, so the whole repo was fetched as a tarball from codeload.github.com instead
## (raw.githubusercontent.com file-by-file fetches also work and aren't rate-limited, used for the
## actual per-file downloads below once file paths were known from the tarball).
##
## Real usable House years: 2000, 2002, 2004, 2006, 2008, 2010, 2012, 2014 (2016+ already covered
## by MEDSL). All years have a clean `county,office,district,party,candidate,votes` schema in their
## `*__general.csv` file EXCEPT 2002, whose `*__general.csv` only contains State Auditor and U.S.
## Senate (no House rows at all, a genuine file-level gap) -- 2002 is instead built from that year's
## precinct-level file (`*__general__precinct.csv`, `county,precinct,office,district,candidate,
## party,votes`), summed by county. Confirmed via readr::read_csv (not naive comma-splitting) that
## no pseudo-total candidate rows exist in ANY year -- every candidate name checked is a real person
## (unlike KS/GA/NY/IA/CO's "Total"/"Write-In Votes"-style rows), so no is_pseudo_row() filter is
## needed here. Party codes are clean throughout (DEM/REP/LIB/CST/GRE/IND/WI/WI2), no write-in-suffix
## style like California's "DEM (W/I)".
##
## Two real county-equivalent gotchas:
##  1. St. Louis City is independent of St. Louis County (same pattern as Virginia's independent
##     cities). The crosswalk has both "ST LOUIS CITY" (29510) and "ST LOUIS COUNTY" (29189), but
##     this source spells the county one bare "ST LOUIS" (no "COUNTY" suffix) -- one alias handles
##     it, `"ST LOUIS" -> "ST LOUIS COUNTY"` (checked this doesn't collide with the separately-
##     spelled "ST LOUIS CITY" rows, which pass through unmodified since they already match the
##     crosswalk's "ST LOUIS CITY" string).
##  2. "Kansas City" appears as its OWN pseudo-jurisdiction row (not a real Missouri county -- KC
##     itself spans Jackson/Clay/Platte/Cass counties) in 2000/2002/2012/2014, carrying a real and
##     non-trivial 4-5% of that year's statewide U.S. House vote. There's no honest way to attribute
##     it to one specific county without a much finer precinct-to-county remap, so (same treatment
##     as Florida's un-broken-out "Fed Abs" row / Hawaii's blank-county absentee rows) it's dropped
##     entirely rather than guessed at -- the crosswalk itself has a "KANSAS CITY" row too, but with
##     two clearly-invalid FIPS values (36000, 2938000 -- neither is a real Missouri FIPS code),
##     confirming this is a known-bad join target, not something to trust.
##
## One genuine per-year county gap found and left alone: 2006 is missing ST FRANCOIS county's House
## row entirely (114 of 115 real counties that year) -- confirmed present for other offices that
## year, a real upstream OpenElections gap, not a bug.
##
## No MEDSL overlap year built (2016+ already fully covered) -- verified via internal consistency
## instead: share in [0,1], totalvote > 0, low-two-party-share county-years checked directly.

source(file.path("R", "00_setup.R"))
library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "missouri")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

county_fips_crosswalk <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))

## The crosswalk itself carries a bogus "KANSAS CITY" entry with two different invalid FIPS codes
## (36000, 2938000 -- neither is a real 5-digit Missouri county FIPS), confirming Kansas City isn't
## a legitimate join target there either -- excluded here, same call as dropping its vote rows below.
mo_fips <- county_fips_crosswalk %>% filter(state == "MISSOURI", !county_fips %in% c(36000, 2938000)) %>%
  select(county_name, county_fips) %>% distinct()
## 115 real county-equivalents (114 counties + independent St. Louis City); crosswalk carries both
## dotted and non-dotted spellings for several counties as separate rows, so raw row count > 115.
stopifnot(length(unique(mo_fips$county_fips)) == 115)

download_oe <- function(remote_path, local_name) {
  dest <- file.path(RAW_DIR, local_name)
  if (!file.exists(dest) || file.size(dest) == 0) {
    url <- paste0("https://raw.githubusercontent.com/openelections/openelections-data-mo/master/", remote_path)
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

normalize_county <- function(x) {
  x <- toupper(trimws(x))
  x <- gsub("^ST\\.?\\s+LOUIS$", "ST LOUIS COUNTY", x)
  x
}

read_general <- function(year, remote_name) {
  path <- download_oe(paste0(year, "/", remote_name), paste0(year, "_general.csv"))
  raw <- read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c"))
  raw %>%
    filter(office == "U.S. House") %>%
    transmute(
      county = normalize_county(county), candidate = trimws(candidate),
      party = to_party(party), votes = as.numeric(votes), year = year
    )
}

## 2002 only: the county-level general.csv has no House rows at all -- use the precinct file
## instead, filtered to U.S. House, summed by county at aggregation time same as any other state's
## precinct-level year.
read_2002_precinct <- function() {
  path <- download_oe("2002/20021105__mo__general__precinct.csv", "2002_general_precinct.csv")
  raw <- read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c"))
  raw %>%
    filter(office == "U.S. House") %>%
    transmute(
      county = normalize_county(county), candidate = trimws(candidate),
      party = to_party(party), votes = as.numeric(votes), year = 2002
    )
}

message("Fetching Missouri House data, 8 years...")
all_rows <- bind_rows(
  read_general(2000, "20001107__mo__general.csv"),
  read_2002_precinct(),
  read_general(2004, "20041102__mo__general.csv"),
  read_general(2006, "20061107__mo__general.csv"),
  read_general(2008, "20081104__mo__general.csv"),
  read_general(2010, "20101102__mo__general.csv"),
  read_general(2012, "20121106__mo__general.csv"),
  read_general(2014, "20141104__mo__general.csv")
) %>%
  filter(!is.na(votes), toupper(trimws(county)) != "KANSAS CITY")

message("Raw rows fetched: ", nrow(all_rows))
print(table(all_rows$year))

elect_he_cty_mo <- all_rows %>%
  group_by(year, county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(mo_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(
    state = "MISSOURI", year, cty_fips = county_fips, sample = "HE",
    demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
  ) %>%
  distinct(cty_fips, year, sample, .keep_all = TRUE) %>%
  save_step("elect_he_cty_mo")

message("MO House county-level rows built: ", nrow(elect_he_cty_mo), " (of possible ", 115 * 8, ")")
print(table(elect_he_cty_mo$year))

sanity <- elect_he_cty_mo$repuvote + elect_he_cty_mo$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")
stopifnot(all(sanity >= 0 & sanity <= 1.001))

low_share <- elect_he_cty_mo %>% filter(demovote + repuvote < 0.5) %>% arrange(year)
message(nrow(low_share), " county-years with demovote+repuvote < 0.5:")
print(low_share)

for (yr in sort(unique(elect_he_cty_mo$year))) {
  present <- elect_he_cty_mo %>% filter(year == yr) %>% pull(cty_fips)
  missing <- mo_fips %>% filter(!county_fips %in% present)
  if (nrow(missing) > 0) {
    message(yr, ": missing ", length(unique(missing$county_fips)), " counties: ",
            paste(unique(missing$county_name), collapse = ", "))
  }
}
