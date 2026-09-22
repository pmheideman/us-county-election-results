## Nebraska: found via OpenElections (github.com/openelections/openelections-data-ne). Part of the
## post-large-states push through the remaining state list. Repo starts at 2006 but 2006 itself is
## an empty placeholder (no data at all that year). Real usable House years: 2008, 2010, 2012, 2014
## (2016+ already covered by MEDSL).
##
## All 4 years share one schema (a single statewide `county,precinct,office,district,party,
## candidate,votes[,legdist]` file with a `county` column already present) -- no per-county
## fetching needed, unlike Iowa/some other states. Nebraska's unicameral legislature (no "State
## Representative" title exists at all -- state legislators are just "Legislature"/"Senator") means
## the office label "Representative" (2010/2012/2014) is UNAMBIGUOUSLY the U.S. House seat with no
## "STATE" collision risk that other states need to guard against -- confirmed by pulling the full
## unique office list per year before trusting this. 2008 alone uses a different label, "U.S. House".
##
## Party labels vary by year: full words "Democrat"/"Republican" (2008, matched fine by the usual
## ^DEM/^REP prefix rule) vs codes "DEM"/"REP" (+ "ZZZ" for an independent, "LIB" for Libertarian)
## in 2010/2012/2014.
##
## 2012/2014 have a "Countywide" precinct row alongside "Absentee/Early Vote"/"Provisional" and the
## normal numbered precincts -- checked directly whether this is a pseudo-total (Kansas/Georgia/NY/
## Colorado-style double-counting row): it is NOT. In 2012 it is always 0 votes. In 2014 exactly one
## county (Grant) has a nonzero "Countywide" row, and Grant ALSO has separate "Absentee/Early Vote"/
## "Grant County"/"Provisional" precinct rows with no overlap -- "Countywide" here is a genuine
## precinct-type bucket (in-person countywide voting location), not a duplicate of the other rows,
## so it is summed like any other precinct rather than excluded. **General lesson: a plausible-
## looking "Countywide"/"Total"-shaped precinct label is not automatically a pseudo-total row --
## check whether its value double-counts an already-present sum (the Colorado/Iowa pattern) or is
## itself just one more disjoint precinct bucket (this case) before deciding to exclude it.**
##
## No pseudo-row candidate names (Total/Write-In/Scattering/Over Votes/Under Votes) were found
## within the U.S. House subset in any of the 4 years -- checked the full candidate list per year
## before writing the aggregation, all real named candidates.
##
## No MEDSL overlap year built (2016+ already fully covered) -- verified via internal consistency
## instead: share in [0,1], totalvote > 0, low-two-party-share county-years checked directly.

source(file.path("R", "00_setup.R"))
library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "nebraska")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

county_fips_crosswalk <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))

ne_fips <- county_fips_crosswalk %>% filter(state == "NEBRASKA") %>% select(county_name, county_fips)
stopifnot(nrow(ne_fips) == 93)

download_oe <- function(remote_path, local_name) {
  dest <- file.path(RAW_DIR, local_name)
  if (!file.exists(dest) || file.size(dest) == 0) {
    url <- paste0("https://raw.githubusercontent.com/openelections/openelections-data-ne/master/", remote_path)
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

is_pseudo_row <- function(candidate) {
  c <- toupper(trimws(candidate))
  grepl("^TOTAL", c) | grepl("^WRITE-IN", c) | grepl("^SCATTERING", c) |
    grepl("^OVER VOTES", c) | grepl("^UNDER VOTES", c)
}

read_year <- function(year, remote_name, office_target) {
  path <- download_oe(paste0(year, "/", remote_name), paste0(year, "_general_precinct.csv"))
  raw <- read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c"))
  raw %>%
    filter(toupper(trimws(office)) == toupper(office_target), !is_pseudo_row(candidate)) %>%
    transmute(
      county = toupper(trimws(county)), candidate = trimws(candidate),
      party = to_party(party), votes = as.numeric(votes), year = year
    )
}

message("Fetching Nebraska House data, 4 years...")
all_rows <- bind_rows(
  read_year(2008, "20081104__ne__general__precinct.csv", "U.S. House"),
  read_year(2010, "20101102__ne__general__precinct.csv", "Representative"),
  read_year(2012, "20121106__ne__general__precinct.csv", "Representative"),
  read_year(2014, "20141104__ne__general__precinct.csv", "Representative")
) %>%
  filter(!is.na(votes))

message("Raw rows fetched: ", nrow(all_rows))
print(table(all_rows$year))

elect_he_cty_ne <- all_rows %>%
  group_by(year, county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(ne_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(
    state = "NEBRASKA", year, cty_fips = county_fips, sample = "HE",
    demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
  ) %>%
  save_step("elect_he_cty_ne")

message("NE House county-level rows built: ", nrow(elect_he_cty_ne), " (of possible ", 93 * 4, ")")
print(table(elect_he_cty_ne$year))

sanity <- elect_he_cty_ne$repuvote + elect_he_cty_ne$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")
stopifnot(all(sanity >= 0 & sanity <= 1.001))

low_share <- elect_he_cty_ne %>% filter(demovote + repuvote < 0.5) %>% arrange(year)
message(nrow(low_share), " county-years with demovote+repuvote < 0.5:")
print(low_share)

## Which counties are missing per year, for the coverage record
for (yr in sort(unique(elect_he_cty_ne$year))) {
  present <- elect_he_cty_ne %>% filter(year == yr) %>% pull(cty_fips)
  missing <- ne_fips %>% filter(!county_fips %in% present)
  if (nrow(missing) > 0) {
    message(yr, ": missing ", nrow(missing), " counties: ", paste(missing$county_name, collapse = ", "))
  }
}

## NOTE: deliberately NOT folded into elect_cty_final.rds and house_results_coverage.csv NOT
## re-run here -- multiple states are being built in parallel by concurrent agents, and the
## fold-in / tracker-refresh step is being done centrally afterward to avoid a lost-update race
## on those shared files.
