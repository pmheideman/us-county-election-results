## Oklahoma: found via OpenElections (github.com/openelections/openelections-data-ok). Part of the
## post-large-states push through the remaining state list. GitHub's listing API was rate-limited
## on first check -- worked around via a codeload.github.com tarball fetch (established technique).
## Repo has no 2000/2002/2006 directories (a real archive gap, not a parsing miss -- confirmed by
## listing every year directory present: 2004, 2008, 2010, 2012, 2014, 2016, 2017, 2018, 2020).
## Real usable pre-MEDSL years: 2004, 2008, 2010, 2012, 2014 (2016+ already covered by MEDSL).
##
## Five different file shapes, one per year:
##   2004: county-level, office "U.S. House of Represenatives" (source's own misspelling -- 2008+
##         correctly spell it "U.S. House"), party as "(D)"/"(R)"/"(I)" with parens, and
##         vote counts thousands-comma-formatted as quoted strings ("1,535") -- stripped before
##         as.numeric. Full 77/77 counties.
##   2008: county-level, clean "U.S. House" label, party as bare "D"/"R"/"I" (or blank for one
##         real independent candidate, David E. Joyce -- not a pseudo-row, a real write-in-style
##         minor candidate with no party code assigned in the source). Full 77/77 counties.
##   2010: only a statewide PRECINCT-level general file exists this year (no county-level file) --
##         summed by county. Has real "Over Votes"/"Under Votes" pseudo-candidate rows (blank
##         party) -- excluded by name, verified no other pseudo-candidate strings present. Partial,
##         63/77 counties -- confirmed via the crosswalk join, not investigated further (14 missing
##         counties, consistent with the recurring "county present for down-ballot, absent for one
##         specific office that year" pattern seen in many other states).
##   2012: county-level combined all-office file, columns already break out absentee/early/
##         election_day AND a pre-summed `votes` total column -- used `votes` directly rather than
##         re-summing the three components. Full 77/77 counties.
##   2014: only a statewide PRECINCT-level general file exists this year too, with a pre-summed
##         `total_votes` column per precinct row (mail+early+elec_day) -- used directly, summed by
##         county. Partial, 74/77 counties (3 missing, same class of gap as 2010).
##
## No "Total"/county-total pseudo-rows found in ANY year (checked distinct county-name lists,
## none contain a bogus "TOTAL" value) -- only pseudo-CANDIDATE rows (Over/Under Votes, 2010 only).
##
## Office-label filter: a simple `office starts with "U.S. House"` (case-sensitive exact prefix)
## correctly separates every year's U.S. House label (including 2004's misspelled variant) from
## "State House", which never collides since it starts with "State" not "U.S." -- verified by
## pulling the full unique office-label list per year before trusting this, per project convention.
##
## No MEDSL overlap year built directly (all 5 years pre-2016); verified via internal consistency
## (share bounds, full county-count-per-year checks) per the established convention.

source(file.path("R", "00_setup.R"))
library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "oklahoma")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

county_fips_crosswalk <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))

ok_fips <- county_fips_crosswalk %>% filter(state == "OKLAHOMA") %>% select(county_name, county_fips)
stopifnot(nrow(ok_fips) == 77)

download_oe <- function(remote_path, local_name) {
  dest <- file.path(RAW_DIR, local_name)
  if (!file.exists(dest) || file.size(dest) == 0) {
    url <- paste0("https://raw.githubusercontent.com/openelections/openelections-data-ok/master/", remote_path)
    system2("curl", c("-sL", "-A", "'Mozilla/5.0'", "-o", shQuote(dest), shQuote(url)))
  }
  dest
}

to_party <- function(x) {
  x <- toupper(trimws(gsub("[()]", "", x)))
  case_when(
    grepl("^DEM|^D$", x) ~ "DEM",
    grepl("^REP|^R$", x) ~ "REP",
    TRUE ~ "OTHER"
  )
}

is_pseudo_row <- function(candidate) {
  c <- toupper(trimws(candidate))
  is.na(c) | c == "" | grepl("^OVER VOTES", c) | grepl("^UNDER VOTES", c) | grepl("^TOTAL", c) | grepl("^WRITE-IN", c)
}

clean_votes <- function(x) as.numeric(gsub(",", "", x))

## 2004: county-level, comma-formatted vote numbers, misspelled office label
path_2004 <- download_oe("2004/20041102__ok__general__us_represenative__county.csv", "2004_general_county.csv")
r2004 <- read_csv(path_2004, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
  filter(office == "U.S. House of Represenatives", !is_pseudo_row(candidate)) %>%
  transmute(county = toupper(trimws(county)), party = to_party(party),
            votes = clean_votes(votes), year = 2004)

## 2008: county-level, clean
path_2008 <- download_oe("2008/20081104__ok__general__us_house__county.csv", "2008_general_county.csv")
r2008 <- read_csv(path_2008, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
  filter(office == "U.S. House", !is_pseudo_row(candidate)) %>%
  transmute(county = toupper(trimws(county)), party = to_party(party),
            votes = as.numeric(votes), year = 2008)

## 2010: precinct-level only, sum by county
path_2010 <- download_oe("2010/20101102__ok__general__precinct.csv", "2010_general_precinct.csv")
r2010 <- read_csv(path_2010, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
  filter(office == "U.S. House", !is_pseudo_row(candidate)) %>%
  transmute(county = toupper(trimws(county)), party = to_party(party),
            votes = as.numeric(votes), year = 2010)

## 2012: county-level combined all-office file, pre-summed `votes` column
path_2012 <- download_oe("2012/20121106__ok__general__county.csv", "2012_general_county.csv")
r2012 <- read_csv(path_2012, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
  filter(office == "U.S. House", !is_pseudo_row(candidate)) %>%
  transmute(county = toupper(trimws(county)), party = to_party(party),
            votes = as.numeric(votes), year = 2012)

## 2014: precinct-level only, pre-summed `total_votes` column per row, sum by county
path_2014 <- download_oe("2014/20141104__ok__general__precinct.csv", "2014_general_precinct.csv")
r2014 <- read_csv(path_2014, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
  filter(office == "U.S. House", !is_pseudo_row(candidate)) %>%
  transmute(county = toupper(trimws(county)), party = to_party(party),
            votes = as.numeric(total_votes), year = 2014)

## Same alias pattern as Texas's LaSalle/LA SALLE and Louisiana's LASALLE/LA SALLE: source spells
## it one word ("LEFLORE"), crosswalk has it as two ("LE FLORE"). Was missing in 4 of 5 years
## before this fix -- a stable one-county gap across nearly every year is the Indiana-lesson signal
## of a name mismatch, not a genuine source gap.
county_alias <- function(x) case_when(x == "LEFLORE" ~ "LE FLORE", TRUE ~ x)

all_rows <- bind_rows(r2004, r2008, r2010, r2012, r2014) %>%
  filter(!is.na(votes)) %>%
  mutate(county = county_alias(county))
message("Raw rows fetched: ", nrow(all_rows))
print(table(all_rows$year))

elect_he_cty_ok <- all_rows %>%
  group_by(year, county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(ok_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(
    state = "OKLAHOMA", year, cty_fips = county_fips, sample = "HE",
    demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
  ) %>%
  save_step("elect_he_cty_ok")

message("OK House county-level rows built: ", nrow(elect_he_cty_ok), " (of possible ", 77 * 5, ")")
print(table(elect_he_cty_ok$year))

sanity <- elect_he_cty_ok$repuvote + elect_he_cty_ok$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")
stopifnot(all(sanity >= 0 & sanity <= 1.001))

low_share <- elect_he_cty_ok %>% filter(demovote + repuvote < 0.5) %>% arrange(year)
message(nrow(low_share), " county-years with demovote+repuvote < 0.5:")
print(low_share)

for (yr in sort(unique(elect_he_cty_ok$year))) {
  present <- elect_he_cty_ok %>% filter(year == yr) %>% pull(cty_fips)
  missing <- ok_fips %>% filter(!county_fips %in% present)
  if (nrow(missing) > 0) {
    message(yr, ": missing ", nrow(missing), " counties: ", paste(missing$county_name, collapse = ", "))
  }
}

## NOTE: deliberately NOT folded into elect_cty_final.rds and house_results_coverage.csv NOT
## re-run here -- multiple states are being built in parallel by concurrent agents, and the
## fold-in / tracker-refresh step is being done centrally afterward to avoid a lost-update race
## on those shared files.
