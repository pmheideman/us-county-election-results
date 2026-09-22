## Washington: found via OpenElections (github.com/openelections/openelections-data-wa). One of the
## last 5 states in the project-wide sweep. Repo spans 2000-2020 with gaps; usable pre-MEDSL general
## election years: 2000, 2002, 2004, 2006, 2010, 2012, 2014 (2008 EXCLUDED: the repo only has
## per-county precinct files for 3 of WA's 39 counties that year -- Adams/Spokane/Whitman -- a
## genuine archive gap too sparse to be worth building).
##
## Three file shapes: (1) 2000/2002/2004/2006 have a single statewide `*__general.csv` already
## aggregated with a `reporting_level` column (kept `== "county"` rows only), office labeled
## "U. S. Representative District #N" (note the two periods-with-spaces, distinct from "State
## Representative District #N" -- matched via `^U\\.?\\s*S\\.?\\s*Representative`), party as a
## clean single-letter/short code (`D`/`R`/`GRN`/`L`/`NL`). (2) 2010/2012 have a single STATEWIDE
## precinct-level file (`*__general__precinct.csv`, no per-county suffix -- distinct from a handful
## of leftover per-county duplicate files also present in those same directories, which were NOT
## used), summed by county; office labeled "US House" (2010, no periods) or "U.S. House" (2012),
## party as "Dem"/"Rep"/"Ind" (2010) or "Democratic"/"Republican" (2012). (3) 2014 has a clean
## county-level file directly (`*__general__county.csv`), office "U.S. House", party full words.
##
## No pseudo-total row found in any of the 7 production years (checked the full candidate list per
## year). One small, accepted data-quality artifact: 2010's statewide precinct file has 6 rows
## (4,617 votes total, out of a statewide total in the hundreds of thousands) with BOTH candidate
## and party blank for a few Pierce County precincts -- doesn't match any single real candidate's
## total (so not a duplicate/pseudo-total), just a small source gap; excluded via a blank-candidate
## filter, same treatment as Hawaii's un-attributable absentee rows.
##
## Washington has used a nonpartisan top-two blanket primary for its GENERAL elections since 2008
## (same system California already handles in 01q) -- expect some districts to have two
## same-party candidates in the general with zero opposing-party votes in a given county. Checked
## for this pattern in the 2010/2012/2014 output; see the low-share investigation below.

source(file.path("R", "00_setup.R"))
library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "washington")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

county_fips_crosswalk <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))

wa_fips <- county_fips_crosswalk %>% filter(state == "WASHINGTON") %>% select(county_name, county_fips)
stopifnot(nrow(wa_fips) == 39)

download_oe <- function(remote_path, local_name) {
  dest <- file.path(RAW_DIR, local_name)
  if (!file.exists(dest) || file.size(dest) == 0) {
    url <- paste0("https://raw.githubusercontent.com/openelections/openelections-data-wa/master/", remote_path)
    system2("curl", c("-sL", "-A", "'Mozilla/5.0'", "-o", shQuote(dest), shQuote(url)))
  }
  dest
}

to_party <- function(x) {
  x <- toupper(trimws(x))
  case_when(x %in% c("D", "DEM", "DEMOCRAT", "DEMOCRATIC") ~ "DEM",
            x %in% c("R", "REP", "REPUBLICAN") ~ "REP",
            TRUE ~ "OTHER")
}

normalize_county <- function(x) toupper(trimws(x))

## ---- 2000/2002/2004/2006: statewide aggregated file, reporting_level=="county" ----
read_wa_statewide <- function(year, remote, local) {
  path <- download_oe(remote, local)
  read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    filter(reporting_level == "county",
           grepl("^U\\.?\\s*S\\.?\\s*Representative", officename)) %>%
    transmute(county = normalize_county(jurisdiction), party = to_party(partycode), votes = as.numeric(votes)) %>%
    filter(!is.na(votes)) %>%
    group_by(county, party) %>% summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
    mutate(year = year)
}

wa_2000 <- read_wa_statewide(2000, "2000/20001107__wa__general.csv", "2000_general.csv")
wa_2002 <- read_wa_statewide(2002, "2002/20021105__wa__general.csv", "2002_general.csv")
wa_2004 <- read_wa_statewide(2004, "2004/20041102__wa__general.csv", "2004_general.csv")
wa_2006 <- read_wa_statewide(2006, "2006/20061107__wa__general.csv", "2006_general.csv")

## ---- 2010/2012: statewide precinct-level file, summed by county ----
## Two real issues found in the 2012 file specifically (checked, not present in 2010): (1) King and
## Snohomish counties carry exact duplicate (county,precinct,candidate,district) rows -- confirmed
## via a direct count, not assumed -- which silently doubled King County's totalvote to an
## implausible 2.23M (a county of ~1.9M people can't cast that many votes in one race); fixed with
## `distinct()` on the full row before summing. (2) administrative per-precinct stat rows
## ("Times Counted", "Times Blank Voted", "Times Over Voted") are recorded as fake "candidates"
## sharing the real office/district label -- same class of bug as Arizona's "Registered Voters"/
## "Times Counted" rows -- inflate totalvote even though they fall into OTHER and don't touch the
## D/R shares; excluded by name. "Write-in" is a real vote category, kept.
ADMIN_STAT_PSEUDO_CANDIDATES <- c("TIMES COUNTED", "TIMES BLANK VOTED", "TIMES OVER VOTED", "REGISTERED VOTERS")

read_wa_precinct <- function(year, remote, local, office_match) {
  path <- download_oe(remote, local)
  df <- read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c"))
  df %>%
    filter(trimws(office) == office_match, !is.na(candidate), trimws(candidate) != "",
           !toupper(trimws(candidate)) %in% ADMIN_STAT_PSEUDO_CANDIDATES) %>%
    distinct(county, precinct, district, candidate, party, votes) %>%
    transmute(county = normalize_county(county), party = to_party(party), votes = as.numeric(votes)) %>%
    filter(!is.na(votes)) %>%
    group_by(county, party) %>% summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
    mutate(year = year)
}

wa_2010 <- read_wa_precinct(2010, "2010/20101102__wa__general__precinct.csv", "2010_general_precinct.csv", "US House")
wa_2012 <- read_wa_precinct(2012, "2012/20121106__wa__general__precinct.csv", "2012_general_precinct.csv", "U.S. House")

## ---- 2014: clean county-level file ----
wa_2014 <- {
  path <- download_oe("2014/20141104__wa__general__county.csv", "2014_general_county.csv")
  read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    filter(trimws(office) == "U.S. House") %>%
    transmute(county = normalize_county(county), party = to_party(party), votes = as.numeric(votes)) %>%
    filter(!is.na(votes)) %>%
    group_by(county, party) %>% summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
    mutate(year = 2014)
}

wa_by_county <- bind_rows(wa_2000, wa_2002, wa_2004, wa_2006, wa_2010, wa_2012, wa_2014)

elect_he_cty_wa <- wa_by_county %>%
  group_by(year, county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(wa_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(
    state = "WASHINGTON", year, cty_fips = county_fips, sample = "HE",
    demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
  ) %>%
  save_step("elect_he_cty_wa")

message("WA House county-level rows built: ", nrow(elect_he_cty_wa), " (of possible ", 39 * 7, ")")
print(table(elect_he_cty_wa$year))

sanity <- elect_he_cty_wa$repuvote + elect_he_cty_wa$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")

low_share <- elect_he_cty_wa %>% filter(demovote + repuvote < 0.5) %>% arrange(year)
message(nrow(low_share), " county-years with demovote+repuvote < 0.5:")
print(low_share)

for (yr in sort(unique(elect_he_cty_wa$year))) {
  present <- elect_he_cty_wa %>% filter(year == yr) %>% pull(cty_fips)
  missing <- wa_fips %>% filter(!county_fips %in% present)
  message(yr, ": ", length(present), "/39 counties", if (nrow(missing) > 0) paste0(" -- missing: ", paste(missing$county_name, collapse = ", ")) else "")
}

## ---- Non-production 2016 cross-check against MEDSL ----
wa_2016_raw <- {
  path <- download_oe("2016/20161108__wa__general__precinct.csv", "2016_general_precinct.csv")
  df <- tryCatch(read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c")), error = function(e) NULL)
  if (is.null(df) || nrow(df) == 0) NULL else {
    df %>% filter(trimws(office) == "U.S. House", !is.na(candidate), trimws(candidate) != "") %>%
      transmute(county = normalize_county(county), party = to_party(party), votes = as.numeric(votes))
  }
}

if (!is.null(wa_2016_raw)) {
  wa_2016 <- wa_2016_raw %>%
    filter(!is.na(votes)) %>%
    group_by(county) %>%
    summarise(
      totalvote = sum(votes, na.rm = TRUE),
      demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
      repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(wa_fips, by = c("county" = "county_name")) %>%
    filter(!is.na(county_fips), totalvote > 0) %>%
    transmute(cty_fips = county_fips, demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote)

  medsl_final <- readRDS(file.path(PROJECT_ROOT, "R", "output", "elect_cty_final.rds"))
  medsl_wa_2016 <- medsl_final %>% filter(sample == "HE", year == 2016, cty_fips %in% wa_fips$county_fips)

  if (nrow(medsl_wa_2016) > 0) {
    cmp <- wa_2016 %>%
      inner_join(medsl_wa_2016, by = "cty_fips", suffix = c("_oe", "_medsl")) %>%
      mutate(diff_dem = abs(demovote_oe - demovote_medsl), diff_rep = abs(repuvote_oe - repuvote_medsl))
    message("2016 cross-check vs MEDSL: ", nrow(cmp), " counties matched. ",
            "mean diff_dem=", round(mean(cmp$diff_dem), 5), " max diff_dem=", round(max(cmp$diff_dem), 5),
            " mean diff_rep=", round(mean(cmp$diff_rep), 5), " max diff_rep=", round(max(cmp$diff_rep), 5))
  } else {
    message("No MEDSL WA 2016 HE rows found to cross-check against.")
  }
} else {
  message("2016 WA precinct file not usable for a bonus cross-check -- skipped, not part of production anyway.")
}

## NOTE: deliberately NOT folded into elect_cty_final.rds and house_results_coverage.csv NOT
## re-run here -- fold-in is done centrally after all remaining states are built.
