## Wyoming: found via OpenElections (github.com/openelections/openelections-data-wy). Last state
## in the whole project-wide state-by-state sweep. Wyoming has had one at-large U.S. House seat
## for its entire statehood (same structure as Montana/North Dakota/South Dakota), 23 counties.
##
## Two source eras: (1) five TOP-LEVEL pre-cleaned "house_county" files for 1978/1980/1982/1984/
## 1988 (bonus coverage beyond the project's stated 1990 goal, included since it's essentially
## free once found -- same philosophy as Idaho/Kentucky/Alabama reaching further back than the
## stated target); (2) year-directory `general__county.csv` files for 2000/2002/2004/2008/2010/
## 2012/2014 (2016+ covered by MEDSL). **1986 and 1990-1998 are a genuine gap** -- no files of any
## kind exist in this repo for those years, confirmed via the full tarball listing, not a search
## failure.
##
## Real gotcha: 1988 has TWO top-level files with conflicting content -- "19881104__wy__general__
## house__county.csv" (Nov 4, 1988, a Friday -- not the actual election date) is a corrupted
## ZERO-BYTE-EFFECTIVE file (0 real lines once parsed; `head` showed what looked like one run-on
## line because the file has no newlines between records at all) and "19881108__wy__general__
## house__county.csv" (Nov 8, 1988, the real Tuesday election date) is the genuine, complete file
## (93 lines, matching candidate counts with Libertarian+Independent candidates the other file was
## missing, and higher vote totals per county). Used 19881108 only; 19881104 is unusable.
##
## Office label is clean and exact ("U.S. House", never colliding with "State House" as a
## substring) every year checked, 2000-2014 -- no per-year alias table needed, unlike many other
## states hit by "HOUSE" substring traps. "Ballots Cast" appears as its own separate OFFICE value
## (not mixed into U.S. House rows), so the office filter alone excludes it -- no separate pseudo-
## row handling needed for that. Within the U.S. House subset itself, "Over Votes"/"Under Votes"
## (2010+, blank party) are real ballot-spoilage stats, not votes for any candidate -- excluded
## from totalvote same as every other state's over/under-vote handling. "Write-ins" (blank party,
## real votes) is kept and bucketed OTHER, same as everywhere else in this project.
##
## No pseudo-total county row in any year (checked: every year has exactly 23 distinct counties in
## the House subset, no "Total"-looking county value). Wyoming's crosswalk entry is exactly 23
## counties, no aliasing needed -- all county names (including two-word "Big Horn"/"Hot Springs")
## match the shared crosswalk directly after toupper().

source(file.path("R", "00_setup.R"))
library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "wyoming")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

county_fips_crosswalk <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))

wy_fips <- county_fips_crosswalk %>% filter(state == "WYOMING") %>% select(county_name, county_fips)
stopifnot(length(unique(wy_fips$county_fips)) == 23)

download_oe <- function(remote_path, local_name) {
  dest <- file.path(RAW_DIR, local_name)
  if (!file.exists(dest) || file.size(dest) == 0) {
    url <- paste0("https://raw.githubusercontent.com/openelections/openelections-data-wy/master/", remote_path)
    system2("curl", c("-sL", "-A", "'Mozilla/5.0'", "-o", shQuote(dest), shQuote(url)))
  }
  dest
}

to_party <- function(x) {
  x <- toupper(trimws(x))
  case_when(
    grepl("^DEM|^D$", x) ~ "DEM",
    grepl("^REP|^R$", x) ~ "REP",
    TRUE ~ "OTHER"
  )
}

## ---- 1978-1988: top-level pre-cleaned files (bonus, beyond the 1990 goal) ----
pre1990_files <- tribble(
  ~year, ~remote,
  1978, "19781105__wy__general__house__county.csv",
  1980, "19801104__wy__general__house__county.csv",
  1982, "19821102__wy__general__house__county.csv",
  1984, "19841106__wy__general__house__county.csv",
  1988, "19881108__wy__general__house__county.csv"  ## NOT 19881104 -- see header note, that file is corrupt/empty
)

read_pre1990 <- function(year, remote) {
  path <- download_oe(remote, paste0(year, "_general_house_county.csv"))
  read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    filter(trimws(office) %in% c("U.S. House", "U.S House")) %>%  ## both spellings seen, with/without period
    transmute(county = toupper(trimws(county)), party = to_party(party),
              votes = as.numeric(votes), year = year) %>%
    filter(!is.na(votes))
}

pre1990_rows <- purrr::pmap_dfr(pre1990_files, read_pre1990)
message("Pre-1990 rows fetched: ", nrow(pre1990_rows))
print(table(pre1990_rows$year))

## ---- 2000-2014: year-directory general__county.csv files (2006 missing from repo -- gap) ----
target_years <- tribble(
  ~year, ~remote,
  2000, "2000/20001107__wy__general__county.csv",
  2002, "2002/20021105__wy__general__county.csv",
  2004, "2004/20041102__wy__general__county.csv",
  2008, "2008/20081104__wy__general__county.csv",
  2010, "2010/20101102__wy__general__county.csv",
  2012, "2012/20121106__wy__general__county.csv",
  2014, "2014/20141104__wy__general__county.csv"
)

read_year <- function(year, remote) {
  path <- download_oe(remote, paste0(year, "_general_county.csv"))
  read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    filter(trimws(office) == "U.S. House") %>%
    transmute(county = toupper(trimws(county)), party = to_party(party),
              votes = as.numeric(votes), year = year) %>%
    filter(!is.na(votes))
}

target_rows <- purrr::pmap_dfr(target_years, read_year)
message("2000-2014 rows fetched: ", nrow(target_rows))
print(table(target_rows$year))

all_rows <- bind_rows(pre1990_rows, target_rows)

elect_he_cty_wy <- all_rows %>%
  group_by(year, county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(wy_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(
    state = "WYOMING", year, cty_fips = county_fips, sample = "HE",
    demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
  ) %>%
  save_step("elect_he_cty_wy")

n_by_year <- elect_he_cty_wy %>% count(year, name = "n_counties")
message("WY House county-level rows built: ", nrow(elect_he_cty_wy))
print(n_by_year)

sanity <- elect_he_cty_wy$repuvote + elect_he_cty_wy$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")
stopifnot(all(sanity >= 0 & sanity <= 1.001))

## ---- 2016 cross-check against MEDSL (validation only, not folded in) ----
path_2016 <- download_oe("2016/20161108__wy__general__county.csv", "2016_general_county.csv")
raw_2016 <- suppressWarnings(read_csv(path_2016, show_col_types = FALSE, col_types = cols(.default = "c")))

wy_2016 <- raw_2016 %>%
  filter(trimws(office) == "U.S. House") %>%
  transmute(county = toupper(trimws(county)), party = to_party(party), votes = as.numeric(votes)) %>%
  filter(!is.na(votes)) %>%
  group_by(county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(wy_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(cty_fips = county_fips, demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote)

medsl_final <- readRDS(file.path(PROJECT_ROOT, "R", "output", "elect_cty_final.rds"))
medsl_wy_2016 <- medsl_final %>% filter(sample == "HE", year == 2016, cty_fips %in% wy_fips$county_fips)

if (nrow(medsl_wy_2016) > 0) {
  cmp <- wy_2016 %>%
    inner_join(medsl_wy_2016, by = "cty_fips", suffix = c("_oe", "_medsl")) %>%
    mutate(diff_dem = abs(demovote_oe - demovote_medsl), diff_rep = abs(repuvote_oe - repuvote_medsl))
  message("2016 cross-check vs MEDSL: ", nrow(cmp), " counties matched. ",
          "mean diff_dem=", round(mean(cmp$diff_dem), 5), " max diff_dem=", round(max(cmp$diff_dem), 5),
          " mean diff_rep=", round(mean(cmp$diff_rep), 5), " max diff_rep=", round(max(cmp$diff_rep), 5))
} else {
  message("No MEDSL WY 2016 HE rows found to cross-check against.")
}

## NOTE: deliberately NOT folded into elect_cty_final.rds and house_results_coverage.csv NOT
## re-run here -- fold-in / tracker-refresh is done centrally by the coordinating session.
