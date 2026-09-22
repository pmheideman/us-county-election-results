## Maine: found via OpenElections (github.com/openelections/openelections-data-me).
##
## Repo only has year directories 2012/2016/2018/2020 -- MEDSL already covers 2016+, so 2012 is
## the ONLY pre-MEDSL year available here (a much smaller repo than most states; no archive gap
## investigation needed beyond confirming this via the repo's own directory listing).
##
## Unlike Connecticut (also New England, also reports by town), Maine's OpenElections file
## already carries a `county` column directly alongside `city`/`precinct` -- no town->county
## crosswalk needed at all, in either year checked (2012 production, 2016 cross-check).
##
## Office label is clean and IDENTICAL across both years checked: exactly "U.S. House" (no
## per-year alias table needed, unlike most other states).
##
## 2012's file uses bare-CR (`\r`-only, no `\n`) line endings -- same gotcha as Arkansas 2010 --
## `read_csv()` silently returns 0 rows without converting these first. 2016 uses normal CRLF.
##
## Party codes differ by year: 2012 uses bare single letters ("D"/"R"), 2016 uses "DEM"/"REP"
## plus write-in variants ("WRI", "WRI LBT", blank) -- handled with one case-insensitive
## startsWith check that covers both eras (x=="D" or starts with "DEM" -> DEM; same for R/REP).
##
## No pseudo-total row found in either year (checked unique `city`/`precinct` values for
## "TOTAL"-like strings -- none). Small number of rows fall under `county` values "UOC"/"UOCAVA"
## (uniformed/overseas absentee voters, no real county attribution possible) or blank -- dropped
## via the crosswalk join (project's crosswalk already has a placeholder "MAINE UOCAVA" -> NA
## fips row, confirming this is a known, expected non-county bucket, not a bug).
##
## All 16 real Maine counties present in 2012's U.S. House rows -- full coverage, zero exclusions.

source(file.path("R", "00_setup.R"))
library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "maine")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

county_fips_crosswalk <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))

me_fips <- county_fips_crosswalk %>% filter(state == "MAINE") %>% select(county_name, county_fips)

download_oe <- function(year, remote_name, local_name) {
  dest <- file.path(RAW_DIR, local_name)
  if (!file.exists(dest) || file.size(dest) == 0) {
    url <- paste0("https://raw.githubusercontent.com/openelections/openelections-data-me/master/",
                   year, "/", remote_name)
    system2("curl", c("-sL", "-A", "'Mozilla/5.0'", "-o", shQuote(dest), shQuote(url)))
  }
  dest
}

HOUSE_RE <- "^U\\.S\\.\\s*House$"

to_party <- function(x) {
  x <- toupper(trimws(x))
  case_when(
    x == "D" | startsWith(x, "DEM") ~ "DEM",
    x == "R" | startsWith(x, "REP") ~ "REP",
    TRUE ~ "OTHER"
  )
}

clean_county <- function(x) {
  x <- toupper(trimws(x))
  x[x %in% c("", "UOC", "UOCAVA")] <- NA
  x
}

## ---- 2012: bare-CR line endings -- convert to LF before parsing (see header). ----
me_2012_path <- download_oe(2012, "20121106__me__general__town.csv", "2012_town.csv")
me_2012_raw <- gsub("\r", "\n", read_file(me_2012_path))
me_2012 <- read_csv(I(me_2012_raw), show_col_types = FALSE, col_types = cols(.default = "c")) %>%
  filter(grepl(HOUSE_RE, office)) %>%
  mutate(county = clean_county(county), party = to_party(party), votes = as.numeric(votes)) %>%
  filter(!is.na(county), !is.na(votes)) %>%
  group_by(county, party) %>%
  summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
  mutate(year = 2012)

elect_he_cty_me <- me_2012 %>%
  group_by(year, county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(me_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(
    state = "MAINE", year, cty_fips = county_fips, sample = "HE",
    demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
  ) %>%
  save_step("elect_he_cty_me")

message("ME House county-level rows built: ", nrow(elect_he_cty_me), " (of possible 16 counties x 1 year)")
print(table(elect_he_cty_me$year))

sanity <- elect_he_cty_me$repuvote + elect_he_cty_me$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")

## ---- 2016 cross-check against MEDSL (built here for validation only, NOT folded into output).
## 2016 (unlike clean 2012) has per-district pseudo-total rows -- candidate "BALLOTS CAST"/
## "ballots cast" (inconsistent casing) whose votes exactly equal the real candidate-row sum
## (confirmed: Androscoggin CD-2 2016, 114396 = 57976+52436+30+3954), plus a "Blanks" (undervote)
## row. Both excluded by candidate-name, same "verify the pseudo-row's value before trusting it"
## discipline used for Delaware/Colorado. ----
me_2016_path <- download_oe(2016, "20161108__me__general__precinct.csv", "2016_precinct.csv")
me_2016 <- read_csv(me_2016_path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
  filter(grepl(HOUSE_RE, office)) %>%
  filter(!tolower(trimws(candidate)) %in% c("ballots cast", "blanks")) %>%
  mutate(county = clean_county(county), party = to_party(party), votes = as.numeric(votes)) %>%
  filter(!is.na(county), !is.na(votes)) %>%
  group_by(county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(me_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(cty_fips = county_fips, demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote)

medsl_final <- readRDS(file.path(PROJECT_ROOT, "R", "output", "elect_cty_final.rds"))
medsl_me_2016 <- medsl_final %>%
  filter(sample == "HE", year == 2016, cty_fips %in% me_fips$county_fips) %>%
  select(cty_fips, demovote_medsl = demovote, repuvote_medsl = repuvote)

cross_check <- me_2016 %>%
  inner_join(medsl_me_2016, by = "cty_fips") %>%
  mutate(diff = abs(demovote - demovote_medsl) + abs(repuvote - repuvote_medsl))

message("2016 cross-check vs MEDSL: ", nrow(cross_check), " of 16 counties matched, mean diff = ",
        round(mean(cross_check$diff), 6), ", max diff = ", round(max(cross_check$diff), 6))
print(cross_check %>% arrange(desc(diff)))
