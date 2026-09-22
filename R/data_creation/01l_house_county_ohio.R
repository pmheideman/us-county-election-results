## Ohio: found via OpenElections (github.com/openelections/openelections-data-oh), same followup
## survey that produced Kansas (01j) and Pennsylvania (01k). Ohio needed more per-year handling
## than either of those -- confirmed 6 different file layouts across 6 usable years (worse than
## PA's 2, closer to NC's per-year format churn in 01d) -- but no pseudo-total row in any of them
## (checked every year explicitly), and all 6 years have the full 88/88 Ohio counties.
##
## Office label varies every single year: "U.S. House" (2000, 2012, 2014), "US Representative"
## (2002, 2008), "US House of Representatives" (2006) -- matched with a single regex
## `^U\.?S\.? ?(House|Representative)` (periods after BOTH letters in some years' "U.S." but not
## others' "US" -- an earlier version only made the period after the first letter optional and
## silently matched zero rows for every "U.S. House"-labeled year, 2000/2012/2014, until fixed;
## caught immediately by the per-year row counts in the "of possible" sanity message below rather
## than a silent gap). Also word-boundary-safe against "State House"/"State Representative", which
## share the "House"/"Representative" word but never "US"/"U.S." in front.
## Party format also varies: single letters (2000, 2002, 2014: R/D/plus WI for write-in, etc.) vs.
## full words (2006, 2008, 2012: Republican/Democratic/etc.) -- handled with an explicit alias map
## rather than assuming one convention.
##
## Two years intentionally NOT included here, left as an open gap:
## - 2004: OpenElections' OH repo has no general-election file at all for this year (only a
##   primary file) -- a genuine gap in their own archive, not a format problem on our end.
## - 2010: the general-election file (`..._general__precinct.csv`) has NO party column at all
##   (just county/district/candidate/votes) -- would need a candidate-name-to-party lookup like
##   Kentucky's Wikipedia-infobox approach (01g/01h) to close. Not attempted here; flagged as a
##   separate, smaller follow-up rather than blocking the other 6 years.

source(file.path("R", "00_setup.R"))
library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "ohio")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

county_fips_crosswalk <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))

oh_fips <- county_fips_crosswalk %>% filter(state == "OHIO") %>% select(county_name, county_fips)

download_oe <- function(year, remote_name, local_name) {
  dest <- file.path(RAW_DIR, local_name)
  if (!file.exists(dest) || file.size(dest) == 0) {
    url <- paste0("https://raw.githubusercontent.com/openelections/openelections-data-oh/master/",
                   year, "/", remote_name)
    system2("curl", c("-sL", "-A", "'Mozilla/5.0'", "-o", shQuote(dest), shQuote(url)))
  }
  dest
}

PARTY_ALIAS <- c(R = "REP", REP = "REP", Republican = "REP",
                  D = "DEM", DEM = "DEM", Democratic = "DEM")
to_party <- function(x) coalesce(PARTY_ALIAS[trimws(x)], "OTHER")

OH_FILES <- tribble(
  ~year, ~remote_name,
  2000,  "20001107__oh__general__house.csv",
  2002,  "20021105__oh__general.csv",
  2006,  "20061107__OH__general__precinct.csv",
  2008,  "20081104__oh__general.csv",
  2012,  "20121106__oh__general__house.csv",
  2014,  "20141104__oh__general.csv"
)

read_oh_year <- function(year, remote_name) {
  path <- download_oe(year, remote_name, paste0(year, "_raw.csv"))
  raw <- read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c"))
  raw %>%
    filter(grepl("^U\\.?S\\.? ?(House|Representative)", office, ignore.case = TRUE)) %>%
    mutate(county = toupper(trimws(county)), party = to_party(party), votes = as.numeric(votes)) %>%
    filter(!is.na(votes)) %>%
    group_by(county, party) %>% summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
    mutate(year = year)
}

oh_by_county <- pmap_dfr(OH_FILES, read_oh_year)

elect_he_cty_oh <- oh_by_county %>%
  group_by(year, county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(oh_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(
    state = "OHIO", year, cty_fips = county_fips, sample = "HE",
    demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
  ) %>%
  save_step("elect_he_cty_oh")

message("OH House county-level rows built: ", nrow(elect_he_cty_oh), " (of possible ", 6 * 88, ")")
print(table(elect_he_cty_oh$year))

sanity <- elect_he_cty_oh$repuvote + elect_he_cty_oh$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")

## ---- Fold into the master panel ----
elect_cty_final <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")) %>%
  bind_rows(elect_he_cty_oh %>% select(-state)) %>%
  distinct(cty_fips, year, sample, .keep_all = TRUE) %>%
  arrange(cty_fips, year, sample) %>%
  save_step("elect_cty_final")

message("elect_cty_final.rds updated with OH 2000-2014 (2004 and 2010 excluded, see header notes). ",
        "Total rows now: ", nrow(elect_cty_final))
