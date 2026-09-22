## New Hampshire: found via OpenElections (github.com/openelections/openelections-data-nh).
##
## NH has weak/symbolic county government -- election results are reported by TOWN, not county
## (10 counties, 234ish towns/unincorporated places). Unlike Connecticut/Massachusetts, this repo
## ships its OWN canonical town->county mapping in Python (`oe_nh/mappings/town_to_county.py`,
## 242 towns, "seeded from 2012/code/county.pkl") -- used that directly (extracted to
## `nh_town_county_crosswalk.csv`) instead of scraping Wikipedia, since it already matches this
## exact data source's own town-name spellings.
##
## GitHub's listing API was rate-limited when checked -- worked around via the
## codeload.github.com tarball fetch (established project convention).
##
## Usable years: 2000's directory has ONLY primary files (president/governor), no general
## election data of any kind -- genuine archive gap, excluded. 2012 and 2014 are the only two
## usable pre-MEDSL years:
##   2012: two separate files, one per congressional district
##     (`..._house__1__town.csv`/`..._house__2__town.csv`), each already carrying its own
##     `county` column directly -- no crosswalk needed this year.
##   2014: a single combined all-office town file. Unlike 2012, the `county` column is literally
##     BLANK for every "Congressional District N" row specifically (though populated for the
##     State House rows in the same file) -- resolved via the town->county crosswalk instead.
## 2016 (town file) used ONLY for the MEDSL cross-check, not folded into the saved output (MEDSL
## already covers 2016+). Same blank-`county`-for-Congressional-District-rows quirk as 2014 (the
## file's `county` column IS populated for other offices like President, which is what an initial
## glance at the file's first few lines showed -- checking the Congressional District subset
## specifically, not just the file's head, is what caught this) -- resolved via the same
## town->county crosswalk as 2014.
##
## Office label is unambiguous and stable: "Congressional District 1"/"Congressional District 2"
## in every year checked (2012/2014/2016) -- no STATE-substring collision risk here, since NH's
## state legislature is labeled "State House District No. N", sharing no ambiguous prefix with
## "Congressional District".
##
## Party codes: "R"/"D" every year, plus "LIB" (2012, a real Libertarian candidate, not a
## pseudo-row) and a blank-party "Scatter" write-in bucket (2014/2016, consistently small vote
## counts across towns -- real, not a pseudo-total; falls to OTHER by construction, matching the
## project's general write-in treatment).
##
## No pseudo-total rows found in any year (checked the full town-name list per year/district --
## no "Total"/"Scatter"/etc. appearing as a TOWN name, unlike states whose pseudo-rows show up
## disguised as a county or candidate value).

source(file.path("R", "00_setup.R"))
library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "new_hampshire")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

county_fips_crosswalk <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))

nh_fips <- county_fips_crosswalk %>% filter(state == "NEW HAMPSHIRE") %>% select(county_name, county_fips)

download_oe <- function(year, remote_path, local_name) {
  dest <- file.path(RAW_DIR, local_name)
  if (!file.exists(dest) || file.size(dest) == 0) {
    url <- paste0("https://raw.githubusercontent.com/openelections/openelections-data-nh/master/",
                   year, "/", remote_path)
    system2("curl", c("-sL", "-A", "'Mozilla/5.0'", "-o", shQuote(dest), shQuote(url)))
  }
  dest
}

## Town -> county lookup, extracted directly from the repo's own
## `oe_nh/mappings/town_to_county.py` (242 towns, 10 counties) rather than an external source,
## since it already matches this data's own town-name spellings exactly.
nh_town_county <- read_csv(
  file.path(RAW_DIR, "nh_town_county_crosswalk.csv"), show_col_types = FALSE
) %>%
  mutate(town = toupper(trimws(town)), county = toupper(trimws(county)))

town_to_county <- function(x) {
  x <- toupper(trimws(x))
  nh_town_county$county[match(x, nh_town_county$town)]
}

clean_county <- function(x) toupper(trimws(x))

to_party <- function(x) {
  x <- toupper(trimws(x))
  case_when(
    x == "D" ~ "DEM",
    x == "R" ~ "REP",
    TRUE ~ "OTHER"
  )
}

## ---- 2012: two per-district files, each with its own `county` column already ----
read_2012_district <- function(remote_name) {
  path <- download_oe(2012, remote_name, paste0("2012_", basename(remote_name)))
  read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    mutate(county = clean_county(county), party = to_party(party), votes = as.numeric(votes)) %>%
    filter(!is.na(county), !is.na(votes)) %>%
    group_by(county, party) %>%
    summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
    mutate(year = 2012)
}
nh_2012 <- bind_rows(
  read_2012_district("20121106__nh__general__house__1__town.csv"),
  read_2012_district("20121106__nh__general__house__2__town.csv")
)

## ---- 2014: combined all-office file, county BLANK for Congressional District rows ----
nh_2014_path <- download_oe(2014, "general/20141104__nh__general__town.csv", "2014_town.csv")
nh_2014 <- read_csv(nh_2014_path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
  filter(grepl("^Congressional District", office)) %>%
  mutate(county = town_to_county(town), party = to_party(party), votes = as.numeric(votes)) %>%
  filter(!is.na(county), !is.na(votes)) %>%
  group_by(county, party) %>%
  summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
  mutate(year = 2014)

nh_by_county <- bind_rows(nh_2012, nh_2014)

elect_he_cty_nh <- nh_by_county %>%
  group_by(year, county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(nh_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(
    state = "NEW HAMPSHIRE", year, cty_fips = county_fips, sample = "HE",
    demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
  ) %>%
  save_step("elect_he_cty_nh")

message("NH House county-level rows built: ", nrow(elect_he_cty_nh), " (of possible ", 2 * 10, ")")
print(table(elect_he_cty_nh$year))

sanity <- elect_he_cty_nh$repuvote + elect_he_cty_nh$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")

## ---- 2016 cross-check against MEDSL (built here for validation only, NOT folded into output) ----
nh_2016_path <- download_oe(2016, "20161108__nh__general__town.csv", "2016_town.csv")
nh_2016 <- read_csv(nh_2016_path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
  filter(grepl("^Congressional District", office)) %>%
  mutate(county = town_to_county(town), party = to_party(party), votes = as.numeric(votes)) %>%
  filter(!is.na(county), !is.na(votes)) %>%
  group_by(county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(nh_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(cty_fips = county_fips, demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote)

medsl_final <- readRDS(file.path(PROJECT_ROOT, "R", "output", "elect_cty_final.rds"))
medsl_nh_2016 <- medsl_final %>%
  filter(sample == "HE", year == 2016, cty_fips %in% nh_fips$county_fips) %>%
  select(cty_fips, demovote_medsl = demovote, repuvote_medsl = repuvote)

cross_check <- nh_2016 %>%
  inner_join(medsl_nh_2016, by = "cty_fips") %>%
  mutate(diff = abs(demovote - demovote_medsl) + abs(repuvote - repuvote_medsl))

message("2016 cross-check vs MEDSL: ", nrow(cross_check), " of 10 counties matched, mean diff = ",
        round(mean(cross_check$diff), 6), ", max diff = ", round(max(cross_check$diff), 6))
print(cross_check %>% arrange(desc(diff)))
