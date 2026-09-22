## Rhode Island: found via OpenElections (github.com/openelections/openelections-data-ri).
##
## RI has no county government at all (abolished 1842) -- all 39 municipalities report directly,
## with 5 nominal counties used only as geographic/judicial regions. No source-provided
## town->county mapping exists in this repo (unlike New Hampshire's), so the crosswalk here is
## hardcoded from each county's Wikipedia page (cross-checked: the 39 towns appearing in every
## year's file below match this list exactly, no leftover/unmapped town names).
##
## Repo starts at 2008 (no 1990s/2000s coverage -- genuine archive limit). Four candidate years
## in the pre-MEDSL window:
##   2008: `county,town,office,district,party,candidate,votes` -- REAL county names given
##     directly, no crosswalk needed this year. Office exactly "U.S. Representative". Party
##     "Democratic"/"Republican"/"Independent" (full words). No pseudo-total rows found (checked
##     the full candidate list for the U.S. House subset).
##   2010: EXCLUDED -- `20101102__ri__general__town.csv` is a byte-for-byth duplicate of the 2012
##     file (confirmed: identical line count, `diff` shows zero differences), including its
##     "PRESIDENTIAL ELECTORS" rows for Obama/McCain, which is impossible for a real 2010
##     (midterm, no presidential race) general election file. A genuine upstream mislabeling in
##     the OpenElections repo, not a parsing issue on this end -- there is no real 2010 RI file to
##     recover from this source.
##   2012: `county,office,district,party,candidate,votes` -- the `county` column is actually TOWN
##     (confirmed against the hardcoded crosswalk: all 39 values are real RI towns, not the 5 real
##     counties). Office exactly "REPRESENTATIVE IN CONGRESS DISTRICT 1"/"DISTRICT 2" (distinct
##     from "REPRESENTATIVE IN GENERAL ASSEMBLY DISTRICT N", RI's state legislature -- no
##     substring-collision risk). Party codes DEM/REP/IND/NON(write-in). Has a pseudo-total row
##     per (town, district): blank party, `candidate=="Total"` -- confirmed exactly equal to the
##     sum of that block's real candidate rows (e.g. Barrington district 1: Total 9312 =
##     4635+4082+586+9), filtered out.
##   2014: `town,office,party,candidate,votes` (no separate district column -- district is
##     embedded in the office string only). Same office pattern as 2012. Party DEM/REP/NON. NO
##     pseudo-total row this year (checked explicitly -- unlike 2012, don't assume the same
##     exclusion pattern applies to every year).
## 2016 (town file) used ONLY for the MEDSL cross-check, not folded into the output (MEDSL already
## covers 2016+). Office label changes AGAIN this year to plain "U.S. House" (a third distinct
## label across the 3 years checked: "U.S. Representative" / "REPRESENTATIVE IN CONGRESS DISTRICT
## N" / "U.S. House") -- confirms the project's standing rule to re-check the office label every
## year rather than reusing one regex. 2016 also has its own "Total" pseudo-row (`candidate ==
## "Total"`, blank party), same as 2012.

source(file.path("R", "00_setup.R"))
library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "rhode_island")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

county_fips_crosswalk <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))

ri_fips <- county_fips_crosswalk %>% filter(state == "RHODE ISLAND") %>% select(county_name, county_fips)

download_oe <- function(year, remote_path, local_name) {
  dest <- file.path(RAW_DIR, local_name)
  if (!file.exists(dest) || file.size(dest) == 0) {
    url <- paste0("https://raw.githubusercontent.com/openelections/openelections-data-ri/master/",
                   year, "/", remote_path)
    system2("curl", c("-sL", "-A", "'Mozilla/5.0'", "-o", shQuote(dest), shQuote(url)))
  }
  dest
}

## Town -> county, hardcoded from each RI county's Wikipedia page (39 towns, 5 counties; verified
## against the union of town names appearing in the 2012/2014 files -- exact match, no leftovers).
ri_town_county <- tribble(
  ~town, ~county,
  "BARRINGTON", "BRISTOL", "BRISTOL", "BRISTOL", "WARREN", "BRISTOL",
  "COVENTRY", "KENT", "EAST GREENWICH", "KENT", "WARWICK", "KENT",
  "WEST GREENWICH", "KENT", "WEST WARWICK", "KENT",
  "JAMESTOWN", "NEWPORT", "LITTLE COMPTON", "NEWPORT", "MIDDLETOWN", "NEWPORT",
  "NEWPORT", "NEWPORT", "PORTSMOUTH", "NEWPORT", "TIVERTON", "NEWPORT",
  "BURRILLVILLE", "PROVIDENCE", "CENTRAL FALLS", "PROVIDENCE", "CRANSTON", "PROVIDENCE",
  "CUMBERLAND", "PROVIDENCE", "EAST PROVIDENCE", "PROVIDENCE", "FOSTER", "PROVIDENCE",
  "GLOCESTER", "PROVIDENCE", "JOHNSTON", "PROVIDENCE", "LINCOLN", "PROVIDENCE",
  "NORTH PROVIDENCE", "PROVIDENCE", "NORTH SMITHFIELD", "PROVIDENCE", "PAWTUCKET", "PROVIDENCE",
  "PROVIDENCE", "PROVIDENCE", "SCITUATE", "PROVIDENCE", "SMITHFIELD", "PROVIDENCE",
  "WOONSOCKET", "PROVIDENCE",
  "CHARLESTOWN", "WASHINGTON", "EXETER", "WASHINGTON", "HOPKINTON", "WASHINGTON",
  "NARRAGANSETT", "WASHINGTON", "NEW SHOREHAM", "WASHINGTON", "NORTH KINGSTOWN", "WASHINGTON",
  "RICHMOND", "WASHINGTON", "SOUTH KINGSTOWN", "WASHINGTON", "WESTERLY", "WASHINGTON"
)
stopifnot(nrow(ri_town_county) == 39)

town_to_county <- function(x) {
  x <- toupper(trimws(x))
  ri_town_county$county[match(x, ri_town_county$town)]
}

clean_county <- function(x) toupper(trimws(x))

to_party <- function(x) {
  x <- toupper(trimws(x))
  case_when(
    x %in% c("D", "DEM", "DEMOCRATIC") ~ "DEM",
    x %in% c("R", "REP", "REPUBLICAN") ~ "REP",
    TRUE ~ "OTHER"
  )
}

is_pseudo_row <- function(candidate) {
  toupper(trimws(candidate)) == "TOTAL"
}

## ---- 2008: real county column given directly, no crosswalk needed ----
ri_2008_path <- download_oe(2008, "20081104__ri__general__town.csv", "2008_town.csv")
ri_2008 <- read_csv(ri_2008_path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
  filter(office == "U.S. Representative", !is_pseudo_row(candidate)) %>%
  mutate(county = clean_county(county), party = to_party(party), votes = as.numeric(votes)) %>%
  filter(!is.na(county), !is.na(votes)) %>%
  group_by(county, party) %>%
  summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
  mutate(year = 2008)

## ---- 2012: `county` column is actually TOWN; has a per-(town,district) "Total" pseudo-row ----
ri_2012_path <- download_oe(2012, "20121106__ri__general__town.csv", "2012_town.csv")
ri_2012 <- read_csv(ri_2012_path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
  filter(grepl("^REPRESENTATIVE IN CONGRESS", office), !is_pseudo_row(candidate)) %>%
  mutate(county = town_to_county(county), party = to_party(party), votes = as.numeric(votes)) %>%
  filter(!is.na(county), !is.na(votes)) %>%
  group_by(county, party) %>%
  summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
  mutate(year = 2012)

## ---- 2014: `town` column; no district column (embedded in office string); no Total row ----
ri_2014_path <- download_oe(2014, "20141104__ri__general__town.csv", "2014_town.csv")
ri_2014 <- read_csv(ri_2014_path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
  filter(grepl("^REPRESENTATIVE IN CONGRESS", office), !is_pseudo_row(candidate)) %>%
  mutate(county = town_to_county(town), party = to_party(party), votes = as.numeric(votes)) %>%
  filter(!is.na(county), !is.na(votes)) %>%
  group_by(county, party) %>%
  summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
  mutate(year = 2014)

ri_by_county <- bind_rows(ri_2008, ri_2012, ri_2014)

elect_he_cty_ri <- ri_by_county %>%
  group_by(year, county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(ri_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(
    state = "RHODE ISLAND", year, cty_fips = county_fips, sample = "HE",
    demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
  ) %>%
  save_step("elect_he_cty_ri")

message("RI House county-level rows built: ", nrow(elect_he_cty_ri), " (of possible ", 3 * 5, ")")
print(table(elect_he_cty_ri$year))

sanity <- elect_he_cty_ri$repuvote + elect_he_cty_ri$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")

## ---- 2016 cross-check against MEDSL (built here for validation only, NOT folded into output) ----
ri_2016_path <- download_oe(2016, "20161108__ri__general__town.csv", "2016_town.csv")
ri_2016 <- read_csv(ri_2016_path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
  filter(office == "U.S. House", !is_pseudo_row(candidate)) %>%
  mutate(county = town_to_county(county), party = to_party(party), votes = as.numeric(votes)) %>%
  filter(!is.na(county), !is.na(votes)) %>%
  group_by(county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(ri_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(cty_fips = county_fips, demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote)

medsl_final <- readRDS(file.path(PROJECT_ROOT, "R", "output", "elect_cty_final.rds"))
medsl_ri_2016 <- medsl_final %>%
  filter(sample == "HE", year == 2016, cty_fips %in% ri_fips$county_fips) %>%
  select(cty_fips, demovote_medsl = demovote, repuvote_medsl = repuvote)

cross_check <- ri_2016 %>%
  inner_join(medsl_ri_2016, by = "cty_fips") %>%
  mutate(diff = abs(demovote - demovote_medsl) + abs(repuvote - repuvote_medsl))

message("2016 cross-check vs MEDSL: ", nrow(cross_check), " of 5 counties matched, mean diff = ",
        round(mean(cross_check$diff), 6), ", max diff = ", round(max(cross_check$diff), 6))
print(cross_check %>% arrange(desc(diff)))
