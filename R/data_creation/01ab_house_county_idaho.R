## Idaho: found via OpenElections (github.com/openelections/openelections-data-id). Repo runs
## 1994-2020 (checked via listing API, then guessed general-election dates via raw.githubusercontent.com
## once the listing API rate-limited -- all 7 guessed dates 2002-2014 returned 200, confirming the
## standard "first Tuesday after first Monday in November" pattern holds here).
##
## Idaho has only 2 congressional districts, and its OpenElections office label for the House race
## is unusually unstable across years -- SIX different conventions observed:
##   1996:            "U.S. Representative 1st District" / "U.S. Representative 2nd District" (full)
##   2004/2008/2012:  "UNITED STATES REPRESENTATIVE 1st DISTRICT" / "...2nd DISTRICT" (full, caps)
##   2006/2010/2014:  "U.S. House" (full, clean -- district is in a separate column)
##   1994/1998/2000:  district 1 gets the full "U.S. House of Representatives 1st District" /
##                    "U.S. Representative 1st District" label, but district 2's OWN office field is
##                    truncated to the bare word "2nd District" (district column still correctly "2")
##   2002:            same bare-label bug, but that year's whole file is upper-cased, so district 2's
##                    office field reads "DISTRICT 2" instead of "2nd District"
## Confirmed empirically (full unique office+district listing per year) that the bare "2nd District"/
## "DISTRICT 2" string is NEVER used for any other office in any of these years (State Representative/
## State Senate district values are alphanumeric like "10A" or plain small integers, never spelled out
## as an office name) -- so a single classifier handles every year with no per-year special-casing:
##   classify as US House if office (upper, trimmed) is exactly "2ND DISTRICT" or "DISTRICT 2",
##   OR (does not start with "STATE") AND (contains "HOUSE" or "REPRESENTATIVE").
## General lesson worth repeating from Kentucky/Ohio: a source can truncate an office label for only
## ONE of two/several districts in a given year while leaving others full -- always check the FULL
## unique office list per year (not just a grep for the obvious keyword) before trusting a regex.
##
## Party format also varies (Dem./Rep. with periods vs DEM/REP clean codes) -- normalized by
## stripping "." and upper-casing before matching, works uniformly across every year.
##
## No pseudo-total row found in any year (checked county field for "total"/"write" substrings).
## Idaho's counties never split across districts (whole-county assignment), so a candidate's row is
## NA for every county outside their own district -- handled automatically by the existing
## `filter(!is.na(votes))` step used by every state script, not a bug needing special handling.

source(file.path("R", "00_setup.R"))
library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "idaho")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

county_fips_crosswalk <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))

id_fips <- county_fips_crosswalk %>% filter(state == "IDAHO") %>% select(county_name, county_fips)

download_oe <- function(year, remote_name, local_name) {
  dest <- file.path(RAW_DIR, local_name)
  if (!file.exists(dest) || file.size(dest) == 0) {
    url <- paste0("https://raw.githubusercontent.com/openelections/openelections-data-id/master/",
                   year, "/", remote_name)
    system2("curl", c("-sL", "-A", "'Mozilla/5.0'", "-o", shQuote(dest), shQuote(url)))
  }
  dest
}

to_party <- function(x) {
  x <- toupper(gsub("\\.", "", trimws(x)))
  case_when(
    x == "DEM" ~ "DEM",
    x == "REP" ~ "REP",
    TRUE ~ "OTHER"
  )
}

is_us_house <- function(office) {
  o <- toupper(trimws(office))
  (o %in% c("2ND DISTRICT", "DISTRICT 2")) |
    (!startsWith(o, "STATE") & (grepl("HOUSE", o) | grepl("REPRESENTATIVE", o)))
}

ID_FILES <- tribble(
  ~year, ~remote_name,
  1994,  "19941108__id__general__county.csv",
  1996,  "19961105__id__general__county.csv",
  1998,  "19981103__id__general__county.csv",
  2000,  "20001107__id__general__county.csv",
  2002,  "20021105__id__general__county.csv",
  2004,  "20041102__id__general__county.csv",
  2006,  "20061107__id__general__county.csv",
  2008,  "20081104__id__general__county.csv",
  2010,  "20101102__id__general__county.csv",
  2012,  "20121106__id__general__county.csv",
  2014,  "20141104__id__general__county.csv"
)

read_id_year <- function(year, remote_name) {
  path <- download_oe(year, remote_name, paste0(year, "_general.csv"))
  read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    filter(is_us_house(office)) %>%
    mutate(county = toupper(trimws(county)), party = to_party(party), votes = as.numeric(votes)) %>%
    filter(!is.na(votes)) %>%
    group_by(county, party) %>% summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
    mutate(year = year)
}

id_by_county <- pmap_dfr(ID_FILES, read_id_year)

elect_he_cty_id <- id_by_county %>%
  group_by(year, county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(id_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(
    state = "IDAHO", year, cty_fips = county_fips, sample = "HE",
    demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
  ) %>%
  save_step("elect_he_cty_id")

message("ID House county-level rows built: ", nrow(elect_he_cty_id), " (of possible ", 11 * 44, ")")
print(table(elect_he_cty_id$year))

sanity <- elect_he_cty_id$repuvote + elect_he_cty_id$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")
stopifnot(all(sanity >= 0 & sanity <= 1.001))

low_share <- elect_he_cty_id %>% filter(demovote + repuvote < 0.85) %>% arrange(year)
message(nrow(low_share), " county-years with demovote+repuvote < 0.85:")
print(low_share)

## ---- Cross-check against MEDSL 2016 (not folded in -- MEDSL already covers 2016+) ----
he_2016 <- read_id_year(2016, "20161108__id__general__county.csv") %>%
  group_by(year, county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(id_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(cty_fips = county_fips, demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote)

medsl_2016 <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")) %>%
  filter(sample == "HE", year == 2016, cty_fips %in% id_fips$county_fips) %>%
  select(cty_fips, demovote_medsl = demovote, repuvote_medsl = repuvote)

cmp <- he_2016 %>% inner_join(medsl_2016, by = "cty_fips") %>%
  mutate(diff_r = abs(repuvote - repuvote_medsl), diff_d = abs(demovote - demovote_medsl))
message(nrow(cmp), " counties matched for 2016 cross-check. Max diff (repu): ", round(max(cmp$diff_r), 4),
        ", mean diff (repu): ", round(mean(cmp$diff_r), 5))
print(cmp %>% filter(diff_r > 0.01))

message("Cross-check DONE. elect_he_cty_id.rds saved -- NOT folded into elect_cty_final.rds here ",
        "(parallel-build safety; fold-in done centrally).")
