## Arizona: found via OpenElections (github.com/openelections/openelections-data-az). Note: AZ's
## own state elections site (apps.azsos.gov) is Cloudflare-bot-blocked when fetched directly, but
## OpenElections is a separate, independently-hosted GitHub archive -- unaffected by that block.
##
## AZ's repo has one clean, already county-level "general" file per even year 2000-2014 (2016+ is
## precinct-only there, left to MEDSL which already covers it), consistent filename pattern
## `<date>__az__general.csv`, and the office column is consistently exactly "U.S. House" every
## year -- no per-year alias table needed, unlike NC/OH. No pseudo-total row found in any year
## (checked explicitly, per the lesson that a state can have more than one such convention -- AZ
## simply has none, like California). Party field is clean codes (DEM/REP/LBT/GRN/...); write-in
## candidates carry party "NONE" with a separate write-in=TRUE flag rather than a suffixed party
## string like California's "DEM (W/I)" -- so no startsWith() collapsing needed here, a stray
## write-in just falls to OTHER same as any other minor-party row, consistent with every state.
##
## 15 counties, all present with matching spelling in the project's FIPS crosswalk (including the
## two-word "La Paz" / "Santa Cruz").

source(file.path("R", "00_setup.R"))
library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "arizona")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

county_fips_crosswalk <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))

az_fips <- county_fips_crosswalk %>% filter(state == "ARIZONA") %>% select(county_name, county_fips)

download_oe <- function(year, remote_name, local_name) {
  dest <- file.path(RAW_DIR, local_name)
  if (!file.exists(dest) || file.size(dest) == 0) {
    url <- paste0("https://raw.githubusercontent.com/openelections/openelections-data-az/master/",
                   year, "/", remote_name)
    system2("curl", c("-sL", "-A", "'Mozilla/5.0'", "-o", shQuote(dest), shQuote(url)))
  }
  dest
}

to_party <- function(x) {
  x <- trimws(x)
  case_when(x == "DEM" ~ "DEM", x == "REP" ~ "REP", TRUE ~ "OTHER")
}

AZ_FILES <- tribble(
  ~year, ~remote_name,
  2000,  "20001107__az__general.csv",
  2002,  "20021105__az__general.csv",
  2004,  "20041102__az__general.csv",
  2006,  "20061107__az__general.csv",
  2008,  "20081104__az__general.csv",
  2010,  "20101102__az__general.csv",
  2012,  "20121106__az__general.csv",
  2014,  "20141104__az__general.csv"
)

read_az_year <- function(year, remote_name) {
  path <- download_oe(year, remote_name, paste0(year, "_general.csv"))
  read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    filter(office == "U.S. House") %>%
    mutate(county = toupper(trimws(county)), party = to_party(party), votes = as.numeric(votes)) %>%
    filter(!is.na(votes)) %>%
    group_by(county, party) %>% summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
    mutate(year = year)
}

az_by_county <- pmap_dfr(AZ_FILES, read_az_year)

elect_he_cty_az <- az_by_county %>%
  group_by(year, county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(az_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(
    state = "ARIZONA", year, cty_fips = county_fips, sample = "HE",
    demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
  ) %>%
  save_step("elect_he_cty_az")

message("AZ House county-level rows built: ", nrow(elect_he_cty_az), " (of possible ", 8 * 15, ")")
print(table(elect_he_cty_az$year))

sanity <- elect_he_cty_az$repuvote + elect_he_cty_az$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")
stopifnot(all(sanity >= 0 & sanity <= 1.001))

low_share <- elect_he_cty_az %>% filter(demovote + repuvote < 0.5) %>% arrange(year)
message(nrow(low_share), " county-years with demovote+repuvote < 0.5:")
print(low_share)

## ---- 2016 cross-check attempted, abandoned as unreliable ----
## AZ's repo has no pre-aggregated county file for 2016 (general-election year), only a precinct
## file (`20161108__az__general__precinct.csv`) -- tried summing it by county to cross-check
## against MEDSL the same way other states did, but this file itself has real data-quality
## problems independent of anything in this script: (1) some precinct rows carry per-precinct
## administrative stats ("Registered Voters", "Times Counted", "Over/Under Votes", etc.) as the
## `candidate` value under the SAME office/district label as the real race, with `party` left NA
## -- inflates totals if not filtered out (a new pseudo-row pattern, worth remembering alongside
## the KS/GA/NY/TX pseudo-total-row lesson: this time it's per-precinct admin stats, not a
## county-total row). Filtering `!is.na(party)` fixes that specific issue and several counties
## (Apache, Gila, Graham, Greenlee, Mohave) then match MEDSL exactly. But (2) several rural
## counties whose true geography splits across multiple congressional districts (Coconino,
## Navajo, Pinal, Yavapai, Santa Cruz, La Paz) come out wildly over- or under-counted even after
## that fix (e.g. Coconino 202,949 vs MEDSL's 57,024; Navajo 3,866 vs 38,903) -- looks like a
## genuine incompleteness/duplication issue in how this particular file attributes split-county
## precincts to districts, not something fixable by filtering. Not investigated further since
## this file was never the target of this script (2016+ is already fully covered by MEDSL) --
## abandoned this cross-check rather than report a number built on a source with confirmed
## unresolved data-quality issues. The real 2000-2014 target years use a completely different,
## already county-aggregated file (`..._general.csv`) with none of these row types present
## (checked explicitly) -- validated via internal consistency instead (share bounds, full county
## coverage every year), same standard applied to California/Georgia when no clean overlap year
## exists.

## ---- Fold into the master panel ----
elect_cty_final <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")) %>%
  bind_rows(elect_he_cty_az %>% select(-state)) %>%
  distinct(cty_fips, year, sample, .keep_all = TRUE) %>%
  arrange(cty_fips, year, sample) %>%
  save_step("elect_cty_final")

message("elect_cty_final.rds updated with AZ 2000-2014. Total rows now: ", nrow(elect_cty_final))
