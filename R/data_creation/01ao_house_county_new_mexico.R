## New Mexico: found via OpenElections (github.com/openelections/openelections-data-nm). Part of
## the post-large-states push through the remaining state list. GitHub's listing API was rate-
## limited when checked, so the whole repo was fetched as a tarball from codeload.github.com to
## find file paths, then individual files pulled via raw.githubusercontent.com (not rate-limited).
##
## Real usable House years: 2000, 2002, 2008, 2010, 2012 (already county-level `*__general.csv`,
## office exactly "UNITED STATES REPRESENTATIVE") and 2014 (only precinct-level file exists that
## year, `*__general__precinct.csv`, office "U.S. House", summed by county). 2016+ already covered
## by MEDSL. NM has 33 counties, 3 congressional districts throughout this span.
##
## Real bug found and fixed, same class as Delaware's mislabeled statewide "Total" row: 2008, 2010,
## and 2012's general.csv each carry a per-district STATEWIDE total under a BLANK county field
## (county == "") rather than any "Total"-looking string -- confirmed exactly, not assumed: summing
## the real county rows for 2008 district 1's Democratic candidate (Heinrich) reproduces the blank
## row's value to the vote (166271 == sum of the 33 real county rows). Filtered via `county != ""`.
## 2000/2002 have no such row (checked explicitly, 0 blank rows in either).
##
## One genuine file-level gap: 2002's file is missing Cibola County ENTIRELY (checked: the string
## "cibola" doesn't appear anywhere in the file, not just missing from the House rows) -- present in
## every other year checked (2000 has it). A real upstream OpenElections gap, not a bug.
##
## Party labels vary across years (DEMOCRAT/REPUBLICAN in 2000/2002, "DEMOCRATIC PARTY"/"REPUBLICAN
## PARTY" in 2008/2010/2012, "Democratic"/"Republican" in 2014) -- one `startsWith`-based classifier
## (after toupper) covers all of them, no per-year alias table needed. No pseudo-total CANDIDATE rows
## found in any year (checked full candidate list per year) -- 2012's "JEANNE PAHLS (Write-In)" and
## 2014's "(write in)" suffixed names are real write-in candidates with real vote counts, not
## placeholders, and pass through the normal party classifier fine ( "GREEN PARTY"/"Democratic"
## respectively -> OTHER/DEM, both correct).
##
## County names match the crosswalk exactly after toupper/trimws (including multi-word names like
## "DE BACA", "DONA ANA", "LOS ALAMOS", "RIO ARRIBA", "SAN JUAN", "SAN MIGUEL") -- no alias table
## needed, unlike most other states in this sweep.
##
## No MEDSL overlap year built (2016+ already fully covered) -- verified via internal consistency
## instead: share in [0,1], totalvote > 0, low-two-party-share county-years checked directly.

source(file.path("R", "00_setup.R"))
library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "new_mexico")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

county_fips_crosswalk <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))

nm_fips <- county_fips_crosswalk %>% filter(state == "NEW MEXICO") %>%
  select(county_name, county_fips) %>% distinct()
stopifnot(length(unique(nm_fips$county_fips)) == 33)

download_oe <- function(remote_path, local_name) {
  dest <- file.path(RAW_DIR, local_name)
  if (!file.exists(dest) || file.size(dest) == 0) {
    url <- paste0("https://raw.githubusercontent.com/openelections/openelections-data-nm/master/", remote_path)
    system2("curl", c("-sL", "-A", "'Mozilla/5.0'", "-o", shQuote(dest), shQuote(url)))
  }
  dest
}

## 2014's precinct file spells Dona Ana with the Spanish diacritic ("Doña Ana"), which toupper()
## alone doesn't normalize to the crosswalk's plain-ASCII "DONA ANA" -- strip it explicitly.
normalize_county <- function(x) {
  x <- toupper(trimws(x))
  gsub("Ñ", "N", x, fixed = TRUE)
}

to_party <- function(x) {
  x <- toupper(trimws(x))
  case_when(
    grepl("^DEM", x) ~ "DEM",
    grepl("^REP", x) ~ "REP",
    TRUE ~ "OTHER"
  )
}

read_general <- function(year, remote_name) {
  path <- download_oe(paste0(year, "/", remote_name), paste0(year, "_general.csv"))
  raw <- read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c"))
  raw %>%
    filter(office == "UNITED STATES REPRESENTATIVE", trimws(county) != "") %>%
    transmute(
      county = normalize_county(county), candidate = trimws(candidate),
      party = to_party(party), votes = as.numeric(votes), year = year
    )
}

## 2014 only: no county-level general file exists, just the statewide precinct file -- sum by county.
read_2014_precinct <- function() {
  path <- download_oe("2014/20141104__nm__general__precinct.csv", "2014_general_precinct.csv")
  raw <- read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c"))
  raw %>%
    filter(office == "U.S. House") %>%
    transmute(
      county = normalize_county(county), candidate = trimws(candidate),
      party = to_party(party), votes = as.numeric(votes), year = 2014
    )
}

message("Fetching New Mexico House data, 6 years...")
all_rows <- bind_rows(
  read_general(2000, "20001107__nm__general__county.csv"),
  read_general(2002, "20021105__nm__general__county.csv"),
  read_general(2008, "20081104__nm__general.csv"),
  read_general(2010, "20101102__nm__general.csv"),
  read_general(2012, "20121106__nm__general.csv"),
  read_2014_precinct()
) %>%
  filter(!is.na(votes))

message("Raw rows fetched: ", nrow(all_rows))
print(table(all_rows$year))

elect_he_cty_nm <- all_rows %>%
  group_by(year, county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(nm_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(
    state = "NEW MEXICO", year, cty_fips = county_fips, sample = "HE",
    demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
  ) %>%
  distinct(cty_fips, year, sample, .keep_all = TRUE) %>%
  save_step("elect_he_cty_nm")

message("NM House county-level rows built: ", nrow(elect_he_cty_nm), " (of possible ", 33 * 6, ")")
print(table(elect_he_cty_nm$year))

sanity <- elect_he_cty_nm$repuvote + elect_he_cty_nm$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")
stopifnot(all(sanity >= 0 & sanity <= 1.001))

low_share <- elect_he_cty_nm %>% filter(demovote + repuvote < 0.5) %>% arrange(year)
message(nrow(low_share), " county-years with demovote+repuvote < 0.5:")
print(low_share)

for (yr in sort(unique(elect_he_cty_nm$year))) {
  present <- elect_he_cty_nm %>% filter(year == yr) %>% pull(cty_fips)
  missing <- nm_fips %>% filter(!county_fips %in% present)
  if (nrow(missing) > 0) {
    message(yr, ": missing ", length(unique(missing$county_fips)), " counties: ",
            paste(unique(missing$county_name), collapse = ", "))
  }
}
