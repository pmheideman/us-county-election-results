## South Dakota: found via OpenElections (github.com/openelections/openelections-data-sd). Part of
## the post-large-states push through the remaining state list. Repo only starts at 2014 (checked
## the full tarball listing -- no 2000s/2010/2012 directories exist at all, a genuine archive
## limit, not a search failure), so 2014 is the ONLY usable pre-MEDSL year here.
##
## 2014's county-level file (county,office,district,party,candidate,votes) is already aggregated,
## office labeled exactly "U.S. House", district always "1" (South Dakota has had one at-large
## seat since 1983, same structure as Montana/North Dakota). 132 rows = 66 counties x 2 candidates
## exactly -- no pseudo-total row (checked: no row count is 67x anything, no county value looks
## like "Total"). Party already clean R/D codes.
##
## Some OTHER offices in this same file have real source typos in the county field (Bufaflo,
## Butet, Melletet, Potetr for Buffalo/Butte/Mellette/Potter) but the U.S. House subset is clean --
## none of those typos appear in the House rows specifically (checked directly). "Shannon" county
## (pre-2015 name for what's now Oglala Lakota County, FIPS 46113) matches the shared crosswalk's
## own "SHANNON" alias directly, no extra handling needed.

source(file.path("R", "00_setup.R"))
library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "south_dakota")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

county_fips_crosswalk <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))

sd_fips <- county_fips_crosswalk %>% filter(state == "SOUTH DAKOTA") %>% select(county_name, county_fips)
## Crosswalk carries 67 unique FIPS for SD, not 66 -- it has BOTH the pre-2015 "Shannon" (46113)
## and post-2015 "Oglala Lakota" (46102) entries since presidential returns predating the county's
## 2015 renaming used the old FIPS. SD has always had 66 real counties at any given time.
stopifnot(length(unique(sd_fips$county_fips)) == 67)

download_oe <- function(remote_path, local_name) {
  dest <- file.path(RAW_DIR, local_name)
  if (!file.exists(dest) || file.size(dest) == 0) {
    url <- paste0("https://raw.githubusercontent.com/openelections/openelections-data-sd/master/", remote_path)
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

path_2014 <- download_oe("2014/20141104__sd__general__county.csv", "2014_general_county.csv")
raw_2014 <- read_csv(path_2014, show_col_types = FALSE, col_types = cols(.default = "c"))

all_rows <- raw_2014 %>%
  filter(trimws(office) == "U.S. House") %>%
  transmute(
    county = toupper(trimws(county)), party = to_party(party),
    votes = as.numeric(votes), year = 2014
  ) %>%
  filter(!is.na(votes))

message("Raw rows fetched: ", nrow(all_rows))
print(table(all_rows$year))

elect_he_cty_sd <- all_rows %>%
  group_by(year, county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(sd_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(
    state = "SOUTH DAKOTA", year, cty_fips = county_fips, sample = "HE",
    demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
  ) %>%
  save_step("elect_he_cty_sd")

message("SD House county-level rows built: ", nrow(elect_he_cty_sd), " (of possible ", 66 * 1, ")")

sanity <- elect_he_cty_sd$repuvote + elect_he_cty_sd$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")
stopifnot(all(sanity >= 0 & sanity <= 1.001))

missing <- sd_fips %>% filter(!county_fips %in% elect_he_cty_sd$cty_fips)
if (nrow(missing) > 0) {
  message("Missing ", nrow(missing), " counties: ", paste(missing$county_name, collapse = ", "))
} else {
  message("Full 66/66 county coverage for 2014.")
}

## ---- Non-production 2018 cross-check against MEDSL (validation only, not folded in) ----
## 2018 already has its own clean aggregated county file (`counties/20181106__sd__general__county.csv`),
## easier to use for a bonus sanity check than assembling South Dakota's 66 individual precinct
## files for 2016. 2018 is already covered by MEDSL (2016+), so this is purely a cross-check.
path_2018 <- download_oe("2018/counties/20181106__sd__general__county.csv", "2018_general_county.csv")
raw_2018 <- suppressWarnings(read_csv(path_2018, show_col_types = FALSE, col_types = cols(.default = "c")))

sd_2018 <- raw_2018 %>%
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
  left_join(sd_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(cty_fips = county_fips, demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote)

medsl_final <- readRDS(file.path(PROJECT_ROOT, "R", "output", "elect_cty_final.rds"))
medsl_sd_2018 <- medsl_final %>%
  filter(sample == "HE", year == 2018, cty_fips %in% sd_fips$county_fips)

if (nrow(medsl_sd_2018) > 0) {
  cmp <- sd_2018 %>%
    inner_join(medsl_sd_2018, by = "cty_fips", suffix = c("_oe", "_medsl")) %>%
    mutate(diff_dem = abs(demovote_oe - demovote_medsl), diff_rep = abs(repuvote_oe - repuvote_medsl))
  message("2018 cross-check vs MEDSL: ", nrow(cmp), " counties matched. ",
          "mean diff_dem=", round(mean(cmp$diff_dem), 5), " max diff_dem=", round(max(cmp$diff_dem), 5),
          " mean diff_rep=", round(mean(cmp$diff_rep), 5), " max diff_rep=", round(max(cmp$diff_rep), 5))
} else {
  message("No MEDSL SD 2018 HE rows found to cross-check against.")
}

## NOTE: deliberately NOT folded into elect_cty_final.rds and house_results_coverage.csv NOT
## re-run here -- multiple states are being built in parallel by concurrent agents, and the
## fold-in / tracker-refresh step is being done centrally afterward to avoid a lost-update race
## on those shared files.
