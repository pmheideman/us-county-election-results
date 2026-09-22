## Vermont: found via OpenElections (github.com/openelections/openelections-data-vt). One of the
## last 5 states in the project-wide sweep. Repo only starts at 2012 (checked the full directory
## listing) -- a genuine small repo, not a search failure, so 2012/2014 is the entire usable
## pre-MEDSL window (same situation as Minnesota).
##
## Vermont has ONE at-large U.S. House seat (Peter Welch both years) -- no district filtering
## needed. Both years already have a `county` column directly in the precinct-level file (unlike
## CT/MA/RI/NH, no town->county crosswalk needed here). Office consistently exactly "U.S. House",
## distinct from "State House".
##
## Pseudo-rows: `candidate == "Total Votes Cast"` (both years, a real per-precinct duplicate of the
## real candidate sum) and `candidate == "Blanks"` (both years -- literal blank/undervoted ballots,
## not a real vote for anyone, same treatment as other states' "Over/Under Votes" rows: excluded
## from totalvote entirely, not counted as OTHER). Write-ins ARE counted as OTHER (a real vote
## choice), unlike Blanks.

source(file.path("R", "00_setup.R"))
library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "vermont")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

county_fips_crosswalk <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))

vt_fips <- county_fips_crosswalk %>% filter(state == "VERMONT") %>% select(county_name, county_fips)
stopifnot(nrow(vt_fips) == 14)

download_oe <- function(remote_path, local_name) {
  dest <- file.path(RAW_DIR, local_name)
  if (!file.exists(dest) || file.size(dest) == 0) {
    url <- paste0("https://raw.githubusercontent.com/openelections/openelections-data-vt/master/", remote_path)
    system2("curl", c("-sL", "-A", "'Mozilla/5.0'", "-o", shQuote(dest), shQuote(url)))
  }
  dest
}

to_party <- function(x) {
  x <- toupper(trimws(x))
  case_when(startsWith(x, "DEM") ~ "DEM",
            startsWith(x, "REP") ~ "REP",
            TRUE ~ "OTHER")
}

normalize_county <- function(x) toupper(trimws(x))

PSEUDO_CANDIDATES <- c("TOTAL VOTES CAST", "BLANKS")

read_vt_year <- function(year, remote, local) {
  path <- download_oe(remote, local)
  df <- read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c"))
  df %>%
    filter(trimws(office) == "U.S. House",
           !toupper(trimws(candidate)) %in% PSEUDO_CANDIDATES) %>%
    transmute(county = normalize_county(county), party = to_party(party), votes = as.numeric(votes)) %>%
    filter(!is.na(votes)) %>%
    group_by(county, party) %>% summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
    mutate(year = year)
}

vt_2012 <- read_vt_year(2012, "2012/20121106__vt__general__precinct.csv", "2012_general_precinct.csv")
vt_2014 <- read_vt_year(2014, "2014/20141104__vt__general__precinct.csv", "2014_general_precinct.csv")

vt_by_county <- bind_rows(vt_2012, vt_2014)

elect_he_cty_vt <- vt_by_county %>%
  group_by(year, county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(vt_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(
    state = "VERMONT", year, cty_fips = county_fips, sample = "HE",
    demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
  ) %>%
  save_step("elect_he_cty_vt")

message("VT House county-level rows built: ", nrow(elect_he_cty_vt), " (of possible ", 14 * 2, ")")
print(table(elect_he_cty_vt$year))

sanity <- elect_he_cty_vt$repuvote + elect_he_cty_vt$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")

low_share <- elect_he_cty_vt %>% filter(demovote + repuvote < 0.5) %>% arrange(year)
message(nrow(low_share), " county-years with demovote+repuvote < 0.5:")
print(low_share)

for (yr in sort(unique(elect_he_cty_vt$year))) {
  present <- elect_he_cty_vt %>% filter(year == yr) %>% pull(cty_fips)
  missing <- vt_fips %>% filter(!county_fips %in% present)
  message(yr, ": ", length(present), "/14 counties", if (nrow(missing) > 0) paste0(" -- missing: ", paste(missing$county_name, collapse = ", ")) else "")
}

## ---- Non-production 2016 cross-check against MEDSL ----
vt_2016_raw <- {
  path <- download_oe("2016/20161108__vt__general__precinct.csv", "2016_general_precinct.csv")
  df <- read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c"))
  df %>% filter(trimws(office) == "U.S. House", !toupper(trimws(candidate)) %in% PSEUDO_CANDIDATES) %>%
    transmute(county = normalize_county(county), party = to_party(party), votes = as.numeric(votes))
}
vt_2016 <- vt_2016_raw %>%
  filter(!is.na(votes)) %>%
  group_by(county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(vt_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(cty_fips = county_fips, demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote)

medsl_final <- readRDS(file.path(PROJECT_ROOT, "R", "output", "elect_cty_final.rds"))
medsl_vt_2016 <- medsl_final %>% filter(sample == "HE", year == 2016, cty_fips %in% vt_fips$county_fips)

if (nrow(medsl_vt_2016) > 0) {
  cmp <- vt_2016 %>%
    inner_join(medsl_vt_2016, by = "cty_fips", suffix = c("_oe", "_medsl")) %>%
    mutate(diff_dem = abs(demovote_oe - demovote_medsl), diff_rep = abs(repuvote_oe - repuvote_medsl))
  message("2016 cross-check vs MEDSL: ", nrow(cmp), " counties matched. ",
          "mean diff_dem=", round(mean(cmp$diff_dem), 5), " max diff_dem=", round(max(cmp$diff_dem), 5),
          " mean diff_rep=", round(mean(cmp$diff_rep), 5), " max diff_rep=", round(max(cmp$diff_rep), 5))
} else {
  message("No MEDSL VT 2016 HE rows found to cross-check against.")
}

## NOTE: deliberately NOT folded into elect_cty_final.rds and house_results_coverage.csv NOT
## re-run here -- fold-in is done centrally after all remaining states are built.
