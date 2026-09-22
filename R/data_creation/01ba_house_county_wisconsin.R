## Wisconsin: found via OpenElections (github.com/openelections/openelections-data-wi). One of the
## last 5 states in the project-wide sweep. Repo spans 2000-2022, ward-level files
## (`*__general__ward.csv`, summed by county -- same technique as NC/VA/KS-2014). WI calls its
## April local/spring elections "general" too, so several year directories have MULTIPLE
## `*__general__ward.csv` files (a spring one plus the real November federal one, sometimes also a
## recall/special) -- picked the November-dated file explicitly per year rather than a glob, to
## avoid grabbing the wrong election.
##
## Office is simply "House" (not "U.S. House") -- distinct from "State Assembly" (WI's own name
## for its state house chamber, not "State House", so no substring-grep trap here at all) and
## "Senate"/"State Senate". Exact match, no per-year alias table needed -- confirmed stable across
## all 8 years checked. Party is clean REP/DEM codes throughout; "Scattering" is WI's own name for
## a write-in-style catch-all bucket (blank party, real votes) -- kept as OTHER, not excluded. No
## pseudo-total ward row found in any year (checked explicitly).

source(file.path("R", "00_setup.R"))
library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "wisconsin")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

county_fips_crosswalk <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))

wi_fips <- county_fips_crosswalk %>% filter(state == "WISCONSIN") %>% select(county_name, county_fips)
stopifnot(nrow(wi_fips) == 72)

download_oe <- function(remote_path, local_name) {
  dest <- file.path(RAW_DIR, local_name)
  if (!file.exists(dest) || file.size(dest) == 0) {
    url <- paste0("https://raw.githubusercontent.com/openelections/openelections-data-wi/master/", remote_path)
    system2("curl", c("-sL", "-A", "'Mozilla/5.0'", "-o", shQuote(dest), shQuote(url)))
  }
  dest
}

to_party <- function(x) {
  x <- toupper(trimws(x))
  case_when(x %in% c("DEM", "DEMOCRAT", "DEMOCRATIC") ~ "DEM",
            x %in% c("REP", "REPUBLICAN") ~ "REP",
            TRUE ~ "OTHER")
}

normalize_county <- function(x) toupper(trimws(x))

## The November general-election ward file for each year, hand-picked from the repo listing
## (several years also have an April local-election file also labeled "general").
WI_GENERAL_FILES <- tribble(
  ~year, ~remote,
  2000, "2000/20001107__wi__general__ward.csv",
  2002, "2002/20021105__wi__general__ward.csv",
  2004, "2004/20041102__wi__general__ward.csv",
  2006, "2006/20061107__wi__general__ward.csv",
  2008, "2008/20081104__wi__general__ward.csv",
  2010, "2010/20101102__wi__general__ward.csv",
  2012, "2012/20121106__wi__general__ward.csv",
  2014, "2014/20141104__wi__general__ward.csv"
)

read_wi_year <- function(year, remote) {
  local <- paste0(year, "_general_ward.csv")
  path <- download_oe(remote, local)
  read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    filter(trimws(office) == "House") %>%
    transmute(county = normalize_county(county), party = to_party(party), votes = as.numeric(votes)) %>%
    filter(!is.na(votes)) %>%
    group_by(county, party) %>% summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
    mutate(year = year)
}

wi_by_county <- purrr::pmap_dfr(WI_GENERAL_FILES, function(year, remote) read_wi_year(year, remote))

elect_he_cty_wi <- wi_by_county %>%
  group_by(year, county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(wi_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(
    state = "WISCONSIN", year, cty_fips = county_fips, sample = "HE",
    demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
  ) %>%
  save_step("elect_he_cty_wi")

message("WI House county-level rows built: ", nrow(elect_he_cty_wi), " (of possible ", 72 * 8, ")")
print(table(elect_he_cty_wi$year))

sanity <- elect_he_cty_wi$repuvote + elect_he_cty_wi$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")

low_share <- elect_he_cty_wi %>% filter(demovote + repuvote < 0.5) %>% arrange(year)
message(nrow(low_share), " county-years with demovote+repuvote < 0.5:")
print(low_share)

for (yr in sort(unique(elect_he_cty_wi$year))) {
  present <- elect_he_cty_wi %>% filter(year == yr) %>% pull(cty_fips)
  missing <- wi_fips %>% filter(!county_fips %in% present)
  message(yr, ": ", length(present), "/72 counties", if (nrow(missing) > 0) paste0(" -- missing: ", paste(missing$county_name, collapse = ", ")) else "")
}

## ---- Non-production 2016 cross-check against MEDSL ----
wi_2016_raw <- {
  path <- download_oe("2016/20161108__wi__general__ward.csv", "2016_general_ward.csv")
  df <- tryCatch(read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c")), error = function(e) NULL)
  if (is.null(df)) NULL else {
    df %>% filter(trimws(office) == "House") %>%
      transmute(county = normalize_county(county), party = to_party(party), votes = as.numeric(votes))
  }
}

if (!is.null(wi_2016_raw)) {
  wi_2016 <- wi_2016_raw %>%
    filter(!is.na(votes)) %>%
    group_by(county) %>%
    summarise(
      totalvote = sum(votes, na.rm = TRUE),
      demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
      repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(wi_fips, by = c("county" = "county_name")) %>%
    filter(!is.na(county_fips), totalvote > 0) %>%
    transmute(cty_fips = county_fips, demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote)

  medsl_final <- readRDS(file.path(PROJECT_ROOT, "R", "output", "elect_cty_final.rds"))
  medsl_wi_2016 <- medsl_final %>% filter(sample == "HE", year == 2016, cty_fips %in% wi_fips$county_fips)

  if (nrow(medsl_wi_2016) > 0) {
    cmp <- wi_2016 %>%
      inner_join(medsl_wi_2016, by = "cty_fips", suffix = c("_oe", "_medsl")) %>%
      mutate(diff_dem = abs(demovote_oe - demovote_medsl), diff_rep = abs(repuvote_oe - repuvote_medsl))
    message("2016 cross-check vs MEDSL: ", nrow(cmp), " counties matched. ",
            "mean diff_dem=", round(mean(cmp$diff_dem), 5), " max diff_dem=", round(max(cmp$diff_dem), 5),
            " mean diff_rep=", round(mean(cmp$diff_rep), 5), " max diff_rep=", round(max(cmp$diff_rep), 5))
  }
} else {
  message("2016 WI file not usable for a bonus cross-check -- skipped, not part of production anyway.")
}

## NOTE: deliberately NOT folded into elect_cty_final.rds and house_results_coverage.csv NOT
## re-run here -- fold-in is done centrally after all remaining states are built.
