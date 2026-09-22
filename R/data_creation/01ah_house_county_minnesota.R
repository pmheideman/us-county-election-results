## Minnesota: found via OpenElections (github.com/openelections/openelections-data-mn). Part of
## the post-large-states push through the remaining state list. Repo only starts at 2012 (checked
## the full directory listing) -- no pre-2012 general-election data exists at all, so the usable
## pre-MEDSL window is just 2012 and 2014 (2016+ already covered by MEDSL).
##
## Both years share one clean schema: a single already-county-level `..._general__county.csv`,
## office consistently labeled exactly "U.S. House" both years, party as clean short codes
## (R/DFL/IP/GP/WI -- Minnesota's Democratic-Farmer-Labor party is "DFL" not "DEM"). No pseudo-total
## row of any kind in either year (checked the full candidate-name list per year explicitly, same
## check as Illinois/Arizona/Georgia). 2014's file is comma-quoted (every field wrapped in ""),
## 2012's is not -- both handled fine by read_csv, no manual delimiter work needed.
##
## GitHub's listing API was rate-limited when checked (documented recurring issue) -- worked
## around by fetching the whole repo as one `codeload.github.com` tarball instead (same technique
## used for Massachusetts), rather than raw.githubusercontent.com per-file fetches.
##
## County-name note: the source spells the state's one two-word county "St. Louis" (with a
## period); stripping punctuation before uppercasing matches it to the crosswalk's "ST LOUIS" row
## cleanly (the crosswalk also has a redundant "SAINT LOUIS" alias row for the same FIPS, unused
## here). All other MN county names matched the crosswalk exactly with no aliasing needed.
##
## 2016 cross-check against MEDSL: see below -- used for validation only, not folded into output.

source(file.path("R", "00_setup.R"))
library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "minnesota")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

county_fips_crosswalk <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))

mn_fips <- county_fips_crosswalk %>%
  filter(state == "MINNESOTA", county_name != "SAINT LOUIS") %>%
  select(county_name, county_fips)
stopifnot(nrow(mn_fips) == 87)

download_oe <- function(remote_path, local_name) {
  dest <- file.path(RAW_DIR, local_name)
  if (!file.exists(dest) || file.size(dest) == 0) {
    url <- paste0("https://raw.githubusercontent.com/openelections/openelections-data-mn/master/", remote_path)
    system2("curl", c("-sL", "-A", "'Mozilla/5.0'", "-o", shQuote(dest), shQuote(url)))
  }
  dest
}

clean_county <- function(x) {
  x <- toupper(trimws(x))
  gsub("\\.", "", x)
}

to_party <- function(x) {
  x <- toupper(trimws(x))
  case_when(
    x == "DFL" ~ "DEM",
    x == "R" ~ "REP",
    TRUE ~ "OTHER"
  )
}

read_general_county <- function(year, remote_name) {
  path <- download_oe(paste0(year, "/", remote_name), paste0(year, "_general_county.csv"))
  raw <- read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c"))
  raw %>%
    filter(toupper(trimws(office)) == "U.S. HOUSE") %>%
    transmute(
      county = clean_county(county), candidate = trimws(candidate),
      party = to_party(party), votes = as.numeric(votes), year = year
    )
}

message("Fetching Minnesota House data, 2 years...")
all_rows <- bind_rows(
  read_general_county(2012, "20121106__mn__general__county.csv"),
  read_general_county(2014, "20141104__mn__general__county.csv")
) %>%
  filter(!is.na(votes))

message("Raw rows fetched: ", nrow(all_rows))
print(table(all_rows$year))

elect_he_cty_mn <- all_rows %>%
  group_by(year, county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(mn_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(
    state = "MINNESOTA", year, cty_fips = county_fips, sample = "HE",
    demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
  ) %>%
  save_step("elect_he_cty_mn")

message("MN House county-level rows built: ", nrow(elect_he_cty_mn), " (of possible ", 87 * 2, ")")
print(table(elect_he_cty_mn$year))

sanity <- elect_he_cty_mn$repuvote + elect_he_cty_mn$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")
stopifnot(all(sanity >= 0 & sanity <= 1.001))

low_share <- elect_he_cty_mn %>% filter(demovote + repuvote < 0.5) %>% arrange(year)
message(nrow(low_share), " county-years with demovote+repuvote < 0.5:")
print(low_share)

for (yr in sort(unique(elect_he_cty_mn$year))) {
  present <- elect_he_cty_mn %>% filter(year == yr) %>% pull(cty_fips)
  missing <- mn_fips %>% filter(!county_fips %in% present)
  if (nrow(missing) > 0) {
    message(yr, ": missing ", nrow(missing), " counties: ", paste(missing$county_name, collapse = ", "))
  }
}

## ---- 2016 MEDSL cross-check (validation only, not folded into elect_he_cty_mn) ----
medsl_check <- tryCatch({
  chk_path <- download_oe("2016/20161108__mn__general__county.csv", "2016_general_county.csv")
  chk_raw <- read_csv(chk_path, show_col_types = FALSE, col_types = cols(.default = "c"))
  chk <- chk_raw %>%
    filter(toupper(trimws(office)) == "U.S. HOUSE") %>%
    transmute(county = clean_county(county), candidate = trimws(candidate),
              party = to_party(party), votes = as.numeric(votes)) %>%
    filter(!is.na(votes)) %>%
    group_by(county) %>%
    summarise(
      totalvote = sum(votes, na.rm = TRUE),
      demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
      repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(mn_fips, by = c("county" = "county_name")) %>%
    filter(!is.na(county_fips), totalvote > 0) %>%
    transmute(cty_fips = county_fips,
              demovote_oe = demovote_n / totalvote, repuvote_oe = repuvote_n / totalvote)

  final <- readRDS(file.path("R", "output", "elect_cty_final.rds"))
  medsl_2016 <- final %>% filter(sample == "HE", year == 2016, substr(sprintf("%05d", as.integer(cty_fips)), 1, 2) == "27")

  cmp <- chk %>% inner_join(medsl_2016, by = "cty_fips")
  cmp$diff <- abs(cmp$repuvote_oe - cmp$repuvote) + abs(cmp$demovote_oe - cmp$demovote)
  message("2016 MEDSL cross-check: ", nrow(cmp), " counties matched, mean diff ", round(mean(cmp$diff), 5),
          ", max diff ", round(max(cmp$diff), 5))
  cmp
}, error = function(e) {
  message("2016 cross-check skipped: ", conditionMessage(e))
  NULL
})

## NOTE: deliberately NOT folded into elect_cty_final.rds and house_results_coverage.csv NOT
## re-run here -- multiple states are being built in parallel by concurrent agents, and the
## fold-in / tracker-refresh step is being done centrally afterward to avoid a lost-update race
## on those shared files.
