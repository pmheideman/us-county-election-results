## Georgia: OpenElections years OTHER than 2012 (2012 already built manually in
## 01d_house_county_open_states.R from a user-downloaded browser file, sos.ga.gov being
## Cloudflare-blocked for scripted access -- OpenElections itself is a separate,
## independently-hosted GitHub archive and is NOT blocked, checked fresh here).
##
## Repo (github.com/openelections/openelections-data-ga) spans 2000-2018 for our purposes.
## Every even year 2000-2010 has a single already county-level `<date>__ga__general.csv` file
## (county,office,district,party,candidate,votes), consistently labeled office "U.S. House"
## (distinct from "State House" -- checked explicitly). 2014 uses a differently-shaped
## `..._county-level.csv` file with four separate vote-method columns (election_day/advanced/
## absentee_by_mail/provisional) instead of one `votes` column -- summed across all four.
##
## No pseudo-total row found in any year for U.S. House specifically (checked candidate names
## directly per year). 2006 has one quirk: a `county == "not available"` pseudo-row carrying a
## handful of unattributed Write-In tallies (64+48+1+3 votes across 4 districts) -- naturally
## dropped by the crosswalk join (no county_fips match), not worth a special exclusion.
##
## County-name matching: all 159 real Georgia counties in every year's source match the
## project's crosswalk (`countypres_2000-2024.tab`) exactly after toupper() -- confirmed via a
## direct diff before writing this script, no alias table needed.
##
## Cross-check: no MEDSL overlap year is built here (this script stops at 2014, MEDSL already
## covers 2016+) -- same situation as Georgia 2012 and Colorado. Verified via internal
## consistency instead (share in [0,1], totalvote > 0, low-two-party-share county-years
## checked directly).
##
## NOTE: per coordination with parallel state-building work, this script does NOT fold its
## output into elect_cty_final.rds or re-run house_coverage_tracker.R -- it only builds and
## saves elect_he_cty_ga.rds. Fold-in happens centrally afterward to avoid concurrent
## read-modify-write collisions with other states being built at the same time.

source(file.path("R", "00_setup.R"))
library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "georgia")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

county_fips_crosswalk <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))

ga_fips <- county_fips_crosswalk %>% filter(state == "GEORGIA") %>% select(county_name, county_fips)
stopifnot(nrow(ga_fips) == 159)

download_oe <- function(year, remote_name, local_name) {
  dest <- file.path(RAW_DIR, local_name)
  if (!file.exists(dest) || file.size(dest) == 0) {
    url <- paste0("https://raw.githubusercontent.com/openelections/openelections-data-ga/master/",
                   year, "/", remote_name)
    system2("curl", c("-sL", "-A", "'Mozilla/5.0'", "-o", shQuote(dest), shQuote(url)))
  }
  dest
}

to_party <- function(x) {
  x <- toupper(trimws(x))
  case_when(
    startsWith(x, "DEM") ~ "DEM",
    startsWith(x, "REP") ~ "REP",
    TRUE ~ "OTHER"
  )
}

## ---- 2000/2002/2004/2006/2008/2010: already county-level, one `votes` column ----
GA_COUNTY_FILES <- tribble(
  ~year, ~remote_dir, ~remote_name,
  2000,  "2000", "20001107__ga__general.csv",
  2002,  "2002", "20021105__ga__general.csv",
  2004,  "2004", "20041102__ga__general.csv",
  2006,  "2006", "20061107__ga__general.csv",
  2008,  "2008", "20081104__ga__general.csv",
  2010,  "2010", "20101102__ga__general.csv"
)

read_ga_county_year <- function(year, remote_dir, remote_name) {
  path <- download_oe(remote_dir, remote_name, paste0(year, "_general.csv"))
  df <- read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    filter(trimws(office) == "U.S. House") %>%
    mutate(county = toupper(trimws(county)), party = to_party(party), votes = as.numeric(votes)) %>%
    filter(!is.na(votes))
  n_real <- n_distinct(df$county[df$county %in% ga_fips$county_name])
  stopifnot(n_real == 159)
  df %>% mutate(year = year) %>% select(year, county, party, votes)
}

ga_county_by_year <- pmap_dfr(GA_COUNTY_FILES, read_ga_county_year)

## ---- 2014: county-level file, votes split across 4 vote-method columns ----
ga_2014_path <- download_oe("2014", "20141104__ga__general__county-level.csv", "2014_general_county.csv")
ga_2014_raw <- read_csv(ga_2014_path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
  filter(trimws(office) == "U.S. House") %>%
  mutate(
    county = toupper(trimws(county)),
    party = to_party(party),
    votes = as.numeric(election_day_votes) + as.numeric(advanced_votes) +
      as.numeric(absentee_by_mail_votes) + as.numeric(provisional_votes)
  ) %>%
  filter(!is.na(votes)) %>%
  mutate(year = 2014) %>%
  select(year, county, party, votes)
stopifnot(n_distinct(ga_2014_raw$county[ga_2014_raw$county %in% ga_fips$county_name]) == 159)

ga_by_county <- bind_rows(ga_county_by_year, ga_2014_raw)

elect_he_cty_ga <- ga_by_county %>%
  group_by(year, county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(ga_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(
    state = "GEORGIA", year, cty_fips = county_fips, sample = "HE",
    demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
  ) %>%
  save_step("elect_he_cty_ga")

message("GA House county-level rows built: ", nrow(elect_he_cty_ga), " (of possible ", 7 * 159, ")")
print(table(elect_he_cty_ga$year))

sanity <- elect_he_cty_ga$repuvote + elect_he_cty_ga$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")
stopifnot(all(sanity >= 0 & sanity <= 1.001))

low_share <- elect_he_cty_ga %>% filter(demovote + repuvote < 0.5) %>% arrange(year)
message(nrow(low_share), " county-years with demovote+repuvote < 0.5:")
print(low_share)

message("NOTE: not folded into elect_cty_final.rds here (see header) -- fold-in happens centrally.")
