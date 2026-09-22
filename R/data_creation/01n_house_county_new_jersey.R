## New Jersey: found via OpenElections (github.com/openelections/openelections-data-nj), same
## followup survey that produced Kansas (01j), Pennsylvania (01k), Ohio (01l), and Michigan (01m).
##
## NJ's repo only starts at 2010 (unlike most other states' repos, which reach back to 2000) --
## confirmed by listing every year directory 2010-2014 directly rather than assuming. Within that
## span:
## - 2010: the only "general" file present is a special State Senate election (district 5, 5
##   counties, "State Senate"/"General Assembly" offices only) -- NO regular November 2010
##   federal general-election file exists anywhere in the repo for this year. A genuine gap in
##   OpenElections' own NJ archive (same class of issue as Ohio's missing 2004), not a format
##   problem on our end. Not attempted further.
## - 2012, 2014: clean, already county-level files (`county,office,district,party,candidate,votes`,
##   office cleanly labeled "U.S. House", no pseudo-total row) -- 21/21 counties both years.
##
## Party field is unusually noisy even by this project's standards: NJ ballot lines let candidates
## register free-text party/slogan names ("None of Them", "Politicians are Crooks", "D-R Party" --
## checked this last one specifically since it looks like a fusion label, but it's a real minor
## candidate (Donald E. Letton in 2014) getting a handful of votes per county under a made-up
## party name, not an actual Democratic-Republican fusion ticket). Only "Republican" and
## "Democratic" are mapped to REP/DEM; every one of these other free-text lines correctly falls
## into neither, matching this project's standard treatment of minor-party/write-in votes.

source(file.path("R", "00_setup.R"))
library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "new_jersey")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

county_fips_crosswalk <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))

nj_fips <- county_fips_crosswalk %>% filter(state == "NEW JERSEY") %>% select(county_name, county_fips)

download_oe <- function(year, remote_name, local_name) {
  dest <- file.path(RAW_DIR, local_name)
  if (!file.exists(dest) || file.size(dest) == 0) {
    url <- paste0("https://raw.githubusercontent.com/openelections/openelections-data-nj/master/",
                   year, "/", remote_name)
    system2("curl", c("-sL", "-A", "'Mozilla/5.0'", "-o", shQuote(dest), shQuote(url)))
  }
  dest
}

PARTY_ALIAS <- c(Republican = "REP", Democratic = "DEM")
to_party <- function(x) coalesce(PARTY_ALIAS[trimws(x)], "OTHER")

NJ_FILES <- tribble(
  ~year, ~remote_name,
  2012,  "20121106__nj__general.csv",
  2014,  "20141104__nj__general__county.csv"
)

read_nj_year <- function(year, remote_name) {
  path <- download_oe(year, remote_name, paste0(year, "_raw.csv"))
  read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    filter(office == "U.S. House") %>%
    mutate(county = toupper(trimws(county)), party = to_party(party), votes = as.numeric(votes)) %>%
    filter(!is.na(votes)) %>%
    group_by(county, party) %>% summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
    mutate(year = year)
}

nj_by_county <- pmap_dfr(NJ_FILES, read_nj_year)

elect_he_cty_nj <- nj_by_county %>%
  group_by(year, county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(nj_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(
    state = "NEW JERSEY", year, cty_fips = county_fips, sample = "HE",
    demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
  ) %>%
  save_step("elect_he_cty_nj")

message("NJ House county-level rows built: ", nrow(elect_he_cty_nj), " (of possible ", 2 * 21, ")")
print(table(elect_he_cty_nj$year))

sanity <- elect_he_cty_nj$repuvote + elect_he_cty_nj$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")

## ---- Fold into the master panel ----
elect_cty_final <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")) %>%
  bind_rows(elect_he_cty_nj %>% select(-state)) %>%
  distinct(cty_fips, year, sample, .keep_all = TRUE) %>%
  arrange(cty_fips, year, sample) %>%
  save_step("elect_cty_final")

message("elect_cty_final.rds updated with NJ 2012/2014 (2010 excluded, see header notes). ",
        "Total rows now: ", nrow(elect_cty_final))
