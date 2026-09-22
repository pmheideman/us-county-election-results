## Illinois: found via OpenElections (github.com/openelections/openelections-data-il), same
## followup survey that produced Kansas (01j), Pennsylvania (01k), Ohio (01l), Michigan (01m),
## New Jersey (01n), and New York (01o).
##
## IL's repo only starts at 2008 (unlike most other states' repos, which reach back to 2000) --
## confirmed by listing every year directory directly. Within the pre-MEDSL span, 2008, 2010,
## 2012, and 2014 all have a clean, already county-level file
## (`county,office,district,party,candidate,votes`), office consistently labeled exactly
## "U.S. House" every year, party already given as clean 3-letter codes (DEM/REP plus minor-party
## codes like GRN/IND) rather than full words or single letters -- no alias table needed beyond
## the trivial DEM/REP passthrough. No pseudo-total row of any kind found in any year (checked
## explicitly for both a literal "TOTAL"-like county name and any candidate name resembling a
## pseudo-total, following the lesson from New York where two DIFFERENT pseudo-total conventions
## turned up across different years of the same state). Cook County (Chicago) is present and
## handled identically to every other county in all 4 years -- no special-casing needed. This is
## the cleanest state yet in this whole OpenElections pass: 408/408 (102 counties x 4 years), zero
## bugs hit, no exclusions.

source(file.path("R", "00_setup.R"))
library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "illinois")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

county_fips_crosswalk <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))

il_fips <- county_fips_crosswalk %>% filter(state == "ILLINOIS") %>% select(county_name, county_fips)

download_oe <- function(year, remote_name, local_name) {
  dest <- file.path(RAW_DIR, local_name)
  if (!file.exists(dest) || file.size(dest) == 0) {
    url <- paste0("https://raw.githubusercontent.com/openelections/openelections-data-il/master/",
                   year, "/", remote_name)
    system2("curl", c("-sL", "-A", "'Mozilla/5.0'", "-o", shQuote(dest), shQuote(url)))
  }
  dest
}

PARTY_ALIAS <- c(DEM = "DEM", REP = "REP")
to_party <- function(x) coalesce(PARTY_ALIAS[trimws(x)], "OTHER")

IL_FILES <- tribble(
  ~year, ~remote_name,
  2008,  "20081104__il__general__county.csv",
  2010,  "20101102__il__general__county.csv",
  2012,  "20121106__il__general__county.csv",
  2014,  "20141104__il__general__county.csv"
)

read_il_year <- function(year, remote_name) {
  path <- download_oe(year, remote_name, paste0(year, "_county.csv"))
  read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    filter(office == "U.S. House") %>%
    mutate(county = toupper(trimws(county)), party = to_party(party), votes = as.numeric(votes)) %>%
    filter(!is.na(votes)) %>%
    group_by(county, party) %>% summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
    mutate(year = year)
}

il_by_county <- pmap_dfr(IL_FILES, read_il_year)

elect_he_cty_il <- il_by_county %>%
  group_by(year, county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(il_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(
    state = "ILLINOIS", year, cty_fips = county_fips, sample = "HE",
    demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
  ) %>%
  save_step("elect_he_cty_il")

message("IL House county-level rows built: ", nrow(elect_he_cty_il), " (of possible ", 4 * 102, ")")
print(table(elect_he_cty_il$year))

sanity <- elect_he_cty_il$repuvote + elect_he_cty_il$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")

## ---- Fold into the master panel ----
elect_cty_final <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")) %>%
  bind_rows(elect_he_cty_il %>% select(-state)) %>%
  distinct(cty_fips, year, sample, .keep_all = TRUE) %>%
  arrange(cty_fips, year, sample) %>%
  save_step("elect_cty_final")

message("elect_cty_final.rds updated with IL 2008-2014. Total rows now: ", nrow(elect_cty_final))
