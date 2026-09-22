## South Carolina: found via OpenElections (github.com/openelections/openelections-data-sc). Part
## of the post-large-states push through the remaining state list. Repo spans 2006-2024 but only
## 2008, 2012, 2014 are usable pre-MEDSL general-election years.
##
## 2006 EXCLUDED: both the county-level and precinct-level general files for 2006 genuinely
## contain ONLY the 8 statewide constitutional offices (Governor, Attorney General, etc.) -- no
## President/Senate/House rows at all, confirmed by listing every distinct office value in both
## files. A real archive gap in the source, not a parsing miss.
##
## 2010 EXCLUDED: no general-election file of any kind exists in the repo's 2010 directory --
## only three special-election files for isolated State House seats. A real archive gap.
##
## 2008: single statewide precinct file, office labeled "U.S. House District N" (a different shape
## than 2012/2014 -- district embedded in the label, not a separate column) -- matched via
## grepl("U.S. House", office), safe because "U.S. House" never appears as a substring of "State
## House of Representatives District N" (SC's own state-house label), unlike the IA/NV
## bare-"HOUSE" substring trap.
##
## 2012 and 2014: no single statewide file -- 46 separate per-county precinct files under a
## `counties/` subdirectory (one download per county), office cleanly "U.S. House" with district in
## its own column. Filenames are a simple lowercase-no-space slug of the county name (verified
## against the crosswalk's all-single-word 46 SC county names -- no McCormick-style multi-word
## name to worry about here).
##
## No pseudo-total row found in any year (checked precinct-name lists explicitly). "Absentee",
## "Failsafe Provisional", "Provisional" are real vote-source precinct types present in every
## year -- same class as Florida's "Fed Abs"/Nebraska's "Countywide" rows, correctly summed in
## rather than excluded, confirmed by checking they don't duplicate any other precinct's value.
## Party codes are clean (REP/DEM/CON/GRN/NON), no write-in-suffix or fusion-voting complexity.

source(file.path("R", "00_setup.R"))
library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "south_carolina")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

county_fips_crosswalk <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))

sc_fips <- county_fips_crosswalk %>% filter(state == "SOUTH CAROLINA") %>% select(county_name, county_fips)
stopifnot(nrow(sc_fips) == 46)

download_oe <- function(remote_path, local_name) {
  dest <- file.path(RAW_DIR, local_name)
  if (!file.exists(dest) || file.size(dest) == 0) {
    url <- paste0("https://raw.githubusercontent.com/openelections/openelections-data-sc/master/", remote_path)
    system2("curl", c("-sL", "-A", "'Mozilla/5.0'", "-o", shQuote(dest), shQuote(url)))
  }
  dest
}

to_party <- function(x) {
  x <- toupper(trimws(x))
  case_when(
    x == "REP" ~ "REP",
    x == "DEM" ~ "DEM",
    TRUE ~ "OTHER"
  )
}

## ---- 2008: single statewide precinct file ----
read_2008 <- function() {
  path <- download_oe("2008/20081104__sc__general__precinct.csv", "2008_general_precinct.csv")
  read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    filter(grepl("U.S. House", office, fixed = TRUE)) %>%
    transmute(
      county = toupper(trimws(county)), votes = as.numeric(votes),
      party = to_party(party), year = 2008
    ) %>%
    filter(!is.na(votes))
}

## ---- 2012 / 2014: 46 per-county precinct files ----
SC_COUNTY_SLUGS <- tolower(sc_fips$county_name)

read_county_year <- function(year_chr, date_str) {
  rows <- lapply(SC_COUNTY_SLUGS, function(slug) {
    remote <- sprintf("%s/counties/%s__sc__general__%s__precinct.csv", year_chr, date_str, slug)
    local_name <- sprintf("%s_%s.csv", year_chr, slug)
    path <- download_oe(remote, local_name)
    df <- tryCatch(read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c")), error = function(e) NULL)
    if (is.null(df) || nrow(df) == 0) return(NULL)
    df %>%
      filter(office == "U.S. House") %>%
      transmute(
        county = toupper(trimws(county)), votes = as.numeric(votes),
        party = to_party(party), year = as.numeric(year_chr)
      ) %>%
      filter(!is.na(votes))
  })
  bind_rows(rows)
}

message("Fetching South Carolina House data: 2008 (statewide), 2012 + 2014 (46 per-county files each)...")
all_rows <- bind_rows(
  read_2008(),
  read_county_year("2012", "20121106"),
  read_county_year("2014", "20141104")
)

message("Raw rows fetched: ", nrow(all_rows))
print(table(all_rows$year))

elect_he_cty_sc <- all_rows %>%
  group_by(year, county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(sc_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(
    state = "SOUTH CAROLINA", year, cty_fips = county_fips, sample = "HE",
    demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
  ) %>%
  save_step("elect_he_cty_sc")

message("SC House county-level rows built: ", nrow(elect_he_cty_sc), " (of possible ", 46 * 3, ")")
print(table(elect_he_cty_sc$year))

sanity <- elect_he_cty_sc$repuvote + elect_he_cty_sc$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")
stopifnot(all(sanity >= 0 & sanity <= 1.001))

low_share <- elect_he_cty_sc %>% filter(demovote + repuvote < 0.5) %>% arrange(year)
message(nrow(low_share), " county-years with demovote+repuvote < 0.5:")
print(low_share)

for (yr in sort(unique(elect_he_cty_sc$year))) {
  present <- elect_he_cty_sc %>% filter(year == yr) %>% pull(cty_fips)
  missing <- sc_fips %>% filter(!county_fips %in% present)
  if (nrow(missing) > 0) {
    message(yr, ": missing ", nrow(missing), " counties: ", paste(missing$county_name, collapse = ", "))
  }
}

## ---- Non-production 2016 cross-check against MEDSL (validation only, not folded in) ----
sc_2016_check <- {
  # 2016 general file lives at top-level (not counties/), one statewide precinct file
  path <- download_oe("2016/20161108__sc__general__precinct.csv", "2016_general_precinct.csv")
  raw <- tryCatch(read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c")), error = function(e) NULL)
  if (is.null(raw)) {
    message("2016 cross-check file not found/unreadable, skipping validation.")
    NULL
  } else {
    raw %>%
      filter(office == "U.S. House") %>%
      mutate(county = toupper(trimws(county)), votes = as.numeric(votes), party = to_party(party)) %>%
      filter(!is.na(votes)) %>%
      group_by(county) %>%
      summarise(
        totalvote = sum(votes, na.rm = TRUE),
        demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
        repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      left_join(sc_fips, by = c("county" = "county_name")) %>%
      filter(!is.na(county_fips), totalvote > 0) %>%
      transmute(cty_fips = county_fips, year = 2016,
                demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote)
  }
}

if (!is.null(sc_2016_check)) {
  medsl_he <- readRDS(file.path(OUTPUT_DIR, "elect_he_cty_medsl.rds"))
  sc_check <- sc_2016_check %>%
    inner_join(medsl_he %>% filter(year == 2016), by = c("cty_fips", "year"), suffix = c("_sc", "_medsl")) %>%
    mutate(repuvote_diff = abs(repuvote_sc - repuvote_medsl))
  message("SC 2016 cross-check (validation only, not folded in): ", nrow(sc_check), " counties matched, max diff = ",
          round(max(sc_check$repuvote_diff, na.rm = TRUE), 4), ", mean diff = ",
          round(mean(sc_check$repuvote_diff, na.rm = TRUE), 5))
}
