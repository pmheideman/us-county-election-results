## Maryland 2000-2014: found on elections.maryland.gov's own archive (elections.maryland.gov/
## elections/archive/<year>/election_data/index.html) -- genuinely raw, machine-readable per-county
## CSVs straight from the state board of elections, not a third-party aggregator. This is a much
## better source than OpenElections' Maryland repo, which was checked early in this project's
## state-by-state sweep and found essentially empty. Closes the entire Maryland pre-MEDSL gap
## (MEDSL already covers 2016+) together with `01bj` (the 1986-1998 + 2002 HTML-table era, where no
## CSV route exists).
##
## Each county has its own "General" results CSV for a given year (e.g.
## `Allegany_County_2012_General.csv`), one row per (candidate, office, district), with a
## `Total Votes` column already computed by the state -- no precinct-level summing needed, no OCR,
## no hand transcription. The office label for the House race changes across years -- "Representative
## in Congress" (2000, 2004), "U.S. Congress" (2006, 2008, 2010), "Rep in Congress" (2012, 2014) --
## matched via an explicit alias list, found by pulling the full unique office-name list from a
## sample county file per year rather than assuming the label was stable across the whole span.
##
## Filename convention changes across years (found by scraping each year's own index page rather
## than hardcoding a guessed pattern, since it changes more than once): `<County>_General_<year>.csv`
## (2000, 2004) vs `<County>_County_<year>_General.csv` (2006, 2008, 2010, 2012, 2014). 2002 has NO
## election_data/CSV folder at all (confirmed) -- handled in `01bj` via its HTML results page
## instead, alongside 1986-1998.

source(file.path("R", "00_setup.R"))
library(readr)
library(rvest)
library(stringr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "maryland")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

county_fips_crosswalk <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))
md_fips <- county_fips_crosswalk %>% filter(state == "MARYLAND") %>%
  mutate(county_name = str_replace(county_name, "^ST MARY'S$", "ST. MARY'S")) %>%
  distinct(county_name, county_fips)
stopifnot(nrow(md_fips) == 24)

fetch <- function(url, dest) {
  if (!file.exists(dest) || file.size(dest) == 0) {
    system2("curl", c("-sL", "-A", "'Mozilla/5.0'", "-o", shQuote(dest), shQuote(url)))
  }
  dest
}

## Normalizes a filename fragment like "Prince_George_s" or "St._Mary_s" or "Baltimore_City" to the
## crosswalk's "PRINCE GEORGE'S" / "ST. MARY'S" / "BALTIMORE CITY" form.
## Filenames drop apostrophes entirely and are inconsistent about periods ("Prince_Georges" with no
## apostrophe at all, "St._Marys" WITH a period before the space) -- strip both before matching, not
## just underscores, or "ST. MARYS" (period preserved) silently fails to match "ST MARYS" and both
## Prince George's and St. Mary's counties go missing for every single year (caught because the SAME
## two counties were missing in every year checked, the "one stable gap = name mismatch, not a real
## source gap" pattern already documented elsewhere in this project).
filename_to_county <- function(x) {
  x <- gsub("[_.]", " ", x)
  x <- toupper(trimws(x))
  x <- gsub("\\s+", " ", x)
  case_when(
    x %in% c("PRINCE GEORGE", "PRINCE GEORGES") ~ "PRINCE GEORGE'S",
    x %in% c("QUEEN ANNE", "QUEEN ANNES") ~ "QUEEN ANNE'S",
    x %in% c("ST MARY", "ST MARYS", "SAINT MARYS") ~ "ST. MARY'S",
    x == "BALTIMORE" ~ "BALTIMORE",
    TRUE ~ x
  )
}

## Scrapes one year's election_data index page for every county's General-election CSV link.
## Returns a tibble (county, url). Deliberately reads hrefs directly rather than constructing a
## guessed filename -- the naming convention changed across years (see header comment) and a
## constructed-URL approach silently 404s instead of erroring loudly.
list_general_csvs <- function(year) {
  index_url <- paste0("https://elections.maryland.gov/elections/archive/", year, "/election_data/index.html")
  index_path <- fetch(index_url, file.path(RAW_DIR, paste0(year, "_index.html")))
  page <- read_html(index_path)
  hrefs <- page %>% html_elements("a") %>% html_attr("href")
  hrefs <- hrefs[!is.na(hrefs)]
  ## A county General-election link: filename contains the year and "General", but NOT "Primary",
  ## "Questions", "Precinct", "Legislative", "Congressional_Districts", "Reference" (those are
  ## statewide/other breakdowns, not per-county files).
  general <- hrefs[str_detect(hrefs, paste0(year, ".*General\\.csv$|General_", year, "\\.csv$")) &
                      !str_detect(hrefs, "(?i)primary|question|precinct|legislative|congressional_district|reference|state_")]
  tibble(
    filename = general,
    county = filename_to_county(str_remove(general, paste0("_(County_)?", year, "_?General.*$|_General_", year, ".*$"))),
    url = paste0("https://elections.maryland.gov/elections/archive/", year, "/election_data/", general)
  ) %>% distinct(county, .keep_all = TRUE)
}

read_county_general <- function(year, county, url) {
  dest <- file.path(RAW_DIR, paste0(year, "_", gsub("[^A-Za-z]", "", county), "_general.csv"))
  path <- fetch(url, dest)
  raw <- tryCatch(
    read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c")),
    error = function(e) NULL
  )
  if (is.null(raw) || !"Office Name" %in% names(raw)) return(NULL)
  raw %>%
    filter(`Office Name` %in% c("Representative in Congress", "U.S. Congress", "Rep in Congress")) %>%
    transmute(
      county = county, year = year,
      party = toupper(trimws(Party)),
      votes = as.numeric(`Total Votes`)
    ) %>%
    filter(!is.na(votes))
}

years_csv <- c(2000, 2004, 2006, 2008, 2010, 2012, 2014)

message("Scraping Maryland election_data index pages for ", length(years_csv), " years...")
all_rows <- purrr::map_dfr(years_csv, function(yr) {
  links <- list_general_csvs(yr)
  message(yr, ": found ", nrow(links), " county CSV links")
  purrr::pmap_dfr(links, function(county, url, filename) read_county_general(yr, county, url))
})

message("Raw rows fetched: ", nrow(all_rows))
print(table(all_rows$year))

to_party <- function(x) {
  case_when(
    str_detect(x, "^DEM") ~ "DEM",
    str_detect(x, "^REP") ~ "REP",
    TRUE ~ "OTHER"
  )
}

elect_he_cty_md_csv <- all_rows %>%
  mutate(party_std = to_party(party)) %>%
  group_by(year, county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party_std == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party_std == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(md_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(
    state = "MARYLAND", year, cty_fips = county_fips, sample = "HE",
    demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
  ) %>%
  save_step("elect_he_cty_md_csv")

message("MD House county-level rows built (CSV era): ", nrow(elect_he_cty_md_csv),
        " (of possible ", 24 * length(years_csv), ")")
print(table(elect_he_cty_md_csv$year))

sanity <- elect_he_cty_md_csv$repuvote + elect_he_cty_md_csv$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")
stopifnot(all(sanity >= 0 & sanity <= 1.001))

for (yr in years_csv) {
  present <- elect_he_cty_md_csv %>% filter(year == yr) %>% pull(cty_fips)
  missing <- md_fips %>% filter(!county_fips %in% present)
  if (nrow(missing) > 0) {
    message(yr, ": missing ", nrow(missing), " counties: ", paste(missing$county_name, collapse = ", "))
  }
}

message("Done. NOTE: deliberately NOT folded into elect_cty_final.rds and house_results_coverage.csv")
message("NOT re-run here -- fold-in / tracker-refresh done separately after review.")
