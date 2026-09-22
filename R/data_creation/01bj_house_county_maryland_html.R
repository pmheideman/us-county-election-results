## Maryland 1986-1998: the pre-CSV era of elections.maryland.gov's own archive. Each general
## election year has a "Representative in Congress" results page under
## `elections.maryland.gov/elections/archive/<year>/results_<year>/` -- `garep.html` for
## gubernatorial-cycle (midterm) years, `pregarep.html` for presidential years -- with one clean
## HTML <table> per congressional district (Maryland has had 8 House seats throughout this span):
## a header row with each candidate's name and party in "Name (Party)" format, county rows with
## vote counts aligned to those columns, and a "Total"/"Percent" row pair at the bottom.
##
## Together with `01bi` (the 2000-2014 CSV era) and 2002 (handled below, in this same script), this
## closes Maryland's ENTIRE pre-MEDSL House gap back to 1986 -- found on the state's own official
## archive, not OpenElections (whose Maryland repo was checked early in this project's state sweep
## and found essentially empty).
##
## **2002 initially looked unreachable** via the combined `results/g_representative_in_congress.html`
## page used for a first pass (it splits some districts' tables across TWO separate <table> elements
## -- a main table plus a second table for a write-in-only column, joined only by row position, not
## any shared header -- structurally different from every other year here). **Fixed by using a
## different, per-district page instead**, found directly by the user: `results/g_cd01.html` ..
## `g_cd08.html`, one page per congressional district, each with its own 1-2 tables in the same
## split-on-write-ins shape but now trivially handled since `parse_district_table()` already
## produces a long (county, party, votes) format that combines correctly across multiple tables for
## the same district without any special merge logic. See `parse_2002()` below.

source(file.path("R", "00_setup.R"))
library(rvest)
library(stringr)
library(readr)

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

num <- function(x) as.numeric(gsub(",", "", x))

## Source spells some county names with a backtick instead of an apostrophe ("Queen Anne`s",
## "St. Mary`s") -- normalize both to the crosswalk's apostrophe form.
normalize_county <- function(x) {
  x <- toupper(trimws(x))
  x <- gsub("`", "'", x)
  x
}

## Parses one district's <table> into a long (county, party, votes) tibble. For 1986-1998's combined
## per-year page, row 1 of the raw grid is an office/district header (ignored) and row 2 has
## candidate names with party in "Name (Party)" format. 2002's per-district pages (`g_cd01.html`
## etc.) have no such combined page -- each district is already its own page, so row 1 IS the party
## header directly (just "Democratic"/"Republican" text, no parens, no candidate name) and county
## data starts at row 2, not row 3 -- set `header_row_idx = 1` for this shape. Populated/blank
## columns still work the same way in both shapes (blank/NA columns are unused write-in slots for
## that district); the party regex matches "Democratic"/"Republican" with or without surrounding
## parens so both page shapes are recognized by the same check.
parse_district_table <- function(tbl, header_row_idx = 2) {
  grid <- tbl %>% html_table(fill = TRUE, header = FALSE)
  if (nrow(grid) < header_row_idx + 1) return(NULL)

  header_row <- as.character(grid[header_row_idx, ])
  party_std <- case_when(
    str_detect(header_row, "(?i)Democratic") ~ "DEM",
    str_detect(header_row, "(?i)Republican") ~ "REP",
    TRUE ~ "OTHER"
  )
  ## Column 1 is the county-name column, never a candidate -- exclude from party assignment.
  party_std[1] <- NA_character_

  total_idx <- which(str_detect(as.character(grid[[1]]), "(?i)^total"))[1]
  if (is.na(total_idx)) return(NULL)
  county_rows <- grid[(header_row_idx + 1):(total_idx - 1), ]
  if (nrow(county_rows) == 0) return(NULL)

  purrr::map_dfr(seq_len(nrow(county_rows)), function(i) {
    row <- county_rows[i, ]
    county <- normalize_county(as.character(row[[1]]))
    if (county == "" || is.na(county)) return(NULL)
    vals <- as.character(row)[-1]
    parties <- party_std[-1]
    keep <- !is.na(vals) & vals != "" & !is.na(parties)
    if (!any(keep)) return(NULL)
    tibble(county = county, party = parties[keep], votes = num(vals[keep]))
  })
}

parse_year <- function(year, page_type) {
  fname <- if (page_type == "pres") "pregarep.html" else "garep.html"
  url <- paste0("https://elections.maryland.gov/elections/archive/", year, "/results_", year, "/", fname)
  path <- fetch(url, file.path(RAW_DIR, paste0(year, "_", fname)))
  page <- read_html(path)
  tbls <- page %>% html_elements("table") %>%
    Filter(function(t) {
      first_cell <- tryCatch((t %>% html_table(fill = TRUE, header = FALSE))[[1]][1], error = function(e) "")
      str_detect(as.character(first_cell), "(?i)Representative in Congress")
    }, .)
  message(year, ": ", length(tbls), " district tables found")
  purrr::map_dfr(tbls, parse_district_table) %>% mutate(year = year)
}

## 2002's results are NOT on one combined page like every other year here -- each of Maryland's 8
## districts has its own page (`results/g_cd01.html` .. `g_cd08.html`), found by the user directly.
## Structurally simpler in one way (no filtering needed to find the right table) but with a real
## complication: 3 of the 8 pages (whichever districts had a write-in candidate that year) split
## their results across TWO <table> elements -- a main Dem/Rep/etc. table plus a second, headerless
## continuation table for the write-in column(s), tied together only by matching county row order,
## not any shared attribute. Confirmed by checking all 8 pages' table counts before writing this:
## cd04, cd05, cd08 have 2 tables each, the other 5 have exactly 1. Handled for free by just running
## `parse_district_table()` (with `header_row_idx = 1`, no office/district text row on these
## per-district pages) over EVERY table found on a page and letting the long-format
## (county, party, votes) output from both tables naturally combine when grouped by county -- no
## special-case merge logic needed, since a second table's votes just add more rows for the same
## counties.
parse_2002 <- function() {
  purrr::map_dfr(sprintf("%02d", 1:8), function(d) {
    url <- paste0("https://elections.maryland.gov/elections/archive/2002/results/g_cd", d, ".html")
    path <- fetch(url, file.path(RAW_DIR, paste0("2002_cd", d, ".html")))
    page <- read_html(path)
    tbls <- page %>% html_elements("table")
    message("2002 CD", d, ": ", length(tbls), " table(s)")
    purrr::map_dfr(tbls, parse_district_table, header_row_idx = 1)
  }) %>% mutate(year = 2002)
}

## Gubernatorial-cycle (midterm) years use garep.html; presidential years use pregarep.html.
year_types <- tribble(
  ~year, ~page_type,
  1986,  "gov",
  1988,  "pres",
  1990,  "gov",
  1992,  "pres",
  1994,  "gov",
  1996,  "pres",
  1998,  "gov"
)

message("Fetching Maryland 1986-1998 House results (HTML-table era)...")
all_rows <- bind_rows(
  purrr::pmap_dfr(year_types, function(year, page_type) parse_year(year, page_type)),
  parse_2002()
)

message("Raw rows fetched: ", nrow(all_rows))
print(table(all_rows$year))

elect_he_cty_md_html <- all_rows %>%
  group_by(year, county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(md_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(
    state = "MARYLAND", year, cty_fips = county_fips, sample = "HE",
    demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
  ) %>%
  save_step("elect_he_cty_md_html")

message("MD House county-level rows built (HTML era): ", nrow(elect_he_cty_md_html),
        " (of possible ", 24 * (nrow(year_types) + 1), ")")
print(table(elect_he_cty_md_html$year))

sanity <- elect_he_cty_md_html$repuvote + elect_he_cty_md_html$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")
stopifnot(all(sanity >= 0 & sanity <= 1.001))

for (yr in c(year_types$year, 2002)) {
  present <- elect_he_cty_md_html %>% filter(year == yr) %>% pull(cty_fips)
  missing <- md_fips %>% filter(!county_fips %in% present)
  if (nrow(missing) > 0) {
    message(yr, ": missing ", nrow(missing), " counties: ", paste(missing$county_name, collapse = ", "))
  }
}

message("Done. NOTE: deliberately NOT folded into elect_cty_final.rds and house_results_coverage.csv")
message("NOT re-run here -- fold-in / tracker-refresh done separately after review.")
