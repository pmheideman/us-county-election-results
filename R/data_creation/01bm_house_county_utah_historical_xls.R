## Utah 2008, 2010: found on vote.utah.gov's OWN historical-election-results archive
## (vote.utah.gov/historical-election-results/), not OpenElections (whose UT repo starts 2012 and
## even then only has 2012/2014 partial -- see `01aw_house_county_utah.R`). Same precedent as
## Maryland/Oregon/New Jersey: a state's own official archive can have much better historical data
## than OpenElections. Utah's own archive is a genuine goldmine going back to 1960 -- see
## `01bn_house_county_utah_historical_pdf.R` for the older, scanned-PDF era (1990-2006); this script
## covers just 2008 and 2010, the two years the state itself published as clean, genuinely
## structured Excel workbooks (2012+ is also Excel but already covered adequately via OpenElections).
##
## Both years have a dedicated House sheet ("U S House" in 2008, "U.S. House" in 2010 -- the name
## drifts, matched via a short alias list) laid out identically: row 1 has each district's name in
## a merged cell (only the first column of that district's block is non-NA, forward-filled here),
## row 2 has "Candidate Name  \"PartyLetter\"" per column, county rows follow, then a "Total" row
## and a "Percent"/"PERCENT" row (both ignored -- we recompute totals from the county rows
## ourselves, not trusted blindly, same discipline as every other source in this project).
## Utah had exactly 3 congressional districts in both years (the 4th was added after 2010
## redistricting, matching OpenElections' 2012+ coverage) -- confirmed directly rather than assumed.

source(file.path("R", "00_setup.R"))
library(readxl)
library(stringr)
library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "utah_historical")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

county_fips_crosswalk <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))
ut_fips <- county_fips_crosswalk %>% filter(state == "UTAH") %>% distinct(county_name, county_fips)
stopifnot(nrow(ut_fips) == 29)

fetch <- function(url, dest) {
  if (!file.exists(dest) || file.size(dest) == 0) {
    system2("curl", c("-sL", "-A", "'Mozilla/5.0'", "-o", shQuote(dest), shQuote(url)))
  }
  dest
}

num <- function(x) as.numeric(gsub(",", "", as.character(x)))

## Row 1's district header is a merged cell in the source spreadsheet, so only the FIRST column of
## each district's block carries text (every other column reads NA) -- forward-fill so every
## column knows which district it belongs to, then this value is only used to split the sheet into
## per-district chunks (never joined to output; we aggregate to COUNTY regardless of district).
parse_house_sheet <- function(path, sheet) {
  raw <- read_excel(path, sheet = sheet, col_names = FALSE)
  district <- as.character(raw[1, ])
  ## fill NA forward
  for (i in seq_along(district)) if (is.na(district[i]) && i > 1) district[i] <- district[i - 1]

  header <- as.character(raw[2, ])
  party_std <- case_when(
    str_detect(header, '"R"') ~ "REP",
    str_detect(header, '"D"') ~ "DEM",
    TRUE ~ "OTHER"
  )

  total_row <- which(str_detect(toupper(as.character(raw[[1]])), "^TOTAL"))[1]
  stopifnot(!is.na(total_row))
  county_rows <- raw[3:(total_row - 1), ]

  ## Columns 1-4 are County/Registered/Cast/Percent, never candidates.
  cand_cols <- which(!is.na(district) & seq_along(district) > 4 & !is.na(header) & header != "NA")

  purrr::map_dfr(seq_len(nrow(county_rows)), function(i) {
    county <- toupper(trimws(as.character(county_rows[i, 1])))
    if (is.na(county) || county == "" || county == "NA") return(NULL)
    vals <- num(as.character(county_rows[i, cand_cols]))
    keep <- !is.na(vals)
    if (!any(keep)) return(NULL)
    tibble(county = county, party = party_std[cand_cols][keep], votes = vals[keep])
  })
}

## Utah's own county-name spelling matches the crosswalk directly except this one abbreviation.
normalize_county <- function(x) {
  case_when(
    x == "DAVIS" ~ "DAVIS",
    TRUE ~ x
  )
}

year_sheets <- tribble(
  ~year, ~url,                                                              ~sheet,
  2008,  "https://vote.utah.gov/wp-content/uploads/2023/09/2008Gen.xls",    "U S House",
  2010,  "https://vote.utah.gov/wp-content/uploads/2023/09/2010Gen.xls",    "U.S. House"
)

message("Fetching Utah 2008/2010 House results (Excel era)...")
all_rows <- purrr::pmap_dfr(year_sheets, function(year, url, sheet) {
  dest <- file.path(RAW_DIR, paste0(year, "gen.xls"))
  path <- fetch(url, dest)
  rows <- parse_house_sheet(path, sheet) %>% mutate(year = year)
  message(year, ": ", nrow(rows), " raw rows")
  rows
})

message("Raw rows fetched: ", nrow(all_rows))
print(table(all_rows$year))

elect_he_cty_ut_xls <- all_rows %>%
  mutate(county = normalize_county(county)) %>%
  group_by(year, county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(ut_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(
    state = "UTAH", year, cty_fips = county_fips, sample = "HE",
    demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
  ) %>%
  save_step("elect_he_cty_ut_xls")

message("UT House county-level rows built (Excel era): ", nrow(elect_he_cty_ut_xls), " (of possible 58)")
print(table(elect_he_cty_ut_xls$year))

sanity <- elect_he_cty_ut_xls$repuvote + elect_he_cty_ut_xls$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")
stopifnot(all(sanity >= 0 & sanity <= 1.001))

for (yr in year_sheets$year) {
  present <- elect_he_cty_ut_xls %>% filter(year == yr) %>% pull(cty_fips)
  missing <- ut_fips %>% filter(!county_fips %in% present)
  if (nrow(missing) > 0) {
    message(yr, ": missing ", nrow(missing), " counties: ", paste(missing$county_name, collapse = ", "))
  }
}

message("Done. NOTE: deliberately NOT folded into elect_cty_final.rds and house_results_coverage.csv")
message("NOT re-run here -- fold-in / tracker-refresh done separately after review.")
