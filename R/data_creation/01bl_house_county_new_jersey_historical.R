## New Jersey 2000-2010: found on nj.gov's OWN election-results archive (nj.gov/state/elections/
## election-information-<year>.shtml), not OpenElections (whose NJ repo only starts 2010, and 2010
## itself was an archive gap there -- see `01n_house_county_new_jersey.R`). Same precedent as
## Maryland and Oregon: a state's own official archive can have much better historical data than
## OpenElections, worth checking directly for any state with a thin OpenElections repo.
##
## nj.gov's PDF archive of "Candidate Returns for House of Representatives" goes back to at least
## 1980, but the text layer is only usable starting in 2000 -- checked 1994/1996/1998 directly and
## all three are scanned/image-only (pdftotext extracts a bare ~13 characters from a 13-15 page
## file); 2000 onward is genuinely born-digital (tens of thousands of characters, clean).  2000-2010
## (6 general-election years) covers exactly the gap right before OpenElections' NJ coverage
## picks up at 2012 -- 1994-1998 would need the same manual page-image-transcription treatment as
## Kentucky 2010 / California 1990-1996 if pursued further, not attempted in this pass.
##
## PDF filename convention drifts every year or two (no stable pattern) -- found via scraping each
## year's own results page rather than guessing (same lesson as Maryland's CSV era): "...house-
## candidate-tallies.pdf" (2000), "...gen-elect-us-house-candidate_tally.pdf" (2002), "...official_
## ..._gen_elect_congressional_results.pdf" (2004), "..._official-house-of-reps_tallies.pdf" (2006),
## "...official-gen-elect-tallies-house-....pdf" (2008, 2010).
##
## Format (all 6 years, identical structure): one page per congressional district, headed
## "Nth Congressional District: County1 (part) - County2 - County3 Counties" (the "(part)" suffix
## marks a split county -- irrelevant here since counties are summed regardless of which district(s)
## touch them, same as every other split-county state in this project). Below that, one block per
## candidate: a first line with the candidate's name, party DESIGNATION, first county, a "slogan"
## column (often identical to the party for major-party candidates, genuinely different text for
## minor candidates -- e.g. "E Pluribus Unum"), and that county's vote count; subsequent lines
## (candidate's mailing address, irrelevant) each carry one more county + vote count; a blank line;
## then "Total <district-wide total for this candidate>". This "Total" line is used here purely to
## VALIDATE the block's own county-row sum (same discipline as every other source in this project),
## not as the primary number.

source(file.path("R", "00_setup.R"))
library(rvest)
library(stringr)
library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "new_jersey")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

county_fips_crosswalk <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))
nj_fips <- county_fips_crosswalk %>% filter(state == "NEW JERSEY") %>% select(county_name, county_fips)
stopifnot(nrow(nj_fips) == 21)
## Longest names first so "CAPE MAY" isn't shadowed by a shorter false match.
nj_county_names <- nj_fips$county_name[order(-nchar(nj_fips$county_name))]

fetch <- function(url, dest) {
  if (!file.exists(dest) || file.size(dest) == 0) {
    system2("curl", c("-sL", "-A", "'Mozilla/5.0'", "-o", shQuote(dest), shQuote(url)))
  }
  dest
}

num <- function(x) as.numeric(gsub(",", "", x))

## Finds this year's real "general election House of Representatives" PDF link by scraping the
## year's own results page -- the filename convention is NOT stable across years (see header
## comment), so this is scraped fresh each year rather than guessed/hardcoded.
find_house_pdf <- function(year) {
  index_url <- paste0("https://www.nj.gov/state/elections/election-information-", year, ".shtml")
  index_path <- fetch(index_url, file.path(RAW_DIR, paste0(year, "_index.html")))
  page <- read_html(index_path)
  hrefs <- page %>% html_elements("a") %>% html_attr("href")
  hrefs <- hrefs[!is.na(hrefs) & str_detect(hrefs, "\\.pdf$")]
  candidates <- hrefs[str_detect(tolower(hrefs), "house|congress") &
                        !str_detect(tolower(hrefs), "pri-elect|primary|senate|assem|delegate")]
  stopifnot(length(candidates) >= 1)
  ## Prefer the one filename that looks like the FULL statewide compiled results (contains "gen" or
  ## "official") over a stray per-district file, if both exist.
  preferred <- candidates[str_detect(tolower(candidates), "gen|official")]
  chosen <- if (length(preferred) >= 1) preferred[1] else candidates[1]
  if (!str_detect(chosen, "^https?://")) {
    chosen <- paste0("https://www.nj.gov/state/elections/", chosen)
  }
  chosen
}

## Parses one full year's PDF text into a long (district, county, party, votes) tibble, validating
## each candidate's own printed "Total" against the sum of that candidate's county rows.
parse_year_text <- function(lines, year) {
  district_idx <- grep("(?i)^\\s*\\w+\\s+Congressional District:", lines, perl = TRUE)
  stopifnot(length(district_idx) > 0)
  ends <- c(district_idx[-1] - 1, length(lines))

  purrr::map_dfr(seq_along(district_idx), function(i) {
    block <- lines[district_idx[i]:ends[i]]
    total_idx <- grep("(?i)^\\s*Total\\s+[0-9,]+\\s*$", block)
    if (length(total_idx) == 0) return(NULL)

    start <- 2  # skip the district header line itself
    purrr::map_dfr(total_idx, function(ti) {
      candidate_lines <- block[start:(ti - 1)]
      start <<- ti + 1
      candidate_lines <- candidate_lines[trimws(candidate_lines) != ""]
      if (length(candidate_lines) == 0) return(NULL)

      ## Party comes from the DESIGNATION column, present once, on the candidate's own name line --
      ## but that's NOT reliably `candidate_lines[1]`: the very first candidate in each district
      ## has 3 lines of column-header junk ("Party /", the "Name/Address ... Tally" header line,
      ## "House of Representatives") ahead of it that survive the blank-line filter, silently
      ## pushing every district's FIRST candidate into "OTHER" (caught via an implausibly low
      ## repuvote+demovote sanity range, as low as 0.099 -- the district's leadoff candidate, often
      ## the incumbent with the plurality of the vote, was being dropped into neither party's
      ## bucket every single time). Fixed by searching ALL of the candidate's lines for the
      ## Democratic/Republican keyword instead of assuming it's on line 1 specifically.
      party <- case_when(
        any(str_detect(candidate_lines, "(?i)\\bDemocratic\\b")) ~ "DEM",
        any(str_detect(candidate_lines, "(?i)\\bRepublican\\b")) ~ "REP",
        TRUE ~ "OTHER"
      )

      ## A handful of candidates (5 out of ~1200 across all 6 years) trip the total-mismatch check
      ## below -- traced to candidates whose PARTY DESIGNATION wraps across two lines ("Socialist
      ## Party" / "USA") while their mailing address ALSO spans a different number of lines than
      ## their county-row count, desynchronizing which printed "Total" line closes out which
      ## candidate's block. Always minor (never a Democratic/Republican) candidate in every case
      ## checked, so it perturbs only the OTHER bucket / totalvote denominator slightly for that one
      ## county-year, not the DEM/REP shares themselves -- accepted rather than chased further,
      ## consistent with this project's general practice for small verified-low-impact residuals.
      printed_total <- num(str_match(block[ti], "([0-9,]+)\\s*$")[, 2])

      county_rows <- purrr::map_dfr(candidate_lines, function(line) {
        hit <- nj_county_names[str_detect(toupper(line), paste0("\\b", nj_county_names, "\\b"))]
        if (length(hit) == 0) return(NULL)
        county <- hit[which.max(nchar(hit))]
        vote_match <- str_match(line, "([0-9,]+)\\s*$")[, 2]
        if (is.na(vote_match)) return(NULL)
        tibble(county = county, votes = num(vote_match))
      })
      if (nrow(county_rows) == 0) return(NULL)

      computed_total <- sum(county_rows$votes)
      if (!is.na(printed_total) && abs(computed_total - printed_total) > pmax(1, 0.01 * printed_total)) {
        warning("Year ", year, ": candidate total mismatch, computed ", computed_total,
                " vs printed ", printed_total)
      }

      county_rows %>% mutate(party = party, year = year)
    })
  })
}

years <- c(2000, 2002, 2004, 2006, 2008, 2010)

message("Fetching New Jersey 2000-2010 House results...")
all_rows <- purrr::map_dfr(years, function(yr) {
  url <- find_house_pdf(yr)
  dest <- file.path(RAW_DIR, paste0(yr, "_house.pdf"))
  path <- fetch(url, dest)
  lines <- system2("pdftotext", c("-layout", shQuote(path), "-"), stdout = TRUE)
  rows <- withCallingHandlers(
    parse_year_text(lines, yr),
    warning = function(w) { message(conditionMessage(w)); invokeRestart("muffleWarning") }
  )
  message(yr, ": ", nrow(rows), " raw rows from ", basename(url))
  rows
})

message("Raw rows fetched: ", nrow(all_rows))
print(table(all_rows$year))

elect_he_cty_nj_historical <- all_rows %>%
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
  save_step("elect_he_cty_nj_historical")

message("NJ historical House county-level rows built: ", nrow(elect_he_cty_nj_historical),
        " (of possible ", 21 * length(years), ")")
print(table(elect_he_cty_nj_historical$year))

sanity <- elect_he_cty_nj_historical$repuvote + elect_he_cty_nj_historical$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")
stopifnot(all(sanity >= 0 & sanity <= 1.001))

for (yr in years) {
  present <- elect_he_cty_nj_historical %>% filter(year == yr) %>% pull(cty_fips)
  missing <- nj_fips %>% filter(!county_fips %in% present)
  if (nrow(missing) > 0) {
    message(yr, ": missing ", nrow(missing), " counties: ", paste(missing$county_name, collapse = ", "))
  }
}

message("Done. NOTE: deliberately NOT folded into elect_cty_final.rds and house_results_coverage.csv")
message("NOT re-run here -- fold-in / tracker-refresh done separately after review.")
