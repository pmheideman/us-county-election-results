## Kentucky: elect.ky.gov, NOT bot-protected (plain curl works), genuine county-level U.S. House
## results going back decades. The user found the first file by hand
## (.../1990-1999/1998/98Gen_usrep.txt) and asked whether the naming is consistent enough to
## script -- it is NOT: filenames vary by year in ways that can't be guessed (90usrep.txt,
## 92usrep.txt, 94gen_USRep.txt, 96Gen_usrep1.txt, 98Gen_usrep.txt -- no two years match the same
## pattern). The reliable way to find each year's real file is via that year's own results-index
## page (a different URL scheme per decade -- see YEAR_INDEX_URLS below, itself hand-collected
## from elect.ky.gov's main results nav since even THAT isn't fully regular).
##
## WORSE: the *year 2000+* "by county" files (found via the same index-page hunt) use a
## COMPLETELY DIFFERENT layout -- transposed (counties as column headers, candidates as rows,
## wrapped 6 counties per block) and, worse, don't show party letters at all in the block itself
## (just candidate full names) -- would need an external candidate->party mapping to use, which
## this script does not attempt. **This script covers 1990-1998 only** (the "usrep"-style
## district-block files, format verified consistent across those 5 years despite each year's
## header line being phrased slightly differently -- see parse logic below). 2000-2016 is a
## separate, harder follow-up -- see project memory for what was found and why it stalled.

source(file.path("R", "00_setup.R"))
library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "kentucky")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

## Hand-verified via each year's own results-index page (elect.ky.gov/results/1990-1999/Pages/
## <year>.aspx) -- filenames do NOT follow a guessable pattern, confirmed by testing several
## plausible variants that 404'd before finding the real links.
KY_USREP_FILES <- tribble(
  ~year, ~url,
  1990, "1990-1999/1990/90usrep.txt",
  1992, "1990-1999/1992/92usrep.txt",
  1994, "1990-1999/1994/94gen_USRep.txt",
  1996, "1990-1999/1996/96Gen_usrep1.txt",
  1998, "1990-1999/1998/98Gen_usrep.txt"
)

fetch_ky_file <- function(rel_url) {
  dest <- file.path(RAW_DIR, gsub("/", "_", rel_url))
  if (!file.exists(dest) || file.size(dest) == 0) {
    url <- paste0("https://elect.ky.gov/SiteCollectionDocuments/Election%20Results/",
                  utils::URLencode(rel_url))
    system2("curl", c("-sL", "-A", "'Mozilla/5.0'", "-o", shQuote(dest), shQuote(url)))
  }
  dest
}

## District header line phrasing differs by year ("U.S. REPRESENTATIVE (1ST DISTRICT)" in 1990,
## "1ST   DISTRICT UNITED STATES REPRESENTATIVE" in 1996, "1ST District United States
## Representative" in 1998) -- matched generically (an ordinal word + "REPRESENTATIVE" on the
## same line) rather than one fixed string per year.
ORDINAL_RE <- "\\b(1ST|2ND|3RD|4TH|5TH|6TH|7TH|8TH|9TH)\\b"

parse_ky_usrep_file <- function(path, year) {
  lines <- readLines(path, warn = FALSE, encoding = "latin1")
  hdr_idx <- which(grepl(ORDINAL_RE, lines, ignore.case = TRUE) &
                      grepl("REPRESENTATIVE", lines, ignore.case = TRUE))
  if (length(hdr_idx) == 0) return(tibble())

  ## Party header row: a line that's just ALL-CAPS word(s), nothing else -- can be ONE token
  ## (an uncontested district shows just "REPUBLICAN" or "DEMOCRAT", spelled out in full, no
  ## opponent at all -- confirmed in 1990 districts 5 and 6, which a stricter "2+ abbreviated
  ## tokens" version of this regex silently skipped entirely, losing 45 counties).
  ## 1992 (at least) repeats the office title as its own all-caps line right after the district
  ## header ("UNITED STATES REPRESENTATIVE") -- matches the bare party-row shape too, so it has
  ## to be excluded explicitly or it gets mistaken for the real party row one line early.
  party_row_re <- "^[[:space:]]*([A-Z]{2,12}[[:space:]]*)+$"
  is_real_party_row <- function(line) {
    grepl(party_row_re, line) && !grepl("REPRESENTATIVE|DISTRICT|UNITED STATES|GENERAL ELECTION", line)
  }

  all_rows <- list()
  for (h in hdr_idx) {
    j <- h + 1
    while (j <= length(lines) && !is_real_party_row(lines[j])) {
      j <- j + 1
      if (j > min(h + 6, length(lines))) break  # give up if it's not within a few lines
    }
    if (j > length(lines) || !is_real_party_row(lines[j])) next
    ## Normalize full party names to the same 3-letter codes used elsewhere (REPUBLICAN->REP,
    ## DEMOCRAT->DEM) so downstream matching on "starts with REP/DEM" works uniformly either way.
    parties <- strsplit(trimws(lines[j]), "\\s+")[[1]]

    ## Data rows start after the next dashed-line separator, end at the next dashed separator
    ## (which precedes "TOTAL VOTES").
    dash_idx <- which(grepl("^-{20,}", lines[(j + 1):min(j + 20, length(lines))]))
    if (length(dash_idx) == 0) next
    data_start <- j + dash_idx[1]
    dash2 <- which(grepl("^-{20,}", lines[(data_start + 1):length(lines)]))
    data_end <- if (length(dash2) > 0) data_start + dash2[1] - 1 else length(lines)

    data_lines <- lines[(data_start + 1):data_end]
    data_lines <- data_lines[trimws(data_lines) != ""]
    for (dl in data_lines) {
      toks <- strsplit(trimws(gsub("\t", " ", dl)), "\\s+")[[1]]
      if (length(toks) != length(parties) + 1) next  # county name + one number per party
      county <- toks[1]
      votes <- suppressWarnings(as.numeric(gsub(",", "", toks[-1])))
      if (any(is.na(votes))) next
      all_rows[[length(all_rows) + 1]] <- tibble(county = county, party = parties, votes = votes)
    }
  }
  if (length(all_rows) == 0) return(tibble())
  bind_rows(all_rows) %>% mutate(year = year)
}

ky_raw <- pmap_dfr(KY_USREP_FILES, function(year, url) {
  path <- fetch_ky_file(url)
  parse_ky_usrep_file(path, year)
})

message("KY raw rows parsed: ", nrow(ky_raw))

ky_fips <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% filter(state == "KENTUCKY") %>% distinct(county_name, county_fips)

elect_he_cty_ky <- ky_raw %>%
  group_by(year, county, party) %>% summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
  group_by(year, county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[startsWith(party, "DEM")], na.rm = TRUE),
    repuvote_n = sum(votes[startsWith(party, "REP")], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(ky_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(
    state = "KENTUCKY", year, cty_fips = county_fips, sample = "HE",
    demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
  ) %>%
  save_step("elect_he_cty_ky")

message("KY House county-level rows built: ", nrow(elect_he_cty_ky), " across years: ",
        paste(sort(unique(elect_he_cty_ky$year)), collapse = ", "))
print(table(elect_he_cty_ky$year))

## No MEDSL overlap year available (this script stops at 1998, MEDSL starts 2016) -- sanity
## check via range/shape instead of a direct cross-check.
sanity <- elect_he_cty_ky$repuvote + elect_he_cty_ky$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")

## ---- Fold into the master panel ----
elect_cty_final <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")) %>%
  bind_rows(elect_he_cty_ky %>% select(-state)) %>%
  distinct(cty_fips, year, sample, .keep_all = TRUE) %>%
  arrange(cty_fips, year, sample) %>%
  save_step("elect_cty_final")

message("elect_cty_final.rds updated with KY (1990-1998). Total rows now: ", nrow(elect_cty_final))
