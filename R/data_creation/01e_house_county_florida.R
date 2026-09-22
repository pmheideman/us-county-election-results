## Florida: results.elections.myflorida.com -- a legacy ASP/frames site (archive goes back to
## 1978), NOT bot-protected (confirmed: plain curl with a browser-like User-Agent gets real
## content, no Cloudflare/WAF challenge).
##
## IMPORTANT, found the hard way: the obvious-looking endpoint --
##   SummaryRpt.asp?ElectionDate=<date>&COUNTY=<3-letter code>&PARTY=&DATAMODE=
## -- does NOT actually filter U.S. Representative vote totals to that county. Confirmed by
## fetching two different counties (Alachua, Bradford) known to share the same district in the
## same year: both returned the IDENTICAL vote totals (193,843/136,338/12,519 for FL-3 in 2016)
## -- that's the district's full total, not either county's share of it. This silently produced
## a first-pass Florida dataset that looked complete (826 rows) but was wrong for any county that
## doesn't happen to BE its entire district alone -- caught via the same MEDSL 2016 cross-check
## that validated NC/VA/GA (max diff 0.40 instead of 0). That version was discarded.
##
## The CORRECT endpoint is organized by district, not by county:
##   DetailRpt.asp?ELECTIONDATE=<date>&RACE=USR&PARTY=&DIST=<3-digit district>&GRP=&DATAMODE=
## which returns a genuine county-by-county breakdown for that one district (confirmed: Alachua
## and Bradford show different, plausible vote counts here). This is also more efficient --
## one request per district (~19-27 per year depending on FL's district count that decade)
## instead of one per county (67) -- and per-county totals from summing across whatever
## districts a county appears in are the true, correct numbers (no district-to-county crosswalk
## needed, same principle as every other state handled this way).
##
## Caveat: this district-level table's own "Fed Abs" (federal absentee) row isn't broken out by
## county, so a county's true total may be missing a handful of absentee votes (order of ~80
## votes out of ~150,000 in the 1992 FL-3 example -- negligible for our purposes, not worth
## chasing down).

source(file.path("R", "00_setup.R"))
library(readr)
library(rvest)
library(xml2)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "florida_by_district")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

FL_ELECTION_DATES <- tribble(
  ~year, ~date_str,
  1992, "11/3/1992", 1994, "11/8/1994", 1996, "11/5/1996", 1998, "11/3/1998",
  2000, "11/7/2000", 2002, "11/5/2002", 2004, "11/2/2004", 2006, "11/7/2006",
  2008, "11/4/2008", 2010, "11/2/2010", 2012, "11/6/2012", 2014, "11/4/2014",
  2016, "11/8/2016"  # overlaps MEDSL -- cross-check
)

## FL's had 19 (1992-2000), 25 (2002-2010), or 27 (2012+) congressional districts -- rather than
## hardcode counts per decade (one more thing to get subtly wrong), just try 1-30 every year and
## skip whichever don't exist (DetailRpt.asp returns a page with no "United States
## Representative" text for an out-of-range district -- confirmed for DIST=030 in 1992).
MAX_DIST_TO_TRY <- 30

fetch_fl_district_page <- function(date_str, dist_num) {
  dist_padded <- sprintf("%03d", dist_num)
  fname <- paste0(gsub("/", "-", date_str), "_D", dist_padded, ".html")
  dest <- file.path(RAW_DIR, fname)
  if (!file.exists(dest) || file.size(dest) == 0) {
    url <- paste0("https://results.elections.myflorida.com/DetailRpt.Asp?ELECTIONDATE=",
                  utils::URLencode(date_str), "&RACE=USR&PARTY=&DIST=", dist_padded,
                  "&GRP=&DATAMODE=")
    system2("curl", c("-sL", "-A", "'Mozilla/5.0'", "-o", shQuote(dest), shQuote(url)))
    Sys.sleep(0.15)
  }
  dest
}

## Parses one district's DetailRpt page: a "County" header row (blank corner cell + one
## "Candidate<br>(PARTY)" cell per candidate), then one row per county with that county's votes,
## until a "Fed Abs"/"Total"/"% Votes" row ends the table.
parse_fl_district_detail <- function(path) {
  html <- tryCatch(read_html(path, encoding = "latin1"), error = function(e) NULL)
  if (is.null(html)) return(tibble())
  tbls <- xml_find_all(html, "//table")
  if (length(tbls) == 0) return(tibble())
  trs <- xml_find_all(tbls[[1]], ".//tr")
  if (length(trs) < 3) return(tibble())

  header_cells <- xml_find_all(trs[[2]], ".//td")
  cand_raw <- xml_text(header_cells, trim = TRUE)[-1]
  if (length(cand_raw) == 0) return(tibble())
  party <- toupper(gsub(".*\\(([A-Za-z]+)\\)\\s*$", "\\1", cand_raw))
  party[!grepl("\\([A-Za-z]+\\)\\s*$", cand_raw)] <- ""

  rows_out <- list()
  for (i in seq(3, length(trs))) {
    cells <- xml_find_all(trs[[i]], ".//td")
    if (length(cells) == 0) next
    label <- gsub(" ", "", xml_text(cells[[1]], trim = TRUE))
    if (label %in% c("", "Fed Abs", "Total", "% Votes") || grepl("^%", label)) next  # not a county row
    votes_raw <- xml_text(cells, trim = TRUE)[-1]
    if (length(votes_raw) != length(party)) next
    votes <- as.numeric(gsub(",", "", votes_raw))
    rows_out[[length(rows_out) + 1]] <- tibble(county = toupper(label), party = party, votes = votes)
  }
  if (length(rows_out) == 0) return(tibble())
  bind_rows(rows_out)
}

read_fl_district_year <- function(year, date_str, dist_num) {
  path <- fetch_fl_district_page(date_str, dist_num)
  parsed <- parse_fl_district_detail(path)
  if (nrow(parsed) == 0) return(tibble())
  parsed %>% mutate(year = year)
}

fl_by_district <- pmap(
  expand_grid(FL_ELECTION_DATES, dist_num = seq_len(MAX_DIST_TO_TRY)),
  function(year, date_str, dist_num) read_fl_district_year(year, date_str, dist_num)
) %>% bind_rows()

message("Raw district-level rows parsed: ", nrow(fl_by_district))

fl_fips <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% filter(state == "FLORIDA") %>% distinct(county_name, county_fips)

elect_he_cty_fl <- fl_by_district %>%
  group_by(year, county, party) %>%
  summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
  group_by(year, county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(fl_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(
    state = "FLORIDA", year, cty_fips = county_fips, sample = "HE",
    demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
  ) %>%
  save_step("elect_he_cty_fl")

message("FL House county-level rows built: ", nrow(elect_he_cty_fl), " across years: ",
        paste(sort(unique(elect_he_cty_fl$year)), collapse = ", "))

## ---- Cross-check against MEDSL for 2016 ----
medsl_he <- readRDS(file.path(OUTPUT_DIR, "elect_he_cty_medsl.rds"))
fl_check <- elect_he_cty_fl %>%
  filter(year == 2016) %>%
  inner_join(medsl_he %>% filter(year == 2016), by = c("cty_fips", "year"), suffix = c("_fl", "_medsl")) %>%
  mutate(repuvote_diff = abs(repuvote_fl - repuvote_medsl))

message("FL vs MEDSL 2016 cross-check: ", nrow(fl_check), " counties matched, max repuvote diff = ",
        round(max(fl_check$repuvote_diff, na.rm = TRUE), 4), ", mean diff = ",
        round(mean(fl_check$repuvote_diff, na.rm = TRUE), 5))

## ---- Fold pre-2016 years into the master panel ----
elect_cty_final <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")) %>%
  bind_rows(elect_he_cty_fl %>% filter(year < 2016) %>% select(-state)) %>%
  distinct(cty_fips, year, sample, .keep_all = TRUE) %>%
  arrange(cty_fips, year, sample) %>%
  save_step("elect_cty_final")

message("elect_cty_final.rds updated with FL. Total rows now: ", nrow(elect_cty_final))
