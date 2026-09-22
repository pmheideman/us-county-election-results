## Louisiana SOS "Graphical Election Results" app -- bulk download of every U.S. House race's
## parish-level CSV, 1990-2024, ALL rounds (primaries, general, runoffs). Download only; parsing and
## the decisive-round rule live in 01bx_house_county_louisiana_sos.R.
##
## Source: https://voterportal.sos.la.gov/graphical (Angular app). It reads plain files from
##   https://voterportal.sos.la.gov/ElectionResults/ElectionResults/Data?blob=<path>
## where <path> is one of:
##   ElectionDates.htm                              -> JSON list of every election (1982-present)
##   <yyyymmdd>/ElectionRaces.htm                   -> JSON list of races (RaceID, OfficeTitleAndDesc)
##   <yyyymmdd>/csv/ByParish_<RaceID>.csv           -> one row per parish, candidate columns
## (found by reading the app's JS bundle; no login or bot protection). Note the sibling page
## sos.la.gov/elections-voting/statewide-post-election-statistics is turnout/registration only.
##
## Which elections: every election dated Aug-Dec of an EVEN year 1990-2024 (covers the open/closed
## primaries, November general and December runoffs), restricted to races whose title starts with
## "U. S. Representative". Some elections have no House race; that is recorded, not an error.
##
## Output: R/data/raw_house_county_open_states/louisiana_sos/<yyyymmdd>/ByParish_<RaceID>.csv
##         R/data/raw_house_county_open_states/louisiana_sos/_manifest.csv (one row per race file)
## Idempotent: existing non-empty files are skipped.

source(file.path("R", "00_setup.R"))
library(jsonlite)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "louisiana_sos")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)
BASE <- "https://voterportal.sos.la.gov/ElectionResults/ElectionResults/Data?blob="

fetch <- function(blob, dest, tries = 4) {
  if (file.exists(dest) && file.size(dest) > 0) return(TRUE)
  for (i in seq_len(tries)) {
    code <- suppressWarnings(system2("curl", c("-sL", "-A", shQuote("Mozilla/5.0"), "-o", shQuote(dest),
                                               "-w", shQuote("%{http_code}"), shQuote(paste0(BASE, blob))),
                                     stdout = TRUE))
    if (identical(code, "200") && file.size(dest) > 0) { Sys.sleep(0.25); return(TRUE) }
    Sys.sleep(2 * i)
  }
  if (file.exists(dest)) file.remove(dest)
  FALSE
}

dates_file <- file.path(RAW_DIR, "ElectionDates.htm")
stopifnot(fetch("ElectionDates.htm", dates_file))
dl <- fromJSON(dates_file)$Dates$Date
elections <- dl %>%
  transmute(election_id = PKElectionID, date = as.Date(ElectionDate, "%m/%d/%Y"),
            official = ResultsOfficial) %>%
  mutate(yr = as.integer(format(date, "%Y")), mo = as.integer(format(date, "%m"))) %>%
  filter(yr >= 1990, yr <= 2024, yr %% 2 == 0, mo >= 8) %>%
  arrange(date)
message(nrow(elections), " candidate elections (even-year Aug-Dec, 1990-2024)")

manifest <- list()
for (i in seq_len(nrow(elections))) {
  d <- format(elections$date[i], "%Y%m%d")
  dir.create(file.path(RAW_DIR, d), showWarnings = FALSE)
  rf <- file.path(RAW_DIR, d, "ElectionRaces.htm")
  if (!fetch(paste0(d, "/ElectionRaces.htm"), rf)) { message(d, ": race list FAILED"); next }
  races <- tryCatch(fromJSON(rf)$Races$Race, error = function(e) NULL)
  if (is.null(races) || nrow(as.data.frame(races)) == 0) { message(d, ": no races"); next }
  house <- as.data.frame(races) %>% filter(grepl("^U\\. ?S\\. ?Representative", OfficeTitleAndDesc))
  message(d, ": ", nrow(house), " U.S. House race(s)")
  for (j in seq_len(nrow(house))) {
    f <- file.path(RAW_DIR, d, paste0("ByParish_", house$RaceID[j], ".csv"))
    ok <- fetch(paste0(d, "/csv/ByParish_", house$RaceID[j], ".csv"), f)
    manifest[[length(manifest) + 1]] <- tibble(date = d, race_id = house$RaceID[j],
      office = house$OfficeTitleAndDesc[j], downloaded = ok,
      bytes = if (ok) file.size(f) else NA_real_)
  }
}
manifest <- bind_rows(manifest)
write_csv <- function(x, p) utils::write.csv(x, p, row.names = FALSE)
write_csv(manifest, file.path(RAW_DIR, "_manifest.csv"))

message("\n== manifest: ", nrow(manifest), " race files; failed downloads: ", sum(!manifest$downloaded))
print(manifest %>% mutate(yr = substr(date, 1, 4)) %>% count(yr, date, name = "n_races"), n = 100)
