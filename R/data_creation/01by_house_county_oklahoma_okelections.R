## Oklahoma county-level U.S. House results from the OK State Election Board results archive
## https://results.okelections.gov/OKER/?elecDate=YYYYMMDD  (Angular single-page app).
##
## How the site works (found by reading its JS bundle; used with the user's explicit OK on 2026-09-20):
##   * A browser-like User-Agent is required (plain "Mozilla/5.0" gets a 403).
##   * The app talks to https://results.okelections.gov/OKERS/enrapi/... and first logs in anonymously:
##     PUT enrapi/login/ with the public viewer's built-in login. That login key ships inside the app's own
##     JavaScript (main.*.bundle.js) and is what the public viewer sends for every visitor; this script reads
##     it from the bundle at run time and does NOT store it in the project.
##   * The login response is a JSON string whose first character must be dropped to get the bearer token.
##   * enrapi/getelec/<yyyymmdd>/SW/xx        -> races for that election (raceID, raceTitle, raceCandidates)
##     enrapi/GetCntyResults/<yyyymmdd>/<id>  -> county rows; candResults[i] lines up with raceCandidates[i]
##
## totalvote = sum of the named candidates' votes in the county (matches the other state builds; the site's
## own per-county "totResults" is not used). Split counties are summed across districts. A district with no
## race listed (uncontested seats are not on Oklahoma's ballot) has no data anywhere -- reported, not filled.
##
## Raw JSON saved under R/data/raw_house_county_open_states/oklahoma_okelections/<yyyymmdd>/.
## RESULT (2026-09-20): the archive only starts at 2012 (2010 and earlier return 'election type SW invalid'), so it
## cannot close any Oklahoma gap. It is used as an independent CHECK on the existing Oklahoma data: 2014 is identical
## to the OpenElections build, 2016/2024 identical to MEDSL. The counties still 'missing' are unopposed seats (OK-1 in
## 2014/2016, OK-3 in 2024 have no race on the ballot), not source gaps.
## Output is named verification_* (NOT elect_he_cty_*) on purpose so the coverage tracker / provenance builder
## do not treat it as a panel source. Does NOT touch elect_cty_final.rds.

source(file.path("R", "00_setup.R"))
library(jsonlite)
library(readr)

ELEC_DATES <- c("20121106", "20141104", "20161108", "20181106", "20201103", "20221108", "20241105")   # site archive starts 2012 (2010 and earlier: "election type SW invalid")

UA <- "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36"
SITE <- "https://results.okelections.gov/OKER/"
API  <- "https://results.okelections.gov/OKERS/"
RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "oklahoma_okelections")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

curl_get <- function(url, dest, headers = character(), method = NULL, body = NULL) {
  args <- c("-s", "-A", shQuote(UA), "--compressed", "-o", shQuote(dest), "-w", shQuote("%{http_code}"))
  if (!is.null(method)) args <- c(args, "-X", method)
  for (h in headers) args <- c(args, "-H", shQuote(h))
  if (!is.null(body)) args <- c(args, "-d", shQuote(body))
  suppressWarnings(system2("curl", c(args, shQuote(url)), stdout = TRUE))
}

## ---- anonymous login (key read from the app's own bundle) ---------------------------------------------
tmp <- tempfile(); idx <- curl_get(paste0(SITE, "?elecDate=", ELEC_DATES[1]), tmp)
stopifnot(idx == "200")
main_js <- regmatches(readLines(tmp, warn = FALSE), regexpr("main\\.[0-9a-f]+\\.bundle\\.js", readLines(tmp, warn = FALSE)))[1]
stopifnot(!is.na(main_js))
js_file <- tempfile(); stopifnot(curl_get(paste0(SITE, main_js), js_file) == "200")
js <- paste(readLines(js_file, warn = FALSE), collapse = "\n")
key <- regmatches(js, regexpr("connKey=\"[^\"]+\"", js)); stopifnot(length(key) == 1)
key <- sub("^connKey=\"", "", sub("\"$", "", key))
lf <- tempfile()
stopifnot(curl_get(paste0(API, "enrapi/login/"), lf, headers = "Content-Type: application/json", method = "PUT",
                   body = toJSON(list(Username = "appuser", Password = key), auto_unbox = TRUE)) == "200")
token <- substring(fromJSON(lf), 2)
auth <- c(paste0("Authorization: Bearer ", token), "Content-Type: application/json")
rm(key, js)

api_get <- function(path, dest) {
  if (!file.exists(dest) || file.size(dest) == 0) {
    code <- curl_get(paste0(API, path), dest, headers = auth)
    if (code != "200") { file.remove(dest); stop("HTTP ", code, " for ", path) }
    Sys.sleep(0.3)
  }
  fromJSON(dest, simplifyVector = FALSE)
}

## ---- crosswalk ---------------------------------------------------------------------------------------
norm <- function(x) gsub("[^A-Z]", "", toupper(x))
ok_fips <- read_delim(file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
                      delim = "\t", show_col_types = FALSE) %>%
  filter(state == "OKLAHOMA", !is.na(county_fips)) %>% distinct(county_name, county_fips) %>%
  mutate(key = norm(county_name)) %>% distinct(key, county_fips)
stopifnot(!anyDuplicated(ok_fips$key), nrow(ok_fips) == 77)

## ---- pull every U.S. House race for each date ------------------------------------------------------------
pull_date <- function(d) {
  ddir <- file.path(RAW_DIR, d); dir.create(ddir, showWarnings = FALSE)
  el <- tryCatch(api_get(paste0("enrapi/getelec/", d, "/SW/xx"), file.path(ddir, "getelec.json")),
                 error = function(e) { message(d, ": no election listing (", conditionMessage(e), ")"); NULL })
  if (is.null(el)) return(NULL)
  races <- Filter(function(r) identical(r$raceTag, "UNITEDSTATESREPRESENTATIVE"), el$races)
  message(d, ": ", length(races), " U.S. House race(s): ", paste(vapply(races, function(r) r$raceTitle, ""), collapse = " | "))
  purrr::map_dfr(races, function(r) {
    res <- api_get(paste0("enrapi/GetCntyResults/", d, "/", r$raceID), file.path(ddir, paste0("cnty_", r$raceID, ".json")))
    stopifnot(identical(res$status, "Normal"))
    cands <- vapply(r$raceCandidates, function(cc) cc$candName, "")
    dist <- as.integer(sub(".*DISTRICT +0*([0-9]+).*", "\\1", toupper(r$raceTitle)))
    counties <- purrr::map_dfr(res$summaryCandResults, function(cy) {
      v <- vapply(cy$candResults, function(z) as.numeric(z$totalVotes), 0)
      stopifnot(length(v) == length(cands))
      tibble(date = d, district = dist, race_id = r$raceID, county = cy$countyName, cand_raw = cands, votes = v,
             precincts = cy$totalPrecincts, reporting = cy$reportingPrecincts)
    })
    ## statewide printed total must equal the sum over counties
    st <- sum(vapply(res$summaryTotResults$candResults, function(z) as.numeric(z$totalVotes), 0))
    stopifnot(st == sum(counties$votes))
    counties
  })
}
long <- purrr::map_dfr(ELEC_DATES, pull_date)
stopifnot(nrow(long) > 0)
long <- long %>% mutate(year = as.integer(substr(date, 1, 4)),
                        party = sub(".*\\(([A-Z]+)\\)\\s*$", "\\1", cand_raw),
                        candidate = trimws(sub("\\s*\\([A-Z]+\\)\\s*$", "", cand_raw)),
                        key = norm(county)) %>% left_join(ok_fips, by = "key")
if (anyNA(long$county_fips)) { print(unique(long$county[is.na(long$county_fips)])); stop("unmatched counties") }
stopifnot(all(long$reporting == long$precincts))          # official, fully reported
message("party tags: ", paste(names(table(long$party)), table(long$party), collapse = ", "))

## ---- aggregate: county-year, split counties summed across districts ------------------------------------------
ok <- long %>% group_by(year, county_fips) %>%
  summarise(totalvote = sum(votes), demovote_n = sum(votes[party == "DEM"]), repuvote_n = sum(votes[party == "REP"]), .groups = "drop") %>%
  filter(totalvote > 0)
elect_he_cty_ok_okelections <- ok %>%
  transmute(state = "OKLAHOMA", year, cty_fips = county_fips, sample = "HE",
            demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote) %>%
  arrange(year, cty_fips) %>% save_step("verification_ok_okelections_he_cty")
save_step(long, "ok_okelections_house_long")

message("\n== districts with a race / counties covered per year ==")
print(long %>% group_by(year) %>% summarise(districts = paste(sort(unique(district)), collapse = ","), counties = n_distinct(county_fips)))
message("two-party sum range by year:")
print(elect_he_cty_ok_okelections %>% group_by(year) %>% summarise(min = round(min(demovote + repuvote), 3), max = round(max(demovote + repuvote), 3)))
message("\n== winners by district ==")
print(long %>% group_by(year, district, candidate, party) %>% summarise(v = sum(votes), .groups = "drop") %>%
        group_by(year, district) %>% arrange(desc(v), .by_group = TRUE) %>% mutate(share = round(v / sum(v), 3)) %>% slice_head(n = 2), n = 40)

## ---- Cross-check against the existing OpenElections-based build (elect_he_cty_ok.rds) ---------------------------
old <- readRDS(file.path(OUTPUT_DIR, "elect_he_cty_ok.rds")) %>% select(year, cty_fips, o_dem = demovote, o_rep = repuvote, o_tot = totalvote)
cmp <- inner_join(elect_he_cty_ok_okelections, old, by = c("year", "cty_fips")) %>%
  mutate(d_dem = abs(demovote - o_dem), d_rep = abs(repuvote - o_rep), d_tot = abs(totalvote - o_tot) / o_tot)
message("\n== new vs existing OpenElections build (same year/county) ==")
print(cmp %>% group_by(year) %>% summarise(n = n(), identical_tot = sum(d_tot < 1e-9), max_dem = round(max(d_dem), 4),
      max_rep = round(max(d_rep), 4), max_tot_rel = round(max(d_tot), 4)))
yrs <- as.integer(substr(ELEC_DATES, 1, 4))
message("counties only in the new build / only in the old build (requested years only):")
print(bind_rows(
  anti_join(elect_he_cty_ok_okelections, old, by = c("year", "cty_fips")) %>% count(year) %>% mutate(what = "new only"),
  anti_join(old %>% filter(year %in% yrs), elect_he_cty_ok_okelections, by = c("year", "cty_fips")) %>% count(year) %>% mutate(what = "old only")))
