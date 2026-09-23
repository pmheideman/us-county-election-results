## Download Colorado U.S. House county-level results, 1990-2000 general elections, from the Colorado
## Secretary of State's Historical Election Data site (https://historicalelectiondata.coloradosos.gov/),
## the SAME Elstats/Civera platform as Virginia/Vermont (confirmed: `x-elstats-has-v3-css` attribute in
## the page HTML) -- just tenant "co". OpenElections already covers 2002-2014 (01w, full 64/64 counties);
## 2016+ is MEDSL. This closes the 1990-2000 gap (6 years -- CO had 6 congressional districts through
## 2000, gaining a 7th only after the 2000 census, matching exactly where OpenElections' own coverage
## starts).
##
## Two calls, both public and used by the site itself (same shape as Virginia/Vermont, see
## 01cl_virginia_download.R / 01du_vermont_download.R):
##   1. POST /api/graphql_pr (header X-Elstats-Tenant: co) query SearchContests. UNLIKE Virginia/
##      Vermont, Colorado's `offices` filter does NOT actually restrict server-side (confirmed: passing
##      offices=[{id:10}] still returns Supreme Court/Court of Appeals/County Court/District Court
##      contests alongside the real U.S. House ones) -- so office is filtered CLIENT-SIDE on
##      office$name == "United States Congressperson" instead. Office id 10 found by requesting one
##      year with an empty offices filter and listing the distinct (officeId, office$name) pairs
##      returned.
##   2. GET /api/download_contest/<contest id>_table.csv?split_party=false -> rows "U.S. Congressperson
##      District" (district total) and "County" (county-level results -- exactly what's needed, no
##      precinct summing required), same layout as Virginia's "Congressional District"/"Locality" rows.
## Everything cached under R/data/raw_house_county_open_states/colorado_elections/ (re-runs skip existing files).
library(jsonlite)
DIR <- file.path("R", "data", "raw_house_county_open_states", "colorado_elections"); dir.create(DIR, showWarnings = FALSE, recursive = TRUE)
API <- "https://historicalelectiondata.coloradosos.gov/api/"
QUERY <- "query SearchContests($pagination: Pagination!, $filters: SearchFilters!) { search(pagination: $pagination, filters: $filters) { meta { currentPage totalPages totalResults } results { id officeId isSpecial isRunoff eventTypeDisplayName event { id startDate isSpecial } office { id name } division { id displayName } candidates { id displayName nVotes isWinner isWriteIn } } } }"
gql <- function(vars) { f <- tempfile(fileext = ".json"); writeLines(toJSON(list(query = QUERY, variables = vars), auto_unbox = TRUE, null = "null"), f)
  out <- system2("curl", c("-s", "-m", "90", "-A", shQuote("Mozilla/5.0 (research; county election data project)"), "-H", shQuote("Content-Type: application/json"), "-H", shQuote("X-Elstats-Tenant: co"),
                           "-X", "POST", "-d", paste0("@", f), shQuote(paste0(API, "graphql_pr"))), stdout = TRUE); fromJSON(paste(out, collapse = ""), simplifyVector = FALSE) }
idx_f <- file.path(DIR, "contests_index.csv")
if (!file.exists(idx_f)) {
  rows <- list()
  for (y in seq(1990, 2000, 2)) {
    f <- list(global = list(years = list(from = y, to = y)), ballotQuestions = list(text = "", types = list(), number = "", divisions = list()),
              contests = list(candidates = list(), divisions = list(), offices = list(list(id = 10))), specialElectionsOnly = FALSE, voterStats = FALSE, stages = list())
    page <- 1; repeat {
      r <- gql(list(pagination = list(page = page, size = 100), filters = f)); stopifnot(is.null(r$errors)); s <- r$data$search
      for (c in s$results) if (!is.null(c$event) && !is.null(c$office) && c$office$name == "United States Congressperson")
        rows[[length(rows) + 1]] <- data.frame(year = y, contest_id = c$id, office = c$office$name, date = substr(c$event$startDate, 1, 10), event_id = c$event$id, event_type = c$eventTypeDisplayName %||% "",
          special = isTRUE(c$isSpecial) || isTRUE(c$event$isSpecial), runoff = isTRUE(c$isRunoff), division = c$division$displayName %||% NA_character_, n_candidates = length(c$candidates))
      if (page >= s$meta$totalPages) break; page <- page + 1; Sys.sleep(0.2)
    }
    Sys.sleep(0.3)
  }
  idx <- do.call(rbind, rows); write.csv(idx, idx_f, row.names = FALSE)
}
idx <- read.csv(idx_f, stringsAsFactors = FALSE)

message("U.S. House contests found (all types): ", nrow(idx)); print(as.data.frame(table(idx$year, idx$event_type)))
## keep only: the November General Election, not special/runoff (division is "U.S. Congressperson District N")
keep_row <- !idx$special & !idx$runoff & idx$event_type == "General" & grepl("^[0-9]{4}-11-", idx$date)
dl <- idx[keep_row, ]
message("U.S. House November general contests: ", nrow(dl)); print(as.data.frame(table(dl$year)))
stopifnot(nrow(dl) == 36, !anyDuplicated(dl[, c("year", "division")]))          # 6 districts x 6 years, 1990-2000
idx$keep <- idx$contest_id %in% dl$contest_id; write.csv(idx, idx_f, row.names = FALSE)
for (i in seq_len(nrow(dl))) { f <- file.path(DIR, sprintf("contest_%d.csv", dl$contest_id[i]))
  if (!file.exists(f) || file.size(f) < 50) { Sys.sleep(0.4); system2("curl", c("-sL", "-m", "90", "-A", shQuote("Mozilla/5.0 (research; county election data project)"), "-H", shQuote("X-Elstats-Tenant: co"), "-o", shQuote(f),
                                                                    shQuote(sprintf("%sdownload_contest/%d_table.csv?split_party=false", API, dl$contest_id[i])))) } }
extra <- setdiff(list.files(DIR, "^contest_.*\\.csv$"), sprintf("contest_%d.csv", dl$contest_id)); unlink(file.path(DIR, extra))
message("csv files: ", length(list.files(DIR, "^contest_.*\\.csv$")), " of ", nrow(dl))
