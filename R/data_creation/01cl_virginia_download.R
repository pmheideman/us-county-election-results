## Download Virginia U.S. House county (locality) results, 1990-2024 general elections, from the Virginia Department of Elections' Historical Elections Database
## (https://historical.elections.virginia.gov/, an ElectionStats/Civera site; tenant header "va").
## Two calls, both public and used by the site itself:
##   1. POST /api/graphql_pr  (header X-Elstats-Tenant: va) query SearchContests, filters: years, offices [{id: 5}] (U.S. House); pages are 1-based. -> contest ids, dates, districts.
##   2. GET /api/download_contest/<contest id>_table.csv?split_party=false  -> the "Results CSV" of the contest page: rows "Congressional District" (district total),
##      "Locality" (counties and independent cities) and "Precinct".
## Everything is cached under R/data/raw_house_county_open_states/virginia_elections/ (re-runs skip existing files).
library(jsonlite)
DIR <- file.path("R", "data", "raw_house_county_open_states", "virginia_elections"); dir.create(DIR, showWarnings = FALSE, recursive = TRUE)
API <- "https://historical.elections.virginia.gov/api/"
QUERY <- "query SearchContests($pagination: Pagination!, $filters: SearchFilters!) { search(pagination: $pagination, filters: $filters) { meta { currentPage totalPages totalResults } results { id officeId isSpecial isRunoff eventTypeDisplayName event { id startDate isSpecial } office { id name } division { id displayName } candidates { id displayName nVotes isWinner isWriteIn } } } }"
gql <- function(vars) { f <- tempfile(fileext = ".json"); writeLines(toJSON(list(query = QUERY, variables = vars), auto_unbox = TRUE, null = "null"), f)
  out <- system2("curl", c("-s", "-m", "90", "-A", shQuote("Mozilla/5.0 (research; county election data project)"), "-H", shQuote("Content-Type: application/json"), "-H", shQuote("X-Elstats-Tenant: va"),
                           "-X", "POST", "-d", paste0("@", f), shQuote(paste0(API, "graphql_pr"))), stdout = TRUE); fromJSON(paste(out, collapse = ""), simplifyVector = FALSE) }
idx_f <- file.path(DIR, "contests_index.csv")
if (file.exists(idx_f) && !"office" %in% names(read.csv(idx_f, nrows = 1))) unlink(idx_f)   # index from a first version without the office column
if (!file.exists(idx_f)) {
  rows <- list()
  for (y in seq(1990, 2024, 2)) {
    f <- list(global = list(years = list(from = y, to = y)), ballotQuestions = list(text = "", types = list(), number = "", divisions = list()),
              contests = list(candidates = list(), divisions = list(), offices = list(list(id = 5))), specialElectionsOnly = FALSE, voterStats = FALSE, stages = list())
    r <- gql(list(pagination = list(page = 1, size = 100), filters = f)); stopifnot(is.null(r$errors)); s <- r$data$search
    stopifnot(s$meta$totalPages == 1)
    for (c in s$results) if (!is.null(c$event)) rows[[length(rows) + 1]] <- data.frame(year = y, contest_id = c$id, office = c$office$name %||% NA_character_, date = substr(c$event$startDate, 1, 10), event_id = c$event$id, event_type = c$eventTypeDisplayName %||% "",
        special = isTRUE(c$isSpecial) || isTRUE(c$event$isSpecial), runoff = isTRUE(c$isRunoff), division = c$division$displayName %||% NA_character_, n_candidates = length(c$candidates))
    Sys.sleep(0.3)
  }
  idx <- do.call(rbind, rows); write.csv(idx, idx_f, row.names = FALSE)
}
idx <- read.csv(idx_f, stringsAsFactors = FALSE)

message("contests found: ", nrow(idx)); print(as.data.frame(table(idx$year, ifelse(idx$special, "special", ifelse(grepl("-11-", idx$date), "Nov general", "other date")))))
## keep only: office "U.S. House", a Congressional District division (not a locality-level rollup), the November General Election, not special
dl <- idx[!idx$special & idx$office == "U.S. House" & grepl("^Congressional District", idx$division) & grepl("General", idx$event_type) & grepl("^[0-9]{4}-11-", idx$date), ]
message("U.S. House November general contests: ", nrow(dl)); print(as.data.frame(table(dl$year)))
idx$keep <- idx$contest_id %in% dl$contest_id; write.csv(idx, idx_f, row.names = FALSE)
for (i in seq_len(nrow(dl))) { f <- file.path(DIR, sprintf("contest_%d.csv", dl$contest_id[i]))
  if (!file.exists(f) || file.size(f) < 50) { Sys.sleep(0.4); system2("curl", c("-sL", "-m", "90", "-A", shQuote("Mozilla/5.0 (research; county election data project)"), "-H", shQuote("X-Elstats-Tenant: va"), "-o", shQuote(f),
                                                                    shQuote(sprintf("%sdownload_contest/%d_table.csv?split_party=false", API, dl$contest_id[i])))) } }
extra <- setdiff(list.files(DIR, "^contest_.*\\.csv$"), sprintf("contest_%d.csv", dl$contest_id)); unlink(file.path(DIR, extra))   # drop cached files of contests we do not use
message("csv files: ", length(list.files(DIR, "^contest_.*\\.csv$")), " of ", nrow(dl))
