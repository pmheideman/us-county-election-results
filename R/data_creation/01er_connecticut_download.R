## Download Connecticut U.S. House town-level results, 1990-2018 general elections, from the Secretary of the State's Election History database (https://electionhistory.ct.gov/),
## the SAME Elstats/Civera platform as Virginia/Vermont/Colorado/New York (tenant "ct"). Unlike New York's site this one answers plain curl, so it is scripted like Colorado (01ea_colorado_download.R):
##   1. POST /api/graphql_pr (header X-Elstats-Tenant: ct) query SearchContests with offices=[{id:330}] ("Representative in Congress"), one call per year -> contest ids
##      (keep: eventTypeDisplayName == "General", not special, not runoff, November).
##   2. GET /api/download_contest/<contest id>_table.csv?split_party=true -> rows "Representative in Congress District" (district total), "City/Town" (town results; a town split between
##      districts appears in each) and, for some years, "Polling Place" (ignored). split_party=true keeps one column per candidate x ballot line (Democratic, Working Families, ...).
## Everything cached under R/data/raw_house_county_open_states/connecticut_elections/ (re-runs skip existing files).
library(jsonlite)
DIR <- file.path("R", "data", "raw_house_county_open_states", "connecticut_elections"); dir.create(DIR, showWarnings = FALSE, recursive = TRUE)
API <- "https://electionhistory.ct.gov/api/"; UA <- "Mozilla/5.0 (research; county election data project)"
QUERY <- "query SearchContests($pagination: Pagination!, $filters: SearchFilters!) { search(pagination: $pagination, filters: $filters) { meta { currentPage totalPages totalResults } results { id officeId isSpecial isRunoff eventTypeDisplayName event { id startDate isSpecial } office { id name } division { id displayName } candidates { id displayName nVotes isWinner isWriteIn } } } }"
gql <- function(vars) { f <- tempfile(fileext = ".json"); writeLines(toJSON(list(query = QUERY, variables = vars), auto_unbox = TRUE, null = "null"), f)
  out <- system2("curl", c("-s", "-m", "90", "-A", shQuote(UA), "-H", shQuote("Content-Type: application/json"), "-H", shQuote("X-Elstats-Tenant: ct"),
                           "-X", "POST", "-d", paste0("@", f), shQuote(paste0(API, "graphql_pr"))), stdout = TRUE); fromJSON(paste(out, collapse = ""), simplifyVector = FALSE) }
idx_f <- file.path(DIR, "contests_index.csv")
if (!file.exists(idx_f)) {
  rows <- list()
  for (y in seq(1990, 2018, 2)) {
    f <- list(global = list(years = list(from = y, to = y)), ballotQuestions = list(text = "", types = list(), number = "", divisions = list()),
              contests = list(candidates = list(), divisions = list(), offices = list(list(id = 330))), specialElectionsOnly = FALSE, voterStats = FALSE, stages = list())
    page <- 1; repeat {
      r <- gql(list(pagination = list(page = page, size = 100), filters = f)); stopifnot(is.null(r$errors)); s <- r$data$search
      for (c in s$results) if (!is.null(c$event) && !is.null(c$office) && c$office$name == "Representative in Congress")
        rows[[length(rows) + 1]] <- data.frame(year = y, contest_id = c$id, date = substr(c$event$startDate, 1, 10), event_type = c$eventTypeDisplayName %||% "",
          special = isTRUE(c$isSpecial) || isTRUE(c$event$isSpecial), runoff = isTRUE(c$isRunoff), division = c$division$displayName %||% NA_character_, n_candidates = length(c$candidates))
      if (page >= s$meta$totalPages) break; page <- page + 1; Sys.sleep(0.2)
    }
    Sys.sleep(0.3)
  }
  idx <- do.call(rbind, rows); write.csv(idx, idx_f, row.names = FALSE)
}
idx <- read.csv(idx_f, stringsAsFactors = FALSE)
keep <- !idx$special & !idx$runoff & idx$event_type == "General" & grepl("^[0-9]{4}-11-", idx$date); dl <- idx[keep, ]
message("U.S. House November general contests: ", nrow(dl)); print(as.data.frame(table(dl$year)))
stopifnot(!anyDuplicated(dl[, c("year", "division")]))
idx$keep <- idx$contest_id %in% dl$contest_id; write.csv(idx, idx_f, row.names = FALSE)
for (i in seq_len(nrow(dl))) { f <- file.path(DIR, sprintf("contest_%d.csv", dl$contest_id[i]))
  if (!file.exists(f) || file.size(f) < 50) { Sys.sleep(0.4); system2("curl", c("-sL", "-m", "90", "-A", shQuote(UA), "-H", shQuote("X-Elstats-Tenant: ct"), "-o", shQuote(f),
                                                                    shQuote(sprintf("%sdownload_contest/%d_table.csv?split_party=true", API, dl$contest_id[i])))) } }
extra <- setdiff(list.files(DIR, "^contest_.*\\.csv$"), sprintf("contest_%d.csv", dl$contest_id)); unlink(file.path(DIR, extra))
message("csv files: ", length(list.files(DIR, "^contest_.*\\.csv$")), " of ", nrow(dl))
