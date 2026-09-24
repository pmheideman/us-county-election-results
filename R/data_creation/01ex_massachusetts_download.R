## Download Massachusetts U.S. House town-level results, 1990-2014 general elections, from the Secretary of the Commonwealth's Elections Statistics site (https://electionstats.state.ma.us/,
## the older "PD43+" ElectionStats platform -- NOT the Elstats/GraphQL platform of VA/VT/CO/NY/CT). Plain curl works:
##   1. GET /elections/search/year_from:1990/year_to:2014/office_id:5/stage:General  (office 5 = U.S. House) lists every election on ONE page (133: 10-11 districts x 13 years + 4 special
##      generals); each block has the year, office, district ("2nd Congressional") and election type ("General Election" / "Special General Election") and a link /elections/view/<id>/.
##   2. GET /elections/download/<id>/precincts_include:0/ -> CSV with one row per city/town (columns: City/Town, Ward, Pct, then one column per candidate, "All Others", "Blanks", "Total Votes Cast"; row 2 =
##      party under each candidate). precincts_include:1 adds a precinct breakdown (not needed).
## Kept: "General Election" only (the 4 special generals -- 1991, 2001, 2007, 2013 -- are dropped, as everywhere in this project). Cached under
## R/data/raw_house_county_open_states/massachusetts_elections/ (re-runs skip existing files).
DIR <- file.path("R", "data", "raw_house_county_open_states", "massachusetts_elections"); dir.create(DIR, showWarnings = FALSE, recursive = TRUE)
UA <- "Mozilla/5.0 (research; county election data project)"; BASE <- "https://electionstats.state.ma.us"
idx_f <- file.path(DIR, "elections_index.csv")
if (!file.exists(idx_f)) {
  html <- paste(system2("curl", c("-sL", "-m", "120", "-A", shQuote(UA), shQuote(paste0(BASE, "/elections/search/year_from:1990/year_to:2014/office_id:5/stage:General"))), stdout = TRUE), collapse = "\n")
  blocks <- strsplit(html, '<tr id="election-id-', fixed = TRUE)[[1]][-1]
  rows <- lapply(blocks, function(b) { id <- sub("^([0-9]+)\".*$", "\\1", b); cells <- regmatches(b, gregexpr('<td class="[^"]*" style="padding: 7px 4px;">[^<]*</td>|<td class="party_border_top">[^<]*</td>', b))[[1]]
    cells <- trimws(gsub("<[^>]+>", "", cells)); data.frame(election_id = id, year = as.integer(cells[1]), office = cells[2], district = cells[3], election_type = cells[4], stringsAsFactors = FALSE) })
  idx <- do.call(rbind, rows); write.csv(idx, idx_f, row.names = FALSE)
}
idx <- read.csv(idx_f, stringsAsFactors = FALSE)
message("elections listed: ", nrow(idx)); print(table(idx$year, idx$election_type))
dl <- idx[idx$election_type == "General Election" & idx$office == "U.S. House", ]
stopifnot(!anyDuplicated(dl[, c("year", "district")]))
for (i in seq_len(nrow(dl))) { f <- file.path(DIR, sprintf("election_%s.csv", dl$election_id[i]))
  if (!file.exists(f) || file.size(f) < 50) { Sys.sleep(0.4); system2("curl", c("-sL", "-m", "90", "-A", shQuote(UA), "-o", shQuote(f), shQuote(sprintf("%s/elections/download/%s/precincts_include:0/", BASE, dl$election_id[i])))) } }
message("csv files: ", length(list.files(DIR, "^election_.*\\.csv$")), " of ", nrow(dl))
