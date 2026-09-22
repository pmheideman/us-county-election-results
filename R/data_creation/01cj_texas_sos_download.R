## Download the Texas Secretary of State "Historical Elections - Official Results" county pages for U.S. Representative, general elections 1992-2018.
## Site: https://elections.sos.state.tx.us/index.htm (1992-current). For an election id E the page elchistE_raceselect.htm lists the races (option value = race id),
## and elchistE_race<id>.htm is a static table: county rows x candidate columns (candidate name + party letters), plus an "ALL COUNTIES" row.
## Raw HTML is cached under R/data/raw_house_county_open_states/texas_sos/ (re-runs skip existing files). 1990 is not on the site.
DIR <- file.path("R", "data", "raw_house_county_open_states", "texas_sos"); dir.create(DIR, showWarnings = FALSE, recursive = TRUE)
ELECT <- c(`1992` = 5, `1994` = 11, `1996` = 56, `1998` = 72, `2000` = 82, `2002` = 95, `2004` = 114, `2006` = 127, `2008` = 141, `2010` = 154, `2012` = 164, `2014` = 175, `2016` = 319, `2018` = 331)
BASE <- "https://elections.sos.state.tx.us/"
get <- function(u, f) { if (!file.exists(f) || file.size(f) < 500) { Sys.sleep(0.4); system2("curl", c("-sL", "-m", "40", "-A", shQuote("Mozilla/5.0 (research; county election data project)"), "-o", shQuote(f), shQuote(u))) }; file.exists(f) && file.size(f) > 500 }
races <- list()
for (y in names(ELECT)) {
  e <- ELECT[[y]]; f <- file.path(DIR, sprintf("elchist%d_raceselect.htm", e)); stopifnot(get(paste0(BASE, basename(f)), f))
  h <- readLines(f, warn = FALSE, encoding = "latin1")
  o <- regmatches(h, regexec("<OPTION value=\"([0-9]+)\"[^>]*>\\s*(U\\. ?S\\. Rep[^<]*?)\\s*$", h, ignore.case = TRUE)); o <- o[lengths(o) > 0]
  d <- data.frame(year = as.integer(y), election = e, race = vapply(o, `[`, "", 2), label = trimws(vapply(o, `[`, "", 3)))
  d <- d[!duplicated(d$race), ]; races[[y]] <- d
  message(y, ": ", nrow(d), " U.S. Representative races")
  for (r in d$race) stopifnot(get(sprintf("%selchist%d_race%s.htm", BASE, e, r), file.path(DIR, sprintf("elchist%d_race%s.htm", e, r))))
}
races <- do.call(rbind, races); write.csv(races, file.path(DIR, "races_index.csv"), row.names = FALSE)
message("total race pages: ", nrow(races))
