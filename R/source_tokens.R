## Release source tokens and the SOURCES.csv table (added 2026-09-21).
## Internal build labels in the long table (e.g. "va_1996", "in", "tx_sos_1998", "medsl", "ks") are mapped, per row, to release tokens
##   <state postal code lower case>_<origin>_<year>      e.g. in_sos_report_1998, va_sos_web_1998, tx_openelections_2004, nc_medsl_2018 (see docs in DATA_DICTIONARY.md)
## using R/data/source_registry.csv (one row per state x internal label x year range: origin, publisher, document, locator, format, how obtained, how transcribed, license).
## When registry ranges overlap for a row (e.g. Indiana OpenElections 2004-2014 and the official 2010 report), the narrowest year range wins.
suppressMessages(library(dplyr))
REGISTRY_FILE <- file.path(PROJECT_ROOT, "R", "data", "source_registry.csv")
load_registry <- function() readr::read_csv(REGISTRY_FILE, col_types = readr::cols(.default = "c")) %>% mutate(year_from = as.integer(year_from), year_to = as.integer(year_to))
base_token <- function(src) sub("_[0-9]{4}$", "", src)
## returns one registry row per input row (NA where nothing matches -> the caller must stop)
match_registry <- function(src, state_po, year, reg = load_registry(), office = "house") {
  key <- tibble(i = seq_along(src), base = base_token(src), state_po = state_po, year = year, office = office)
  j <- key %>% inner_join(reg, by = c("base" = "base_token"), relationship = "many-to-many") %>%
    filter((state_po.y == "*" | state_po.y == state_po.x), office.y == office.x, year >= year_from, year <= year_to) %>% mutate(span = year_to - year_from) %>%
    group_by(i) %>% slice_min(span, n = 1, with_ties = FALSE) %>% ungroup()
  reg_rows <- j[match(seq_along(src), j$i), ]
  reg_rows
}
release_source_token <- function(src, state_po, year, reg = load_registry(), office = "house") {
  m <- match_registry(src, state_po, year, reg, office)
  if (anyNA(m$origin)) stop("no source registry entry for: ", paste(unique(paste(src, state_po, year)[is.na(m$origin)])[1:10], collapse = "; "))
  paste(tolower(state_po), m$origin, year, sep = "_")
}
## years strings of the corrections log ("1990-2010", "1998;2000", "2016-2024", "n/a") -> logical: does the entry cover this year
years_cover <- function(spec, y) { parts <- strsplit(spec, "[;,] *")[[1]]; any(vapply(parts, function(p) { p <- trimws(p); if (grepl("^[0-9]{4}-[0-9]{4}$", p)) { r <- as.integer(strsplit(p, "-")[[1]]); y >= r[1] & y <= r[2] } else if (grepl("^[0-9]{4}$", p)) y == as.integer(p) else FALSE }, NA)) }
build_sources_table <- function(long_rel, log_file, indiana_manifest = file.path(PROJECT_ROOT, "R", "data", "indiana_sos_reports", "reports.csv"), reg = load_registry()) {
  s <- long_rel %>% group_by(source, source_internal, office, state_po, year) %>% summarise(n_rows = n(), n_counties = n_distinct(county_fips), n_districts = n_distinct(district), .groups = "drop")
  m <- match_registry(s$source_internal, s$state_po, s$year, reg, s$office)
  out <- bind_cols(s %>% select(-source_internal), m %>% select(origin, publisher, document, locator, format, obtained, transcription, license, license_status, notes))
  in_url <- if (file.exists(indiana_manifest)) readr::read_csv(indiana_manifest, col_types = readr::cols(.default = "c")) %>% transmute(year = as.integer(year), url = source_url) else tibble(year = integer(), url = character())
  out <- out %>% left_join(in_url, by = "year") %>% mutate(locator = ifelse(locator == "INDIANA_URL", paste0(url, " ; local copy R/data/indiana_sos_reports/indiana_election_report_", year, ".pdf"), locator)) %>% select(-url)
  fill <- function(x) mapply(function(v, y, st) gsub("\\{state\\}", st, gsub("\\{year\\}", y, v)), x, out$year, out$state_po, USE.NAMES = FALSE)   # gsub's replacement is not vectorised
  out <- out %>% mutate(document = fill(document), locator = fill(locator))
  lg <- read.csv(log_file, stringsAsFactors = FALSE); lg$id <- seq_len(nrow(lg))
  lg_states <- strsplit(lg$state, "[;,/ ]+")
  out$corrections_log_entries <- vapply(seq_len(nrow(out)), function(k) { hit <- lg$id[vapply(seq_len(nrow(lg)), function(r) out$state_po[k] %in% lg_states[[r]] && years_cover(lg$years[r], out$year[k]), NA)]; paste(hit, collapse = ";") }, "")
  out %>% arrange(state_po, year)
}
