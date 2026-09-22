## South Dakota county-level U.S. House results, 1990, 1994 and 1996-2012, from the SD Secretary of State's Election History
## archive: https://sdsos.gov/elections-voting/election-resources/election-history/election-history-search.aspx
## (a static index page of ~450 links; used here with an ordinary browser User-Agent, no login).
##
## South Dakota has ONE at-large seat, so each year is one race reported for all 66 counties (Shannon County, FIPS 46113, is
## used for all years before its 2015 rename to Oglala Lakota 46102 -- same as the existing 2014 build).
## Formats found (the page's own link table drives which file holds what):
##   1990        statewide-offices grid; the House race is the UNTITLED table with Tim Johnson-D / Don Frankenfeld-R.
##   1994, 1996, 1998  grid page with several statewide tables; the House one is titled "United States Representative".
##   2000-2006   a dedicated U.S. House page: one grid (counties x candidates "Name - Rep/Dem/Lib") with printed TOTAL row.
##   2008, 2010  long tables (Race, County, Party, Candidate, Votes, Percentage) filtered to "United States Representative".
##   2012        PDF "statewide candidates by county"; the House section has County / Noem / Varilek / TOTALS.
## NOT on the page: 1992 (registration/turnout/legislature only; guessed statewide-returns file names 302) -- still needs another
## source. 1994's "statewide offices" link on the index points to a turnout page, but the real returns page exists at the URL used below.
##
## Verification: grid pages with a printed TOTAL row (2000-2006) must tie column sums to it; every table must have 66 counties;
## and each year's candidate shares are compared with the infobox percentages on Wikipedia's page for that election.
## totalvote = sum of the named candidates' votes (no blanks/undervotes exist in these tables).
## Outputs R/output/elect_he_cty_sd_sos.rds and R/output/sd_sos_house_long.rds. Does NOT touch elect_cty_final.rds.

source(file.path("R", "00_setup.R"))
library(readr); library(xml2)

UA <- "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36"
RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "south_dakota_sos")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)
BASE <- "https://sdsos.gov/elections-voting/election-resources/election-history/"
SRC <- tribble(
  ~year, ~kind,  ~url,
  1990,  "grid1990", paste0(BASE, "1990/1990-general-election-returns-statewide-offices.aspx"),
  1994,  "grid",     paste0(BASE, "1994/1994_general_election_returns_statewide_offices.aspx"),   # NOT linked correctly from the index page (its link goes to a turnout page); found by guessing the file name
  1996,  "grid",     paste0(BASE, "1996/1996_general_election_returns_presidential_statewide_candidates.aspx"),
  1998,  "grid",     paste0(BASE, "1998/1998_general_election_official_canvass_statewide_candidates.aspx"),
  2000,  "grid1",    paste0(BASE, "2000/2000_general_election_official_returns_US_house_representatives.aspx"),
  2002,  "grid1",    paste0(BASE, "2002/2002_US_representative_official_returns.aspx"),
  2004,  "grid1",    paste0(BASE, "2004/2004_general_election_official_returns_us_house_of_representatives.aspx"),
  2006,  "grid1",    paste0(BASE, "2006/2006_general_election_official_returns_us_house_of_representatives.aspx"),
  2008,  "long",     paste0(BASE, "2008/2008_general_election_statewide_races_county_totals.aspx"),
  2010,  "long",     paste0(BASE, "2010/2010_general_election_statewide_candidates_by_county.aspx"),
  2012,  "pdf2012",  "https://sdsos.gov/elections-voting/assets/Archive/2012%20Assets/2012generalelectionstatewidecandidatesbycounty.pdf")

fetch <- function(url, dest) {
  if (!file.exists(dest) || file.size(dest) == 0)
    system2("curl", c("-sL", "-A", shQuote(UA), "--compressed", "-o", shQuote(dest), shQuote(url)))
  stopifnot(file.exists(dest), file.size(dest) > 1000)
  dest
}
num <- function(x) as.numeric(gsub("[^0-9.]", "", x))
party3 <- function(p) { p <- toupper(trimws(p)); ifelse(grepl("^R", p), "R", ifelse(grepl("^D", p), "D", "O")) }
txt <- function(nodes) trimws(gsub("\\s+", " ", gsub("\u00a0", " ", xml_text(nodes))))

## ---- HTML table -> list of character matrices ---------------------------------------------------------------------------
html_tables <- function(path) {
  doc <- read_html(path, options = "RECOVER")
  lapply(xml_find_all(doc, "//table"), function(tb) {
    rows <- lapply(xml_find_all(tb, ".//tr"), function(tr) txt(xml_find_all(tr, "./td|./th")))
    rows <- rows[lengths(rows) > 0]
    w <- max(lengths(rows)); do.call(rbind, lapply(rows, function(r) c(r, rep("", w - length(r)))))
  })
}
## grid table (county rows x candidate columns "Name - Party") -> long data frame; checks TOTAL row when present
parse_grid <- function(m, year) {
  hdr <- m[1, ]
  cand_cols <- which(grepl("(^|\\s)[-\u2013]\\s*(Rep|Dem|Lib|R|D|L|I|Ind)\\b\\s*$|[A-Za-z]-(R|D|L|I)$", hdr))
  stopifnot(length(cand_cols) >= 2)
  body <- m[-1, , drop = FALSE]
  county <- body[, 1]
  is_total <- grepl("^TOTALS?\\b", toupper(county)); is_pct <- grepl("^PERCENT", toupper(county))
  is_county <- nzchar(county) & !is_total & !is_pct & !grepl("^\\d", county)
  votes <- apply(body[is_county, cand_cols, drop = FALSE], 2, num)
  if (is.null(dim(votes))) votes <- matrix(votes, nrow = sum(is_county))
  stopifnot(!anyNA(votes))
  if (any(is_total)) {
    tot <- num(body[which(is_total)[1], cand_cols])
    stopifnot(all(colSums(votes) == tot))           # county sums == printed TOTAL row
    message(year, ": county sums tie to the printed TOTAL row (", format(sum(tot), big.mark = ","), " votes)")
  } else message(year, ": no printed TOTAL row in this table (checked against Wikipedia below)")
  cand <- hdr[cand_cols]
  tibble(year = year, county = rep(county[is_county], times = length(cand_cols)),
         candidate = rep(trimws(sub("\\s*[-\u2013]\\s*[A-Za-z]+\\s*$", "", cand)), each = sum(is_county)),
         party = rep(party3(sub(".*[-\u2013]\\s*([A-Za-z]+)\\s*$", "\\1", cand)), each = sum(is_county)),
         votes = as.vector(votes))
}

out <- list()
for (i in seq_len(nrow(SRC))) {
  y <- SRC$year[i]; k <- SRC$kind[i]
  ext <- if (k == "pdf2012") "pdf" else "html"
  f <- fetch(SRC$url[i], file.path(RAW_DIR, paste0(y, "_house_source.", ext)))
  if (k %in% c("grid1", "grid", "grid1990")) {
    tabs <- html_tables(f)
    pick <- if (k == "grid1") 1 else if (k == "grid") which(vapply(tabs, function(m) grepl("Representative", m[1, 1], ignore.case = TRUE) && nrow(m) > 60, NA))
            else which(vapply(tabs, function(m) any(grepl("Frankenfeld", m[1, ])), NA))
    stopifnot(length(pick) == 1)
    out[[as.character(y)]] <- parse_grid(tabs[[pick]], y)
  } else if (k == "long") {
    m <- html_tables(f)[[1]]; stopifnot(identical(unname(m[1, 1:5]), c("Race", "County", "Party", "Candidate", "Votes")))
    d <- as_tibble(m[-1, 1:5, drop = FALSE], .name_repair = "minimal"); names(d) <- c("race", "county", "party", "candidate", "votes")
    d <- d %>% filter(grepl("^United States Representative", race)) %>%
      transmute(year = y, county, candidate, party = party3(party), votes = num(votes))
    stopifnot(nrow(d) > 0); out[[as.character(y)]] <- d
    message(y, ": long table -> ", n_distinct(d$county), " counties, ", n_distinct(d$candidate), " candidates")
  } else {                                                    # 2012 PDF
    lines <- system2("pdftotext", c("-layout", shQuote(f), "-"), stdout = TRUE)
    s <- grep("United States Representative \\(South Dakota\\)", lines)[1]; stopifnot(!is.na(s))
    hdr <- lines[s + which(grepl("^County\\s", lines[(s + 1):length(lines)]))[1]]
    cands <- str_split(str_trim(sub("^County", "", sub("\\s+TOTALS.*$", "", hdr))), "\\s{2,}")[[1]]
    body <- lines[(s + which(grepl("^County\\s", lines[(s + 1):length(lines)]))[1] + 1):length(lines)]
    rows <- list()
    for (ln in body) {
      mm <- str_match(ln, "^([A-Za-z][A-Za-z .'-]*?)\\s{2,}((?:[0-9,]+\\s+)*[0-9,]+)\\s*$")
      if (is.na(mm[1, 1])) { if (length(rows) >= 60) break else next }
      v <- num(str_split(str_trim(mm[1, 3]), "\\s+")[[1]])
      if (length(v) != length(cands) + 1) break
      if (grepl("^TOTAL", toupper(trimws(mm[1, 2])))) { pdf_total <- v; break }        # printed statewide TOTAL row: checksum, not a county
      stopifnot(v[length(v)] == sum(v[-length(v)]))          # row TOTALS column == sum of candidates
      rows[[length(rows) + 1]] <- c(list(county = trimws(mm[1, 2])), as.list(v[-length(v)]))
    }
    stopifnot(exists("pdf_total"), all(colSums(do.call(rbind, lapply(rows, function(r) unlist(r[-1])))) == pdf_total[-length(pdf_total)]))
    message(y, ": PDF county sums tie to the printed TOTAL row (", format(pdf_total[length(pdf_total)], big.mark = ","), " votes)")
    d <- purrr::map_dfr(rows, function(r) tibble(year = y, county = r$county, candidate = cands, votes = unlist(r[-1]),
                                                 party = ifelse(grepl("Noem", cands), "R", ifelse(grepl("Varilek", cands), "D", "O"))))
    message(y, ": PDF -> ", n_distinct(d$county), " counties; each row's TOTALS column equals the sum of its candidates")
    out[[as.character(y)]] <- d
  }
}
long <- bind_rows(out)

## ---- counties -> FIPS (Shannon 46113 for all pre-2015 years) ------------------------------------------------------------------
norm <- function(x) gsub("[^A-Z]", "", toupper(x))
sd_fips <- read_delim(file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  filter(state == "SOUTH DAKOTA", !is.na(county_fips), county_fips != 46102, county_name != "OGLALA LAKOTA") %>% distinct(county_name, county_fips) %>%
  mutate(key = norm(county_name)) %>% distinct(key, county_fips)
stopifnot(nrow(sd_fips) == 66, !anyDuplicated(sd_fips$key))
long <- long %>% mutate(key = norm(county)) %>% left_join(sd_fips, by = "key")
if (anyNA(long$county_fips)) { print(unique(long$county[is.na(long$county_fips)])); stop("unmatched county names") }
cnt <- long %>% group_by(year) %>% summarise(counties = n_distinct(county_fips), candidates = n_distinct(candidate), votes = sum(votes))
stopifnot(all(cnt$counties == 66))
message("\n== counties per year (all must be 66) =="); print(as.data.frame(cnt))
save_step(long, "sd_sos_house_long")

sd <- long %>% group_by(year, county_fips) %>%
  summarise(totalvote = sum(votes), demovote_n = sum(votes[party == "D"]), repuvote_n = sum(votes[party == "R"]), .groups = "drop") %>%
  filter(totalvote > 0) %>%
  transmute(state = "SOUTH DAKOTA", year, cty_fips = county_fips, sample = "HE",
            demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote) %>%
  arrange(year, cty_fips) %>% save_step("elect_he_cty_sd_sos")
message("two-party sum range by year:"); print(as.data.frame(sd %>% group_by(year) %>% summarise(min = round(min(demovote + repuvote), 3), max = round(max(demovote + repuvote), 3))))

## ---- statewide winners / shares vs Wikipedia infobox percentages ---------------------------------------------------------------------
tot <- long %>% group_by(year, candidate, party) %>% summarise(v = sum(votes), .groups = "drop") %>% group_by(year) %>%
  arrange(desc(v), .by_group = TRUE) %>% mutate(share = v / sum(v)) %>% ungroup()
wp <- purrr::map_dfr(SRC$year, function(y) {
  f <- file.path(RAW_DIR, paste0("wikipedia_", y, ".txt"))
  if (!file.exists(f) || file.size(f) == 0) system2("curl", c("-sL", "-A", shQuote(UA), "-o", shQuote(f),
      shQuote(paste0("https://en.wikipedia.org/w/index.php?title=", y, "_United_States_House_of_Representatives_election_in_South_Dakota&action=raw"))))
  t <- paste(readLines(f, warn = FALSE), collapse = "\n")
  nm <- str_match_all(t, "\\|\\s*nominee(\\d)\\s*=\\s*(?:'''\\s*)?(?:\\[\\[(?:[^\\]|]*\\|)?)?([^\\]'|<\n]+)")[[1]]
  pc <- str_match_all(t, "\\|\\s*percentage(\\d)\\s*=\\s*(?:'''\\s*)?([0-9.]+)")[[1]]
  if (nrow(nm) == 0 || nrow(pc) == 0) return(tibble(year = y, wp_name = NA_character_, wp_pct = NA_real_))
  tibble(year = y, n = nm[, 2], wp_name = trimws(nm[, 3])) %>% inner_join(tibble(n = pc[, 2], wp_pct = as.numeric(pc[, 3])), by = "n") %>% select(-n)
})
last <- function(x) tolower(tail(strsplit(gsub("[^A-Za-z ]", "", x), " ")[[1]][nzchar(strsplit(gsub("[^A-Za-z ]", "", x), " ")[[1]])], 1))
cmp <- tot %>% mutate(k = vapply(candidate, last, "")) %>% inner_join(wp %>% mutate(k = vapply(wp_name, last, "")), by = c("year", "k")) %>%
  mutate(diff_pp = round(100 * share - wp_pct, 2)) %>% select(year, candidate, party, v, share, wp_pct, diff_pp)
message("\n== SOS statewide shares vs Wikipedia infobox (percentage points); |diff| should be < 0.1 ==")
print(as.data.frame(cmp %>% mutate(share = round(share, 4))))
message("max |diff| over ", nrow(cmp), " matched candidates: ", max(abs(cmp$diff_pp)), " pp")
message("\n== winners =="); print(as.data.frame(tot %>% group_by(year) %>% slice_head(n = 2) %>% mutate(share = round(share, 3))))
