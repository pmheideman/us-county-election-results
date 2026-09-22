## Oklahoma county-level U.S. House results from five official State Election Board PDFs
## (placed in R/data/county_house_files/ by the user): 1994, 2000, 2002, 2006 general-election "U.S. Representative"
## tables and the 2010 "Election Night Results by County" report. All five carry a real text layer, so
## `pdftotext -layout` is enough (no OCR / hand transcription).
##
## Common structure: one table per congressional district = heading, candidate header, a "=====" rule, county rows
## (candidate votes ... TOTAL VOTES), then a printed "STATE TOTAL" row. Differences handled here:
##   * 2000/2002/2006 (short files) and 1994 (163 pp, tables spill across pages, bare page-number lines between):
##     heading "UNITED STATES REPRESENTATIVE, DISTRICT n"; party printed in the header as "(R)/(D)/(L)/(I)" or, in 2000, "(Rfm)"
##     (Reform). Only D and R feed demovote/repuvote; every other tag counts toward totalvote only.
##   * 2010: heading "Race: FOR U.S. REPRESENTATIVE, DISTRICT NO. n"; header has names only (no party). Parties are a
##     small hard-coded lookup, verified against Wikipedia's 2010 Oklahoma House results, whose statewide candidate
##     totals equal this PDF's STATE TOTAL rows exactly (e.g. Sullivan 151,173, O'Dell 45,656 [an INDEPENDENT, so
##     District 1 had no Democrat in 2010]). District 4 was unopposed in 2010 and does not appear in the report.
##
## Verification is built in and stops the run on any failure: every parsed row must have ncand+1 numbers with the
## TOTAL VOTES column equal to the sum of the candidate columns, and every candidate's county sum must equal the
## printed STATE TOTAL. Split counties are summed across districts. totalvote = sum of the named candidates' votes.
## Unopposed districts have no race on the ballot, hence no rows (reported, never filled).
##
## Outputs R/output/elect_he_cty_ok_pdfs.rds (all five years) and R/output/ok_pdfs_house_long.rds.
## Does NOT touch elect_cty_final.rds (fold-in is a separate step).

source(file.path("R", "00_setup.R"))
library(readr)

PDF_DIR <- file.path(PROJECT_ROOT, "R", "data", "county_house_files")
PDFS <- tribble(
  ~year, ~file,                              ~fmt,
  1994,  "OK_1994-general-results.pdf",      "old",
  2000,  "OK-2000_usrep.pdf",                "old",
  2002,  "OK_2002_usrep.pdf",                "old",
  2006,  "OK_2006_usrep.pdf",                "old",
  2010,  "OK_ge-results-county-20101102.pdf", "night"
)

## 2010 report prints no party. Verified against Wikipedia (statewide totals == printed STATE TOTAL rows).
PARTY_2010 <- c("JOHN SULLIVAN" = "R", "ANGELIA O'DELL" = "I",
                "CHARLES THOMPSON" = "R", "DAN BOREN" = "D",
                "FRANK D. LUCAS" = "R", "FRANKIE ROBBINS" = "D",
                "JAMES LANKFORD" = "R", "BILLY COYLE" = "D", "CLARK DUFFE" = "I", "DAVE WHITE" = "I")

num <- function(x) as.numeric(gsub(",", "", x))

parse_pdf <- function(year, file, fmt) {
  lines <- system2("pdftotext", c("-layout", shQuote(file.path(PDF_DIR, file)), "-"), stdout = TRUE)
  head_re <- if (fmt == "night") "^Race:\\s+FOR U\\.S\\. REPRESENTATIVE, DISTRICT NO\\. (\\d+)" else "^\\s*UNITED STATES REPRESENTATIVE,? DISTRICT (\\d+)"
  rule_re <- "^\\s*=+(\\s+=+)+\\s*$"
  out <- list(); rows_out <- list(); i <- 1; n <- length(lines)
  while (i <= n) {
    m <- str_match(lines[i], head_re)
    if (is.na(m[1, 2])) { i <- i + 1; next }
    district <- as.integer(m[1, 2])
    ## header = lines up to the "=====" rule
    j <- i + 1; hdr <- character()
    while (j <= n && !grepl(rule_re, lines[j])) { hdr <- c(hdr, lines[j]); j <- j + 1 }
    stopifnot(j <= n)
    n_groups <- length(str_extract_all(lines[j], "=+")[[1]])
    if (fmt == "night") {
      ncand <- n_groups - 2                                    # County + candidates + Total
      hl <- hdr[grepl("^County", hdr)]; stopifnot(length(hl) == 1)
      # split the header line on runs of 2+ spaces: County, cand1..candN, Votes
      parts <- str_split(str_trim(hl), "\\s{2,}")[[1]]
      cand <- parts[2:(1 + ncand)]
      party <- unname(PARTY_2010[cand]); stopifnot(!anyNA(party), length(cand) == ncand)
    } else {
      ncand <- n_groups - 1                                    # candidates + Total
      hs <- paste(hdr, collapse = "\n")
      party <- str_match_all(hs, "\\(([A-Za-z]+)\\)")[[1]][, 2]   # tags like (R) (D) (L) (I) and (Rfm) = Reform
      stopifnot(length(party) == ncand)
      # surname (col 2 of the header) for readable output
      cand <- str_match_all(hs, "([A-Z][A-Za-z.' ,-]*?) \\([A-Za-z]+\\)")[[1]][, 2] %>% trimws()
      stopifnot(length(cand) == ncand)
    }
    ## rows until STATE TOTAL
    k <- j + 1; rows <- list(); total_row <- NULL
    while (k <= n) {
      ln <- lines[k]
      if (grepl("STATE TOTAL", ln)) { total_row <- num(str_extract_all(ln, "[0-9][0-9,]*")[[1]]); break }
      if (grepl(head_re, ln)) stop("district ", district, " (", year, ") ran into the next heading without a STATE TOTAL row")
      mm <- str_match(ln, "^\\s*([A-Za-z][A-Za-z'. ]*?)\\s+((?:[0-9][0-9,]*\\s+)*[0-9][0-9,]*)\\s*$")
      if (!is.na(mm[1, 1]) && !grepl("Total|Votes|County|Race|Party|Run |Page", mm[1, 2])) {
        v <- num(str_extract_all(mm[1, 3], "[0-9][0-9,]*")[[1]])
        if (length(v) != ncand + 1) stop("row with ", length(v), " numbers, expected ", ncand + 1, ": ", ln)
        rows[[length(rows) + 1]] <- c(list(county = trimws(mm[1, 2])), as.list(v))
      }
      k <- k + 1
    }
    stopifnot(!is.null(total_row), length(total_row) == ncand + 1)
    mat <- do.call(rbind, lapply(rows, function(r) unlist(r[-1])))
    # (1) per-row: TOTAL VOTES column == sum of candidate columns
    stopifnot(all(rowSums(mat[, seq_len(ncand), drop = FALSE]) == mat[, ncand + 1]))
    # (2) per-candidate: county sums == printed STATE TOTAL
    stopifnot(all(colSums(mat) == total_row))
    counties <- vapply(rows, function(r) r$county, "")
    stopifnot(!anyDuplicated(counties))
    out[[length(out) + 1]] <- tibble(year = year, district = district,
      county = rep(counties, times = ncand), candidate = rep(cand, each = length(counties)),
      party = rep(party, each = length(counties)), votes = as.vector(mat[, seq_len(ncand)]))
    message(year, " CD", district, ": ", length(counties), " counties, ", ncand, " candidates; county sums == STATE TOTAL (",
            format(total_row[ncand + 1], big.mark = ","), " votes)")
    i <- k + 1
  }
  bind_rows(out)
}

long <- purrr::pmap_dfr(PDFS, function(year, file, fmt) parse_pdf(year, file, fmt))

## ---- counties -> FIPS ----------------------------------------------------------------------------------
norm <- function(x) gsub("[^A-Z]", "", toupper(x))
ok_fips <- read_delim(file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
                      delim = "\t", show_col_types = FALSE) %>%
  filter(state == "OKLAHOMA", !is.na(county_fips)) %>% distinct(county_name, county_fips) %>%
  mutate(key = norm(county_name)) %>% distinct(key, county_fips)
stopifnot(!anyDuplicated(ok_fips$key), nrow(ok_fips) == 77)
long <- long %>% mutate(key = norm(county)) %>% left_join(ok_fips, by = "key")
if (anyNA(long$county_fips)) { print(unique(long$county[is.na(long$county_fips)])); stop("unmatched county names") }
save_step(long, "ok_pdfs_house_long")

## ---- aggregate ---------------------------------------------------------------------------------------------
elect_he_cty_ok_pdfs <- long %>% group_by(year, county_fips) %>%
  summarise(totalvote = sum(votes), demovote_n = sum(votes[party == "D"]), repuvote_n = sum(votes[party == "R"]), .groups = "drop") %>%
  filter(totalvote > 0) %>%
  transmute(state = "OKLAHOMA", year, cty_fips = county_fips, sample = "HE",
            demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote) %>%
  arrange(year, cty_fips) %>% save_step("elect_he_cty_ok_pdfs")

message("\n== districts / counties per year (OK had 6 seats through 2002, 5 after) ==")
print(long %>% group_by(year) %>% summarise(districts = paste(sort(unique(district)), collapse = ","), counties = n_distinct(county_fips)))
message("two-party sum range:")
print(elect_he_cty_ok_pdfs %>% group_by(year) %>% summarise(min = round(min(demovote + repuvote), 3), max = round(max(demovote + repuvote), 3)))

## ---- winners (sanity vs known results) -----------------------------------------------------------------------------
message("\n== winners ==")
print(long %>% group_by(year, district, candidate, party) %>% summarise(v = sum(votes), .groups = "drop") %>%
        group_by(year, district) %>% arrange(desc(v), .by_group = TRUE) %>% mutate(share = round(v / sum(v), 3)) %>%
        slice_head(n = 2), n = 60)

## ---- House total vs presidential total, same county (catches doubled/missing data; expect ~0.7-1.0) ------------------------
pe <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")) %>% filter(sample == "PE", cty_fips %/% 1000 == 40) %>%
  select(pe_year = year, cty_fips, pe_tot = totalvote)
ratio <- elect_he_cty_ok_pdfs %>% mutate(pe_year = ifelse(year %% 4 == 0, year, year - 2)) %>%
  left_join(pe, by = c("pe_year", "cty_fips")) %>% mutate(r = totalvote / pe_tot)
message("\n== House total / presidential total (nearest presidential year), by year ==")
print(ratio %>% group_by(year) %>% summarise(n = n(), min = round(min(r, na.rm = TRUE), 2), median = round(median(r, na.rm = TRUE), 2), max = round(max(r, na.rm = TRUE), 2)))

## ---- 2010 vs existing OpenElections-based rows (63 counties) -----------------------------------------------------------
old <- readRDS(file.path(OUTPUT_DIR, "elect_he_cty_ok.rds")) %>% filter(year == 2010) %>% select(cty_fips, o_dem = demovote, o_rep = repuvote, o_tot = totalvote)
c10 <- inner_join(elect_he_cty_ok_pdfs %>% filter(year == 2010), old, by = "cty_fips") %>%
  mutate(dd = abs(demovote - o_dem), dr = abs(repuvote - o_rep), dt = abs(totalvote - o_tot) / o_tot)
message("\n== 2010 PDF vs OpenElections build: ", nrow(c10), " counties in common; identical totals in ", sum(c10$dt < 1e-9),
        "; max |dem diff| ", round(max(c10$dd), 4), "; max |rep diff| ", round(max(c10$dr), 4), "; max total rel diff ", round(max(c10$dt), 4))
message("counties in PDF 2010 not in OpenElections: ", nrow(anti_join(elect_he_cty_ok_pdfs %>% filter(year == 2010), old, by = "cty_fips")),
        "; in OpenElections not in PDF: ", nrow(anti_join(old, elect_he_cty_ok_pdfs %>% filter(year == 2010), by = "cty_fips")))
