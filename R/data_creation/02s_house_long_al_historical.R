## Candidate-level LONG table for Alabama U.S. House 1980-2012 from the Auburn University spreadsheet (shares file elect_he_cty_al_historical.rds,
## script 01ar). Southeast batch. Every helper/constant of 01ar is REUSED unchanged by parsing that file and evaluating only its top-level
## assignments (never its parse_* functions, the loops, or the final saveRDS). The four sheet parsers are copied below with one change: each
## emitted row also carries the column's header text (the candidate) and the district, which 01ar discarded after classifying the party.
## party_group = 01ar's classify_party() result (DEMOCRAT -> DEM, REPUBLICAN -> REP, OTHER -> OTHER), incl. its Bonner (10Gen) correction.
## Output: R/output/long/he_al_historical.rds. Acceptance: check_long_vs_source against ALL keys of elect_he_cty_al_historical.rds (1980-2012).

source(file.path("R", "00_setup.R"))
save_step <- function(df, name) invisible(df)
library(readxl); library(stringr); library(readr)
source(file.path("R", "long_helpers.R"))

exprs <- parse(file = file.path("R", "data_creation", "01ar_house_county_alabama_historical.R"))
skip <- c("parse_stacked_sheet", "parse_wide_sheet", "parse_2002_sheet", "parse_2012", "res2012", "results", "all_raw", "agg", "al1_1980")
keep <- vapply(exprs, function(e) {
  if (!is.call(e) || !identical(e[[1]], as.name("<-"))) return(FALSE)
  t <- e[[2]]
  (is.name(t) && !as.character(t) %in% skip) || (is.call(t) && identical(t[[1]], as.name("names")))
}, NA)
eval(exprs[keep])

dist_of <- function(x) { m <- str_match(as.character(x), "(\\d+)"); ifelse(is.na(m[, 2]), NA_character_, m[, 2]) }
## candidate display name from a header cell (strip trailing party code; bare write-in headers -> "Write-in")
cand_name <- function(h, party_group) {
  h <- trimws(gsub("\\s+", " ", h)); if (is.na(h) || !nzchar(h)) return(NA_character_)
  if (toupper(h) %in% c("WI", "W/I", "WRITE-IN", "WRITE-INS", "WRITE IN", "WRITE INS")) return("Write-in")
  y <- gsub("\\(\\s*[A-Za-z]+\\s*\\)", " ", h); y <- sub("\\s*-\\s*[A-Za-z]+\\s*$", "", y); y <- trimws(gsub("\\s+", " ", y))
  y <- trimws(sub("\\s+WI$", "", y))
  if (!nzchar(y) || toupper(y) %in% c("DEM", "REP", "LIB", "IND", "CON", "OTHER", "WI")) return(NA_character_)
  y
}
mk <- function(year, county, party, votes, header, district) {
  tibble(year = year, county = county, party = party, votes = votes, cand = cand_name(header, party), district = district)
}
results <- list()

## ---------------- 1. stacked sheets (1980-1998) ---------------------------------------------------------------------------------------------------
parse_stacked_sheet <- function(sheet, year) {
  d <- suppressMessages(read_excel(XLS_PATH, sheet = sheet, col_names = FALSE, col_types = "text"))
  nr <- nrow(d); nc <- ncol(d); m <- as.matrix(d)
  hits <- which(apply(m, c(1, 2), function(x) !is.na(x) && grepl(district_header_re, x, ignore.case = TRUE)), arr.ind = TRUE)
  hits <- hits[order(hits[, "row"], hits[, "col"]), , drop = FALSE]
  distinct_start_rows <- sort(unique(hits[, "row"]))
  block_defs <- list()
  for (r in distinct_start_rows) {
    same_row_hits <- sort(hits[hits[, "row"] == r, "col"])
    next_start_row <- distinct_start_rows[distinct_start_rows > r]
    row_end <- if (length(next_start_row) > 0) min(next_start_row) - 1 else nr
    for (i in seq_along(same_row_hits)) {
      col_start <- if (i == 1) 1 else same_row_hits[i] - 1
      col_end <- if (i < length(same_row_hits)) same_row_hits[i + 1] - 2 else nc
      block_defs[[length(block_defs) + 1]] <- list(row_start = r, row_end = row_end, col_start = col_start, col_end = col_end,
                                                   district = dist_of(m[r, same_row_hits[i]]))
    }
  }
  out <- list(); gen88_block_idx <- 0
  for (bi in seq_along(block_defs)) {
    bd <- block_defs[[bi]]; cur_district <- bd$district
    block <- m[bd$row_start:bd$row_end, bd$col_start:bd$col_end, drop = FALSE]
    if (nrow(block) < 2) next
    row_is_county <- function(r) { for (cell in r) { if (!is.na(cell) && !is.na(match_county(cell))) return(TRUE) }; FALSE }
    data_start <- NA
    for (ri in 2:nrow(block)) { if (row_is_county(block[ri, ])) { data_start <- ri; break } }
    no_labels <- is.na(data_start)
    if (no_labels) {
      gen88_block_idx <- gen88_block_idx + 1
      if (sheet != "GEN88" || is.null(gen88_fallback[[as.character(gen88_block_idx)]])) next
      counties <- gen88_fallback[[as.character(gen88_block_idx)]]
      for (ri in 2:nrow(block)) { if (any(!is.na(as_num(block[ri, ])))) { data_start <- ri; break } }
      header_text <- apply(block[2:(data_start - 1), , drop = FALSE], 2, function(col) paste(col[!is.na(col)], collapse = " "))
    } else {
      header_text <- apply(block[2:(data_start - 1), , drop = FALSE], 2, function(col) paste(col[!is.na(col)], collapse = " "))
    }
    party_of_col <- sapply(header_text, classify_party)
    ri <- data_start; county_i <- 0
    while (ri <= nrow(block)) {
      row <- block[ri, ]
      if (all(is.na(row) | trimws(row) == "")) {
        nxt <- ri + 1
        if (nxt <= nrow(block)) {
          nxt_row <- block[nxt, ]
          if (sum(!is.na(sapply(nxt_row, classify_party))) >= 1 && !no_labels) {
            party_of_col <- sapply(nxt_row, classify_party)
            header_text <- ifelse(is.na(nxt_row), "", nxt_row)
            cur_district <- as.character(as.integer(cur_district) + 1L)     # unlabeled sub-block (GEN94 district 6): previous district + 1
            ri <- nxt + 1; next
          }
        }
        break
      }
      if (no_labels) {
        county_i <- county_i + 1
        if (county_i > length(counties)) { ri <- ri + 1; next }
        county <- toupper(counties[county_i]); val_cells <- seq_along(row)
      } else {
        found_col <- NA; county <- NA
        for (ci in seq_along(row)) { cand <- match_county(row[ci]); if (!is.na(cand)) { found_col <- ci; county <- cand; break } }
        if (is.na(found_col)) { ri <- ri + 1; next }
        val_cells <- setdiff(seq_along(row), found_col)
      }
      for (ci in val_cells) {
        if (ci > length(party_of_col) || is.na(party_of_col[ci])) next
        v <- as_num(row[ci]); if (is.na(v)) next
        out[[length(out) + 1]] <- mk(year, county, party_of_col[ci], v, header_text[ci], cur_district)
      }
      ri <- ri + 1
    }
  }
  if (length(out) == 0) return(tibble()); bind_rows(out)
}
for (s in names(stacked_sheets)) results[[s]] <- parse_stacked_sheet(s, stacked_sheets[[s]])

## ---------------- 2. wide sheets (2000, 2004-2010) ------------------------------------------------------------------------------------------------
parse_wide_sheet <- function(sheet, year) {
  d <- suppressMessages(read_excel(XLS_PATH, sheet = sheet, col_names = FALSE, col_types = "text"))
  m <- as.matrix(d); nr <- nrow(m); nc <- ncol(m)
  match_counts <- sapply(seq_len(nc), function(ci) sum(!is.na(sapply(m[, ci], match_county))))
  county_col <- which.max(match_counts)
  header_row <- NA
  for (ri in 1:min(4, nr)) { if (sum(!is.na(sapply(m[ri, ], classify_party))) >= 2) { header_row <- ri; break } }
  if (is.na(header_row)) stop("could not find header row in ", sheet)
  party_of_col <- sapply(m[header_row, ], classify_party)
  party_of_col <- apply_known_corrections(sheet, m[header_row, ], party_of_col)
  ## district of each column: nearest cell at or to the left, in the rows above the header, that contains a number
  district_of_col <- sapply(seq_len(nc), function(ci) {
    for (r in rev(seq_len(header_row - 1))) { for (k in ci:1) { x <- m[r, k]; if (!is.na(x) && grepl("\\d", x)) return(dist_of(x)) } }
    NA_character_
  })
  out <- list()
  for (ri in (header_row + 1):nr) {
    row <- m[ri, ]; lbl <- row[county_col]
    if (is.na(lbl) || trimws(lbl) == "") next
    county <- match_county(lbl); if (is.na(county)) next
    for (ci in seq_len(nc)) {
      if (ci == county_col || is.na(party_of_col[ci])) next
      v <- as_num(row[ci]); if (is.na(v)) next
      out[[length(out) + 1]] <- mk(year, county, party_of_col[ci], v, m[header_row, ci], district_of_col[ci])
    }
  }
  bind_rows(out)
}
for (s in names(wide_sheets)) results[[s]] <- parse_wide_sheet(s, wide_sheets[[s]])

## ---------------- 3. 2002 (four sheets; row 1 district, row 2 party code, row 3 candidate) ---------------------------------------------------------
parse_2002_sheet <- function(sheet) {
  d <- suppressMessages(read_excel(XLS_PATH, sheet = sheet, col_names = FALSE, col_types = "text"))
  m <- as.matrix(d); nr <- nrow(m); nc <- ncol(m)
  party_of_col <- sapply(m[2, ], function(x) { xu <- toupper(trimws(x)); if (is.na(x) || xu == "") return(NA_character_)
    if (xu == "DEM") return("DEMOCRAT"); if (xu == "REP") return("REPUBLICAN"); "OTHER" })
  district_of_col <- sapply(seq_len(nc), function(ci) { for (k in ci:1) { x <- m[1, k]; if (!is.na(x) && grepl("\\d", x)) return(dist_of(x)) }; NA_character_ })
  out <- list()
  for (ri in 4:nr) {
    row <- m[ri, ]; lbl <- row[1]
    if (is.na(lbl) || trimws(lbl) == "") next
    county <- match_county(lbl); if (is.na(county)) next
    for (ci in 2:nc) {
      if (is.na(party_of_col[ci])) next
      v <- as_num(row[ci]); if (is.na(v)) next
      nm <- if (!is.na(m[3, ci]) && nzchar(trimws(m[3, ci]))) m[3, ci] else m[2, ci]
      out[[length(out) + 1]] <- mk(2002, county, party_of_col[ci], v, nm, district_of_col[ci])
    }
  }
  bind_rows(out)
}
results[["2002"]] <- bind_rows(parse_2002_sheet("Gen.02.Hse.1&2"), parse_2002_sheet("Hse.3&4"), parse_2002_sheet("Hse.5&6"), parse_2002_sheet("Hse.7"))

## ---------------- 4. 2012 (2 x 2 grid of blocks) -----------------------------------------------------------------------------------------------------
parse_2012 <- function() {
  d <- suppressMessages(read_excel(XLS_PATH, sheet = "12 Gen.", col_names = FALSE, col_types = "text"))
  m <- as.matrix(d); nr <- nrow(m); nc <- ncol(m)
  block_starts <- which(apply(m, 1, function(r) any(grepl("FOR UNITED STATES REPRESENTATIVE", r, ignore.case = TRUE))))
  out <- list()
  for (bi in seq_along(block_starts)) {
    hdr_row <- block_starts[bi]
    title_col <- which(grepl("FOR UNITED STATES REPRESENTATIVE", m[hdr_row, ], ignore.case = TRUE))
    for (tc in title_col) {
      same_row_titles <- title_col[title_col > tc]
      end_col <- if (length(same_row_titles) > 0) min(same_row_titles) - 1 else nc
      cand_row <- hdr_row + 1; county_hdr_row <- hdr_row + 2; block_cols <- tc:end_col
      party_of_col <- sapply(m[cand_row, block_cols], classify_party); names(party_of_col) <- block_cols
      dist <- dist_of(m[hdr_row, tc])
      ri <- county_hdr_row + 1
      while (ri <= nr) {
        lbl <- m[ri, tc]
        if (is.na(lbl) || trimws(lbl) == "") break
        if (toupper(trimws(lbl)) %in% c("TOTALS:", "TOTAL:", "TOTALS", "TOTAL")) break
        county <- match_county(lbl); if (is.na(county)) { ri <- ri + 1; next }
        for (ci in block_cols) {
          if (ci == tc) next
          pc <- party_of_col[as.character(ci)]; if (is.na(pc)) next
          v <- as_num(m[ri, ci]); if (is.na(v)) next
          out[[length(out) + 1]] <- mk(2012, county, pc, v, m[cand_row, ci], dist)
        }
        ri <- ri + 1
      }
    }
  }
  bind_rows(out)
}
results[["2012"]] <- parse_2012()

## ---------------- combine, finalize, verify ---------------------------------------------------------------------------------------------------------
all_raw <- bind_rows(results) %>% filter(!is.na(county), !is.na(party), !is.na(votes))
raw <- all_raw %>% left_join(al_counties, by = c("county" = "county_name")) %>% filter(!is.na(county_fips)) %>%
  mutate(party_group = case_when(party == "DEMOCRAT" ~ "DEM", party == "REPUBLICAN" ~ "REP", TRUE ~ "OTHER")) %>%   # BEFORE the label column is blanked
  transmute(year, county_fips, district, candidate = ifelse(is.na(cand), paste0("(unnamed ", tolower(party), " column)"), cand),
            party = NA_character_, party_group, votes)
long <- finalize_long(raw, "al_historical")
save_long(long, "he_al_historical")
res <- check_long_vs_source(long, file.path(OUTPUT_DIR, "elect_he_cty_al_historical.rds")); print(res)
message("rows ", nrow(long), "; candidates ", nrow(distinct(long, year, district, candidate)), "; rows with NA district: ", sum(is.na(long$district)),
        "; unnamed-candidate rows: ", sum(grepl("^\\(unnamed", long$candidate)))
print(as.data.frame(long %>% group_by(year) %>% summarise(districts = n_distinct(district, na.rm = TRUE), na_district = sum(is.na(district)), unnamed = sum(grepl("^\\(unnamed", candidate)), .groups = "drop")))
print(as.data.frame(head(long %>% filter(year == 2012, county_fips == 1073), 6)))
