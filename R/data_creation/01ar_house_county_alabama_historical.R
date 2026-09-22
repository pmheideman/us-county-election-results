## Alabama county-level U.S. House general election results, 1980-2012, parsed from a
## user-supplied spreadsheet (R/data/county_house_files/eaushouse1980-2012_0.xls -- Auburn
## University's compiled Alabama election archive). Fills a much deeper pre-MEDSL gap than the
## OpenElections-based 01s_house_county_alabama.R build, which only got partial 2012 (61/67
## counties) and 2014 (64/67), and flagged that MEDSL's own AL 2016 data looks internally
## inflated/unreliable (see project notes).
##
## The spreadsheet packs Alabama's ~7 congressional districts in two very different layouts
## depending on era:
##   - "Stacked" sheets (1980-1998): each district is its own vertically-stacked mini-table
##     (district-header row, then a candidate/party header -- sometimes wrapped across 2 rows --
##     then one row per county, then a Total row), separated by blank rows.
##   - "Wide matrix" sheets (2000-2012): all counties are rows once; each congressional district
##     is a side-by-side block of columns (district-header row, candidate/party header row, then
##     the same county rows populated only in their own district's columns).
## The 2002 election is split across 4 sheets (one block of 1-2 districts each), and 2012 uses
## its own 2x2 grid-of-blocks layout distinct from every other wide sheet.
##
## Party-code convention is NOT stable across sheets or even across district blocks within one
## sheet -- confirmed empirically (not assumed) via known real race outcomes: "(AD)"/"(AR)"/"(AL)"
## in the 1980s sheets stands for Democrat/Republican/Libertarian respectively (verified e.g. via
## Gen82's Albert Lee Smith Jr. (AR) getting a large, incumbent-plausible vote share while Charles
## Ewing (AL) gets under 1% -- Libertarian, not Democrat, despite the "L" superficially suggesting
## otherwise). Later sheets use plain "(D)/(R)/(L)/(I)" or a " - D"/" - R" dash suffix. Handled
## generically by taking the LAST letter of whatever trailing code is found and mapping D->DEMOCRAT,
## R->REPUBLICAN, everything else (L, I, C, S, WI, Write-In, ...) -> OTHER.

source(file.path("R", "00_setup.R"))
library(readxl)
library(stringr)
library(readr)

XLS_PATH <- file.path(PROJECT_ROOT, "R", "data", "county_house_files", "eaushouse1980-2012_0.xls")

## ---- Alabama county reference (67 counties) ----
cw <- read_delim(file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
                  delim = "\t", show_col_types = FALSE)
al_counties <- cw %>% filter(toupper(state) == "ALABAMA") %>%
  distinct(county_name, county_fips) %>%
  mutate(county_name = toupper(county_name))
al_county_names <- al_counties$county_name

## Known misspellings/variants observed directly in the source file, mapped to the crosswalk's
## canonical name (verified per-instance while inspecting the sheets, not guessed).
county_alias <- c(
  "ESCAMBHIA" = "ESCAMBIA", "CLEBURGE" = "CLEBURNE", "MORTGAN" = "MORGAN",
  "COCTAW" = "CHOCTAW", "DEKALB" = "DEKALB", "ST CLAIR" = "ST. CLAIR",
  "ST. CLAIR" = "ST. CLAIR"
)

normalize_county <- function(x) {
  x <- toupper(trimws(x))
  x <- str_replace_all(x, "\\.", "")            # drop periods (St. Clair -> ST CLAIR)
  x <- str_replace_all(x, "\\s+", " ")
  x
}
al_lookup_key <- normalize_county(al_county_names)
names(al_lookup_key) <- al_county_names
alias_key <- normalize_county(names(county_alias))
names(alias_key) <- county_alias  # normalized-misspelling -> canonical name

match_county <- function(raw) {
  if (is.na(raw) || trimws(raw) == "") return(NA_character_)
  key <- normalize_county(raw)
  hit <- al_county_names[al_lookup_key == key]
  if (length(hit) == 1) return(hit)
  # try alias table (normalized misspelling -> canonical)
  alias_hit <- names(county_alias)[normalize_county(names(county_alias)) == key]
  if (length(alias_hit) >= 1) return(county_alias[[alias_hit[1]]])
  NA_character_
}

## ---- Party classification: take the LAST letter of a trailing party code ----
## Handles "(AD)"/"(AR)"/"(AL)", "(D)"/"(R)"/"(L)"/"(I)"/"(C)"/"(S)", " - D"/" - R", and bare
## "WI"/"W/I"/"Write-In"/"Write-Ins" (no code at all -> OTHER, still a real vote column).
classify_party <- function(header_text) {
  h <- trimws(header_text)
  h_upper <- toupper(h)
  if (h_upper %in% c("WI", "W/I", "WRITE-IN", "WRITE-INS", "WRITE IN", "WRITE INS")) return("OTHER")
  # explicit party-code cell (2002 sheets: DEM/LIB/REP/WI directly, no candidate name)
  if (h_upper %in% c("DEM", "DEMOCRAT", "DEMOCRATIC")) return("DEMOCRAT")
  if (h_upper %in% c("REP", "REPUBLICAN")) return("REPUBLICAN")
  if (h_upper %in% c("LIB", "LIBERTARIAN", "IND", "CON", "OTHER")) return("OTHER")
  # trailing parenthetical, e.g. "Jack Edwards (AR)" or "Bonner (R)"
  m <- str_match(h, "\\(([A-Za-z]+)\\)\\s*$")
  if (!is.na(m[1, 2])) {
    code <- toupper(m[1, 2])
    last <- substr(code, nchar(code), nchar(code))
    if (last == "D") return("DEMOCRAT")
    if (last == "R") return("REPUBLICAN")
    return("OTHER")
  }
  # trailing dash code, e.g. "Belk - D" or "Jo Bonner - R"
  m2 <- str_match(h, "-\\s*([A-Za-z]+)\\s*$")
  if (!is.na(m2[1, 2])) {
    code <- toupper(m2[1, 2])
    if (code %in% c("WRITEINS", "WRITEIN")) return("OTHER")
    last <- substr(code, nchar(code), nchar(code))
    if (last == "D") return("DEMOCRAT")
    if (last == "R") return("REPUBLICAN")
    return("OTHER")
  }
  # fallback: a parenthetical ANYWHERE in the string, not just at the very end -- catches a real
  # merged-cell artifact in "12 Gen." where a district's last candidate cell reads
  # "MO BROOKS ( R)                         WI" (the adjacent WI column's header got merged onto
  # this cell by the source spreadsheet, pushing the "(R)" code away from the string's end).
  m3 <- str_match(h, "\\(\\s*([A-Za-z])\\s*\\)")
  if (!is.na(m3[1, 2])) {
    code <- toupper(m3[1, 2])
    if (code == "D") return("DEMOCRAT")
    if (code == "R") return("REPUBLICAN")
    return("OTHER")
  }
  NA_character_  # not a recognizable candidate/party header at all
}

## Confirmed source data-entry error, not a parsing bug: "10Gen"'s header row labels incumbent
## Jo Bonner (AL-1) as "Bonner (D)" -- but he was a Republican his entire tenure (2003-2013),
## and the SAME spreadsheet's own 2004/2006/2008 sheets all correctly label him "(R)" ("Bonner -
## R"/"Bonner (R)"). Left uncorrected this would flip AL-1's 2010 party shares (Baldwin/Mobile
## counties would show ~80% demovote instead of ~80% repuvote). Applied as a targeted override
## rather than trusting the literal cell, since it's contradicted by the same source's own other
## years and by well-established public record.
known_header_corrections <- list(
  list(sheet = "10Gen", pattern = "Bonner", forced_party = "REPUBLICAN")
)
apply_known_corrections <- function(sheet, header_text, party_of_col) {
  for (fix in known_header_corrections) {
    if (fix$sheet != sheet) next
    hit <- grepl(fix$pattern, header_text, fixed = TRUE)
    if (any(hit)) {
      message("  [", sheet, "] applying known correction: '", header_text[hit][1],
              "' forced to ", fix$forced_party, " (source data-entry error, see comment)")
      party_of_col[hit] <- fix$forced_party
    }
  }
  party_of_col
}

is_skip_label <- function(x) {
  x <- toupper(trimws(x))
  x %in% c("TOTAL", "TOTALS", "TOTALS:", "CERTIFIED", "% OF TOTAL VOTE", "%OF TOTAL VOTE",
           "MARGIN", "") | is.na(x)
}

as_num <- function(x) suppressWarnings(as.numeric(x))

results <- list()  # accumulate (year, county, party, votes) tibbles

## =========================================================================================
## 1. STACKED SHEETS (1980-1998, one sheet per year, districts stacked vertically)
## =========================================================================================
stacked_sheets <- c("Gen.80" = 1980, "Gen82" = 1982, "Gen. 84" = 1984, "GEN86" = 1986,
                     "GEN88" = 1988, "GEN90" = 1990, "GEN92" = 1992, "GEN94" = 1994,
                     "GEN96" = 1996, "GEN98" = 1998)

## GEN88's district-1 and district-2 blocks have NO county-label column at all (confirmed by
## inspection) -- their county order is identical to every other 1980s-era sheet's district 1/2
## order (pre-1992 district lines held constant across this whole span).
## District 1's 6-row total (row 9 of that block) sums exactly to the 6 visible rows -- confirms
## Wilcox is genuinely absent from AL-1 in 1988 (not a bug: a later "Certified"-style row shows a
## larger, non-county-attributable total, same class of gap as Florida's un-broken-out "Fed Abs"
## row elsewhere in this project -- left unattributed rather than guessed at).
gen88_fallback <- list(
  `1` = c("Baldwin", "Clarke", "Escambia", "Mobile", "Monroe", "Washington"),
  `2` = c("Barbour", "Bullock", "Butler", "Coffee", "Conecuh", "Covington",
          "Crenshaw", "Dale", "Geneva", "Henry", "Houston", "Montgomery", "Pike")
)

## Matches "1st Congressional District", "4th Congressional" (GEN88 drops the word "District" in
## one instance), etc. -- broadened after finding GEN88's exact-phrase match failed silently.
district_header_re <- "\\d+(st|nd|rd|th)\\s*Congressional"

parse_stacked_sheet <- function(sheet, year) {
  d <- suppressMessages(read_excel(XLS_PATH, sheet = sheet, col_names = FALSE, col_types = "text"))
  nr <- nrow(d); nc <- ncol(d)
  m <- as.matrix(d)

  # Find every (row, col) cell that looks like a district header -- a single row can hold MORE
  # THAN ONE (confirmed: Gen.80 row 37 has "3rd Congressional District" AND "4th  Congressional
  # District" side by side) -- so blocks are (row, col) rectangles, not just row ranges.
  hits <- which(apply(m, c(1, 2), function(x) !is.na(x) && grepl(district_header_re, x, ignore.case = TRUE)), arr.ind = TRUE)
  hits <- hits[order(hits[, "row"], hits[, "col"]), , drop = FALSE]
  distinct_start_rows <- sort(unique(hits[, "row"]))

  # A block's own county-label column sits ONE COLUMN BEFORE its header-hit column (confirmed:
  # Gen.80 row 37's 2nd header hit "4th Congressional District" is at col 5, but that district's
  # actual county names ("Blount", ...) are in col 4) -- so col_start = hit_col - 1, except the
  # FIRST block in a row, which reaches back to column 1 (nothing precedes it). col_end for block
  # i is (next hit's column - 2), to also exclude the NEXT block's own label column.
  block_defs <- list()
  for (r in distinct_start_rows) {
    same_row_hits <- sort(hits[hits[, "row"] == r, "col"])
    next_start_row <- distinct_start_rows[distinct_start_rows > r]
    row_end <- if (length(next_start_row) > 0) min(next_start_row) - 1 else nr
    for (i in seq_along(same_row_hits)) {
      col_start <- if (i == 1) 1 else same_row_hits[i] - 1
      col_end <- if (i < length(same_row_hits)) same_row_hits[i + 1] - 2 else nc
      block_defs[[length(block_defs) + 1]] <- list(row_start = r, row_end = row_end, col_start = col_start, col_end = col_end)
    }
  }

  out <- list()
  gen88_block_idx <- 0

  for (bi in seq_along(block_defs)) {
    bd <- block_defs[[bi]]
    block <- m[bd$row_start:bd$row_end, bd$col_start:bd$col_end, drop = FALSE]
    if (nrow(block) < 2) next

    # find the first data row: leading-ish cell matches a county name, OR (GEN88 fallback) a row
    # that is all-numeric with no recognizable county cell anywhere in the block.
    row_is_county <- function(r) {
      for (cell in r) { if (!is.na(cell) && !is.na(match_county(cell))) return(TRUE) }
      FALSE
    }
    data_start <- NA
    for (ri in 2:nrow(block)) {
      if (row_is_county(block[ri, ])) { data_start <- ri; break }
    }

    no_labels <- is.na(data_start)
    if (no_labels) {
      # fall back to position-based county list (GEN88 districts 1 & 2 only, confirmed by inspection)
      gen88_block_idx <- gen88_block_idx + 1
      if (sheet != "GEN88" || is.null(gen88_fallback[[as.character(gen88_block_idx)]])) {
        message("  [", sheet, "] block ", bi, ": no county labels found and no fallback available -- skipping")
        next
      }
      counties <- gen88_fallback[[as.character(gen88_block_idx)]]
      # first numeric row after header(s): first row (from 2) where any cell is a parseable number
      for (ri in 2:nrow(block)) {
        if (any(!is.na(as_num(block[ri, ])))) { data_start <- ri; break }
      }
      header_text <- apply(block[2:(data_start - 1), , drop = FALSE], 2,
                            function(col) paste(col[!is.na(col)], collapse = " "))
      label_col <- NA  # no label column; counties assigned positionally below
    } else {
      header_text <- apply(block[2:(data_start - 1), , drop = FALSE], 2,
                            function(col) paste(col[!is.na(col)], collapse = " "))
      label_col <- NA
    }

    party_of_col <- sapply(header_text, classify_party)

    # data rows: from data_start until a blank row or end of block. A blank row does NOT always
    # mean the block truly ends here -- GEN94's district 6 has NO "Congressional District" header
    # of its own at all (confirmed: only a blank row separates it from district 5's block above),
    # so its Fortenberry(D)/Bachus(R)/county rows would otherwise be silently dropped entirely.
    # If the row right after a blank row looks like a fresh candidate/party header (>=1 recognizable
    # party cell), treat it as a nested, unlabeled sub-block: reset party_of_col from it and keep going.
    ri <- data_start
    county_i <- 0
    while (ri <= nrow(block)) {
      row <- block[ri, ]
      if (all(is.na(row) | trimws(row) == "")) {
        nxt <- ri + 1
        if (nxt <= nrow(block)) {
          nxt_row <- block[nxt, ]
          nxt_hits <- sum(!is.na(sapply(nxt_row, classify_party)))
          if (nxt_hits >= 1 && !no_labels) {
            message("  [", sheet, "] block ", bi, ": unlabeled nested sub-block found after row ",
                    ri, ", continuing (new header: ", paste(nxt_row[!is.na(nxt_row)], collapse = " | "), ")")
            party_of_col <- sapply(nxt_row, classify_party)
            ri <- nxt + 1
            next
          }
        }
        break
      }

      if (no_labels) {
        county_i <- county_i + 1
        if (county_i > length(counties)) { ri <- ri + 1; next }
        county <- toupper(counties[county_i])  # match_county() always returns UPPER CASE; must match
        val_cells <- seq_along(row)
      } else {
        # find which column holds the county name (search columns until a match)
        found_col <- NA; county <- NA
        for (ci in seq_along(row)) {
          cand <- match_county(row[ci])
          if (!is.na(cand)) { found_col <- ci; county <- cand; break }
        }
        if (is.na(found_col)) {
          # not a county row -- either a Total/Certified/% row (skip) or genuinely unrecognized
          lbl <- trimws(row[1])
          if (!is_skip_label(lbl)) {
            # try any cell as a label for skip-detection
            if (!any(sapply(row, is_skip_label) & !is.na(row))) {
              message("  [", sheet, "] block ", bi, " row ", ri, ": unrecognized row, skipping: ",
                      paste(row[!is.na(row)], collapse = " | "))
            }
          }
          ri <- ri + 1; next
        }
        val_cells <- setdiff(seq_along(row), found_col)
      }

      for (ci in val_cells) {
        if (ci > length(party_of_col) || is.na(party_of_col[ci])) next
        v <- as_num(row[ci])
        if (is.na(v)) next
        out[[length(out) + 1]] <- tibble(year = year, county = county, party = party_of_col[ci], votes = v)
      }
      ri <- ri + 1
    }
  }
  if (length(out) == 0) return(tibble(year = integer(), county = character(), party = character(), votes = numeric()))
  bind_rows(out)
}

for (s in names(stacked_sheets)) {
  message("Parsing stacked sheet: ", s, " (", stacked_sheets[[s]], ")")
  results[[s]] <- parse_stacked_sheet(s, stacked_sheets[[s]])
}

## =========================================================================================
## 2. WIDE MATRIX SHEETS (2000, 2004, 2006, 2008, 2010): district blocks as column ranges,
##    all counties as rows once, county name in column 1 or 2 (an optional "County ID" leading
##    column is dropped by detecting it's not text-matchable to a county name).
## =========================================================================================
wide_sheets <- c("GEN00" = 2000, "2004 General" = 2004, "06Gen" = 2006, "08Gen" = 2008, "10Gen" = 2010)

parse_wide_sheet <- function(sheet, year) {
  d <- suppressMessages(read_excel(XLS_PATH, sheet = sheet, col_names = FALSE, col_types = "text"))
  m <- as.matrix(d)
  nr <- nrow(m); nc <- ncol(m)

  # find the county-label column: whichever column has the most matches against al_county_names
  match_counts <- sapply(seq_len(nc), function(ci) sum(!is.na(sapply(m[, ci], match_county))))
  county_col <- which.max(match_counts)

  # find header row(s): the row with district labels (row 1) and the row with candidate/party
  # labels (first row below it containing a classify_party() hit)
  header_row <- NA
  for (ri in 1:min(4, nr)) {
    hits <- sum(!is.na(sapply(m[ri, ], classify_party)))
    if (hits >= 2) { header_row <- ri; break }
  }
  if (is.na(header_row)) stop("could not find header row in ", sheet)

  party_of_col <- sapply(m[header_row, ], classify_party)
  party_of_col <- apply_known_corrections(sheet, m[header_row, ], party_of_col)

  out <- list()
  for (ri in (header_row + 1):nr) {
    row <- m[ri, ]
    lbl <- row[county_col]
    if (is.na(lbl) || trimws(lbl) == "") next
    county <- match_county(lbl)
    if (is.na(county)) {
      if (!is_skip_label(lbl)) {
        message("  [", sheet, "] row ", ri, ": unrecognized county label '", lbl, "', skipping")
      }
      next
    }
    for (ci in seq_len(nc)) {
      if (ci == county_col) next
      if (is.na(party_of_col[ci])) next
      v <- as_num(row[ci])
      if (is.na(v)) next
      out[[length(out) + 1]] <- tibble(year = year, county = county, party = party_of_col[ci], votes = v)
    }
  }
  bind_rows(out)
}

for (s in names(wide_sheets)) {
  message("Parsing wide sheet: ", s, " (", wide_sheets[[s]], ")")
  results[[s]] <- parse_wide_sheet(s, wide_sheets[[s]])
}

## =========================================================================================
## 3. 2002: split across 4 sheets, each with a clean 3-row header (district / party code /
##    candidate name) -- party comes directly from row 2, no suffix parsing needed.
## =========================================================================================
parse_2002_sheet <- function(sheet) {
  d <- suppressMessages(read_excel(XLS_PATH, sheet = sheet, col_names = FALSE, col_types = "text"))
  m <- as.matrix(d)
  nr <- nrow(m); nc <- ncol(m)
  party_row <- m[2, ]
  party_of_col <- sapply(party_row, function(x) {
    xu <- toupper(trimws(x))
    if (is.na(x) || xu == "") return(NA_character_)
    if (xu == "DEM") return("DEMOCRAT")
    if (xu == "REP") return("REPUBLICAN")
    return("OTHER")  # LIB, WI, etc.
  })
  out <- list()
  for (ri in 4:nr) {
    row <- m[ri, ]
    lbl <- row[1]
    if (is.na(lbl) || trimws(lbl) == "") next
    county <- match_county(lbl)
    if (is.na(county)) {
      if (!is_skip_label(lbl)) message("  [", sheet, "] row ", ri, ": unrecognized county '", lbl, "'")
      next
    }
    for (ci in 2:nc) {
      if (is.na(party_of_col[ci])) next
      v <- as_num(row[ci])
      if (is.na(v)) next
      out[[length(out) + 1]] <- tibble(year = 2002, county = county, party = party_of_col[ci], votes = v)
    }
  }
  bind_rows(out)
}

message("Parsing 2002 (4 sheets)...")
results[["2002"]] <- bind_rows(
  parse_2002_sheet("Gen.02.Hse.1&2"),
  parse_2002_sheet("Hse.3&4"),
  parse_2002_sheet("Hse.5&6"),
  parse_2002_sheet("Hse.7")
)

## =========================================================================================
## 4. 2012: "12 Gen." -- 2x2 grid of district blocks, each with its own "County" header + a
##    "Totals:" row (used here for verification, not as the value source).
## =========================================================================================
parse_2012 <- function() {
  d <- suppressMessages(read_excel(XLS_PATH, sheet = "12 Gen.", col_names = FALSE, col_types = "text"))
  m <- as.matrix(d)
  nr <- nrow(m); nc <- ncol(m)

  block_starts <- which(apply(m, 1, function(r) any(grepl("FOR UNITED STATES REPRESENTATIVE", r, ignore.case = TRUE))))
  out <- list()
  validation <- list()

  for (bi in seq_along(block_starts)) {
    hdr_row <- block_starts[bi]
    # each block's title cell also marks its starting column
    title_col <- which(grepl("FOR UNITED STATES REPRESENTATIVE", m[hdr_row, ], ignore.case = TRUE))
    for (tc in title_col) {
      # block spans from tc to just before the next title column on the same row (or end)
      same_row_titles <- title_col[title_col > tc]
      end_col <- if (length(same_row_titles) > 0) min(same_row_titles) - 1 else nc
      cand_row <- hdr_row + 1
      county_hdr_row <- hdr_row + 2
      # find "County" cell to confirm block start column, and "Totals:" rows below to bound the block
      block_cols <- tc:end_col
      # candidate/party header text per column (row cand_row)
      party_of_col <- sapply(m[cand_row, block_cols], classify_party)
      names(party_of_col) <- block_cols
      # data rows: from county_hdr_row+1 until a blank county cell or "Totals:"
      ri <- county_hdr_row + 1
      while (ri <= nr) {
        lbl <- m[ri, tc]
        if (is.na(lbl) || trimws(lbl) == "") break
        if (toupper(trimws(lbl)) %in% c("TOTALS:", "TOTAL:", "TOTALS", "TOTAL")) {
          # validation: compare this row's values to the running sum (computed after loop) -- store raw for now
          validation[[length(validation) + 1]] <- list(block = bi, tc = tc, row = as_num(m[ri, block_cols]))
          break
        }
        county <- match_county(lbl)
        if (is.na(county)) { ri <- ri + 1; next }
        for (ci in block_cols) {
          if (ci == tc) next
          pc <- party_of_col[as.character(ci)]
          if (is.na(pc)) next
          v <- as_num(m[ri, ci])
          if (is.na(v)) next
          out[[length(out) + 1]] <- tibble(year = 2012, county = county, party = pc, votes = v)
        }
        ri <- ri + 1
      }
    }
  }
  list(data = bind_rows(out), validation = validation)
}

message("Parsing 2012 (12 Gen.)...")
res2012 <- parse_2012()
results[["2012"]] <- res2012$data

## =========================================================================================
## Combine, aggregate, join FIPS
## =========================================================================================
all_raw <- bind_rows(results) %>% filter(!is.na(county), !is.na(party), !is.na(votes))
message("Total raw (year, county, party, votes) rows parsed: ", nrow(all_raw))
message("Years present: ", paste(sort(unique(all_raw$year)), collapse = ", "))

agg <- all_raw %>%
  group_by(year, county) %>%
  summarise(
    demovote_n = sum(votes[party == "DEMOCRAT"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REPUBLICAN"], na.rm = TRUE),
    totalvote  = sum(votes, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(al_counties, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  mutate(demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, sample = "HE") %>%
  select(year, cty_fips = county_fips, sample, demovote, repuvote, totalvote)

message("Final (year, county) rows: ", nrow(agg))
print(agg %>% count(year) %>% arrange(year))

## ---- Sanity checks ----
message("\nSanity range demovote+repuvote: [",
        round(min(agg$demovote + agg$repuvote), 3), ", ",
        round(max(agg$demovote + agg$repuvote), 3), "]")

## Known real races, checked directly (Alabama's own 2016 MEDSL data is unreliable -- see project
## notes -- so validate against known history instead of a MEDSL cross-check):
##  - 1980 AL-1: Jack Edwards (R, incumbent) beat Steve Smith (Libertarian) -- AL-1 counties should
##    show near-0 demovote (no Democrat on the ballot that year).
al1_1980 <- agg %>% filter(year == 1980, cty_fips %in% c(1003, 1097))  # Baldwin, Mobile
message("\n1980 AL-1 (Baldwin, Mobile) demovote (expect ~0, no Democrat on ballot):")
print(al1_1980)

saveRDS(agg, file.path(PROJECT_ROOT, "R", "output", "elect_he_cty_al_historical.rds"))
message("\nSaved R/output/elect_he_cty_al_historical.rds")
