## *** SUPERSEDED 2026-09-20 by 02c_california_1998_2000_rebuild.R. This prefix-match parse counted Reform (Rfm) columns as Republican (e.g. 1998 CA-21 Thomas 115,989 + Evans Rfm 30,994 = 146,983 stored as Republican) and mis-parsed 2000 CA-16.
## Do not use its output; re-running it (or 02w_house_long_california_historical.R) reproduces the errors -- re-run 02c afterwards. ***
##
## California 1990-2000: closes CA's only remaining House-data gap (CA is otherwise full 2002-2024
## via OpenElections, 01q). Source: California Secretary of State's official "Statement of Vote"
## PDFs, manually downloaded by the user into R/data/county_house_files/ (the established manual
## drop-folder pattern for sites too old/awkward to script against). Six PDFs exist (1990/92/94/96/
## 98/2000) but they are NOT uniform quality -- checked each one's actual per-district result tables
## (not just whether pdftotext "succeeds") before committing to a parsing strategy:
##
## - **1998 and 2000: clean, born-digital text.** pdftotext -layout extraction is fully readable,
##   digits and labels both clean. Parsed here.
## - **1994: born-digital but with real OCR-style character corruption in TEXT labels** (candidate
##   names and party codes are frequently garbled -- "Dem" prints as "Dan"/"Dem-1Dc" depending on
##   district, "Rep-Inc" as "Rep-1Dc") -- BUT the vote NUMBERS themselves are clean and internally
##   consistent (checked: every district's county rows sum to exactly its own printed "District
##   Totals" row). Since no minor party name starts with "D" or "R" in any CA ballot this era
##   (checked the full distinct code list across all three clean years), a broad prefix match
##   (`^D`/`^R`) on the garbled party-code row safely identifies the Democratic/Republican columns
##   even when the text itself is corrupted. Parsed here using that prefix rule.
## - **1990 and 1992: REJECTED for now.** Same visual OCR garbling as 1994's labels, but here the
##   VOTE NUMBERS themselves are also corrupted (spot-checked: 1992's 2nd Congressional District
##   county rows sum to 73,800 for the Dem column against a printed "District Totals" of 71,780 --
##   a real ~2,000-vote discrepancy, not a rounding/formatting artifact). Confirmed the corruption
##   is in the digits, not just cosmetic character-substitution in text labels the way 1994's is.
##   Automated extraction from this text layer would silently produce wrong vote counts. Would need
##   the same treatment as Kentucky's 2010 scanned PDF (render pages as images, transcribe by hand,
##   verify against each district's own printed "District Totals" row) -- a much larger manual
##   effort (82 + 84 = 166 pages) not undertaken in this pass.
## - **1996: REJECTED, essentially no text layer at all.** pdftotext yields ~145 chars/page average
##   across 64 pages (62 of 64 pages near-empty) -- this PDF is image-only with no usable embedded
##   text, unlike the other five. Would need `ocrmypdf --force-ocr` + manual page-image transcription
##   from scratch (no OCR baseline to even locate sections from), the heaviest lift of the six.
##
## Table structure (1994/1998/2000, all three parsed years): each of CA's congressional districts
## gets its own block headed "Nth Congressional District". Below that, 2-3 lines of candidate names
## (sometimes wrapped across lines, not parsed -- not needed), then ONE party-code row (e.g. "Dem
## Rep-Inc Lib P&F Write-in"), then one row per county in that district, then a "District Totals"
## row and a "Percent" row. Critically, the LAST candidate-name column ("Votes not / Cast in Race")
## has NO corresponding party code in the party-code row -- so a county row always has exactly
## (n_party_codes + 1) numbers: the first n_party_codes are real candidate votes in the same column
## order as the party-code row, and the trailing extra number is uncast/blank ballots, which is
## real people but NOT a candidate vote -- confirmed by reproducing each district's own printed
## "Percent" row (percentages are of real candidate votes only, EXCLUDING this trailing column: e.g.
## 1998 CD-1's Thompson share prints as 61.85%, which only matches 121,710 / 196,769 -- the sum of
## the 5 real candidate columns -- not 121,710 / 207,789, which would include the "votes not cast"
## column). Excluded from totalvote here for the same reason.

source(file.path("R", "00_setup.R"))
library(stringr)

PDF_DIR <- file.path(PROJECT_ROOT, "R", "data", "county_house_files")
TXT_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "california_historical")
dir.create(TXT_DIR, showWarnings = FALSE, recursive = TRUE)

county_fips_crosswalk <- read.delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  sep = "\t", stringsAsFactors = FALSE
) %>%
  distinct(state, county_name, county_fips) %>%
  mutate(county_name = toupper(trimws(county_name)))
ca_fips <- county_fips_crosswalk %>% filter(state == "CALIFORNIA") %>% select(county_name, county_fips)
stopifnot(nrow(ca_fips) == 58)
## Longest names first, so a greedy start-of-line match doesn't stop early (e.g. "SAN" alone
## before checking "SAN BERNARDINO").
ca_county_names <- ca_fips$county_name[order(-nchar(ca_fips$county_name))]

pdf_to_text <- function(year, pdf_name) {
  txt_path <- file.path(TXT_DIR, paste0(year, ".txt"))
  if (!file.exists(txt_path)) {
    system2("pdftotext", c("-layout", shQuote(file.path(PDF_DIR, pdf_name)), shQuote(txt_path)))
  }
  readLines(txt_path, warn = FALSE)
}

num <- function(x) as.numeric(gsub("[,\\s]", "", x))

## Splits the full text into per-district blocks, each running from one "Nth Congressional
## District" header to just before the next (or EOF). Only the text AFTER the first such header is
## used -- everything before it (preface, other offices) is irrelevant.
##
## Both 1998 and 2000's prefaces include a fully-formed WORKED EXAMPLE of a real district's table
## (verbatim CD-6 data, Marin/Sonoma) to illustrate how to read the report -- a naive scan for "Nth
## Congressional District" headers finds this too and, since it's real, correctly-formatted data,
## it parses and validates cleanly just like the genuine results table later in the document,
## silently double-counting that one district's votes. Fixed by anchoring the search to start only
## after the actual "Representative(s) in Congress" SECTION header (found by excluding the table-
## of-contents line, which has the same phrase but with a "...### 13"-style dot-leader page ref).
split_district_blocks <- function(lines) {
  section_idx <- grep("(?i)Representatives?\\s+in\\s+Congress", lines, perl = TRUE)
  section_idx <- section_idx[!str_detect(lines[section_idx], "\\.\\.\\.\\.|\\.{3,}")]
  stopifnot(length(section_idx) > 0)
  start_at <- min(section_idx)

  header_idx <- grep("(?i)^\\s*\\d+(st|nd|rd|th)\\s+Congressional District\\s*$", lines, perl = TRUE)
  header_idx <- header_idx[header_idx >= start_at]
  stopifnot(length(header_idx) > 0)
  ends <- c(header_idx[-1] - 1, length(lines))
  purrr::map2(header_idx, ends, ~ lines[.x:.y])
}

## Parses one district block into county-level (party, votes) rows, validated against that
## district's own printed "District Totals" row.
parse_district <- function(block) {
  ## Party-code row: 2-7 short alphabetic/hyphen tokens, no digits, not itself a section label.
  ## Character class includes parens/slash for write-in labels like "IND (W/I)"/"REP (W/I)".
  party_idx <- which(str_detect(block, "^\\s*([A-Za-z(][A-Za-z&/'.()-]{0,12}\\s+){1,7}[A-Za-z(][A-Za-z&/'.()-]{0,12}\\s*$") &
                        !str_detect(block, "(?i)Congressional District|County|Votes|Cast|Percent|District Total"))
  ## The party row is the first such line that has 2+ tokens AND is followed shortly by a real
  ## county-name data row -- take the first match after the header (candidate-name lines above it
  ## also match this shape, but a party-code row is always immediately followed within a few lines
  ## by a line starting with a known county name).
  party_idx <- party_idx[party_idx > 1]
  find_party_row <- function(idx) {
    for (i in idx) {
      look_ahead <- block[(i + 1):min(i + 4, length(block))]
      if (any(str_detect(toupper(str_trim(look_ahead)), paste0("^(", paste(ca_county_names, collapse = "|"), ")\\b")))) {
        return(i)
      }
    }
    NA_integer_
  }
  pidx <- find_party_row(party_idx)
  if (is.na(pidx)) return(NULL)

  ## Self-identified write-in candidates get a two-word party label ("IND (W/I)", "REP (W/I)") --
  ## a naive whitespace split turns that into two tokens, inflating the apparent column count past
  ## the real number of vote columns and making every county row in the district look short by one
  ## field (silently failing to parse 4 of 2000's 52 districts before this fix, all of them ones
  ## with a write-in candidate). Fixed by merging any "(...)"-only token into the token before it.
  raw_tokens <- str_split(str_trim(block[pidx]), "\\s+")[[1]]
  party_tokens <- character(0)
  for (tok in raw_tokens) {
    if (str_detect(tok, "^\\(") && length(party_tokens) > 0) {
      party_tokens[length(party_tokens)] <- paste(party_tokens[length(party_tokens)], tok)
    } else {
      party_tokens <- c(party_tokens, tok)
    }
  }
  n_party <- length(party_tokens)
  ## No minor-party code in this era's CA ballots starts with D or R (checked the full distinct
  ## code list across 1994/1998/2000: Dem, Rep, Lib, P&F, NL, REF, GRN, AI, Write-in, and OCR
  ## variants of these) -- a bare prefix match safely separates DEM/REP even when 1994's labels
  ## are character-garbled ("Dan" for "Dem", "Rep-1Dc" for "Rep-Inc" both still match their prefix).
  party_std <- case_when(
    str_detect(toupper(party_tokens), "^D") ~ "DEM",
    str_detect(toupper(party_tokens), "^R") ~ "REP",
    TRUE ~ "OTHER"
  )

  data_lines <- block[(pidx + 1):length(block)]
  ## Single-county districts (e.g. Sacramento alone in CD-5 in 1998) print NO separate "District
  ## Totals" line at all -- the one county row IS the total. An earlier version required a totals
  ## line to exist and silently dropped every such district (about half of 1998's districts turned
  ## out to be this shape) -- fixed by only using the totals line to VALIDATE when one exists,
  ## rather than requiring it to exist before parsing any county rows at all.
  totals_idx <- which(str_detect(data_lines, "(?i)District Total"))[1]
  county_rows <- if (is.na(totals_idx)) data_lines else data_lines[seq_len(totals_idx - 1)]

  ## Returns a length-n_party numeric vector (the row's first n_party numbers) or NULL if the line
  ## isn't a real county data row / doesn't have enough numbers.
  read_row <- function(line, expect_n) {
    line_u <- toupper(str_trim(line))
    hit <- ca_county_names[str_detect(line_u, paste0("^", ca_county_names, "\\b"))]
    if (length(hit) == 0) return(NULL)
    county <- hit[which.max(nchar(hit))]
    rest <- str_trim(str_remove(line_u, paste0("^", county)))
    nums <- num(str_extract_all(rest, "[0-9][0-9,]*\\.?[0-9]*")[[1]])
    nums <- nums[!is.na(nums)]
    if (length(nums) < expect_n) return(NULL)
    list(county = county, votes = nums[seq_len(expect_n)])
  }

  parsed <- purrr::compact(lapply(county_rows, read_row, expect_n = n_party))
  if (length(parsed) == 0) return(NULL)

  votes_mat <- do.call(rbind, lapply(parsed, `[[`, "votes"))
  counties <- vapply(parsed, `[[`, character(1), "county")

  ## Validate column sums against the printed District Totals row (first n_party numbers on that
  ## line) whenever one exists -- catches both parsing mistakes and (for 1990/1992-style corrupted
  ## source digits, not used here but the same check would flag them) genuine OCR errors. No totals
  ## line at all (single-county district) means nothing to validate against -- accept as-is.
  if (!is.na(totals_idx)) {
    totals_nums <- num(str_extract_all(data_lines[totals_idx], "[0-9][0-9,]*\\.?[0-9]*")[[1]])
    totals_nums <- totals_nums[!is.na(totals_nums)][seq_len(n_party)]
    col_sums <- colSums(votes_mat)
    if (any(is.na(totals_nums)) || !all(abs(col_sums - totals_nums) < 1)) {
      warning("District Totals mismatch: computed ", paste(col_sums, collapse = ","),
              " vs printed ", paste(totals_nums, collapse = ","))
      return(NULL)
    }
  }

  tibble(
    county = counties,
    demovote_n = as.numeric(votes_mat %*% (party_std == "DEM")),
    repuvote_n = as.numeric(votes_mat %*% (party_std == "REP")),
    totalvote = rowSums(votes_mat)
  )
}

parse_year <- function(year, pdf_name) {
  lines <- pdf_to_text(year, pdf_name)
  blocks <- split_district_blocks(lines)
  message(year, ": ", length(blocks), " district blocks found")
  district_results <- purrr::compact(lapply(blocks, parse_district))
  message(year, ": ", length(district_results), " districts parsed and validated against printed totals")

  bind_rows(district_results) %>%
    group_by(county) %>%
    summarise(demovote_n = sum(demovote_n), repuvote_n = sum(repuvote_n), totalvote = sum(totalvote), .groups = "drop") %>%
    left_join(ca_fips, by = c("county" = "county_name")) %>%
    filter(!is.na(county_fips), totalvote > 0) %>%
    transmute(
      state = "CALIFORNIA", year = year, cty_fips = county_fips, sample = "HE",
      demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
    )
}

message("Parsing California Statement of Vote PDFs: 1998, 2000 (1990/1992/1994/1996 excluded, see header comment)...")
elect_he_cty_ca_historical <- bind_rows(
  parse_year(1998, "CA1998-sov1998-general.pdf"),
  parse_year(2000, "CA2000-sov-complete.pdf")
) %>%
  save_step("elect_he_cty_ca_historical")

message("CA historical House county-level rows built: ", nrow(elect_he_cty_ca_historical), " (of possible ", 58 * 3, ")")
print(table(elect_he_cty_ca_historical$year))

sanity <- elect_he_cty_ca_historical$repuvote + elect_he_cty_ca_historical$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")
stopifnot(all(sanity >= 0 & sanity <= 1.001))

low_share <- elect_he_cty_ca_historical %>% filter(demovote + repuvote < 0.5) %>% arrange(year)
message(nrow(low_share), " county-years with demovote+repuvote < 0.5:")
print(low_share)

for (yr in sort(unique(elect_he_cty_ca_historical$year))) {
  present <- elect_he_cty_ca_historical %>% filter(year == yr) %>% pull(cty_fips)
  missing <- ca_fips %>% filter(!county_fips %in% present)
  if (nrow(missing) > 0) {
    message(yr, ": missing ", nrow(missing), " counties: ", paste(missing$county_name, collapse = ", "))
  }
}

message("Done. NOTE: deliberately NOT folded into elect_cty_final.rds and house_results_coverage.csv")
message("NOT re-run here -- fold-in / tracker-refresh done separately after review.")
