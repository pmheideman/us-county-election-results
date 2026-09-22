## *** SUPERSEDED 2026-09-20 by 02c_california_1998_2000_rebuild.R (strict party codes: Reform is OTHER; every candidate column ties to the printed District Totals). Re-running this script reproduces the OLD errors. ***
##
## Long table for California Statement-of-Vote PDFs 1998 & 2000 (01bd). Reuses 01bd's district-block parsing and totals validation, but keeps each
## candidate's county votes and reads candidate names from the 1-2 header lines above the party row by column position.
source(file.path("R", "data_creation", "02w_common.R"))
library(stringr)
TXT_DIR <- file.path(RAW_ROOT, "california_historical"); ca_fips <- fips_of("CALIFORNIA"); stopifnot(nrow(ca_fips) == 58)
ca_county_names <- ca_fips$county_name[order(-nchar(ca_fips$county_name))]
num <- function(x) as.numeric(gsub("[,\\s]", "", x))

split_district_blocks <- function(lines) {
  section_idx <- grep("(?i)Representatives?\\s+in\\s+Congress", lines, perl = TRUE)
  section_idx <- section_idx[!str_detect(lines[section_idx], "\\.\\.\\.\\.|\\.{3,}")]
  start_at <- min(section_idx)
  header_idx <- grep("(?i)^\\s*\\d+(st|nd|rd|th)\\s+Congressional District\\s*$", lines, perl = TRUE)
  header_idx <- header_idx[header_idx >= start_at]
  ends <- c(header_idx[-1] - 1, length(lines))
  purrr::map2(header_idx, ends, ~ lines[.x:.y])
}

## words of a line with their center column
words_pos <- function(line) { m <- gregexpr("\\S+", line)[[1]]; if (m[1] == -1) return(tibble(w = character(), x = numeric()))
  tibble(w = regmatches(line, list(m))[[1]], x = as.numeric(m) + (attr(m, "match.length") - 1) / 2) }

## 2000 only: the statewide summary lists every candidate whole ("Wally Herger, REP  168,172  65.8%") in a two-column page layout. Parse each half of
## the page as its own stream, tracking the current district from "United States Representative District N" headers.
listing_2000 <- function() {
  L <- readLines(file.path(TXT_DIR, "2000.txt"), warn = FALSE)
  hdr <- grep("United States Representative District [0-9]+", L); rng <- L[min(hdr):(max(hdr) + 9)]
  ent <- "^\\s*(.+?),\\s+([A-Z]{2,4}(?:\\s*\\((?:W/I|w/i)\\))?)\\s+([0-9][0-9,]*)\\s+[0-9.]+%"
  out <- list()
  for (side in c("left", "right")) {
    cur <- NA_integer_
    for (ln in rng) {
      pos <- gregexpr("United States Representative District [0-9]+", ln)[[1]]
      cut <- if (pos[1] != -1 && length(pos) > 1) pos[2] else if (pos[1] != -1 && pos[1] > 40) pos[1] else NA
      # split the line into halves at the second header position, else at column 66
      cs <- if (!is.na(cut)) cut else 66
      part <- if (side == "left") substr(ln, 1, cs - 1) else substr(ln, cs, nchar(ln))
      h <- str_match(part, "United States Representative District ([0-9]+)"); if (!is.na(h[1, 2])) cur <- as.integer(h[1, 2])
      m <- str_match(part, ent)
      if (!is.na(m[1, 1]) && !is.na(cur) && !str_detect(part, "Votes Not Cast")) out[[length(out) + 1]] <- tibble(district = cur, name = str_trim(m[1, 2]), party = str_trim(m[1, 3]), votes = num(m[1, 4]))
    }
  }
  bind_rows(out)
}
LIST2000 <- listing_2000()
match_names <- function(dnum, totals, tokens, fallback) {
  cand <- LIST2000 %>% filter(district == dnum); used <- rep(FALSE, nrow(cand)); out <- fallback
  for (j in seq_along(totals)) {
    ok <- which(!used & abs(cand$votes - totals[j]) < 1)
    if (length(ok) > 1) { ok2 <- ok[toupper(gsub("\\s+", "", cand$party[ok])) == toupper(gsub("\\s+", "", tokens[j]))]; if (length(ok2)) ok <- ok2 }
    if (length(ok) >= 1) { out[j] <- cand$name[ok[1]]; used[ok[1]] <- TRUE }
  }
  out
}

PICK <- Sys.getenv("CA_PICK", "first")   # "first" = 01bd logic (party row = FIRST candidate row with county rows below); "last" was a debugging alternative that double counts (picked a later header-like row and mixed blocks)
parse_district <- function(block, year) {
  dnum <- as.integer(str_match(block[1], "(?i)(\\d+)(st|nd|rd|th)\\s+Congressional")[, 2])
  party_idx <- which(str_detect(block, "^\\s*([A-Za-z(][A-Za-z&/'.()-]{0,12}\\s+){1,7}[A-Za-z(][A-Za-z&/'.()-]{0,12}\\s*$") &
                       !str_detect(block, "(?i)Congressional District|County|Votes|Cast|Percent|District Total"))
  party_idx <- party_idx[party_idx > 1]
  find_party_row <- function(idx) { hit <- integer(0); for (i in idx) { look <- block[(i + 1):min(i + 4, length(block))]
    if (any(str_detect(toupper(str_trim(look)), paste0("^(", paste(ca_county_names, collapse = "|"), ")\\b")))) hit <- c(hit, i) }
    if (!length(hit)) NA_integer_ else if (PICK == "first") hit[1] else hit[length(hit)] }
  pidx <- find_party_row(party_idx); if (is.na(pidx)) return(NULL)
  ## party tokens with positions (merge tokens starting with "(" into the previous one, as 01bd does)
  wp <- words_pos(block[pidx]); tok <- wp$w; xs <- wp$x; party_tokens <- character(0); party_x <- numeric(0)
  for (i in seq_along(tok)) {
    if (str_detect(tok[i], "^\\(") && length(party_tokens) > 0) { party_tokens[length(party_tokens)] <- paste(party_tokens[length(party_tokens)], tok[i]); party_x[length(party_x)] <- mean(c(party_x[length(party_x)], xs[i])) }
    else { party_tokens <- c(party_tokens, tok[i]); party_x <- c(party_x, xs[i]) } }
  n_party <- length(party_tokens)
  party_std <- case_when(str_detect(toupper(party_tokens), "^D") ~ "DEM", str_detect(toupper(party_tokens), "^R") ~ "REP", TRUE ~ "OTHER")
  ## candidate names: non-blank lines between the district header and the party row, words assigned to the nearest party column
  nl <- vapply(block[2:(pidx - 1)][nzchar(str_trim(block[2:(pidx - 1)]))], function(l) { l <- gsub("Votes not", "         ", l, fixed = TRUE); gsub("Cast in Race", "             ", l, fixed = TRUE) }, "")
  nm <- rep("", n_party)
  for (l in nl) { w <- words_pos(l); if (!nrow(w)) next
    for (k in seq_len(nrow(w))) { a <- which.min(abs(party_x - w$x[k])); nm[a] <- paste(nm[a], w$w[k]) } }
  nm <- str_trim(gsub("\\*", "", nm)); nm[!nzchar(nm)] <- paste0("Unnamed ", party_tokens[!nzchar(nm)])

  data_lines <- block[(pidx + 1):length(block)]
  totals_idx <- which(str_detect(data_lines, "(?i)District Total"))[1]
  county_rows <- if (is.na(totals_idx)) data_lines else data_lines[seq_len(totals_idx - 1)]
  read_row <- function(line, expect_n) {
    line_u <- toupper(str_trim(line)); hit <- ca_county_names[str_detect(line_u, paste0("^", ca_county_names, "\\b"))]
    if (length(hit) == 0) return(NULL); county <- hit[which.max(nchar(hit))]
    rest <- str_trim(str_remove(line_u, paste0("^", county)))
    nums <- num(str_extract_all(rest, "[0-9][0-9,]*\\.?[0-9]*")[[1]]); nums <- nums[!is.na(nums)]
    if (length(nums) < expect_n) return(NULL); list(county = county, votes = nums[seq_len(expect_n)]) }
  parsed <- purrr::compact(lapply(county_rows, read_row, expect_n = n_party)); if (length(parsed) == 0) return(NULL)
  votes_mat <- do.call(rbind, lapply(parsed, `[[`, "votes")); counties <- vapply(parsed, `[[`, character(1), "county")
  if (year == 2000) nm <- match_names(dnum, colSums(votes_mat), party_tokens, nm)
  if (!is.na(totals_idx)) {
    totals_nums <- num(str_extract_all(data_lines[totals_idx], "[0-9][0-9,]*\\.?[0-9]*")[[1]]); totals_nums <- totals_nums[!is.na(totals_nums)][seq_len(n_party)]
    if (any(is.na(totals_nums)) || !all(abs(colSums(votes_mat) - totals_nums) < 1)) return(NULL) }
  tibble(year = year, county = rep(counties, times = n_party), district = dnum, candidate = rep(nm, each = length(counties)),
         party = rep(party_tokens, each = length(counties)), party_group = rep(party_std, each = length(counties)), votes = as.vector(votes_mat))
}
long <- bind_rows(lapply(list(c(1998, "1998.txt"), c(2000, "2000.txt")), function(a) {
  lines <- readLines(file.path(TXT_DIR, a[2]), warn = FALSE); blocks <- split_district_blocks(lines)
  bind_rows(purrr::compact(lapply(blocks, parse_district, year = as.integer(a[1])))) }))
long <- long %>% inner_join(ca_fips, by = c("county" = "county_name")) %>% select(-county)
nmdf <- long %>% distinct(year, district, candidate, party) %>% arrange(year, district) %>% group_by(year, district) %>% summarise(names = paste0(candidate, " (", party, ")", collapse = "; "), .groups = "drop"); for (i in seq_len(nrow(nmdf))) message("NAMES ", nmdf$year[i], " CD", nmdf$district[i], ": ", nmdf$names[i])
long <- finalize_long(long, "ca_historical"); if (PICK == "first") save_long(long, "he_ca_historical")
message("CA historical long rows: ", nrow(long)); check_long_vs_source(long, SRC("ca_historical"))
