## REBUILD of California 1998 and 2000 U.S. House county results from the official Statement of Vote PDFs (text layer), replacing 01bd's parse.
## Why (California audit fork, 2026-09-20): 01bd matched party codes by PREFIX ("^R" -> REP), which also caught REF/Rfm (Reform Party) columns,
## so Reform candidates were counted as Republicans (e.g. 1998 CA-21: Bill Thomas Rep-Inc 115,989 + John Evans Rfm 30,994 = 146,983 was stored as
## "Republican"; same for CA-10, CA-36, CA-47 ...). It also silently dropped any district whose column sums did not tie, and picked the wrong header
## row in the 2000 CA-16 block (fixed then only by a patch, 02zz_fix_ca2000_d16.R -- now OBSOLETE, this rebuild reproduces its values directly).
## Changes vs the old parse:
##   * party row = the last non-blank line before the FIRST county row (never a name-fragment line); its tokens must be all recognised party codes.
##   * party group by EXACT code: DEM = "Dem" / "Dem-Inc" / "DEM (W/I)"; REP = "Rep" / "Rep-Inc" / "REP (W/I)"; everything else (Lib, P&F, NL, Rfm/REF, Grn, AI, IND ...) = OTHER.
##   * every district must parse AND every candidate column of the county rows must equal the printed "District Totals" row (single-county districts print
##     none: the county row is the total); any failure STOPS the script naming the district.
##   * "Votes not cast in race" (the last number on each row, no party code) is not a candidate vote and is excluded, as before.
## Candidate names: same column-position logic as 02w_house_long_california_historical.R (2000 also uses the statewide candidate listing matched by vote total).
## Outputs: R/output/long/he_ca_historical.rds, R/output/elect_he_cty_ca_historical.rds (both overwritten), R/output/ca_fixes_for_panel.rds (see below),
##          R/output/ca_1998_2000_rebuild_vs_old.csv (every county-candidate difference vs the old long table).
source(file.path("R", "data_creation", "02w_common.R"))
library(stringr)
TXT_DIR <- file.path(RAW_ROOT, "california_historical"); ca_fips <- fips_of("CALIFORNIA"); stopifnot(nrow(ca_fips) == 58)
ca_county_names <- ca_fips$county_name[order(-nchar(ca_fips$county_name))]
num <- function(x) as.numeric(gsub("[,\\s]", "", x))
old_long <- readRDS(file.path(LONG_DIR, "he_ca_historical.rds")); old_shares <- readRDS(SRC("ca_historical"))
if (!file.exists(file.path(LONG_DIR, "he_ca_historical_OLD_PARSE.rds"))) saveRDS(old_long, file.path(LONG_DIR, "he_ca_historical_OLD_PARSE.rds"))   # keep the pre-fix table for the audit trail
if (!file.exists(file.path(dirname(SRC("x")), "elect_he_cty_ca_historical_OLD_PARSE.rds"))) saveRDS(old_shares, file.path(dirname(SRC("x")), "elect_he_cty_ca_historical_OLD_PARSE.rds"))

split_district_blocks <- function(lines) {
  section_idx <- grep("(?i)Representatives?\\s+in\\s+Congress", lines, perl = TRUE)
  section_idx <- section_idx[!str_detect(lines[section_idx], "\\.\\.\\.\\.|\\.{3,}")]
  start_at <- min(section_idx)
  header_idx <- grep("(?i)^\\s*\\d+(st|nd|rd|th)\\s+Congressional District\\s*$", lines, perl = TRUE)
  header_idx <- header_idx[header_idx >= start_at]
  ends <- c(header_idx[-1] - 1, length(lines))
  purrr::map2(header_idx, ends, ~ lines[.x:.y])
}
words_pos <- function(line) { m <- gregexpr("\\S+", line)[[1]]; if (m[1] == -1) return(tibble(w = character(), x = numeric()))
  tibble(w = regmatches(line, list(m))[[1]], x = as.numeric(m) + (attr(m, "match.length") - 1) / 2) }

## 2000 statewide candidate listing (full names) -- identical to 02w's helper
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

county_re <- paste0("^(", paste(ca_county_names, collapse = "|"), ")\\b")
is_county_row <- function(l) str_detect(toupper(str_trim(l)), county_re)
## exact party-code recognition (tokens as printed in 1998/2000)
KNOWN_CODE <- "^(DEM|REP|LIB|NL|REF|RFM|GRN|GREEN|AI|IND|P&F|PF|AIP|NLP|PFP|W/I|WRITE-IN|NP)(-INC)?(\\s*\\((W/I|w/i)\\))?$"
group_of <- function(tok) { t <- toupper(tok); ifelse(str_detect(t, "^DEM(-INC)?(\\s*\\(W/I\\))?$"), "DEM", ifelse(str_detect(t, "^REP(-INC)?(\\s*\\(W/I\\))?$"), "REP", "OTHER")) }

fails <- character()
parse_district <- function(block, year) {
  dnum <- as.integer(str_match(block[1], "(?i)(\\d+)(st|nd|rd|th)\\s+Congressional")[, 2])
  first_county <- which(is_county_row(block))[1]
  if (is.na(first_county)) { fails <<- c(fails, paste(year, "CD", dnum, "no county rows")); return(NULL) }
  # party row = last non-blank line before the first county row
  cand_rows <- which(nzchar(str_trim(block[seq_len(first_county - 1)])))
  pidx <- max(cand_rows[cand_rows > 1])
  wp <- words_pos(block[pidx]); tok <- wp$w; xs <- wp$x; party_tokens <- character(0); party_x <- numeric(0)
  for (i in seq_along(tok)) {
    if (str_detect(tok[i], "^\\(") && length(party_tokens) > 0) { party_tokens[length(party_tokens)] <- paste(party_tokens[length(party_tokens)], tok[i]); party_x[length(party_x)] <- mean(c(party_x[length(party_x)], xs[i])) }
    else { party_tokens <- c(party_tokens, tok[i]); party_x <- c(party_x, xs[i]) } }
  if (!all(str_detect(toupper(party_tokens), KNOWN_CODE))) { fails <<- c(fails, paste(year, "CD", dnum, "party row not all known codes:", paste(party_tokens, collapse = " "))); return(NULL) }
  n_party <- length(party_tokens); party_std <- group_of(party_tokens)
  # candidate names from the header lines above the party row, by nearest party column
  hdr_lines <- block[2:(pidx - 1)]; nl <- vapply(hdr_lines[nzchar(str_trim(hdr_lines))], function(l) { l <- gsub("Votes not", "         ", l, fixed = TRUE); gsub("Cast in Race", "             ", l, fixed = TRUE) }, "")
  nm <- rep("", n_party)
  for (l in nl) { w <- words_pos(l); if (!nrow(w)) next; for (k in seq_len(nrow(w))) { a <- which.min(abs(party_x - w$x[k])); nm[a] <- paste(nm[a], w$w[k]) } }
  nm <- str_trim(gsub("\\*", "", nm)); nm[!nzchar(nm)] <- paste0("Unnamed ", party_tokens[!nzchar(nm)])
  data_lines <- block[(pidx + 1):length(block)]
  totals_idx <- which(str_detect(data_lines, "(?i)District Total"))[1]
  county_rows <- if (is.na(totals_idx)) data_lines else data_lines[seq_len(totals_idx - 1)]
  read_row <- function(line) {
    line_u <- toupper(str_trim(line)); hit <- ca_county_names[str_detect(line_u, paste0("^", ca_county_names, "\\b"))]
    if (length(hit) == 0) return(NULL); county <- hit[which.max(nchar(hit))]
    rest <- str_trim(str_remove(line_u, paste0("^", county))); nums <- num(str_extract_all(rest, "[0-9][0-9,]*\\.?[0-9]*")[[1]]); nums <- nums[!is.na(nums)]
    list(county = county, nums = nums) }
  parsed <- purrr::compact(lapply(county_rows, read_row))
  bad <- vapply(parsed, function(p) length(p$nums) != n_party + 1, NA)     # every county row must have n_party candidate numbers + "votes not cast"
  if (any(bad)) { fails <<- c(fails, paste(year, "CD", dnum, "county row(s) with unexpected number count:", paste(vapply(parsed[bad], `[[`, "", "county"), collapse = ","))); return(NULL) }
  votes_mat <- do.call(rbind, lapply(parsed, function(p) p$nums[seq_len(n_party)])); counties <- vapply(parsed, `[[`, character(1), "county")
  if (year == 2000) nm <- match_names(dnum, colSums(votes_mat), party_tokens, nm)
  if (!is.na(totals_idx)) {
    tn <- num(str_extract_all(data_lines[totals_idx], "[0-9][0-9,]*\\.?[0-9]*")[[1]]); tn <- tn[!is.na(tn)][seq_len(n_party)]
    if (any(is.na(tn)) || !all(abs(colSums(votes_mat) - tn) < 1)) { fails <<- c(fails, paste(year, "CD", dnum, "sums", paste(colSums(votes_mat), collapse = ","), "vs printed", paste(tn, collapse = ","))); return(NULL) }
  }
  tibble(year = year, county = rep(counties, times = n_party), district = dnum, candidate = rep(nm, each = length(counties)),
         party = rep(party_tokens, each = length(counties)), party_group = rep(party_std, each = length(counties)), votes = as.vector(votes_mat))
}
res <- lapply(list(c(1998, "1998.txt"), c(2000, "2000.txt")), function(a) {
  lines <- readLines(file.path(TXT_DIR, a[2]), warn = FALSE); blocks <- split_district_blocks(lines)
  message(a[1], ": ", length(blocks), " district blocks (expect 52)"); stopifnot(length(blocks) == 52)
  bind_rows(purrr::compact(lapply(blocks, parse_district, year = as.integer(a[1])))) })
if (length(fails)) { message("PARSE FAILURES:\n", paste(fails, collapse = "\n")); stop("districts failed to parse / tie to printed totals -- fix before overwriting anything") }
long <- bind_rows(res); stopifnot(n_distinct(long$district[long$year == 1998]) == 52, n_distinct(long$district[long$year == 2000]) == 52)
long <- long %>% inner_join(ca_fips, by = c("county" = "county_name")) %>% select(-county)
long <- finalize_long(long, "ca_historical")

## ---- differences vs the OLD long table (per county-district-candidate-party-group) -----------------------------------------------------------------
old_agg <- old_long %>% group_by(year, county_fips, district, party_group) %>% summarise(old_votes = sum(votes), .groups = "drop")
new_agg <- long %>% group_by(year, county_fips, district, party_group) %>% summarise(new_votes = sum(votes), .groups = "drop")
dif <- full_join(old_agg, new_agg, by = c("year", "county_fips", "district", "party_group")) %>% mutate(old_votes = coalesce(old_votes, 0), new_votes = coalesce(new_votes, 0), diff = new_votes - old_votes) %>% filter(diff != 0)
write.csv(dif %>% arrange(year, county_fips, district, party_group), file.path(OUTPUT_DIR, "ca_1998_2000_rebuild_vs_old.csv"), row.names = FALSE)
message("county-district-party-group cells that differ from the OLD parse: ", nrow(dif), " in ", nrow(distinct(dif, year, district)), " district-years")
print(as.data.frame(dif %>% group_by(year, party_group) %>% summarise(cells = n(), net_votes = sum(diff), .groups = "drop")))

## ---- write long + shares (state column = CALIFORNIA, as the shares files carry) --------------------------------------------------------------------
save_long(long, "he_ca_historical")
new_shares <- derive_shares(long) %>% transmute(state = "CALIFORNIA", year = as.numeric(year), cty_fips = as.integer(cty_fips), sample, demovote, repuvote, totalvote)
saveRDS(new_shares, SRC("ca_historical"))
message("wrote he_ca_historical.rds (", nrow(long), " rows) and elect_he_cty_ca_historical.rds (", nrow(new_shares), " keys)")
print(check_long_vs_source(long, SRC("ca_historical")))
