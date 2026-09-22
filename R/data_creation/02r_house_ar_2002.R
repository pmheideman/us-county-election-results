## Arkansas 2002 U.S. House, county level, from the official Arkansas Secretary of State "Certification Report, 2002 General" (Nov 5 2002),
## R/data/county_house_files/AR_2002_General.pdf (1,400 pages, real text layer, `pdftotext -layout`).
##
## Layout / gotchas:
##   * pages 1-~20 print STATEWIDE totals per contest ("U.S. Congress District 01" ... with "Name - Party  votes  pct", then Over/Under votes); these
##     are the checksum: every district's county sums must equal them exactly.
##   * then one block per county x contest: "U.S. Congress District 03 (Benton County)" followed by one line per candidate. A county that lies in
##     2+ districts has one block per district (kept by district; shares sum them). Blocks can straddle page breaks (page-header lines are ignored).
##   * candidate lines: "Congressman Marion Berry - Democrat 129,701 66.84%"; titles (Congressman, Sen., Governor, ...) are stripped. Over / Under vote
##     lines are pseudo rows and skipped. Party lines: Democrat / Republican = DEM / REP; Write-In and everything else = OTHER (counted in totals).
##   * this year all four districts (AR-1..4) were contested; a candidate line with 0 votes is kept (zero rows do not change shares).
## Supersedes the partial OpenElections 2002 rows in elect_he_cty_ar.rds (compared below; that file is not modified).
## Outputs: R/output/long/he_ar_2002.rds, R/output/elect_he_cty_ar_2002.rds (NEW files).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
PDF <- file.path(PROJECT_ROOT, "R", "data", "county_house_files", "AR_2002_General.pdf")
TXT <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "arkansas_official", "AR_2002_General.txt")
dir.create(dirname(TXT), showWarnings = FALSE, recursive = TRUE)
if (!file.exists(TXT) || file.size(TXT) == 0) system2("pdftotext", c("-layout", shQuote(PDF), shQuote(TXT)))
L <- gsub("\f", "", readLines(TXT, warn = FALSE, encoding = "UTF-8"))   # a block that starts a new page has a form-feed (^L) before its first line
CAND <- "^\\s*(.+?) - ([A-Za-z][A-Za-z -]*?)\\s+([0-9][0-9,]*)\\s+([0-9.]+)%\\s*$"
HOUSE_STATE <- "^U\\.S\\. Congress District ([0-9]{2})\\s*$"
HOUSE_CTY   <- "^U\\.S\\. Congress District ([0-9]{2}) \\((.+) County\\)\\s*$"
ANY_CTY     <- "^.+ \\(.+ County\\)\\s*$"
strip_title <- function(z) sub("^(Congressman|Congresswoman|Governor|Sen\\.|Senator|Rep\\.|Representative|State Senator|State Representative|Attorney General|Lieutenant Governor)\\s+", "", z)

parse_lines <- function(idx, header_re, county = FALSE) {
  out <- list(); cur <- NULL
  for (i in idx) {
    ln <- L[i]
    m <- regmatches(ln, regexec(header_re, ln))[[1]]
    if (length(m)) { cur <- list(district = m[2], county = if (county) m[3] else NA_character_); next }
    if (grepl(ANY_CTY, ln) || grepl("^[A-Z][A-Za-z .,&'-]+$", trimws(ln)) && !grepl(CAND, ln) && nchar(trimws(ln)) > 3 && !grepl("^(Over|Under|Certification|Vote totals)", trimws(ln)) && grepl("^(Governor|Attorney|Secretary|Treasurer|Auditor|Commissioner|Lieutenant|State|U\\.S\\.|Proposed|Supreme|Court|Judge|Prosecuting|Circuit|County|Sheriff)", trimws(ln))) { cur <- NULL; next }
    if (is.null(cur)) next
    c1 <- regmatches(ln, regexec(CAND, ln))[[1]]
    if (length(c1)) out[[length(out) + 1]] <- tibble(district = cur$district, county = cur$county, candidate = strip_title(trimws(c1[2])), party = trimws(c1[3]), votes = as.numeric(gsub(",", "", c1[4])))
  }
  bind_rows(out)
}
## statewide block: lines from the first "U.S. Congress District 01" up to the next non-House heading (top of the file)
top_end <- which(grepl("^Governor\\s*$", L))[1]; stopifnot(!is.na(top_end))
state <- parse_lines(which(grepl(HOUSE_STATE, L))[1]:top_end, HOUSE_STATE)
state_tot <- state %>% group_by(district, candidate, party) %>% summarise(votes = sum(votes), .groups = "drop")
message("statewide House lines: ", nrow(state), " in districts ", paste(sort(unique(state$district)), collapse = ","))

first_cty <- which(grepl(HOUSE_CTY, L))[1]
cty <- parse_lines(seq(first_cty - 2, length(L)), HOUSE_CTY, county = TRUE)
message("county House lines: ", nrow(cty), "; blocks: ", nrow(distinct(cty, district, county)))
stopifnot(!anyDuplicated(cty[, c("district", "county", "candidate", "party")]))

xw <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  filter(state == "ARKANSAS", !is.na(county_fips)) %>% distinct(county_name, county_fips)
norm <- function(z) gsub("[^A-Z]", "", toupper(z)); xw <- xw %>% mutate(key = norm(county_name)) %>% distinct(key, county_fips)
stopifnot(nrow(xw) == 75)
cty <- cty %>% mutate(county_fips = xw$county_fips[match(norm(county), xw$key)], year = 2002L,
                      party_group = case_when(party == "Democrat" ~ "DEM", party == "Republican" ~ "REP", TRUE ~ "OTHER"))
if (anyNA(cty$county_fips)) { print(unique(cty$county[is.na(cty$county_fips)])); stop("unmatched county names") }

## ---- verification 1: county sums == printed statewide totals, for every candidate ---------------------------------------------------------------------
cmp <- cty %>% group_by(district, candidate, party) %>% summarise(county_sum = sum(votes), .groups = "drop") %>% full_join(state_tot, by = c("district", "candidate", "party"))
cat("\n== county sums vs printed statewide totals ==\n"); print(as.data.frame(cmp %>% mutate(diff = county_sum - votes)))
stopifnot(!anyNA(cmp$county_sum), !anyNA(cmp$votes), all(cmp$county_sum == cmp$votes))
cat("ALL", nrow(cmp), "candidate totals tie exactly to the printed statewide totals\n")

long <- finalize_long(cty %>% select(year, county_fips, district, candidate, party, party_group, votes), "ar_2002")
save_long(long, "he_ar_2002")
shares <- derive_shares(long) %>% transmute(state = "ARKANSAS", year, cty_fips, sample, demovote, repuvote, totalvote)
saveRDS(shares, file.path(OUTPUT_DIR, "elect_he_cty_ar_2002.rds"))
check_long_vs_source(long, file.path(OUTPUT_DIR, "elect_he_cty_ar_2002.rds"))

## ---- verification 2: coverage, splits, presidential ratio, comparison with the partial OpenElections rows -----------------------------------------------
cat("\ncounties covered:", n_distinct(long$county_fips), "of 75; split counties (>1 district):", sum(long %>% group_by(county_fips) %>% summarise(d = n_distinct(district)) %>% pull(d) > 1), "\n")
print(as.data.frame(long %>% group_by(district) %>% summarise(counties = n_distinct(county_fips), candidates = n_distinct(candidate), votes = sum(votes))))
pe <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")) %>% filter(sample == "PE", year == 2000) %>% select(cty_fips, pe = totalvote)
r <- shares %>% left_join(pe, by = "cty_fips") %>% mutate(ratio = totalvote / pe)
cat("House 2002 / presidential 2000 total: min", round(min(r$ratio), 2), "median", round(median(r$ratio), 2), "max", round(max(r$ratio), 2), "\n")
print(as.data.frame(r %>% filter(ratio < 0.5 | ratio > 1.1) %>% select(cty_fips, totalvote, pe, ratio)))
oe <- readRDS(file.path(OUTPUT_DIR, "elect_he_cty_ar.rds")) %>% filter(year == 2002) %>% select(cty_fips, o_dem = demovote, o_rep = repuvote, o_tot = totalvote)
d <- shares %>% inner_join(oe, by = "cty_fips") %>% mutate(dd = abs(demovote - o_dem), dr = abs(repuvote - o_rep), dt = abs(totalvote - o_tot) / o_tot)
cat("\nvs partial OpenElections 2002 rows:", nrow(oe), "counties there;", nrow(d), "in common; identical totals in", sum(d$dt < 1e-9), "; max |dem diff|", round(max(d$dd), 4), "; max total rel diff", round(max(d$dt), 4), "\n")
print(as.data.frame(head(d %>% arrange(desc(dt)) %>% select(cty_fips, totalvote, o_tot, demovote, o_dem), 6)))
cat("OpenElections counties absent from this build:", sum(!oe$cty_fips %in% shares$cty_fips), "\n")
