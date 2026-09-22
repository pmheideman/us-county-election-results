## Indiana U.S. House by county, general elections 1990, 1992 and 1994, from the Indiana Secretary of State's election reports
## (R/data/indiana_sos_reports/indiana_election_report_<year>.pdf: image-only 400-dpi scans, no text layer). For each district and candidate the report lists the counties with their
## votes and a TOTAL line (district 10 = Marion County only: no TOTAL line). The county rows were READ BY EYE from the scans (crops at 220-400 dpi) into
## R/data/raw_house_county_open_states/indiana_official/in_transcription_<year>.txt, one block per candidate ("B|year|district|party|candidate|printed total: County votes, ..."); every block
## adds up to its printed TOTAL (checked below; three misreads in 1990 were caught this way and fixed from 400-dpi crops, see R/output/in_ocr_corrections_1990_1994.csv).
## Checks: (1) block sums == printed TOTALs; (2) county count per district; (3) district vote shares vs Wikipedia (national pages, percentages only);
## (4) each county's House total vs the same-year Senate / nearest presidential total in the panel.
## Outputs: R/output/long/he_in_<year>.rds and R/output/elect_he_cty_in_<year>.rds for 1990, 1992, 1994 (NEW files; the OpenElections files elect_he_cty_in.rds / he_in.rds for 2002-2014 are untouched).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "indiana_official")
parse_file <- function(f) {
  ln <- readLines(f, warn = FALSE, encoding = "UTF-8"); ln <- ln[startsWith(ln, "B|")]
  purrr::map_dfr(ln, function(l) {
    h <- strsplit(l, ": ", fixed = TRUE)[[1]]; p <- strsplit(h[1], "|", fixed = TRUE)[[1]]; items <- strsplit(paste(h[-1], collapse = ": "), ", ")[[1]]
    tibble(year = as.integer(p[2]), district = p[3], party = p[4], candidate = p[5], printed = suppressWarnings(as.numeric(p[6])),
           county = sub(" [0-9]+$", "", items), votes = as.numeric(sub("^.* ", "", items))) })
}
raw <- purrr::map_dfr(file.path(DIR, sprintf("in_transcription_%d.txt", c(1990, 1992, 1994))), parse_file)
chk <- raw %>% group_by(year, district, party, candidate) %>% summarise(sum = sum(votes), printed = first(printed), n_cty = n(), .groups = "drop") %>% mutate(ok = is.na(printed) | sum == printed)
cat("candidate blocks:", nrow(chk), "| blocks with a printed TOTAL:", sum(!is.na(chk$printed)), "| county sums equal the printed TOTAL:", sum(chk$ok & !is.na(chk$printed)), "| district 10 (Marion only, no TOTAL line):", sum(is.na(chk$printed)), "\n")
stopifnot(all(chk$ok))
cat("counties per district (1990):", paste(names(table(raw$district[raw$year == 1990 & !duplicated(raw[, c("year", "district", "county")])])), table(raw$district[raw$year == 1990 & !duplicated(raw[, c("year", "district", "county")])]), sep = "=", collapse = ", "), "\n")

xw <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  filter(state == "INDIANA", !is.na(county_fips)) %>% distinct(county_name, county_fips)
norm <- function(z) gsub("[^A-Z]", "", toupper(z)); xw <- xw %>% mutate(key = norm(county_name)) %>% distinct(key, county_fips); stopifnot(nrow(xw) == 92)
raw <- raw %>% mutate(county_fips = xw$county_fips[match(norm(county), xw$key)]); stopifnot(!anyNA(raw$county_fips))
PARTY <- c(D = "Democratic", R = "Republican", L = "Libertarian", I = "Independent", W = "Write-In", A = "New Alliance")
out <- raw %>% transmute(year, county_fips, district, candidate, party_group = case_when(party == "D" ~ "DEM", party == "R" ~ "REP", TRUE ~ "OTHER"), party = PARTY[party], votes)   # party_group BEFORE the label overwrites the code
res <- list()
for (y in c(1990L, 1992L, 1994L)) {
  o <- out %>% filter(year == y); long <- finalize_long(o, paste0("in_", y))
  ## finalize_long strips punctuation from names; the printed forms are restored from the transcription
  nm <- raw %>% filter(year == y) %>% distinct(candidate) %>% pull(candidate); key <- function(z) gsub("[^a-z]", "", tolower(z))
  long$candidate <- ifelse(key(long$candidate) %in% key(nm), nm[match(key(long$candidate), key(nm))], long$candidate)
  save_long(long, paste0("he_in_", y))
  shares <- derive_shares(long) %>% transmute(state = "INDIANA", year, cty_fips, sample, demovote, repuvote, totalvote); saveRDS(shares, file.path(OUTPUT_DIR, sprintf("elect_he_cty_in_%d.rds", y)))
  r <- check_long_vs_source(long, file.path(OUTPUT_DIR, sprintf("elect_he_cty_in_%d.rds", y))); stopifnot(all(r$pass))
  cat(y, ": counties", n_distinct(long$county_fips), "| districts", n_distinct(long$district), "| candidates", n_distinct(paste(long$district, long$candidate)), "| split counties", sum(long %>% group_by(county_fips) %>% summarise(d = n_distinct(district)) %>% pull(d) > 1), "\n")
  res[[as.character(y)]] <- shares
}
## ---- (3) Wikipedia district shares (national page, Indiana section; percentages only) -------------------------------------------------------------
wp_dir <- file.path(PROJECT_ROOT, "R", "data", "raw_election", "wikipedia_house")
for (y in c(1990, 1992, 1994)) {
  s <- paste(readLines(file.path(wp_dir, sprintf("%d_United_States_House_of_Representatives_elections_national.txt", y)), warn = FALSE), collapse = "\n")
  i <- regexpr("== ?Indiana ?==", s); seg <- substr(s, i, i + 12000); seg <- substr(seg, 1, regexpr("== ?Iowa ?==", seg) - 1)
  blocks <- strsplit(seg, "\\|-\\s*\n! \\{\\{[Uu]shr\\|Indiana\\|")[[1]][-1]
  wp <- purrr::map_dfr(blocks, function(b) { d <- sprintf("%02d", as.integer(sub("^([0-9]+)\\|.*", "\\1", b)))
    ln <- regmatches(b, gregexpr("\\* \\{\\{Party stripe[^\n]*", b))[[1]]
    tibble(district = d, line = ln) }) %>% mutate(pct = as.numeric(sub(".*\\) ([0-9.]+)%.*", "\\1", line)), nm = gsub("\\[\\[|\\]\\]|'''|\\{\\{Aye\\}\\}", "", sub(".*\\}\\}(\\{\\{Aye\\}\\})?\\s*", "", sub("\\((Democratic|Republican|Independent|Libertarian|American|Write-in)[^)]*\\).*", "", line))))
  mine <- res[[as.character(y)]]; L <- readRDS(file.path(LONG_DIR, sprintf("he_in_%d.rds", y)))
  dist <- L %>% group_by(district, candidate) %>% summarise(v = sum(votes), .groups = "drop") %>% group_by(district) %>% mutate(share = round(100 * v / sum(v), 1)) %>% ungroup()
  cmp <- wp %>% mutate(sur = tolower(sub(".*\\s", "", trimws(gsub("\\|[^\\]]*", "", nm))))) %>% left_join(dist %>% mutate(sur = tolower(sub(",.*", "", sub(".*\\s", "", sub(",? Jr\\.?$", "", candidate))))), by = c("district", "sur"))
  cat(y, ": Wikipedia candidates matched", sum(!is.na(cmp$share)), "of", nrow(cmp), "| max |share difference| (points):", round(max(abs(cmp$pct - cmp$share), na.rm = TRUE), 2), "\n")
  print(as.data.frame(cmp %>% filter(is.na(share) | abs(pct - share) > 0.15) %>% select(district, nm, pct, candidate, share)))
}
## ---- (4) county totals vs Senate / presidential totals in the panel ---------------------------------------------------------------------------------
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")) %>% filter(cty_fips %/% 1000 == 18)
for (y in c(1990, 1992, 1994)) {
  ref <- panel %>% filter(sample %in% c("SE", "PE"), year == ifelse(y == 1992, 1992, y)) %>% group_by(sample) %>% summarise(n = n(), .groups = "drop")
  smp <- if (y == 1992) "PE" else if (any(panel$sample == "SE" & panel$year == y)) "SE" else "PE"; yr <- if (smp == "PE" && y != 1992) y + ifelse(y == 1990, 2, -2) else y
  ref <- panel %>% filter(sample == smp, year == yr) %>% select(cty_fips, ref = totalvote)
  r <- res[[as.character(y)]] %>% left_join(ref, by = "cty_fips") %>% mutate(ratio = totalvote / ref)
  cat(y, ": House total / ", smp, " ", yr, " total in the panel (", sum(!is.na(r$ref)), " counties): min ", round(min(r$ratio, na.rm = TRUE), 2), " median ", round(median(r$ratio, na.rm = TRUE), 2), " max ", round(max(r$ratio, na.rm = TRUE), 2), "\n", sep = "")
}
