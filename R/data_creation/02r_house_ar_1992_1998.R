## Arkansas county-level U.S. House results, 1992, 1994, 1996, 1998, from the official Arkansas Secretary of State workbooks the user placed in
## R/data/county_house_files/ (AR_92generalelectionresults.xls, AR_94general_election_results.xls, AR_96_results.xls, AR_Gen98Ver5.zip -> Gen98Ver5.xls).
##
## FILE LAYOUTS
##  * 1992 / 1994 / 1998: 75 county sheets (precinct columns, a TOTAL(S) column) + one statewide sheet ("GRAND TOTALS", "Summary", "Summary").
##    The statewide sheet is a candidate x county MATRIX: col 1 = name, col 2 = party, then one column per county (header = county name), then the printed
##    statewide total (header "TOTAL"/"Grand Totals"). NB the sheet called "ARKANSAS"/"Arkansas" is ARKANSAS COUNTY (05001), not a statewide sheet.
##  * 1996: ONE sheet "WEB TOTALS" with the same matrix layout (header "TOTAL" for the statewide total); no county sheets, so 1996 is verified against its
##    printed totals, Wikipedia and presidential turnout only.
##  * 1994 and 1998 summary sheets say "Unopposed Races Not Included On This Sheet". In 1998 District 1 (Marion Berry, unopposed) is therefore missing from
##    the Summary but the county sheets DO list him with votes, so 1998 is built from the county sheets (all four districts) and the Summary is the check.
##  * House blocks are headed "U.S. CONGRESS DISTRICT 0n" (1992, 1996, 1998 in mixed case) or "1st District" ... (1994). Names carry titles in some years
##    ("Congressman Marion Berry", "Senator Phil Wyrick", "Justice Ralph Forbes", "Representative Judy Smith"): stripped. 1996 names are upper case.
##  * County columns are matched by normalized name to the crosswalk (HOTSPRING, LITTLERIVER, STFRANCIS, VANBUREN ...), never by position.
##  * Blank county cells mean "not in this district"; a county that appears in two House blocks (split) is summed by county, district kept in the long table.
##
## party_group: Democrat/Republican lines -> DEM/REP; Reform, Independent, Libertarian, write-in ... -> OTHER (write-ins count in totals), like the rest of the project.
## VERIFICATION (all asserted or printed): county sums per candidate == the printed statewide total column; county-sheet cross-check where county sheets exist
## (every county x district x candidate identical); 75 counties present; check_long_vs_source; House/presidential total per county; statewide totals vs Wikipedia.
## Outputs: R/output/long/he_ar_<year>.rds and R/output/elect_he_cty_ar_<year>.rds (NEW files). Nothing existing is modified.

source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr); library(readxl)
FD <- file.path(PROJECT_ROOT, "R", "data", "county_house_files")
F98 <- file.path("/tmp/claude-1000/-home-paul-Stats-substack-projects-immigration/831fbe11-26b0-4ba2-9f03-261a9fbdf053/scratchpad/ar/AR_Gen98Ver5/Gen98Ver5.xls")
if (!file.exists(F98)) { dir.create(dirname(F98), recursive = TRUE, showWarnings = FALSE); system2("unzip", c("-o", "-q", shQuote(file.path(FD, "AR_Gen98Ver5.zip")), "-d", shQuote(dirname(F98)))) }
stopifnot(file.exists(F98))

norm <- function(x) gsub("[^A-Z]", "", toupper(x))
ar_fips <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  filter(state == "ARKANSAS", !is.na(county_fips)) %>% distinct(county_name, county_fips) %>% mutate(key = norm(county_name)) %>% distinct(key, county_fips)
stopifnot(nrow(ar_fips) == 75, !anyDuplicated(ar_fips$key))

DIST_RE <- "^(U\\.?S\\.? CONGRESS( DISTRICT)? ?0?([1-4])|([1-4])(ST|ND|RD|TH) DISTRICT)$"
district_of <- function(x) { m <- regmatches(toupper(trimws(x)), regexec(DIST_RE, toupper(trimws(x))))[[1]]; if (!length(m)) return(NA_character_); sprintf("%02d", as.integer(ifelse(nzchar(m[4]), m[4], m[5]))) }
strip_title <- function(x) trimws(gsub("^(Congressman|Congresswoman|Senator|Sen\\.|Representative|Rep\\.|Justice|Judge|Governor|Gov\\.|Dr\\.)\\s+", "", trimws(x), ignore.case = TRUE))
num <- function(x) suppressWarnings(as.numeric(gsub("[,\\s]", "", x)))
grp <- function(p) { p <- toupper(trimws(p)); ifelse(grepl("^DEMOCRAT", p), "DEM", ifelse(grepl("^REPUBLICAN", p), "REP", "OTHER")) }
rd <- function(f, sh) suppressMessages(read_excel(f, sheet = sh, col_names = FALSE, col_types = "text", .name_repair = "minimal"))

## candidate rows below a House header row r0: rows with a name AND a party, until the first row with no name
block_rows <- function(g, r0) { r <- r0 + 1; out <- integer(); while (r <= nrow(g) && !is.na(g[[1]][r]) && nzchar(trimws(g[[1]][r])) && !is.na(g[[2]][r])) { out <- c(out, r); r <- r + 1 }; out }

## ---- statewide MATRIX sheet: county columns by header name, total column by header ----
parse_matrix <- function(f, sheet, year) {
  g <- rd(f, sheet); hdr <- as.character(unlist(g[1, ])); keys <- norm(hdr)
  cty_cols <- which(keys %in% ar_fips$key & seq_along(hdr) >= 3); stopifnot(length(cty_cols) == 75, !anyDuplicated(keys[cty_cols]))
  tot_col <- which(grepl("^(GRAND )?TOTALS?$", toupper(trimws(hdr))))[1]; stopifnot(!is.na(tot_col))
  out <- list(); printed <- list()
  for (r0 in which(!is.na(vapply(g[[1]], district_of, "")))) {
    d <- district_of(g[[1]][r0]); rows <- block_rows(g, r0)
    for (r in rows) {
      v <- num(unlist(g[r, cty_cols])); nm <- strip_title(g[[1]][r])
      out[[length(out) + 1]] <- tibble(year = year, district = d, candidate = nm, party = trimws(g[[2]][r]), key = keys[cty_cols], votes = v)
      printed[[length(printed) + 1]] <- tibble(district = d, candidate = nm, printed_total = num(g[[tot_col]][r]))
    }
  }
  list(long = bind_rows(out), printed = bind_rows(printed))
}

## ---- county sheets: rows below any "CONGRESS" header, TOTAL(S) column (last header matching; 1994 Union also has an extra column "AS PER UNION CTY ELEC. COMM."
## that the statewide Summary uses). District labels on the county sheets are NOT trusted (e.g. 1992 Carroll is headed "DISTRICT 04" but is District 3 in the
## Summary), so the cross-check is by county and sorted vote values, not by district/name (county sheets also carry typos in names).
parse_county_sheet <- function(f, sheet, year) {
  g <- rd(f, sheet); hdr <- as.character(unlist(g[1, ])); tc <- tail(which(grepl("^TOTALS?$", toupper(trimws(hdr)))), 1)
  if (!length(tc)) stop("no TOTAL column in sheet ", sheet)
  out <- list()
  for (h in which(!is.na(g[[1]]) & is.na(g[[2]]) & grepl("CONGRESS", toupper(g[[1]]))))
    for (r in block_rows(g, h)) out[[length(out) + 1]] <- tibble(year = year, candidate = strip_title(g[[1]][r]), party = trimws(g[[2]][r]), key = norm(sheet), votes = num(g[[tc]][r]))
  bind_rows(out) %>% mutate(cn = norm(candidate))
}
parse_county_workbook <- function(f, year) {
  sh <- setdiff(excel_sheets(f), c("GRAND TOTALS", "Summary", "SUMMARY")); stopifnot(length(sh) == 75)
  bind_rows(lapply(sh, function(s) parse_county_sheet(f, s, year)))
}

## ---- Wikipedia national-page Arkansas sections (cached in R/data/raw_election/wikipedia_house): candidate + district share (percent only) ----
wp_pct <- function(year) {
  f <- file.path(PROJECT_ROOT, "R/data/raw_election/wikipedia_house", paste0(year, "_US_House_elections_national_page_Arkansas_section.txt"))
  L <- readLines(f, warn = FALSE, encoding = "UTF-8"); L <- gsub("\\[\\[[^]|]*\\|([^]]*)\\]\\]", "\\1", L); L <- gsub("\\[\\[([^]|]*)\\]\\]", "\\1", L)
  d <- NA_character_; out <- list()
  for (l in L) {
    m <- regmatches(l, regexec("(?i)ushr\\|(?:Arkansas|AR)\\|([0-9]+)", l, perl = TRUE))[[1]]; if (length(m)) d <- sprintf("%02d", as.integer(m[2]))
    m <- regmatches(l, regexec("^\\* .*?(?:\\}\\}|''')\\s*'*([^'{}]+?)'*\\s*\\(([^)]*)\\)\\s*([0-9.]+)%", l))[[1]]
    if (length(m) && !is.na(d)) out[[length(out) + 1]] <- tibble(district = d, wp_name = trimws(m[2]), wp_party = m[3], wp_pct = as.numeric(m[4]), wp_dec = nchar(sub("^[0-9]*\\.?", "", m[4])))
  }
  bind_rows(out)
}

pe <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")) %>% filter(sample == "PE", cty_fips %/% 1000 == 5) %>% select(pe_year = year, cty_fips, pe_tot = totalvote)
YEARS <- list(list(y = 1992, f = file.path(FD, "AR_92generalelectionresults.xls"), sw = "GRAND TOTALS", pey = 1992, county = TRUE),
              list(y = 1994, f = file.path(FD, "AR_94general_election_results.xls"), sw = "Summary", pey = 1992, county = TRUE),
              list(y = 1996, f = file.path(FD, "AR_96_results.xls"), sw = "WEB TOTALS", pey = 1996, county = FALSE),
              list(y = 1998, f = F98, sw = "Summary", pey = 1996, county = TRUE))
summary_rows <- list()
for (Y in YEARS) {
  y <- Y$y; message("\n################ ", y, " ################")
  M <- parse_matrix(Y$f, Y$sw, y)
  ## (1) county sums == printed statewide total column (per candidate)
  chk <- M$long %>% group_by(district, candidate) %>% summarise(sum_counties = sum(votes, na.rm = TRUE), .groups = "drop") %>% left_join(M$printed, by = c("district", "candidate"))
  bad <- chk %>% filter(is.na(printed_total) | sum_counties != printed_total)
  message("matrix: ", n_distinct(M$long$district), " House districts, ", nrow(chk), " candidates; county sums == printed statewide totals for ", nrow(chk) - nrow(bad), " of ", nrow(chk))
  if (nrow(bad)) { print(as.data.frame(bad)); stop("statewide total mismatch in ", y) }
  base <- M$long %>% filter(!is.na(votes))
  ## (2) county-sheet cross-check (per county: sorted House votes in the statewide matrix vs sorted TOTAL column of the county sheet's congressional block)
  if (Y$county) {
    C <- parse_county_workbook(Y$f, y)
    C_all <- C; if (y == 1998) C <- C %>% filter(!grepl("BERRY", toupper(candidate)))   # Berry (unopposed D1) handled below
    mv <- base %>% group_by(key) %>% summarise(m = list(sort(votes)), .groups = "drop")
    cv <- C %>% filter(!is.na(votes)) %>% group_by(key) %>% summarise(cnt = list(sort(votes)), .groups = "drop")
    cmp <- full_join(mv, cv, by = "key") %>% mutate(m = lapply(m, function(x) if (is.null(x)) numeric() else x), cnt = lapply(cnt, function(x) if (is.null(x)) numeric() else x))
    msg <- cmp %>% rowwise() %>% mutate(same = identical(as.numeric(m), as.numeric(cnt))) %>% ungroup()
    message("county-sheet cross-check: ", sum(msg$same), " of ", nrow(msg), " counties identical (matrix vs county TOTAL column, House rows)")
    dd_ <- msg %>% filter(!same); if (nrow(dd_)) for (k in seq_len(nrow(dd_))) message("  ", dd_$key[k], ": statewide-matrix [", paste(dd_$m[[k]], collapse = ","), "] vs county sheet [", paste(dd_$cnt[[k]], collapse = ","), "]")
    if (y == 1998) {
      ## District 1 (Marion Berry, unopposed) is missing from the Summary. The county sheets list him, but with a printed tally in only 12 of the 25 District-1 counties
      ## (0/blank in the other 13) and the tallies that exist are incomplete (unopposed races are not officially counted), so they are NOT used as county results:
      ## District 1 is treated as an unopposed_no_ballot gap, like LA/OK. The tallies are kept for reference next to the US Senate county totals in the same workbook.
      d1 <- C_all %>% filter(grepl("BERRY", toupper(candidate)))
      gs <- rd(Y$f, Y$sw); hs <- which(grepl("^U\\.?S\\.? SENATE", toupper(trimws(gs[[1]])))); stopifnot(length(hs) == 1)
      sen_rows <- block_rows(gs, hs); hh <- as.character(unlist(gs[1, ])); cc <- which(norm(hh) %in% ar_fips$key & seq_along(hh) >= 3)
      sen <- tibble(key = norm(hh[cc]), senate_total = colSums(matrix(num(unlist(gs[sen_rows, cc])), nrow = length(sen_rows)), na.rm = TRUE))
      d1t <- d1 %>% transmute(key, berry_tally = votes) %>% left_join(sen, by = "key") %>% left_join(ar_fips, by = "key") %>% mutate(tally_over_senate_total = round(berry_tally / senate_total, 3)) %>% arrange(county_fips)
      write.csv(d1t %>% transmute(county_fips, county_key = key, berry_tally, us_senate_total_1998 = senate_total, tally_over_senate_total), file.path(OUTPUT_DIR, "ar_1998_district1_unopposed_tallies.csv"), row.names = FALSE)
      message("1998 D1 Berry (unopposed): listed in ", nrow(d1), " county sheets; positive tally in ", sum(d1t$berry_tally > 0, na.rm = TRUE), " (tally/US Senate total: median ", round(median(d1t$tally_over_senate_total[d1t$berry_tally > 0], na.rm = TRUE), 2),
              ", range ", paste(round(range(d1t$tally_over_senate_total[d1t$berry_tally > 0], na.rm = TRUE), 2), collapse = "-"), "); 0/blank in ", sum(is.na(d1t$berry_tally) | d1t$berry_tally == 0), "; NOT used (see comment); saved to R/output/ar_1998_district1_unopposed_tallies.csv")
    }
  }
  df <- base %>% left_join(ar_fips, by = "key"); stopifnot(!anyNA(df$county_fips))
  df <- df %>% transmute(year, county_fips, district, candidate, party, party_group = grp(party), votes)
  long <- finalize_long(df, paste0("ar_", y))
  save_long(long, paste0("he_ar_", y))
  shares <- derive_shares(long) %>% transmute(state = "ARKANSAS", year, cty_fips, sample, demovote, repuvote, totalvote)
  saveRDS(shares, file.path(OUTPUT_DIR, paste0("elect_he_cty_ar_", y, ".rds")))
  ## (3) check_long_vs_source (reads the file just written) + coverage
  res <- check_long_vs_source(long, file.path(OUTPUT_DIR, paste0("elect_he_cty_ar_", y, ".rds")))
  ncty <- n_distinct(long$county_fips); message("counties with House rows: ", ncty, " of 75; long rows ", nrow(long), "; candidates ", n_distinct(long$district, long$candidate))
  dd <- long %>% group_by(district) %>% summarise(cand = n_distinct(candidate), counties = n_distinct(county_fips), votes = sum(votes), .groups = "drop"); print(as.data.frame(dd))
  message("districts with NO race in this file: ", paste(setdiff(sprintf("%02d", 1:4), unique(long$district)), collapse = ", "), " (none if blank)")
  ## (4) House total vs presidential total (nearest presidential year in the panel)
  rat <- shares %>% left_join(pe %>% filter(pe_year == Y$pey), by = "cty_fips") %>% mutate(ratio = totalvote / pe_tot)
  message("House/presidential total per county (", Y$pey, " presidential): min ", round(min(rat$ratio, na.rm = TRUE), 2), " median ", round(median(rat$ratio, na.rm = TRUE), 2), " max ", round(max(rat$ratio, na.rm = TRUE), 2),
          "; counties with ratio > 1.1 or < 0.4: ", sum(rat$ratio > 1.1 | rat$ratio < 0.4, na.rm = TRUE))
  if (any(rat$ratio > 1.1 | rat$ratio < 0.4, na.rm = TRUE)) print(as.data.frame(rat %>% filter(ratio > 1.1 | ratio < 0.4) %>% transmute(cty_fips, totalvote, pe_tot, ratio = round(ratio, 2))))
  ## (5) Wikipedia (national-page Arkansas section, percentages only): district shares of the vote per candidate, tolerance = rounding of the printed figure
  w <- wp_pct(y)
  lt <- function(x) { t <- strsplit(toupper(gsub("[^A-Za-z' -]", " ", x)), "[ ]+")[[1]]; t <- t[!t %in% c("JR", "SR", "II", "III", "")]; tail(t, 1) }
  o <- long %>% group_by(district) %>% mutate(dtot = sum(votes)) %>% group_by(district, candidate, party_group, dtot) %>% summarise(ours = sum(votes), .groups = "drop") %>% mutate(k = vapply(candidate, lt, ""), our_pct = 100 * ours / dtot)
  ## surname match; a candidate who changed surname (1994 Blanche Lambert / "Blanche Lincoln") is matched by district + major party instead
  w$k <- vapply(w$wp_name, lt, "")
  for (i in seq_len(nrow(w))) if (!any(o$district == w$district[i] & o$k == w$k[i])) {
    pg <- ifelse(grepl("Democrat", w$wp_party[i]), "DEM", ifelse(grepl("Republican", w$wp_party[i]), "REP", "OTHER")); z <- o$k[o$district == w$district[i] & o$party_group == pg]
    if (pg != "OTHER" && length(z) == 1) w$k[i] <- z
  }
  ww <- w %>% left_join(o, by = c("district", "k")) %>% mutate(diff_pp = our_pct - wp_pct, tol = 0.5 * 10^(-wp_dec) + 0.005)
  message("Wikipedia (percentages): ", nrow(ww), " candidates; matched ", sum(!is.na(ww$ours)), "; within rounding of the printed figure: ", sum(abs(ww$diff_pp) <= ww$tol, na.rm = TRUE), "; max |diff| = ", round(max(abs(ww$diff_pp), na.rm = TRUE), 3), " pp")
  bw <- ww %>% filter(is.na(ours) | abs(diff_pp) > tol); if (nrow(bw)) { message("outside rounding / unmatched:"); print(as.data.frame(bw %>% select(district, wp_name, wp_pct, candidate, our_pct))) }
  unm <- o %>% anti_join(ww %>% filter(!is.na(ours)), by = c("district", "candidate")); if (nrow(unm)) { message("our candidates not listed on Wikipedia:"); print(as.data.frame(unm %>% select(district, candidate, ours, our_pct))) }
  summary_rows[[as.character(y)]] <- tibble(year = y, rows = nrow(long), counties = ncty, districts = paste(sort(unique(long$district)), collapse = ","), pass = res$pass)
}
message("\n== SUMMARY =="); print(as.data.frame(bind_rows(summary_rows)))
