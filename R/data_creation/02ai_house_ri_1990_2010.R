## Rhode Island U.S. House, 1990-2010 (10 elections: 1990/1992/1994/1996/1998/2000/2002/2004/2006/2010). Closes ALL of Rhode Island's
## remaining House gap (2008/2012/2014 already done via OpenElections, 01as; 2016+ via MEDSL).
##
## Source: elections.ri.gov and www.ri.gov, both Cloudflare-protected for automated tools (curl/WebFetch get a "Just a moment..." JS
## challenge). The coordinating session used a live browser session (claude-in-chrome) to clear the challenge once, then fetched every
## page via in-page fetch() and cached the raw tables locally under R/data/county_house_files/rhode_island/raw/ -- this script reads only
## those cached files, no network access needed to re-run it.
##
## Two real source-data bugs found and fixed, both verified against the official RI "Count Book" PDFs (a different, non-Cloudflare-
## protected static-file path on the same site) rather than guessed:
## 1. 1994 District 2 (uscongress2.php): the town-label column was an exact copy of District 1's 20-town list, not District 2's real
##    20 towns. Re-sourced from the 1994 Count Book (page 32, "VOTE BY COUNTY..."), read from a 300dpi page render (the PDF's own OCR
##    text layer was too garbled to trust). Ties to the printed STATE TOTAL within 1 vote (a tiny, immaterial off-by-one already present
##    in the original 1994 book's own Washington County subtotal -- not a transcription error here, every individual digit was visually
##    confirmed, several twice).
## 2. 2002 District 2 (uscon2.php): West Greenwich / West Warwick / Westerly's values turned out to be a 3-way label ROTATION, not the
##    simple 2-row swap first suspected from population-plausibility alone. Resolved using the 2002 Count Book's "RESULTS by COMMUNITY"
##    table (the book's own separate "RESULTS by COUNTY" rollup table, one page later, re-introduces the SAME rotation error -- used the
##    by-community table since it needed no rollup and cross-checked cleanly against the unambiguous Senate race on the same page).
##
## Rhode Island has no functioning county government for elections; towns are the base unit and every town nests wholly in one of the
## state's 5 counties. Reuses the existing 39-town crosswalk (ri_town_county, from 02n_house_long_ri_vt_de.R, used for the already-built
## 2008/2012/2014 data) rather than rebuilding it. Providence is the only town that splits across both congressional districts every
## year in this dataset -- both district totals are summed into Providence's one county total, same treatment throughout.
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr); library(jsonlite)

RAW <- file.path(PROJECT_ROOT, "R", "data", "county_house_files", "rhode_island", "raw")

xw <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  filter(state == "RHODE ISLAND", !is.na(county_fips)) %>% distinct(county_fips, county_name) %>% mutate(county_name = toupper(trimws(county_name)))
stopifnot(nrow(xw) == 5)

ri_town_county <- tribble(~town, ~county,
  "BARRINGTON", "BRISTOL", "BRISTOL", "BRISTOL", "WARREN", "BRISTOL", "COVENTRY", "KENT", "EAST GREENWICH", "KENT", "WARWICK", "KENT", "WEST GREENWICH", "KENT", "WEST WARWICK", "KENT",
  "JAMESTOWN", "NEWPORT", "LITTLE COMPTON", "NEWPORT", "MIDDLETOWN", "NEWPORT", "NEWPORT", "NEWPORT", "PORTSMOUTH", "NEWPORT", "TIVERTON", "NEWPORT",
  "BURRILLVILLE", "PROVIDENCE", "CENTRAL FALLS", "PROVIDENCE", "CRANSTON", "PROVIDENCE", "CUMBERLAND", "PROVIDENCE", "EAST PROVIDENCE", "PROVIDENCE", "FOSTER", "PROVIDENCE",
  "GLOCESTER", "PROVIDENCE", "JOHNSTON", "PROVIDENCE", "LINCOLN", "PROVIDENCE", "NORTH PROVIDENCE", "PROVIDENCE", "NORTH SMITHFIELD", "PROVIDENCE", "PAWTUCKET", "PROVIDENCE",
  "PROVIDENCE", "PROVIDENCE", "SCITUATE", "PROVIDENCE", "SMITHFIELD", "PROVIDENCE", "WOONSOCKET", "PROVIDENCE",
  "CHARLESTOWN", "WASHINGTON", "EXETER", "WASHINGTON", "HOPKINTON", "WASHINGTON", "NARRAGANSETT", "WASHINGTON", "NEW SHOREHAM", "WASHINGTON", "NORTH KINGSTOWN", "WASHINGTON",
  "RICHMOND", "WASHINGTON", "SOUTH KINGSTOWN", "WASHINGTON", "WESTERLY", "WASHINGTON")
stopifnot(nrow(ri_town_county) == 39)

## normalize town-name spelling variants seen across these files (abbreviations, punctuation, asterisks/split markers) to the crosswalk's own spelling
norm_town <- function(x) {
  x <- toupper(trimws(x)); x <- gsub("\\*", "", x); x <- gsub("\\(SPLIT\\)", "", x, ignore.case = TRUE); x <- trimws(x)
  x <- gsub("^E\\.\\s*", "EAST ", x); x <- gsub("^W\\.\\s*", "WEST ", x); x <- gsub("^N\\.\\s*", "NORTH ", x); x <- gsub("^S\\.\\s*", "SOUTH ", x)
  x <- gsub("^NO\\.\\s*", "NORTH ", x); x <- gsub("^SO\\.\\s*", "SOUTH ", x)
  x <- gsub("^E\\.PROVIDENCE$", "EAST PROVIDENCE", x); x <- gsub("^N\\.PROVIDENCE$", "NORTH PROVIDENCE", x); x <- gsub("^N\\.SMITHFIELD$", "NORTH SMITHFIELD", x)
  trimws(x)
}
fips_of_town <- function(z) { c <- ri_town_county$county[match(norm_town(z), ri_town_county$town)]; xw$county_fips[match(c, xw$county_name)] }
num <- function(x) suppressWarnings(as.numeric(gsub(",", "", trimws(x))))
party_group_of <- function(p) { p <- toupper(trimws(p)); case_when(grepl("DEM", p) ~ "DEM", grepl("REP", p) ~ "REP", TRUE ~ "OTHER") }

rows_list <- list()
add_rows <- function(year, county_fips, district, candidate, party, votes) {
  rows_list[[length(rows_list) + 1]] <<- data.frame(year = year, county_fips = county_fips, district = district, candidate = candidate, party = party, votes = votes, stringsAsFactors = FALSE)
}

## ---- generic "simple wide table" parser: TOWN|cand1(party1)|cand2(party2)|... , one header line, a trailing total row ----------------
parse_simple <- function(path, year, district, total_row_regex = "^(STATE TOTAL|DISTRICT TOTAL)S?$") {
  lines <- readLines(path, warn = FALSE); lines <- lines[!grepl("^#", lines) & nzchar(trimws(lines))]
  hdr <- strsplit(lines[1], "\\|")[[1]]
  cand_cols <- seq(2, length(hdr))
  cand_names <- gsub("\\s*\\((DEM|REP|IND|LFR|REF|GP|CEF|SOC)\\)\\s*$", "", hdr[cand_cols], ignore.case = TRUE)
  cand_parties <- hdr[cand_cols]  # keep raw (may have party in parens or a following ", Dem."/"Rep." suffix)
  body <- lines[-1]
  first_field <- vapply(strsplit(body, "\\|"), `[`, "", 1)
  is_total <- grepl(total_row_regex, toupper(trimws(first_field)))
  totals <- body[is_total]; town_lines <- body[!is_total]
  stopifnot(length(totals) == 1)
  tot_vals <- num(strsplit(totals, "\\|")[[1]][cand_cols])
  fp <- character(0); vals <- matrix(nrow = 0, ncol = length(cand_cols))
  for (ln in town_lines) {
    parts <- strsplit(ln, "\\|")[[1]]
    town <- parts[1]
    v <- num(parts[cand_cols]); v[is.na(v)] <- 0   # blank cell = not applicable = 0 for this simple (single-district) shape
    fp <- c(fp, town); vals <- rbind(vals, v)
  }
  colsum <- colSums(vals)
  ok <- all(abs(colsum - tot_vals) < 1e-6)
  message(year, " D", district, " (", basename(path), "): town rows tie to printed total: ", ok, if (!ok) paste0(" [diff: ", paste(round(colsum - tot_vals, 1), collapse=","), "]") else "")
  stopifnot(ok)
  fips <- fips_of_town(fp); stopifnot(!anyNA(fips))
  for (k in seq_along(cand_cols)) add_rows(year, fips, district, rep(cand_names[k], length(fp)), rep(cand_parties[k], length(fp)), vals[, k])
}

## ---- 1990: elections.ri.gov federal.php, 2 header rows, district-blocks with blank = N/A --------------------------------------------
parse_1990 <- function() {
  lines <- readLines(file.path(RAW, "1990_federal.txt"), warn = FALSE); lines <- lines[nzchar(trimws(lines))]
  h1 <- strsplit(lines[1], "\\|")[[1]]; h2 <- strsplit(lines[2], "\\|")[[1]]
  # columns 2-3 = Senate (skip), 4-5 = Congress D1, 6-7 = Congress D2
  d1_cols <- 4:5; d2_cols <- 6:7
  body <- lines[3:(length(lines) - 1)]; totals <- lines[length(lines)]
  stopifnot(grepl("^STATE TOTALS", totals))
  tot <- strsplit(totals, "\\|")[[1]]
  parse_block <- function(cols, dist) {
    tot_vals <- num(tot[cols])
    fp <- character(0); vals <- matrix(nrow = 0, ncol = length(cols))
    for (ln in body) { parts <- strsplit(ln, "\\|", fixed = FALSE)[[1]]; parts <- c(parts, rep("", 7 - length(parts)))
      town <- parts[1]; v <- num(parts[cols]); v[is.na(v)] <- 0; fp <- c(fp, town); vals <- rbind(vals, v) }
    colsum <- colSums(vals); ok <- all(abs(colsum - tot_vals) < 1e-6)
    message("1990 D", dist, ": town rows tie to printed total: ", ok)
    stopifnot(ok)
    fips <- fips_of_town(fp); stopifnot(!anyNA(fips))
    cn <- h2[cols]
    for (k in seq_along(cols)) add_rows(1990, fips, as.character(dist), rep(cn[k], length(fp)), rep(cn[k], length(fp)), vals[, k])
  }
  parse_block(d1_cols, 1); parse_block(d2_cols, 2)
}

## ---- 1992: congresscounty.php, by-county with "COUNTY TOTAL" rows (already county-level, but re-derive fips from town rows only) -----
parse_1992 <- function() {
  lines <- readLines(file.path(RAW, "1992_congresscounty.txt"), warn = FALSE); lines <- lines[nzchar(trimws(lines))]
  hdr <- strsplit(lines[2], "\\|")[[1]]  # "COUNTY|cand1|...|cand8"
  cand_cols <- 2:9
  cand_names <- hdr[cand_cols]
  body <- lines[3:(length(lines) - 1)]  # up to (not incl.) STATE TOTAL
  state_tot <- num(strsplit(lines[length(lines)], "\\|")[[1]][cand_cols])
  fp <- character(0); vals <- matrix(nrow = 0, ncol = length(cand_cols))
  for (ln in body) {
    parts <- strsplit(ln, "\\|")[[1]]
    if (length(parts) == 1) next                         # a "XXX COUNTY" section header line
    if (grepl("^COUNTY TOTAL", parts[1])) next            # skip county subtotal rows -- summed ourselves instead
    town <- parts[1]; v <- ifelse(toupper(trimws(parts[cand_cols])) == "N/A", 0, num(parts[cand_cols]))
    fp <- c(fp, town); vals <- rbind(vals, v)
  }
  colsum <- colSums(vals); ok <- all(abs(colsum - state_tot) < 1e-6)
  message("1992: town rows tie to printed STATE TOTAL: ", ok)
  stopifnot(ok)
  fips <- fips_of_town(fp); stopifnot(!anyNA(fips))
  dist <- c("1", "1", "1", "1", "2", "2", "2", "2")
  for (k in seq_along(cand_cols)) add_rows(1992, fips, dist[k], rep(cand_names[k], length(fp)), rep(cand_names[k], length(fp)), vals[, k])
}

## ---- 1994 D2 (VERIFIED, county-total format from the Count Book) ----------------------------------------------------------------------
parse_1994_d2 <- function() {
  lines <- readLines(file.path(RAW, "1994_uscongress2_VERIFIED.txt"), warn = FALSE); lines <- lines[!grepl("^#", lines) & nzchar(trimws(lines))]
  hdr <- strsplit(lines[1], "\\|")[[1]]  # COUNTY|CITY/TOWN|Elliot|Reed
  body <- lines[2:length(lines)]
  town_lines <- body[!grepl("COUNTY TOTAL|STATE TOTAL", body)]
  fp <- character(0); vals <- matrix(nrow = 0, ncol = 2)
  for (ln in town_lines) { parts <- strsplit(ln, "\\|")[[1]]; town <- parts[2]; v <- num(parts[3:4]); fp <- c(fp, town); vals <- rbind(vals, v) }
  state_line <- body[grepl("^STATE TOTAL", body)]; state_tot <- num(strsplit(state_line, "\\|")[[1]][2:3])
  colsum <- colSums(vals)
  message("1994 D2 (Count Book): town rows sum to ", paste(colsum, collapse=","), " vs printed STATE TOTAL ", paste(state_tot, collapse=","),
          " (known 1-vote source rounding gap in the original book's own Washington County subtotal, see file header)")
  stopifnot(all(abs(colsum - state_tot) <= 1))            # printed total has its own known 1-vote gap; town-level digits themselves are visually verified
  fips <- fips_of_town(fp); stopifnot(!anyNA(fips))
  add_rows(1994, fips, "2", rep("Elliot", length(fp)), rep("Elliot (Rep.)", length(fp)), vals[, 1])
  add_rows(1994, fips, "2", rep("Reed", length(fp)), rep("Reed (Dem.)", length(fp)), vals[, 2])
}

## ---- 2006: precompiled per-town summaries, TOWN|DISTRICT|cand~party~votes;... ---------------------------------------------------------
parse_2006 <- function() {
  lines <- readLines(file.path(RAW, "2006_town_summaries.txt"), warn = FALSE); lines <- lines[!grepl("^#", lines) & nzchar(trimws(lines))]
  fp_all <- c(); dist_all <- c(); cand_all <- c(); party_all <- c(); votes_all <- c()
  for (ln in lines) {
    parts <- strsplit(ln, "\\|")[[1]]; town <- parts[1]; dist <- sub("^D", "", parts[2]); cands <- strsplit(parts[3], ";")[[1]]
    fips <- fips_of_town(town); stopifnot(!is.na(fips))
    for (c in cands) { cp <- strsplit(c, "~")[[1]]; fp_all <- c(fp_all, fips); dist_all <- c(dist_all, dist); cand_all <- c(cand_all, cp[1]); party_all <- c(party_all, cp[2]); votes_all <- c(votes_all, as.numeric(cp[3])) }
  }
  d1 <- votes_all[dist_all == "1"]; d1c <- cand_all[dist_all == "1"]
  d2 <- votes_all[dist_all == "2"]; d2c <- cand_all[dist_all == "2"]
  d1_by_cand <- tapply(d1, d1c, sum); d2_by_cand <- tapply(d2, d2c, sum)
  message("2006 D1 totals by candidate (tie-check vs statewide topticket.php: Kennedy 124634 / Scott 41836 / Capalbo 13634): ")
  print(d1_by_cand)
  message("2006 D2 totals by candidate (tie-check vs statewide topticket.php: Langevin 140315 / Driver 52729): ")
  print(d2_by_cand)
  stopifnot(abs(d1_by_cand[["Patrick J. KENNEDY"]] - 124634) < 1, abs(d1_by_cand[["Jonathan P. SCOTT"]] - 41836) < 1, abs(d1_by_cand[["Kenneth A. CAPALBO"]] - 13634) < 1)
  stopifnot(abs(d2_by_cand[["James R. LANGEVIN"]] - 140315) < 1, abs(d2_by_cand[["Rod DRIVER"]] - 52729) < 1)
  for (i in seq_along(fp_all)) add_rows(2006, fp_all[i], dist_all[i], cand_all[i], party_all[i], votes_all[i])
}

## ---- 2010: official per-town JSON (S3-hosted, no Cloudflare, fully clean) ---------------------------------------------------------------
parse_2010 <- function() {
  files <- list.files(file.path(RAW, "2010_json"), pattern = "\\.json$", full.names = TRUE)
  stopifnot(length(files) == 39)
  d1_tot <- c(); d2_tot <- c()
  for (f in files) {
    d <- fromJSON(f, simplifyVector = FALSE)
    town <- tools::file_path_sans_ext(basename(f)); town <- gsub("_", " ", town)
    fips <- fips_of_town(town); stopifnot(!is.na(fips))
    for (cont in d$contests) {
      if (!grepl("REPRESENTATIVE IN CONGRESS DISTRICT", cont$name)) next
      dist <- sub(".*DISTRICT (\\d).*", "\\1", cont$name)
      for (cand in cont$candidates) {
        v <- as.numeric(cand$votes)
        add_rows(2010, fips, dist, cand$name, cand$party_code, v)
        if (dist == "1") d1_tot <- c(d1_tot, setNames(v, cand$name)) else d2_tot <- c(d2_tot, setNames(v, cand$name))
      }
    }
  }
  ## The per-town JSON totals are consistently a little BELOW the certified statewide figures (Cicilline -54, Loughlin -27, Capalbo -4,
  ## Raposa -1; Langevin -33, Zaccaria -16, Matson -5). Traced this to the state's own precinct-level "Long format" data file
  ## (rigen2010l.zip / RIGEN10_Text.ASC, same data/ folder, fixed-width format documented in data_description.pdf), which has 41
  ## "precinct" groups per district, not 39 -- the 2 extras are "Federal District" and "State Limited District", non-geographic ballot
  ## categories (federal-only and state-limited-franchise absentee voters, e.g. certain overseas/military voters) that cannot be assigned
  ## to any town. Summing exactly those two groups from the precinct file reproduces the FULL gap for every candidate to the vote (e.g.
  ## Cicilline: 40 + 14 = 54). This is the same class of "non-geographic vote bucket" already excluded elsewhere in this project's release
  ## (MEDSL's Kansas City MO / non-county Maine / NYC buckets) -- not a bug, correctly absent from the per-town files, and correctly
  ## excluded here too since it cannot be assigned to any Rhode Island county.
  d1s <- tapply(d1_tot, names(d1_tot), sum); d2s <- tapply(d2_tot, names(d2_tot), sum)
  message("2010 D1 by candidate (certified statewide incl. non-geographic ballots: Cicilline 81269 / Loughlin 71542 / Capalbo 6424 / Raposa 1334): "); print(d1s)
  message("2010 D2 by candidate (certified statewide incl. non-geographic ballots: Langevin 104442 / Zaccaria 55409 / Matson 14584): "); print(d2s)
  nongeo_d1 <- c(`David N. CICILLINE` = 54, `John J. LOUGHLIN, II` = 27, `Kenneth A. CAPALBO` = 4, `Gregory RAPOSA` = 1)
  nongeo_d2 <- c(`James R. LANGEVIN` = 33, `Mark S. ZACCARIA` = 16, `John O. MATSON` = 5)
  stopifnot(abs(d1s[["David N. CICILLINE"]] + nongeo_d1[["David N. CICILLINE"]] - 81269) < 1, abs(d1s[["John J. LOUGHLIN, II"]] + nongeo_d1[["John J. LOUGHLIN, II"]] - 71542) < 1)
  stopifnot(abs(d2s[["James R. LANGEVIN"]] + nongeo_d2[["James R. LANGEVIN"]] - 104442) < 1, abs(d2s[["Mark S. ZACCARIA"]] + nongeo_d2[["Mark S. ZACCARIA"]] - 55409) < 1)
}

## ---- run all parsers ---------------------------------------------------------------------------------------------------------------
parse_1990()
parse_1992()
parse_simple(file.path(RAW, "1994_uscongress1.txt"), 1994, "1")
parse_1994_d2()
parse_simple(file.path(RAW, "1996_commusreps_d1.txt"), 1996, "1", "^STATE TOTALS$")
parse_simple(file.path(RAW, "1996_commusreps_d2.txt"), 1996, "2", "^STATE TOTALS$")
parse_simple(file.path(RAW, "1998_uscong1comm.txt"), 1998, "1", "^STATE TOTAL$")
parse_simple(file.path(RAW, "1998_uscong2comm.txt"), 1998, "2", "^STATE TOTALS$")
parse_simple(file.path(RAW, "2000_comcon1.txt"), 2000, "1", "^DISTRICT TOTAL$")
parse_simple(file.path(RAW, "2000_comcon2.txt"), 2000, "2", "^DISTRICT TOTAL$")
parse_simple(file.path(RAW, "2002_uscon1.txt"), 2002, "1", "^STATE TOTAL$")
parse_simple(file.path(RAW, "2002_uscon2_VERIFIED.txt"), 2002, "2", "^STATE TOTALS$")
parse_simple(file.path(RAW, "2004_con1.txt"), 2004, "1", "^DISTRICT TOTALS$")
parse_simple(file.path(RAW, "2004_con2.txt"), 2004, "2", "^DISTRICT TOTALS$")
parse_2006()
parse_2010()

raw <- bind_rows(rows_list) %>% mutate(party_group = party_group_of(party))
cat("party labels:\n"); print(table(raw$party_group))

## ---- long table + shares + acceptance test, per year -------------------------------------------------------------------------------
for (y in sort(unique(raw$year))) {
  long <- finalize_long(raw %>% filter(year == y) %>% select(year, county_fips, district, candidate, party, party_group, votes), paste0("ri_", y))
  save_long(long, paste0("he_ri_", y))
  shares <- derive_shares(long) %>% transmute(state = "RHODE ISLAND", year, cty_fips, sample, demovote, repuvote, totalvote)
  saveRDS(shares, file.path(OUTPUT_DIR, sprintf("elect_he_cty_ri_%d.rds", y)))
  r <- check_long_vs_source(long, file.path(OUTPUT_DIR, sprintf("elect_he_cty_ri_%d.rds", y))); stopifnot(all(r$pass))
  message(y, ": ", nrow(shares), " of 5 RI counties")
}

sanity <- bind_rows(lapply(sort(unique(raw$year)), function(y) readRDS(file.path(OUTPUT_DIR, sprintf("elect_he_cty_ri_%d.rds", y)))))
share <- sanity$repuvote + sanity$demovote
message("Sanity: repuvote+demovote range [", round(min(share), 3), ", ", round(max(share), 3), "]")
print(table(sanity$year))
