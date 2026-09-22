## South Carolina U.S. House by county, general elections 2000, 2002, 2004 and 2006, from the State Election Commission election reports
## (R/data/county_house_files/SC_Election_Report_{2000,2002,2004,2006}.pdf; the printouts are scanned with an OCR text layer for 2000-2004, the 2006 book has a native text layer).
## Step 1 (Python, kept in R/data/raw_house_county_open_states/south_carolina_official/): sc_parse_text.py (2000-2004) and sc_parse_2006.py parse the pdftotext -layout text into
##   sc_raw_cells_<year>.csv: one row per printed cell (district, county as printed, column, candidate header, party code, raw token, cleaned votes, is_total for the 'STATE TOTAL' row).
##   OCR clean-up in the parser: Z->2, l/I->1, O->0, B->8, '.'-thousands separators, blank cells placed by the right edge of the STATE TOTAL columns, county names matched to the 46 SC counties.
## Step 2 (this script): county fixes for two rows the text split or lost (below), candidate names/parties from the page images, FIPS, checks, long + shares files.
## Checks: (a) county sums equal the printed STATE TOTAL for every candidate column of every district (all of them tie exactly); (b) 2006: every county row adds up to its TOTAL cell;
##   (c) statewide candidate totals vs Wikipedia (fusion candidates compared as the sum of their party lines); (d) county House total vs the presidential total of the same/nearest year;
##   (e) the same parser logic reproduces the 2008 build (he_sc_2008.rds, from the ENR XML) from SC_Election_Report_2008.pdf pp.58-59.
## Outputs: R/output/long/he_sc_<year>.rds, R/output/elect_he_cty_sc_<year>.rds (NEW files), R/output/sc_ocr_corrections_2000_2006.csv.
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "south_carolina_official")
xw <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  filter(state == "SOUTH CAROLINA", !is.na(county_fips)) %>% distinct(county_name, county_fips)
norm <- function(z) gsub("[^A-Z]", "", toupper(z)); xw <- xw %>% mutate(key = norm(county_name)) %>% distinct(key, county_fips); stopifnot(nrow(xw) == 46)
cells <- purrr::map_dfr(c(2000, 2002, 2004, 2006), function(y) read_csv(file.path(DIR, sprintf("sc_raw_cells_%d.csv", y)), col_types = cols(.default = "c")) %>%
  mutate(year = as.integer(year), col = as.integer(col), votes = as.numeric(votes), is_total = as.integer(is_total)))

## ---- manual fixes (each verified on the page image) ---------------------------------------------------------------------------------------------------------------
## 2000 D6 (p.38): the row between FLORENCE and MARION has its name cut off by the binder-hole punch; the visible tail is 'E' -> LEE (Lee is split between D5 and D6 in 2000).
## 2004 D6 (p.48): the OCR text breaks the COLLETON line in two (383 | 5,980 6,982 5); the image shows one row.
fix <- tribble(~year, ~district, ~county_new, ~why, ~ocr_value,
  2000L, "6", "LEE", "name cut off by the punched hole (visible tail 'E', between FLORENCE and MARION; Lee is split between D5 and D6)", "(blank county)",
  2004L, "6", "COLLETON", "text layer splits the COLLETON row onto two lines; page image shows one row", "(blank county)")
noname <- cells %>% filter(!is_total, is.na(county) | county == "")
stopifnot(nrow(noname %>% distinct(year, district)) == 2, all(paste(noname$year, noname$district) %in% paste(fix$year, fix$district)))
cells <- cells %>% left_join(fix %>% transmute(year, district, county_new), by = c("year", "district")) %>% mutate(county = ifelse(!is_total & (is.na(county) | county == ""), county_new, county)) %>% select(-county_new)
stopifnot(!any(!cells$is_total & (is.na(cells$county) | grepl("^\\?", cells$county))))
stopifnot(!anyDuplicated(cells %>% filter(!is_total) %>% select(year, district, county, col)))

## ---- candidates (names and parties read from the page images; the OCR garbles names: JAHES, HAHPTON, JIH DEHINT, ...) -------------------------------------------------------
Q <- function(...) c(...)
cand <- tribble(~year, ~district, ~col, ~candidate, ~party, ~pgroup,
  2000, "1", 0, "Andy Brack", "Democratic", "DEM", 2000, "1", 1, "Bob Batchelder", "Reform", "OTHER", 2000, "1", 2, "Joseph F. Innella", "Natural Law", "OTHER",
  2000, "1", 3, "Henry E. Brown Jr.", "Republican", "REP", 2000, "1", 4, "Bill Woolsey", "Libertarian", "OTHER", 2000, "1", 5, "Write-In", "Write-In", "OTHER",
  2000, "2", 0, "Jane Frederick", "Democratic", "DEM", 2000, "2", 1, "George C. Taylor", "Natural Law", "OTHER", 2000, "2", 2, "Floyd Spence", "Republican", "REP",
  2000, "2", 3, "Timothy Moultrie", "Libertarian", "OTHER", 2000, "2", 4, "Write-In", "Write-In", "OTHER",
  2000, "3", 0, "George L. Brightharp", "Democratic", "DEM", 2000, "3", 1, "George L. Brightharp", "United Citizens", "OTHER", 2000, "3", 2, "Leroy J. Klein", "Natural Law", "OTHER",
  2000, "3", 3, "Lindsey Graham", "Republican", "REP", 2000, "3", 4, "Adrian Banks", "Libertarian", "OTHER", 2000, "3", 5, "Write-In", "Write-In", "OTHER",
  2000, "4", 0, "Ted Adams", "Constitution", "OTHER", 2000, "4", 1, "Peter J. Ashy", "United Citizens", "OTHER", 2000, "4", 2, "Peter J. Ashy", "Reform", "OTHER",
  2000, "4", 3, "C. Faye Walters", "Natural Law", "OTHER", 2000, "4", 4, "Jim DeMint", "Republican", "REP", 2000, "4", 5, "April Bishop", "Libertarian", "OTHER", 2000, "4", 6, "Write-In", "Write-In", "OTHER",
  2000, "5", 0, "John Spratt", "Democratic", "DEM", 2000, "5", 1, "Carl L. Gullick", "Republican", "REP", 2000, "5", 2, "Tom Campbell", "Libertarian", "OTHER", 2000, "5", 3, "Write-In", "Write-In", "OTHER",
  2000, "6", 0, "James E. \"Jim\" Clyburn", "Democratic", "DEM", 2000, "6", 1, "Dianne L. Nevins", "Natural Law", "OTHER", 2000, "6", 2, "Vince Ellison", "Republican", "REP",
  2000, "6", 3, "Lynwood E. Hines", "Libertarian", "OTHER", 2000, "6", 4, "Write-In", "Write-In", "OTHER",
  2002, "1", 0, "James E. Dunn", "United Citizens", "OTHER", 2002, "1", 1, "Joe Innella", "Natural Law", "OTHER", 2002, "1", 2, "Henry E. Brown Jr.", "Republican", "REP", 2002, "1", 3, "Write-In", "Write-In", "OTHER",
  2002, "2", 0, "Mark Whittington", "United Citizens", "OTHER", 2002, "2", 1, "Joe Wilson", "Republican", "REP", 2002, "2", 2, "James R. \"Jim\" Legg", "Libertarian", "OTHER", 2002, "2", 3, "Write-In", "Write-In", "OTHER",
  2002, "3", 0, "George L. Brightharp", "Democratic", "DEM", 2002, "3", 1, "J. Gresham Barrett", "Republican", "REP", 2002, "3", 2, "Mike Boerste", "Libertarian", "OTHER", 2002, "3", 3, "Write-In", "Write-In", "OTHER",
  2002, "4", 0, "Peter J. Ashy", "Democratic", "DEM", 2002, "4", 1, "Peter J. Ashy", "United Citizens", "OTHER", 2002, "4", 2, "C. Faye Walters", "Natural Law", "OTHER",
  2002, "4", 3, "Jim DeMint", "Republican", "REP", 2002, "4", 4, "Write-In", "Write-In", "OTHER",
  2002, "5", 0, "John Spratt", "Democratic", "DEM", 2002, "5", 1, "Steve Lefemine", "Constitution", "OTHER", 2002, "5", 2, "Doug Kendall", "Libertarian", "OTHER", 2002, "5", 3, "Write-In", "Write-In", "OTHER",
  2002, "6", 0, "James E. \"Jim\" Clyburn", "Democratic", "DEM", 2002, "6", 1, "Gary McLeod", "Republican", "REP", 2002, "6", 2, "R. Craig Augenstein", "Libertarian", "OTHER", 2002, "6", 3, "Write-In", "Write-In", "OTHER",
  2004, "1", 0, "James E. Dunn", "Green", "OTHER", 2004, "1", 1, "Henry E. Brown Jr.", "Republican", "REP", 2004, "1", 2, "Write-In", "Write-In", "OTHER",
  2004, "2", 0, "Steve Lefemine", "Constitution", "OTHER", 2004, "2", 1, "Joe Wilson", "Republican", "REP", 2004, "2", 2, "Michael R. Ellisor", "Democratic", "DEM", 2004, "2", 3, "Write-In", "Write-In", "OTHER",
  2004, "3", 0, "J. Gresham Barrett", "Republican", "REP", 2004, "3", 1, "Write-In", "Write-In", "OTHER",
  2004, "4", 0, "C. Faye Walters", "Green", "OTHER", 2004, "4", 1, "Bob Inglis", "Republican", "REP", 2004, "4", 2, "Brandon P. Brown", "Democratic", "DEM", 2004, "4", 3, "Write-In", "Write-In", "OTHER",
  2004, "5", 0, "Albert F. Spencer", "Republican", "REP", 2004, "5", 1, "John Spratt", "Democratic", "DEM", 2004, "5", 2, "Write-In", "Write-In", "OTHER",
  2004, "6", 0, "Gary McLeod", "Constitution", "OTHER", 2004, "6", 1, "Gary McLeod", "Republican", "REP", 2004, "6", 2, "James E. \"Jim\" Clyburn", "Democratic", "DEM", 2004, "6", 3, "Write-In", "Write-In", "OTHER",
  2006, "1", 0, "James E. Dunn", "Green", "OTHER", 2006, "1", 1, "Henry E. Brown Jr.", "Republican", "REP", 2006, "1", 2, "Randy Maatta", "Democratic", "DEM", 2006, "1", 3, "Randy Maatta", "Working Families", "OTHER", 2006, "1", 4, "Write-In", "Write-In", "OTHER",
  2006, "2", 0, "Joe Wilson", "Republican", "REP", 2006, "2", 1, "Michael Ray Ellisor", "Democratic", "DEM", 2006, "2", 2, "Write-In", "Write-In", "OTHER",
  2006, "3", 0, "J. Gresham Barrett", "Republican", "REP", 2006, "3", 1, "Lee Ballenger", "Democratic", "DEM", 2006, "3", 2, "Lee Ballenger", "Working Families", "OTHER", 2006, "3", 3, "Write-In", "Write-In", "OTHER",
  2006, "4", 0, "C. Faye Walters", "Green", "OTHER", 2006, "4", 1, "Bob Inglis", "Republican", "REP", 2006, "4", 2, "John Cobin", "Libertarian", "OTHER", 2006, "4", 3, "William Griff Griffith", "Democratic", "DEM", 2006, "4", 4, "Write-In", "Write-In", "OTHER",
  2006, "5", 0, "Ralph Norman", "Republican", "REP", 2006, "5", 1, "John Spratt", "Democratic", "DEM", 2006, "5", 2, "Write-In", "Write-In", "OTHER",
  2006, "6", 0, "Antonio Williams", "Green", "OTHER", 2006, "6", 1, "Gary McLeod", "Republican", "REP", 2006, "6", 2, "James E. \"Jim\" Clyburn", "Democratic", "DEM", 2006, "6", 3, "Write-In", "Write-In", "OTHER") %>% mutate(year = as.integer(year), col = as.integer(col))
stopifnot(!anyDuplicated(cand[, c("year", "district", "col")]))
## the 2000 District 1 Natural Law column is printed 'BOB BATCHELDER' (same as the Reform column); the NL nominee was Joseph F. Innella (Wikipedia: 1,110 votes) - logged as a flagged name correction.
## the two party lines of one person (Brightharp, Ashy, Batchelder?, McLeod, Maatta, Ballenger) are kept as separate rows (one per party line).
cells <- cells %>% rename(candidate_ocr = candidate) %>% left_join(cand, by = c("year", "district", "col"))
stopifnot(!anyNA(cells$candidate))
tot <- cells %>% filter(is_total == 1) %>% select(year, district, col, candidate, party, printed = votes)
sums <- cells %>% filter(!is_total) %>% group_by(year, district, col) %>% summarise(sum = sum(votes), n_cty = n_distinct(county), .groups = "drop")
tie <- tot %>% left_join(sums, by = c("year", "district", "col")); stopifnot(all(tie$sum == tie$printed))
cat("STATE TOTAL check:", nrow(tie), "district-candidate columns, all equal the county sums\n")
cat("counties per district-year:\n"); print(as.data.frame(tie %>% group_by(year, district) %>% summarise(counties = first(n_cty), candidates = n(), .groups = "drop") %>% tidyr::pivot_wider(names_from = district, values_from = counties, id_cols = year)))

## ---- FIPS + long/shares files ----------------------------------------------------------------------------------------------------------------------------------------
raw <- cells %>% filter(!is_total) %>% mutate(county_fips = xw$county_fips[match(norm(county), xw$key)]); stopifnot(!anyNA(raw$county_fips))
raw <- raw %>% transmute(year, county_fips, district = sprintf("%02d", as.integer(district)), candidate, party, party_group = pgroup, votes)
## one record per person and party line even when a fusion candidate appears twice; rows with 0 votes are kept out of the long table (they do not change any share)
raw <- raw %>% filter(votes > 0)
fin <- function(y) { r <- raw %>% filter(year == y); l <- finalize_long(r, paste0("sc_", y))
  ## finalize_long canonicalises names (drops punctuation): restore the hand-checked names afterwards
  nm <- cand %>% filter(year == y) %>% distinct(candidate); key <- function(z) tolower(gsub("[^A-Za-z]", "", z)); l$candidate <- nm$candidate[match(key(l$candidate), key(nm$candidate))]; stopifnot(!anyNA(l$candidate)); l }
POST <- list()
for (y in c(2000, 2002, 2004, 2006)) {
  long <- fin(y); save_long(long, paste0("he_sc_", y))
  shares <- derive_shares(long) %>% transmute(state = "SOUTH CAROLINA", year, cty_fips, sample, demovote, repuvote, totalvote); saveRDS(shares, file.path(OUTPUT_DIR, sprintf("elect_he_cty_sc_%d.rds", y)))
  r <- check_long_vs_source(long, file.path(OUTPUT_DIR, sprintf("elect_he_cty_sc_%d.rds", y))); stopifnot(all(r$pass))
  POST[[as.character(y)]] <- list(long = long, shares = shares)
  cat(y, ": counties", n_distinct(long$county_fips), "| split counties", sum(long %>% group_by(county_fips) %>% summarise(d = n_distinct(district)) %>% pull(d) > 1), "| candidates", n_distinct(long$district, long$candidate),
      "| rows", nrow(long), "\n")
}
## the panel must have no SC House rows for these years
pn <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")) %>% filter(sample == "HE", cty_fips %/% 1000 == 45); cat("panel SC House rows by year:", paste(names(table(pn$year)), table(pn$year), sep = ":", collapse = " "), "\n")

## ---- sanity: House total vs presidential total (same year 2000/2004; 2002 vs 2000; 2006 vs 2004) -----------------------------------------------------------------------------
pe <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")) %>% filter(sample == "PE") %>% select(year, cty_fips, pe = totalvote)
for (y in c(2000, 2002, 2004, 2006)) { ref <- c(`2000` = 2000, `2002` = 2000, `2004` = 2004, `2006` = 2004)[[as.character(y)]]
  r <- POST[[as.character(y)]]$shares %>% left_join(pe %>% filter(year == ref) %>% select(-year), by = "cty_fips") %>% mutate(ratio = totalvote / pe)
  cat("House", y, "/ presidential", ref, ": min", round(min(r$ratio, na.rm = TRUE), 2), "median", round(median(r$ratio, na.rm = TRUE), 2), "max", round(max(r$ratio, na.rm = TRUE), 2), "\n") }

## ---- Wikipedia statewide candidate totals ---------------------------------------------------------------------------------------------------------------------------------
wp <- function(y) { f <- file.path(PROJECT_ROOT, "R/data/raw_election/wikipedia_house", sprintf("%d_United_States_House_of_Representatives_elections_in_South_Carolina.txt", y)); if (!file.exists(f)) return(NULL); t <- paste(readLines(f, warn = FALSE), collapse = "\n")
  m <- regmatches(t, gregexpr("\\|\\s*votes\\s*=\\s*[0-9,]+", t))[[1]]; as.numeric(gsub("[^0-9]", "", m)) }
cmp <- list()
for (y in c(2000, 2002, 2004, 2006)) { w <- wp(y); if (is.null(w)) next
  st <- POST[[as.character(y)]]$long %>% group_by(district, candidate, party) %>% summarise(v = sum(votes), .groups = "drop") %>% filter(candidate != "Write-In") %>%
    group_by(district, candidate) %>% summarise(v_person = sum(v), v_lines = paste(v, collapse = "+"), .groups = "drop")            # a person on 2 party lines: Wikipedia shows the sum
  st$in_wikipedia <- st$v_person %in% w; cmp[[as.character(y)]] <- st %>% mutate(year = y) }
cmp <- bind_rows(cmp); cat("\nstatewide candidate totals found on Wikipedia:", sum(cmp$in_wikipedia), "of", nrow(cmp), "\n"); print(as.data.frame(cmp %>% filter(!in_wikipedia)))

## ---- 2008 parser test: the 2008 report (clean text) against the ENR-XML build --------------------------------------------------------------------------------------------------
tf <- tempfile(fileext = ".txt"); system2("pdftotext", c("-layout", "-f", "58", "-l", "59", shQuote(file.path(PROJECT_ROOT, "R/data/county_house_files/SC_Election_Report_2008.pdf")), shQuote(tf)))
L <- gsub("‐", "-", readLines(tf, warn = FALSE)); dist <- NA; rows <- list()
for (l in L) { if (grepl("\\(Vote For", l)) dist <- if (grepl("U\\.S\\. House of Representatives District", l)) sprintf("%02d", as.integer(sub(".*District (\\d+).*", "\\1", l))) else NA_character_
  m <- regmatches(l, regexec("^\\s*([A-Za-z]+)\\s+([0-9][0-9,]*(?:\\s+[0-9][0-9,]*)*)\\s*$", l))[[1]]
  if (length(m) && !m[2] %in% c("Totals", "County") && !is.na(dist)) { v <- as.numeric(gsub(",", "", strsplit(trimws(m[3]), "\\s+")[[1]])); rows[[length(rows) + 1]] <- tibble(district = dist, county = toupper(m[2]), total = v[length(v)], n = length(v)) } }
r08 <- bind_rows(rows) %>% mutate(county_fips = xw$county_fips[match(norm(county), xw$key)])
x08 <- readRDS(file.path(LONG_DIR, "he_sc_2008.rds")) %>% group_by(district, county_fips) %>% summarise(total_long = sum(votes), .groups = "drop")
j <- r08 %>% full_join(x08, by = c("district", "county_fips")); cat("\n2008 report pp.58-59 vs he_sc_2008.rds: county-district rows", nrow(j), "| identical totals:", sum(j$total == j$total_long, na.rm = TRUE), "| missing on one side:", sum(is.na(j$total) | is.na(j$total_long)), "\n")
print(as.data.frame(j %>% filter(is.na(total) | is.na(total_long) | total != total_long)))

## ---- corrections log --------------------------------------------------------------------------------------------------------------------------------------------------------------------
log <- bind_rows(
  fix %>% transmute(year, district, county = county_new, ocr_value, corrected_value = county_new, how_verified = why),
  tibble(year = 2000L, district = "1", county = "(NL column)", ocr_value = "BOB BATCHELDER (Natural Law)", corrected_value = "Joseph F. Innella (Natural Law)",
         how_verified = "the book prints Batchelder over both the Reform and the Natural Law column; Wikipedia gives Bob Batchelder 2,067 and Joseph F. Innella 1,110 (= the NL column total); FLAGGED name correction, votes unchanged"))
write_csv(log, file.path(OUTPUT_DIR, "sc_ocr_corrections_2000_2006.csv")); print(as.data.frame(log))
