## Utah U.S. House, county-level, 2000/2002/2004/2006 general elections, from vote.utah.gov's official canvass books
## (R/data/county_house_files/utah/<year>Gen.pdf; downloaded from https://vote.utah.gov/historical-election-results/). Born-digital PDFs (confirmed via
## pdftotext text-length check), no OCR needed -- extracted with pdftools::pdf_data() word coordinates rather than pdftotext -layout's fixed-width text,
## because several years pack 2-3 unrelated races' columns side by side on one page and fixed-width alignment is fragile across that many columns.
## Utah had exactly 3 U.S. House districts this whole span (a 4th was added after 2010 redistricting, same fact already used by 01bm's 2008/2010 build).
## Companion build: 01bm_house_county_utah_historical_xls.R (2008/2010) and 01aw_house_county_utah.R (2012-2014, OpenElections). 1990-1998 (scanned,
## needs OCR) is a separate build. Together these close Utah's full 1990-2010 pre-OpenElections gap.
##
## KNOWN PITFALL from the 2012-2014 Utah build (01aw), reconfirmed here: this state's canvass PDFs print "State House District N" (state legislature)
## tables in the SAME document as "U.S. House District N" -- every page/text search below anchors on "U.S." + "House" + "District" together, never a
## bare "House District N" match.
##
## Per-year layout, each hand-verified against the PDF's own printed statewide total for that district before trusting the parser:
##  - 2000: ONE PAGE PER DISTRICT (pages 3/4/5), but each page ALSO carries an unrelated race's columns interleaved on the same page (Senate on the
##    District-1 page, an unidentified race on the District-2 page, Governor on the District-3 page) -- confirmed by cross-checking candidate names
##    and vote totals against known 2000 Utah election history (Hatch/Howell = Senate, Smith/Matheson = District 2, Cannon/Dunn = District 3,
##    Leavitt/Walker vs Orton/Hale = Governor). The House columns are identified by an x-coordinate boundary read off the "U.S. House District N"
##    title's own x-position relative to the candidate-name header row (hardcoded per district below, verified against the printed total each carries).
##    County names + the shared row grid (y-coordinate) are read from PAGE 1 (the U.S. President table), which uses the same physical row positions as
##    every later page in this print job -- confirmed by exact y-coordinate matches for Beaver/Weber/TOTAL across pages. Utah's "Washington" county
##    name wraps onto 2 lines on page 1 ("Washingto" / "n") -- the vote NUMBERS are on the "Washingto" row's y, not the wrapped "n" line; the row list
##    below is built to reflect that.
##  - 2002: one page per district (pages 1/2/3), county names in the SAME table (no cross-page work needed).
##  - 2004, 2006: all 3 districts side by side on ONE page (page 3 and page 2 respectively), county names in the same table, no unrelated races mixed
##    in (checked).
## Party codes seen: "R" Republican, "D" Democratic, "L" Libertarian, "C"/"IA" Constitution/Independent American (varies by year), "G" Green,
## "NL" Natural Law, "GP" (2006 Green Party), "PC" and "Unaffiliated"/"Write-In" -- anything that is not D/R is OTHER, matching this project's standard.
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(pdftools)
DIR <- file.path(PROJECT_ROOT, "R", "data", "county_house_files", "utah")

xw_cty <- readr::read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  filter(state == "UTAH") %>% distinct(county_fips, county_name) %>% mutate(county_name = toupper(trimws(county_name)))
stopifnot(nrow(xw_cty) == 29)
fips_of <- function(nm) xw_cty$county_fips[match(toupper(trimws(nm)), xw_cty$county_name)]

UT_COUNTIES <- c("Beaver", "Box Elder", "Cache", "Carbon", "Daggett", "Davis", "Duchesne", "Emery", "Garfield", "Grand", "Iron", "Juab", "Kane",
                 "Millard", "Morgan", "Piute", "Rich", "Salt Lake", "San Juan", "Sanpete", "Sevier", "Summit", "Tooele", "Uintah", "Utah",
                 "Wasatch", "Washington", "Wayne", "Weber")
stopifnot(length(UT_COUNTIES) == 29)

party_group_of <- function(code) { code <- toupper(trimws(code)); case_when(code == "D" ~ "DEM", code == "R" ~ "REP", TRUE ~ "OTHER") }

## ---- shared helpers ------------------------------------------------------------------------------------------------------------------------------
cluster_1d <- function(v, gap) { v <- sort(unique(v)); if (!length(v)) return(integer(0)); grp <- cumsum(c(1, diff(v) > gap)); setNames(grp, v) }

## numeric-only word tokens (commas stripped) within an x/y box on one PDF page
num_tokens <- function(pg, x_min = -Inf, x_max = Inf, y_min = -Inf, y_max = Inf) {
  pg %>% filter(x >= x_min, x < x_max, y >= y_min, y <= y_max) %>%
    mutate(clean = gsub(",", "", text)) %>% filter(grepl("^[0-9]+$", clean)) %>% mutate(val = as.numeric(clean))
}

## assign each token to the nearest of a fixed set of row y-positions (tol in points)
assign_row <- function(y, row_ys, tol = 4) { i <- sapply(y, function(yy) { d <- abs(row_ys - yy); w <- which.min(d); if (d[w] <= tol) w else NA_integer_ }); i }
## assign each token to the nearest of a fixed set of column x-positions
assign_col <- function(x, col_xs, tol = 30) { i <- sapply(x, function(xx) { d <- abs(col_xs - xx); w <- which.min(d); if (d[w] <= tol) w else NA_integer_ }); i }

## Build a (county, votes) matrix from a set of tokens, given canonical row y's (named by county, in UT_COUNTIES order) and column x anchors
## (in candidate order). total_check: named vector of expected column totals (from the page's own printed TOTAL row) -- stops if the sum doesn't tie.
build_matrix <- function(toks, row_ys, col_xs, total_check) {
  toks$row <- assign_row(toks$y, row_ys); toks$col <- assign_col(toks$x, col_xs)
  stopifnot(!anyNA(toks$row), !anyNA(toks$col))
  m <- matrix(0, nrow = length(row_ys), ncol = length(col_xs), dimnames = list(names(row_ys), NULL))
  for (i in seq_len(nrow(toks))) m[toks$row[i], toks$col[i]] <- m[toks$row[i], toks$col[i]] + toks$val[i]
  colsum <- colSums(m)
  if (!missing(total_check)) stopifnot(all(colsum == total_check))
  m
}

raw_all <- list()
add_district <- function(year, district, county_votes_mat, candidates, parties) {
  stopifnot(ncol(county_votes_mat) == length(candidates), length(candidates) == length(parties))
  df <- do.call(rbind, lapply(seq_along(candidates), function(k) data.frame(
    year = year, county_fips = fips_of(rownames(county_votes_mat)), district = sprintf("%02d", district),
    candidate = candidates[k], party = parties[k], votes = county_votes_mat[, k], stringsAsFactors = FALSE)))
  raw_all[[length(raw_all) + 1]] <<- df
}

## ============================================================================================================================================
## 2000 -- one page per district (3/4/5), unrelated races interleaved, county rows read from page 1's shared row grid
## ============================================================================================================================================
d2000 <- pdf_data(file.path(DIR, "2000Gen.pdf"))
p1 <- d2000[[1]]
## canonical row y's: take page 1's numeric-row y's between Beaver and TOTAL (30 rows: 29 counties + TOTAL), in ascending y order.
## Filtered to rows containing at least one NUMBER (not just text) -- Utah's "Washington" county name wraps onto its own text-only line
## ("Washingto" at y=556 with no numbers; the real data row "n 47602 32760 ..." is at y=568), which would otherwise count as a spurious 31st row.
row_ys_all <- num_tokens(p1, y_min = 220, y_max = 606) %>% pull(y) %>% unique() %>% sort()
stopifnot(length(row_ys_all) == 30)                                            # 29 counties + TOTAL
row_ys <- setNames(row_ys_all[1:29], UT_COUNTIES); total_y <- row_ys_all[30]

## District 1 (page 3): House columns are the LAST 3 (x >= 340) -- Senate (5 candidates) occupies x < 340
p3 <- d2000[[3]]
t1 <- num_tokens(p3, x_min = 340, y_min = min(row_ys), y_max = total_y - 1)
tot1 <- num_tokens(p3, x_min = 340, y_min = total_y, y_max = total_y)
col_xs1 <- sort(unique(assign_col(tot1$x, sort(unique(tot1$x)), tol = 0))); col_xs1 <- sort(unique(tot1$x))
m1 <- build_matrix(t1, row_ys, col_xs1, total_check = tot1$val[order(tot1$x)])
add_district(2000, 1, m1, c("Jim Dexter", "Edward Bowen", "James V. Hansen"), c("L", "IA", "R"))

## District 2 (page 4): House columns are the LAST 4 (x >= 300) -- an unrelated race occupies x < 300
p4 <- d2000[[4]]
t2 <- num_tokens(p4, x_min = 300, y_min = min(row_ys), y_max = total_y - 1)
tot2 <- num_tokens(p4, x_min = 300, y_min = total_y, y_max = total_y)
col_xs2 <- sort(unique(tot2$x))
m2 <- build_matrix(t2, row_ys, col_xs2, total_check = tot2$val[order(tot2$x)])
add_district(2000, 2, m2, c("Derek W. Smith", "Jim Matheson", "Steven Alberts Voris", "Peter Pixton"), c("R", "D", "Unaffiliated", "L"))

## District 3 (page 5): House columns are the FIRST 6 (x < 410) -- Governor occupies x >= 432 (its own column anchor). 410, not 390, because a
## single-digit Tolpinrud (col 6, anchor x=380) value right-aligns as far as x=391 -- 390 clipped it, silently dropping Daggett/Duchesne/Emery/
## Garfield/Morgan/Wayne's values and undershooting the printed total by 39 votes until caught by the total-tie check and traced here.
p5 <- d2000[[5]]
t3 <- num_tokens(p5, x_max = 410, y_min = min(row_ys), y_max = total_y - 1)
tot3 <- num_tokens(p5, x_max = 410, y_min = total_y, y_max = total_y)
col_xs3 <- sort(unique(tot3$x))
m3 <- build_matrix(t3, row_ys, col_xs3, total_check = tot3$val[order(tot3$x)])
add_district(2000, 3, m3, c("Bruce Bangerter", "Chris Cannon", "Donald Dunn", "Michael J. Lehman", "Kitty K. Burton", "Randall Tolpinrud"),
             c("IA", "R", "D", "IA", "L", "NL"))
message("2000: districts built (", nrow(m1), "/", nrow(m2), "/", nrow(m3), " counties), all 3 tie exactly to their own printed total")

## ============================================================================================================================================
## 2002 -- one page per district (1/2/3), county names in the same table, single race per page (no boundary needed)
## ============================================================================================================================================
parse_2002_page <- function(page, year_) {
  pg <- pdf_data(file.path(DIR, "2002Gen.pdf"))[[page]]
  county_rows <- pg %>% filter(text == "COUNTY") %>% pull(y)
  total_y_ <- pg %>% filter(toupper(text) == "TOTAL") %>% pull(y)
  stopifnot(length(total_y_) == 1)
  ## Column x-anchors come from the printed TOTAL row's own numbers (right-aligned, same alignment convention as every county data row), NOT the
  ## header's candidate-name/party-code row -- the header's "Write-in" is a LEFT-aligned word, printed ~35-40pt left of its own column's
  ## right-aligned numbers, which silently dropped every Write-in column's county-level votes (an assign_col miss) until caught by the NA check
  ## in build_matrix. The TOTAL row's first 2 numbers are the REGISTERED VOTERS / NUMBER VOTING totals, not candidates -- dropped by position.
  tot_all <- num_tokens(pg, y_min = total_y_, y_max = total_y_) %>% arrange(x)
  tot <- tot_all[-(1:2), ]
  col_xs <- tot$x
  row_ys_ <- pg %>% filter(y > county_rows, y < total_y_) %>% filter(!grepl("^[0-9,]+$", text)) %>%
    filter(x < 130) %>% pull(y) %>% unique() %>% sort()                        # county-name cells are the leftmost text on each row
  stopifnot(length(row_ys_) == 29)
  names(row_ys_) <- UT_COUNTIES
  toks <- num_tokens(pg, x_min = min(col_xs) - 15, y_min = min(row_ys_), y_max = total_y_ - 1)
  build_matrix(toks, row_ys_, col_xs, total_check = tot$val)
}
m1 <- parse_2002_page(1, 2002); add_district(2002, 1, m1, c("Craig Axford", "Rob Bishop", "Dave Thomas", "Susan Howard", "Charles Johnston", "Cody Judy"),
                                              c("G", "R", "D", "Write-in", "Write-in", "Write-in"))
m2 <- parse_2002_page(2, 2002); add_district(2002, 2, m2, c("Ron Copier", "Patrick Diehl", "Jim Matheson", "John Swallow"), c("L", "G", "D", "R"))
m3 <- parse_2002_page(3, 2002); add_district(2002, 3, m3, c("Kitty K. Burton", "Chris Cannon", "Nancy Jane Woodside", "John William Maurin", "Mark H. Williams"),
                                              c("L", "R", "D", "Write-In", "Write-In"))
message("2002: districts built (", nrow(m1), "/", nrow(m2), "/", nrow(m3), " counties), all 3 tie exactly to their own printed total")

## ============================================================================================================================================
## 2004, 2006 -- all 3 districts side by side on one page; county names in the same table; no unrelated race mixed in
## ============================================================================================================================================
parse_combined_page <- function(pdf_file, page, districts) {
  ## districts: list of list(x_min, x_max, candidates, parties) in left-to-right column order
  pg <- pdf_data(file.path(DIR, pdf_file))[[page]]
  total_y_ <- pg %>% filter(toupper(text) == "TOTAL") %>% pull(y); stopifnot(length(total_y_) == 1)
  county_hdr_y <- pg %>% filter(text == "COUNTY") %>% pull(y); stopifnot(length(county_hdr_y) == 1)
  row_ys_ <- pg %>% filter(y > county_hdr_y, y < total_y_, x < 130) %>% filter(!grepl("^[0-9,]+$", text)) %>% pull(y) %>% unique() %>% sort()
  stopifnot(length(row_ys_) == 29); names(row_ys_) <- UT_COUNTIES
  out <- list()
  for (dd in districts) {
    tot <- num_tokens(pg, x_min = dd$x_min, x_max = dd$x_max, y_min = total_y_, y_max = total_y_)
    col_xs <- sort(unique(tot$x)); stopifnot(length(col_xs) == length(dd$candidates))
    toks <- num_tokens(pg, x_min = dd$x_min, x_max = dd$x_max, y_min = min(row_ys_), y_max = total_y_ - 1)
    m <- build_matrix(toks, row_ys_, col_xs, total_check = tot$val[order(tot$x)])
    out[[length(out) + 1]] <- list(m = m, candidates = dd$candidates, parties = dd$parties)
  }
  out
}

## Column boundaries derived from the printed TOTAL row's own 16 candidate-column x-positions (207,257,299,337 | 395,425,467,502,551,586 | 632,670,
## 720,762,804,859), split at the midpoint between each district's last and the next district's first anchor -- NOT from the district title's own
## x-position, which turned out to be an unreliable guide (a first attempt split naively at "under each title", producing a wrong 4/4/8 split that
## misassigned Jeremy Paul Petersen and John Swallow to District 3; caught because Swallow's resulting district-3 vote share made no sense against
## known 2004 Utah election history -- Swallow challenged Matheson in DISTRICT 2, not 3. Verified: District 1 = Bishop(R) landslide winner; District 2
## = Matheson(D) vs Swallow(R), a real close race; District 3 = Cannon(R) vs Beau/"Beua" Babka(D), Cannon winning big -- all match the historical
## record, and each district's own column sums now tie exactly to the printed total (the build_matrix stopifnot below enforces this).
## District 1's x_min is 180, not -Inf -- the TOTAL row's REGISTERED VOTERS / NUMBER VOTING columns (x=116,165) are not candidates and would
## otherwise be picked up as 2 extra "candidate" columns for whichever district's boundary starts at -Inf.
r2004 <- parse_combined_page("2004Gen.pdf", 3, list(
  list(x_min = 180, x_max = 366, candidates = c("Rob Bishop", "Charles Johnston", "Richard W. Soderberg", "Steven Thompson"), parties = c("R", "C", "PC", "D")),
  list(x_min = 366, x_max = 609, candidates = c("Vena Finau", "Ronald R. Amos", "Patrick S. Diehl", "Jim Matheson", "Jeremy Paul Petersen", "John Swallow"),
       parties = c("WI", "PC", "G", "D", "C", "R")),
  list(x_min = 609, x_max = Inf, candidates = c("Beua Babka", "Chris Cannon", "Jim Dexter", "Curtis Darrell James", "Ronald Winfield", "Nicolle Rivetti"),
       parties = c("D", "R", "L", "PC", "C", "WI"))))
for (i in seq_along(r2004)) add_district(2004, i, r2004[[i]]$m, r2004[[i]]$candidates, r2004[[i]]$parties)
message("2004: districts built, all 3 tie exactly to their own printed total")

## Same rank-based-anchor + historical-sanity-check method as 2004. 2006's TOTAL row has 2 extra trailing numbers (x=644: 569,690 and x=685: 11,394)
## that are NOT a 14th/15th candidate -- 569,690 is exactly the sum of the 13 real candidate totals (a checksum column the report appends), confirmed
## by adding them by hand; excluded by capping District 3's x_max at 629 (the midpoint between the last real column, x=613, and the checksum at x=644).
r2006 <- parse_combined_page("2006Gen.pdf", 2, list(
  list(x_min = 200, x_max = 340, candidates = c("Lynn Badler", "Rob Bishop", "Mark Hudson", "Steven Olsen"), parties = c("L", "R", "C", "D")),
  list(x_min = 340, x_max = 471, candidates = c("Bob Brister", "Lavar Christensen", "Austin Sherwood Lett", "Jim Matheson"), parties = c("GP", "R", "L", "D")),
  list(x_min = 471, x_max = 629, candidates = c("W. David Perry", "Christian Burridge", "Chris Cannon", "Philip Lear Hallman", "Jim Noorlander"), parties = c("C", "D", "R", "L", "C"))))
for (i in seq_along(r2006)) add_district(2006, i, r2006[[i]]$m, r2006[[i]]$candidates, r2006[[i]]$parties)
message("2006: districts built, all 3 tie exactly to their own printed total")

## ============================================================================================================================================
## finalize, save, verify
## ============================================================================================================================================
raw <- bind_rows(raw_all) %>% mutate(party = ifelse(is.na(party) | party == "", "Write-In", party), party_lc = tolower(party),
                                      party_group = case_when(grepl("^d$|democrat", party_lc) ~ "DEM", grepl("^r$|republican", party_lc) ~ "REP", TRUE ~ "OTHER")) %>%
  select(-party_lc)
stopifnot(!anyDuplicated(raw[, c("year", "county_fips", "district", "candidate", "party")]))
cat("party labels:", paste(names(table(raw$party)), table(raw$party), collapse = "; "), "\n")

for (y in sort(unique(raw$year))) save_long(finalize_long(raw %>% filter(year == y) %>% select(year, county_fips, district, candidate, party, party_group, votes), paste0("ut_", y)), paste0("he_ut_", y))

for (y in sort(unique(raw$year))) {
  long <- readRDS(file.path(LONG_DIR, sprintf("he_ut_%d.rds", y)))
  shares <- derive_shares(long) %>% transmute(state = "UTAH", year, cty_fips, sample, demovote, repuvote, totalvote)
  saveRDS(shares, file.path(OUTPUT_DIR, sprintf("elect_he_cty_ut_%d.rds", y)))
  r <- check_long_vs_source(long, file.path(OUTPUT_DIR, sprintf("elect_he_cty_ut_%d.rds", y))); stopifnot(all(r$pass))
  message(y, ": ", nrow(shares), " county-district rows")
}

sanity <- bind_rows(lapply(sort(unique(raw$year)), function(y) readRDS(file.path(OUTPUT_DIR, sprintf("elect_he_cty_ut_%d.rds", y)))))
share <- sanity$repuvote + sanity$demovote
message("Sanity: repuvote+demovote range [", round(min(share), 3), ", ", round(max(share), 3), "]; rows total ", nrow(sanity))
