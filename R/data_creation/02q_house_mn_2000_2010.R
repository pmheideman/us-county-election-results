## Minnesota U.S. House, county level, November general elections 2000, 2002, 2004, 2006, 2008, 2010, from the official election chapters of "Minnesota Votes"
## (Secretary of State / Legislative Reference Library): R/data/county_house_files/MN_<year>-11-0?-g-man.pdf, table "VOTE FOR UNITED STATES REPRESENTATIVE BY COUNTY".
## Pipeline (see R/data/raw_house_county_open_states/minnesota_official/README_minnesota_official.md): pdftotext -bbox word coordinates -> district tables
## (mn_parse_blocks.py) -> mn_raw_cells_<year>.csv (one line per county x candidate column, plus TOTAL rows, NO corrections). This script does everything after that:
## county FIPS, candidate names / parties (read from the page headers and the page images), the 2 corrections below, all checks, the long tables and the shares files.
## Party mapping (Minnesota bug history): DFL -> DEM group, label "Democratic-Farmer-Labor"; R -> REP; everything else (IP, GP, CP, LIB, NNT, IND, PF, UP, IPR, WI write-ins) -> OTHER.
## Party abbreviation labels: IP Independence, GP Green, CP Constitution, LIB Libertarian, NNT No New Taxes (2002 district 2, read from the page image), IND Independent,
##   PF "Party Free" (2010 district 1, Wikipedia), UP "Unity" (2006 district 8, Wikipedia: Unity America), IPR Independent Progressive (2010 district 5), WI write-in.
## Write-in columns printed in the tables (2002-2010, one "Write-In" column per district, named write-ins in 2006/2008) are real votes and are kept (OTHER); 2000 prints none.
## Verification (all stop the script when violated): every county row has one cell per candidate column; every county appears once per district; county sums == the printed TOTAL row
## for every candidate column; each district total appears in Wikipedia's district results (independent cross-check, printed); House total vs presidential / Senate total of the panel per county.
## Corrections (also written to R/output/mn_ocr_corrections_2000_2010.csv):
##   * 2000 district 5 Sabo, Hennepin (and the TOTAL row): the book prints "17,6629" both times (a typo in the book itself, visible on the page image); Wikipedia's official total for
##     Sabo is 176,629 (69.2%) -> 176,629.
##   * 2006 district 6 Binkowski (IP), Sherburne: the book prints 393 (page image confirms), but the counties add up to 21,557 vs the printed TOTAL 23,557 (= Wikipedia's 23,557); Binkowski's
##     share is 7-9% in every other county and 1.3% in Sherburne, so the county cell is 2,393 (INFERRED from the totals, the only single-cell fix that reconciles them).
## Zero-only county-district blocks (Hennepin in district 2 and Ramsey in district 6, printed with 0 votes) carry no votes and are dropped.
## Outputs: R/output/long/he_mn_<year>.rds, R/output/elect_he_cty_mn_<year>.rds (NEW files), R/output/mn_ocr_corrections_2000_2010.csv.
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "minnesota_official")
YEARS <- c(2000, 2002, 2004, 2006, 2008, 2010)

## ---- candidates by printed column (year, district, column number counted left to right) -----------------------------------------------------------------------
cand <- tribble(~year, ~district, ~col, ~candidate, ~party_code,
  ## 2000 (tables print no write-in column)
  2000, 1, 1, "Mary Rieder", "DFL", 2000, 1, 2, "Gil Gutknecht", "R", 2000, 1, 3, "Rich Osness", "LIB",
  2000, 2, 1, "Dennis A. Burda", "CP", 2000, 2, 2, "Gerald W. Brekke", "IP", 2000, 2, 3, "David Minge", "DFL", 2000, 2, 4, "Mark Kennedy", "R", 2000, 2, 5, "Ron Helwig", "LIB",
  2000, 3, 1, "Arne Niska", "CP", 2000, 3, 2, "Sue Shuff", "DFL", 2000, 3, 3, "Jim Ramstad", "R", 2000, 3, 4, "Bob Odden", "LIB",
  2000, 4, 1, "Nicholas Skrivanek", "CP", 2000, 4, 2, "Tom Foley", "IP", 2000, 4, 3, "Betty McCollum", "DFL", 2000, 4, 4, "Linda Runbeck", "R",
  2000, 5, 1, "Renee Lavoi", "CP", 2000, 5, 2, "Rob Tomich", "IP", 2000, 5, 3, "Martin Olav Sabo", "DFL", 2000, 5, 4, "Frank Taylor", "R", 2000, 5, 5, "Chuck P. Charnstrom", "LIB",
  2000, 6, 1, "Ralph A. Hubbard", "CP", 2000, 6, 2, "Bill Luther", "DFL", 2000, 6, 3, "John Kline", "R",
  2000, 7, 1, "Owen Sivertson", "CP", 2000, 7, 2, "Collin C. Peterson", "DFL", 2000, 7, 3, "Glen Menze", "R",
  2000, 8, 1, "James L. Oberstar", "DFL", 2000, 8, 2, "Bob Lemen", "R", 2000, 8, 3, "Mike Darling", "IND",
  ## 2002
  2002, 1, 1, "Greg Mikkelson", "GP", 2002, 1, 2, "Gil Gutknecht", "R", 2002, 1, 3, "Steve Andreasen", "DFL", 2002, 1, 4, "Write-In", "WI",
  2002, 2, 1, "John Kline", "R", 2002, 2, 2, "Bill Luther", "DFL", 2002, 2, 3, "Samuel D. Garst", "NNT", 2002, 2, 4, "Write-In", "WI",
  2002, 3, 1, "Jim Ramstad", "R", 2002, 3, 2, "Darryl Stanton", "DFL", 2002, 3, 3, "Write-In", "WI",
  2002, 4, 1, "Scott J. Raskiewicz", "GP", 2002, 4, 2, "Clyde Billington", "R", 2002, 4, 3, "Betty McCollum", "DFL", 2002, 4, 4, "Write-In", "WI",
  2002, 5, 1, "Tim Davis", "GP", 2002, 5, 2, "Daniel Nielsen Mathias", "R", 2002, 5, 3, "Martin Olav Sabo", "DFL", 2002, 5, 4, "Write-In", "WI",
  2002, 6, 1, "Dan Becker", "IP", 2002, 6, 2, "Mark R. Kennedy", "R", 2002, 6, 3, "Janet Robert", "DFL", 2002, 6, 4, "Write-In", "WI",
  2002, 7, 1, "Dan Stevens", "R", 2002, 7, 2, "Collin C. Peterson", "DFL", 2002, 7, 3, "Write-In", "WI",
  2002, 8, 1, "Bob Lemen", "R", 2002, 8, 2, "James L. Oberstar", "DFL", 2002, 8, 3, "Write-In", "WI",
  ## 2004
  2004, 1, 1, "Gregory Mikkelson", "IP", 2004, 1, 2, "Gil Gutknecht", "R", 2004, 1, 3, "Leigh Pomeroy", "DFL", 2004, 1, 4, "Write-In", "WI",
  2004, 2, 1, "Doug Williams", "IP", 2004, 2, 2, "John Kline", "R", 2004, 2, 3, "Teresa Daly", "DFL", 2004, 2, 4, "Write-In", "WI",
  2004, 3, 1, "Jim Ramstad", "R", 2004, 3, 2, "Deborah Watts", "DFL", 2004, 3, 3, "Write-In", "WI",
  2004, 4, 1, "Peter F. Vento", "IP", 2004, 4, 2, "Patrice Bataglia", "R", 2004, 4, 3, "Betty McCollum", "DFL", 2004, 4, 4, "Write-In", "WI",
  2004, 5, 1, "Jay Pond", "GP", 2004, 5, 2, "Daniel Mathias", "R", 2004, 5, 3, "Martin Olav Sabo", "DFL", 2004, 5, 4, "Write-In", "WI",
  2004, 6, 1, "Mark Kennedy", "R", 2004, 6, 2, "Patty Wetterling", "DFL", 2004, 6, 3, "Write-In", "WI",
  2004, 7, 1, "David E. Sturrock", "R", 2004, 7, 2, "Collin C. Peterson", "DFL", 2004, 7, 3, "Write-In", "WI",
  2004, 8, 1, "Van Presley", "GP", 2004, 8, 2, "Mark Groettum", "R", 2004, 8, 3, "James L. Oberstar", "DFL", 2004, 8, 4, "Write-In", "WI",
  ## 2006 (named write-ins are printed as their own columns; no aggregate write-in column)
  2006, 1, 1, "Gil Gutknecht", "R", 2006, 1, 2, "Tim Walz", "DFL", 2006, 1, 3, "Stephen Williams", "WI",
  2006, 2, 1, "Douglas Williams", "IP", 2006, 2, 2, "John Kline", "R", 2006, 2, 3, "Coleen Rowley", "DFL",
  2006, 3, 1, "Jim Ramstad", "R", 2006, 3, 2, "Wendy Wilde", "DFL",
  2006, 4, 1, "Obi Sium", "R", 2006, 4, 2, "Betty McCollum", "DFL", 2006, 4, 3, "Tom Fiske", "WI",
  2006, 5, 1, "Tammy Lee", "IP", 2006, 5, 2, "Alan Fine", "R", 2006, 5, 3, "Keith Ellison", "DFL", 2006, 5, 4, "Jay Pond", "GP", 2006, 5, 5, "Larry Leininger", "WI", 2006, 5, 6, "Julian Santana", "WI",
  2006, 6, 1, "John Paul Binkowski", "IP", 2006, 6, 2, "Michele Bachmann", "R", 2006, 6, 3, "Patty Wetterling", "DFL",
  2006, 7, 1, "Michael J. Barrett", "R", 2006, 7, 2, "Collin C. Peterson", "DFL", 2006, 7, 3, "Ken Lucier", "CP",
  2006, 8, 1, "Rod Grams", "R", 2006, 8, 2, "James L. Oberstar", "DFL", 2006, 8, 3, "Harry Welty", "UP",
  ## 2008
  2008, 1, 1, "Gregory Mikkelson", "IP", 2008, 1, 2, "Brian J. Davis", "R", 2008, 1, 3, "Tim Walz", "DFL", 2008, 1, 4, "Write-In", "WI",
  2008, 2, 1, "John Kline", "R", 2008, 2, 2, "Steve Sarvi", "DFL", 2008, 2, 3, "Write-In", "WI", 2008, 2, 4, "Kevin Masrud", "WI", 2008, 2, 5, "Curt Walor", "WI",
  2008, 3, 1, "David Dillon", "IP", 2008, 3, 2, "Erik Paulsen", "R", 2008, 3, 3, "Ashwin Madia", "DFL", 2008, 3, 4, "Write-In", "WI", 2008, 3, 5, "Harley Swarm Jr.", "WI",
  2008, 4, 1, "Ed Matthews", "R", 2008, 4, 2, "Betty McCollum", "DFL", 2008, 4, 3, "Write-In", "WI", 2008, 4, 4, "Amber Garlan", "WI",
  2008, 5, 1, "Bill McGaughey", "IP", 2008, 5, 2, "Barb Davis White", "R", 2008, 5, 3, "Keith Ellison", "DFL", 2008, 5, 4, "Write-In", "WI",
  2008, 6, 1, "Bob Anderson", "IP", 2008, 6, 2, "Michele Bachmann", "R", 2008, 6, 3, "Elwyn Tinklenberg", "DFL", 2008, 6, 4, "Write-In", "WI", 2008, 6, 5, "Aubrey Immelman", "WI",
  2008, 7, 1, "Glen Menze", "R", 2008, 7, 2, "Collin C. Peterson", "DFL", 2008, 7, 3, "Write-In", "WI", 2008, 7, 4, "Leon Carlton Williams", "WI",
  2008, 8, 1, "Michael Cummins", "R", 2008, 8, 2, "James L. Oberstar", "DFL", 2008, 8, 3, "Write-In", "WI",
  ## 2010
  2010, 1, 1, "Steve Wilson", "IP", 2010, 1, 2, "Randy Demmer", "R", 2010, 1, 3, "Tim Walz", "DFL", 2010, 1, 4, "Lars Johnson", "PF", 2010, 1, 5, "Write-In", "WI",
  2010, 2, 1, "John Kline", "R", 2010, 2, 2, "Shelly Madore", "DFL", 2010, 2, 3, "Write-In", "WI",
  2010, 3, 1, "Jon Olseon", "IP", 2010, 3, 2, "Erik Paulsen", "R", 2010, 3, 3, "Jim Meffert", "DFL", 2010, 3, 4, "Write-In", "WI",
  2010, 4, 1, "Steve Carlson", "IP", 2010, 4, 2, "Teresa Collett", "R", 2010, 4, 3, "Betty McCollum", "DFL", 2010, 4, 4, "Write-In", "WI",
  2010, 5, 1, "Tom Schrunk", "IP", 2010, 5, 2, "Joel Demos", "R", 2010, 5, 3, "Keith Ellison", "DFL", 2010, 5, 4, "Lynne Torgerson", "IND", 2010, 5, 5, "Michael James Cavlan", "IPR", 2010, 5, 6, "Write-In", "WI",
  2010, 6, 1, "Bob Anderson", "IP", 2010, 6, 2, "Michele Bachmann", "R", 2010, 6, 3, "Tarryl Clark", "DFL", 2010, 6, 4, "Aubrey Immelsman", "IND", 2010, 6, 5, "Write-In", "WI",
  2010, 7, 1, "Glen R. Menze", "IP", 2010, 7, 2, "Lee Byberg", "R", 2010, 7, 3, "Collin C. Peterson", "DFL", 2010, 7, 4, "Gene Waldorf", "IND", 2010, 7, 5, "Write-In", "WI",
  2010, 8, 1, "Timothy Olson", "IP", 2010, 8, 2, "Chip Cravaack", "R", 2010, 8, 3, "James L. Oberstar", "DFL", 2010, 8, 4, "Richard \"George\" Burton", "CP", 2010, 8, 5, "Write-In", "WI")
PARTY_LABEL <- c(DFL = "Democratic-Farmer-Labor", R = "Republican", IP = "Independence", GP = "Green", CP = "Constitution", LIB = "Libertarian", NNT = "No New Taxes",
                 IND = "Independent", PF = "Party Free", UP = "Unity", IPR = "Independent Progressive", WI = "Write-In")
cand <- cand %>% mutate(party = PARTY_LABEL[party_code], party_group = case_when(party_code == "DFL" ~ "DEM", party_code == "R" ~ "REP", TRUE ~ "OTHER"))
stopifnot(!anyDuplicated(cand[, c("year", "district", "col")]), !anyNA(cand$party))

## ---- raw cells --------------------------------------------------------------------------------------------------------------------------------------------------
cells <- bind_rows(lapply(YEARS, function(y) read_csv(file.path(DIR, sprintf("mn_raw_cells_%d.csv", y)), col_types = cols(.default = "c")))) %>%
  mutate(across(c(year, district, col, ncells, ncol), as.integer))
cells <- cells %>% filter(!(ncells == 0 & county_raw != "TOTAL"))                    # label-only rows (2010 district 7 "LAKE OF THE" / "WOODS" wraps onto two lines)

## corrections (applied to the raw text, TOTAL rows included)
fixes <- tribble(~year, ~district, ~county, ~col, ~ocr_value, ~corrected_value, ~how_verified,
  2000, 5, "HENNEPIN", 3, "17,6629", "176,629", "book prints 17,6629 in the county row and in the TOTAL row (page image); Wikipedia MN-5 2000 Sabo 176,629 (69.2%)",
  2000, 5, "TOTAL", 3, "17,6629", "176,629", "same typo in the printed TOTAL row",
  2006, 6, "SHERBURNE", 1, "393", "2,393", "INFERRED: county cells add to 21,557 but the printed TOTAL is 23,557 (= Wikipedia); page image shows 393; Binkowski is 7-9% elsewhere, 1.3% here")
for (i in seq_len(nrow(fixes))) {
  k <- cells$year == fixes$year[i] & cells$district == fixes$district[i] & cells$county_raw == fixes$county[i] & cells$col == fixes$col[i]
  stopifnot(sum(k) == 1, cells$votes_raw[k] == fixes$ocr_value[i]); cells$votes_raw[k] <- fixes$corrected_value[i] }
write_csv(fixes %>% rename(county_or_row = county), file.path(OUTPUT_DIR, "mn_ocr_corrections_2000_2010.csv"))

## ---- counties -> FIPS ---------------------------------------------------------------------------------------------------------------------------------------------
xw <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  filter(state == "MINNESOTA", !is.na(county_fips)) %>% distinct(county_name, county_fips)
norm <- function(z) gsub("[^A-Z]", "", toupper(z)); xwk <- xw %>% mutate(key = norm(county_name)) %>% distinct(key, county_fips); stopifnot(n_distinct(xwk$county_fips) == 87)
ALIAS <- c(LAKEOFTHE = "LAKEOFTHEWOODS", LAKEOFTHEWDS = "LAKEOFTHEWOODS", LKOFTHEWDS = "LAKEOFTHEWOODS", LCQUIPARLE = "LACQUIPARLE", LESUEU = "LESUEUR",
           SCON = "SCOTT", YELLOWMED = "YELLOWMEDICINE", YLLWMED = "YELLOWMEDICINE", SAINTLOUIS = "STLOUIS")   # abbreviations of the 2004/2008 tables; "scon" = OCR of SCOTT (2000)
k <- norm(cells$county_raw); k <- ifelse(k %in% names(ALIAS), ALIAS[k], k)
cells$county_fips <- ifelse(cells$county_raw == "TOTAL", NA, xwk$county_fips[match(k, xwk$key)])
stopifnot(!anyNA(cells$county_fips[cells$county_raw != "TOTAL"]))

## ---- verification -------------------------------------------------------------------------------------------------------------------------------------------------
num <- function(z) as.numeric(gsub(",", "", z))
stopifnot(all(cells$ncells == cells$ncol), !anyNA(num(cells$votes_raw)), all(grepl("^[0-9]{1,3}(,[0-9]{3})*$|^[0-9]+$", cells$votes_raw)))
body <- cells %>% filter(county_raw != "TOTAL") %>% mutate(votes = num(votes_raw)); tots <- cells %>% filter(county_raw == "TOTAL") %>% transmute(year, district, col, printed = num(votes_raw))
stopifnot(!anyDuplicated(body[, c("year", "district", "county_fips", "col")]))
chk <- body %>% group_by(year, district, col) %>% summarise(sum = sum(votes), n_counties = n(), .groups = "drop") %>% inner_join(tots, by = c("year", "district", "col")) %>%
  left_join(cand, by = c("year", "district", "col"))
stopifnot(!anyNA(chk$candidate), nrow(chk) == nrow(cand), all(chk$sum == chk$printed))
cat("candidate columns:", nrow(chk), "| all county sums equal the printed TOTAL row (", sum(chk$sum == chk$printed), "of", nrow(chk), ")\n")
cnt <- body %>% group_by(year, district) %>% summarise(counties = n_distinct(county_fips), .groups = "drop")
cat("counties per district:\n"); print(as.data.frame(cnt %>% tidyr::pivot_wider(names_from = district, values_from = counties)))
## independent cross-check: the printed district total of every named candidate appears in Wikipedia's district results
wp <- read_csv(file.path(DIR, "wp_mn_district_totals_2000_2010.csv"), show_col_types = FALSE)
xc <- chk %>% filter(party_code != "WI") %>% rowwise() %>% mutate(in_wikipedia = printed %in% wp$votes[wp$year == year & wp$district == district]) %>% ungroup()
cat("named candidates whose district total is found in Wikipedia:", sum(xc$in_wikipedia), "of", nrow(xc), "\n")
print(as.data.frame(xc %>% filter(!in_wikipedia) %>% select(year, district, candidate, party_code, printed)))

## ---- long tables and shares ----------------------------------------------------------------------------------------------------------------------------------------
raw <- body %>% left_join(cand %>% select(year, district, col, candidate, party, party_group), by = c("year", "district", "col")) %>%
  group_by(year, district, county_fips) %>% filter(sum(votes) > 0) %>% ungroup()                        # drops the zero-only Hennepin (district 2) / Ramsey (district 6) blocks
dropped <- body %>% group_by(year, district, county_fips) %>% filter(sum(votes) == 0) %>% distinct(year, district, county_fips)
cat("zero-only county-district blocks dropped:", nrow(dropped), "\n"); print(as.data.frame(dropped))
for (y in YEARS) {
  r <- raw %>% filter(year == y) %>% transmute(year, county_fips, district = sprintf("%02d", district), candidate, party, party_group, votes)
  long <- finalize_long(r, paste0("mn_", y)); save_long(long, paste0("he_mn_", y))
  shares <- derive_shares(long) %>% transmute(state = "MINNESOTA", year, cty_fips, sample, demovote, repuvote, totalvote)
  saveRDS(shares, file.path(OUTPUT_DIR, sprintf("elect_he_cty_mn_%d.rds", y)))
  res <- check_long_vs_source(long, file.path(OUTPUT_DIR, sprintf("elect_he_cty_mn_%d.rds", y))); stopifnot(res$pass)
  cat(y, ": candidates", n_distinct(long$candidate), "| districts", n_distinct(long$district), "| counties", n_distinct(long$county_fips), "| split counties",
      sum((long %>% group_by(county_fips) %>% summarise(d = n_distinct(district)) %>% pull(d)) > 1), "| total votes", format(sum(long$votes), big.mark = ","), "\n")
}

## ---- sanity: House total vs presidential / Senate total in the panel (county level) ------------------------------------------------------------------------------
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")) %>% filter(cty_fips %/% 1000 == 27)
cat("\npanel Minnesota House rows before 2012:", sum(panel$sample == "HE" & panel$year < 2012), "(0 expected)\n")
for (y in YEARS) {
  h <- readRDS(file.path(OUTPUT_DIR, sprintf("elect_he_cty_mn_%d.rds", y))); ref_year <- if (y %% 4 == 0) y else y - 2
  ref <- panel %>% filter(year %in% c(y, ref_year), sample %in% c("PE", "SE")) %>% arrange(desc(year == y)) %>% group_by(cty_fips) %>% slice(1) %>% ungroup() %>% select(cty_fips, ref = totalvote, ref_sample = sample, ref_y = year)
  r <- h %>% inner_join(ref, by = "cty_fips") %>% mutate(ratio = totalvote / ref)
  cat(y, ": vs", paste(unique(paste(r$ref_sample, r$ref_y)), collapse = "/"), "(", nrow(r), "counties ) ratio min", round(min(r$ratio), 2), "median", round(median(r$ratio), 2), "max", round(max(r$ratio), 2), "\n")
}
