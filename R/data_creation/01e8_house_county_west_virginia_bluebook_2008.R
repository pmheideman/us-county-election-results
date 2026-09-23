## West Virginia House 2008: the same "2011" Blue Book edition used for 2010 (01e5) also carries the
## "2008 General Election Returns" sub-section (pp. 731-733), immediately before the 2010 section --
## a clean U.S. House table per district, county rows, a printed TOTAL row. This closes the LAST
## remaining WV House gap: 48/55 counties via OpenElections, extended here to 55/55.
##
## CD1 (Mollohan, D, opposed only by 2 write-in candidates) 20 counties, CD2 (Barth D / *Capito R)
## 18 counties, CD3 (Rahall D / Gearheart R) 17 counties = 55.
##
## Verified: every district's county column sums to the book's own printed TOTAL row exactly. Two
## write-in cells were OCR-blank (Mineral's "Osgood" column, Pleasants' "Smith" column) and one R
## write-in cell in CD2 (Upshur's "Mill" column) printed as a garbled "d" -- all three resolved to
## their correct value (1, 1, 0 respectively) via the same total-row-tie-out technique used
## throughout this project (see e.g. the Kentucky/Oklahoma notes in the project's own memory).
##
## Cross-checked against the existing OpenElections build for all 48 overlap counties: 32 of 48
## differ, several by a lot (e.g. Wetzel 5,267 vs 331 -- OpenElections' 2008 file is drastically
## incomplete for that county) -- the same "OpenElections' WV precinct files are themselves
## incomplete for a number of counties" problem already documented and resolved the same way for
## 2010 (01e5). The Blue Book, which ties exactly to its own printed totals, is adopted as
## authoritative for the full 55 counties, replacing OpenElections entirely for this year.

source(file.path("R", "00_setup.R"))
library(readr)

county_fips_crosswalk <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))
wv_fips <- county_fips_crosswalk %>% filter(state == "WEST VIRGINIA") %>% select(county_name, county_fips)
stopifnot(nrow(wv_fips) == 55)

## Transcribed from `WVS_Bluebook_2011.pdf` (pdftotext -layout), pp. 731-733.
cd1 <- tribble(
  ~county,       ~dem,   ~osgood, ~smith,   # *Mollohan (D) / Ted Osgood (Write-In) / R.J. Smith (Write-In)
  "BARBOUR",      4798,   3,       0,
  "BROOKE",       8080,   1,       2,
  "DODDRIDGE",    2151,   2,       0,
  "GILMER",       2037,   0,       0,
  "GRANT",        2504,   2,       2,
  "HANCOCK",     10499,   0,       4,
  "HARRISON",    24091,   4,       8,
  "MARION",      18852,  12,       2,
  "MARSHALL",    11070,   2,      15,
  "MINERAL",      8556,   1,       5,   # Osgood cell OCR-blank; resolved to 1 via the district TOTAL check
  "MONONGALIA",  24319,   0,       0,
  "OHIO",        15285,   1,       4,
  "PLEASANTS",    2372,   0,       1,   # Smith cell OCR-blank; resolved to 1 via the district TOTAL check
  "PRESTON",      8746,   9,      11,
  "RITCHIE",      2728,   1,       1,
  "TAYLOR",       4922,   0,       1,
  "TUCKER",       2344,   0,       0,
  "TYLER",        2857,   1,       0,
  "WETZEL",       5259,   7,       1,
  "WOOD",        26264,  23,       4
)
cd1_total <- c(dem = 187734, osgood = 69, smith = 61)

cd2 <- tribble(
  ~county,       ~dem,   ~rep,   ~mill,   # Anne Barth (D) / *Shelley Moore Capito (R) / Aaron Mill (Write-In)
  "BERKELEY",    15273,  21261,   3,
  "BRAXTON",      2873,   2419,   0,
  "CALHOUN",      1183,   1240,   0,
  "CLAY",         1512,   1790,   0,
  "HAMPSHIRE",    2977,   5272,   1,
  "HARDY",        1874,   3478,   0,
  "JACKSON",      5116,   7185,   0,
  "JEFFERSON",   11076,  11058,   3,
  "KANAWHA",     38751,  43601,   3,
  "LEWIS",        2243,   4371,   2,
  "MASON",        4640,   6042,   0,
  "MORGAN",       2624,   4471,   3,
  "PENDLETON",    1253,   2128,   0,
  "PUTNAM",       9211,  15605,   1,
  "RANDOLPH",     4046,   6699,   0,
  "ROANE",        2646,   2981,   0,
  "UPSHUR",       2614,   6359,   0,   # Mill cell OCR-garbled ("d"); resolved to 0 via the district TOTAL check
  "WIRT",          907,   1374,   0
)
cd2_total <- c(dem = 110819, rep = 147334, mill = 16)

cd3 <- tribble(
  ~county,       ~dem,   ~rep,   # *Rahall II (D) / Gearheart (R)
  "BOONE",        6207,   1958,
  "CABELL",      22210,  11237,
  "FAYETTE",     10545,   4424,
  "GREENBRIER",   9057,   4352,
  "LINCOLN",      4467,   2174,
  "LOGAN",        8922,   3757,
  "MCDOWELL",     5031,   1263,
  "MERCER",      11662,   8791,
  "MINGO",        5946,   1904,
  "MONROE",       3483,   2029,
  "NICHOLAS",     6501,   2675,
  "POCAHONTAS",   2461,   1062,
  "RALEIGH",     16295,  11072,
  "SUMMERS",      3597,   1563,
  "WAYNE",        9755,   4784,
  "WEBSTER",      2372,    613,
  "WYOMING",      5011,   2347
)
cd3_total <- c(dem = 133522, rep = 66005)

for (d in list(list(df = cd1, tot = cd1_total, n = 1), list(df = cd2, tot = cd2_total, n = 2), list(df = cd3, tot = cd3_total, n = 3))) {
  for (nm in names(d$tot)) stopifnot(sum(d$df[[nm]]) == d$tot[nm])
  message("CD", d$n, ": ", nrow(d$df), " counties, all columns match printed totals")
}

## Fold write-ins into REP (both CD1 write-ins have no clear party; the CD2 write-in "Mill" too) --
## same "OTHER" treatment as everywhere else in this project (repuvote counts only the actual R
## nominee; a write-in with no listed party affiliation goes to OTHER, not REP). CD1 has no listed R
## nominee at all, so its write-ins fall entirely into OTHER (repuvote = 0 for CD1 counties, a real
## `one_party` case -- Mollohan ran essentially unopposed by a Republican in 2008).
wv_bluebook_2008 <- bind_rows(
  cd1 %>% transmute(county, dem, rep = 0, other = osgood + smith),
  cd2 %>% transmute(county, dem, rep, other = mill),
  cd3 %>% transmute(county, dem, rep, other = 0)
)
stopifnot(nrow(wv_bluebook_2008) == 55, !anyDuplicated(wv_bluebook_2008$county))

elect_he_cty_wv_bluebook_2008 <- wv_bluebook_2008 %>%
  left_join(wv_fips, by = c("county" = "county_name")) %>%
  mutate(totalvote = dem + rep + other) %>%
  transmute(state = "WEST VIRGINIA", year = 2008, cty_fips = county_fips, sample = "HE",
            demovote = dem / totalvote, repuvote = rep / totalvote, totalvote)
stopifnot(!anyNA(elect_he_cty_wv_bluebook_2008$cty_fips), nrow(elect_he_cty_wv_bluebook_2008) == 55)

sanity <- elect_he_cty_wv_bluebook_2008$demovote + elect_he_cty_wv_bluebook_2008$repuvote
message("sanity range demovote+repuvote: [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")
stopifnot(all(sanity >= 0 & sanity <= 1.001))

## ---- cross-check against the existing OpenElections build ----
wv_existing <- readRDS(file.path(OUTPUT_DIR, "elect_he_cty_wv.rds")) %>% filter(year == 2008)
cmp <- elect_he_cty_wv_bluebook_2008 %>% inner_join(wv_existing, by = "cty_fips", suffix = c("_bb", "_oe"))
diff_total <- abs(cmp$totalvote_bb - cmp$totalvote_oe)
message(nrow(cmp), " counties present in both sources (of ", nrow(wv_existing), " existing OE counties); ",
        sum(diff_total > 0), " differ, max |totalvote diff| = ", max(diff_total),
        " -- expected: OpenElections' 2008 WV precinct files are themselves incomplete for a number of counties (see header note)")
stopifnot(nrow(cmp) == nrow(wv_existing))

message("New counties beyond the existing OE build: ", nrow(elect_he_cty_wv_bluebook_2008) - nrow(cmp), " (", 55 - nrow(wv_existing), " expected)")

saveRDS(elect_he_cty_wv_bluebook_2008, file.path(OUTPUT_DIR, "elect_he_cty_wv_bluebook_2008.rds"))
message("Saved elect_he_cty_wv_bluebook_2008.rds: ", nrow(elect_he_cty_wv_bluebook_2008), " rows (55/55 counties)")
