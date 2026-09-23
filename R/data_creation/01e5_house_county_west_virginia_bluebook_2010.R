## West Virginia House 2010: the SAME Blue Book collection as 01e3 (2012) also includes a "2011"
## edition (`WVS_Bluebook_2011.pdf`, Vol. 91, a scanned/ClearScan-OCR volume) whose Section 8 covers
## "2008 General Election Returns / 2010 Primary, Special and General Election Returns / 2011
## Special Election Returns". The "2010 GENERAL ELECTION" sub-section (pp. 764-766) has a clean
## U.S. House table per district, county rows, a printed TOTAL row per district.
##
## This closes the existing WV House 2010 gap (was 31/55 via OpenElections precinct files, even
## after 01az's TOTALS-row doubling fix -- see 01e4). CD1 20 counties, CD2 18 counties (Mason was
## still in CD2 in 2010, moved to CD3 for 2012 by the post-2010-census map -- a real districting
## change, not a transcription error, confirmed both district totals tie out with Mason placed
## either way), CD3 17 counties = 55/55.
##
## Cross-checked against the (already TOTALS-bug-fixed) OpenElections build for all 31 overlap
## counties: unlike 2012 (where only 2 small residuals showed up), 20 of 31 counties differ here,
## several substantially (e.g. Marshall +1,499, Kanawha -823). Investigated Marshall directly: its
## OpenElections precinct file's real per-precinct rows sum to only 8,653 (vs the book's certified
## 10,152) -- i.e. OpenElections' 2010 West Virginia precinct files are themselves incomplete for a
## number of counties (missing precincts or a pre-certification snapshot), a DIFFERENT and larger
## problem than the single TOTALS-rollup bug fixed in 01e4. The Blue Book, by contrast, is the
## state's own certified canvass and ties exactly to its own printed district totals (verified
## below for all three districts) -- so it is adopted here as the authoritative source for the full
## 55 counties, the same treatment 2012 already got in 01e3, just with a larger, better-explained gap
## between it and OpenElections' incomplete precinct scrape.

source(file.path("R", "00_setup.R"))
library(readr)

county_fips_crosswalk <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))

wv_fips <- county_fips_crosswalk %>% filter(state == "WEST VIRGINIA") %>% select(county_name, county_fips)
stopifnot(nrow(wv_fips) == 55)

## Transcribed from `WVS_Bluebook_2011.pdf` (pdftotext -layout), pp. 764-766, "2010 GENERAL
## ELECTION" / "U.S. HOUSE OF REPRESENTATIVES" / First/Second/Third Congressional District. One
## OCR digit corrected by the total-row check: Braxton's Capito (R) column read "2;t22" (an OCR
## artifact); the total-row check pinned it to 2,022, not the naive 2,722 reading (off by exactly
## 700 from the printed CD2 Republican total).
cd1 <- tribble(
  ~county,       ~dem,   ~rep,
  "BARBOUR",      2451,   2169,
  "BROOKE",       3535,   3357,
  "DODDRIDGE",     762,   1456,
  "GILMER",       1317,    929,
  "GRANT",         758,   2527,
  "HANCOCK",      4598,   5123,
  "HARRISON",    12304,   9492,
  "MARION",      11651,   6370,
  "MARSHALL",     4389,   5763,
  "MINERAL",      2950,   5172,
  "MONONGALIA",  14354,   9496,
  "OHIO",         5451,   8678,
  "PLEASANTS",    1244,   1195,
  "PRESTON",      4745,   4806,
  "RITCHIE",       961,   1926,
  "TAYLOR",       2764,   2186,
  "TUCKER",       1437,   1175,
  "TYLER",         895,   1768,
  "WETZEL",       2324,   2329,
  "WOOD",        10330,  14743
) %>% mutate(district = 1)
cd1_total <- c(dem = 89220, rep = 90660)   # Oliverio (D) / *McKinley (R)

cd2 <- tribble(
  ~county,       ~rep,   ~dem,
  "BERKELEY",    16751,   7155,
  "BRAXTON",      2022,   1670,
  "CALHOUN",      1096,    593,
  "CLAY",         1492,    880,
  "HAMPSHIRE",    4626,   1473,
  "HARDY",        2874,    865,
  "JACKSON",      6644,   2594,
  "JEFFERSON",    8918,   6403,
  "KANAWHA",     39145,  18446,
  "LEWIS",        3981,   1085,
  "MASON",        5033,   2156,
  "MORGAN",       3852,   1467,
  "PENDLETON",    1944,    677,
  "PUTNAM",      13834,   4311,
  "RANDOLPH",     5553,   2125,
  "ROANE",        2660,   1376,
  "UPSHUR",       5159,   1311,
  "WIRT",         1230,    414
) %>% mutate(district = 2)
cd2_total <- c(rep = 126814, dem = 55001)  # *Capito (R) / Graf (D)

cd3 <- tribble(
  ~county,       ~dem,   ~rep,
  "BOONE",        4311,   1994,
  "CABELL",      13725,  11079,
  "FAYETTE",      6570,   4374,
  "GREENBRIER",   5232,   4232,
  "LINCOLN",      3029,   1977,
  "LOGAN",        5507,   3359,
  "MCDOWELL",     3286,   1262,
  "MERCER",       7081,   8077,
  "MINGO",        4119,   2793,
  "MONROE",       2102,   1933,
  "NICHOLAS",     4106,   2997,
  "POCAHONTAS",   1576,   1114,
  "RALEIGH",     10104,  10804,
  "SUMMERS",      2106,   1587,
  "WAYNE",        6232,   4989,
  "WEBSTER",      1486,    773,
  "WYOMING",      3064,   2267
) %>% mutate(district = 3)
cd3_total <- c(dem = 83636, rep = 65611)   # *Rahall II (D) / Maynard (R)

for (d in list(list(df = cd1, tot = cd1_total, n = 1), list(df = cd2, tot = cd2_total, n = 2), list(df = cd3, tot = cd3_total, n = 3))) {
  stopifnot(sum(d$df$dem) == d$tot["dem"], sum(d$df$rep) == d$tot["rep"])
  message("CD", d$n, ": ", nrow(d$df), " counties, dem sum ", sum(d$df$dem), " == printed ", d$tot["dem"],
          "; rep sum ", sum(d$df$rep), " == printed ", d$tot["rep"])
}

wv_bluebook_2010 <- bind_rows(cd1, cd2, cd3)
stopifnot(nrow(wv_bluebook_2010) == 55, !anyDuplicated(wv_bluebook_2010$county))

elect_he_cty_wv_bluebook_2010 <- wv_bluebook_2010 %>%
  left_join(wv_fips, by = c("county" = "county_name")) %>%
  transmute(
    state = "WEST VIRGINIA", year = 2010, cty_fips = county_fips, sample = "HE",
    demovote = dem / (dem + rep), repuvote = rep / (dem + rep), totalvote = dem + rep
  )
stopifnot(!anyNA(elect_he_cty_wv_bluebook_2010$cty_fips), nrow(elect_he_cty_wv_bluebook_2010) == 55)

sanity <- elect_he_cty_wv_bluebook_2010$demovote + elect_he_cty_wv_bluebook_2010$repuvote
stopifnot(all(abs(sanity - 1) < 1e-9))

## ---- cross-check against the existing (TOTALS-bug-fixed) OpenElections build ----
wv_existing <- readRDS(file.path(OUTPUT_DIR, "elect_he_cty_wv.rds")) %>% filter(year == 2010)
cmp <- elect_he_cty_wv_bluebook_2010 %>% inner_join(wv_existing, by = "cty_fips", suffix = c("_bb", "_oe"))
diff_total <- abs(cmp$totalvote_bb - cmp$totalvote_oe)
message(nrow(cmp), " counties present in both sources (of ", nrow(wv_existing), " existing OE counties); ",
        sum(diff_total > 0), " differ, max |totalvote diff| = ", max(diff_total),
        " -- expected: OpenElections' 2010 WV precinct files are themselves incomplete for a number of counties (see header note)")
stopifnot(nrow(cmp) == nrow(wv_existing))

message("New counties beyond the existing OE build: ", nrow(elect_he_cty_wv_bluebook_2010) - nrow(cmp), " (", 55 - nrow(wv_existing), " expected)")

saveRDS(elect_he_cty_wv_bluebook_2010, file.path(OUTPUT_DIR, "elect_he_cty_wv_bluebook_2010.rds"))
message("Saved elect_he_cty_wv_bluebook_2010.rds: ", nrow(elect_he_cty_wv_bluebook_2010), " rows (55/55 counties)")
