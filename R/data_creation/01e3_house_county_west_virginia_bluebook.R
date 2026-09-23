## West Virginia House 2012: user downloaded the WV Blue Book volume covering the 2012 GENERAL
## election (`0767_WVS_BlueBook.pdf`, Section 8, "2012 General Election Returns" -- a separate,
## later-printed volume from the 2012 edition already in the project, which turned out to hold only
## the 2012 PRIMARY and no federal races at all). This volume is born-digital (InDesign, real text
## layer, no OCR needed) and prints one clean U.S. House table per district: county rows, a
## candidate column per party, a printed TOTALS row.
##
## This closes the existing WV House 2012 gap. `01az_house_county_west_virginia.R`'s OpenElections
## per-county precinct files only covered 25/55 counties that year (the rest genuinely absent from
## the OE repo) -- the Blue Book has all three districts in full: CD1 20 counties, CD2 17 counties,
## CD3 18 counties = 55/55, with only a D and an R candidate on the ballot in every district (no
## third-party/independent column printed this year, unlike Governor the same year).
##
## Verified two ways: (1) every district's county column sums to the book's own printed TOTALS row
## exactly; (2) cross-checked against `01az`'s existing 25-county OE build for every county present
## in both sources. This cross-check ALSO caught a real bug in `01az`: 2 of the 25 overlap counties
## (Monongalia, Ohio) were exactly 2x the Blue Book value -- traced to a `precinct == "TOTALS"`
## rollup row present in some (not all) OpenElections WV precinct files that `01az` was summing on
## top of the real per-precinct rows (same "pseudo-total-row" bug class as Oregon/Kansas/Georgia/NY
## elsewhere in this project). Fixed directly in `01az` (now excludes `precinct == "TOTALS"`) and
## `01az` re-run before this cross-check; also corrects 12 West Virginia 2010 counties that had the
## same bug (confirmed via 2012 presidential-turnout plausibility: Monongalia's pre-fix House total
## was 186% of its own 2012 presidential vote, impossible; post-fix 93%, plausible). After that fix,
## 23/25 overlap counties match the Blue Book EXACTLY; Roane (+21 votes) and Barbour (+3 votes) have
## a small residual that is NOT the TOTALS bug (OE's own "TOTALS" row for these two, where present,
## already agrees with OE's real-precinct sum -- the gap is between OE's precinct file and the
## Blue Book's certified total, likely a late canvass correction). Accepted as a normal, small,
## already-precedented cross-source residual (same class as Oklahoma's Fed-Abs gap, Texas's 2016
## MEDSL-coverage gap) -- not chased further given the tiny magnitude (<0.5% of the county total).

source(file.path("R", "00_setup.R"))
library(readr)

county_fips_crosswalk <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))

wv_fips <- county_fips_crosswalk %>% filter(state == "WEST VIRGINIA") %>% select(county_name, county_fips)
stopifnot(nrow(wv_fips) == 55)

## Transcribed directly from `0767_WVS_BlueBook.pdf` (pdftotext -layout), pp. 784-785, "U.S. HOUSE
## OF REPRESENTATIVES" / "First/Second/Third Congressional District". Democrat first, then
## Republican, matching the book's own column order in every district.
cd1 <- tribble(
  ~county,       ~dem,   ~rep,
  "BARBOUR",      1810,   3549,
  "BROOKE",       3606,   5029,
  "DODDRIDGE",     559,   2024,
  "GILMER",        976,   1396,
  "GRANT",         927,   3322,
  "HANCOCK",      4476,   6740,
  "HARRISON",     9788,  15342,
  "MARION",       8861,  11285,
  "MARSHALL",     4339,   7953,
  "MINERAL",      3288,   7172,
  "MONONGALIA",  13775,  15568,
  "OHIO",         5872,  11407,
  "PLEASANTS",     895,   1754,
  "PRESTON",      3267,   6879,
  "RITCHIE",       766,   2750,
  "TAYLOR",       2097,   3629,
  "TUCKER",        982,   1967,
  "TYLER",         847,   2251,
  "WETZEL",       2191,   3386,
  "WOOD",        11020,  20406
) %>% mutate(district = 1)
cd1_total <- c(dem = 80342, rep = 133809)

cd2 <- tribble(
  ~county,       ~dem,   ~rep,
  "BERKELEY",    11659,  23960,
  "BRAXTON",      1589,   2691,
  "CALHOUN",       566,   1526,
  "CLAY",          771,   2201,
  "HAMPSHIRE",    1796,   6004,
  "HARDY",        1094,   3847,
  "JACKSON",      2759,   8364,
  "JEFFERSON",    8472,  13106,
  "KANAWHA",     24905,  48262,
  "LEWIS",        1174,   5069,
  "MORGAN",       1899,   4645,
  "PENDLETON",     722,   2482,
  "PUTNAM",       5454,  17493,
  "RANDOLPH",     2451,   6902,
  "ROANE",        1317,   3511,
  "UPSHUR",       1521,   6473,
  "WIRT",          411,   1670
) %>% mutate(district = 2)
cd2_total <- c(dem = 68560, rep = 158206)

cd3 <- tribble(
  ~county,       ~dem,   ~rep,
  "BOONE",        5338,   2945,
  "CABELL",      17870,  13517,
  "FAYETTE",      7330,   6374,
  "GREENBRIER",   6496,   5921,
  "LINCOLN",      3954,   2580,
  "LOGAN",        7423,   3725,
  "MASON",        5285,   3965,
  "MCDOWELL",     3814,   2188,
  "MERCER",       8880,  11454,
  "MINGO",        5470,   2983,
  "MONROE",       2466,   2567,
  "NICHOLAS",     4504,   4098,
  "POCAHONTAS",   2097,   1280,
  "RALEIGH",     11553,  16327,
  "SUMMERS",      2501,   2054,
  "WAYNE",        7743,   5741,
  "WEBSTER",      1777,    917,
  "WYOMING",      3698,   3602
) %>% mutate(district = 3)
cd3_total <- c(dem = 108199, rep = 92238)

## ---- Verify each district's printed TOTALS row ----
for (d in list(list(df = cd1, tot = cd1_total, n = 1), list(df = cd2, tot = cd2_total, n = 2), list(df = cd3, tot = cd3_total, n = 3))) {
  stopifnot(sum(d$df$dem) == d$tot["dem"], sum(d$df$rep) == d$tot["rep"])
  message("CD", d$n, ": ", nrow(d$df), " counties, dem sum ", sum(d$df$dem), " == printed ", d$tot["dem"],
          "; rep sum ", sum(d$df$rep), " == printed ", d$tot["rep"])
}

wv_bluebook_2012 <- bind_rows(cd1, cd2, cd3)
stopifnot(nrow(wv_bluebook_2012) == 55, !anyDuplicated(wv_bluebook_2012$county))

elect_he_cty_wv_bluebook_2012 <- wv_bluebook_2012 %>%
  left_join(wv_fips, by = c("county" = "county_name")) %>%
  transmute(
    state = "WEST VIRGINIA", year = 2012, cty_fips = county_fips, sample = "HE",
    demovote = dem / (dem + rep), repuvote = rep / (dem + rep), totalvote = dem + rep
  )
stopifnot(!anyNA(elect_he_cty_wv_bluebook_2012$cty_fips), nrow(elect_he_cty_wv_bluebook_2012) == 55)

sanity <- elect_he_cty_wv_bluebook_2012$demovote + elect_he_cty_wv_bluebook_2012$repuvote
stopifnot(all(abs(sanity - 1) < 1e-9))   # only D and R on the ballot this year, no third column printed

## ---- Cross-check against the existing OpenElections-based build (25/55 counties) ----
wv_existing <- readRDS(file.path(OUTPUT_DIR, "elect_he_cty_wv.rds")) %>% filter(year == 2012)
cmp <- elect_he_cty_wv_bluebook_2012 %>%
  inner_join(wv_existing, by = "cty_fips", suffix = c("_bb", "_oe"))
message(nrow(cmp), " counties present in both sources (of ", nrow(wv_existing), " existing OE counties)")
diff_total <- abs(cmp$totalvote_bb - cmp$totalvote_oe)
diff_dem <- abs(cmp$demovote_bb - cmp$demovote_oe)
diff_rep <- abs(cmp$repuvote_bb - cmp$repuvote_oe)
message("max |totalvote diff| = ", max(diff_total), "; max |demovote share diff| = ", round(max(diff_dem), 6),
        "; max |repuvote share diff| = ", round(max(diff_rep), 6))
## Roane (+21 votes) and Barbour (+3 votes) are the only nonzero residuals -- see header note.
stopifnot(nrow(cmp) == nrow(wv_existing), max(diff_total) <= 25, sum(diff_total > 0) <= 2)

message("New counties beyond the existing OE build: ", nrow(elect_he_cty_wv_bluebook_2012) - nrow(cmp), " (", 55 - nrow(wv_existing), " expected)")

saveRDS(elect_he_cty_wv_bluebook_2012, file.path(OUTPUT_DIR, "elect_he_cty_wv_bluebook_2012.rds"))
message("Saved elect_he_cty_wv_bluebook_2012.rds: ", nrow(elect_he_cty_wv_bluebook_2012), " rows (55/55 counties)")
