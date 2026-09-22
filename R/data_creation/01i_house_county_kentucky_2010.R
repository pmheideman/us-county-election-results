## Kentucky, 2010: the one remaining gap in the 1990-2024 Kentucky House panel.
## elect.ky.gov's 2010 general-election results PDF ("off2010gen.pdf", manually downloaded by the
## user to R/data/county_house_files/kentucky_2010.pdf) is scanned/image-only -- `pdftotext` pulls
## essentially zero text (154 pages, ~154 characters) unlike every other year 1990-2016, which are
## all born-digital. OCR'd with `ocrmypdf --force-ocr` (tesseract 5.5.0) in ~19 seconds; the text
## layer is usable for locating sections but NOT trusted for the vote counts themselves -- spot
## checks found real digit errors (e.g. Whitley county's 5th-district Democratic vote OCR'd as
## "4618" when the source image clearly reads "1,513"). Instead, all 6 US Representative pages
## (PDF pages 8-14, found via the OCR text's "Congressional District" headers) were rendered at
## 300dpi and read directly as images, transcribed by hand below, and every single district's
## column sum was checked against that district's own printed "Total Votes" row -- all 6 districts
## (Whitfield/Hatchett, Guthrie/Marksberry, Yarmuth/Lally/Martin/Hansen, Davis/Waltz,
## Rogers/Holbert, Chandler/Barr/Collins/Vance) matched exactly, so this transcription is treated
## as fully verified, not merely plausible.
##
## Unlike the 2000s/2010s born-digital PDFs, this one prints party labels directly as column
## headers ("Republican Party"/"Democratic Party"/etc.) -- no Wikipedia-infobox party lookup
## needed here.
##
## A handful of counties are split across two districts (Jefferson: 2nd+3rd; Ohio: 1st+2nd; Bath:
## 4th+5th; Scott: 4th+6th; Lincoln: 1st+6th) -- expected, resolved automatically by summing by
## county_name regardless of district, same as every other source in this project.

source(file.path("R", "00_setup.R"))
library(readr)

KY_COUNTIES <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% filter(state == "KENTUCKY") %>% distinct(county_name, county_fips)

## ---- Hand-transcribed from R/data/county_house_files/kentucky_2010.pdf, PDF pages 8-14 ----
## (Republican, Democratic) unless noted. Party abbreviations: REP, DEM, OTHER (Libertarian,
## Independent, and write-ins all fold into OTHER, matching every other Kentucky-year script).

d1 <- tribble(
  ~county_name,   ~REP,   ~DEM,
  "ADAIR",        5153,   1251,
  "ALLEN",        5048,   1374,
  "BALLARD",      2075,   951,
  "BUTLER",       3211,   728,
  "CALDWELL",     3358,   1336,
  "CALLOWAY",     7250,   3282,
  "CARLISLE",     1571,   643,
  "CASEY",        3496,   666,
  "CHRISTIAN",    10372,  3968,
  "CLINTON",      2915,   512,
  "CRITTENDEN",   2535,   889,
  "CUMBERLAND",   2080,   440,
  "FULTON",       1307,   752,
  "GRAVES",       8528,   3890,
  "HENDERSON",    8521,   4997,
  "HICKMAN",      1119,   577,
  "HOPKINS",      11668,  3636,
  "LINCOLN",      1218,   646,
  "LIVINGSTON",   2569,   1307,
  "LOGAN",        5529,   2536,
  "LYON",         2135,   1051,
  "MARSHALL",     7871,   4114,
  "MCCRACKEN",    15790,  6342,
  "MCLEAN",       2431,   1142,
  "METCALFE",     2349,   1287,
  "MONROE",       3911,   614,
  "MUHLENBERG",   5211,   3638,
  "OHIO",         3182,   1393,
  "RUSSELL",      5071,   1178,
  "SIMPSON",      3261,   1569,
  "TODD",         2823,   1024,
  "TRIGG",        4225,   1383,
  "UNION",        3399,   1566,
  "WEBSTER",      2658,   1408
)
stopifnot(sum(d1$REP) == 153840, sum(d1$DEM) == 62090)  # matches printed "Total Votes" row

d2 <- tribble(
  ~county_name,     ~REP,   ~DEM,
  "BARREN",         9201,   3399,
  "BRECKINRIDGE",   4323,   1959,
  "BULLITT",        15681,  7047,
  "DAVIESS",        18191,  12537,
  "EDMONSON",       3366,   920,
  "GRAYSON",        6609,   1893,
  "GREEN",          3480,   948,
  "HANCOCK",        1640,   1455,
  "HARDIN",         18597,  8509,
  "HART",           3268,   1619,
  "JEFFERSON",      2349,   1758,
  "LARUE",          3139,   1370,
  "MARION",         2754,   2338,
  "MEADE",          5826,   3231,
  "NELSON",         8648,   5205,
  "OHIO",           2390,   1351,
  "SHELBY",         9678,   4474,
  "SPENCER",        4732,   2006,
  "TAYLOR",         6733,   2589,
  "WARREN",         22564,  7751,
  "WASHINGTON",     2737,   1390
)
stopifnot(sum(d2$REP) == 155906, sum(d2$DEM) == 73749)

## District 3 is Jefferson County alone (entirely within the 3rd district's boundaries).
## REP/DEM columns are labeled Republican=Lally, Democratic=Yarmuth on the source page; Martin
## (Libertarian) and Hansen (Independent) both fold into OTHER.
d3 <- tribble(
  ~county_name,  ~REP,     ~DEM,     ~OTHER,
  "JEFFERSON",   112627,   139940,   2029 + 1334
)
stopifnot(sum(d3$REP) == 112627, sum(d3$DEM) == 139940, sum(d3$OTHER) == 3363)

d4 <- tribble(
  ~county_name,   ~REP,   ~DEM,
  "BATH",         1277,   1091,
  "BOONE",        25714,  7006,
  "BOYD",         9439,   4814,
  "BRACKEN",      2029,   1077,
  "CAMPBELL",     19672,  8583,
  "CARROLL",      1844,   1409,
  "CARTER",       4874,   3226,
  "ELLIOTT",      589,    818,
  "FLEMING",      3217,   1708,
  "GALLATIN",     1665,   1065,
  "GRANT",        4331,   1696,
  "GREENUP",      6935,   4530,
  "HARRISON",     3613,   1997,
  "HENRY",        3573,   1905,
  "KENTON",       31306,  12641,
  "LEWIS",        2646,   768,
  "MASON",        3252,   1699,
  "NICHOLAS",     1149,   771,
  "OLDHAM",       16426,  5637,
  "OWEN",         2200,   1185,
  "PENDLETON",    2788,   1166,
  "ROBERTSON",    427,    309,
  "SCOTT",        1182,   582,
  "TRIMBLE",      1665,   1011
)
stopifnot(sum(d4$REP) == 151813, sum(d4$DEM) == 66694)

d5 <- tribble(
  ~county_name,   ~REP,   ~DEM,
  "BATH",         421,    393,
  "BELL",         7392,   1397,
  "BREATHITT",    2840,   1536,
  "CLAY",         4328,   660,
  "FLOYD",        7443,   3197,
  "HARLAN",       6676,   1853,
  "JACKSON",      3939,   491,
  "JOHNSON",      6444,   1472,
  "KNOTT",        3567,   2143,
  "KNOX",         7485,   1484,
  "LAUREL",       12457,  2544,
  "LAWRENCE",     3220,   1608,
  "LEE",          2407,   342,
  "LESLIE",       4007,   481,
  "LETCHER",      5687,   1926,
  "MAGOFFIN",     3821,   2023,
  "MARTIN",       3044,   620,
  "MCCREARY",     4298,   684,
  "MENIFEE",      1498,   881,
  "MORGAN",       2751,   1300,
  "OWSLEY",       1623,   296,
  "PERRY",        6876,   2011,
  "PIKE",         10217,  4819,
  "PULASKI",      16528,  3059,
  "ROCKCASTLE",   4030,   544,
  "ROWAN",        3964,   2486,
  "WAYNE",        5456,   1490,
  "WHITLEY",      7598,   1513,
  "WOLFE",        1002,   781
)
stopifnot(sum(d5$REP) == 151019, sum(d5$DEM) == 44034)

## District 6: Democratic=Chandler, Republican=Barr, both write-in columns (Collins, Vance) fold
## into OTHER.
d6 <- tribble(
  ~county_name,    ~DEM,    ~REP,    ~write1, ~write2,
  "ANDERSON",      3947,    4199,    7,       0,
  "BOURBON",       3083,    2841,    4,       0,
  "BOYLE",         4155,    4755,    0,       0,
  "CLARK",         4919,    5892,    16,      0,
  "ESTILL",        2215,    2379,    0,       0,
  "FAYETTE",       46370,   41985,   121,     14,
  "FRANKLIN",      11659,   7084,    8,       5,
  "GARRARD",       1931,    3436,    1,       0,
  "JESSAMINE",     6094,    9721,    5,       1,
  "LINCOLN",       2506,    2768,    0,       0,
  "MADISON",       11605,   12767,   30,      1,
  "MERCER",        3439,    4295,    13,      0,
  "MONTGOMERY",    4328,    3612,    2,       0,
  "POWELL",        2328,    1578,    0,       0,
  "SCOTT",         6051,    7035,    6,       1,
  "WOODFORD",      5182,    4817,    12,      0
) %>% mutate(OTHER = write1 + write2) %>% select(-write1, -write2)
stopifnot(sum(d6$DEM) == 119812, sum(d6$REP) == 119164, sum(d6$OTHER) == 225 + 22)

ky_2010_raw <- bind_rows(d1, d2, d3, d4, d5, d6) %>%
  pivot_longer(-county_name, names_to = "party", values_to = "votes", values_drop_na = TRUE) %>%
  filter(!is.na(votes))

elect_he_cty_ky_2010 <- ky_2010_raw %>%
  group_by(county_name, party) %>% summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
  group_by(county_name) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  inner_join(KY_COUNTIES, by = "county_name") %>%
  filter(totalvote > 0) %>%
  transmute(
    state = "KENTUCKY", year = 2010, cty_fips = county_fips, sample = "HE",
    demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
  ) %>%
  save_step("elect_he_cty_ky_2010")

message("KY 2010 House county-level rows built: ", nrow(elect_he_cty_ky_2010), " (of possible 120)")

sanity <- elect_he_cty_ky_2010$repuvote + elect_he_cty_ky_2010$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")

## ---- Fold into the master panel and the combined KY file ----
elect_he_cty_ky_all <- bind_rows(
  readRDS(file.path(OUTPUT_DIR, "elect_he_cty_ky.rds")),
  elect_he_cty_ky_2010
) %>% distinct(cty_fips, year, sample, .keep_all = TRUE) %>%
  save_step("elect_he_cty_ky")

elect_cty_final <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")) %>%
  bind_rows(elect_he_cty_ky_2010 %>% select(-state)) %>%
  distinct(cty_fips, year, sample, .keep_all = TRUE) %>%
  arrange(cty_fips, year, sample) %>%
  save_step("elect_cty_final")

message("elect_cty_final.rds updated with KY 2010. Total rows now: ", nrow(elect_cty_final))
message("Kentucky House panel is now complete, 1990-2024 (2018-2024 via MEDSL, 1990-2016 via this project's own extraction).")
