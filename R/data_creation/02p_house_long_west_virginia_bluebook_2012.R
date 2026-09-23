## Candidate-level LONG table for West Virginia House 2012, sourced from the WV Blue Book (see
## 01e3_house_county_west_virginia_bluebook.R) rather than 02p_house_long_west_virginia.R's
## OpenElections build -- this source wins the 2012 panel rows (55/55 counties vs OE's 25/55), so
## it needs its own candidate-level file for 02z_house_long_assemble.R to resolve to (source key
## "he_cty_wv_bluebook_2012" -> file "he_wv_bluebook_2012.rds", exact match, no fallback needed).
## Acceptance: derived shares == elect_he_cty_wv_bluebook_2012.rds.
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
save_step <- function(df, name) invisible(df)

xw <- readr::read_delim(file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))
wv_fips <- xw %>% filter(state == "WEST VIRGINIA") %>% select(county_name, county_fips)

## Same transcription as 01e3, plus which candidate is which party (only D and R printed this year).
cd1 <- tribble(
  ~county,       ~dem,   ~rep,
  "BARBOUR",      1810,   3549, "BROOKE", 3606, 5029, "DODDRIDGE", 559, 2024, "GILMER", 976, 1396, "GRANT", 927, 3322,
  "HANCOCK",      4476,   6740, "HARRISON", 9788, 15342, "MARION", 8861, 11285, "MARSHALL", 4339, 7953, "MINERAL", 3288, 7172,
  "MONONGALIA",  13775,  15568, "OHIO", 5872, 11407, "PLEASANTS", 895, 1754, "PRESTON", 3267, 6879, "RITCHIE", 766, 2750,
  "TAYLOR",       2097,   3629, "TUCKER", 982, 1967, "TYLER", 847, 2251, "WETZEL", 2191, 3386, "WOOD", 11020, 20406
) %>% mutate(district = 1, dem_name = "Sue Thorn", rep_name = "David McKinley")

cd2 <- tribble(
  ~county,       ~dem,   ~rep,
  "BERKELEY",    11659,  23960, "BRAXTON", 1589, 2691, "CALHOUN", 566, 1526, "CLAY", 771, 2201, "HAMPSHIRE", 1796, 6004,
  "HARDY",        1094,   3847, "JACKSON", 2759, 8364, "JEFFERSON", 8472, 13106, "KANAWHA", 24905, 48262, "LEWIS", 1174, 5069,
  "MORGAN",       1899,   4645, "PENDLETON", 722, 2482, "PUTNAM", 5454, 17493, "RANDOLPH", 2451, 6902, "ROANE", 1317, 3511,
  "UPSHUR",       1521,   6473, "WIRT", 411, 1670
) %>% mutate(district = 2, dem_name = "Howard Swint", rep_name = "Shelley Moore Capito")

cd3 <- tribble(
  ~county,       ~dem,   ~rep,
  "BOONE",        5338,   2945, "CABELL", 17870, 13517, "FAYETTE", 7330, 6374, "GREENBRIER", 6496, 5921, "LINCOLN", 3954, 2580,
  "LOGAN",        7423,   3725, "MASON", 5285, 3965, "MCDOWELL", 3814, 2188, "MERCER", 8880, 11454, "MINGO", 5470, 2983,
  "MONROE",       2466,   2567, "NICHOLAS", 4504, 4098, "POCAHONTAS", 2097, 1280, "RALEIGH", 11553, 16327, "SUMMERS", 2501, 2054,
  "WAYNE",        7743,   5741, "WEBSTER", 1777, 917, "WYOMING", 3698, 3602
) %>% mutate(district = 3, dem_name = "Nick J. Rahall II", rep_name = "Rick Snuffer")

raw <- bind_rows(cd1, cd2, cd3) %>%
  left_join(wv_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips)) %>%
  { bind_rows(
      transmute(., year = 2012, county_fips, district, candidate = dem_name, party = "Democratic", party_group = "DEM", votes = dem),
      transmute(., year = 2012, county_fips, district, candidate = rep_name, party = "Republican", party_group = "REP", votes = rep)
    ) }

long <- finalize_long(raw, "wv_bluebook_2012")
save_long(long, "he_wv_bluebook_2012")
message("WV Blue Book 2012 long rows: ", nrow(long))
check_long_vs_source(long, file.path(OUTPUT_DIR, "elect_he_cty_wv_bluebook_2012.rds"))
