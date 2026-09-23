## Candidate-level LONG table for West Virginia House 2010, sourced from the WV Blue Book (see
## 01e5_house_county_west_virginia_bluebook_2010.R). Same pattern as
## 02p_house_long_west_virginia_bluebook_2012.R: source key "he_cty_wv_bluebook_2010" resolves
## exactly to file "he_wv_bluebook_2010.rds" in 02z_house_long_assemble.R.
## Acceptance: derived shares == elect_he_cty_wv_bluebook_2010.rds.
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
save_step <- function(df, name) invisible(df)

xw <- readr::read_delim(file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))
wv_fips <- xw %>% filter(state == "WEST VIRGINIA") %>% select(county_name, county_fips)

cd1 <- tribble(
  ~county,       ~dem,   ~rep,
  "BARBOUR", 2451, 2169, "BROOKE", 3535, 3357, "DODDRIDGE", 762, 1456, "GILMER", 1317, 929, "GRANT", 758, 2527,
  "HANCOCK", 4598, 5123, "HARRISON", 12304, 9492, "MARION", 11651, 6370, "MARSHALL", 4389, 5763, "MINERAL", 2950, 5172,
  "MONONGALIA", 14354, 9496, "OHIO", 5451, 8678, "PLEASANTS", 1244, 1195, "PRESTON", 4745, 4806, "RITCHIE", 961, 1926,
  "TAYLOR", 2764, 2186, "TUCKER", 1437, 1175, "TYLER", 895, 1768, "WETZEL", 2324, 2329, "WOOD", 10330, 14743
) %>% mutate(district = 1, dem_name = "Michael A. Oliverio", rep_name = "David B. McKinley")

cd2 <- tribble(
  ~county,       ~rep,   ~dem,
  "BERKELEY", 16751, 7155, "BRAXTON", 2022, 1670, "CALHOUN", 1096, 593, "CLAY", 1492, 880, "HAMPSHIRE", 4626, 1473,
  "HARDY", 2874, 865, "JACKSON", 6644, 2594, "JEFFERSON", 8918, 6403, "KANAWHA", 39145, 18446, "LEWIS", 3981, 1085,
  "MASON", 5033, 2156, "MORGAN", 3852, 1467, "PENDLETON", 1944, 677, "PUTNAM", 13834, 4311, "RANDOLPH", 5553, 2125,
  "ROANE", 2660, 1376, "UPSHUR", 5159, 1311, "WIRT", 1230, 414
) %>% mutate(district = 2, rep_name = "Shelley Moore Capito", dem_name = "Virginia Lynch Graf")

cd3 <- tribble(
  ~county,       ~dem,   ~rep,
  "BOONE", 4311, 1994, "CABELL", 13725, 11079, "FAYETTE", 6570, 4374, "GREENBRIER", 5232, 4232, "LINCOLN", 3029, 1977,
  "LOGAN", 5507, 3359, "MCDOWELL", 3286, 1262, "MERCER", 7081, 8077, "MINGO", 4119, 2793, "MONROE", 2102, 1933,
  "NICHOLAS", 4106, 2997, "POCAHONTAS", 1576, 1114, "RALEIGH", 10104, 10804, "SUMMERS", 2106, 1587, "WAYNE", 6232, 4989,
  "WEBSTER", 1486, 773, "WYOMING", 3064, 2267
) %>% mutate(district = 3, dem_name = "Nick Joe Rahall II", rep_name = "Elliott E. Maynard")

raw <- bind_rows(cd1, cd2, cd3) %>%
  left_join(wv_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips)) %>%
  { bind_rows(
      transmute(., year = 2010, county_fips, district, candidate = dem_name, party = "Democratic", party_group = "DEM", votes = dem),
      transmute(., year = 2010, county_fips, district, candidate = rep_name, party = "Republican", party_group = "REP", votes = rep)
    ) }

long <- finalize_long(raw, "wv_bluebook_2010")
save_long(long, "he_wv_bluebook_2010")
message("WV Blue Book 2010 long rows: ", nrow(long))
check_long_vs_source(long, file.path(OUTPUT_DIR, "elect_he_cty_wv_bluebook_2010.rds"))
