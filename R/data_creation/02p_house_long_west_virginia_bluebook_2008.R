## Candidate-level LONG table for West Virginia House 2008, sourced from the WV Blue Book (see
## 01e8_house_county_west_virginia_bluebook_2008.R). Same pattern as the 2010/2012 Blue Book long
## tables: source key "he_cty_wv_bluebook_2008" resolves exactly to "he_wv_bluebook_2008.rds".
## Acceptance: derived shares == elect_he_cty_wv_bluebook_2008.rds.
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
save_step <- function(df, name) invisible(df)

xw <- readr::read_delim(file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))
wv_fips <- xw %>% filter(state == "WEST VIRGINIA") %>% select(county_name, county_fips)

cd1 <- tribble(
  ~county,       ~dem,   ~osgood, ~smith,
  "BARBOUR", 4798, 3, 0, "BROOKE", 8080, 1, 2, "DODDRIDGE", 2151, 2, 0, "GILMER", 2037, 0, 0, "GRANT", 2504, 2, 2,
  "HANCOCK", 10499, 0, 4, "HARRISON", 24091, 4, 8, "MARION", 18852, 12, 2, "MARSHALL", 11070, 2, 15, "MINERAL", 8556, 1, 5,
  "MONONGALIA", 24319, 0, 0, "OHIO", 15285, 1, 4, "PLEASANTS", 2372, 0, 1, "PRESTON", 8746, 9, 11, "RITCHIE", 2728, 1, 1,
  "TAYLOR", 4922, 0, 1, "TUCKER", 2344, 0, 0, "TYLER", 2857, 1, 0, "WETZEL", 5259, 7, 1, "WOOD", 26264, 23, 4
) %>% mutate(district = 1)

cd2 <- tribble(
  ~county,       ~dem,   ~rep,   ~mill,
  "BERKELEY", 15273, 21261, 3, "BRAXTON", 2873, 2419, 0, "CALHOUN", 1183, 1240, 0, "CLAY", 1512, 1790, 0, "HAMPSHIRE", 2977, 5272, 1,
  "HARDY", 1874, 3478, 0, "JACKSON", 5116, 7185, 0, "JEFFERSON", 11076, 11058, 3, "KANAWHA", 38751, 43601, 3, "LEWIS", 2243, 4371, 2,
  "MASON", 4640, 6042, 0, "MORGAN", 2624, 4471, 3, "PENDLETON", 1253, 2128, 0, "PUTNAM", 9211, 15605, 1, "RANDOLPH", 4046, 6699, 0,
  "ROANE", 2646, 2981, 0, "UPSHUR", 2614, 6359, 0, "WIRT", 907, 1374, 0
) %>% mutate(district = 2)

cd3 <- tribble(
  ~county,       ~dem,   ~rep,
  "BOONE", 6207, 1958, "CABELL", 22210, 11237, "FAYETTE", 10545, 4424, "GREENBRIER", 9057, 4352, "LINCOLN", 4467, 2174,
  "LOGAN", 8922, 3757, "MCDOWELL", 5031, 1263, "MERCER", 11662, 8791, "MINGO", 5946, 1904, "MONROE", 3483, 2029,
  "NICHOLAS", 6501, 2675, "POCAHONTAS", 2461, 1062, "RALEIGH", 16295, 11072, "SUMMERS", 3597, 1563, "WAYNE", 9755, 4784,
  "WEBSTER", 2372, 613, "WYOMING", 5011, 2347
) %>% mutate(district = 3)

raw <- bind_rows(
  cd1 %>% transmute(county, district,
                     dem_c = "Alan B. Mollohan", dem_p = "Democratic", dem_v = dem,
                     osgood_c = "Ted Osgood", osgood_p = "Write-In", osgood_v = osgood,
                     smith_c = "R.J. Smith", smith_p = "Write-In", smith_v = smith),
  cd2 %>% transmute(county, district,
                     dem_c = "Anne Barth", dem_p = "Democratic", dem_v = dem,
                     rep_c = "Shelley Moore Capito", rep_p = "Republican", rep_v = rep,
                     mill_c = "Aaron Mill", mill_p = "Write-In", mill_v = mill),
  cd3 %>% transmute(county, district,
                     dem_c = "Nick Joe Rahall II", dem_p = "Democratic", dem_v = dem,
                     rep_c = "Marty Gearheart", rep_p = "Republican", rep_v = rep)
)

to_long <- function(df, cand_col, party_col, votes_col, party_group) {
  if (!votes_col %in% names(df)) return(NULL)
  df %>% filter(!is.na(.data[[votes_col]])) %>%
    transmute(county, district, candidate = .data[[cand_col]], party = .data[[party_col]], party_group = !!party_group, votes = .data[[votes_col]])
}
long_raw <- bind_rows(
  to_long(raw, "dem_c", "dem_p", "dem_v", "DEM"),
  to_long(raw, "rep_c", "rep_p", "rep_v", "REP"),
  to_long(raw, "osgood_c", "osgood_p", "osgood_v", "OTHER"),
  to_long(raw, "smith_c", "smith_p", "smith_v", "OTHER"),
  to_long(raw, "mill_c", "mill_p", "mill_v", "OTHER")
) %>% mutate(year = 2008) %>%
  left_join(wv_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips)) %>% select(-county)

long <- finalize_long(long_raw, "wv_bluebook_2008")
save_long(long, "he_wv_bluebook_2008")
message("WV Blue Book 2008 long rows: ", nrow(long))
check_long_vs_source(long, file.path(OUTPUT_DIR, "elect_he_cty_wv_bluebook_2008.rds"))
