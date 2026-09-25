## Add New Hampshire House 1994, 1998, 2002, 2004, 2008 (elect_he_cty_nhmanual_<year>.rds; 01jn_house_county_new_hampshire_1994_2008.R) to the panel. 2026-09-24.
source(file.path("R", "00_setup.R")); source(file.path("R", "apply_helper.R"))
apply_state_years("nhmanual", 33, c(1994, 1998, 2002, 2004, 2008), expected_rows = 50, allow_replace = FALSE)
