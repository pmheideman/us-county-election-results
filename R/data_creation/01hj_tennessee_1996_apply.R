## Add Tennessee House 1996 (elect_he_cty_tnbb_1996.rds; 01hi_house_county_tennessee_1996.R) to the panel. 2026-09-24.
source(file.path("R", "00_setup.R")); source(file.path("R", "apply_helper.R"))
apply_state_years("tnbb", 47, 1996, expected_rows = 95, allow_replace = FALSE)
