## Add Tennessee House 1998 (elect_he_cty_tnsos_1998.rds; 01gz_house_county_tennessee_1998.R) to the panel. 2026-09-24.
source(file.path("R", "00_setup.R")); source(file.path("R", "apply_helper.R"))
apply_state_years("tnsos", 47, 1998, expected_rows = 95, allow_replace = FALSE)
