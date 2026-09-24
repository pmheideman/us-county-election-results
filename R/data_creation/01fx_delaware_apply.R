## Add Delaware House 2012 (elect_he_cty_dede_2012.rds; 01fw_house_county_delaware_2012.R) to the panel. 2026-09-24.
source(file.path("R", "00_setup.R")); source(file.path("R", "apply_helper.R"))
apply_state_years("dede", 10, 2012, expected_rows = 3, allow_replace = FALSE)
