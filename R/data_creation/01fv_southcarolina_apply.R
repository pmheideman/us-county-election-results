## Add South Carolina House 2010 (elect_he_cty_scenr_2010.rds; 01fu_house_county_southcarolina_2010.R) to the panel. 2026-09-24.
source(file.path("R", "00_setup.R")); source(file.path("R", "apply_helper.R"))
apply_state_years("scenr", 45, 2010, expected_rows = 46, allow_replace = FALSE)
