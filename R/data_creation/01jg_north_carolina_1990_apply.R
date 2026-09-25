## Add North Carolina House 1990 (elect_he_cty_ncmanual_1990.rds; 01jf_house_county_north_carolina_1990.R) to the panel. 2026-09-24.
source(file.path("R", "00_setup.R")); source(file.path("R", "apply_helper.R"))
apply_state_years("ncmanual", 37, 1990, expected_rows = 100, allow_replace = FALSE)
