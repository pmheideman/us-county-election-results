## Add Wyoming House 1990, 1992, 1994 (elect_he_cty_wyia_<year>.rds; 01gw_house_county_wyoming_1990_1994.R) to the panel. 2026-09-24.
source(file.path("R", "00_setup.R")); source(file.path("R", "apply_helper.R"))
apply_state_years("wyia", 56, c(1990, 1992, 1994), expected_rows = 69, allow_replace = FALSE)
