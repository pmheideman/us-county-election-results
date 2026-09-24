## Add Delaware House 1990-1996 (elect_he_cty_dede_<year>.rds; 01gf_house_county_delaware_1990_1996.R) to the panel. 2026-09-24.
source(file.path("R", "00_setup.R")); source(file.path("R", "apply_helper.R"))
apply_state_years("dede", 10, c(1990, 1992, 1994, 1996), expected_rows = 12, allow_replace = FALSE)
