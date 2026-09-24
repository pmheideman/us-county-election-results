## Add Michigan House 1990 and 1992 (elect_he_cty_mimanual_<year>.rds; 01hs_house_county_michigan_1990_1992.R) to the panel. 2026-09-24.
source(file.path("R", "00_setup.R")); source(file.path("R", "apply_helper.R"))
apply_state_years("mimanual", 26, c(1990, 1992), expected_rows = 166, allow_replace = FALSE)
