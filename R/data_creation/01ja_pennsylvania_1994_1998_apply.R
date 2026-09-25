## Add Pennsylvania House 1994, 1996, 1998 (elect_he_cty_padosprec_<year>.rds; 01iz_house_county_pennsylvania_1994_1998.R) to the panel. 2026-09-24.
source(file.path("R", "00_setup.R")); source(file.path("R", "apply_helper.R"))
apply_state_years("padosprec", 42, c(1994, 1996, 1998), expected_rows = 201, allow_replace = FALSE)
