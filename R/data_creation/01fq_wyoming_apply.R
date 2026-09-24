## Add Wyoming House 1996, 1998, 2006 (elect_he_cty_wysos_<year>.rds; 01fp_house_county_wyoming.R) to the panel. 2026-09-24.
source(file.path("R", "00_setup.R")); source(file.path("R", "apply_helper.R"))
apply_state_years("wysos", 56, c(1996, 1998, 2006), expected_rows = 69, allow_replace = FALSE)
