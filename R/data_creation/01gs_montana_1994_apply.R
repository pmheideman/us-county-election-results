## Add Montana House 1990 and 1994 (elect_he_cty_mtsos_<year>.rds; 01gu_house_county_montana_1990.R, 01gr_house_county_montana_1994.R) to the panel. 2026-09-24.
source(file.path("R", "00_setup.R")); source(file.path("R", "apply_helper.R"))
apply_state_years("mtsos", 30, c(1990, 1994), expected_rows = 112, allow_replace = FALSE)
