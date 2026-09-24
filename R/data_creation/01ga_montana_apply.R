## Add Montana House 1992, 1996, 1998 (elect_he_cty_mtsos_<year>.rds; 01fz_house_county_montana_1992_1998.R) to the panel. 2026-09-24.
source(file.path("R", "00_setup.R")); source(file.path("R", "apply_helper.R"))
apply_state_years("mtsos", 30, c(1992, 1996, 1998), expected_rows = 168, allow_replace = FALSE)
