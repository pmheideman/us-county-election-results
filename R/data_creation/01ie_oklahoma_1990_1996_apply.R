## Add Oklahoma House 1990, 1992, 1996 (elect_he_cty_okseb_<year>.rds; 01id_house_county_oklahoma_1990_1996.R) to the panel. 2026-09-24.
source(file.path("R", "00_setup.R")); source(file.path("R", "apply_helper.R"))
apply_state_years("okseb", 40, c(1990, 1992, 1996), expected_rows = 231, allow_replace = FALSE)
