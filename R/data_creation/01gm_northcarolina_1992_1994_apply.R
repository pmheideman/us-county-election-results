## Add North Carolina House 1992 and 1994 (elect_he_cty_ncdcr_<year>.rds; 01gl_house_county_northcarolina_1992_1994.R) to the panel. 2026-09-24.
source(file.path("R", "00_setup.R")); source(file.path("R", "apply_helper.R"))
apply_state_years("ncdcr", 37, c(1992, 1994), expected_rows = 200, allow_replace = FALSE)
