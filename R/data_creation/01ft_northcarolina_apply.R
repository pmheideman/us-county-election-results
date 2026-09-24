## Add North Carolina House 1996 and 1998 (elect_he_cty_ncsbe_<year>.rds; 01fs_house_county_northcarolina_1996_1998.R) to the panel. 2026-09-24.
source(file.path("R", "00_setup.R")); source(file.path("R", "apply_helper.R"))
apply_state_years("ncsbe", 37, c(1996, 1998), expected_rows = 200, allow_replace = FALSE)
