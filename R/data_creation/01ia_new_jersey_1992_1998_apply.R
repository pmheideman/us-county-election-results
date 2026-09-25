## Add New Jersey House 1992-1998 (elect_he_cty_njdoe_<year>.rds; 01hz_house_county_new_jersey_1992_1998.R) to the panel. 2026-09-24.
source(file.path("R", "00_setup.R")); source(file.path("R", "apply_helper.R"))
apply_state_years("njdoe", 34, c(1992, 1994, 1996, 1998), expected_rows = 84, allow_replace = FALSE)
