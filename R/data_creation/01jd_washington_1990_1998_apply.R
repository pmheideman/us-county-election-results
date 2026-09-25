## Add Washington House 1990-1998 (elect_he_cty_wasosdb_<year>.rds; 01jc_house_county_washington_1990_1998.R) to the panel. 2026-09-24.
source(file.path("R", "00_setup.R")); source(file.path("R", "apply_helper.R"))
apply_state_years("wasosdb", 53, c(1990, 1992, 1994, 1996, 1998), expected_rows = 195, allow_replace = FALSE)
