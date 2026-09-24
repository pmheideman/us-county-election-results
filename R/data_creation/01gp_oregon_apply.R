## Add Oregon House 1990-1998, 2000, 2002, 2004, 2012 (elect_he_cty_orsl_<year>.rds; 01go_house_county_oregon.R) to the panel, replacing the earlier partial 2000, 2002, 2004 and 2012 rows. 2026-09-24.
source(file.path("R", "00_setup.R")); source(file.path("R", "apply_helper.R"))
apply_state_years("orsl", 41, c(seq(1990, 2004, 2), 2012), expected_rows = 36 * 9, allow_replace = TRUE)
