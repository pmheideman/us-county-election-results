## Add Ohio House 2004 and 2010 (elect_he_cty_ohsosweb_<year>.rds; 01it_house_county_ohio_2004_2010.R) to the panel. 2026-09-24.
source(file.path("R", "00_setup.R")); source(file.path("R", "apply_helper.R"))
apply_state_years("ohsosweb", 39, c(2004, 2010), expected_rows = 176, allow_replace = FALSE)
