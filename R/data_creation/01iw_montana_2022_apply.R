## Add Montana House 2022 (elect_he_cty_medslmt_2022.rds; 01iv_house_county_montana_2022_medsl.R) to the panel. 2026-09-24.
source(file.path("R", "00_setup.R")); source(file.path("R", "apply_helper.R"))
apply_state_years("medslmt", 30, 2022, expected_rows = 56, allow_replace = FALSE)
