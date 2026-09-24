## Add South Dakota House 1992 (elect_he_cty_sdsos_1992.rds; 01gc_house_county_southdakota_1992.R) to the panel. 2026-09-24.
source(file.path("R", "00_setup.R")); source(file.path("R", "apply_helper.R"))
apply_state_years("sdsos", 46, 1992, expected_rows = 66, allow_replace = FALSE)
