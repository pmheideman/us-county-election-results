## Add North Dakota House 1990-1998 (elect_he_cty_ndsos_<year>.rds; 01gi_house_county_northdakota_1990_1998.R) to the panel. 2026-09-24.
source(file.path("R", "00_setup.R")); source(file.path("R", "apply_helper.R"))
apply_state_years("ndsos", 38, seq(1990, 1998, 2), expected_rows = 265, allow_replace = FALSE)
