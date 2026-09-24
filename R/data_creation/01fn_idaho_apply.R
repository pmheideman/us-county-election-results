## Fold Idaho 1990, 1992 (added) and 2022 (replaced: MEDSL had 43 of 44 counties, identical values for those; the 44th county is new) from elect_he_cty_idcv_<year>.rds (01fm_house_county_idaho_canvass.R). 2026-09-24.
source(file.path("R", "00_setup.R")); source(file.path("R", "apply_helper.R"))
apply_state_years("idcv", 16, c(1990, 1992, 2022), expected_rows = 132)
