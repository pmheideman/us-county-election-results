## Replace Utah House 2000 (elect_he_cty_ut_2000.rds; 02ad_house_ut_2000_2006.R 2000 block corrected 2026-09-24: Senate candidates Dexter and Bowen had been
## counted as District 1, and District 1's Collinwood (D), Seely, Anderson and Frandsen were dropped; Bangerter moved from District 3 to 2).
source(file.path("R", "00_setup.R")); source(file.path("R", "apply_helper.R"))
apply_state_years("ut", 49, 2000, expected_rows = 29)
