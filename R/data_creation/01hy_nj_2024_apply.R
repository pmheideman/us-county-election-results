## Replace New Jersey House 2024 (MEDSL: Sussex County missing, Bergen doubled, blank-party/nonpartisan rows; 4.34M votes in 20 counties vs 4.04M official in 21) with the
## Division of Elections' official results (elect_he_cty_nj_2024.rds; 02aa2_house_nj_2024.R: 54 candidates and 12 district totals tie exactly). 2026-09-24.
source(file.path("R", "00_setup.R")); source(file.path("R", "apply_helper.R"))
apply_state_years("nj", 34, 2024, expected_rows = 21)
