## Add Arkansas House 2004 (46 counties; District 4 unopposed) and Delaware House 1998 (01jl_house_county_ar2004_de1998.R) to the panel. 2026-09-24.
source(file.path("R", "00_setup.R")); source(file.path("R", "apply_helper.R"))
apply_state_years("arsoscert", 5, 2004, expected_rows = 46, allow_replace = FALSE)
apply_state_years("deedsum", 10, 1998, expected_rows = 3, allow_replace = FALSE)
