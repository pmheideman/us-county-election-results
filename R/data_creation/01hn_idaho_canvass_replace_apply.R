## Replace Idaho House 1994, 1996, 1998, 2000 (OpenElections: District 1 Republican Helen Chenoweth missing in 1994-1998, Libertarian Bramwell missing in 2000) and 2016 (MEDSL: one
## county undercounted) with the Idaho Secretary of State's Elections Database (elect_he_cty_idcv_<year>.rds; 01fm_house_county_idaho_canvass.R). 2026-09-24.
source(file.path("R", "00_setup.R")); source(file.path("R", "apply_helper.R"))
apply_state_years("idcv", 16, c(1994, 1996, 1998, 2000, 2016), expected_rows = 220)
