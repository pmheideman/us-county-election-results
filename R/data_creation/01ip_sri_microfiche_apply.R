## Fold SRI-microfiche House builds (elect_he_cty_sri<po>_<year>.rds; 01io_house_county_sri_microfiche.R) into the panel, one state at a time. 2026-09-24.
## Usage: Rscript R/data_creation/01ip_sri_microfiche_apply.R <state_po> <state_fips> <year> [<year> ...]   Replaces existing rows (partial years) and reports them.
source(file.path("R", "00_setup.R")); source(file.path("R", "apply_helper.R"))
a <- commandArgs(trailingOnly = TRUE); stopifnot(length(a) >= 3)
apply_state_years(paste0("sri", tolower(a[1])), as.integer(a[2]), as.integer(a[-(1:2)]))
