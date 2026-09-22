## Pulls the raw IPUMS microdata needed to extend the pipeline through 2024.
##
## Two separate gaps, found by checking the actual year range of the built .rds outputs against
## what the 2024 extension needs (Census_cz_final.rds tops out at 2016; election_immi_CPS_IV.rds
## tops out at 2018; the election panel itself already reaches 2024 via MEDSL):
##
##  (1) IPUMS CPS ASEC, 2019-2024: same shape as the shipped Input/CPS_citizen.dat (a raw
##      person-level fixed-width extract -- see 5th_CPS_read.do's `infix` block for its exact
##      layout), which 05_cps_read.R's aggregation logic already consumes directly. Extending
##      this is a straight drop-in: pull the same variables for the missing years and append.
##
##  (2) IPUMS USA (ACS), 2017-2024: the shipped Input/IPUMS/*_2002_2016.dta files
##      (Census_cty_2002_2016.dta, BPL_cty_2002_2016.dta, voters_cty_2002_2016.dta,
##      totalsAWEIGHTS_cty_2002_2016.dta) are NOT raw extracts -- they're already collapsed to
##      statefip x puma x year, with pre-built "universe" and education-bucket columns
##      (nat_education1-5, imm_rich_universe, voters_income, etc.). The original authors' code
##      that collapsed raw ACS microdata into that shape isn't part of this replication package
##      -- only the already-aggregated output was shipped. So this pull is raw ACS person-level
##      microdata with a variable list broad enough to reconstruct those same buckets; a NEW
##      aggregation script (mirroring the *definitions* implied by the existing columns) still
##      needs to be written before this feeds 02_census_merge_cz.R / 03_composition_immigrants_income.R.
##      Tracked as a follow-up, not done in this script.
##
##  Also note: ACS switched from 2010-vintage to 2020-vintage PUMA geography starting with the
##  2022 ACS. The existing crosswalks (cw_puma1990/2000_czone) don't cover 2020 PUMAs -- a new
##  PUMA(2020)->czone crosswalk (or 2020->2010 PUMA correspondence file) will be needed separately
##  before the 2022-2024 ACS rows can be assigned to commuting zones.

library(ipumsr)

PROJECT_ROOT <- "/home/paul/Stats/substack_projects/immigration"
readRenviron(file.path(PROJECT_ROOT, ".Renviron"))
set_ipums_api_key(Sys.getenv("IPUMS_API_KEY"))

RAW_CPS_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_ipums_cps")
RAW_USA_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_ipums_usa")
dir.create(RAW_CPS_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(RAW_USA_DIR, showWarnings = FALSE, recursive = TRUE)

## ---- (1) IPUMS CPS ASEC 2019-2024, matching CPS_citizen.dat's variable list ----
## HFLAG ("Flag for the 3/8 file 2014") was specific to the 2014 sample-split and isn't available
## in 2019-2024 samples -- dropped here, unused by 05_cps_read.R anyway.
## Note: unlike IPUMS USA, IPUMS CPS has no separate BPL/BPLD split -- its single BPL variable is
## already detailed (5-digit codes, e.g. 60045 = Kenya), matching what classify_cobgrp() expects.
cps_vars <- c("YEAR", "SERIAL", "MONTH", "CPSID", "ASECFLAG", "ASECWTH", "STATEFIP",
              "PERNUM", "CPSIDP", "ASECWT", "AGE", "BPL", "YRIMMIG", "CITIZEN", "NATIVITY", "EDUC99")

cps_extract <- define_extract_micro(
  collection = "cps",
  description = "Mayda et al 2022 extension: ASEC 2019-2024, matching Input/CPS_citizen.dat layout",
  samples = paste0("cps", 2019:2024, "_03s"),
  variables = cps_vars
)

## ---- (2) IPUMS USA (ACS) 2017-2024, raw person-level, broad variable list ----
## Variable list chosen to cover everything the existing aggregate columns imply is needed:
## nativity/citizenship/origin (BPL, BPLD, CITIZEN, YRIMMIG), education (EDUC, EDUCD), race/
## ethnicity (RACE, RACED, HISPAN, HISPAND), the "voters" universe (AGE, CITIZEN), employment
## (EMPSTAT, EMPSTATD), marital status (MARST), urbanicity (METRO, DENSITY), income (INCTOT), and
## PUMA-level geography (STATEFIP, PUMA) for the later czone crosswalk step.
usa_vars <- c("YEAR", "SAMPLE", "SERIAL", "PERNUM", "PERWT", "STATEFIP", "PUMA", "METRO", "DENSITY",
              "AGE", "SEX", "MARST", "RACE", "RACED", "HISPAN", "HISPAND", "BPL", "BPLD", "CITIZEN",
              "YRIMMIG", "EDUC", "EDUCD", "EMPSTAT", "EMPSTATD", "INCTOT")

usa_extract <- define_extract_micro(
  collection = "usa",
  description = "Mayda et al 2022 extension: ACS 2017-2024 raw microdata (pre-aggregation not yet ported)",
  samples = paste0("us", 2017:2024, "a"),
  variables = usa_vars
)

## ---- Submit both, save the definitions (for reproducibility / resubmission), wait, download ----
save_extract_as_json(cps_extract, file.path(RAW_CPS_DIR, "extract_definition.json"), overwrite = TRUE)
save_extract_as_json(usa_extract, file.path(RAW_USA_DIR, "extract_definition.json"), overwrite = TRUE)

message("Submitting CPS extract...")
cps_submitted <- submit_extract(cps_extract)
message("Submitting USA extract...")
usa_submitted <- submit_extract(usa_extract)

## Download each right after its own wait, rather than waiting for both before downloading
## anything -- wait_for_extract has been observed to occasionally hang on the status-polling
## HTTP call for large extracts (its own 3-hour timeout_seconds didn't trigger), so this keeps a
## stuck USA wait from also blocking the (usually much faster) CPS download.
message("Waiting for CPS extract to finish processing...")
cps_ready <- wait_for_extract(cps_submitted)
message("Downloading CPS extract...")
download_extract(cps_ready, download_dir = RAW_CPS_DIR, overwrite = TRUE)

message("Waiting for USA extract to finish processing (this one is much larger, may take a while)...")
usa_ready <- wait_for_extract(usa_submitted)
message("Downloading USA extract...")
download_extract(usa_ready, download_dir = RAW_USA_DIR, overwrite = TRUE)

message("Done. Raw files in ", RAW_CPS_DIR, " and ", RAW_USA_DIR)
