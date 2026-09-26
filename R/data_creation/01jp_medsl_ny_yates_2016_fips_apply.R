## Recode Yates County, NY from MEDSL's 36122 to 36123 in the stored 2016 MEDSL House and Senate files and the panel. 2026-09-26.
## MEDSL's 2016 precinct files code every Yates precinct (jurisdiction "Yates") as FIPS 36122, which is not a county, with a blank county_name. The release
## scripts dropped 36122 as a non-county bucket, so Yates was missing from New York's 2016 Senate results (61 of 62 counties; above the 98% gaps threshold,
## so not listed as a gap). The House is unaffected in the panel (NY BOE 2016 supersedes MEDSL). 01a's read_precinct_file() now recodes it too, so
## 02a/02b rebuild with 36123. Gate: one 2016 row per file and one Senate panel row, all MEDSL-sourced; no 36123 row already there; values unchanged.
source(file.path("R", "00_setup.R"))
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")); prov <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final_provenance.rds"))
bk <- "/tmp/claude-1000/-home-paul-Stats-substack-projects-immigration/2b13fac3-077e-42ab-baf7-1daa6760b0c3/scratchpad/backup_yates"; dir.create(bk, showWarnings = FALSE, recursive = TRUE)
if (!file.exists(file.path(bk, "elect_cty_final_before_yates.rds"))) saveRDS(panel, file.path(bk, "elect_cty_final_before_yates.rds"))
for (o in c("he", "se")) {
  f <- file.path(OUTPUT_DIR, sprintf("elect_%s_cty_medsl.rds", o)); s <- readRDS(f); file.copy(f, file.path(bk, basename(f)), overwrite = FALSE)
  hit <- which(s$year == 2016 & s$cty_fips == 36122L)
  if (!length(hit)) { message(o, ": already recoded"); next }
  stopifnot(length(hit) == 1, !any(s$year == 2016 & s$cty_fips == 36123L, na.rm = TRUE))
  s$cty_fips[hit] <- 36123L; saveRDS(s, f); message(o, ": recoded 1 row in ", basename(f)) }
hit <- which(panel$sample == "SE" & panel$year == 2016 & panel$cty_fips == 36122L)
if (length(hit)) {
  stopifnot(length(hit) == 1, !any(panel$sample == "SE" & panel$year == 2016 & panel$cty_fips == 36123L))
  stopifnot((prov %>% filter(sample == "SE", year == 2016, cty_fips == 36122L))$source == "se_cty_medsl")
  p2 <- panel; p2$cty_fips[hit] <- 36123L; stopifnot(anyDuplicated(p2[, c("year", "cty_fips", "sample")]) == 0, nrow(p2) == nrow(panel), all(p2$cty_fips[-hit] == panel$cty_fips[-hit]))
  saveRDS(p2, file.path(OUTPUT_DIR, "elect_cty_final.rds")); message("panel: Senate 2016 Yates recoded 36122 -> 36123")
} else message("panel: already recoded")
