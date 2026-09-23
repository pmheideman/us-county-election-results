## Fold New Mexico House 2004/2006 (33 new keys each; see 01ei_house_county_new_mexico_statewide_pdf.R)
## into the panel -- neither year had any county-level House source before.
## Gate: no existing NM House rows for 2004/2006; afterwards, everything outside them is unchanged.
source(file.path("R", "00_setup.R"))

panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds"))
bk <- file.path(tempdir(), "backup_nm_2004_2006"); dir.create(bk, showWarnings = FALSE, recursive = TRUE)
if (!file.exists(file.path(bk, "elect_cty_final_before_nm_2004_2006.rds"))) saveRDS(panel, file.path(bk, "elect_cty_final_before_nm_2004_2006.rds"))

yrs <- c(2004, 2006)
is_nm_target <- panel$sample == "HE" & panel$cty_fips %/% 1000 == 35 & panel$year %in% yrs
stopifnot(sum(is_nm_target) == 0)

new <- purrr::map_dfr(sprintf("elect_he_cty_nm_%d.rds", yrs), function(f) readRDS(file.path(OUTPUT_DIR, f))) %>%
  transmute(year, cty_fips, sample, demovote, repuvote, totalvote, state = NA_character_)
stopifnot(!anyDuplicated(new[, c("year", "cty_fips", "sample")]), nrow(new) == 33 * length(yrs))

p2 <- bind_rows(panel, new[, names(panel)])
stopifnot(anyDuplicated(p2[, c("year", "cty_fips", "sample")]) == 0)
other0 <- panel %>% arrange(year, cty_fips, sample)
other1 <- p2 %>% filter(!(sample == "HE" & cty_fips %/% 1000 == 35 & year %in% yrs)) %>% arrange(year, cty_fips, sample)
stopifnot(isTRUE(all.equal(other0, other1, check.attributes = FALSE)))

message("panel rows ", nrow(panel), " -> ", nrow(p2), " (NM House 2004/2006: +", nrow(new), " new keys)")
saveRDS(p2, file.path(OUTPUT_DIR, "elect_cty_final.rds"))
