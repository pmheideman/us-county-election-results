## Fold Colorado House 1990/1992/1994/1996/1998/2000 (elect_he_cty_co_<year>.rds; see
## 01ea_colorado_download.R / 01eb_house_county_colorado_1990_2000.R, from the Colorado SOS's
## historical Elstats database) into the panel. Every row here is ADDED, none replaced (Colorado
## House had no rows before 2002, per OpenElections' own repo start date).
## Gate: no existing CO House rows for 1990-2000; afterwards, everything outside them is unchanged.
source(file.path("R", "00_setup.R"))

panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds"))
bk <- file.path(tempdir(), "backup_co_1990_2000"); dir.create(bk, showWarnings = FALSE, recursive = TRUE)
if (!file.exists(file.path(bk, "elect_cty_final_before_co_1990_2000.rds"))) saveRDS(panel, file.path(bk, "elect_cty_final_before_co_1990_2000.rds"))

yrs <- seq(1990, 2000, 2)
is_co_target <- panel$sample == "HE" & panel$cty_fips %/% 1000 == 8 & panel$year %in% yrs
stopifnot(sum(is_co_target) == 0)

new <- purrr::map_dfr(sprintf("elect_he_cty_co_%d.rds", yrs), function(f) readRDS(file.path(OUTPUT_DIR, f))) %>%
  transmute(year, cty_fips, sample, demovote, repuvote, totalvote, state = NA_character_)
stopifnot(!anyDuplicated(new[, c("year", "cty_fips", "sample")]), !anyNA(new[, c("demovote", "repuvote", "totalvote")]), all(new$totalvote > 0), nrow(new) == 63 * length(yrs))

add <- new %>% anti_join(panel %>% select(year, cty_fips, sample), by = c("year", "cty_fips", "sample"))
stopifnot(nrow(add) == nrow(new))
message("new CO House rows to add: ", nrow(add), " (63 counties x ", length(yrs), " years)")

p2 <- bind_rows(panel, new[, names(panel)])
stopifnot(anyDuplicated(p2[, c("year", "cty_fips", "sample")]) == 0)
other0 <- panel %>% arrange(year, cty_fips, sample)
other1 <- p2 %>% filter(!(sample == "HE" & cty_fips %/% 1000 == 8 & year %in% yrs)) %>% arrange(year, cty_fips, sample)
stopifnot(isTRUE(all.equal(other0, other1, check.attributes = FALSE)))

saveRDS(p2, file.path(OUTPUT_DIR, "elect_cty_final.rds")); message("panel rows ", nrow(panel), " -> ", nrow(p2))
