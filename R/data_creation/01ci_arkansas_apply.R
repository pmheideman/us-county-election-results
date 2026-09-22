## Fold the new official Arkansas House builds (elect_he_cty_ar_<year>.rds, years 1990, 1992-2000, 2002, 2006 as available) into the panel.
## Written 2026-09-20. Sources: Arkansas Secretary of State election books / certification reports (see 02r_house_ar_*.R).
##   * years with no AR House rows in the panel: rows are ADDED;
##   * 2002: the 74 partial OpenElections rows (source he_cty_ar) are REPLACED by the complete official build (75 counties; the 74 shared counties are identical in
##     dem share and total, verified in 02r_house_ar_2002.R; the extra county is Marion).
## Gate: before writing, the panel's AR House rows must be exactly the known state (2002/2008/2010/2012/2014 he_cty_ar rows + 2016+ MEDSL), otherwise abort.
## Nothing outside Arkansas (fips 5xxx) and House (sample "HE") changes; checked after the write. Backup: <scratchpad>/backup_ar/elect_cty_final_before_ar.rds
source(file.path("R", "00_setup.R"))
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds"))
bk <- file.path(Sys.getenv("CLAUDE_SCRATCH", unset = "/tmp/claude-1000/-home-paul-Stats-substack-projects-immigration/831fbe11-26b0-4ba2-9f03-261a9fbdf053/scratchpad"), "backup_ar"); dir.create(bk, showWarnings = FALSE)
if (!file.exists(file.path(bk, "elect_cty_final_before_ar.rds"))) saveRDS(panel, file.path(bk, "elect_cty_final_before_ar.rds"))
is_ar_he <- panel$sample == "HE" & panel$cty_fips %/% 1000 == 5
ar_now <- panel[is_ar_he, ]
old <- readRDS(file.path(OUTPUT_DIR, "elect_he_cty_ar.rds")) %>% filter(year != 2002 | TRUE)
chk <- ar_now %>% filter(year %in% 2002:2014) %>% inner_join(old, by = c("year", "cty_fips", "sample"), suffix = c("", ".s"))
stopifnot(all(abs(chk$demovote - chk$demovote.s) < 1e-9 & abs(chk$repuvote - chk$repuvote.s) < 1e-9 & abs(chk$totalvote - chk$totalvote.s) < 0.5))
stopifnot(!any(ar_now$year %in% c(1990:2000, 2004, 2006)), sum(ar_now$year == 2002) == 74)
message("gate passed: panel AR House rows are the known state (", nrow(ar_now), " rows)")

fs <- list.files(OUTPUT_DIR, pattern = "^elect_he_cty_ar_(1990|1992|1994|1996|1998|2000|2002|2006)\\.rds$", full.names = TRUE)
new <- purrr::map_dfr(fs, readRDS) %>% transmute(year, cty_fips, sample, demovote, repuvote, totalvote, state = NA_character_)
stopifnot(!anyDuplicated(new[, c("year", "cty_fips", "sample")]), all(new$sample == "HE"), !anyNA(new[, c("demovote", "repuvote", "totalvote")]), all(new$totalvote > 0))
message("adding/replacing years: ", paste(sort(unique(new$year)), collapse = ", "), " (", nrow(new), " rows)")
print(as.data.frame(new %>% count(year)))
p2 <- panel %>% filter(!(sample == "HE" & cty_fips %/% 1000 == 5 & year %in% unique(new$year)))
p2 <- bind_rows(p2, new[, names(panel)])
stopifnot(anyDuplicated(p2[, c("year", "cty_fips", "sample")]) == 0)
## nothing else changed
rest0 <- panel %>% filter(!is_ar_he) %>% arrange(year, cty_fips, sample); rest1 <- p2 %>% filter(!(sample == "HE" & cty_fips %/% 1000 == 5)) %>% arrange(year, cty_fips, sample)
stopifnot(isTRUE(all.equal(rest0, rest1, check.attributes = FALSE)))
saveRDS(p2, file.path(OUTPUT_DIR, "elect_cty_final.rds"))
message("panel rows ", nrow(panel), " -> ", nrow(p2), "; AR House rows ", nrow(ar_now), " -> ", sum(p2$sample == "HE" & p2$cty_fips %/% 1000 == 5))
