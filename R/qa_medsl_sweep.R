## QA sweep of the MEDSL-sourced House and Senate county rows (2016-2024) against independent references. 2026-09-21.
##  T1 county total vs the county's presidential total (same year in presidential years, else the nearest presidential year): flag counties whose ratio is > 1.35x or < 0.6x the state's median ratio
##     (a doubled county shows ~2x; a county with missing precincts <0.6x). Uncontested House districts lower the ratio legitimately, so House flags are listed with the county's districts.
##  T2 House vs Senate total in the same county and year (both MEDSL): ratio outside 0.6-1.6 with both offices present.
##  T3 party-line arithmetic: dem + rep share > 1 or a county whose Democratic and Republican shares are both zero although the total is > 1,000.
## Output: R/output/qa_medsl_sweep_flags.csv
source(file.path("R", "00_setup.R")); suppressMessages(library(dplyr))
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")); prov <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final_provenance.rds"))
d <- panel %>% filter(year >= 2016, sample %in% c("HE", "SE", "PE")) %>% inner_join(prov %>% select(year, cty_fips, sample, source), by = c("year", "cty_fips", "sample")) %>% mutate(st = cty_fips %/% 1000) %>% filter(!st %in% c(2, 11, 15))
pe <- d %>% filter(sample == "PE") %>% select(cty_fips, year_pe = year, pe = totalvote)
near_pe <- function(y) ifelse(y %% 4 == 0, y, ifelse(y == 2018, 2016, 2020)); pe_y <- function(y) near_pe(y)
t1 <- d %>% filter(sample %in% c("HE", "SE"), grepl("medsl", source)) %>% mutate(year_pe = pe_y(year)) %>% inner_join(pe, by = c("cty_fips", "year_pe")) %>% mutate(ratio = totalvote / pe) %>%
  group_by(sample, year, st) %>% mutate(med = median(ratio), rel = ratio / med, n_st = n()) %>% ungroup() %>% filter(n_st >= 3, totalvote > 2000, rel > 1.35 | rel < 0.6) %>% transmute(test = "T1_ratio_to_presidential", sample, year, cty_fips, st, totalvote, detail = sprintf("ratio %.2f vs state median %.2f (rel %.2f)", ratio, med, rel))
w <- d %>% filter(sample %in% c("HE", "SE"), grepl("medsl", source)) %>% select(sample, year, cty_fips, st, totalvote) %>% tidyr::pivot_wider(names_from = sample, values_from = totalvote)
t2 <- w %>% filter(!is.na(HE), !is.na(SE), HE > 2000, SE > 2000) %>% mutate(r = HE / SE) %>% filter(r > 1.6 | r < 0.6) %>% transmute(test = "T2_house_vs_senate", sample = "HE/SE", year, cty_fips, st, totalvote = HE, detail = sprintf("House %d vs Senate %d (ratio %.2f)", HE, SE, r))
t3 <- d %>% filter(sample %in% c("HE", "SE", "PE"), totalvote > 1000, (demovote + repuvote > 1.0001) | (demovote == 0 & repuvote == 0)) %>% transmute(test = "T3_share_arithmetic", sample, year, cty_fips, st, totalvote, detail = sprintf("dem %.3f rep %.3f", demovote, repuvote))
out <- bind_rows(t1, t2, t3) %>% arrange(test, sample, year, st, cty_fips); write.csv(out, file.path(OUTPUT_DIR, "qa_medsl_sweep_flags.csv"), row.names = FALSE)
cat("flags:", nrow(out), "\n"); print(as.data.frame(out %>% count(test, sample, year) %>% arrange(test, sample, year)))
print(as.data.frame(out %>% count(test, st) %>% arrange(desc(n)) %>% head(20)))
