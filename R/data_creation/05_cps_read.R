## Port of Files/Dofiles/Data_creation/5th_CPS_read.do
##
## Builds the shift-share ("Bartik") instrument for immigrant inflows by county, in two
## vintages that get appended together:
##   (A) 1980->1990, built from the Census/ACS-derived BPL_cty_1980_final / BPL_cty_1990_final
##       country-of-birth counts (commuting-zone level), and
##   (B) 1992->2016+, built from the IPUMS CPS ASEC microdata (CPS_citizen.dat), which gives
##       annual (not just decennial) national immigrant counts by country-of-birth group.
## Both vintages allocate the *national* change in immigration from each country-of-birth
## group to a given county using that county's *initial* 1980 share of that group's stock
## (sh_c_i_80) -- the classic shift-share/Bartik construction -- then accumulate these shares
## into predicted ("hat_") levels via hat[t] = hat[t-1] + dhat[t], seeded at fb (the county's
## actual 1980 foreign-born count). Output: election_immi_CPS_IV.rds.
##
## Faithfulness notes:
##  - The county-level 1980 initial-distribution block (natives_universe, sh_c_us_80,
##    sh_c_i_80, the fb_cob reshape) is IDENTICAL to the block already computed in
##    03_composition_immigrants_income.R (same source file, same output file names in the
##    original Stata) -- reused here via the saved 1980_pop_cty / 1980-2010_pop_cty rds
##    files rather than recomputed.
##  - Unlike 03_composition_immigrants_income.R's cobgrp blocks, this file's 1980/1990 census
##    block (a) filters the Natives_cz_final join to matched rows only (inner_join, not
##    full_join) and (b) does NOT drop cobgrp==0 (US-born / born-abroad-to-American-parents),
##    so a small "domestic" bucket stays in the country-of-birth totals here. Both replicated
##    exactly as departures from the 03 script's version of the same-looking block.
##  - `gen hat_imm_education_low_<i> = ...; replace ... = dhat_imm_education_low_<i> +
##    l.hat_imm_education_low if year>1980` (source line ~410) uses the *overall* lagged level,
##    not the group-<i>-specific lag, for the low-education leave-one-out instrument -- while
##    the parallel high-education line correctly uses the group-specific lag. This looks like a
##    copy/paste slip in the original but is reproduced exactly, since "fixing" it would change
##    every table built on this instrument.
##  - The "rich" country flag (source ~line 323, `rich=1 if cobgrp==1|4|4|7|3|14`) again includes
##    cobgrp==3 (Mexico) -- same inconsistency as in 03_composition_immigrants_income.R,
##    reproduced as-is.

source(file.path("R", "00_setup.R"))
source(file.path("R", "utils.R"))

classify_cobgrp <- function(bpld) {
  cobgrp <- rep(NA_real_, length(bpld))
  cobgrp[bpld < 15000 | bpld %in% c(90011, 90021)] <- 0
  cobgrp[bpld >= 15000 & bpld < 16000] <- 1
  cobgrp[bpld >= 16000 & bpld < 20000] <- 2
  cobgrp[bpld >= 21000 & bpld < 31000] <- 2
  cobgrp[bpld >= 20000 & bpld < 21000] <- 3
  cobgrp[(bpld >= 40000 & bpld <= 42900) | (bpld >= 43100 & bpld <= 44000) |
           bpld == 45000 | bpld == 45300] <- 4
  cobgrp[bpld == 43000 | (bpld >= 45100 & bpld <= 46500) | bpld == 45303 | bpld == 45340] <- 5
  cobgrp[bpld >= 50000 & bpld < 50100] <- 6
  cobgrp[bpld == 50100] <- 7
  cobgrp[bpld == 50200] <- 8
  cobgrp[bpld == 51500] <- 9
  cobgrp[bpld == 51800] <- 10
  cobgrp[bpld >= 52100 & bpld < 52200] <- 11
  cobgrp[(bpld >= 50900 & bpld <= 51400) | (bpld >= 51600 & bpld <= 51700) |
           (bpld >= 51900 & bpld <= 52000) | (bpld >= 52200 & bpld <= 59900)] <- 12
  cobgrp[bpld >= 60000 & bpld < 70000] <- 13
  cobgrp[bpld >= 70000 & bpld < 80000] <- 14
  cobgrp[bpld == 49900 | (bpld > 80000 & bpld < 95000)] <- 15
  cobgrp[is.na(cobgrp)] <- 15
  cobgrp
}

pop_cty_1980 <- readRDS(file.path(OUTPUT_DIR, "1980_pop_cty.rds"))          # cobgrp, fips_tc, sh_c_i_80, sh_c_us_80 (year==1980 only)
pop_cty_full <- readRDS(file.path(OUTPUT_DIR, "1980-2010_pop_cty.rds"))     # fips_tc, year, cobgrp, fb_cob + raw Census pop counts
natives_cz_final <- readRDS(file.path(OUTPUT_DIR, "Natives_cz_final.rds"))
czone_rich_poor <- readRDS(file.path(OUTPUT_DIR, "Czone_rich_poor.rds"))
czone_mex <- readRDS(file.path(OUTPUT_DIR, "Czone_mex.rds"))
czone_lat <- readRDS(file.path(OUTPUT_DIR, "Czone_lat.rds"))
cw_cty_czone <- read_stata(file.path(INPUT_DIR, "cw_cty_czone.dta"))

## =============================================================================
## PART A: 1980 -> 1990 instrument, from Census/ACS country-of-birth counts
## =============================================================================

## ---- CZ-level initial skill shares for 1980/1990 (source lines 64-193) ----
bpl_8090 <- bind_rows(
  readRDS(file.path(OUTPUT_DIR, "BPL_cty_1980_final.rds")),
  readRDS(file.path(OUTPUT_DIR, "BPL_cty_1990_final.rds"))
) %>%
  inner_join(natives_cz_final, by = c("year", "czone"), relationship = "many-to-many") %>%
  rename(t_natives_universe = nat_natives_universes) %>%
  mutate(cobgrp = classify_cobgrp(bpld)) %>%
  filter(!is.na(cobgrp)) %>%     # cobgrp==0 (US-born) is intentionally kept here
  mutate(
    imm_unskill      = cit_education1 + noncit_education1,
    imm_skill         = cit_education2 + cit_education3 + cit_education4 + cit_education5 +
                          noncit_education2 + noncit_education3 + noncit_education4 + noncit_education5,
    immNCIT_unskill = noncit_education1,
    immNCIT_skill    = noncit_education2 + noncit_education3 + noncit_education4 + noncit_education5
  )

czone_skill_totals <- bpl_8090 %>%
  group_by(year, czone) %>%
  summarise(
    c_imm_education_low  = sum(imm_unskill, na.rm = TRUE),
    c_imm_education_high = sum(imm_skill, na.rm = TRUE),
    c_immNCIT_edu_low    = sum(immNCIT_unskill, na.rm = TRUE),
    c_immNCIT_edu_high   = sum(immNCIT_skill, na.rm = TRUE),
    c_imm_universe        = sum(imm_universe, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    czone_imm_education_low_share  = c_imm_education_low / c_imm_universe,
    czone_imm_education_high_share = c_imm_education_high / c_imm_universe,
    czone_immNCIT_edu_low_share    = c_immNCIT_edu_low / c_imm_universe,
    czone_immNCIT_edu_high_share   = c_immNCIT_edu_high / c_imm_universe
  ) %>%
  left_join(czone_rich_poor, by = c("year", "czone")) %>%
  left_join(czone_mex, by = c("year", "czone")) %>%
  left_join(czone_lat, by = c("year", "czone")) %>%
  mutate(
    czone_imm_poor_low_share  = rp_imm_unskill / c_imm_universe,
    czone_imm_poor_high_share = rp_imm_skill / c_imm_universe,
    czone_imm_rich_low_share  = r_imm_unskill / c_imm_universe,
    czone_imm_rich_high_share = r_imm_skill / c_imm_universe,
    czone_imm_mex_low_share    = mex_imm_unskill / c_imm_universe,
    czone_imm_mex_high_share   = mex_imm_skill / c_imm_universe,
    czone_imm_nomex_low_share  = nomex_imm_unskill / c_imm_universe,
    czone_imm_nomex_high_share = nomex_imm_skill / c_imm_universe,
    czone_imm_lat_low_share    = lat_imm_unskill / c_imm_universe,
    czone_imm_lat_high_share   = lat_imm_skill / c_imm_universe,
    czone_imm_nolat_low_share  = nolat_imm_unskill / c_imm_universe,
    czone_imm_nolat_high_share = nolat_imm_skill / c_imm_universe
  ) %>%
  select(year, czone, starts_with("czone_")) %>%
  save_step("test_initial_skill")

## ---- National (by cobgrp) YoY change in immigrant counts, 1980->1990 (source lines 197-260) ----
national_cobgrp_change <- bpl_8090 %>%
  group_by(year, cobgrp) %>%
  summarise(
    c_imm_universe        = sum(imm_universe, na.rm = TRUE),
    c_imm_education_low  = sum(imm_unskill, na.rm = TRUE),
    c_imm_education_high = sum(imm_skill, na.rm = TRUE),
    c_immNCIT_edu_low    = sum(immNCIT_unskill, na.rm = TRUE),
    c_immNCIT_edu_high   = sum(immNCIT_skill, na.rm = TRUE),
    t_natives_universe    = first(t_natives_universe),
    .groups = "drop"
  ) %>%
  arrange(cobgrp, year) %>%
  group_by(cobgrp) %>%
  mutate(
    d_c_imm_education_low   = c_imm_education_low - lag(c_imm_education_low),
    d_c_imm_education_high  = c_imm_education_high - lag(c_imm_education_high),
    d_c_immNCIT_edu_low     = c_immNCIT_edu_low - lag(c_immNCIT_edu_low),
    d_c_immNCIT_edu_high    = c_immNCIT_edu_high - lag(c_immNCIT_edu_high)
  ) %>%
  ungroup()

# National (not fips_tc- or cobgrp-specific) YoY change in the total native population, used to
# scale each county's 1980 native share (sh_c_us_80) into a predicted county-level native count.
national_native_change <- national_cobgrp_change %>%
  distinct(year, t_natives_universe) %>%
  arrange(year) %>%
  mutate(d_nat_natives_universe = t_natives_universe - lag(t_natives_universe))

## Broadcast each county's initial-1980 country-of-birth share (sh_c_i_80) and raw 1980-2010
## Census counts (fb, totpop, ...) onto every (fips_tc x cobgrp x year) combination, then
## attach the county -> commuting-zone crosswalk.
county_cobgrp_year <- pop_cty_1980 %>%
  select(cobgrp, fips_tc, sh_c_i_80, sh_c_us_80) %>%
  inner_join(national_cobgrp_change, by = "cobgrp", relationship = "many-to-many") %>%
  inner_join(pop_cty_full, by = c("fips_tc", "year", "cobgrp")) %>%
  select(-tot_c_fb_cob) %>%
  left_join(cw_cty_czone %>% select(cty_fips, czone), by = c("fips_tc" = "cty_fips")) %>%
  save_step("test_1980_1990")

## ---- Combine with the CZ-level initial skill shares and build the shift-share deltas (source lines 264-425) ----
base_8090 <- county_cobgrp_year %>%
  inner_join(czone_skill_totals, by = c("year", "czone"), relationship = "many-to-many") %>%
  mutate(
    mex  = as.integer(cobgrp == 3),
    lat   = as.integer(cobgrp %in% c(2, 3)),
    rich = as.integer(cobgrp %in% c(1, 4, 7, 3, 14))  # includes Mexico (cobgrp==3) -- see file header note
  )

county_year_totals <- base_8090 %>%
  group_by(year, fips_tc) %>%
  summarise(
    dhat_imm_education_low   = sum(sh_c_i_80 * d_c_imm_education_low, na.rm = TRUE),
    dhat_imm_education_high  = sum(sh_c_i_80 * d_c_imm_education_high, na.rm = TRUE),
    dhat_immNCIT_edu_low     = sum(sh_c_i_80 * d_c_immNCIT_edu_low, na.rm = TRUE),
    dhat_immNCIT_edu_high    = sum(sh_c_i_80 * d_c_immNCIT_edu_high, na.rm = TRUE),
    fb = first(fb), totpop = first(totpop), fb_notcit_o18 = first(fb_notcit_o18), fb_natcit_o18 = first(fb_natcit_o18),
    sh_c_us_80 = first(sh_c_us_80),
    czone_imm_education_low_share = first(czone_imm_education_low_share),
    czone_immNCIT_edu_low_share    = first(czone_immNCIT_edu_low_share),
    czone_imm_education_high_share = first(czone_imm_education_high_share),
    czone_immNCIT_edu_high_share   = first(czone_immNCIT_edu_high_share),
    czone_imm_rich_low_share = first(czone_imm_rich_low_share), czone_imm_poor_low_share = first(czone_imm_poor_low_share),
    czone_imm_rich_high_share = first(czone_imm_rich_high_share), czone_imm_poor_high_share = first(czone_imm_poor_high_share),
    czone_imm_lat_low_share = first(czone_imm_lat_low_share), czone_imm_nolat_low_share = first(czone_imm_nolat_low_share),
    czone_imm_lat_high_share = first(czone_imm_lat_high_share), czone_imm_nolat_high_share = first(czone_imm_nolat_high_share),
    czone_imm_mex_low_share = first(czone_imm_mex_low_share), czone_imm_nomex_low_share = first(czone_imm_nomex_low_share),
    czone_imm_mex_high_share = first(czone_imm_mex_high_share), czone_imm_nomex_high_share = first(czone_imm_nomex_high_share),
    .groups = "drop"
  ) %>%
  left_join(national_native_change %>% select(year, d_nat_natives_universe), by = "year")

# mex/lat/rich splits and the 15-way leave-one-out splits, each as a (year,fips_tc)-level pair of columns.
add_split <- function(totals, base, flag, value, label_in, label_out) {
  s <- shift_share_split(base, id_vars = c("year", "fips_tc"), flag = flag, weight = "sh_c_i_80", value = value)
  names(s)[names(s) == "in_group"] <- label_in
  names(s)[names(s) == "out_group"] <- label_out
  left_join(totals, s, by = c("year", "fips_tc"))
}

county_year_totals <- county_year_totals %>%
  add_split(base_8090, "mex", "d_c_imm_education_low", "dhat_imm_education_low_mex", "dhat_imm_education_low_nmex") %>%
  add_split(base_8090, "mex", "d_c_imm_education_high", "dhat_imm_education_high_mex", "dhat_imm_education_high_nmex") %>%
  add_split(base_8090, "lat", "d_c_imm_education_low", "dhat_imm_education_low_lat", "dhat_imm_education_low_nlat") %>%
  add_split(base_8090, "lat", "d_c_imm_education_high", "dhat_imm_education_high_lat", "dhat_imm_education_high_nlat") %>%
  add_split(base_8090, "rich", "d_c_imm_education_low", "dhat_imm_education_low_rich", "dhat_imm_education_low_nrich") %>%
  add_split(base_8090, "rich", "d_c_imm_education_high", "dhat_imm_education_high_rich", "dhat_imm_education_high_nrich")

# Leave-group-i-out shift-share totals: grand total minus group i's own (year,fips_tc) contribution.
own_contrib <- base_8090 %>%
  mutate(contrib_low = sh_c_i_80 * d_c_imm_education_low, contrib_high = sh_c_i_80 * d_c_imm_education_high) %>%
  select(year, fips_tc, cobgrp, contrib_low, contrib_high)

for (i in 1:15) {
  own_i <- own_contrib %>% filter(cobgrp == i) %>% select(year, fips_tc, own_low = contrib_low, own_high = contrib_high)
  county_year_totals <- county_year_totals %>%
    left_join(own_i, by = c("year", "fips_tc")) %>%
    mutate(own_low = replace_na(own_low, 0), own_high = replace_na(own_high, 0))
  county_year_totals[[paste0("dhat_imm_education_low_", i)]] <- county_year_totals$dhat_imm_education_low - county_year_totals$own_low
  county_year_totals[[paste0("dhat_imm_education_high_", i)]] <- county_year_totals$dhat_imm_education_high - county_year_totals$own_high
  county_year_totals <- county_year_totals %>% select(-own_low, -own_high)
}

## ---- Accumulate deltas into predicted levels: hat[1980] = fb*share; hat[t] = dhat[t] + hat[t-1] ----
base_1980 <- county_year_totals %>% filter(year == 1980) %>% select(fips_tc, ends_with("share"), fb, totpop, fb_notcit_o18, fb_natcit_o18)

level_1980 <- base_1980 %>%
  transmute(
    fips_tc, year = 1980,
    hat_imm_education_low        = fb * czone_imm_education_low_share,
    hat_imm_education_high       = fb * czone_imm_education_high_share,
    hat_immNCIT_edu_low          = fb * czone_immNCIT_edu_low_share,
    hat_immNCIT_edu_high         = fb * czone_immNCIT_edu_high_share,
    hat_imm_education_low_rich   = fb * czone_imm_rich_low_share,
    hat_imm_education_low_nrich  = fb * czone_imm_poor_low_share,
    hat_imm_education_high_rich  = fb * czone_imm_rich_high_share,
    hat_imm_education_high_nrich = fb * czone_imm_poor_high_share,
    hat_imm_education_low_lat    = fb * czone_imm_lat_low_share,
    hat_imm_education_low_nlat   = fb * czone_imm_nolat_low_share,
    hat_imm_education_high_lat   = fb * czone_imm_lat_high_share,
    hat_imm_education_high_nlat  = fb * czone_imm_nolat_high_share,
    hat_imm_education_low_mex    = fb * czone_imm_mex_low_share,
    hat_imm_education_low_nmex   = fb * czone_imm_nomex_low_share,
    hat_imm_education_high_mex   = fb * czone_imm_mex_high_share,
    hat_imm_education_high_nmex  = fb * czone_imm_nomex_high_share,
    hat_imm_universe     = fb_notcit_o18 + fb_natcit_o18,
    hat_natives_universe = totpop - fb
  )
for (i in 1:15) {
  level_1980[[paste0("hat_imm_education_low_", i)]]  <- base_1980$fb * base_1980$czone_imm_education_low_share
  level_1980[[paste0("hat_imm_education_high_", i)]] <- base_1980$fb * base_1980$czone_imm_education_high_share
}

deltas_1990 <- county_year_totals %>% filter(year == 1990)

# All 1980 predicted levels, renamed with a prev_ prefix and keyed only by fips_tc, so they join
# onto the 1990 deltas below with no name collisions (avoids fragile join-suffix matching).
prev_1980 <- level_1980 %>% select(-year) %>% rename_with(~ paste0("prev_", .x), -fips_tc)
joined_1990 <- deltas_1990 %>% left_join(prev_1980, by = "fips_tc")

level_1990 <- joined_1990 %>%
  transmute(
    fips_tc, year = 1990,
    hat_imm_education_low        = dhat_imm_education_low + prev_hat_imm_education_low,
    hat_imm_education_high       = dhat_imm_education_high + prev_hat_imm_education_high,
    hat_immNCIT_edu_low          = dhat_immNCIT_edu_low + prev_hat_immNCIT_edu_low,
    hat_immNCIT_edu_high         = dhat_immNCIT_edu_high + prev_hat_immNCIT_edu_high,
    hat_imm_education_low_rich   = dhat_imm_education_low_rich + prev_hat_imm_education_low_rich,
    hat_imm_education_low_nrich  = dhat_imm_education_low_nrich + prev_hat_imm_education_low_nrich,
    hat_imm_education_high_rich  = dhat_imm_education_high_rich + prev_hat_imm_education_high_rich,
    hat_imm_education_high_nrich = dhat_imm_education_high_nrich + prev_hat_imm_education_high_nrich,
    hat_imm_education_low_lat    = dhat_imm_education_low_lat + prev_hat_imm_education_low_lat,
    hat_imm_education_low_nlat   = dhat_imm_education_low_nlat + prev_hat_imm_education_low_nlat,
    hat_imm_education_high_lat   = dhat_imm_education_high_lat + prev_hat_imm_education_high_lat,
    hat_imm_education_high_nlat  = dhat_imm_education_high_nlat + prev_hat_imm_education_high_nlat,
    hat_imm_education_low_mex    = dhat_imm_education_low_mex + prev_hat_imm_education_low_mex,
    hat_imm_education_low_nmex   = dhat_imm_education_low_nmex + prev_hat_imm_education_low_nmex,
    hat_imm_education_high_mex   = dhat_imm_education_high_mex + prev_hat_imm_education_high_mex,
    hat_imm_education_high_nmex  = dhat_imm_education_high_nmex + prev_hat_imm_education_high_nmex,
    hat_imm_universe     = (dhat_imm_education_low + dhat_imm_education_high) + prev_hat_imm_universe,
    hat_natives_universe = sh_c_us_80 * d_nat_natives_universe + prev_hat_natives_universe
  )
for (i in 1:15) {
  # NB: the low-education leave-one-out instrument adds the *overall* 1980 level (not the
  # group-i-specific one) -- see file header note. The high-education one is group-specific.
  level_1990[[paste0("hat_imm_education_low_", i)]] <-
    joined_1990[[paste0("dhat_imm_education_low_", i)]] + joined_1990$prev_hat_imm_education_low
  level_1990[[paste0("hat_imm_education_high_", i)]] <-
    joined_1990[[paste0("dhat_imm_education_high_", i)]] + joined_1990[[paste0("prev_hat_imm_education_high_", i)]]
}

# natives_universe / imm_universe (kept in the final `keep` list, source lines 419-420 & 427)
fb_o18 <- pop_cty_full %>% distinct(fips_tc, year, totpop, fb, fb_o18) %>%
  transmute(fips_tc, year, natives_universe = totpop - fb, imm_universe = fb_o18)

level_1980 <- level_1980 %>% left_join(fb_o18, by = c("fips_tc", "year"))
level_1990 <- level_1990 %>% left_join(fb_o18, by = c("fips_tc", "year"))

instruments_1980_1990 <- bind_rows(level_1980, level_1990) %>%
  filter(!is.na(hat_natives_universe)) %>%
  arrange(fips_tc, year) %>%
  save_step("Instruments_census_1980_1990")

## =============================================================================
## PART B: 1994 -> 2018 instrument, from annual IPUMS CPS ASEC microdata
## =============================================================================
##
## Unlike Part A (which only has Census/ACS snapshots every ~2-10 years and so must
## *accumulate* shift-share deltas into a level), the CPS gives an annual national count of
## immigrants by country-of-birth group directly, so here hat_imm_education_low/high is a
## direct cross-sectional shift-share allocation of that year's national LEVEL (source lines
## 1086-1092), not a recursive hat[t] = hat[t-1] + dhat[t] accumulation. The "dhat_" (delta)
## variables computed alongside it in the source (lines 1074-1078, 1084-1085, 1098-1099, ...)
## are carried through several joins but never appear in the final `keep` (source line 1241)
## or in the final imm_share_IV construction (source lines 1304+) -- they are dead code
## relative to the saved output, so they are not reproduced here.
##
## Bug-for-bug note: the per-country-of-birth-group instrument `hat_imm_education_low_<i>` /
## `_high_<i>` (source lines 1206-1218) computes `test_low_2`/`test_high_2` as a leave-group-i
## -out version of imm_unskill/imm_skill, but the subsequent `egen hat_imm_education_low_<i> =
## total(sh_c_i_80*imm_unskill)` references `imm_unskill` directly, not `test_low_2` -- so for
## every i, hat_imm_education_low_<i>/high_<i> here is identical to the overall
## hat_imm_education_low/high, not an actual leave-one-out level. Reproduced as written.

## ---- Read + collapse the CPS ASEC microdata to national (year x cobgrp) totals ----
## Combines two vintages of the raw extract:
##  - Input/CPS_citizen.dat: the original 1994-2018 fixed-width extract shipped with the
##    replication package (infix layout per 5th_CPS_read.do's dictionary).
##  - R/data/raw_ipums_cps/cps_00034.xml (+ .dat.gz): a same-shape IPUMS CPS API pull for
##    2019-2024 (built by 00b_fetch_ipums_extension.R), read via ipumsr so its implied-decimal
##    weight scaling is applied automatically -- CPS_citizen.dat's raw fixed-width ASECWT column,
##    by contrast, is an integer that must be divided by 10000 by hand (below). IPUMS CPS's BPL
##    variable has no separate detailed/coarse split (unlike IPUMS USA's BPL/BPLD) -- it's already
##    at the same 5-digit-code detail as CPS_citizen.dat's "bpld" column, so no remapping needed.
cps_positions <- readr::fwf_positions(
  start = c(1, 56, 66, 68, 77, 78, 79),
  end   = c(4, 65, 67, 72, 77, 78, 80),
  col_names = c("year", "asecwt_raw", "age", "bpld", "citizen", "nativity", "educ99")
)
cps_raw_orig <- readr::read_fwf(file.path(INPUT_DIR, "CPS_citizen.dat"), cps_positions, col_types = readr::cols(.default = "d")) %>%
  transmute(year, wtsupp = asecwt_raw / 10000, age, bpld, citizen, nativity, educ99)

cps_raw_ext <- ipumsr::read_ipums_micro(
  file.path(PROJECT_ROOT, "R", "data", "raw_ipums_cps", "cps_00034.xml"), verbose = FALSE
) %>%
  transmute(year = YEAR, wtsupp = ASECWT, age = AGE, bpld = BPL, citizen = CITIZEN, nativity = NATIVITY, educ99 = EDUC99)

cps_raw <- bind_rows(cps_raw_orig, cps_raw_ext)

low_ed  <- c(1, 4, 5, 6, 7, 8, 9)
high_ed <- c(12, 13, 14, 15)
top_ed  <- c(16, 17, 18)

cps_indiv <- cps_raw %>%
  mutate(
    native            = as.integer(nativity %in% 1:4),
    natives_universe = if_else(age >= 18 & native == 1, 1, NA_real_),
    immigrantNONCIT    = as.integer(nativity == 5 & citizen == 5),
    immNONCIT_universe = if_else(age >= 18 & immigrantNONCIT == 1, 1, NA_real_),
    immigrant    = as.integer(nativity == 5 & !(citizen %in% c(1, 2, 3))),
    imm_universe = if_else(age >= 18 & immigrant == 1, 1, NA_real_),
    immigrantCIT    = as.integer(nativity == 5 & citizen == 4),
    immCIT_universe = if_else(age >= 18 & immigrantCIT == 1, 1, NA_real_),
    cobgrp = classify_cobgrp(bpld),
    nat_education1 = if_else(natives_universe == 1 & educ99 %in% low_ed, 1, NA_real_),
    nat_education2 = if_else(natives_universe == 1 & educ99 == 10, 1, NA_real_),
    nat_education3 = if_else(natives_universe == 1 & educ99 == 11, 1, NA_real_),
    nat_education4 = if_else(natives_universe == 1 & educ99 %in% high_ed, 1, NA_real_),
    nat_education5 = if_else(natives_universe == 1 & educ99 %in% top_ed, 1, NA_real_),
    noncit_education1 = if_else(immNONCIT_universe == 1 & educ99 %in% low_ed, 1, NA_real_),
    noncit_education2 = if_else(immNONCIT_universe == 1 & educ99 == 10, 1, NA_real_),
    noncit_education3 = if_else(immNONCIT_universe == 1 & educ99 == 11, 1, NA_real_),
    noncit_education4 = if_else(immNONCIT_universe == 1 & educ99 %in% high_ed, 1, NA_real_),
    noncit_education5 = if_else(immNONCIT_universe == 1 & educ99 %in% top_ed, 1, NA_real_),
    imm_education1 = if_else(imm_universe == 1 & educ99 %in% low_ed, 1, NA_real_),
    imm_education2 = if_else(imm_universe == 1 & educ99 == 10, 1, NA_real_),
    imm_education3 = if_else(imm_universe == 1 & educ99 == 11, 1, NA_real_),
    imm_education4 = if_else(imm_universe == 1 & educ99 %in% high_ed, 1, NA_real_),
    imm_education5 = if_else(imm_universe == 1 & educ99 %in% top_ed, 1, NA_real_)
  )

cps_immi <- cps_indiv %>%
  weighted_collapse(
    group_vars = c("year", "cobgrp"), weight = "wtsupp",
    sum_prefixes = c("natives_universe", "immNONCIT_universe", "imm_universe", "immCIT_universe",
                      "nat_education", "imm_education", "noncit_education")
  ) %>%
  mutate(
    imm_unskill      = imm_education1,
    imm_skill         = imm_education2 + imm_education3 + imm_education4 + imm_education5,
    immNCIT_unskill = noncit_education1,
    immNCIT_skill    = noncit_education2 + noncit_education3 + noncit_education4 + noncit_education5
  ) %>%
  group_by(year) %>%
  mutate(natives_universe = sum(nat_education1 + nat_education2 + nat_education3 + nat_education4 + nat_education5, na.rm = TRUE)) %>%
  ungroup() %>%
  save_step("CPS_immi")

## National natives total, by year (used directly -- see Part A's `hat_natives_universe` note).
cps_national_natives <- cps_immi %>% distinct(year, natives_universe)

## ---- Broadcast each county's 1980 initial country-of-birth share across all CPS years ----
cps_base <- cps_immi %>%
  filter(cobgrp != 0) %>%
  inner_join(pop_cty_1980 %>% select(cobgrp, fips_tc, sh_c_i_80, sh_c_us_80), by = "cobgrp", relationship = "many-to-many") %>%
  filter(!is.na(sh_c_us_80)) %>%
  mutate(
    mex  = as.integer(cobgrp == 3),
    lat   = as.integer(cobgrp %in% c(2, 3)),
    rich = as.integer(cobgrp %in% c(1, 4, 7, 3, 14))  # includes Mexico (cobgrp==3) -- see file header note
  )

cps_county_year <- cps_base %>%
  group_by(year, fips_tc) %>%
  summarise(
    hat_imm_education_low   = sum(sh_c_i_80 * imm_unskill, na.rm = TRUE),
    hat_imm_education_high  = sum(sh_c_i_80 * imm_skill, na.rm = TRUE),
    hat_immNCIT_edu_low     = sum(sh_c_i_80 * immNCIT_unskill, na.rm = TRUE),
    hat_immNCIT_edu_high    = sum(sh_c_i_80 * immNCIT_skill, na.rm = TRUE),
    sh_c_us_80 = first(sh_c_us_80),
    .groups = "drop"
  ) %>%
  left_join(cps_national_natives, by = "year") %>%
  mutate(hat_natives_universe = sh_c_us_80 * natives_universe) %>%
  select(-natives_universe)

cps_county_year <- cps_county_year %>%
  add_split(cps_base, "mex", "imm_unskill", "hat_imm_education_low_mex", "hat_imm_education_low_nmex") %>%
  add_split(cps_base, "mex", "imm_skill", "hat_imm_education_high_mex", "hat_imm_education_high_nmex") %>%
  add_split(cps_base, "lat", "imm_unskill", "hat_imm_education_low_lat", "hat_imm_education_low_nlat") %>%
  add_split(cps_base, "lat", "imm_skill", "hat_imm_education_high_lat", "hat_imm_education_high_nlat") %>%
  add_split(cps_base, "rich", "imm_unskill", "hat_imm_education_low_rich", "hat_imm_education_low_nrich") %>%
  add_split(cps_base, "rich", "imm_skill", "hat_imm_education_high_rich", "hat_imm_education_high_nrich")

# hat_imm_education_low_<i>/high_<i>: identical to the overall hat_ for every i -- see the
# "bug-for-bug note" above.
for (i in 1:15) {
  cps_county_year[[paste0("hat_imm_education_low_", i)]]  <- cps_county_year$hat_imm_education_low
  cps_county_year[[paste0("hat_imm_education_high_", i)]] <- cps_county_year$hat_imm_education_high
}

instruments_cps <- cps_county_year %>% arrange(fips_tc, year)

## =============================================================================
## Combine both vintages, interpolate 1992, and build the final instrument shares
## =============================================================================

## ---- Append, then fill the 1980-1990-to-1994 gap year (1992) as the average of its
## immediate neighbors in the resulting (sparse) year sequence -- source lines 1244-1301.
## Also requires a strongly-balanced fips_tc x year panel (`tsfill, full`) and drops any county
## that doesn't have all vintages present (source lines 1257-1267: `drop if m<28`, i.e. keep
## only counties observed in every year of the final panel: originally 1980, 1990, 1992,
## 1994-2018 (28 years); now extended through 2024 via the CPS pull in cps_raw_ext above. The
## `length(all_years) - 1` below is written generically off `all_years` rather than a literal 28,
## so it already accounts for the added years.
combined <- bind_rows(instruments_1980_1990, instruments_cps)

all_years <- sort(unique(c(combined$year, 1992)))
all_counties <- sort(unique(combined$fips_tc))

balanced <- expand_grid(fips_tc = all_counties, year = all_years) %>%
  left_join(combined, by = c("fips_tc", "year")) %>%
  arrange(fips_tc, year)

counties_with_full_history <- balanced %>%
  group_by(fips_tc) %>%
  summarise(n_present = sum(!is.na(hat_natives_universe)), .groups = "drop") %>%
  filter(n_present >= length(all_years) - 1)  # every year except the blank 1992 placeholder

hat_cols <- names(combined)[startsWith(names(combined), "hat_")]

balanced <- balanced %>%
  semi_join(counties_with_full_history, by = "fips_tc") %>%
  group_by(fips_tc) %>%
  mutate(across(all_of(hat_cols), ~ if_else(year == 1992, (lag(.x) + lead(.x)) / 2, .x))) %>%
  ungroup()

## ---- Final instrument shares (source lines 1304-1330) ----
election_immi_CPS_IV <- balanced %>%
  filter(!is.na(hat_natives_universe)) %>%
  mutate(denom = hat_imm_education_low + hat_imm_education_high + hat_natives_universe) %>%
  transmute(
    year, cty_fips = fips_tc,
    imm_share_IV      = (hat_imm_education_low + hat_imm_education_high) / denom,
    imm_share_low_IV  = hat_imm_education_low / denom,
    imm_share_high_IV = hat_imm_education_high / denom,
    immNONCIT_share_low_IV  = hat_immNCIT_edu_low / denom,
    immNONCIT_share_high_IV = hat_immNCIT_edu_high / denom,
    imm_share_low_mex_IV  = hat_imm_education_low_mex / denom,
    imm_share_high_mex_IV = hat_imm_education_high_mex / denom,
    imm_share_low_nmex_IV  = hat_imm_education_low_nmex / denom,
    imm_share_high_nmex_IV = hat_imm_education_high_nmex / denom,
    imm_share_low_lat_IV  = hat_imm_education_low_lat / denom,
    imm_share_high_lat_IV = hat_imm_education_high_lat / denom,
    imm_share_low_nlat_IV  = hat_imm_education_low_nlat / denom,
    imm_share_high_nlat_IV = hat_imm_education_high_nlat / denom,
    imm_share_low_rich_IV  = hat_imm_education_low_rich / denom,
    imm_share_high_rich_IV = hat_imm_education_high_rich / denom,
    imm_share_low_nrich_IV  = hat_imm_education_low_nrich / denom,
    imm_share_high_nrich_IV = hat_imm_education_high_nrich / denom,
    !!!setNames(lapply(1:15, function(i) rlang::expr((!!rlang::sym(paste0("hat_imm_education_low_", i))) / denom)),
                paste0("imm_share_low_IV_", 1:15)),
    !!!setNames(lapply(1:15, function(i) rlang::expr((!!rlang::sym(paste0("hat_imm_education_high_", i))) / denom)),
                paste0("imm_share_high_IV_", 1:15))
  ) %>%
  arrange(cty_fips, year) %>%
  save_step("election_immi_CPS_IV")
