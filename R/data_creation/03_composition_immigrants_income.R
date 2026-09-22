## Port of Files/Dofiles/Data_creation/3rd_Composition_immigrants_income.do
##
## Builds three CZ-by-year splits of the immigrant stock by country-of-birth group:
##   - Czone_rich_poor: immigrants from rich (OECD-ish) vs. other countries
##   - Czone_mex:       Mexican vs. non-Mexican immigrants
##   - Czone_lat:        Latino vs. non-Latino immigrants
## plus a county-level 1980 initial-distribution file (1980_pop_cty / 1980-2010_pop_cty)
## used later as a shift-share instrument base.
##
## Faithfulness notes:
##  - The source do-file writes `bpl` (not `bpld`) in the cobgrp==5 condition. No `bpl`
##    variable exists anywhere in this pipeline; Stata silently resolves it via variable-name
##    abbreviation to the only var starting with "bpl", i.e. `bpld`. Replicated as bpld.
##  - cobgrp is built with sequential Stata `replace` statements, so later conditions
##    overwrite earlier ones where ranges overlap (e.g. bpld==45300 is caught by both the
##    Western Europe rule (cobgrp=4) and the later Eastern Europe range rule (cobgrp=5) --
##    the Eastern Europe rule wins because it runs later). classify_cobgrp() replicates this
##    with sequential `[<-` overwrites, not case_when(), to preserve last-write-wins order.
##  - `gen rich=1 if ... | (bpld<21000 & bpld>=20000) | ...` includes the Mexico bpld range in
##    the "rich" country dummy. This does not match the canonical rich-country definition used
##    to build imm_rich_universe (Canada/W.Europe/Japan/Oceania, no Mexico) a few files earlier,
##    and looks like a copy-paste artifact from the adjacent cobgrp==3 (Mexico) line. It is
##    reproduced exactly as written for fidelity to the published results; imm_rich_universe
##    itself (used for r_imm_rich) is unaffected since it comes from the ACS extract step, not
##    from this `rich` dummy.

source(file.path("R", "00_setup.R"))
source(file.path("R", "utils.R"))

classify_cobgrp <- function(bpld) {
  cobgrp <- rep(NA_real_, length(bpld))
  cobgrp[bpld < 15000 | bpld %in% c(90011, 90021)] <- 0                                    # United States
  cobgrp[bpld >= 15000 & bpld < 16000] <- 1                                                # Canada
  cobgrp[bpld >= 16000 & bpld < 20000] <- 2                                                # Other Americas
  cobgrp[bpld >= 21000 & bpld < 31000] <- 2                                                # Other Americas
  cobgrp[bpld >= 20000 & bpld < 21000] <- 3                                                # Mexico
  cobgrp[(bpld >= 40000 & bpld <= 42900) | (bpld >= 43100 & bpld <= 44000) |
           bpld == 45000 | bpld == 45300] <- 4                                             # Western Europe
  cobgrp[bpld == 43000 | (bpld >= 45100 & bpld <= 46500) | bpld == 45303 | bpld == 45340] <- 5  # Eastern Europe
  cobgrp[bpld >= 50000 & bpld < 50100] <- 6                                                # China
  cobgrp[bpld == 50100] <- 7                                                               # Japan
  cobgrp[bpld == 50200] <- 8                                                               # Korea
  cobgrp[bpld == 51500] <- 9                                                               # Philippines
  cobgrp[bpld == 51800] <- 10                                                              # Vietnam
  cobgrp[bpld >= 52100 & bpld < 52200] <- 11                                               # India
  cobgrp[(bpld >= 50900 & bpld <= 51400) | (bpld >= 51600 & bpld <= 51700) |
           (bpld >= 51900 & bpld <= 52000) | (bpld >= 52200 & bpld <= 59900)] <- 12         # Other Asia
  cobgrp[bpld >= 60000 & bpld < 70000] <- 13                                               # Africa
  cobgrp[bpld >= 70000 & bpld < 80000] <- 14                                               # Oceania
  cobgrp[bpld == 49900 | (bpld > 80000 & bpld < 95000)] <- 15                              # Other
  cobgrp[is.na(cobgrp)] <- 15                                                              # catch-all
  cobgrp
}

## ---------------------------------------------------------------------------
## County-level 1980 initial distribution (for the shift-share instrument)
## ---------------------------------------------------------------------------
pop_cty <- read_stata(file.path(INPUT_DIR, "1980-2010_vote+pop_cty.dta")) %>%
  rename(year = period) %>%
  mutate(natives_universe = totpop - fb) %>%
  group_by(year) %>%
  mutate(tot_natives_universe = sum(natives_universe, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(sh_c_us_80 = if_else(year == 1980, natives_universe / tot_natives_universe, NA_real_)) %>%
  pivot_longer(cols = starts_with("fb_cob"), names_to = "cobgrp", names_prefix = "fb_cob",
               names_transform = list(cobgrp = as.integer), values_to = "fb_cob") %>%
  group_by(year, cobgrp) %>%
  mutate(tot_c_fb_cob = sum(fb_cob, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(sh_c_i_80 = if_else(year == 1980, fb_cob / tot_c_fb_cob, NA_real_))

pop_cty %>%
  filter(year == 1980) %>%
  select(cobgrp, year, fips_tc, sh_c_i_80, sh_c_us_80) %>%
  save_step("1980_pop_cty")

pop_cty %>%
  select(-sh_c_i_80, -sh_c_us_80) %>%
  arrange(fips_tc, year, cobgrp) %>%
  save_step("1980-2010_pop_cty")

## ---------------------------------------------------------------------------
## Stack the by-country-of-birth CZ panel (shared base for all three splits below)
## ---------------------------------------------------------------------------
natives_cz_final <- readRDS(file.path(OUTPUT_DIR, "Natives_cz_final.rds"))

bpl_base <- bind_rows(
  readRDS(file.path(OUTPUT_DIR, "BPL_cty_1980_final.rds")),
  readRDS(file.path(OUTPUT_DIR, "BPL_cty_1990_final.rds")),
  readRDS(file.path(OUTPUT_DIR, "BPL_cty_2006_2016.rds")) %>% filter(year != 2010),
  readRDS(file.path(OUTPUT_DIR, "BPL_cty_2000_2010.rds"))
) %>%
  full_join(natives_cz_final, by = c("year", "czone"), relationship = "many-to-many") %>%
  rename(t_natives_universe = nat_natives_universes) %>%
  mutate(cobgrp = classify_cobgrp(bpld)) %>%
  filter(cobgrp != 0) %>%
  mutate(
    imm_unskill_cit    = cit_education1,
    imm_unskill_noncit = noncit_education1,
    imm_skill_cit    = cit_education2 + cit_education3 + cit_education4 + cit_education5,
    imm_skill_noncit = noncit_education2 + noncit_education3 + noncit_education4 + noncit_education5
  )

## ---------------------------------------------------------------------------
## Rich vs. poor country of origin
## ---------------------------------------------------------------------------
rich_split <- bpl_base %>%
  mutate(rich = as.integer(
    bpld == 15000 |
      (bpld >= 40000 & bpld <= 42900) | (bpld >= 43100 & bpld <= 44000) |
      bpld == 45000 | bpld == 45300 | bpld == 50100 |
      (bpld < 21000 & bpld >= 20000) |  # Mexico range -- see file header note
      (bpld < 71000 & bpld >= 70000)
  )) %>%
  group_by(year, rich, czone) %>%
  summarise(
    r_imm_unskill = sum(imm_unskill_cit + imm_unskill_noncit, na.rm = TRUE),
    r_imm_skill    = sum(imm_skill_cit + imm_skill_noncit, na.rm = TRUE),
    r_imm_rich    = sum(imm_rich_universe, na.rm = TRUE),
    r_imm          = sum(imm_universe, na.rm = TRUE),
    .groups = "drop"
  )

czone_rich_poor <- full_join(
  rich_split %>% filter(rich == 1) %>% select(year, czone, r_imm_unskill, r_imm_skill, r_imm_rich, r_imm),
  rich_split %>% filter(rich == 0) %>%
    select(year, czone, rp_imm_unskill = r_imm_unskill, rp_imm_skill = r_imm_skill,
           rp_imm_rich = r_imm_rich, rp_imm = r_imm),
  by = c("year", "czone")
) %>%
  filter(!is.na(year)) %>%
  arrange(czone, year) %>%
  save_step("Czone_rich_poor")

## ---------------------------------------------------------------------------
## Mexican vs. non-Mexican
## ---------------------------------------------------------------------------
mex_split <- bpl_base %>%
  mutate(mex = as.integer(bpld < 21000 & bpld >= 20000)) %>%
  group_by(year, mex, czone) %>%
  summarise(
    r_imm_unskill = sum(imm_unskill_cit + imm_unskill_noncit, na.rm = TRUE),
    r_imm_skill    = sum(imm_skill_cit + imm_skill_noncit, na.rm = TRUE),
    r_imm          = sum(imm_universe, na.rm = TRUE),
    .groups = "drop"
  )

czone_mex_unbalanced <- full_join(
  mex_split %>% filter(mex == 1) %>% select(year, czone, mex_imm_unskill = r_imm_unskill, mex_imm_skill = r_imm_skill, mex_imm = r_imm),
  mex_split %>% filter(mex == 0) %>% select(year, czone, nomex_imm_unskill = r_imm_unskill, nomex_imm_skill = r_imm_skill, nomex_imm = r_imm),
  by = c("year", "czone")
) %>%
  filter(!is.na(year))

# `tsfill, full`: force a strongly-balanced czone x year panel; only the mex_* columns are
# zero-filled for rows that didn't exist before (nomex_* is left NA), matching the source.
czone_mex <- expand_grid(czone = unique(czone_mex_unbalanced$czone), year = unique(czone_mex_unbalanced$year)) %>%
  left_join(czone_mex_unbalanced, by = c("czone", "year")) %>%
  mutate(across(c(mex_imm_unskill, mex_imm_skill, mex_imm), ~ replace_na(.x, 0))) %>%
  arrange(czone, year) %>%
  save_step("Czone_mex")

## ---------------------------------------------------------------------------
## Latino vs. non-Latino  (Mexico + Central America + Caribbean/South America)
## ---------------------------------------------------------------------------
lat_split <- bpl_base %>%
  mutate(lat = as.integer(
    (bpld < 21000 & bpld >= 20000) | (bpld < 20000 & bpld >= 16000) | (bpld < 31000 & bpld >= 21000)
  )) %>%
  group_by(year, lat, czone) %>%
  summarise(
    r_imm_unskill = sum(imm_unskill_cit + imm_unskill_noncit, na.rm = TRUE),
    r_imm_skill    = sum(imm_skill_cit + imm_skill_noncit, na.rm = TRUE),
    r_imm_rich    = sum(imm_rich_universe, na.rm = TRUE),
    r_imm          = sum(imm_universe, na.rm = TRUE),
    .groups = "drop"
  )

czone_lat_unbalanced <- full_join(
  lat_split %>% filter(lat == 1) %>%
    select(year, czone, lat_imm_unskill = r_imm_unskill, lat_imm_skill = r_imm_skill, lat_imm_rich = r_imm_rich, lat_imm = r_imm),
  lat_split %>% filter(lat == 0) %>%
    select(year, czone, nolat_imm_unskill = r_imm_unskill, nolat_imm_skill = r_imm_skill, nolat_imm_rich = r_imm_rich, nolat_imm = r_imm),
  by = c("year", "czone")
) %>%
  filter(!is.na(year))

# Here BOTH lat_* and nolat_* (unskill/skill/imm, but not *_rich) are zero-filled after tsfill.
czone_lat <- expand_grid(czone = unique(czone_lat_unbalanced$czone), year = unique(czone_lat_unbalanced$year)) %>%
  left_join(czone_lat_unbalanced, by = c("czone", "year")) %>%
  mutate(across(c(lat_imm_unskill, lat_imm_skill, lat_imm, nolat_imm_unskill, nolat_imm_skill, nolat_imm), ~ replace_na(.x, 0))) %>%
  arrange(czone, year) %>%
  save_step("Czone_lat")
