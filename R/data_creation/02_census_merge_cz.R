## Port of Files/Dofiles/Data_creation/2nd_Census_merge_CZ.do
##
## Combines the IPUMS-derived Census/ACS extracts (Data/Input/IPUMS/*.dta) with the
## PUMA/county -> commuting-zone crosswalks to build a commuting-zone-by-year panel of
## immigrant and native population characteristics. Output: Census_cz_final.rds.
##
## Faithfulness notes (deviations would silently change every downstream table, so these
## are preserved even where they look like they could be simplified):
##  - Geography keys are built by zero-padding state/PUMA codes and concatenating, exactly
##    matching the crosswalk files' own key construction (verified against the raw crosswalk
##    values, e.g. state=1/cntygp98=1 -> ctygrp1980=1001, i.e. state is NOT zero-padded when
##    combined with a 3-digit county group, but IS zero-padded to 2 digits for the 2010-PUMA
##    crosswalk, which uses two separate string columns instead of one concatenated key).
##  - Some crosswalk joins keep only matched rows (Stata `keep if _merge==3`, i.e. inner_join)
##    and some do not (Stata leaves the unmatched rows in, i.e. full_join) -- this is
##    inconsistent in the original code but replicated as-is.
##  - The set of variables swept up by each `collapse (sum) prefix*` differs slightly across
##    year-blocks (e.g. the 1980 block omits tot_income/voters_employed even though the 1990+
##    blocks include them) because the source variables simply don't exist for 1980. Preserved
##    via explicit per-block prefix lists rather than a single shared list.

source(file.path("R", "00_setup.R"))
source(file.path("R", "utils.R"))

SUM_TERMS_1980 <- c(
  "natives_universe", "imm", "noncit_education", "cit_education", "nat_education",
  "nat_white", "nat_urban", "voters_universe", "voters_education", "voters_aframerican",
  "voters_hispanic", "voters_urban", "voters_unemployed", "voters_males", "voters_married",
  "voters_white"
)
SUM_TERMS_STD <- c(
  "natives_universe", "imm", "noncit_education", "cit_education", "tot_income",
  "nat_education", "nat_white", "nat_urban", "voters_universe", "voters_education",
  "voters_aframerican", "voters_hispanic", "voters_urban", "voters_unemployed",
  "voters_employed", "voters_males", "voters_married", "voters_white"
)
BPL_SUM_TERMS <- c("immNONCIT_universe", "imm_universe", "immCIT_universe", "cit_education", "noncit_education", "imm_rich")

## ---------------------------------------------------------------------------
## BPL_cty_1980_final: immigrants by country of birth, 1980, at the CZ level
## ---------------------------------------------------------------------------
bpl_1980 <- read_stata(file.path(IPUMS_DIR, "BPL_cty_1980.dta")) %>%
  mutate(ctygrp1980 = statefip * 1000L + cntygp98)

cw_ctygrp1980 <- read_stata(file.path(INPUT_DIR, "cw_ctygrp1980_czone.dta"))

bpl_cty_1980_final <- bpl_1980 %>%
  inner_join(cw_ctygrp1980, by = "ctygrp1980", relationship = "many-to-many") %>%
  weighted_collapse(group_vars = c("year", "czone", "bpld"), weight = "afactor", sum_prefixes = BPL_SUM_TERMS) %>%
  save_step("BPL_cty_1980_final")

## ---------------------------------------------------------------------------
## BPL_cty_1990_final
## ---------------------------------------------------------------------------
bpl_1990 <- read_stata(file.path(IPUMS_DIR, "BPL_cty_1990.dta")) %>%
  mutate(puma1990 = statefip * 10000L + puma)

cw_puma1990 <- read_stata(file.path(INPUT_DIR, "cw_puma1990_czone.dta"))

bpl_cty_1990_final <- bpl_1990 %>%
  inner_join(cw_puma1990, by = "puma1990", relationship = "many-to-many") %>%
  weighted_collapse(group_vars = c("year", "czone", "bpld"), weight = "afactor", sum_prefixes = BPL_SUM_TERMS) %>%
  save_step("BPL_cty_1990_final")

## ---------------------------------------------------------------------------
## 2010-vintage PUMA -> 2000-vintage PUMA crosswalk (used for both BPL and Census below)
## ---------------------------------------------------------------------------
corr <- read_excel(file.path(INPUT_DIR, "PUMA2000_PUMA2010_crosswalk.xls"), sheet = "PUMA2000_PUMA2010") %>%
  select(State00, PUMA00, State10, PUMA10, pPUMA10_Pop00) %>%
  mutate(weight = pPUMA10_Pop00 / 100)

cw_puma2000 <- read_stata(file.path(INPUT_DIR, "cw_puma2000_czone.dta"))

## ---------------------------------------------------------------------------
## BPL_cty_2010_1: the ACS 2009-2013 pooled file's 2010-PUMA-vintage rows (multyear>2011),
## re-expressed on 2000-vintage PUMAs so they can later join the 2000-PUMA czone crosswalk.
## ---------------------------------------------------------------------------
bpl_2010_raw <- read_stata(file.path(IPUMS_DIR, "BPL_cty_2010.dta")) %>%
  mutate(year = 2010)

bpl_cty_2010_1 <- bpl_2010_raw %>%
  filter(multyear > 2011) %>%
  mutate(State10 = pad(statefip, 2), PUMA10 = pad(puma, 5)) %>%
  inner_join(corr, by = c("State10", "PUMA10"), relationship = "many-to-many") %>%
  weighted_collapse(group_vars = c("year", "State00", "PUMA00", "bpld"), weight = "weight", sum_prefixes = BPL_SUM_TERMS) %>%
  mutate(statefip = as.integer(State00), puma = as.integer(PUMA00)) %>%
  select(-State00, -PUMA00) %>%
  save_step("BPL_cty_2010_1")

## ---------------------------------------------------------------------------
## BPL_cty_2000_2010: 2000 5% sample + both 2010 pieces, all on 2000 PUMAs, joined to CZ.
## Note: unlike the two blocks above, the source do-file does NOT filter to matched rows on
## the puma2000 -> czone join here (replicated as full_join).
## ---------------------------------------------------------------------------
bpl_2000 <- read_stata(file.path(IPUMS_DIR, "BPL_cty_2000.dta"))

bpl_cty_2000_2010 <- bpl_2010_raw %>%
  filter(multyear < 2012) %>%
  bind_rows(bpl_2000) %>%
  bind_rows(bpl_cty_2010_1) %>%
  mutate(puma2000 = statefip * 10000L + puma) %>%
  full_join(cw_puma2000, by = "puma2000", relationship = "many-to-many") %>%
  weighted_collapse(group_vars = c("year", "czone", "bpld"), weight = "afactor", sum_prefixes = BPL_SUM_TERMS) %>%
  save_step("BPL_cty_2000_2010")

## ---------------------------------------------------------------------------
## BPL_cty_2011_2016: biennial ACS 2012/2014/2016 (2010-PUMA vintage) -> 2000 PUMAs
## ---------------------------------------------------------------------------
bpl_2002_2016 <- read_stata(file.path(IPUMS_DIR, "BPL_cty_2002_2016.dta"))

bpl_cty_2011_2016 <- bpl_2002_2016 %>%
  filter(year > 2011) %>%
  mutate(State10 = pad(statefip, 2), PUMA10 = pad(puma, 5)) %>%
  inner_join(corr, by = c("State10", "PUMA10"), relationship = "many-to-many") %>%
  weighted_collapse(group_vars = c("year", "State00", "PUMA00", "bpld"), weight = "weight", sum_prefixes = BPL_SUM_TERMS) %>%
  mutate(statefip = as.integer(State00), puma = as.integer(PUMA00)) %>%
  select(-State00, -PUMA00) %>%
  save_step("BPL_cty_2011_2016")

## ---------------------------------------------------------------------------
## BPL_cty_2006_2016: biennial ACS 2006/2008/2010 (already 2000-PUMA vintage) + the
## 2011_2016 block above, joined to CZ. Again NOT filtered to matched rows (full_join),
## matching the source. Unlike the Census version of this step, there is no
## `drop if year==2010` here -- the source do-file leaves BPL_cty_2000_2010 and
## BPL_cty_2006_2016 both carrying a 2010 vintage (5-year ACS vs. single-year ACS);
## whichever one is used downstream is decided in 3rd_Composition_immigrants_income.do.
## ---------------------------------------------------------------------------
bpl_cty_2006_2016 <- bpl_2002_2016 %>%
  filter(year < 2012 & year > 2004) %>%
  bind_rows(bpl_cty_2011_2016) %>%
  mutate(puma2000 = statefip * 10000L + puma) %>%
  full_join(cw_puma2000, by = "puma2000", relationship = "many-to-many") %>%
  weighted_collapse(group_vars = c("year", "czone", "bpld"), weight = "afactor", sum_prefixes = BPL_SUM_TERMS) %>%
  save_step("BPL_cty_2006_2016")

## ---------------------------------------------------------------------------
## BPL_cty_2002_2004: 2002/2004 ACS has no reliable PUMA-level geography, so this stays at
## the state level and is an *unweighted* sum (no pweight in the source collapse).
## ---------------------------------------------------------------------------
bpl_cty_2002_2004 <- bpl_2002_2016 %>%
  filter(year < 2006) %>%
  group_by(year, statefip, bpld) %>%
  summarise(across(all_of(c("immNONCIT_universe", "imm_universe", "immCIT_universe",
                             paste0("cit_education", 1:5), paste0("noncit_education", 1:5),
                             "imm_rich_universe")), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
  save_step("BPL_cty_2002_2004")

## =============================================================================
## Census/voter side: natives + all-immigrant (not just by-country) characteristics
## =============================================================================

## ---------------------------------------------------------------------------
## Census_cz_1980_final
## ---------------------------------------------------------------------------
census_1980 <- read_stata(file.path(IPUMS_DIR, "Census_cty_1980.dta")) %>%
  full_join(read_stata(file.path(IPUMS_DIR, "voters_cty_1980.dta")), by = c("year", "statefip", "cntygp98")) %>%
  full_join(read_stata(file.path(IPUMS_DIR, "totalsAWEIGHTS_cty_1980.dta")), by = c("year", "statefip", "cntygp98")) %>%
  mutate(ctygrp1980 = statefip * 1000L + cntygp98)

census_cz_1980_final <- census_1980 %>%
  inner_join(cw_ctygrp1980, by = "ctygrp1980", relationship = "many-to-many") %>%
  weighted_collapse(
    group_vars = c("year", "czone"), weight = "afactor",
    mean_vars = c("voters_income", "av_income"), sum_prefixes = SUM_TERMS_1980
  ) %>%
  save_step("Census_cz_1980_final")

# Attach each CZ's total native population onto the by-country-of-birth BPL panel (1980).
bpl_cty_1980_final <- census_cz_1980_final %>%
  select(year, czone, natives_universe) %>%
  full_join(bpl_cty_1980_final, by = c("year", "czone")) %>%
  save_step("BPL_cty_1980_final")

## ---------------------------------------------------------------------------
## Census_cz_1990_final
## ---------------------------------------------------------------------------
census_1990 <- read_stata(file.path(IPUMS_DIR, "Census_cty_1990.dta")) %>%
  full_join(read_stata(file.path(IPUMS_DIR, "voters_cty_1990.dta")), by = c("year", "statefip", "puma")) %>%
  full_join(read_stata(file.path(IPUMS_DIR, "totalsAWEIGHTS_cty_1990.dta")), by = c("year", "statefip", "puma")) %>%
  mutate(puma1990 = statefip * 10000L + puma)

census_cz_1990_final <- census_1990 %>%
  inner_join(cw_puma1990, by = "puma1990", relationship = "many-to-many") %>%
  weighted_collapse(
    group_vars = c("year", "czone"), weight = "afactor",
    mean_vars = c("voters_income", "av_income"), sum_prefixes = SUM_TERMS_STD
  ) %>%
  save_step("Census_cz_1990_final")

## ---------------------------------------------------------------------------
## Census_cz_2000_final: 2000 5% sample, left at PUMA level here (CZ-ified together with
## the 2010 block below, since both are on 2000-vintage PUMAs).
## ---------------------------------------------------------------------------
census_cz_2000_final <- read_stata(file.path(IPUMS_DIR, "Census_cty_2000.dta")) %>%
  full_join(read_stata(file.path(IPUMS_DIR, "voters_cty_2000.dta")), by = c("year", "statefip", "puma")) %>%
  full_join(read_stata(file.path(IPUMS_DIR, "totalsAWEIGHTS_cty_2000.dta")), by = c("year", "statefip", "puma")) %>%
  save_step("Census_cz_2000_final")

## ---------------------------------------------------------------------------
## Census_cz_2010_final: ACS 2009-2013 pooled file, still at PUMA level (multyear, not year)
## ---------------------------------------------------------------------------
census_cz_2010_final <- read_stata(file.path(IPUMS_DIR, "Census_cty_2010.dta")) %>%
  full_join(read_stata(file.path(IPUMS_DIR, "voters_cty_2010.dta")), by = c("multyear", "statefip", "puma")) %>%
  full_join(read_stata(file.path(IPUMS_DIR, "totalsAWEIGHTS_cty_2010.dta")), by = c("multyear", "statefip", "puma")) %>%
  save_step("Census_cz_2010_final")

## Census_cz_2010_final_1: the multyear>2011 (2010-PUMA-vintage) slice, converted to 2000 PUMAs
census_cz_2010_final_1 <- census_cz_2010_final %>%
  mutate(year = 2010) %>%
  filter(multyear > 2011) %>%
  mutate(State10 = pad(statefip, 2), PUMA10 = pad(puma, 5)) %>%
  inner_join(corr, by = c("State10", "PUMA10"), relationship = "many-to-many") %>%
  weighted_collapse(
    group_vars = c("year", "State00", "PUMA00"), weight = "weight",
    mean_vars = c("voters_income", "av_income"), sum_prefixes = SUM_TERMS_STD
  ) %>%
  mutate(statefip = as.integer(State00), puma = as.integer(PUMA00)) %>%
  select(-State00, -PUMA00) %>%
  save_step("Census_cz_2010_final_1")

## ---------------------------------------------------------------------------
## Census_cz_2000_2010_final: 2000 sample + both 2010 pieces, all on 2000 PUMAs -> CZ
## (full_join / unfiltered on the puma2000->czone step, matching the source)
## ---------------------------------------------------------------------------
census_cz_2000_2010_final <- census_cz_2010_final %>%
  mutate(year = 2010) %>%
  filter(multyear < 2012) %>%
  bind_rows(census_cz_2000_final) %>%
  bind_rows(census_cz_2010_final_1) %>%
  mutate(puma2000 = statefip * 10000L + puma) %>%
  full_join(cw_puma2000, by = "puma2000", relationship = "many-to-many") %>%
  weighted_collapse(
    group_vars = c("year", "czone"), weight = "afactor",
    mean_vars = c("voters_income", "av_income"), sum_prefixes = SUM_TERMS_STD
  ) %>%
  save_step("Census_cz_2000_2010_final")

## ---------------------------------------------------------------------------
## Census_cz_2002_2016_final: biennial ACS pooled file, still at PUMA level
## ---------------------------------------------------------------------------
census_cz_2002_2016_final <- read_stata(file.path(IPUMS_DIR, "Census_cty_2002_2016.dta")) %>%
  full_join(read_stata(file.path(IPUMS_DIR, "voters_cty_2002_2016.dta")), by = c("year", "statefip", "puma")) %>%
  full_join(read_stata(file.path(IPUMS_DIR, "totalsAWEIGHTS_cty_2002_2016.dta")), by = c("year", "statefip", "puma")) %>%
  save_step("Census_cz_2002_2016_final")

## Census_cz_2012_2016_final: 2012/2014/2016 (2010-PUMA vintage) -> 2000 PUMAs
census_cz_2012_2016_final <- census_cz_2002_2016_final %>%
  filter(year > 2011) %>%
  mutate(State10 = pad(statefip, 2), PUMA10 = pad(puma, 5)) %>%
  inner_join(corr, by = c("State10", "PUMA10"), relationship = "many-to-many") %>%
  weighted_collapse(
    group_vars = c("year", "State00", "PUMA00"), weight = "weight",
    mean_vars = c("voters_income", "av_income"), sum_prefixes = SUM_TERMS_STD
  ) %>%
  mutate(statefip = as.integer(State00), puma = as.integer(PUMA00)) %>%
  select(-State00, -PUMA00) %>%
  save_step("Census_cz_2012_2016_final")

## Census_cz_2006_2016_final: 2006/2008/2010 (2000-PUMA vintage) + 2012-2016 block -> CZ.
## The 5-year-ACS 2010 vintage (Census_cz_2000_2010_final) is the one kept for year 2010,
## so the single-year-ACS 2010 vintage is dropped here to avoid a duplicate.
census_cz_2006_2016_final <- census_cz_2002_2016_final %>%
  filter(year < 2012 & year > 2004) %>%
  bind_rows(census_cz_2012_2016_final) %>%
  mutate(puma2000 = statefip * 10000L + puma) %>%
  full_join(cw_puma2000, by = "puma2000", relationship = "many-to-many") %>%
  weighted_collapse(
    group_vars = c("year", "czone"), weight = "afactor",
    mean_vars = c("voters_income", "av_income"), sum_prefixes = SUM_TERMS_STD
  ) %>%
  filter(year != 2010) %>%
  save_step("Census_cz_2006_2016_final")

## =============================================================================
## Stack every vintage into one CZ-by-year panel and build the derived share variables
## =============================================================================
census_cz_all <- bind_rows(
  census_cz_2000_2010_final,
  census_cz_2006_2016_final,
  census_cz_1980_final,
  census_cz_1990_final
) %>%
  mutate(aweight = natives_universe_w + imm_universe_w) %>%
  select(-natives_universe_w, -imm_universe_w) %>%
  mutate(
    nat_education1_share = nat_education1 / natives_universe,
    nat_education2_share = nat_education2 / natives_universe,
    nat_education3_share = nat_education3 / natives_universe,
    nat_education4_share = nat_education4 / natives_universe,
    nat_education5_share = nat_education5 / natives_universe,
    nat_white_share       = nat_white / natives_universe,
    nat_urban_share        = nat_urban / natives_universe,

    # Immigrants from rich countries (Canada, Western Europe, Japan, Australia) and white immigrants
    czone_imm_rich_share      = imm_rich_universe / imm_universe,
    czone_imm_rich_pop_share  = imm_rich_universe / (imm_universe + natives_universe),
    czone_imm_white_share     = imm_white_universe / imm_universe,
    czone_imm_white_pop_share = imm_white_universe / (imm_universe + natives_universe),

    noncit_education1_share = noncit_education1 / immNONCIT_universe,
    noncit_education2_share = noncit_education2 / immNONCIT_universe,
    noncit_education3_share = noncit_education3 / immNONCIT_universe,
    noncit_education4_share = noncit_education4 / immNONCIT_universe,
    noncit_education5_share = noncit_education5 / immNONCIT_universe,

    cit_education1_share = cit_education1 / immCIT_universe,
    cit_education2_share = cit_education2 / immCIT_universe,
    cit_education3_share = cit_education3 / immCIT_universe,
    cit_education4_share = cit_education4 / immCIT_universe,
    cit_education5_share = cit_education5 / immCIT_universe,

    white_education1_share = voters_white_education1 / (immCIT_universe + immNONCIT_universe),
    white_education2_share = voters_white_education2 / (immCIT_universe + immNONCIT_universe),
    white_education3_share = voters_white_education3 / (immCIT_universe + immNONCIT_universe),
    white_education4_share = voters_white_education4 / (immCIT_universe + immNONCIT_universe),
    white_education5_share = voters_white_education5 / (immCIT_universe + immNONCIT_universe),

    voters_education1_share = voters_education1 / voters_universe,
    voters_education2_share = voters_education2 / voters_universe,
    voters_education3_share = voters_education3 / voters_universe,
    voters_education4_share = voters_education4 / voters_universe,
    voters_education5_share = voters_education5 / voters_universe,
    voters_aframerican_share = voters_aframerican / voters_universe,
    voters_hispanic_share    = voters_hispanic / voters_universe,
    voters_urban_share       = voters_urban / voters_universe,
    voters_unemployed_share  = voters_unemployed / voters_universe,
    voters_employed_share    = voters_employed / voters_universe,
    voters_males_share       = voters_males / voters_universe,
    voters_married_share     = voters_married / voters_universe,
    voters_white_share       = voters_white / voters_universe,
    voters_white_low_skill      = voters_white_education1 / voters_universe,
    voters_white_male_low_skill = voters_white_male_edu1 / voters_universe
  )

## National (all-CZ) native population total per year, and its change vs. the previous
## observed census/ACS year -- broadcast onto every CZ for that year.
census_cz_with_national <- census_cz_all %>%
  group_by(year) %>%
  mutate(nat_natives_universes = sum(natives_universe, na.rm = TRUE)) %>%
  ungroup() %>%
  save_step("test")

natives_cz_final <- census_cz_with_national %>%
  arrange(czone, year) %>%
  group_by(czone) %>%
  mutate(d_nat_natives_universe = nat_natives_universes - lag(nat_natives_universes)) %>%
  ungroup() %>%
  select(year, czone, natives_universe, d_nat_natives_universe, nat_natives_universes) %>%
  save_step("Natives_cz_final")

## Final CZ-level panel used by the downstream data-creation and regression files.
census_cz_final <- census_cz_with_national %>%
  mutate(
    czone_nat_skill_low_share  = nat_education1_share,
    czone_nat_skill_high_share = nat_education2_share + nat_education3_share + nat_education4_share + nat_education5_share,

    czone_noncit_education1_share = noncit_education1_share,
    czone_noncit_education2_share = noncit_education2_share,
    czone_noncit_education3_share = noncit_education3_share,
    czone_noncit_education4_share = noncit_education4_share,
    czone_noncit_education5_share = noncit_education5_share,

    czone_cit_education1_share = cit_education1_share,
    czone_cit_education2_share = cit_education2_share,
    czone_cit_education3_share = cit_education3_share,
    czone_cit_education4_share = cit_education4_share,
    czone_cit_education5_share = cit_education5_share,

    czone_white_education1_share = white_education1_share,
    czone_white_education2_share = white_education2_share,
    czone_white_education3_share = white_education3_share,
    czone_white_education4_share = white_education4_share,
    czone_white_education5_share = white_education5_share,

    czone_noncit_skill_low_share  = czone_noncit_education1_share,
    czone_noncit_skill_high_share = czone_noncit_education2_share + czone_noncit_education3_share + czone_noncit_education4_share + czone_noncit_education5_share,
    czone_cit_skill_low_share     = czone_cit_education1_share,
    czone_cit_skill_high_share    = czone_cit_education2_share + czone_cit_education3_share + czone_cit_education4_share + czone_cit_education5_share,
    czone_white_skill_low_share   = czone_white_education1_share,
    czone_white_skill_high_share  = czone_white_education2_share + czone_white_education3_share + czone_white_education4_share + czone_white_education5_share,

    czone_voters_education1_share = voters_education1_share,
    czone_voters_education2_share = voters_education2_share,
    czone_voters_education3_share = voters_education3_share,
    czone_voters_education4_share = voters_education4_share,
    czone_voters_education5_share = voters_education5_share,
    czone_voters_aframerican_share = voters_aframerican_share,
    czone_voters_hispanic_share    = voters_hispanic_share,
    czone_voters_urban_share       = voters_urban_share,
    czone_voters_unemployed_share  = voters_unemployed_share,
    czone_voters_employed_share    = voters_employed_share,
    czone_voters_males_share       = voters_males_share,
    czone_voters_married_share     = voters_married_share,
    czone_voters_income_share      = voters_income,
    czone_voters_white_share       = voters_white_share,
    czone_voters_white_low_sk      = voters_white_low_skill,
    czone_voters_white_low_mal_sk  = voters_white_male_low_skill,

    czone_voters_employed = voters_employed,
    czone_av_income = av_income,
    czone_tot_income = tot_income,

    czone_imm_universe      = imm_universe,
    czone_immCIT_universe   = immNONCIT_universe,  # names swapped to match the source do-file exactly (see 2nd_Census_merge_CZ.do lines 579-580)
    czone_immNONCIT_universe = immCIT_universe,
    czone_natives_universe  = natives_universe
  ) %>%
  select(
    czone, year, starts_with("czone_"),
    starts_with("voters_education"),
    starts_with("nat_education"),
    starts_with("nat_"),
    starts_with("czone_voters_white_")
  ) %>%
  arrange(czone, year) %>%
  save_step("Census_cz_final")
