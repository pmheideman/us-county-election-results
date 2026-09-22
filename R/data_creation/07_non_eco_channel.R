## Port of Files/Dofiles/Data_creation/7th_Non_eco_channel.do
##
## Builds a commuting-zone-level "linguistic distance from English" variable (difflang_1980),
## used as the non-economic ("cultural assimilation") channel variable: each CZ's 1980
## immigrant stock is broken out by country of origin, each country is assigned a
## language-similarity-to-English index (Adsera & Pytlikova 2015 / CEPII gravity data), and
## the CZ-level value is the population-weighted average across the CZ's countries of origin.
##
## The source do-file also builds Output/concordance_imf.dta (from Concordance_IMF.xlsx) and
## runs two diagnostic regressions of language distance on destination-country GDP per capita
## (source lines 61-67) -- neither feeds into non_eco_channel.dta or any other downstream file,
## so neither is reproduced here.

source(file.path("R", "00_setup.R"))

## ---- Language-similarity-to-English index for USA's migrant-sending countries, 2000 (source lines 15-23) ----
language_country <- read_stata(file.path(INPUT_DIR, "OECD migration data and language EJ2014.dta")) %>%
  filter(tocode == "USA", year == 2000) %>%
  transmute(year, countryisocode = fromcode, Dyen, DyenAll, DyenMajor, index, indexAll, indexMajor)

## ---- Language distance by destination country, keyed off USA-origin gravity pairs (source lines 31-76) ----
immigrant_similarity <- read_stata(file.path(INPUT_DIR, "gravdata_cepii.dta")) %>%
  mutate(iso3_o = if_else(iso3_o == "BLX", "BEL", iso3_o)) %>%
  select(iso3_o, iso3_d, year, contig, comlang_off, comlang_ethno, colony, comcol, curcol, gdp_d, pop_d, gdpcap_d) %>%
  rename(border = contig, ocountryisocode = iso3_o, countryisocode = iso3_d) %>%
  filter(year == 2000) %>%
  inner_join(language_country, by = "countryisocode") %>%
  filter(ocountryisocode == "USA") %>%
  distinct(countryisocode, ocountryisocode, .keep_all = TRUE) %>%
  mutate(
    difflang = 1 - indexAll,
    countryisocode = case_when(
      countryisocode == "DEU" ~ "GER",
      countryisocode == "HKG" ~ "HGK",
      TRUE ~ countryisocode
    )
  ) %>%
  select(countryisocode, difflang) %>%
  save_step("immigrant_similarity")

## ---- bpld -> ISO country code crosswalk, with manual fixes for codes missing from cps_countries.dta ----
cps_countries <- read_stata(file.path(INPUT_DIR, "cps_countries.dta")) %>% distinct(bpld, .keep_all = TRUE)

bpld_iso_overrides <- tribble(
  ~bpld, ~countryisocode,
  30045, "PRY", 45100, "BGR", 45340, "GER", 45310, "GER",
  46541, "AZE", 46530, "UKR", 46540, "ARM", 46542, "GEO",
  46543, "KAZ", 46544, "KGZ", 46545, "TJK", 46546, "TKM",
  46547, "UZB", 46548, "RUS", 53100, "CYP", 60011, "DZA",
  60012, "EGY", 60013, "LBY", 60021, "BFA", 60022, "GMB",
  60023, "GHA", 60024, "GIN", 60025, "GNB", 60026, "CIV",
  60027, "LBR", 60028, "MLI", 60029, "MRT", 60030, "NER",
  60054, "TZA", 60055, "UGA", 60056, "ZMB", 60057, "ZWE",
  60079, "ZAR", 46500, "RUS"   # "Other USSR/Russia" (last, matching source's line-order override)
)

## ---- CZ-level weighted average language distance, 1980 (source lines 86-166) ----
non_eco_channel <- readRDS(file.path(OUTPUT_DIR, "BPL_cty_1980_final.rds")) %>%
  left_join(cps_countries %>% select(bpld, countryisocode), by = "bpld") %>%
  rows_update(bpld_iso_overrides, by = "bpld", unmatched = "ignore") %>%
  filter(countryisocode != "PRK", !is.na(countryisocode), countryisocode != "", countryisocode != "USA") %>%
  filter(!is.na(year)) %>%
  inner_join(immigrant_similarity, by = "countryisocode") %>%
  group_by(czone) %>%
  mutate(weight = imm_universe / sum(imm_universe, na.rm = TRUE)) %>%
  summarise(difflang_1980 = sum(difflang * weight, na.rm = TRUE), .groups = "drop") %>%
  arrange(czone) %>%
  save_step("non_eco_channel")
