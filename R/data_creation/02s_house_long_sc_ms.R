## Candidate-level LONG tables for South Carolina (2008 statewide precinct file + 2012/2014 per-county precinct files; script 01at, shares
## file elect_he_cty_sc.rds) and Mississippi (2006-2014 county files from the OpenElections repo tarball; script 01ai, shares file
## elect_he_cty_ms.rds). Southeast batch. Reuses each original's definitions (crosswalk, downloader/extractor, party mapper, slugs) and its
## exact row filters; adds the candidate names, raw party label and district that the originals dropped.
## party_group: SC party == "REP"/"DEM" exactly (else OTHER); MS party starts with DEM/REP (else OTHER) -- as in the originals.
## Outputs: R/output/long/he_sc.rds, R/output/long/he_ms.rds.

source(file.path("R", "00_setup.R"))
save_step <- function(df, name) invisible(df)
library(readr)
source(file.path("R", "long_helpers.R"))

## ------------------------------------------------------------------ South Carolina
ex <- parse(file = file.path("R", "data_creation", "01at_house_county_south_carolina.R"))
want <- c("RAW_DIR", "county_fips_crosswalk", "sc_fips", "download_oe", "to_party", "SC_COUNTY_SLUGS")
eval(ex[vapply(ex, function(e) is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) && as.character(e[[2]]) %in% want, NA)])

sc08 <- read_csv(download_oe("2008/20081104__sc__general__precinct.csv", "2008_general_precinct.csv"), show_col_types = FALSE, col_types = cols(.default = "c")) %>%
  filter(grepl("U.S. House", office, fixed = TRUE)) %>%
  transmute(county = toupper(trimws(county)), votes = as.numeric(votes), district, candidate, party, party_group = to_party(party), year = 2008) %>%
  filter(!is.na(votes))
sc_county_year <- function(year_chr, date_str) {
  bind_rows(lapply(SC_COUNTY_SLUGS, function(slug) {
    path <- download_oe(sprintf("%s/counties/%s__sc__general__%s__precinct.csv", year_chr, date_str, slug), sprintf("%s_%s.csv", year_chr, slug))
    df <- tryCatch(read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c")), error = function(e) NULL)
    if (is.null(df) || nrow(df) == 0) return(NULL)
    df %>% filter(office == "U.S. House") %>%
      transmute(county = toupper(trimws(county)), votes = as.numeric(votes), district, candidate, party, party_group = to_party(party), year = as.numeric(year_chr)) %>%
      filter(!is.na(votes))
  }))
}
sc_raw <- bind_rows(sc08, sc_county_year("2012", "20121106"), sc_county_year("2014", "20141104"))
## SC prints 3-letter tags; expand for display only (party_group above is computed from the raw tag, unchanged).
## NB the 2008 statewide file has NO district column (source limit, 2008 is the partial 28-of-46-county year): its rows keep district NA.
SC_TAGS <- c(NON = "Nonpartisan", WFM = "Working Families", WFW = "Working Families", CON = "Constitution", LAB = "Labor", GRN = "Green", LIB = "Libertarian")
sc_raw <- sc_raw %>% mutate(party = ifelse(toupper(trimws(party)) %in% names(SC_TAGS), unname(SC_TAGS[toupper(trimws(party))]), party))
sc_long <- sc_raw %>% inner_join(sc_fips, by = c("county" = "county_name")) %>% select(-county) %>% finalize_long("sc")
save_long(sc_long, "he_sc")
res_sc <- check_long_vs_source(sc_long, file.path(OUTPUT_DIR, "elect_he_cty_sc.rds"))

## ------------------------------------------------------------------ Mississippi
ex <- parse(file = file.path("R", "data_creation", "01ai_house_county_mississippi.R"))
want <- c("RAW_DIR", "county_fips_crosswalk", "ms_fips", "tarball", "extract_if_needed", "to_party")
eval(ex[vapply(ex, function(e) is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) && as.character(e[[2]]) %in% want, NA)])
ms_year <- function(year, member) {
  read_csv(extract_if_needed(member), show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    filter(toupper(trimws(office)) == "U.S. HOUSE", toupper(trimws(county)) != "TOTAL") %>%
    transmute(county = toupper(trimws(county)), district, candidate = trimws(candidate), party, party_group = to_party(party), votes = as.numeric(votes), year = year)
}
ms_raw <- bind_rows(ms_year(2006, "2006/20061107__ms__general.csv"), ms_year(2008, "2008/20081104__ms__general.csv"), ms_year(2010, "2010/20101102__ms__general.csv"),
                    ms_year(2012, "2012/20121106__ms__general.csv"), ms_year(2014, "2014/20141104__ms__general.csv")) %>% filter(!is.na(votes))
ms_long <- ms_raw %>% inner_join(ms_fips, by = c("county" = "county_name")) %>% select(-county) %>% finalize_long("ms")
save_long(ms_long, "he_ms")
res_ms <- check_long_vs_source(ms_long, file.path(OUTPUT_DIR, "elect_he_cty_ms.rds"))

print(bind_rows(res_sc, res_ms))
message("SC rows ", nrow(sc_long), "; candidates ", nrow(distinct(sc_long, year, district, candidate)), "; NA district rows ", sum(is.na(sc_long$district)),
        " | MS rows ", nrow(ms_long), "; candidates ", nrow(distinct(ms_long, year, district, candidate)), "; NA district rows ", sum(is.na(ms_long$district)))
print(as.data.frame(head(sc_long %>% filter(year == 2012, county_fips == 45001), 5)))
print(as.data.frame(head(ms_long %>% filter(year == 2012, county_fips == 28001), 5)))
