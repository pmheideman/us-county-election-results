## Candidate-level LONG tables for Alabama U.S. House. Southeast batch.
##   (1) he_al: 2012-2014 from OpenElections precinct files (shares file elect_he_cty_al.rds; script 01s).
##   (2) he_al_historical: 1980-2012 from the Auburn University spreadsheet (shares file elect_he_cty_al_historical.rds; script 01ar).
## Part (1) reuses 01s's definitions (downloader, AL_2012_PARTY lookup, to_party_2014, crosswalk) and its exact filters: precinct rows named
## like "total"/"registered" are excluded (they duplicated votes), two literal county typos are repaired, 2012 party comes from the hand-built
## candidate lookup (the 2012 source has no usable party), everything not DEM/REP is OTHER. Adds candidate names + district.
## Part (2) is appended below by the historical block.
## Acceptance: check_long_vs_source against each shares file.

source(file.path("R", "00_setup.R"))
save_step <- function(df, name) invisible(df)
library(readr)
source(file.path("R", "long_helpers.R"))
exprs <- parse(file = file.path("R", "data_creation", "01s_house_county_alabama.R"))
want <- c("RAW_DIR", "county_fips_crosswalk", "al_fips", "download_oe", "AL_2012_PARTY", "to_party_2014")
eval(exprs[vapply(exprs, function(e) is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) && as.character(e[[2]]) %in% want, NA)])

r12 <- read_csv(download_oe("2012/20121106__al__general__precinct.csv", "2012_general_precinct.csv"), show_col_types = FALSE, col_types = cols(.default = "c")) %>%
  filter(office == "U.S. House") %>% filter(!grepl("total|registered", precinct, ignore.case = TRUE)) %>%
  mutate(county = toupper(trimws(county)),
         county = case_when(county == "RADOLPH" ~ "RANDOLPH", county == "ST" ~ "ST. CLAIR", TRUE ~ county),
         votes = as.numeric(votes)) %>%
  filter(!is.na(votes)) %>% select(-party) %>%
  left_join(AL_2012_PARTY, by = "candidate") %>%
  mutate(party_label = party, party = ifelse(is.na(party), "OTHER", party)) %>%
  transmute(year = 2012, county, district, candidate, party = party_label, party_group = party, votes)

r14 <- read_csv(download_oe("2014/20141104__al__general__precinct.csv", "2014_general_precinct.csv"), show_col_types = FALSE, col_types = cols(.default = "c")) %>%
  filter(office == "U.S. House") %>% filter(!grepl("total|registered", precinct, ignore.case = TRUE)) %>%
  mutate(county = toupper(trimws(county)), party_group = to_party_2014(party), votes = as.numeric(votes)) %>%
  filter(!is.na(votes)) %>% transmute(year = 2014, county, district, candidate, party, party_group, votes)

al_long <- bind_rows(r12, r14) %>% inner_join(al_fips, by = c("county" = "county_name")) %>% select(-county) %>% finalize_long("al")
save_long(al_long, "he_al")
res_al <- check_long_vs_source(al_long, file.path(OUTPUT_DIR, "elect_he_cty_al.rds"))
print(res_al)
message("AL (OE) rows ", nrow(al_long), "; candidates ", nrow(distinct(al_long, year, district, candidate)), "; party label blanks filled from group for 2012: ",
        sum(r12$candidate %in% AL_2012_PARTY$candidate), " of ", nrow(r12), " rows have a looked-up party")
print(as.data.frame(head(al_long %>% filter(year == 2012, county_fips == 1001), 6)))
