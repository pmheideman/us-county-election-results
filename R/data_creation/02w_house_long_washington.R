## Long table for Washington (01ay): 2000-2006 statewide county files, 2010/2012 precinct files (de-duplicated exactly as 01ay), 2014 county file.
source(file.path("R", "data_creation", "02w_common.R"))
RAW <- file.path(RAW_ROOT, "washington"); wa_fips <- fips_of("WASHINGTON"); stopifnot(nrow(wa_fips) == 39)
to_group <- function(x) { x <- toupper(trimws(x)); case_when(x %in% c("D", "DEM", "DEMOCRAT", "DEMOCRATIC") ~ "DEM", x %in% c("R", "REP", "REPUBLICAN") ~ "REP", TRUE ~ "OTHER") }
nc <- function(x) toupper(trimws(x))
stat <- function(y) read_csv(file.path(RAW, paste0(y, "_general.csv")), show_col_types = FALSE, col_types = cols(.default = "c")) %>%
  filter(reporting_level == "county", grepl("^U\\.?\\s*S\\.?\\s*Representative", officename)) %>%
  transmute(year = y, county = nc(jurisdiction), district = officeposition, candidate = ballotname, party = partyname, party_group = to_group(partycode), votes = as.numeric(votes)) %>% filter(!is.na(votes))
PSEUDO <- c("TIMES COUNTED", "TIMES BLANK VOTED", "TIMES OVER VOTED", "REGISTERED VOTERS")
prec <- function(y, office_match) read_csv(file.path(RAW, paste0(y, "_general_precinct.csv")), show_col_types = FALSE, col_types = cols(.default = "c")) %>%
  filter(trimws(office) == office_match, !is.na(candidate), trimws(candidate) != "", !toupper(trimws(candidate)) %in% PSEUDO) %>%
  distinct(county, precinct, district, candidate, party, votes) %>%
  transmute(year = y, county = nc(county), district, candidate, party, party_group = to_group(party), votes = as.numeric(votes)) %>% filter(!is.na(votes))
c14 <- read_csv(file.path(RAW, "2014_general_county.csv"), show_col_types = FALSE, col_types = cols(.default = "c")) %>% filter(trimws(office) == "U.S. House") %>%
  transmute(year = 2014, county = nc(county), district, candidate, party, party_group = to_group(party), votes = as.numeric(votes)) %>% filter(!is.na(votes))
long <- bind_rows(stat(2000), stat(2002), stat(2004), stat(2006), prec(2010, "US House"), prec(2012, "U.S. House"), c14)
long <- long %>% inner_join(wa_fips, by = c("county" = "county_name")) %>% select(-county)
long <- finalize_long(long, "wa"); save_long(long, "he_wa")
message("WA long rows: ", nrow(long)); print(check_long_vs_source(long, SRC("wa")))
