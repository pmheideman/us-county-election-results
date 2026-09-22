## Candidate-level LONG table for Connecticut U.S. House (source: elect_he_cty_ct.rds, built by 01x). Same logic: literal ballot-line party classification
## (NO cross-line consolidation), 2000-2010 files carry a county column (statewide/TOTAL rollup rows dropped), 2012 maps towns to counties. 2016 in 01x is only
## a cross-check and is not part of the shares file, so it is not built here.
source(file.path("R", "00_setup.R")); library(readr); source(file.path("R", "long_helpers.R"))
save_step <- function(df, name) invisible(df)
RAW <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "connecticut")
ct_fips <- read_delim(file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name))) %>% filter(state == "CONNECTICUT") %>% select(county_name, county_fips)
TOWN_ALIAS <- c(BEACONFALLS = "BEACON FALLS", DEEPRIVER = "DEEP RIVER", EASTHADDAM = "EAST HADDAM", EASTHAMPTON = "EAST HAMPTON", MERIDIEN = "MERIDEN",
                NORTHCANAAN = "NORTH CANAAN", NORTHSTONINGTON = "NORTH STONINGTON", OLDLYME = "OLD LYME", `NEW MILLFORD` = "NEW MILFORD")
ct_town_county <- read_csv(file.path(RAW, "ct_town_county_crosswalk.csv"), show_col_types = FALSE) %>%
  mutate(town = toupper(trimws(town)), county = toupper(trimws(sub(" County$", "", county))))
town_to_county <- function(x) { x <- toupper(trimws(x)); x <- coalesce(TOWN_ALIAS[x], x); ct_town_county$county[match(x, ct_town_county$town)] }
clean_county <- function(x) { x <- toupper(trimws(x)); x <- gsub("\\.", " ", x); x[x %in% c("CONNECTICUT", "TOTAL")] <- NA; x }
HOUSE_RE <- "^U\\.?S\\.?\\s*House"
to_party <- function(x) { x <- toupper(trimws(x)); case_when(x == "DEM" | startsWith(x, "DEMOCRAT") ~ "DEM", x == "REP" | startsWith(x, "REPUBLICAN") ~ "REP", TRUE ~ "OTHER") }

direct <- purrr::map_dfr(c(2000, 2002, 2006, 2008, 2010), function(y) read_csv(file.path(RAW, paste0(y, "_direct.csv")), show_col_types = FALSE, col_types = cols(.default = "c")) %>%
  filter(grepl(HOUSE_RE, office)) %>% mutate(county = clean_county(county), party_lbl = party, party_group = to_party(party), votes = as.numeric(votes)) %>%
  filter(!is.na(county), !is.na(votes)) %>% mutate(district = ifelse(is.na(district), sub("^.*House\\s*", "", office), district)) %>%   # 2006: district only in the office string ("US House 3")
  transmute(year = y, county, district, candidate, party = party_lbl, party_group, votes))
p12 <- read_csv(file.path(RAW, "2012_precinct.csv"), show_col_types = FALSE, col_types = cols(.default = "c")) %>%
  filter(grepl(HOUSE_RE, office)) %>% mutate(county = town_to_county(town), party_lbl = party, party_group = to_party(party), votes = as.numeric(votes)) %>%
  filter(!is.na(county), !is.na(votes)) %>% transmute(year = 2012, county, district, candidate, party = party_lbl, party_group, votes)
df <- bind_rows(direct, p12) %>% inner_join(ct_fips, by = c("county" = "county_name"))
long <- finalize_long(df %>% select(year, county_fips, district, candidate, party, party_group, votes), "ct"); save_long(long, "he_ct")
res <- check_long_vs_source(long, file.path(OUTPUT_DIR, "elect_he_cty_ct.rds")); print(res)
message("NA district rows: ", sum(is.na(long$district)), " of ", nrow(long), "; distinct candidate-names: ", n_distinct(long$candidate))
