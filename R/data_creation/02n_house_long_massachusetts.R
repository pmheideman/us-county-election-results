## Candidate-level LONG table for Massachusetts U.S. House (source: elect_he_cty_ma.rds, built by 01ag). Same logic: town-level files 2000-2010 + 2014 mapped to
## counties through the town crosswalk (party WORDS; "Total Votes Cast"/"Blank Votes" dropped); 2012 has a county column, county-wide rollup pseudo-rows
## (district CON/CD/ALL) and blanks dropped, blank parties filled from the candidate's own non-blank rows elsewhere in the file (single-letter D/R codes).
source(file.path("R", "00_setup.R")); library(readr); library(stringr); source(file.path("R", "long_helpers.R"))
save_step <- function(df, name) invisible(df)
RAW <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "massachusetts")
ma_fips <- read_delim(file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name))) %>% filter(state == "MASSACHUSETTS") %>% select(county_name, county_fips)
ma_town_county <- read_csv(file.path(RAW, "ma_town_county_crosswalk.csv"), show_col_types = FALSE) %>% mutate(town = toupper(trimws(town)), county = toupper(trimws(county)))
expand_abbrev <- function(x) { x <- toupper(trimws(x)); x <- sub("^E\\.\\s+", "EAST ", x); x <- sub("^N\\.\\s+", "NORTH ", x); x <- sub("^S\\.\\s+", "SOUTH ", x); sub("^W\\.\\s+", "WEST ", x) }
town_to_county <- function(x) ma_town_county$county[match(expand_abbrev(x), ma_town_county$town)]
HOUSE_RE <- "^U\\.S\\. House$"
to_party_word <- function(x) { x <- toupper(trimws(x)); case_when(startsWith(x, "DEMOCRAT") ~ "DEM", startsWith(x, "REPUBLICAN") ~ "REP", TRUE ~ "OTHER") }
to_party_code <- function(x) { x <- toupper(trimws(x)); case_when(x == "D" ~ "DEM", x == "R" ~ "REP", TRUE ~ "OTHER") }
DROP_CANDIDATES <- c("Total Votes Cast", "Blank Votes")

town <- purrr::map_dfr(c(2000, 2002, 2004, 2006, 2008, 2010, 2014), function(y) read_csv(file.path(RAW, paste0(y, "_town.csv")), show_col_types = FALSE, col_types = cols(.default = "c")) %>%
  filter(str_detect(office, HOUSE_RE), !(candidate %in% DROP_CANDIDATES)) %>%
  mutate(county = town_to_county(town), party_lbl = party, party_group = to_party_word(party), votes = as.numeric(votes)) %>%
  filter(!is.na(county), !is.na(votes)) %>% transmute(year = y, county, district, candidate, party = party_lbl, party_group, votes))

r12 <- read_csv(file.path(RAW, "2012_precinct.csv"), show_col_types = FALSE, col_types = cols(.default = "c"))
h12 <- r12 %>% filter(str_detect(office, HOUSE_RE)) %>% filter(!(district %in% c("CON", "CD", "ALL"))) %>% filter(!(candidate %in% c("Blanks", "blanks", DROP_CANDIDATES)))
lookup <- h12 %>% filter(trimws(party) != "") %>% count(candidate, party) %>% group_by(candidate) %>% slice_max(n, n = 1, with_ties = FALSE) %>% ungroup() %>% select(candidate, party_filled = party)
y12 <- h12 %>% left_join(lookup, by = "candidate") %>% mutate(party_raw = coalesce(na_if(trimws(party), ""), party_filled)) %>%
  mutate(county = toupper(trimws(county)), party_group = to_party_code(party_raw), votes = as.numeric(votes)) %>% filter(!is.na(votes)) %>%
  transmute(year = 2012, county, district, candidate, party = party_raw, party_group, votes)

df <- bind_rows(town, y12) %>% inner_join(ma_fips, by = c("county" = "county_name"))
## 2012 party labels are single letters (D/R/...): standard_party_label maps D/R; other codes are shown as reported
long <- finalize_long(df %>% select(year, county_fips, district, candidate, party, party_group, votes), "ma"); save_long(long, "he_ma")
res <- check_long_vs_source(long, file.path(OUTPUT_DIR, "elect_he_cty_ma.rds")); print(res)
message("NA district rows: ", sum(is.na(long$district)), " of ", nrow(long))
