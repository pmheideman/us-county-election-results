## Candidate-level LONG table for the Wisconsin House build (shares file elect_he_cty_wi.rds, script 01ba). Ward files, the office == "House" filter and the
## DEM/DEMOCRAT/DEMOCRATIC, REP/REPUBLICAN grouping copied from 01ba; candidate/district kept. (2004's file yields no "House" rows, exactly as in 01ba, so the
## shares file has 7 years.) Output: R/output/long/he_wi.rds
source(file.path("R", "00_setup.R")); library(readr); source(file.path("R", "long_helpers.R"))
save_step <- function(df, name) invisible(df)
RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "wisconsin")
wi_fips <- read_delim(file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name))) %>% filter(state == "WISCONSIN") %>% select(county_name, county_fips)
stopifnot(nrow(wi_fips) == 72)
to_party <- function(x) { x <- toupper(trimws(x)); case_when(x %in% c("DEM", "DEMOCRAT", "DEMOCRATIC") ~ "DEM", x %in% c("REP", "REPUBLICAN") ~ "REP", TRUE ~ "OTHER") }
read_wi_year <- function(year) {
  read_csv(file.path(RAW_DIR, paste0(year, "_general_ward.csv")), show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    filter(trimws(office) == "House") %>%
    transmute(county = toupper(trimws(county)), district, candidate, party_raw = party, party_group = to_party(party), votes = as.numeric(votes)) %>%
    filter(!is.na(votes)) %>%
    group_by(county, district, candidate, party_raw, party_group) %>% summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>% mutate(year = year)
}
raw <- purrr::map_dfr(c(2000, 2002, 2004, 2006, 2008, 2010, 2012, 2014), read_wi_year) %>% rename(party = party_raw) %>%
  left_join(wi_fips, by = c("county" = "county_name")) %>% filter(!is.na(county_fips))
long <- finalize_long(raw, "wi"); save_long(long, "he_wi")
res <- check_long_vs_source(long, file.path(OUTPUT_DIR, "elect_he_cty_wi.rds")); print(res)
message("rows ", nrow(long), "; years present: ", paste(sort(unique(long$year)), collapse = ","), "; candidates ", nrow(distinct(long, year, district, candidate)), "; blank-party raw rows: ", sum(is.na(raw$party) | raw$party == ""))
