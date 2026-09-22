## Long table for Wyoming (01bb): OpenElections 1978-1988 house files and 2000-2014 county files. At-large seat -> district "00".
source(file.path("R", "data_creation", "02w_common.R"))
RAW <- file.path(RAW_ROOT, "wyoming"); wy_fips <- fips_of("WYOMING")
to_group <- function(x) { x <- toupper(trimws(x)); case_when(grepl("^DEM|^D$", x) ~ "DEM", grepl("^REP|^R$", x) ~ "REP", TRUE ~ "OTHER") }
mk <- function(path, year, offices) read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c")) %>% filter(trimws(office) %in% offices) %>%
  transmute(year = year, county = toupper(trimws(county)), district = "STATEWIDE", candidate, party, party_group = to_group(party), votes = as.numeric(votes)) %>% filter(!is.na(votes))
long <- bind_rows(
  purrr::map_dfr(c(1978, 1980, 1982, 1984, 1988), function(y) mk(file.path(RAW, paste0(y, "_general_house_county.csv")), y, c("U.S. House", "U.S House"))),
  purrr::map_dfr(c(2000, 2002, 2004, 2008, 2010, 2012, 2014), function(y) mk(file.path(RAW, paste0(y, "_general_county.csv")), y, "U.S. House")))
long <- long %>% inner_join(wy_fips %>% distinct(county_name, .keep_all = TRUE), by = c("county" = "county_name")) %>% select(-county)
long <- finalize_long(long, "wy"); save_long(long, "he_wy")
message("WY long rows: ", nrow(long)); print(check_long_vs_source(long, SRC("wy")))
