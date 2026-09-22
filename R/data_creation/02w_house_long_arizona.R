## Long table for Arizona OpenElections build (01u): 2000-2014. party_group: exactly "DEM"/"REP" else OTHER (as 01u).
source(file.path("R", "data_creation", "02w_common.R"))
RAW <- file.path(RAW_ROOT, "arizona"); az_fips <- fips_of("ARIZONA")
to_group <- function(x) { x <- trimws(x); case_when(x == "DEM" ~ "DEM", x == "REP" ~ "REP", TRUE ~ "OTHER") }
long <- purrr::map_dfr(c(2000, 2002, 2004, 2006, 2008, 2010, 2012, 2014), function(y)
  read_csv(file.path(RAW, paste0(y, "_general.csv")), show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    filter(office == "U.S. House") %>%
    transmute(year = y, county = toupper(trimws(county)), district, candidate, party, party_group = to_group(party), votes = as.numeric(votes)) %>% filter(!is.na(votes)))
long <- long %>% inner_join(az_fips, by = c("county" = "county_name")) %>% select(-county)
long <- finalize_long(long, "az"); save_long(long, "he_az")
message("AZ long rows: ", nrow(long)); print(check_long_vs_source(long, SRC("az")))
