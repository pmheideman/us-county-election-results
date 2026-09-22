## Long table for California OpenElections build (01q): 2002-2014. Same parsing; party_group via startsWith DEM/REP (as 01q).
source(file.path("R", "data_creation", "02w_common.R"))
RAW <- file.path(RAW_ROOT, "california"); ca_fips <- fips_of("CALIFORNIA")
to_group <- function(x) { x <- trimws(x); case_when(startsWith(x, "DEM") ~ "DEM", startsWith(x, "REP") ~ "REP", TRUE ~ "OTHER") }
long <- purrr::map_dfr(c(2002, 2004, 2006, 2008, 2010, 2012, 2014), function(y)
  read_csv(file.path(RAW, paste0(y, "_general.csv")), show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    filter(office == "U.S. House") %>%
    transmute(year = y, county = toupper(trimws(county)), district, candidate, party, party_group = to_group(party), votes = as.numeric(votes)) %>%
    filter(!is.na(votes)))
long <- long %>% inner_join(ca_fips, by = c("county" = "county_name")) %>% select(-county)
long <- finalize_long(long, "ca"); save_long(long, "he_ca")
message("CA long rows: ", nrow(long)); check_long_vs_source(long, SRC("ca"))
