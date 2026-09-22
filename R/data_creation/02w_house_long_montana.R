## Long table for Montana (01ak): OpenElections county files 2000-2014; at-large seat -> district "00".
source(file.path("R", "data_creation", "02w_common.R"))
RAW <- file.path(RAW_ROOT, "montana"); mt_fips <- fips_of("MONTANA"); stopifnot(nrow(mt_fips) == 56)
to_group <- function(x) { x <- toupper(trimws(x)); case_when(grepl("^DEM", x) ~ "DEM", grepl("^REP", x) ~ "REP", TRUE ~ "OTHER") }
long <- purrr::map_dfr(2000 + 2 * (0:7), function(y)
  read_csv(file.path(RAW, paste0(y, "_general_county.csv")), show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    filter(trimws(office) == "U.S. House", trimws(county) != "Total") %>%
    transmute(year = y, county = case_when(toupper(trimws(county)) == "LEWIS & CLARK" ~ "LEWIS AND CLARK", TRUE ~ toupper(trimws(county))),
              district = "STATEWIDE", candidate = trimws(candidate), party, party_group = to_group(party), votes = as.numeric(votes)) %>% filter(!is.na(votes)))
long <- long %>% inner_join(mt_fips, by = c("county" = "county_name")) %>% select(-county)
long <- finalize_long(long, "mt"); save_long(long, "he_mt")
message("MT long rows: ", nrow(long)); print(check_long_vs_source(long, SRC("mt")))
