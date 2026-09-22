## Long table for New Mexico (01ao): OpenElections 2000/2002/2008/2010/2012 county files + 2014 precinct file (summed to county).
source(file.path("R", "data_creation", "02w_common.R"))
RAW <- file.path(RAW_ROOT, "new_mexico"); nm_fips <- fips_of("NEW MEXICO") %>% distinct()
nc <- function(x) gsub("Ñ", "N", toupper(trimws(x)), fixed = TRUE)
to_group <- function(x) { x <- toupper(trimws(x)); case_when(grepl("^DEM", x) ~ "DEM", grepl("^REP", x) ~ "REP", TRUE ~ "OTHER") }
gen <- function(y) read_csv(file.path(RAW, paste0(y, "_general.csv")), show_col_types = FALSE, col_types = cols(.default = "c")) %>%
  filter(office == "UNITED STATES REPRESENTATIVE", trimws(county) != "") %>%
  transmute(year = y, county = nc(county), district, candidate = trimws(candidate), party, party_group = to_group(party), votes = as.numeric(votes))
p14 <- read_csv(file.path(RAW, "2014_general_precinct.csv"), show_col_types = FALSE, col_types = cols(.default = "c")) %>% filter(office == "U.S. House") %>%
  transmute(year = 2014, county = nc(county), district, candidate = trimws(candidate), party, party_group = to_group(party), votes = as.numeric(votes))
long <- bind_rows(gen(2000), gen(2002), gen(2008), gen(2010), gen(2012), p14) %>% filter(!is.na(votes)) %>%
  inner_join(nm_fips, by = c("county" = "county_name")) %>% select(-county)
long <- finalize_long(long, "nm"); save_long(long, "he_nm")
message("NM long rows: ", nrow(long)); print(check_long_vs_source(long, SRC("nm")))
