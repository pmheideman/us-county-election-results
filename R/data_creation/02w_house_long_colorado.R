## Long table for Colorado (01w): OpenElections 2002 & 2014 county files, 2004-2012 precinct files summed to county. Filters copied from 01w
## (2002/2014: candidate non-blank; 2002: county != TOTALS; precinct years: NO candidate filter, exactly as 01w).
source(file.path("R", "data_creation", "02w_common.R"))
RAW <- file.path(RAW_ROOT, "colorado"); co_fips <- fips_of("COLORADO")
to_group <- function(x) { x <- toupper(trimws(x)); case_when(startsWith(x, "DEM") ~ "DEM", startsWith(x, "REP") ~ "REP", TRUE ~ "OTHER") }
is_us_house <- function(office) { office <- trimws(office); !is.na(office) & (office == "U.S. House" | grepl("United States Congress", office, fixed = TRUE)) }
rd <- function(f, year) read_csv(file.path(RAW, f), show_col_types = FALSE, col_types = cols(.default = "c")) %>% mutate(year = year)
fin <- function(d) d %>% transmute(year, county = toupper(trimws(county)), district, candidate, party, party_group = to_group(party), votes = as.numeric(votes)) %>% filter(!is.na(votes))
d02 <- rd("2002_general.csv", 2002) %>% filter(is_us_house(office), county != "TOTALS", trimws(candidate) != "") %>% fin()
dpr <- purrr::map_dfr(c(2004, 2006, 2008, 2010, 2012), function(y) rd(paste0(y, "_precinct.csv"), y) %>% filter(is_us_house(office)) %>% fin())
d14 <- rd("2014_general.csv", 2014) %>% filter(is_us_house(office), trimws(candidate) != "") %>% fin()
long <- bind_rows(d02, dpr, d14) %>% inner_join(co_fips, by = c("county" = "county_name")) %>% select(-county)
message("blank/NA candidate rows kept (as in 01w): ", sum(is.na(long$candidate) | !nzchar(trimws(long$candidate))), " (votes ", sum(long$votes[is.na(long$candidate) | !nzchar(trimws(long$candidate))]), ")")
long <- finalize_long(long, "co"); save_long(long, "he_co")
message("CO long rows: ", nrow(long)); print(check_long_vs_source(long, SRC("co")))
