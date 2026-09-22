## SUPERSEDED 2026-09-22 by 02af_house_ut_2012_2014.R -- see 01aw_house_county_utah.R's header. Do not re-run this script; its output
## files (he_ut.rds, elect_he_cty_ut.rds) were deleted and its source_registry.csv row removed.
##
## Long table for Utah OpenElections 2012/2014 (01aw): county precinct files from the cached repo tarball, blank-party fallback via the
## candidate lookup exactly as 01aw. Candidate spelling for display comes from the raw string.
source(file.path("R", "data_creation", "02w_common.R"))
library(stringr)
ut_fips <- fips_of("UTAH"); stopifnot(nrow(ut_fips) == 29)
REPO_ROOT <- list.dirs(file.path(RAW_ROOT, "utah", "extracted"), recursive = FALSE)[1]
to_party <- function(x) { x <- toupper(trimws(x)); case_when(grepl("^DEM", x) ~ "DEM", grepl("^REP", x) ~ "REP", TRUE ~ "OTHER") }
is_house <- function(x) { x <- toupper(trimws(x)); grepl("^(U\\.?\\s*S\\.?|UNITED\\s+STATES)\\s*(HOUSE|CONGRESS)", x) }
PSEUDO <- c("REGISTERED VOTERS", "BALLOTS CAST")
rd <- function(y) { files <- list.files(file.path(REPO_ROOT, as.character(y)), pattern = "precinct\\.csv$", full.names = TRUE)
  bind_rows(lapply(files, read_csv, show_col_types = FALSE, col_types = cols(.default = "c"))) %>%
    filter(is_house(office), !(toupper(trimws(candidate)) %in% PSEUDO)) %>%
    transmute(year = y, county = toupper(trimws(gsub("\\s*COUNTY\\s*$", "", county, ignore.case = TRUE))), district = trimws(district), candidate,
              ckey = toupper(str_squish(gsub("[ \\s]+", " ", candidate))), party_raw = party, votes = as.numeric(votes)) %>% filter(!is.na(votes)) }
all_rows <- bind_rows(rd(2012), rd(2014)) %>% mutate(party_group = to_party(party_raw))
lk <- all_rows %>% filter(party_group %in% c("DEM", "REP")) %>% count(ckey, party_group) %>% group_by(ckey) %>% slice_max(n, n = 1, with_ties = FALSE) %>% ungroup() %>% select(ckey, pl = party_group)
all_rows <- all_rows %>% left_join(lk, by = "ckey") %>% mutate(party_group = if_else(party_group == "OTHER" & is.na(party_raw) & !is.na(pl), pl, party_group)) %>% select(-pl, -ckey)
long <- all_rows %>% rename(party = party_raw) %>% inner_join(ut_fips, by = c("county" = "county_name")) %>% select(-county)
long <- finalize_long(long, "ut"); save_long(long, "he_ut")
message("UT long rows: ", nrow(long)); print(check_long_vs_source(long, SRC("ut")))
