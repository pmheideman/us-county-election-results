## Candidate-level LONG table for Tennessee U.S. House 2000-2014 (OpenElections county files). Southeast batch.
## Reuses 01av's definitions (crosswalk, downloader, TN_GENERAL_FILES) and its exact filter/grouping; adds the candidate names, the raw party
## label and the district column the original dropped. party_group: label starts with DEM / REP else OTHER, exactly as the original to_party().
## Output: R/output/long/he_tn.rds. Acceptance: check_long_vs_source against elect_he_cty_tn.rds.

source(file.path("R", "00_setup.R"))
save_step <- function(df, name) invisible(df)
library(readr)
source(file.path("R", "long_helpers.R"))
exprs <- parse(file = file.path("R", "data_creation", "01av_house_county_tennessee.R"))
want <- c("RAW_DIR", "county_fips_crosswalk", "tn_fips", "download_oe", "to_party", "TN_GENERAL_FILES")
eval(exprs[vapply(exprs, function(e) is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) && as.character(e[[2]]) %in% want, NA)])

read_year <- function(year) {
  path <- download_oe(TN_GENERAL_FILES[[as.character(year)]], paste0(year, "_general_county.csv"))
  read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    filter(toupper(trimws(office)) == "U.S. HOUSE") %>%
    transmute(year = year, county = toupper(trimws(county)), district, candidate = trimws(candidate), party,
              party_group = to_party(party), votes = as.numeric(votes))
}
raw <- bind_rows(lapply(c(2000, 2002, 2004, 2006, 2008, 2010, 2012, 2014), read_year)) %>% filter(!is.na(votes))
long <- raw %>% inner_join(tn_fips, by = c("county" = "county_name")) %>% select(-county) %>% finalize_long("tn")
save_long(long, "he_tn")
res <- check_long_vs_source(long, file.path(OUTPUT_DIR, "elect_he_cty_tn.rds")); print(res)
message("TN rows ", nrow(long), "; candidates ", nrow(distinct(long, year, district, candidate)), "; districts per year: ",
        paste(long %>% distinct(year, district) %>% count(year) %>% pull(n), collapse = ","))
print(as.data.frame(long %>% count(party, party_group, sort = TRUE) %>% head(8)))
print(as.data.frame(head(long %>% filter(year == 2012, county_fips == 47001), 6)))
