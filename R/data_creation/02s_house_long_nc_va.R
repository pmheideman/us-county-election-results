## Candidate-level LONG tables for North Carolina (2000-2016, NCSBE precinct files) and Virginia (2006-2016, SBE CSVs).
## Southeast batch. Reuses the parsing helpers of 01d_house_county_open_states.R by parsing that file and evaluating ONLY its
## function/constant definitions (the fold-in code in 01d, which writes elect_cty_final.rds, is never run).
## party_group mirrors the original builds exactly: NC party_cd == "DEM"/"REP"; VA Party == "Democratic"/"Republican"; everything else
## (incl. write-ins and blank party) is OTHER and still counts toward totalvote, as in the original.
## Outputs: R/output/long/he_nc.rds, R/output/long/he_va.rds. Acceptance: check_long_vs_source against elect_he_cty_nc.rds / _va.rds.

source(file.path("R", "00_setup.R"))
save_step <- function(df, name) invisible(df)           # never write shares/panel files from here
library(readr)
source(file.path("R", "long_helpers.R"))

exprs <- parse(file = file.path("R", "data_creation", "01d_house_county_open_states.R"))
want <- c("RAW_DIR", "county_fips_crosswalk", "NC_GENERAL_ELECTIONS", "NO_HEADER_COLS", "COL_ALIASES", "standardize_names",
          "download_nc_file", "curl_get", "VA_BASE", "VA_YEARS", "find_va_general_csv_url", "download_va_file")
keep <- vapply(exprs, function(e) is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) && as.character(e[[2]]) %in% want, NA)
eval(exprs[keep])

## ------------------------------------------------------------------ North Carolina
read_nc_candidates <- function(year, date_str, delim, has_header, quote_char) {
  path <- download_nc_file(date_str)
  if (has_header) {
    raw <- read_delim(path, delim = delim, quote = quote_char, show_col_types = FALSE, col_types = cols(.default = "c"))
  } else {
    raw <- read_delim(path, delim = delim, quote = quote_char, show_col_types = FALSE, col_names = NO_HEADER_COLS, col_types = cols(.default = "c"))
  }
  names(raw) <- standardize_names(names(raw))
  candcol <- intersect(c("name_on_ballot", "choice"), names(raw))[1]
  raw %>%
    filter(grepl("^\"?US (HOUSE|CONGRESS)", contest, ignore.case = TRUE)) %>%
    mutate(votes = as.numeric(votes),
           party = coalesce(trimws(gsub('"', "", party)), ""),
           county = trimws(gsub('"', "", county)),
           contest = trimws(gsub('"', "", contest)),
           candidate = trimws(gsub('"', "", .data[[candcol]]))) %>%
    group_by(county, contest, candidate, party) %>%
    summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
    mutate(year = year)
}
nc_c <- purrr::pmap(NC_GENERAL_ELECTIONS, function(year, date_str, delim, has_header, quote_char) read_nc_candidates(year, date_str, delim, has_header, quote_char)) %>% bind_rows()
nc_fips <- county_fips_crosswalk %>% filter(state == "NORTH CAROLINA") %>% select(county_name, county_fips)
nc_c <- nc_c %>% mutate(district = sub("^.*?(\\d+).*$", "\\1", contest)) %>%
  inner_join(nc_fips, by = c("county" = "county_name"))          # same join as the original; unmatched counties are dropped there too
## original also dropped county-years with totalvote <= 0; derive_shares() applies the same rule
nc_long <- nc_c %>% transmute(year, county_fips, district, candidate, party, party_group = case_when(party == "DEM" ~ "DEM", party == "REP" ~ "REP", TRUE ~ "OTHER"), votes) %>%
  finalize_long("nc")
save_long(nc_long, "he_nc")
message("NC contests seen (first 6): ", paste(head(unique(nc_c$contest), 6), collapse = " | "))
nc_res <- check_long_vs_source(nc_long, file.path(OUTPUT_DIR, "elect_he_cty_nc.rds"))

## ------------------------------------------------------------------ Virginia
read_va_candidates <- function(year) {
  path <- download_va_file(year)
  raw <- read_csv(path, col_types = cols(.default = "c"), show_col_types = FALSE)
  raw %>%
    filter(grepl("House of Representatives", OfficeTitle, ignore.case = TRUE)) %>%
    mutate(votes = as.numeric(TOTAL_VOTES), party = coalesce(trimws(Party), ""),
           locality = trimws(gsub("\\s+COUNTY$", "", toupper(LocalityName))),
           candidate = trimws(gsub("\\s+", " ", paste(coalesce(FirstName, ""), coalesce(MiddleName, ""), coalesce(LastName, ""), coalesce(Suffix, "")))),
           candidate = ifelse(!nzchar(candidate) | grepl("^WRITE.?IN( VOTES)?$", toupper(candidate)), "Write-in", candidate),
           district = DistrictName) %>%
    group_by(locality, district, candidate, party) %>%
    summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>% mutate(year = year)
}
va_c <- purrr::map_dfr(VA_YEARS, read_va_candidates)

## same fips repair as the original (Fairfax/Franklin/Richmond bare-name collisions)
va_fips_raw <- county_fips_crosswalk %>% filter(state == "VIRGINIA") %>% mutate(county_name = gsub("\\s+COUNTY$", "", county_name)) %>% distinct(county_name, county_fips)
city_fips_by_base_name <- va_fips_raw %>% filter(grepl("\\s+CITY$", county_name)) %>% mutate(base_name = gsub("\\s+CITY$", "", county_name)) %>% pull(county_fips)
ambiguous_names <- va_fips_raw %>% count(county_name) %>% filter(n > 1) %>% pull(county_name)
va_fips <- va_fips_raw %>% filter(!(county_name %in% ambiguous_names & county_fips %in% city_fips_by_base_name))

## The original summed to (year, locality) FIRST, joined the fips, then `distinct(year, cty_fips)` kept the first locality per fips.
## Reproduce which (year, locality, fips) triples survived, then keep exactly those rows' candidates.
va_keep <- va_c %>% group_by(year, locality) %>% summarise(totalvote = sum(votes), .groups = "drop") %>%
  left_join(va_fips, by = c("locality" = "county_name")) %>% filter(!is.na(county_fips), totalvote > 0) %>%
  distinct(year, county_fips, .keep_all = TRUE) %>% select(year, locality, county_fips)
va_long <- va_c %>% inner_join(va_keep, by = c("year", "locality")) %>%
  transmute(year, county_fips, district, candidate, party, party_group = case_when(party == "Democratic" ~ "DEM", party == "Republican" ~ "REP", TRUE ~ "OTHER"), votes) %>%
  finalize_long("va")
save_long(va_long, "he_va")
va_res <- check_long_vs_source(va_long, file.path(OUTPUT_DIR, "elect_he_cty_va.rds"))

print(bind_rows(nc_res, va_res))
message("NC rows ", nrow(nc_long), ", candidates ", nrow(distinct(nc_long, year, district, candidate)), "; VA rows ", nrow(va_long), ", candidates ", nrow(distinct(va_long, year, district, candidate)))
print(as.data.frame(nc_long %>% count(year, district) %>% count(year, name = "districts")))
print(as.data.frame(head(nc_long %>% filter(year == 2012, county_fips == 37001), 8)))
print(as.data.frame(head(va_long %>% filter(year == 2012, county_fips == 51001), 8)))
