## Long table for Oregon (01bc): 2000/2002/2004 precinct files per county (file list evaluated from 01bc), 2006-2014 county files.
## Pseudo rows (TOTAL*, WRITE-IN*, SCATTERING*, OVER/UNDER VOTES) excluded exactly as 01bc's is_pseudo_row.
## Precinct-level files also drop precinct=="Total" rows -- a county-wide rollup disguised as an
## ordinary precinct row that duplicates the real precinct rows (see 01bc's is_total_precinct and
## 01dr_oregon_total_precinct_apply.R, which found and fixed the resulting doubling in the shares panel).
source(file.path("R", "data_creation", "02w_common.R"))
RAW <- file.path(RAW_ROOT, "oregon"); or_fips <- fips_of("OREGON"); stopifnot(nrow(or_fips) == 36)
e <- new.env(parent = globalenv())
for (x in parse(file = file.path("R", "data_creation", "01bc_house_county_oregon.R")))
  if (is.call(x) && identical(x[[1]], as.name("<-")) && is.name(x[[2]]) && as.character(x[[2]]) %in% c("to_party", "is_pseudo_row", "is_total_precinct", "precinct_files", "county_files")) eval(x, e)
to_group <- e$to_party; is_pseudo <- e$is_pseudo_row; is_total_prec <- e$is_total_precinct
rd <- function(path, year, drop_total_precinct) {
  raw <- read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    filter(toupper(trimws(office)) == "U.S. HOUSE", !is_pseudo(candidate))
  if (drop_total_precinct) raw <- raw %>% filter(!is_total_prec(precinct))
  raw %>% transmute(year = as.integer(year), county = toupper(trimws(county)), district, candidate, party, party_group = to_group(party), votes = as.numeric(votes))
}
prec <- purrr::map_dfr(names(e$precinct_files), function(yr) purrr::map_dfr(names(e$precinct_files[[yr]]), function(cty)
  rd(file.path(RAW, paste0(yr, "_", cty, "_precinct.csv")), yr, drop_total_precinct = TRUE)))
cty <- purrr::map_dfr(names(e$county_files), function(yr) rd(file.path(RAW, paste0(yr, "_general_county.csv")), yr, drop_total_precinct = FALSE))
long <- bind_rows(prec, cty) %>% filter(!is.na(votes)) %>% inner_join(or_fips, by = c("county" = "county_name")) %>% select(-county)
long <- finalize_long(long, "or"); save_long(long, "he_or")
message("OR long rows: ", nrow(long)); print(check_long_vs_source(long, SRC("or")))
