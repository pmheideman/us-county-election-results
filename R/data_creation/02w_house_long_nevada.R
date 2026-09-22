## Long table for Nevada (01am): 2000-2010 per-county files (party from file), 2012/2014 statewide precinct file with the hand party_map from 01am.
source(file.path("R", "data_creation", "02w_common.R"))
RAW <- file.path(RAW_ROOT, "nevada"); nv_fips <- fips_of("NEVADA"); stopifnot(nrow(nv_fips) == 17)
e <- new.env(parent = globalenv())
for (x in parse(file = file.path("R", "data_creation", "01am_house_county_nevada.R")))
  if (is.call(x) && identical(x[[1]], as.name("<-")) && is.name(x[[2]]) && as.character(x[[2]]) %in% c("nv_counties", "to_party", "is_pseudo_row", "party_map")) eval(x, e)
to_group <- e$to_party
rows_a <- purrr::map_dfr(c(2000, 2002, 2004, 2006, 2008, 2010), function(y) purrr::map_dfr(e$nv_counties, function(cty) {
  raw <- read_csv(file.path(RAW, sprintf("%d_%s.csv", y, cty)), show_col_types = FALSE, col_types = cols(.default = "c"))
  h <- raw %>% filter(grepl("REPRESENTATIVE IN CONGRESS", toupper(office))); if (nrow(h) == 0) return(NULL)
  h %>% filter(!e$is_pseudo_row(candidate)) %>%
    transmute(year = y, county = toupper(gsub("_", " ", cty)), district = NA_character_, candidate = trimws(candidate), party, party_group = to_group(party), votes = as.numeric(gsub(",", "", votes))) }))
rows_b <- purrr::map_dfr(c(2012, 2014), function(y)
  read_csv(file.path(RAW, sprintf("%d_statewide_precinct.csv", y)), show_col_types = FALSE, col_types = cols(.default = "c")) %>% filter(office == "U.S. House") %>%
    left_join(e$party_map %>% filter(year == !!y) %>% select(-year) %>% rename(pm = party), by = "candidate") %>%
    transmute(year = y, county = toupper(trimws(county)), district, candidate = trimws(candidate), party = coalesce(pm, "OTHER"), party_group = coalesce(pm, "OTHER"), votes = as.numeric(votes)))
long <- bind_rows(rows_a, rows_b) %>% filter(!is.na(votes)) %>% inner_join(nv_fips, by = c("county" = "county_name")) %>% select(-county)
long <- finalize_long(long, "nv"); save_long(long, "he_nv")
message("NV long rows: ", nrow(long)); print(check_long_vs_source(long, SRC("nv")))
