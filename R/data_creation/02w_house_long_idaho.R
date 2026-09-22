## Long table for Idaho (01ab): OpenElections county files 1994-2014, house rows via 01ab's is_us_house(); party via 01ab's to_party.
source(file.path("R", "data_creation", "02w_common.R"))
RAW <- file.path(RAW_ROOT, "idaho"); id_fips <- fips_of("IDAHO")
e <- new.env(parent = globalenv())
for (x in parse(file = file.path("R", "data_creation", "01ab_house_county_idaho.R")))
  if (is.call(x) && identical(x[[1]], as.name("<-")) && is.name(x[[2]]) && as.character(x[[2]]) %in% c("to_party", "is_us_house")) eval(x, e)
long <- purrr::map_dfr(c(1994, 1996, 1998, 2000, 2002, 2004, 2006, 2008, 2010, 2012, 2014), function(y)
  read_csv(file.path(RAW, paste0(y, "_general.csv")), show_col_types = FALSE, col_types = cols(.default = "c")) %>% filter(e$is_us_house(office)) %>%
    transmute(year = y, county = toupper(trimws(county)), district = dplyr::coalesce(ifelse(grepl("^[0-9]+$", district), district, NA_character_), ifelse(grepl("[0-9]", office), sub(".*?([0-9]+).*", "\\1", office), NA_character_)),
              candidate, party, party_group = e$to_party(party), votes = as.numeric(votes)) %>% filter(!is.na(votes)))
long <- long %>% inner_join(id_fips, by = c("county" = "county_name")) %>% select(-county)
long <- finalize_long(long, "id"); save_long(long, "he_id")
message("ID long rows: ", nrow(long)); print(check_long_vs_source(long, SRC("id")))
