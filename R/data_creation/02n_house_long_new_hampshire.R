## Candidate-level LONG tables for New Hampshire U.S. House: he_cty_nh (2012, 2014 OpenElections; 01an) and he_cty_nh_historical (1996-2010; 01bp).
## Same parsing / town->county mapping / party grouping as the originals. The historical sources name candidates as "Surname, party-letter" (no first names), so the
## long candidate is the surname and the party label is decoded from the letter. Districts are kept (the shares summed across the two districts).
source(file.path("R", "00_setup.R")); library(readr); library(stringr); source(file.path("R", "long_helpers.R"))
save_step <- function(df, name) invisible(df)
RAW <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "new_hampshire"); ASSET <- file.path(RAW, "assets")
nh_fips <- read_delim(file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name))) %>% filter(state == "NEW HAMPSHIRE") %>% select(county_name, county_fips)

## ---------------- 2012, 2014 ----------------
nh_town_county <- read_csv(file.path(RAW, "nh_town_county_crosswalk.csv"), show_col_types = FALSE) %>% mutate(town = toupper(trimws(town)), county = toupper(trimws(county)))
town_to_county <- function(x) nh_town_county$county[match(toupper(trimws(x)), nh_town_county$town)]
to_party <- function(x) { x <- toupper(trimws(x)); case_when(x == "D" ~ "DEM", x == "R" ~ "REP", TRUE ~ "OTHER") }
y12 <- purrr::map_dfr(c("20121106__nh__general__house__1__town.csv", "20121106__nh__general__house__2__town.csv"), function(f)
  read_csv(file.path(RAW, paste0("2012_", f)), show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    mutate(county = toupper(trimws(county)), party_lbl = party, party_group = to_party(party), votes = as.numeric(votes)) %>% filter(!is.na(county), !is.na(votes)) %>%
    transmute(year = 2012, county, district, candidate, party = party_lbl, party_group, votes))
y14 <- read_csv(file.path(RAW, "2014_town.csv"), show_col_types = FALSE, col_types = cols(.default = "c")) %>% filter(grepl("^Congressional District", office)) %>%
  mutate(county = town_to_county(town), party_lbl = party, party_group = to_party(party), votes = as.numeric(votes), district = sub("^Congressional District\\s*", "", office)) %>%
  filter(!is.na(county), !is.na(votes)) %>% transmute(year = 2014, county, district, candidate, party = party_lbl, party_group, votes)
d1 <- bind_rows(y12, y14) %>% inner_join(nh_fips, by = c("county" = "county_name"))
long1 <- finalize_long(d1 %>% select(year, county_fips, district, candidate, party, party_group, votes), "nh"); save_long(long1, "he_nh")
res1 <- check_long_vs_source(long1, file.path(OUTPUT_DIR, "elect_he_cty_nh.rds")); print(res1)

## ---------------- 1996-2010 (historical) ----------------
norm_key <- function(x) { x <- toupper(x); x <- str_replace_all(x, "\\*", ""); x <- str_replace_all(x, "\\s+(WARD|WD)\\s*\\d+\\s*$", ""); str_replace_all(x, "[^A-Z0-9&]", "") }
nh_tc2 <- read_csv(file.path(RAW, "nh_town_county_crosswalk.csv"), show_col_types = FALSE) %>% mutate(key = norm_key(town), county = toupper(trimws(county))) %>% select(key, county)
unincorporated <- tribble(~town, ~county,
  "Hale's Location", "CARROLL", "Hart's Location", "CARROLL", "Kilkenny", "COOS", "Atkinson & Gilmanton Academy Grant", "COOS", "Bean's Grant", "COOS", "Bean's Purchase", "COOS",
  "Chandler's Purchase", "COOS", "Crawford's Purchase", "COOS", "Cutt's Grant", "COOS", "Dix's Grant", "COOS", "Erving's Location", "COOS", "Green's Grant", "COOS",
  "Hadley's Purchase", "COOS", "Low & Burbank's Grant", "COOS", "Martin's Location", "COOS", "Pinkham's Grant", "COOS", "Sargent's Purchase", "COOS",
  "Thompson & Meserve's Purchase", "COOS", "Thompson & Meserve's Pur.", "COOS", "Wentworth's Location", "COOS") %>% mutate(key = norm_key(town)) %>% select(key, county)
town_lookup <- bind_rows(nh_tc2, unincorporated) %>% distinct(key, .keep_all = TRUE)
suffix_of <- function(cand) tolower(str_trim(str_extract(cand, "(?<=,)\\s*[A-Za-z]+\\s*$")))
hist_party <- function(cand) { sf <- suffix_of(cand); case_when(sf == "r" ~ "REP", sf == "d" ~ "DEM", TRUE ~ "OTHER") }
parse_sheet <- function(f, sheet) {
  x <- read_excel(f, sheet = sheet, col_names = FALSE, col_types = "text", .name_repair = "minimal"); names(x) <- paste0("V", seq_len(ncol(x)))
  hdr <- which(apply(x, 1, function(r) any(grepl(",\\s*(r|d|l|lib|ind|ci|i|u|c)\\s*$", r, ignore.case = TRUE))))[1]
  h <- as.character(unlist(x[hdr, ])); tot <- which(grepl("^totals?$", trimws(x$V1), ignore.case = TRUE))[1]; stopifnot(!is.na(hdr), !is.na(tot))
  cols <- which(!is.na(h) & seq_along(h) > 1); body <- x[(hdr + 1):(tot - 1), ]
  body <- body %>% filter(!is.na(V1), !grepl("^[0-9.]+$", trimws(V1)), !grepl("^Page \\d+ of \\d+$", trimws(V1)))
  body <- body %>% mutate(across(all_of(paste0("V", cols)), ~ if_else(grepl("^\\s*[-–—]+\\s*$", .x), "0", .x)))
  votes_raw <- body[, paste0("V", cols), drop = FALSE]
  is_bad <- apply(votes_raw, 1, function(r) any(!is.na(r) & is.na(suppressWarnings(as.numeric(gsub(",", "", r)))))); body <- body[!is_bad, ]
  long <- body %>% pivot_longer(all_of(paste0("V", cols)), names_to = "col", values_to = "v") %>%
    mutate(cand = h[as.integer(sub("V", "", col))], votes = coalesce(suppressWarnings(as.numeric(gsub(",", "", v))), 0)) %>% transmute(town_raw = trimws(V1), cand, votes)
  printed <- setNames(suppressWarnings(as.numeric(gsub(",", "", unlist(x[tot, cols])))), h[cols])
  chk <- long %>% group_by(cand) %>% summarise(s = sum(votes), .groups = "drop") %>% mutate(printed = printed[cand]) %>% filter(!is.na(printed))
  if (any(chk$s != chk$printed)) { print(chk); stop("column sums != printed totals: ", f, " / ", sheet) }
  long
}
specs <- tribble(~year, ~district, ~file, ~sheet, 2000, 1, "2000_cd12.xls", "congress1", 2000, 2, "2000_cd12.xls", "congress2", 2006, 1, "2006_cd12.xls", "rcongress1",
                 2006, 2, "2006_cd12.xls", "rcongress2", 2010, 1, "2010_cd1.xls", "congress1", 2010, 2, "2010_cd2.xls", " congress2")
tv <- purrr::pmap_dfr(specs, function(year, district, file, sheet) parse_sheet(file.path(ASSET, file), sheet) %>% mutate(year = year, district = district))
t96 <- read_csv(file.path(RAW, "nh_1996_town_votes.csv"), show_col_types = FALSE)
tv <- bind_rows(tv, t96 %>% transmute(town_raw, cand, votes, year, district)) %>% mutate(key = norm_key(town_raw)) %>% left_join(town_lookup, by = "key")
stopifnot(!anyNA(tv$county))
lab <- function(cand) { sf <- suffix_of(cand); case_when(sf == "r" ~ "Republican", sf == "d" ~ "Democratic", sf %in% c("l", "lib") ~ "Libertarian", sf %in% c("i", "ind", "ia") ~ "Independent", is.na(sf) ~ NA_character_, TRUE ~ toupper(sf)) }
d2 <- tv %>% mutate(party_group = hist_party(cand), party = lab(cand), candidate = str_trim(sub(",\\s*[A-Za-z]+\\s*$", "", cand))) %>%
  inner_join(nh_fips, by = c("county" = "county_name")) %>% transmute(year, county_fips, district, candidate, party, party_group, votes)
long2 <- finalize_long(d2, "nh_historical"); save_long(long2, "he_nh_historical")
res2 <- check_long_vs_source(long2, file.path(OUTPUT_DIR, "elect_he_cty_nh_historical.rds")); print(res2)
message("NH: rows ", nrow(long1), " / ", nrow(long2), "; NA district ", sum(is.na(long1$district)), " / ", sum(is.na(long2$district)))
print(as.data.frame(long2 %>% count(candidate, party, party_group) %>% filter(candidate %in% c("Scatter", "Sununu", "Lamirande", "Kendel"))))
