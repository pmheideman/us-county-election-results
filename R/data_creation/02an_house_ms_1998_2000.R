## Mississippi U.S. House, 1998 and 2000 general elections, from a born-digital scan of the Mississippi "Official and Statistical Register"
## (despite its filename, R/data/county_house_files/MS_98-2000.pdf is the state's own title page's "2000-2004" edition, published 2001).
## Unlike the older 1990/1992 and other-era Mississippi register volumes, this one is clean typeset text (not a dot-leader scan), so every
## number was read directly from `pdftotext -layout` output rather than page images, and cross-checked against each district's own printed
## TOTAL row (source PDF pages 532-534 for 1998, all 5 districts; 675-677 for 2000, all 5 districts). Transcribed source data is cached at
## R/data/county_house_files/mississippi/transcribed/{1998,2000}.csv.
##
## District 5 in 2000 has a genuine, correctly-printed ballot oddity: TWO separate Democratic-party lines, Katie Perrone and Gene Taylor
## (both real rows, both counted as party_group DEM -- not a data error, this is how Mississippi's official return records that ballot).
## "Jeff Davis" in the source is Jefferson Davis County (aliased below); every other county name matches the project's crosswalk directly.
## Party codes: D/R map to DEM/REP; L (Libertarian), RP (Reform Party), NL (Natural Law), and Vince P. (T) Thornton's unlabeled "Taxpayers"
## line (recorded here as party code "O" for "other", matching the source, which prints no party letter for him) all map to OTHER.
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)

DIR <- file.path(PROJECT_ROOT, "R", "data", "county_house_files", "mississippi", "transcribed")
xw <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  filter(state == "MISSISSIPPI") %>% distinct(county_fips, county_name) %>% mutate(county_name = toupper(trimws(county_name)))
stopifnot(nrow(xw) == 82)

normalize_county <- function(x) {
  x <- toupper(trimws(x))
  ifelse(x == "JEFF DAVIS", "JEFFERSON DAVIS", x)
}
party_group_of <- function(code) case_when(code == "D" ~ "DEM", code == "R" ~ "REP", TRUE ~ "OTHER")

build_year <- function(year, file) {
  raw <- read_csv(file.path(DIR, file), show_col_types = FALSE) %>%
    mutate(county_name = normalize_county(county)) %>%
    left_join(xw, by = "county_name")
  stopifnot(!anyNA(raw$county_fips))

  ## tie-check: every district's candidate columns must sum exactly to that district's own printed TOTAL row (hard-coded from the source images)
  totals <- raw %>% group_by(district, candidate) %>% summarise(votes = sum(votes), .groups = "drop")
  print(as.data.frame(totals %>% arrange(district, desc(votes))))

  df <- raw %>% transmute(year = year, county_fips, district = norm_district(district), candidate, party, party_group = party_group_of(party), votes)
  stopifnot(n_distinct(raw$county_fips) == 82)
  df
}

EXPECTED <- tribble(
  ~year, ~district, ~candidate, ~total,
  1998, "01", "Rex N. Weathers", 30442, 1998, "01", "Roger F. Wicker", 66756, 1998, "01", "John A. (Andy) Rouse", 2156,
  1998, "02", "Bennie G. Thompson", 80673, 1998, "02", "William G. Chipman", 32369,
  1998, "03", "Charles W. (Chip) Pickering", 84885, 1998, "03", "C.T. Scarborough Jr.", 15474,
  1998, "04", "Ronnie Shows", 73268, 1998, "04", "Delbert Hosemann", 61566, 1998, "04", "Bill Fausek", 898, 1998, "04", "Vince P. (T) Thornton", 799, 1998, "04", "Kenneth (K.W.) Welch", 699,
  1998, "05", "Gene Taylor", 78312, 1998, "05", "Randy McDonnell", 19307, 1998, "05", "Ray E. Coffey", 1530, 1998, "05", "Bob Claunch", 1053, 1998, "05", "Philip Mayeux", 495,
  2000, "01", "Joe T. (Joey) Grist Jr.", 59763, 2000, "01", "Roger F. Wicker", 145967, 2000, "01", "Chris Lawrence", 3310,
  2000, "02", "Bennie G. Thompson", 112777, 2000, "02", "Hardy Caraway", 54090, 2000, "02", "William G. Chipman", 4305, 2000, "02", "Lee F. Dilworth", 2135,
  2000, "03", "William Clay Thrash", 54151, 2000, "03", "Charles W. (Chip) Pickering", 153899, 2000, "03", "Jonathan Golden", 2377,
  2000, "04", "Ronnie Shows", 115732, 2000, "04", "Dunn Lampton", 79218, 2000, "04", "Ernie John Hopkins", 2580, 2000, "04", "Betty Pharr", 1504,
  2000, "05", "Katie Perrone", 2820, 2000, "05", "Gene Taylor", 153264, 2000, "05", "Randy McDonnell", 35309, 2000, "05", "Wayne L. Parker", 3002
) %>% mutate(district = sprintf("%02d", as.integer(district)))

y1998 <- build_year(1998, "1998.csv")
y2000 <- build_year(2000, "2000.csv")
raw_all <- bind_rows(y1998 %>% mutate(district_raw = district), y2000 %>% mutate(district_raw = district))

chk <- raw_all %>% group_by(year, district = district_raw, candidate) %>% summarise(votes = sum(votes), .groups = "drop") %>%
  inner_join(EXPECTED, by = c("year", "district", "candidate"))
stopifnot(nrow(chk) == nrow(EXPECTED), all(chk$votes == chk$total))
message("All ", nrow(EXPECTED), " district-candidate totals tie EXACTLY to their printed source totals (1998: 5 districts, 2000: 5 districts).")

for (y in c(1998, 2000)) {
  long <- finalize_long(get(paste0("y", y)) %>% select(year, county_fips, district, candidate, party, party_group, votes), paste0("ms_", y))
  save_long(long, paste0("he_ms_", y))
  shares <- derive_shares(long) %>% transmute(state = "MISSISSIPPI", year, cty_fips, sample, demovote, repuvote, totalvote)
  saveRDS(shares, file.path(OUTPUT_DIR, sprintf("elect_he_cty_ms_%d.rds", y)))
  r <- check_long_vs_source(long, file.path(OUTPUT_DIR, sprintf("elect_he_cty_ms_%d.rds", y))); stopifnot(all(r$pass))
  message(y, ": ", nrow(shares), " of 82 MS counties")
}

sanity <- bind_rows(lapply(c(1998, 2000), function(y) readRDS(file.path(OUTPUT_DIR, sprintf("elect_he_cty_ms_%d.rds", y)))))
share <- sanity$repuvote + sanity$demovote
message("Sanity: repuvote+demovote range [", round(min(share), 3), ", ", round(max(share), 3), "]")

for (y in c(1998, 2000)) {
  l <- readRDS(file.path(LONG_DIR, sprintf("he_ms_%d.rds", y)))
  win <- l %>% group_by(district, candidate) %>% summarise(votes = sum(votes), .groups = "drop") %>% group_by(district) %>% slice_max(votes, n = 1) %>% ungroup()
  message(y, " district winners: ", paste(sprintf("D%s=%s", win$district, win$candidate), collapse = "; "))
}
