## Kansas U.S. House, county level, 2006, 2008, 2010, from the official Kansas Secretary of State "Election Statistics" books:
##   R/data/county_house_files/KS_2006.pdf (pp. 90-93), KS_2008.pdf (pp. 108-111), KS_2010.pdf (pp. 102-106); KS_p16884coll129_82.pdf is a byte-identical copy of KS_2010.pdf.
## The books are scans with an OCR text layer (thousands separators come out as "." and some words are split, "M arion"). Section "U.S. House of Representatives": one table per
## district (1-4), header = candidate names + party, one row per county, "Total" row. Text was extracted with `pdftotext -layout`, parsed (columns assigned by position) and saved as
## R/data/raw_house_county_open_states/kansas_official/ks_<year>_transcription.csv (+ ks_<year>_printed_totals.csv = the printed "Total" rows). This script reads those files.
## Split counties: Kansas counties are listed once per district they lie in (2006: Geary in D1+D2, Douglas D2+D3, Franklin, Anderson, Miami ... see output); shares sum them.
## Checks (all printed at the end): county sums == printed Total row per candidate; district totals == the "Total Vote for Federal and State Offices" page (overview, embedded below);
## all 105 counties covered; House total vs the panel's Senate/president total per county. Exceptions are listed in R/output/ks_ocr_corrections_2006_2010.csv.
## Outputs (new files): R/output/long/he_ks_<year>.rds, R/output/elect_he_cty_ks_<year>.rds, R/output/ks_ocr_corrections_2006_2010.csv
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "kansas_official")
xw <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  filter(state == "KANSAS", !is.na(county_fips)) %>% distinct(county_name, county_fips)
norm <- function(z) gsub("[^A-Z]", "", toupper(z)); xw <- xw %>% mutate(key = norm(county_name)) %>% distinct(key, county_fips); stopifnot(nrow(xw) == 105)

## district totals from "Total Vote for Federal and State Offices" (2006 p.17, 2008 p.18, 2010 p.18 of the books)
overview <- tribble(~year, ~district, ~total,
  2006, "01", 199378, 2006, "02", 225562, 2006, "03", 236980, 2006, "04", 183207,
  2008, "01", 262027, 2008, "02", 307308, 2008, "03", 358858, 2008, "04", 280109,
  2010, "01", 192886, 2010, "02", 205975, 2010, "03", 233285, 2010, "04", 203383)

panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds"))
corr <- list(); summ <- list()
for (y in c(2006L, 2008L, 2010L)) {
  t <- read_csv(file.path(DIR, sprintf("ks_%d_transcription.csv", y)), col_types = cols(district = "c", .default = "c", votes = "d")) %>%
    mutate(district = sprintf("%02d", as.integer(district)), county_fips = xw$county_fips[match(norm(county), xw$key)])
  pt <- read_csv(file.path(DIR, sprintf("ks_%d_printed_totals.csv", y)), col_types = cols(district = "c", .default = "c", printed_total = "d")) %>% mutate(district = sprintf("%02d", as.integer(district)))
  stopifnot(!anyNA(t$county_fips), !anyDuplicated(t[, c("district", "county_fips", "candidate")]), all(t$votes >= 0))
  ## check 1: county sums == printed Total row, per candidate
  s <- t %>% group_by(district, candidate) %>% summarise(county_sum = sum(votes), n_cty = n_distinct(county_fips), .groups = "drop") %>% left_join(pt %>% select(district, candidate, printed_total), by = c("district", "candidate")) %>% mutate(diff = county_sum - printed_total)
  ## check 2: district total == overview page
  o <- s %>% group_by(district) %>% summarise(county_total = sum(county_sum), printed_total = sum(printed_total), n_cty = first(n_cty), .groups = "drop") %>% left_join(overview %>% filter(year == y) %>% select(district, overview = total), by = "district")
  cat("\n==== ", y, " ====\ncandidate columns: county sum vs printed Total row (non-zero diff listed):\n"); print(as.data.frame(s %>% filter(diff != 0)))
  cat("district totals (county sum / printed Total row / overview page):\n"); print(as.data.frame(o))
  cat("counties covered:", n_distinct(t$county_fips), "of 105; split counties:", sum((t %>% distinct(district, county_fips) %>% count(county_fips))$n > 1), "\n")
  summ[[as.character(y)]] <- list(s = s, o = o)
  ## check 4: House total vs Senate/President total in the panel (counties in one district only: expect ratio ~0.9-1.02)
  cty <- t %>% group_by(county_fips) %>% summarise(house = sum(votes), nd = n_distinct(district), .groups = "drop")
  pn <- panel %>% filter(sample %in% c("SE", "PE"), year == ifelse(y == 2006, 2006, y), cty_fips %/% 1000 == 20) %>% group_by(cty_fips) %>% summarise(ref = max(totalvote), .groups = "drop")
  r <- cty %>% left_join(pn, by = c("county_fips" = "cty_fips")) %>% mutate(ratio = house / ref) %>% filter(nd == 1)
  cat("House / reference (Senate or president) total, single-district counties: min", round(min(r$ratio, na.rm = TRUE), 3), "median", round(median(r$ratio, na.rm = TRUE), 3), "max", round(max(r$ratio, na.rm = TRUE), 3), "\n")
  print(as.data.frame(r %>% filter(ratio < 0.85 | ratio > 1.05) %>% select(county_fips, house, ref, ratio)))

  ## long table + shares
  raw <- t %>% transmute(year = y, county_fips, district, candidate, party, party_group = case_when(party == "Democratic" ~ "DEM", party == "Republican" ~ "REP", TRUE ~ "OTHER"), votes)
  long <- finalize_long(raw, paste0("ks_", y)); save_long(long, paste0("he_ks_", y))
  shares <- derive_shares(long) %>% transmute(state = "KANSAS", year, cty_fips, sample, demovote, repuvote, totalvote)
  saveRDS(shares, file.path(OUTPUT_DIR, sprintf("elect_he_cty_ks_%d.rds", y)))
  print(check_long_vs_source(long, file.path(OUTPUT_DIR, sprintf("elect_he_cty_ks_%d.rds", y))) %>% select(source, keys_source, matched, mismatched, pass))
}

## exceptions log (no digit was corrected by hand: every district ties except the one below; page images of 2008 pp.108-109 were read and agree with the text layer)
corr <- tibble(year = 2008L, district = "01", county = NA_character_, ocr_value = "Moran county sum 214,278", corrected_value = "not corrected (printed Total row and overview page: 214,549; difference 271)",
  how_verified = "All 69 county rows of pp.108-109 compared with the page images (and a second OCR): every value agrees with the text layer; the Total row 214,549 and the overview total 262,027 = 34,771+5,562+7,145+214,549 agree with each other, so the book's county table is 271 votes short in the Moran column; the culprit county cannot be identified (Geary and Marshall have the lowest House/Senate ratios). Values kept as printed.")
write.csv(corr, file.path(OUTPUT_DIR, "ks_ocr_corrections_2006_2010.csv"), row.names = FALSE)
