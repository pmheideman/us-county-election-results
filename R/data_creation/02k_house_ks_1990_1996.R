## Kansas U.S. House, county level, general elections 1990, 1992, 1994, 1996, from the official Kansas Secretary of State election-statistics books
## (R/data/county_house_files/KS_1990.pdf ... KS_1996.pdf: scanned books with an OCR text layer; section "U.S. House of Representatives", one table per district).
## The OCR text was parsed with R/data/raw_house_county_open_states/kansas_official/ks_parse_ocr.py + ks_build_transcription.py, and every value the OCR got wrong or
## dropped was corrected by READING THE PAGE IMAGE (ks_manual_corrections.py; log: R/output/ks_ocr_corrections_1990_1996.csv, 31 cell corrections in 18 county rows).
## The result is ks_1990_1996_transcription.csv (one row per county x district x candidate); this script rebuilds the long tables and shares from it and re-runs the checks:
##   (a) county sums == the printed 'Total' row of the district, for every candidate (all 17 districts except 1992 district 1, see below);
##   (b) district vote == the district total on the book's overview page 'Total vote for federal and state offices' (1990, 1992, 1994; the 1996 book has no such table);
##   (c) county count / split counties; (d) House total vs the presidential total of the panel (sanity only).
## 1992 district 1: the book's printed total row (94,165 / 37,826 / 3,286, also copied to the overview page as 135,277) is only the sum of the district's SECOND page, so
##   the check there is: page-2 rows (Morris .. Wichita) == that subtotal exactly, and page-1 rows were read from the image.
## Split counties (one row per district, "(pt)" in the book): Marion (D1/D4) and Douglas (D2/D3) in 1992, 1994 and 1996; none in 1990 (five districts, no split).
## Outputs (new files): R/output/long/he_ks_<year>.rds, R/output/elect_he_cty_ks_<year>.rds.
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "kansas_official")
tr  <- read_csv(file.path(DIR, "ks_1990_1996_transcription.csv"), show_col_types = FALSE)
pt  <- read_csv(file.path(DIR, "ks_1990_1996_printed_totals.csv"), show_col_types = FALSE)
ov  <- read_csv(file.path(DIR, "ks_1990_1996_overview_totals.csv"), show_col_types = FALSE)
stopifnot(!anyNA(tr$votes), !anyDuplicated(tr[, c("year", "district", "county_fips", "candidate")]))

## (a) printed district totals
a <- tr %>% group_by(year, district, candidate, party) %>% summarise(county_sum = sum(votes), .groups = "drop") %>% inner_join(pt, by = c("year", "district", "candidate", "party"))
cat("(a) printed district Total rows compared:", nrow(a), "candidate totals in", nrow(distinct(a, year, district)), "districts; tie exactly:", sum(a$county_sum == a$printed_total), "\n"); stopifnot(all(a$county_sum == a$printed_total))
## 1992 district 1: page-2 subtotal
p2 <- tr %>% filter(year == 1992, district == 1, county %in% c("Morris","Morton","Ness","Norton","Osborne","Ottawa","Pawnee","Phillips","Pratt","Rawlins","Reno","Republic","Rice","Rooks","Rush","Russell","Saline","Scott","Seward","Sheridan","Sherman","Smith","Stafford","Stanton","Stevens","Thomas","Trego","Wabaunsee","Wallace","Washington","Wichita")) %>%
  group_by(candidate) %>% summarise(v = sum(votes)); print(as.data.frame(p2)); stopifnot(identical(as.numeric(p2$v[match(c("Pat Roberts","Duane West","Steven A. Rosile"), p2$candidate)]), c(94165, 37826, 3286)))
## (b) overview
b <- tr %>% group_by(year, district) %>% summarise(v = sum(votes), .groups = "drop") %>% inner_join(ov, by = c("year", "district"))
b <- b %>% mutate(ok = v == overview_total); cat("(b) overview totals compared:", nrow(b), "districts; equal:", sum(b$ok), "\n"); print(as.data.frame(b %>% filter(!ok)))
stopifnot(all(b$ok[!(b$year == 1992 & b$district == 1)]))          # 1992 D1: the overview repeats the book's page-2 subtotal (135,277), see header
## (c) coverage
cat("(c) counties per district-year:\n"); print(as.data.frame(tr %>% distinct(year, district, county_fips, split_county_part) %>% count(year, district, name = "counties") %>% tidyr::pivot_wider(names_from = district, values_from = counties)))
cat("split counties (rows marked (pt)):", paste(unique(tr$county[tr$split_county_part == 1]), collapse = ", "), "\n")

PARTY_GROUP <- c(Democratic = "DEM", Republican = "REP")
for (y in c(1990L, 1992L, 1994L, 1996L)) {
  raw <- tr %>% filter(year == y) %>% transmute(year, county_fips, district = sprintf("%02d", district), candidate, party, party_group = ifelse(party %in% names(PARTY_GROUP), PARTY_GROUP[party], "OTHER"), votes)
  long <- finalize_long(raw, paste0("ks_", y)); save_long(long, paste0("he_ks_", y))
  shares <- derive_shares(long) %>% transmute(state = "KANSAS", year, cty_fips, sample, demovote, repuvote, totalvote)
  saveRDS(shares, file.path(OUTPUT_DIR, sprintf("elect_he_cty_ks_%d.rds", y)))
  r <- check_long_vs_source(long, file.path(OUTPUT_DIR, sprintf("elect_he_cty_ks_%d.rds", y))); stopifnot(all(r$pass))
  cat(y, ": counties", n_distinct(long$county_fips), "| candidates", n_distinct(long$candidate), "| county-districts", nrow(distinct(long, county_fips, district)), "\n")
  ## (d) sanity vs the presidential total of the panel (same year for 1992/1996; nearest presidential year otherwise)
  pe <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")) %>% filter(sample == "PE", year == c(`1990` = 1992, `1992` = 1992, `1994` = 1996, `1996` = 1996)[[as.character(y)]], cty_fips %/% 1000 == 20) %>% select(cty_fips, pe = totalvote)
  z <- shares %>% left_join(pe, by = "cty_fips") %>% mutate(ratio = totalvote / pe)
  cat("   (d) House / presidential total per county: n", sum(!is.na(z$ratio)), "min", round(min(z$ratio, na.rm = TRUE), 2), "median", round(median(z$ratio, na.rm = TRUE), 2), "max", round(max(z$ratio, na.rm = TRUE), 2), "\n")
}
