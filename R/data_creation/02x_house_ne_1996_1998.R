## Nebraska 1996 and 1998 U.S. House by county, from the Nebraska Board of State Canvassers official reports (image-only scans):
##   R/data/county_house_files/nebraska/zip2/1990s Books/1996 General.pdf (PDF p.8, page rotated 90 degrees: three districts side by side) and 1998 General.pdf (PDF p.11).
## Every cell was READ BY HAND from page images (tesseract only located the page) into R/data/raw_house_county_open_states/nebraska_official/ne_1996_1998_transcription.csv;
## the printed TOTAL rows are in ne_1996_1998_printed_totals.csv. Books print candidate names but no parties: parties from Wikipedia's 1996/1998 Nebraska House pages (cached in
## R/data/raw_election/wikipedia_house/). Checks: county sums == printed TOTAL per candidate, county count per district, Wikipedia district totals, House total vs the panel's presidential total.
## Corrections: one digit misread while transcribing (1996 D1 Combs, Saline 1987 -> 1967), found by the total tie and confirmed on a 300-dpi crop: R/output/ne_ocr_corrections_1996_1998.csv.
## Outputs: R/output/long/he_ne_<year>.rds, R/output/elect_he_cty_ne_<year>.rds for 1996 and 1998 (NEW files).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R")); library(readr)
D <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "nebraska_official")
tr <- read_csv(file.path(D, "ne_1996_1998_transcription.csv"), col_types = cols(.default = "c")); pt <- read_csv(file.path(D, "ne_1996_1998_printed_totals.csv"), col_types = cols(.default = "c"))
## candidate columns per year/district: name, party label (Wikipedia; the books print no parties)
CAND <- tibble::tribble(~year, ~district, ~col, ~candidate, ~party,
  1996, "01", "c1", "Patrick J. Combs", "Democratic", 1996, "01", "c2", "Doug Bereuter", "Republican", 1996, "01", "c3", "Write-In", "Write-In",
  1996, "02", "c1", "James Martin Davis", "Democratic", 1996, "02", "c2", "Jon Christensen", "Republican", 1996, "02", "c3", "Phillip E. Torrison", "Libertarian", 1996, "02", "c4", "Patricia A. Dunn", "Natural Law", 1996, "02", "c5", "Write-In", "Write-In",
  1996, "03", "c1", "John Webster", "Democratic", 1996, "03", "c2", "Bill Barrett", "Republican", 1996, "03", "c3", "Write-In", "Write-In",
  1998, "01", "c1", "Don Eret", "Democratic", 1998, "01", "c2", "Doug Bereuter", "Republican", 1998, "01", "c3", "Write-In", "Write-In",
  1998, "02", "c1", "Michael Scott", "Democratic", 1998, "02", "c2", "Lee Terry", "Republican", 1998, "02", "c3", "Write-In", "Write-In",
  1998, "03", "c1", "Bill Barrett", "Republican", 1998, "03", "c2", "Jerry Hickman", "Libertarian", 1998, "03", "c3", "George P. Remmenga", "Write-In", 1998, "03", "c4", "Write-In", "Write-In") %>% mutate(year = as.character(year))
long_tr <- tr %>% tidyr::pivot_longer(c1:c5, names_to = "col", values_to = "votes") %>% filter(!is.na(votes)) %>% mutate(votes = as.numeric(votes)) %>% inner_join(CAND, by = c("year", "district", "col"))
stopifnot(nrow(long_tr) == sum(!is.na(as.matrix(tr[, c("c1", "c2", "c3", "c4", "c5")]))))
pt_l <- pt %>% tidyr::pivot_longer(c1:c5, names_to = "col", values_to = "printed") %>% filter(!is.na(printed)) %>% mutate(printed = as.numeric(printed))
ck <- long_tr %>% group_by(year, district, col, candidate) %>% summarise(sum = sum(votes), n_cty = n(), .groups = "drop") %>% left_join(pt_l, by = c("year", "district", "col")) %>% mutate(ok = sum == printed)
cat("candidate lines:", nrow(ck), "| county sums equal the printed TOTAL:", sum(ck$ok), "\n"); print(as.data.frame(ck)); stopifnot(all(ck$ok))
xw <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>% filter(state == "NEBRASKA", !is.na(county_fips)) %>% distinct(county_name, county_fips)
norm <- function(z) gsub("[^A-Z]", "", toupper(z)); xw <- xw %>% mutate(key = norm(county_name)) %>% distinct(key, county_fips); stopifnot(nrow(xw) == 93)
raw <- long_tr %>% mutate(county_fips = xw$county_fips[match(norm(county), xw$key)]); stopifnot(!anyNA(raw$county_fips))
raw <- raw %>% transmute(year = as.integer(year), county_fips, district, candidate, party_group = case_when(party == "Democratic" ~ "DEM", party == "Republican" ~ "REP", TRUE ~ "OTHER"), party, votes)   # party_group first
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds"))
for (y in c(1996L, 1998L)) {
  long <- finalize_long(raw %>% filter(year == y) %>% select(year, county_fips, district, candidate, party, party_group, votes), paste0("ne_", y)); save_long(long, paste0("he_ne_", y))
  shares <- derive_shares(long) %>% transmute(state = "NEBRASKA", year, cty_fips, sample, demovote, repuvote, totalvote); saveRDS(shares, file.path(OUTPUT_DIR, sprintf("elect_he_cty_ne_%d.rds", y)))
  stopifnot(all(check_long_vs_source(long, file.path(OUTPUT_DIR, sprintf("elect_he_cty_ne_%d.rds", y)))$pass))
  ref <- panel %>% filter(sample == "PE", year == ifelse(y == 1996, 1996, 1996)) %>% select(cty_fips, pe = totalvote); r <- shares %>% left_join(ref, by = "cty_fips") %>% mutate(ratio = totalvote / pe)
  cat(sprintf("%d: candidates %d | districts %d | counties %d | split counties %d | median dem %.3f rep %.3f | House / 1996 presidential total: min %.2f median %.2f max %.2f | panel NE rows: %d\n", y, n_distinct(long$candidate), n_distinct(long$district), n_distinct(long$county_fips),
      sum(long %>% group_by(county_fips) %>% summarise(d = n_distinct(district)) %>% pull(d) > 1), median(shares$demovote), median(shares$repuvote), min(r$ratio, na.rm = TRUE), median(r$ratio, na.rm = TRUE), max(r$ratio, na.rm = TRUE),
      nrow(panel %>% filter(sample == "HE", cty_fips %/% 1000 == 31, year <= 2006))))
}
