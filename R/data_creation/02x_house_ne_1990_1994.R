## Nebraska U.S. House by county, general elections 1990, 1992, 1994, from the Nebraska Board of State Canvassers official reports
## (R/data/county_house_files/nebraska/zip2/1990s Books/{1990,1992,1994} General.pdf; image-only scans of dot-matrix "Abstract of Votes" pages: 1990 PDF pp.4-5, 1992 pp.8-9, 1994 pp.6-7).
## EVERY cell was read by hand from page images (250 dpi crops); tesseract was used only to locate the pages. Transcriptions: R/data/raw_house_county_open_states/nebraska_official/ne_<year>_transcription.csv.
## Each district table lists the counties of the district (Cass, Cedar/Sarpy... split counties appear in several districts with their part) and a printed TOTAL row per candidate column: all
## county sums equal the printed TOTAL exactly on first reading (nothing needed correcting: R/output/ne_ocr_corrections_1990_1994.csv holds a single 'none needed' note row).
## 1990 prints the Republican column before the Democratic one; 1992 and 1994 Democrat first. "WRITE IN" columns are aggregated write-ins (kept as party Write-In); 1990 D2 also has a named
## write-in (Jess Pritchett, 1 vote in Douglas). Nebraska has 3 districts and 93 counties.
## Outputs: R/output/long/he_ne_<year>.rds and R/output/elect_he_cty_ne_<year>.rds for 1990, 1992, 1994 (NEW files).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R")); library(readr)
DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "nebraska_official")
xw <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>% filter(state == "NEBRASKA", !is.na(county_fips)) %>% distinct(county_name, county_fips)
norm <- function(z) gsub("[^A-Z]", "", toupper(z)); xw <- xw %>% mutate(key = norm(county_name)) %>% distinct(key, county_fips); stopifnot(nrow(xw) == 93)
## printed TOTAL rows (checksums) per district: c(first column, second column, write-in, named write-in)
printed <- list(`1990` = list(`01` = c(129654, 70587, 140, 0), `02` = c(80845, 111903, 672, 1), `03` = c(98607, 94234, 103, 0)),
                `1992` = list(`01` = c(96309, 142713, 86, 0), `02` = c(119512, 113828, 32, 0), `03` = c(67457, 170857, 41, 0)),
                `1994` = list(`01` = c(70369, 117967, 214, 0), `02` = c(90750, 92516, 2044, 0), `03` = c(41943, 154919, 41, 0)))
## candidates in printed column order: (dem, rep names) ; 1990 column order is REP then DEM
cand <- list(`1990` = tribble(~district, ~c1, ~c2, "01", "Doug Bereuter", "Larry Hall", "02", "Ally Milder", "Peter Hoagland", "03", "Bill Barrett", "Sandra K. Scofield"),
             `1992` = tribble(~district, ~c1, ~c2, "01", "Gerry Finnegan", "Doug Bereuter", "02", "Peter Hoagland", "Ronald L. Staskiewicz", "03", "Lowell Fisher", "Bill Barrett"),
             `1994` = tribble(~district, ~c1, ~c2, "01", "Patrick Combs", "Doug Bereuter", "02", "Peter Hoagland", "Jon Christensen", "03", "Gil Chapin", "Bill Barrett"))
## party of column 1: 1990 = Republican (col 1), Democratic (col 2); 1992/1994 = Democratic (col 1), Republican (col 2)
col_party <- list(`1990` = c("Republican", "Democratic"), `1992` = c("Democratic", "Republican"), `1994` = c("Democratic", "Republican"))
raw_all <- list(); corr <- data.frame(year = NA, district = NA, county = NA, candidate = NA, ocr_value = NA, corrected_value = NA, how_verified = "none needed: every cell was read from the page images and every district ties exactly to the printed TOTAL row")[0, ]
for (yr in c(1990, 1992, 1994)) {
  t <- read_csv(file.path(DIR, sprintf("ne_%d_transcription.csv", yr)), col_types = cols(district = "c", county = "c", .default = "i"), show_col_types = FALSE)
  nm <- names(t); t <- t %>% rename(v1 = 3, v2 = 4, wi = 5); if (!"pritchett" %in% nm) t$pritchett <- 0L
  t <- t %>% mutate(district = sprintf("%02d", as.integer(district)), county_fips = xw$county_fips[match(norm(county), xw$key)]); stopifnot(!anyNA(t$county_fips), n_distinct(t$county_fips) == 93)
  for (d in c("01", "02", "03")) { x <- t %>% filter(district == d); p <- printed[[as.character(yr)]][[d]]
    stopifnot(sum(x$v1) == p[1], sum(x$v2) == p[2], sum(x$wi) == p[3], sum(x$pritchett) == p[4], !anyDuplicated(x$county_fips)) }
  cat(yr, ": 3 districts, county rows", nrow(t), "| distinct counties", n_distinct(t$county_fips), "| all", 3 * 4 - 3, "printed TOTAL rows reproduced exactly\n")
  cn <- cand[[as.character(yr)]]; pt <- col_party[[as.character(yr)]]
  raw_all[[as.character(yr)]] <- bind_rows(
    t %>% left_join(cn, by = "district") %>% transmute(year = yr, county_fips, district, candidate = c1, party = pt[1], votes = v1),
    t %>% left_join(cn, by = "district") %>% transmute(year = yr, county_fips, district, candidate = c2, party = pt[2], votes = v2),
    t %>% filter(wi > 0) %>% transmute(year = yr, county_fips, district, candidate = "Write-In", party = "Write-In", votes = wi),
    t %>% filter(pritchett > 0) %>% transmute(year = yr, county_fips, district, candidate = "Jess Pritchett", party = "Write-In", votes = pritchett))
}
raw <- bind_rows(raw_all) %>% mutate(party_group = case_when(party == "Democratic" ~ "DEM", party == "Republican" ~ "REP", TRUE ~ "OTHER"))      # party_group is computed BEFORE anything overwrites the party label
write.csv(data.frame(year = NA, district = NA, county = NA, candidate = NA, ocr_value = NA, corrected_value = NA, how_verified = "none needed: every cell was read from the page images and every district ties exactly to the printed TOTAL row")[0, ], file.path(OUTPUT_DIR, "ne_ocr_corrections_1990_1994.csv"), row.names = FALSE)
write.csv(data.frame(year = "1990-1994", district = "all", county = "all", candidate = "all", ocr_value = "", corrected_value = "", how_verified = "none needed: every cell read from page images (250 dpi crops); all 9 districts' county sums equal the printed TOTAL rows exactly on first reading"), file.path(OUTPUT_DIR, "ne_ocr_corrections_1990_1994.csv"), row.names = FALSE)
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds"))
cat("panel Nebraska House rows before 2008:", sum(panel$sample == "HE" & panel$cty_fips %/% 1000 == 31 & panel$year < 2008), "\n")
for (yr in c(1990, 1992, 1994)) {
  long <- finalize_long(raw %>% filter(year == yr) %>% select(year, county_fips, district, candidate, party, party_group, votes), paste0("ne_", yr))
  ## finalize_long keeps names/labels; restore the exact written names it may have re-cased
  save_long(long, paste0("he_ne_", yr))
  shares <- derive_shares(long) %>% transmute(state = "NEBRASKA", year, cty_fips, sample, demovote, repuvote, totalvote); saveRDS(shares, file.path(OUTPUT_DIR, sprintf("elect_he_cty_ne_%d.rds", yr)))
  r <- check_long_vs_source(long, file.path(OUTPUT_DIR, sprintf("elect_he_cty_ne_%d.rds", yr))); stopifnot(all(r$pass))
  ref <- panel %>% filter(sample == ifelse(yr == 1992, "PE", "SE"), year == yr) %>% select(cty_fips, ref = totalvote)
  rr <- shares %>% left_join(ref, by = "cty_fips") %>% mutate(ratio = totalvote / ref)
  cat(sprintf("%d: candidates %d | counties %d | districts %d | split counties %d | median dem %.3f rep %.3f | House / %s %d total: n %d min %.2f median %.2f max %.2f\n", yr, n_distinct(long$candidate[long$party_group != "OTHER" | long$party != "Write-In"]), n_distinct(long$county_fips), n_distinct(long$district),
      sum(long %>% group_by(county_fips) %>% summarise(d = n_distinct(district)) %>% pull(d) > 1), median(shares$demovote), median(shares$repuvote), ifelse(yr == 1992, "presidential", "Senate"), yr, sum(!is.na(rr$ratio)),
      min(rr$ratio, na.rm = TRUE), median(rr$ratio, na.rm = TRUE), max(rr$ratio, na.rm = TRUE)))
}
