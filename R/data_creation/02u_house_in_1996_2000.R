## Indiana U.S. House by county, general elections 1996, 1998 and 2000, from the Indiana Secretary of State's election reports (image-only scans from the Indiana State Library:
## R/data/indiana_sos_reports/indiana_election_report_{1996,1998,2000}.pdf; U.S. Representative pages: 1996 PDF pp.35-37, 1998 pp.39-41, 2000 pp.59-61).
## One table per district (10 districts), county rows with one column per candidate, and a printed TOTALS row; a county in two districts appears in both (its part of the vote).
##   1996: printed with commas, OCR'd (tesseract 300 dpi, in_ocr_parse.py) and corrected from image crops: R/data/raw_house_county_open_states/indiana_official/in_1996_ocr_*.csv,
##         in_1996_corrections.csv (every hand fix with the reason), in_1996_printed_totals.csv (TOTALS cells that OCR could not read), in_1996_manual_header.csv (names, parties).
##   1998, 2000: shaded / white-on-black tables OCR poorly, so every cell was READ FROM THE PAGE IMAGES: in_{1998,2000}_manual_rows.csv, _manual_totals.csv, _manual_header.csv.
## Checks: county sums == printed TOTALS for every candidate column (all 30 district-years tie); county count per district; party totals vs Wikipedia; House total vs presidential total (1996, 2000)
## or the U.S. Senate total (1998) in the panel.
## Outputs: R/output/long/he_in_<year>.rds, R/output/elect_he_cty_in_<year>.rds (NEW files) and R/output/in_ocr_corrections_1996_2000.csv (log of hand corrections).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "indiana_official")
xw <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  filter(state == "INDIANA", !is.na(county_fips)) %>% distinct(county_name, county_fips)
norm <- function(z) gsub("[^A-Z]", "", toupper(z)); xw <- xw %>% mutate(key = norm(county_name)) %>% distinct(key, county_fips); stopifnot(nrow(xw) == 92)
COUNTY_FIX <- c("JACKON" = "JACKSON", "BOONES" = "BOONE")            # OCR misspellings in 1996
PARTY <- c(D = "Democratic", R = "Republican", L = "Libertarian", G = "Green", I = "Independent", W = "Write-In", GW = "Green (Write-In)", IW = "Independent (Write-In)", NP = "Non-Partisan")
## Wikipedia infobox party totals (statewide R and D popular votes) for a check
wp_party <- function(y) { t <- readLines(file.path(PROJECT_ROOT, "R/data/raw_election/wikipedia_house", sprintf("%d_United_States_House_of_Representatives_elections_in_Indiana.txt", y)), warn = FALSE)
  pv <- gsub("[^0-9]", "", sub(".*=", "", grep("^\\| *popular_vote[12] *=", t, value = TRUE))); c(pv[1], pv[2]) }
corr_log <- list()

load_year <- function(y) {
  hdr <- read_csv(file.path(DIR, sprintf("in_%d_manual_header.csv", y)), show_col_types = FALSE, col_types = cols(district = "i", col = "i", name = "c", party = "c"))
  if (y == 1996) {
    rows <- read_csv(file.path(DIR, "in_1996_ocr_rows.csv"), show_col_types = FALSE) %>% transmute(district, county, col, votes = votes_ocr)
    co <- read_csv(file.path(DIR, "in_1996_corrections.csv"), show_col_types = FALSE)
    rows <- rows %>% mutate(county = ifelse(norm(county) %in% names(COUNTY_FIX), tools::toTitleCase(tolower(COUNTY_FIX[norm(county)])), county))
    key <- function(d) paste(d$district, norm(d$county), d$col)
    old <- rows$votes[match(key(co), key(rows))]
    corr_log[["1996"]] <<- co %>% transmute(year = 1996, district, county, candidate = hdr$name[match(paste(district, col), paste(hdr$district, hdr$col))], ocr_value = old, corrected_value = votes, how_verified = how)
    rows <- rows %>% filter(!key(rows) %in% key(co)) %>% bind_rows(co %>% select(district, county, col, votes))
    ptot <- bind_rows(read_csv(file.path(DIR, "in_1996_ocr_totals.csv"), show_col_types = FALSE) %>% transmute(district, col, total = total_ocr),
                      read_csv(file.path(DIR, "in_1996_printed_totals.csv"), show_col_types = FALSE) %>% select(district, col, total))
    ptot <- ptot %>% group_by(district, col) %>% summarise(total = last(total), .groups = "drop")      # manual entries (later rows) override unreadable OCR totals
  } else {
    rows <- read_csv(file.path(DIR, sprintf("in_%d_manual_rows.csv", y)), show_col_types = FALSE)
    ptot <- read_csv(file.path(DIR, sprintf("in_%d_manual_totals.csv", y)), show_col_types = FALSE)
  }
  list(rows = rows %>% mutate(year = y), hdr = hdr, ptot = ptot)
}

for (y in c(1996, 1998, 2000)) {
  L <- load_year(y); rows <- L$rows; hdr <- L$hdr; ptot <- L$ptot
  stopifnot(!anyDuplicated(rows[, c("district", "county", "col")]))
  ## ---- verification 1: county sums == printed totals, every candidate column
  cs <- rows %>% group_by(district, col) %>% summarise(sum = sum(votes), n_cty = n_distinct(county), .groups = "drop") %>% left_join(ptot, by = c("district", "col")) %>% mutate(ok = sum == total)
  cat("\n=====", y, ": candidate columns:", nrow(cs), "| county sums equal the printed TOTALS:", sum(cs$ok, na.rm = TRUE), "\n"); print(as.data.frame(cs %>% filter(!ok | is.na(ok))))
  stopifnot(all(cs$ok), nrow(cs) == nrow(hdr))
  cat("counties per district:", paste(cs$n_cty[cs$col == 1], collapse = " "), "\n")
  ## ---- names, FIPS, long table
  rows <- rows %>% mutate(county_fips = xw$county_fips[match(norm(county), xw$key)]); stopifnot(!anyNA(rows$county_fips))
  raw <- rows %>% left_join(hdr, by = c("district", "col")) %>%
    transmute(year = y, county_fips, district = sprintf("%02d", district), candidate = name, party = PARTY[party], party_group = case_when(party == "Democratic" ~ "DEM", party == "Republican" ~ "REP", TRUE ~ "OTHER"), votes)
  stopifnot(!anyNA(raw$party), !anyNA(raw$candidate))
  long <- finalize_long(raw, paste0("in_", y))
  ## finalize_long strips punctuation from names: restore the printed names
  nm <- setNames(hdr$name, gsub("[^a-z]", "", tolower(hdr$name))); k <- gsub("[^a-z]", "", tolower(long$candidate)); hit <- k %in% names(nm)
  long$candidate[hit] <- nm[k[hit]]
  save_long(long, sprintf("he_in_%d", y))
  shares <- derive_shares(long) %>% transmute(state = "INDIANA", year, cty_fips, sample, demovote, repuvote, totalvote); saveRDS(shares, file.path(OUTPUT_DIR, sprintf("elect_he_cty_in_%d.rds", y)))
  print(check_long_vs_source(long, file.path(OUTPUT_DIR, sprintf("elect_he_cty_in_%d.rds", y))) %>% select(source, keys_source, matched, mismatched, pass))
  cat("counties:", n_distinct(long$county_fips), " split counties:", sum(long %>% group_by(county_fips) %>% summarise(d = n_distinct(district)) %>% pull(d) > 1), "| candidates:", nrow(hdr), "\n")
  ## ---- verification 2: Wikipedia party totals
  w <- wp_party(y); mine <- c(sum(long$votes[long$party_group == "REP"]), sum(long$votes[long$party_group == "DEM"]))
  cat("Republican / Democratic votes: ours", mine, "| Wikipedia", w, "\n")
  ## ---- verification 3: House total vs presidential total (1996, 2000) or U.S. Senate total (1998) per county
  pan <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")); ref <- pan %>% filter(sample == ifelse(y == 1998, "SE", "PE"), year == y, cty_fips %/% 1000 == 18) %>% select(cty_fips, ref = totalvote)
  r <- shares %>% left_join(ref, by = "cty_fips") %>% mutate(ratio = totalvote / ref)
  cat("reference counties:", sum(!is.na(r$ref)), "| House / reference total: min", round(min(r$ratio, na.rm = TRUE), 3), "median", round(median(r$ratio, na.rm = TRUE), 3), "max", round(max(r$ratio, na.rm = TRUE), 3), "\n")
  print(as.data.frame(r %>% filter(ratio < 0.85 | ratio > 1.1) %>% select(cty_fips, totalvote, ref, ratio)))
}
write.csv(bind_rows(corr_log), file.path(OUTPUT_DIR, "in_ocr_corrections_1996_2000.csv"), row.names = FALSE)
