## Utah 2012, 2014 U.S. House: the state's OWN official canvass workbooks (vote.utah.gov/historical-election-results/,
## "2012-General-Canvass-Report.xls" / "2014-General-Canvass-Report-1-1.xlsx"), same archive as 2008/2010 (01bm) and 1990-2006 (02ae/02ad).
## Replaces the OpenElections-sourced 01aw/02w_house_long_utah.R build, which only had 15/29 (2012) and 22/29 (2014) counties -- a genuine
## per-county-file gap in that repo, not a parsing issue. The state's own canvass has full 29/29 both years.
##
## Layout is IDENTICAL in shape to 2008/2010 (row 1 = district name in a merged/forward-filled cell, row 2 = "Candidate Name (Party)", county
## rows, then TOTAL / OFFICE SUM / PERCENTAGE rows -- not trusted, county rows are summed and checked against OFFICE SUM instead). Utah had 4
## U.S. House districts both years (redistricted after the 2010 census, up from 3 in 1990-2010). Party format changes between the two years
## (2012: single letters in parens "(R)"/"(D)"/"(C)"/"(U)"/"(L)"; 2014: full/abbreviated words "(REP)"/"(DEM)"/"(IAP)"/"(LIB)"/"(CON)"/
## "(Unaffiliated)"/"(Write-in)") -- handled with one prefix rule (starts with R -> REP, starts with D -> DEM, else OTHER) that works for both,
## the same convention already used for Utah's other eras (01aw/02w_house_long_utah.R, 01bm).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readxl)

DIR <- file.path(PROJECT_ROOT, "R", "data", "county_house_files", "utah")
xw <- readr::read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  filter(state == "UTAH") %>% distinct(county_fips, county_name) %>% mutate(county_name = toupper(trimws(county_name)))
stopifnot(nrow(xw) == 29)
fips_of_county <- function(z) xw$county_fips[match(toupper(trimws(z)), xw$county_name)]

num <- function(x) suppressWarnings(as.numeric(gsub(",", "", as.character(x))))
party_group_of <- function(code) { x <- toupper(trimws(code)); case_when(startsWith(x, "D") ~ "DEM", startsWith(x, "R") ~ "REP", TRUE ~ "OTHER") }

parse_year <- function(path, sheet, year) {
  raw <- read_excel(path, sheet = sheet, col_names = FALSE)
  district_hdr <- as.character(unlist(raw[1, ]))                              # row 1 = district name, merged cell -> only the first column of each block is non-NA
  for (i in seq_along(district_hdr)) if (is.na(district_hdr[i]) && i > 1) district_hdr[i] <- district_hdr[i - 1]
  cand_hdr <- as.character(unlist(raw[2, ]))
  ## candidate columns are >4 (1=COUNTY,2=REGISTERED,3=CAST,4=PERCENT) with a real "District N" header and a real candidate cell
  is_cand_col <- seq_along(cand_hdr) > 4 & !is.na(district_hdr) & grepl("^U\\.S\\. House District [0-9]+$", district_hdr) & !is.na(cand_hdr)
  cand_cols <- which(is_cand_col)
  m <- regexec("^(.*?)\\s*\\(([^()]+)\\)\\s*$", cand_hdr[cand_cols])
  parts <- regmatches(cand_hdr[cand_cols], m)
  stopifnot(all(lengths(parts) == 3))
  candidate <- trimws(vapply(parts, `[`, "", 2)); party_code <- trimws(vapply(parts, `[`, "", 3))
  district <- norm_district(sub("^U\\.S\\. House District ([0-9]+)$", "\\1", district_hdr[cand_cols]))

  total_row <- which(toupper(trimws(as.character(raw[[1]]))) == "TOTAL")[1]
  officesum_row <- which(toupper(trimws(as.character(raw[[1]]))) == "OFFICE SUM")[1]
  stopifnot(!is.na(total_row), !is.na(officesum_row))
  county_rows <- 3:(total_row - 1)

  county_vals <- sapply(cand_cols, function(cc) num(as.character(unlist(raw[county_rows, cc]))))
  if (is.null(dim(county_vals))) county_vals <- matrix(county_vals, ncol = length(cand_cols))
  county_names <- toupper(trimws(as.character(unlist(raw[county_rows, 1]))))
  ## OFFICE SUM is printed ONCE per district, in that district's FIRST candidate column, as the district's total votes cast for the office --
  ## not a per-candidate total. Tie check: within each district, all its candidate columns must sum to that one printed value.
  office_sum_row_vals <- num(as.character(unlist(raw[officesum_row, cand_cols])))
  colsum <- colSums(county_vals, na.rm = TRUE)
  district_total <- ave(colsum, district, FUN = sum)
  printed_total <- ave(office_sum_row_vals, district, FUN = function(x) x[!is.na(x)][1])
  tie <- all(abs(district_total - printed_total) < 1e-6)
  message(year, ": every district's candidate columns sum to its own printed OFFICE SUM: ", tie)
  stopifnot(tie)

  fp <- fips_of_county(county_names); stopifnot(!anyNA(fp))
  out <- do.call(rbind, lapply(seq_along(cand_cols), function(k) {
    v <- county_vals[, k]; keep <- !is.na(v)
    if (!any(keep)) return(NULL)
    data.frame(year = year, county_fips = fp[keep], district = district[k], candidate = candidate[k], party = party_code[k], votes = v[keep])
  }))
  out
}

y2012 <- parse_year(file.path(DIR, "2012-General-Canvass-Report.xls"), "U.S. House", 2012)
y2014 <- parse_year(file.path(DIR, "2014-General-Canvass-Report.xlsx"), "U.S. House", 2014)
raw <- bind_rows(y2012, y2014) %>% mutate(party_group = party_group_of(party))
stopifnot(!anyDuplicated(raw[, c("year", "county_fips", "district", "candidate", "party")]))
cat("party labels:", paste(names(table(raw$party)), table(raw$party), collapse = "; "), "\n")

for (y in c(2012, 2014)) {
  long <- finalize_long(raw %>% filter(year == y) %>% select(year, county_fips, district, candidate, party, party_group, votes), paste0("ut_", y))
  save_long(long, paste0("he_ut_", y))
  shares <- derive_shares(long) %>% transmute(state = "UTAH", year, cty_fips, sample, demovote, repuvote, totalvote)
  saveRDS(shares, file.path(OUTPUT_DIR, sprintf("elect_he_cty_ut_%d.rds", y)))
  r <- check_long_vs_source(long, file.path(OUTPUT_DIR, sprintf("elect_he_cty_ut_%d.rds", y))); stopifnot(all(r$pass))
  message(y, ": ", nrow(shares), " of 29 UT counties")
}

sanity <- bind_rows(lapply(c(2012, 2014), function(y) readRDS(file.path(OUTPUT_DIR, sprintf("elect_he_cty_ut_%d.rds", y)))))
share <- sanity$repuvote + sanity$demovote
message("Sanity: repuvote+demovote range [", round(min(share), 3), ", ", round(max(share), 3), "]")

## ---- independent check: district winners should match known Utah political history ---------------------------------------------------------
for (y in c(2012, 2014)) {
  l <- readRDS(file.path(LONG_DIR, sprintf("he_ut_%d.rds", y)))
  win <- l %>% group_by(district, candidate) %>% summarise(votes = sum(votes), .groups = "drop") %>% group_by(district) %>% slice_max(votes, n = 1) %>% ungroup()
  message(y, " district winners: ", paste(sprintf("D%s=%s", win$district, win$candidate), collapse = "; "))
}
