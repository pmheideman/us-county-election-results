## Kansas U.S. House, county level, 1998 / 2000 / 2002 / 2004, from the Kansas Secretary of State election-statistics books
## (R/data/county_house_files/KS_<year>.pdf; scanned books with an OCR text layer; section "U.S. House of Representatives", one table per district:
## county rows with dotted leaders, "Total" row per district, split counties (Douglas, Marion, Geary, Miami) once in each district they lie in).
## Steps: (1) `pdftotext -layout` -> raw_house_county_open_states/kansas_official/ks<year>.txt; (2) parse the county and Total rows of the House pages with an OCR-tolerant number reader
## (leader dots dropped, "5 3 0" -> 530, '%' -> 6, '£' -> 2, '°' -> 0, l/I -> 1); (3) apply hand corrections read from the page images
## (kansas_official/ks_corrections_1998_2004.csv, logged to R/output/ks_ocr_corrections_1998_2004.csv); (4) hard checks per district: county sums == printed Total row for every
## candidate, district total == the book's overview page ("Total vote for federal and state offices"), county counts; (5) long tables + shares files.
## Candidate names come from the printed column headers (embedded below). Outputs (new files): R/output/long/he_ks_<year>.rds, R/output/elect_he_cty_ks_<year>.rds.
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
YEARS <- c(1998, 2000, 2002, 2004)
DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "kansas_official"); dir.create(DIR, showWarnings = FALSE, recursive = TRUE)
## first/last page of the House section in the pdftotext output (1-based), and the book's printed "Total vote for U.S. Representatives" by district (overview page 18)
PAGES <- list(`1998` = c(105, 108), `2000` = c(108, 111), `2002` = c(106, 109), `2004` = c(111, 114))
OVERVIEW <- list(`1998` = c(189393, 178048, 197314, 162693), `2000` = c(242327, 244759, 308710, 242583), `2002` = NULL, `2004` = c(264293, 294436, 335739, 261915))
PARTY <- c(Dem = "Democratic", Rep = "Republican", Lib = "Libertarian", Ref = "Reform", Tax = "U.S. Taxpayers")
## candidates per district in printed column order (name, party abbreviation)
C <- function(...) { z <- list(...); data.frame(candidate = vapply(z, `[`, "", 1), party = vapply(z, `[`, "", 2)) }
CANDS <- list(
  `1998` = list(`1` = C(c("Jim Phillips", "Dem"), c("Jerry Moran", "Rep")), `2` = C(c("Jim Clark", "Dem"), c("Jim Ryun", "Rep")), `3` = C(c("Dennis Moore", "Dem"), c("Vince Snowbarger", "Rep")),
                `4` = C(c("Jim Lawing", "Dem"), c("Todd Tiahrt", "Rep"), c("Craig Newland", "Tax"))),
  `2000` = list(`1` = C(c("Jack Warner", "Lib"), c("Jerry Moran", "Rep")), `2` = C(c("Stanley Wiles", "Dem"), c("Dennis Hawver", "Lib"), c("Jim Ryun", "Rep")),
                `3` = C(c("Dennis Moore", "Dem"), c("Chris Mina", "Lib"), c("Phill Kline", "Rep")), `4` = C(c("Carlos Nolla", "Dem"), c("Steven A. Rosile", "Lib"), c("Todd Tiahrt", "Rep"))),
  `2002` = list(`1` = C(c("Jack Warner", "Lib"), c("Jerry Moran", "Rep")), `2` = C(c("Dan Lykins", "Dem"), c("Art Clack", "Lib"), c("Jim Ryun", "Rep")),
                `3` = C(c("Dennis Moore", "Dem"), c("Douglas Martin", "Lib"), c("Dawn Bly", "Ref"), c("Adam Taff", "Rep")), `4` = C(c("Carlos Nolla", "Dem"), c("Maike Warren", "Lib"), c("Todd Tiahrt", "Rep"))),
  `2004` = list(`1` = C(c("Jack Warner", "Lib"), c("Jerry Moran", "Rep")), `2` = C(c("Nancy Boyda", "Dem"), c("Dennis Hawver", "Lib"), c("Jim Ryun", "Rep")),
                `3` = C(c("Dennis Moore", "Dem"), c("Joe Bellis", "Lib"), c("Richard Wells", "Ref"), c("Kris Kobach", "Rep")), `4` = C(c("Michael Kinard", "Dem"), c("David Loomis", "Lib"), c("Todd Tiahrt", "Rep"))))

## ---- county names -> FIPS ----------------------------------------------------------------------------------------------------------------------------
xw <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  filter(state == "KANSAS", !is.na(county_fips)) %>% distinct(county_name, county_fips)
norm <- function(z) gsub("[^A-Z]", "", toupper(z)); xw <- xw %>% mutate(key = norm(county_name)) %>% distinct(key, county_fips); stopifnot(nrow(xw) == 105)
fips_of <- function(z) { k <- norm(sub("\\(\\s*p\\s*t\\s*\\)", "", z, ignore.case = TRUE)); h <- match(k, xw$key)
  for (i in which(is.na(h))) { d <- adist(k[i], xw$key); if (sum(d == min(d)) == 1 && min(d) <= 2) h[i] <- which.min(d) }; xw$county_fips[h] }

## ---- number reader ---------------------------------------------------------------------------------------------------------------------------------
GROUP <- "[0-9%£°lIO]+(?:[ ]?[,.]?[ ]?[0-9%£°lIO]+)*"
read_nums <- function(rest) { m <- regmatches(rest, gregexpr(GROUP, rest, perl = TRUE))[[1]]
  v <- suppressWarnings(as.numeric(gsub("[^0-9]", "", chartr("£°lIO", "20110", m)))); v[grepl("%", m)] <- NA; v }   # '%' is an unreadable digit: NA, must be corrected by hand
row_of <- function(ln) { m <- regmatches(ln, regexec("^\\s*([A-Za-z][A-Za-z '().]*?)\\s*(?:\\.{2,}|\\s{2,}|(?=[0-9]))(.*)$", ln, perl = TRUE))[[1]]
  if (!length(m)) return(NULL); nm <- trimws(m[2]); if (grepl("Office|Election|Statistics|District|Total vote|House|Senate", nm, ignore.case = TRUE)) return(NULL)
  if (!grepl("[0-9]", m[3])) return(NULL); nums <- read_nums(m[3]); if (!length(nums)) return(NULL); list(name = nm, nums = nums, raw = ln) }

## ---- parse -------------------------------------------------------------------------------------------------------------------------------------------
raw_rows <- list(); tot_rows <- list()
for (y in YEARS) {
  pdf <- file.path(PROJECT_ROOT, "R", "data", "county_house_files", sprintf("KS_%d.pdf", y)); txt <- file.path(DIR, sprintf("ks%d.txt", y))
  if (!file.exists(txt)) system2("pdftotext", c("-layout", shQuote(pdf), shQuote(txt)))
  pg <- strsplit(paste(readLines(txt, warn = FALSE, encoding = "UTF-8"), collapse = "\n"), "\f")[[1]]
  rng <- PAGES[[as.character(y)]]; dist <- 1L      # the first table of the House section is district 1 (2002 prints no "District 1" heading)
  for (p in rng[1]:rng[2]) for (ln in strsplit(pg[p], "\n")[[1]]) {
    dm <- regmatches(ln, regexec("^\\s*D\\s*i\\s*s\\s*t\\s*r\\s*i\\s*c\\s*t\\s*([1-4])\\s*$", ln))[[1]]
    if (length(dm)) { dist <- as.integer(dm[2]); next }
    if (is.na(dist)) next
    r <- row_of(ln); if (is.null(r)) next
    if (grepl("^T\\s*o\\s*t\\s*a\\s*l", r$name, ignore.case = TRUE)) tot_rows[[length(tot_rows) + 1]] <- data.frame(year = y, page = p, district = dist, nums = I(list(r$nums)), raw = ln)
    else raw_rows[[length(raw_rows) + 1]] <- data.frame(year = y, page = p, district = dist, county_raw = r$name, nums = I(list(r$nums)), raw = ln)
  }
}
rows <- bind_rows(raw_rows); tots <- bind_rows(tot_rows)
rows$county_fips <- fips_of(rows$county_raw)   # (Kansas county names in the books are matched after removing spaces and "(pt)"; OCR misspellings by nearest name)
cat("rows parsed:", nrow(rows), "| county names not matched:", sum(is.na(rows$county_fips)), "\n"); print(as.data.frame(rows %>% filter(is.na(county_fips)) %>% select(year, page, district, county_raw, raw)))
saveRDS(list(rows = rows, tots = tots), file.path(DIR, "ks_parse_stage1.rds"))

## ---- stage 2: hand corrections, cells -----------------------------------------------------------------------------------------------------------------
corr <- read_csv(file.path(DIR, "ks_corrections_1998_2004.csv"), col_types = cols(.default = "c")) %>% mutate(year = as.integer(year), district = as.integer(district), corrected_value = as.numeric(corrected_value), ckey = ifelse(county == "Total", "TOTAL", norm(county)))
rows$ckey <- norm(sub("\\(\\s*p\\s*t\\s*\\)", "", rows$county_raw, ignore.case = TRUE))
cells <- purrr::pmap_dfr(list(rows$year, rows$district, rows$county_raw, rows$county_fips, rows$ckey, rows$nums, rows$raw, rows$page), function(y, d, cr, cf, ck, nums, raw, pg) {
  cd <- CANDS[[as.character(y)]][[as.character(d)]]; ok <- length(nums) == nrow(cd)
  tibble(year = y, district = d, page = pg, county_raw = cr, county_fips = cf, ckey = ck, candidate = cd$candidate, party_abbr = cd$party,
         ocr_value = if (ok) nums else NA_real_, ocr_text = if (ok) as.character(nums) else paste0("(row unreadable: ", trimws(gsub("[. ]{3,}", " ... ", raw)), ")")) })
tot <- purrr::pmap_dfr(list(tots$year, tots$district, tots$nums), function(y, d, nums) { cd <- CANDS[[as.character(y)]][[as.character(d)]]
  tibble(year = y, district = d, candidate = cd$candidate, printed = c(nums, rep(NA_real_, nrow(cd)))[seq_len(nrow(cd))]) })
cells <- cells %>% left_join(corr %>% filter(ckey != "TOTAL") %>% select(year, district, ckey, candidate, corrected_value, how_verified), by = c("year", "district", "ckey", "candidate")) %>%
  mutate(votes = ifelse(!is.na(corrected_value), corrected_value, ocr_value))
tot <- tot %>% left_join(corr %>% filter(ckey == "TOTAL") %>% select(year, district, candidate, corrected_value, how_verified), by = c("year", "district", "candidate")) %>% mutate(printed = ifelse(!is.na(corrected_value), corrected_value, printed))
stopifnot(!anyNA(cells$votes), !anyNA(tot$printed), !anyDuplicated(cells[, c("year", "district", "county_fips", "candidate")]))
unused <- corr %>% anti_join(bind_rows(cells %>% distinct(year, district, ckey, candidate), tot %>% distinct(year, district, candidate) %>% mutate(ckey = "TOTAL")), by = c("year", "district", "ckey", "candidate")); stopifnot(nrow(unused) == 0)
write_csv(bind_rows(cells %>% filter(!is.na(corrected_value)) %>% transmute(year, district, county = county_raw, candidate, ocr_value = ocr_text, corrected_value, how_verified),
                    tot %>% filter(!is.na(corrected_value)) %>% transmute(year, district, county = "Total (printed)", candidate, ocr_value = "(cell missing in OCR)", corrected_value, how_verified)) %>% arrange(year, district, county),
          file.path(OUTPUT_DIR, "ks_ocr_corrections_1998_2004.csv"))
cat("hand corrections applied:", sum(!is.na(cells$corrected_value)), "cells +", sum(!is.na(tot$corrected_value)), "printed-total cells\n")

## ---- checks --------------------------------------------------------------------------------------------------------------------------------------
chk <- cells %>% group_by(year, district, candidate) %>% summarise(county_sum = sum(votes), n_counties = n(), .groups = "drop") %>% left_join(tot %>% select(year, district, candidate, printed), by = c("year", "district", "candidate")) %>% mutate(diff = county_sum - printed)
cat("\n(a) county sums vs printed Total rows: ", sum(chk$diff == 0), " of ", nrow(chk), " candidate columns tie exactly\n"); print(as.data.frame(chk %>% filter(diff != 0)))
stopifnot(all(chk$diff == 0))
dt <- cells %>% group_by(year, district) %>% summarise(total = sum(votes), n_counties = n_distinct(county_fips), .groups = "drop")
OV <- OVERVIEW; OV[["2002"]] <- c(208561, 210977, 219389, 190963)
dt$overview <- mapply(function(y, d) OV[[as.character(y)]][d], dt$year, dt$district)
cat("\n(b) district totals vs the overview page (Total vote for U.S. Representatives):\n"); print(as.data.frame(dt %>% mutate(ties = total == overview))); stopifnot(all(dt$total == dt$overview))
cat("\n(c) counties covered per year (105 in Kansas) and split counties:\n")
print(as.data.frame(cells %>% distinct(year, district, county_fips) %>% group_by(year, county_fips) %>% summarise(nd = n(), .groups = "drop") %>% group_by(year) %>% summarise(counties = n(), split = sum(nd > 1), split_names = paste(xw$key[match(county_fips[nd > 1], xw$county_fips)], collapse = ", "))))

## ---- (d) sanity vs the presidential vote of the nearest presidential year ---------------------------------------------------------------------------
pe <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")) %>% filter(sample == "PE", cty_fips %/% 1000 == 20) %>% select(year, cty_fips, pe = totalvote)
san <- cells %>% group_by(year, county_fips) %>% summarise(house = sum(votes), .groups = "drop") %>% mutate(py = ifelse(year == 1998, 1996, ifelse(year == 2002, 2000, year))) %>% left_join(pe, by = c("py" = "year", "county_fips" = "cty_fips")) %>% mutate(ratio = house / pe)
cat("\n(d) House total / presidential total (nearest presidential year): by year\n"); print(as.data.frame(san %>% group_by(year) %>% summarise(n = n(), n_pe = sum(!is.na(pe)), min = round(min(ratio, na.rm = TRUE), 2), median = round(median(ratio, na.rm = TRUE), 2), max = round(max(ratio, na.rm = TRUE), 2))))
print(as.data.frame(san %>% filter(!is.na(ratio), ratio < 0.5 | ratio > 1.15)))

## ---- long tables + shares files ----------------------------------------------------------------------------------------------------------------
for (y in YEARS) {
  raw <- cells %>% filter(year == y) %>% transmute(year, county_fips, district = sprintf("%02d", district), candidate, party = unname(PARTY[party_abbr]), votes,
                                                     party_group = case_when(party_abbr == "Dem" ~ "DEM", party_abbr == "Rep" ~ "REP", TRUE ~ "OTHER"))
  long <- finalize_long(raw, paste0("ks_", y)); save_long(long, paste0("he_ks_", y))
  shares <- derive_shares(long) %>% transmute(state = "KANSAS", year, cty_fips, sample, demovote, repuvote, totalvote)
  saveRDS(shares, file.path(OUTPUT_DIR, sprintf("elect_he_cty_ks_%d.rds", y)))
  print(check_long_vs_source(long, file.path(OUTPUT_DIR, sprintf("elect_he_cty_ks_%d.rds", y))) %>% select(source, keys_source, matched, mismatched, pass))
}
