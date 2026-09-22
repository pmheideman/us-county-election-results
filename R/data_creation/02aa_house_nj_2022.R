## New Jersey 2022 U.S. House by county, from the Division of Elections' official results (Official List, 01/18/2023):
## https://www.nj.gov/state/elections/assets/pdf/election-results/2022/2022-official-general-results-us-house.pdf (saved in R/data/raw_house_county_open_states/new_jersey_doe/; text layer).
## Layout: "First Congressional District: ... Counties", then per candidate a line "NAME [(w)] [*] address ... PARTY", county rows "COUNTY PARTY tally" and "Total n".
## MEDSL's 2022 New Jersey House county rows repeat each district's total in every county (unusable); this replaces them. Checks: county rows add up to each candidate's printed Total.
## Outputs: R/output/long/he_nj_2022.rds, R/output/elect_he_cty_nj_2022.rds (NEW files).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R")); library(readr)
PDF <- file.path(PROJECT_ROOT, "R/data/raw_house_county_open_states/new_jersey_doe/2022-official-general-results-us-house.pdf")
L <- system2("pdftotext", c("-layout", shQuote(PDF), "-"), stdout = TRUE); L <- gsub("\f", "", L)
xw <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>% filter(state_po == "NJ", year == 2020, !is.na(county_fips)) %>% distinct(county_name, county_fips)
norm <- function(z) gsub("[^A-Z]", "", toupper(z)); xw <- xw %>% mutate(key = norm(county_name)); stopifnot(nrow(xw) == 21)
ORD <- c(First = 1, Second = 2, Third = 3, Fourth = 4, Fifth = 5, Sixth = 6, Seventh = 7, Eighth = 8, Ninth = 9, Tenth = 10, Eleventh = 11, Twelfth = 12)
dist <- NA; cand <- NA; rows <- list(); tots <- list(); dtots <- list(); n_since <- 0
cty_re <- paste0("^\\s{30,}(", paste(toupper(xw$county_name), collapse = "|"), ")\\s+([A-Z\"][A-Z0-9 .&'\"-]*?)\\s{2,}([0-9,]+)\\s*$")   # county names are followed by one space when long (CUMBERLAND REPUBLICAN); "OTHER" candidates print a quoted slogan instead of a party
zip_re <- ",\\s*[A-Z]{2}(\\s+[0-9]{5})?\\s*$"   # "CITY, NJ 08053"; long city names wrap the ZIP onto the next line
for (i in seq_along(L)) {
  ln <- L[i]
  m <- regmatches(ln, regexec("^(First|Second|Third|Fourth|Fifth|Sixth|Seventh|Eighth|Ninth|Tenth|Eleventh|Twelfth) Congressional District", ln))[[1]]
  if (length(m)) { dist <- sprintf("%02d", ORD[[m[2]]]); cand <- NA; next }   # a district must start with its own candidate line (a missed candidate line must not leak rows to the previous district's last candidate)
  ## a candidate line = a flush-left line whose next non-blank line (or the one after it, for two-line addresses) carries "CITY, NJ 08053"
  if (grepl("^[A-Z]", ln) && !grepl("^(Name|Candidates|For |Official)", ln)) {
    nx <- head(L[(i + 1):min(i + 6, length(L))][nzchar(trimws(L[(i + 1):min(i + 6, length(L))]))], 2)
    if (any(grepl(zip_re, nx))) {
      nm <- sub("\\s+(\\(w\\)|\\*).*$", "", ln); nm <- sub("\\s{2,}.*$", "", nm); nm <- sub("\\s+([0-9]|PO BOX|P\\.O\\.).*$", "", nm)
      cand <- trimws(gsub("\\(w\\)|\\*", "", nm)); next } }
  m <- regmatches(ln, regexec(cty_re, ln))[[1]]
  if (length(m)) { rows[[length(rows) + 1]] <- data.frame(district = dist, candidate = cand, county = m[2], party = m[3], votes = as.numeric(gsub(",", "", m[4]))); n_since <- n_since + 1; next }
  m <- regmatches(ln, regexec("^\\s+Total\\s+([0-9,]+)\\s*$", ln))[[1]]
  if (length(m)) { v <- as.numeric(gsub(",", "", m[2]))
    if (n_since > 0) tots[[length(tots) + 1]] <- data.frame(district = dist, candidate = cand, printed = v)
    else dtots[[length(dtots) + 1]] <- data.frame(district = dist, district_total = v)   # a Total with no county rows since the previous Total = the district total printed after its last candidate
    n_since <- 0 }
}
stopifnot(!anyNA(vapply(rows, function(r) r$candidate, ""))); raw <- bind_rows(rows) %>% mutate(party = ifelse(grepl("^\"", party), "OTHER", party)); tots <- bind_rows(tots)
ck <- raw %>% group_by(district, candidate) %>% summarise(sum = sum(votes), n_cty = n(), .groups = "drop") %>% left_join(tots, by = c("district", "candidate")) %>% mutate(ok = sum == printed)
cat("candidates:", nrow(ck), "| printed Total lines:", nrow(tots), "| county sums equal the printed Total:", sum(ck$ok, na.rm = TRUE), "\n"); print(as.data.frame(ck %>% filter(!ok | is.na(ok))))
dchk <- raw %>% group_by(district) %>% summarise(sum = sum(votes), .groups = "drop") %>% left_join(bind_rows(dtots), by = "district")
cat("district totals printed:", sum(!is.na(dchk$district_total)), "of", nrow(dchk), "| equal to the sum of the county rows:", sum(dchk$sum == dchk$district_total, na.rm = TRUE), "\n"); print(as.data.frame(dchk %>% filter(is.na(district_total) | sum != district_total)))
stopifnot(nrow(ck) == nrow(tots), all(ck$ok), all(dchk$sum == dchk$district_total, na.rm = TRUE))
raw <- raw %>% mutate(county_fips = xw$county_fips[match(norm(county), xw$key)]); stopifnot(!anyNA(raw$county_fips))
cat("districts:", n_distinct(raw$district), "| counties:", n_distinct(raw$county_fips), "| party labels:", paste(head(names(sort(table(raw$party), decreasing = TRUE)), 12), collapse = "; "), "\n")
raw <- raw %>% transmute(year = 2022L, county_fips, district, candidate = pretty_name(candidate), party_group = case_when(party == "DEMOCRATIC" ~ "DEM", party == "REPUBLICAN" ~ "REP", TRUE ~ "OTHER"), party = pretty_party(party), votes)   # party_group BEFORE the label
long <- finalize_long(raw, "nj_2022"); save_long(long, "he_nj_2022")
sh <- derive_shares(long) %>% transmute(state = "NEW JERSEY", year, cty_fips, sample, demovote, repuvote, totalvote); saveRDS(sh, file.path(OUTPUT_DIR, "elect_he_cty_nj_2022.rds"))
print(check_long_vs_source(long, file.path(OUTPUT_DIR, "elect_he_cty_nj_2022.rds")) %>% select(source, keys_source, matched, pass))
cat("counties:", nrow(sh), "| split counties:", sum(long %>% group_by(county_fips) %>% summarise(d = n_distinct(district)) %>% pull(d) > 1), "| median dem", round(median(sh$demovote), 3), "rep", round(median(sh$repuvote), 3), "\n")
pe <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")); r <- sh %>% inner_join(pe %>% filter(sample == "PE", year == 2020) %>% select(cty_fips, pe = totalvote), by = "cty_fips") %>% mutate(ratio = totalvote / pe)
cat("House 2022 / presidential 2020 total: min", round(min(r$ratio), 2), "median", round(median(r$ratio), 2), "max", round(max(r$ratio), 2), "\n")
old <- pe %>% filter(sample == "HE", year == 2022, cty_fips %/% 1000 == 34) %>% select(cty_fips, o_tot = totalvote); cat("MEDSL panel NJ 2022 rows:", nrow(old), "| their total votes", sum(old$o_tot), "vs official", sum(sh$totalvote), "\n")
