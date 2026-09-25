## U.S. Senate county results from official state documents for two MEDSL gaps (2026-09-24):
##  * California 2022 (regular, full-term race): MEDSL's 2022 Senate file has no California rows at all. Source: Secretary of State, Statement of Vote,
##    November 8, 2022 General Election, 'United States Senator (Full Term) by County' (https://elections.cdn.sos.ca.gov/sov/2022-general/sov/complete.pdf;
##    local copy R/data/raw_senate_county/ca_sos/). The same-day special for the partial/unexpired term is excluded (project rule: regular elections only).
##  * New Jersey 2024: MEDSL's rows lack Sussex County (blank `mode` rows dropped by 01a). Source: Division of Elections, Official List, Candidates for
##    US Senate, General Election 11/05/2024 (https://www.nj.gov/state/elections/assets/pdf/election-results/2024/2024-official-general-results-us-senate.pdf;
##    local copy R/data/raw_senate_county/nj_doe/).
## Checks (stop on failure): county rows add up to the printed statewide / candidate totals; California's totals equal the FEC's 2022 full-term figures
## (Padilla 6,621,621; Meuser 4,222,029). Outputs: long/se_ca_sos_2022.rds, elect_se_cty_ca_sos_2022.rds, long/se_nj_2024.rds, elect_se_cty_nj_2024.rds.
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R")); library(readr)
cp <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>% filter(!is.na(county_fips), state_po %in% c("CA", "NJ")) %>%
  transmute(state_po, county_fips, key = gsub("[^A-Z]", "", toupper(county_name))) %>% distinct()
key <- function(z) gsub("[^A-Z]", "", toupper(z))

## ---- California 2022, full term ------------------------------------------------------------------------------------------------------------------
L <- system2("pdftotext", c("-layout", shQuote(file.path(PROJECT_ROOT, "R/data/raw_senate_county/ca_sos/sov_2022_general_complete.pdf")), "-"), stdout = TRUE)
a <- which(grepl("United States Senator \\(Full Term\\)$", trimws(L)))[1]; b <- which(grepl("United States Senator \\(Partial/Unexpired Term\\)$", trimws(L)))[1]
seg <- L[a:b]
rows <- regmatches(seg, regexec("^([A-Z][A-Za-z .]+?)\\s{2,}([0-9,]+)\\s+([0-9,]+)\\s*$", seg)); rows <- rows[lengths(rows) == 4]
ca <- do.call(rbind, lapply(rows, function(m) data.frame(county = trimws(m[2]), padilla = as.numeric(gsub(",", "", m[3])), meuser = as.numeric(gsub(",", "", m[4])))))
tot <- ca[ca$county == "State Totals", ]; ca <- ca[ca$county != "State Totals", ]
stopifnot(nrow(ca) == 58, sum(ca$padilla) == tot$padilla, sum(ca$meuser) == tot$meuser, tot$padilla == 6621621, tot$meuser == 4222029)
ca <- ca %>% mutate(key = key(county)) %>% left_join(cp %>% filter(state_po == "CA"), by = "key"); stopifnot(!anyNA(ca$county_fips))
raw_ca <- bind_rows(ca %>% transmute(year = 2022L, county_fips, district = NA_character_, candidate = "Alex Padilla", party = "Democratic", party_group = "DEM", votes = padilla),
                    ca %>% transmute(year = 2022L, county_fips, district = NA_character_, candidate = "Mark P. Meuser", party = "Republican", party_group = "REP", votes = meuser))

## ---- New Jersey 2024 (Official List layout, as the House parser 02aa2) -----------------------------------------------------------------------------
L <- gsub("\f", "", system2("pdftotext", c("-layout", shQuote(file.path(PROJECT_ROOT, "R/data/raw_senate_county/nj_doe/2024-official-general-results-us-senate.pdf")), "-"), stdout = TRUE))
nj_names <- cp %>% filter(state_po == "NJ")
nj_cty <- toupper(unique(read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>% filter(state_po == "NJ", !is.na(county_fips)) %>% pull(county_name)))
cty_re <- paste0("^\\s{30,}(", paste(nj_cty, collapse = "|"), ")\\s+([A-Z\"][A-Z0-9 .&'\"!?/()-]*?)\\s{2,}([0-9,]+)\\s*$"); zip_re <- ",\\s*[A-Z]{2}(\\s+[0-9]{5})?\\s*$"
cand <- NA; rows <- list(); tots <- list(); n_since <- 0; grand <- NA
for (i in seq_along(L)) {
  ln <- L[i]
  if (grepl("^[A-Z]", ln) && !grepl("^(Name|Candidates|For |Official)", ln)) {
    nx <- head(L[(i + 1):min(i + 6, length(L))][nzchar(trimws(L[(i + 1):min(i + 6, length(L))]))], 2)
    if (any(grepl(zip_re, nx))) { nm <- sub("\\s{2,}.*$", "", ln); cand <- trimws(gsub("\\(w\\)|\\*", "", nm)); next } }
  m <- regmatches(ln, regexec(cty_re, ln))[[1]]
  if (length(m) && key(m[2]) %in% nj_names$key) { rows[[length(rows) + 1]] <- data.frame(candidate = cand, county = m[2], party = m[3], votes = as.numeric(gsub(",", "", m[4]))); n_since <- n_since + 1; next }
  m <- regmatches(ln, regexec("^\\s+Total\\s+([0-9,]+)\\s*$", ln))[[1]]
  if (length(m)) { v <- as.numeric(gsub(",", "", m[2])); if (n_since > 0) tots[[length(tots) + 1]] <- data.frame(candidate = cand, printed = v) else grand <- v; n_since <- 0 }
}
nj <- bind_rows(rows); tots <- bind_rows(tots)
ck <- nj %>% group_by(candidate) %>% summarise(sum = sum(votes), n = n(), .groups = "drop") %>% left_join(tots, by = "candidate")
print(as.data.frame(ck)); stopifnot(!anyNA(nj$candidate), all(ck$sum == ck$printed), is.na(grand) || sum(nj$votes) == grand, n_distinct(nj$county) == 21)
raw_nj <- nj %>% mutate(key = key(county)) %>% left_join(nj_names, by = "key") %>%
  transmute(year = 2024L, county_fips, district = NA_character_, candidate = pretty_name(candidate), party_group = case_when(party == "DEMOCRATIC" ~ "DEM", party == "REPUBLICAN" ~ "REP", TRUE ~ "OTHER"),
            party = pretty_party(party), votes)
stopifnot(!anyNA(raw_nj$county_fips))

for (x in list(list(raw_ca, "ca_sos_2022", "CALIFORNIA"), list(raw_nj, "nj_2024", "NEW JERSEY"))) {
  long <- finalize_long(x[[1]], x[[2]], office = "senate"); save_long(long, paste0("se_", x[[2]]))
  sh <- derive_shares(long) %>% transmute(state = x[[3]], year, cty_fips, sample, demovote, repuvote, totalvote); f <- file.path(OUTPUT_DIR, paste0("elect_se_cty_", x[[2]], ".rds")); saveRDS(sh, f)
  stopifnot(all(check_long_vs_source(long, f)$pass))
  message(x[[2]], ": ", nrow(sh), " counties, votes ", format(sum(long$votes), big.mark = ","), ", median dem ", round(median(sh$demovote), 3))
}
