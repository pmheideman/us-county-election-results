## Candidate-level LONG table for President 2000-2024 from MEDSL's County Presidential Election Returns (R/data/raw_election/countypres_2000-2024.tab; Harvard Dataverse).
## Same de-duplication as 01a (per year x county keep only mode == "TOTAL" rows if any exist, otherwise sum all mode rows). Candidate x party rows; MEDSL's own aggregate "OTHER" candidate row is kept
## (candidate "Other candidates"). party_group DEMOCRAT / REPUBLICAN -> DEM / REP, everything else OTHER (01a's convention). District is blank (statewide office).
## Acceptance test: shares derived from this table must equal elect_pe_cty_medsl.rds row for row (01a uses MEDSL's own `totalvotes` column as the denominator; the check shows whether the candidate rows add up to it).
## Output: R/output/long/pe_medsl.rds
source(file.path("R", "00_setup.R")); library(readr); source(file.path("R", "long_helpers.R"))
raw <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  mutate(candidatevotes = as.numeric(candidatevotes), totalvotes = as.numeric(totalvotes), mode = toupper(mode))
## 2024 files for ID, NC, RI, UT, WA, WV, WI and WY have a BLANK vote mode (read as NA): 01a's `any(mode == "TOTAL")` was NA and its filter silently dropped those 367 counties. A blank mode is a total.
raw$mode[is.na(raw$mode) | !nzchar(raw$mode)] <- "TOTAL"
has_total <- raw %>% group_by(year, county_fips) %>% summarise(has_total = any(mode == "TOTAL"), .groups = "drop")
## Non-candidate rows are not votes (docs/DECISIONS.md): e.g. South Carolina 2024 carries a "Total Votes Cast" pseudo-candidate row in every county (it doubled the candidate sum). Same pattern list as 01a's PSEUDO_RE.
exprs <- parse(file = file.path("R", "data_creation", "01a_election_data_medsl.R")); eval(exprs[vapply(exprs, function(e) is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) && as.character(e[[2]]) == "PSEUDO_RE", NA)])
n_pseudo <- sum(grepl(PSEUDO_RE, toupper(trimws(raw$candidate)))); message("pseudo-candidate rows removed: ", n_pseudo, " (", paste(names(sort(table(raw$candidate[grepl(PSEUDO_RE, toupper(trimws(raw$candidate)))]), decreasing = TRUE))[1:5], collapse = "; "), ")")
d <- raw %>% filter(!grepl(PSEUDO_RE, toupper(trimws(candidate)))) %>% left_join(has_total, by = c("year", "county_fips")) %>% filter(!has_total | mode == "TOTAL", !is.na(county_fips), !is.na(candidatevotes))
d <- d %>% mutate(candidate = ifelse(toupper(candidate) == "OTHER", "Other candidates", candidate)) %>%
  group_by(year, county_fips, candidate, party) %>% summarise(votes = sum(candidatevotes), .groups = "drop") %>% filter(votes >= 0)
raw2 <- d %>% transmute(year, county_fips = as.integer(county_fips), district = NA_character_, candidate, party, party_group = case_when(tolower(party) == "democrat" ~ "DEM", tolower(party) == "republican" ~ "REP", TRUE ~ "OTHER"), votes)
long <- finalize_long(raw2, "medsl", office = "president"); save_long(long, "pe_medsl")
message("long rows: ", nrow(long), " | county-years: ", nrow(distinct(long, year, county_fips)), " | candidates: ", nrow(distinct(long, year, candidate)))
res <- check_long_vs_source(long, file.path(OUTPUT_DIR, "elect_pe_cty_medsl.rds")); print(res)
## where does it differ? (candidate rows vs MEDSL totalvotes)
src <- readRDS(file.path(OUTPUT_DIR, "elect_pe_cty_medsl.rds")); dd <- derive_shares(long) %>% inner_join(src, by = c("year", "cty_fips", "sample"), suffix = c(".l", ".s")) %>%
  mutate(diff = totalvote.l - totalvote.s, bad = !(abs(demovote.l - demovote.s) < 1e-9 & abs(repuvote.l - repuvote.s) < 1e-9 & abs(diff) < 0.5))
print(as.data.frame(dd %>% filter(bad) %>% count(year) %>% arrange(year))); print(as.data.frame(head(dd %>% filter(bad) %>% select(year, cty_fips, totalvote.l, totalvote.s, demovote.l, demovote.s), 8)))
