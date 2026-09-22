## Candidate-level LONG table for Florida U.S. House 1992-2016 (results.elections.myflorida.com DetailRpt by district; cached HTML).
## Southeast batch. Reuses 01e's constants and fetch helper (definitions only; 01e's fold-in code is never run) and its exact table parsing,
## adding what the original threw away: the candidate names in the header cells, and the district (from the file name).
## party_group: party tag "(DEM)" / "(REP)" exactly as the original; every other tag or none -> OTHER (still counted in totalvote).
## Output: R/output/long/he_fl.rds. Acceptance: check_long_vs_source against elect_he_cty_fl.rds.

source(file.path("R", "00_setup.R"))
save_step <- function(df, name) invisible(df)
library(readr); library(rvest); library(xml2)
source(file.path("R", "long_helpers.R"))
exprs <- parse(file = file.path("R", "data_creation", "01e_house_county_florida.R"))
want <- c("RAW_DIR", "FL_ELECTION_DATES", "MAX_DIST_TO_TRY", "fetch_fl_district_page")
eval(exprs[vapply(exprs, function(e) is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) && as.character(e[[2]]) %in% want, NA)])

parse_fl_candidates <- function(path) {
  html <- tryCatch(read_html(path, encoding = "latin1"), error = function(e) NULL)
  if (is.null(html)) return(tibble())
  tbls <- xml_find_all(html, "//table"); if (length(tbls) == 0) return(tibble())
  trs <- xml_find_all(tbls[[1]], ".//tr"); if (length(trs) < 3) return(tibble())
  cand_raw <- xml_text(xml_find_all(trs[[2]], ".//td"), trim = TRUE)[-1]
  if (length(cand_raw) == 0) return(tibble())
  party <- toupper(gsub(".*\\(([A-Za-z]+)\\)\\s*$", "\\1", cand_raw)); party[!grepl("\\([A-Za-z]+\\)\\s*$", cand_raw)] <- ""
  candidate <- trimws(sub("\\s*\\([A-Za-z]+\\)\\s*$", "", cand_raw))
  rows <- list()
  for (i in seq(3, length(trs))) {
    cells <- xml_find_all(trs[[i]], ".//td"); if (length(cells) == 0) next
    label <- gsub("\u00a0", "", xml_text(cells[[1]], trim = TRUE))   # the ORIGINAL strips a non-breaking space (U+00A0), not a normal space
    if (label %in% c("", "Fed Abs", "Total", "% Votes") || grepl("^%", label)) next
    votes_raw <- xml_text(cells, trim = TRUE)[-1]; if (length(votes_raw) != length(party)) next
    rows[[length(rows) + 1]] <- tibble(county = toupper(label), candidate = candidate, party = party, votes = as.numeric(gsub(",", "", votes_raw)))
  }
  if (length(rows) == 0) tibble() else bind_rows(rows)
}
fl_raw <- purrr::pmap(expand_grid(FL_ELECTION_DATES, dist_num = seq_len(MAX_DIST_TO_TRY)), function(year, date_str, dist_num) {
  p <- parse_fl_candidates(fetch_fl_district_page(date_str, dist_num)); if (nrow(p) == 0) return(tibble()); p %>% mutate(year = year, district = dist_num)
}) %>% bind_rows()
fl_fips <- read_delim(file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  filter(state == "FLORIDA") %>% distinct(county_name, county_fips)
## Florida prints 3-letter tags; expand them for display (party_group is still derived from the raw tag, above/below)
FL_TAGS <- c(WRI = "Write-in", NPA = "No Party Affiliation", LPF = "Libertarian", LIB = "Libertarian", REF = "Reform", CPF = "Constitution",
             TEA = "Tea Party", GRE = "Green", LAW = "Ecology (Florida)", FWP = "Florida Whig", TLP = "Independent (TLP)", IND = "Independent",
             NON = "Nonpartisan", NPA = "No Party Affiliation")
long <- fl_raw %>% inner_join(fl_fips, by = c("county" = "county_name")) %>%
  mutate(party_raw = party, party = ifelse(party %in% names(FL_TAGS), unname(FL_TAGS[party]), party)) %>%
  transmute(year, county_fips, district, candidate, party,
            party_group = case_when(party_raw == "DEM" ~ "DEM", party_raw == "REP" ~ "REP", TRUE ~ "OTHER"), votes) %>%
  finalize_long("fl")
save_long(long, "he_fl")
res <- check_long_vs_source(long, file.path(OUTPUT_DIR, "elect_he_cty_fl.rds")); print(res)
message("FL rows ", nrow(long), "; candidates ", nrow(distinct(long, year, district, candidate)), "; blank party tags: ", sum(fl_raw$party == ""), " raw rows")
print(as.data.frame(head(long %>% filter(year == 2012, county_fips == 12001), 6)))
print(as.data.frame(long %>% count(party, party_group, sort = TRUE) %>% head(8)))
