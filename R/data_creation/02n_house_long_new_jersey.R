## Candidate-level LONG tables for New Jersey U.S. House: he_cty_nj (2012, 2014 OpenElections; 01n) and he_cty_nj_historical (2000-2010 official PDFs; 01bl).
## Parsing/grouping copied from the originals; adds candidate names (and districts) that they aggregated away.
source(file.path("R", "00_setup.R")); library(readr); library(stringr); source(file.path("R", "long_helpers.R"))
save_step <- function(df, name) invisible(df)
RAW <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "new_jersey")
nj_fips <- read_delim(file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name))) %>% filter(state == "NEW JERSEY") %>% select(county_name, county_fips)
stopifnot(nrow(nj_fips) == 21)

## ---- 2012, 2014 (OpenElections) ----
PARTY_ALIAS <- c(Republican = "REP", Democratic = "DEM")
to_party <- function(x) coalesce(PARTY_ALIAS[trimws(x)], "OTHER")
oe <- purrr::map_dfr(c(2012, 2014), function(y) read_csv(file.path(RAW, paste0(y, "_raw.csv")), show_col_types = FALSE, col_types = cols(.default = "c")) %>%
  filter(office == "U.S. House") %>% mutate(county = toupper(trimws(county)), party_lbl = trimws(party), party_group = to_party(party), votes = as.numeric(votes)) %>%
  filter(!is.na(votes)) %>% transmute(year = y, county, district, candidate, party = party_lbl, party_group = unname(party_group), votes)) %>%
  inner_join(nj_fips, by = c("county" = "county_name"))
long_oe <- finalize_long(oe %>% select(year, county_fips, district, candidate, party, party_group, votes), "nj"); save_long(long_oe, "he_nj")
res1 <- check_long_vs_source(long_oe, file.path(OUTPUT_DIR, "elect_he_cty_nj.rds")); print(res1)

## ---- 2000-2010 (official PDFs) ----
num <- function(x) as.numeric(gsub(",", "", x))
nj_county_names <- nj_fips$county_name[order(-nchar(nj_fips$county_name))]
ORD <- c(First = 1, Second = 2, Third = 3, Fourth = 4, Fifth = 5, Sixth = 6, Seventh = 7, Eighth = 8, Ninth = 9, Tenth = 10, Eleventh = 11, Twelfth = 12, Thirteenth = 13)
parse_year_text <- function(lines, year) {
  district_idx <- grep("(?i)^\\s*\\w+\\s+Congressional District:", lines, perl = TRUE)
  stopifnot(length(district_idx) > 0)
  ends <- c(district_idx[-1] - 1, length(lines))
  purrr::map_dfr(seq_along(district_idx), function(i) {
    block <- lines[district_idx[i]:ends[i]]
    dist <- unname(ORD[str_match(block[1], "(?i)^\\s*(\\w+)\\s+Congressional")[, 2]])
    dist <- unname(ORD[tools::toTitleCase(tolower(str_match(block[1], "(?i)^\\s*(\\w+)\\s+Congressional")[, 2]))])
    total_idx <- grep("(?i)^\\s*Total\\s+[0-9,]+\\s*$", block)
    if (length(total_idx) == 0) return(NULL)
    start <- 2
    purrr::map_dfr(total_idx, function(ti) {
      candidate_lines <- block[start:(ti - 1)]
      start <<- ti + 1
      candidate_lines <- candidate_lines[trimws(candidate_lines) != ""]
      if (length(candidate_lines) == 0) return(NULL)
      party <- case_when(
        any(str_detect(candidate_lines, "(?i)\\bDemocratic\\b")) ~ "DEM",
        any(str_detect(candidate_lines, "(?i)\\bRepublican\\b")) ~ "REP",
        TRUE ~ "OTHER")
      county_rows <- purrr::map_dfr(candidate_lines, function(line) {
        hit <- nj_county_names[str_detect(toupper(line), paste0("\\b", nj_county_names, "\\b"))]
        if (length(hit) == 0) return(NULL)
        county <- hit[which.max(nchar(hit))]
        vote_match <- str_match(line, "([0-9,]+)\\s*$")[, 2]
        if (is.na(vote_match)) return(NULL)
        ## candidate name / designation = first two columns of the FIRST line that carries a county row
        parts <- str_split(str_trim(line), "\\s{2,}")[[1]]
        tibble(county = county, votes = num(vote_match), first_part = parts[1], second_part = ifelse(length(parts) > 1, parts[2], NA_character_), slogan = ifelse(length(parts) > 3, parts[4], NA_character_))
      })
      if (nrow(county_rows) == 0) return(NULL)
      nm <- county_rows$first_part[1]; des <- county_rows$second_part[1]; slo <- county_rows$slogan[1]
      ## NJ prints "Independent" as the designation and the real party in the slogan column
      if (!is.na(des) && grepl("^Independent$", des, ignore.case = TRUE) && !is.na(slo) && nzchar(slo)) des <- slo
      county_rows %>% transmute(county, votes, party_group = party, year = year, district = dist,
                                candidate = trimws(sub("\\s*\\(w\\)\\s*$", "", nm)), party = des)
    })
  })
}
years <- c(2000, 2002, 2004, 2006, 2008, 2010)
hist <- purrr::map_dfr(years, function(yr) {
  lines <- system2("pdftotext", c("-layout", shQuote(file.path(RAW, paste0(yr, "_house.pdf"))), "-"), stdout = TRUE)
  withCallingHandlers(parse_year_text(lines, yr), warning = function(w) invokeRestart("muffleWarning"))
}) %>% inner_join(nj_fips, by = c("county" = "county_name"))
message("historical rows: ", nrow(hist), "; NA district: ", sum(is.na(hist$district)), "; candidates: ", n_distinct(hist$candidate))
long_h <- finalize_long(hist %>% select(year, county_fips, district, candidate, party, party_group, votes), "nj_historical"); save_long(long_h, "he_nj_historical")
res2 <- check_long_vs_source(long_h, file.path(OUTPUT_DIR, "elect_he_cty_nj_historical.rds")); print(res2)
print(as.data.frame(long_h %>% filter(year == 2000, county_fips == 34007) %>% head(8)))
