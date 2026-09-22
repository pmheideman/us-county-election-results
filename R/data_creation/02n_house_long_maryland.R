## Candidate-level LONG tables for Maryland U.S. House: he_cty_md_csv (2000-2014, per-county official CSVs; 01bi) and he_cty_md_html (1986-2002, results HTML pages; 01bj).
## Parsing copied from the originals (same file selection, same party grouping), keeping the candidate name, district and reported party that they aggregated away.
## CSV county files: one file per county listed on each year's archive index (cached index + county files); "Office District" is the congressional district.
## HTML pages: one table per district, header row "Name (Party)", first row "Congressional District: NN"; 2002 uses one page per district (g_cdNN.html).
source(file.path("R", "00_setup.R")); library(readr); library(rvest); library(stringr); source(file.path("R", "long_helpers.R"))
save_step <- function(df, name) invisible(df)
RAW <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "maryland")
md_fips <- read_delim(file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name))) %>% filter(state == "MARYLAND") %>%
  mutate(county_name = str_replace(county_name, "^ST MARY'S$", "ST. MARY'S")) %>% distinct(county_name, county_fips)
stopifnot(nrow(md_fips) == 24)
fetch <- function(url, dest) { if (!file.exists(dest) || file.size(dest) == 0) system2("curl", c("-sL", "-A", "'Mozilla/5.0'", "-o", shQuote(dest), shQuote(url))); dest }

## ---------------- CSV years ----------------
filename_to_county <- function(x) { x <- gsub("[_.]", " ", x); x <- toupper(trimws(x)); x <- gsub("\\s+", " ", x)
  case_when(x %in% c("PRINCE GEORGE", "PRINCE GEORGES") ~ "PRINCE GEORGE'S", x %in% c("QUEEN ANNE", "QUEEN ANNES") ~ "QUEEN ANNE'S", x %in% c("ST MARY", "ST MARYS", "SAINT MARYS") ~ "ST. MARY'S", TRUE ~ x) }
list_general_csvs <- function(year) {
  page <- read_html(fetch(paste0("https://elections.maryland.gov/elections/archive/", year, "/election_data/index.html"), file.path(RAW, paste0(year, "_index.html"))))
  hrefs <- page %>% html_elements("a") %>% html_attr("href"); hrefs <- hrefs[!is.na(hrefs)]
  general <- hrefs[str_detect(hrefs, paste0(year, ".*General\\.csv$|General_", year, "\\.csv$")) & !str_detect(hrefs, "(?i)primary|question|precinct|legislative|congressional_district|reference|state_")]
  tibble(filename = general, county = filename_to_county(str_remove(general, paste0("_(County_)?", year, "_?General.*$|_General_", year, ".*$"))),
         url = paste0("https://elections.maryland.gov/elections/archive/", year, "/election_data/", general)) %>% distinct(county, .keep_all = TRUE)
}
read_county_general <- function(year, county, url) {
  path <- fetch(url, file.path(RAW, paste0(year, "_", gsub("[^A-Za-z]", "", county), "_general.csv")))
  raw <- tryCatch(read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c")), error = function(e) NULL)
  if (is.null(raw) || !"Office Name" %in% names(raw)) return(NULL)
  if (!"Candidate" %in% names(raw)) raw$Candidate <- raw$`Candidate Name`      # 2006+ files name the column "Candidate Name"
  raw %>% filter(`Office Name` %in% c("Representative in Congress", "U.S. Congress", "Rep in Congress")) %>%
    transmute(county = county, year = year, district = `Office District`, candidate = trimws(Candidate), party_lbl = toupper(trimws(Party)), votes = as.numeric(`Total Votes`)) %>% filter(!is.na(votes))
}
csv_rows <- purrr::map_dfr(c(2000, 2004, 2006, 2008, 2010, 2012, 2014), function(yr) purrr::pmap_dfr(list_general_csvs(yr), function(county, url, filename) read_county_general(yr, county, url)))
to_party <- function(x) case_when(str_detect(x, "^DEM") ~ "DEM", str_detect(x, "^REP") ~ "REP", TRUE ~ "OTHER")
d_csv <- csv_rows %>% mutate(party_group = to_party(party_lbl)) %>% inner_join(md_fips, by = c("county" = "county_name")) %>%
  transmute(year, county_fips, district, candidate, party = party_lbl, party_group, votes)
long_csv <- finalize_long(d_csv, "md_csv"); save_long(long_csv, "he_md_csv")
res_csv <- check_long_vs_source(long_csv, file.path(OUTPUT_DIR, "elect_he_cty_md_csv.rds")); print(res_csv)

## ---------------- HTML years ----------------
num <- function(x) as.numeric(gsub(",", "", x))
parse_district_table <- function(tbl, header_row_idx = 2, district = NA_character_) {
  grid <- tbl %>% html_table(fill = TRUE, header = FALSE)
  if (nrow(grid) < header_row_idx + 1) return(NULL)
  header_row <- as.character(grid[header_row_idx, ])
  if (is.na(district)) district <- str_match(as.character(grid[[1]][1]), "(?i)District:?\\s*0*(\\d+)")[, 2]
  party_std <- case_when(str_detect(header_row, "(?i)Democratic") ~ "DEM", str_detect(header_row, "(?i)Republican") ~ "REP", TRUE ~ "OTHER"); party_std[1] <- NA_character_
  total_idx <- which(str_detect(as.character(grid[[1]]), "(?i)^total"))[1]; if (is.na(total_idx)) return(NULL)
  county_rows <- grid[(header_row_idx + 1):(total_idx - 1), ]; if (nrow(county_rows) == 0) return(NULL)
  cand <- str_trim(sub("\\s*\\([^)]*\\)\\s*$", "", header_row)); plab <- str_match(header_row, "\\(([^)]*)\\)\\s*$")[, 2]
  purrr::map_dfr(seq_len(nrow(county_rows)), function(i) {
    row <- county_rows[i, ]; county <- toupper(trimws(as.character(row[[1]]))); county <- gsub("`", "'", county)
    if (county == "" || is.na(county)) return(NULL)
    vals <- as.character(row)[-1]; parties <- party_std[-1]
    keep <- !is.na(vals) & vals != "" & !is.na(parties)
    if (!any(keep)) return(NULL)
    tibble(county = county, district = district, candidate = cand[-1][keep], party = plab[-1][keep], party_group = parties[keep], votes = num(vals[keep]))
  })
}
parse_year <- function(year, page_type) {
  fname <- if (page_type == "pres") "pregarep.html" else "garep.html"
  page <- read_html(file.path(RAW, paste0(year, "_", fname)))
  tbls <- page %>% html_elements("table") %>% Filter(function(t) { fc <- tryCatch((t %>% html_table(fill = TRUE, header = FALSE))[[1]][1], error = function(e) ""); str_detect(as.character(fc), "(?i)Representative in Congress") }, .)
  purrr::map_dfr(tbls, parse_district_table) %>% mutate(year = year)
}
parse_2002 <- function() purrr::map_dfr(sprintf("%02d", 1:8), function(d) {
  tbls <- read_html(file.path(RAW, paste0("2002_cd", d, ".html"))) %>% html_elements("table")
  purrr::map_dfr(tbls, parse_district_table, header_row_idx = 1, district = as.character(as.integer(d)))
}) %>% mutate(year = 2002)
yt <- tribble(~year, ~page_type, 1986, "gov", 1988, "pres", 1990, "gov", 1992, "pres", 1994, "gov", 1996, "pres", 1998, "gov")
html_rows <- bind_rows(purrr::pmap_dfr(yt, function(year, page_type) parse_year(year, page_type)), parse_2002())
message("html rows: ", nrow(html_rows), "; NA district: ", sum(is.na(html_rows$district)))
d_html <- html_rows %>% inner_join(md_fips, by = c("county" = "county_name")) %>% transmute(year, county_fips, district, candidate, party, party_group, votes)
long_html <- finalize_long(d_html, "md_html"); save_long(long_html, "he_md_html")
res_html <- check_long_vs_source(long_html, file.path(OUTPUT_DIR, "elect_he_cty_md_html.rds")); print(res_html)
for (n in c("csv", "html")) { l <- get(paste0("long_", n)); message("MD ", n, ": rows ", nrow(l), "; NA district ", sum(is.na(l$district)), "; candidates ", n_distinct(l$year, l$district, l$candidate)) }
print(as.data.frame(long_html %>% filter(year == 1992, county_fips == 24003) %>% head(6)))
