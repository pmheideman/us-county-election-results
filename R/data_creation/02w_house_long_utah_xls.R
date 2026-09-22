## Long table for Utah 2008/2010 Excel sheets (01bm): same column selection as 01bm's parse_house_sheet, but keeps each candidate column's
## name (header row 2, e.g. 'Morgan Philpot "R"') and district (row 1, carried right).
source(file.path("R", "data_creation", "02w_common.R"))
library(readxl); library(stringr)
ut_fips <- fips_of("UTAH") %>% distinct(county_name, county_fips); stopifnot(nrow(ut_fips) == 29)
num <- function(x) as.numeric(gsub(",", "", as.character(x)))
parse_sheet <- function(path, sheet, year) {
  raw <- read_excel(path, sheet = sheet, col_names = FALSE)
  district <- as.character(raw[1, ]); for (i in seq_along(district)) if (is.na(district[i]) && i > 1) district[i] <- district[i - 1]
  header <- as.character(raw[2, ])
  party_std <- case_when(str_detect(header, '"R"') ~ "REP", str_detect(header, '"D"') ~ "DEM", TRUE ~ "OTHER")
  total_row <- which(str_detect(toupper(as.character(raw[[1]])), "^TOTAL"))[1]; county_rows <- raw[3:(total_row - 1), ]
  cand_cols <- which(!is.na(district) & seq_along(district) > 4 & !is.na(header) & header != "NA")
  cname <- str_squish(str_replace_all(header, '\\s*"[A-Za-z]+"\\s*$', "")); plab <- str_match(header, '"([A-Za-z]+)"\\s*$')[, 2]
  purrr::map_dfr(seq_len(nrow(county_rows)), function(i) {
    county <- toupper(trimws(as.character(county_rows[i, 1]))); if (is.na(county) || county == "" || county == "NA") return(NULL)
    vals <- num(as.character(county_rows[i, cand_cols])); keep <- !is.na(vals); if (!any(keep)) return(NULL)
    tibble(year = year, county = county, district = str_extract(district[cand_cols][keep], "[0-9]+"), candidate = cname[cand_cols][keep],
           party = c(R = "Republican", D = "Democratic")[plab[cand_cols][keep]], party_group = party_std[cand_cols][keep], votes = vals[keep]) }) }
d <- RAW_ROOT; f <- function(y) file.path(d, "utah_historical", paste0(y, "gen.xls"))
long <- bind_rows(parse_sheet(f(2008), "U S House", 2008), parse_sheet(f(2010), "U.S. House", 2010)) %>%
  inner_join(ut_fips, by = c("county" = "county_name")) %>% select(-county)
cat("candidates:\n"); print(as.data.frame(long %>% distinct(year, district, candidate, party_group) %>% arrange(year, district)))
long <- finalize_long(long, "ut_xls"); save_long(long, "he_ut_xls")
message("UT xls long rows: ", nrow(long)); print(check_long_vs_source(long, SRC("ut_xls")))
