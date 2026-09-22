## Candidate-level LONG tables for Maine U.S. House: he_cty_me (2012; OpenElections; 01af), he_cty_me_historical (1990-2010, 2014; official spreadsheets/text; 01bo) and
## he_cty_me_2018 (2018; SOS spreadsheets; 01bq). The 01bo parsing section (everything before its "Aggregate to county-year" step) is evaluated unchanged in a private
## environment so its exact party logic (incl. the Wikipedia lookups for last-name-only years) is reused, not re-implemented; only the aggregation is replaced.
source(file.path("R", "00_setup.R")); library(readr); library(stringr); source(file.path("R", "long_helpers.R"))
save_step <- function(df, name) invisible(df)
RAW <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "maine")
me_fips <- read_delim(file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name))) %>% filter(state == "MAINE", !is.na(county_fips)) %>% select(county_name, county_fips)
grp_lbl <- function(g) c(DEM = "Democratic", REP = "Republican", OTHER = "Other")[g]

## ---------------- 2012 (OpenElections; bare-CR line endings) ----------------
to_party <- function(x) { x <- toupper(trimws(x)); case_when(x == "D" | startsWith(x, "DEM") ~ "DEM", x == "R" | startsWith(x, "REP") ~ "REP", TRUE ~ "OTHER") }
me12 <- read_csv(I(gsub("\r", "\n", read_file(file.path(RAW, "2012_town.csv")))), show_col_types = FALSE, col_types = cols(.default = "c")) %>%
  filter(grepl("^U\\.S\\.\\s*House$", office)) %>%
  mutate(county = toupper(trimws(county)), county = ifelse(county %in% c("", "UOC", "UOCAVA"), NA, county), party_lbl = party, party_group = to_party(party), votes = as.numeric(votes)) %>%
  filter(!is.na(county), !is.na(votes)) %>% inner_join(me_fips, by = c("county" = "county_name")) %>%
  transmute(year = 2012, county_fips, district, candidate, party = party_lbl, party_group, votes)
long12 <- finalize_long(me12, "me"); save_long(long12, "he_me")
res12 <- check_long_vs_source(long12, file.path(OUTPUT_DIR, "elect_he_cty_me.rds")); print(res12)

## ---------------- 1990-2010, 2014 (reuse 01bo's parsing section) ----------------
src <- readLines(file.path(PROJECT_ROOT, "R", "data_creation", "01bo_house_county_maine_historical.R"))
cut <- grep("^## ---- Aggregate to county-year", src)[1]; stopifnot(!is.na(cut))
env <- new.env(parent = globalenv())
eval(parse(text = src[seq_len(cut - 1)]), envir = env)
la <- get("long_all", envir = env)                       # county (name), year, district, cand, party (DEM/REP/OTHER), votes
message("01bo long_all rows: ", nrow(la), "; years ", paste(sort(unique(la$year)), collapse = " "))
## "(OTHER/WRITE-IN)" rows are the sheets' printed write-in totals: keep as a candidate named "Write-ins / Other"
hist <- la %>% mutate(candidate = ifelse(cand == "(OTHER/WRITE-IN)", "Write-ins / Other", cand), party_group = party) %>%
  inner_join(me_fips, by = c("county" = "county_name")) %>% transmute(year = as.integer(year), county_fips, district, candidate, party = NA_character_, party_group, votes)
## candidates in last-name-only years are printed in capitals -> pretty_name in finalize_long; a few sheets print "Last, First": flip them
hist$candidate <- ifelse(grepl(",", hist$candidate), sub("^\\s*([^,]+),\\s*(.+)$", "\\2 \\1", hist$candidate), hist$candidate)
long_h <- finalize_long(hist, "me_historical"); save_long(long_h, "he_me_historical")
res_h <- check_long_vs_source(long_h, file.path(OUTPUT_DIR, "elect_he_cty_me_historical.rds")); print(res_h)

## ---------------- 2018 (SOS spreadsheets; CD1 town rows, CD2 ward rows) ----------------
CTY_CODE <- c(AND = "ANDROSCOGGIN", ARO = "AROOSTOOK", CUM = "CUMBERLAND", FRA = "FRANKLIN", HAN = "HANCOCK", KEN = "KENNEBEC", KNO = "KNOX", LIN = "LINCOLN", OXF = "OXFORD",
              PEN = "PENOBSCOT", PIS = "PISCATAQUIS", SAG = "SAGADAHOC", SOM = "SOMERSET", WAL = "WALDO", WAS = "WASHINGTON", YOR = "YORK")
numx <- function(x) suppressWarnings(as.numeric(gsub(",", "", trimws(as.character(x)))))
rdx <- function(f) suppressMessages(read_excel(file.path(RAW, f), sheet = 1, col_names = FALSE, col_types = "text"))
flip <- function(x) sub("^\\s*([^,]+),\\s*(.+)$", "\\2 \\1", trimws(x))
grab <- function(x, dist, cols, ptype) {
  d <- x[toupper(trimws(x[[2]])) %in% names(CTY_CODE), ]
  purrr::map_dfr(cols, function(cl) tibble(county = CTY_CODE[toupper(trimws(d[[2]]))], district = dist, candidate = flip(x[[cl]][1]),
                                            party = trimws(x[[cl]][3]), votes = numx(d[[cl]])))
}
c1 <- rdx("2018_house_cd1.xlsx"); c2 <- rdx("2018_house_cd2.xlsx")
stopifnot(grepl("Grohman", c1[[4]][1]), grepl("Pingree", c1[[6]][1]), grepl("Bond", c2[[5]][1]), grepl("Poliquin", c2[[8]][1]))
y18 <- bind_rows(grab(c1, "1", 4:6), grab(c2, "2", 5:8)) %>%
  mutate(party_group = case_when(grepl("^Dem", party, ignore.case = TRUE) ~ "DEM", grepl("^Rep", party, ignore.case = TRUE) ~ "REP", TRUE ~ "OTHER"),
         party = ifelse(grepl("^Ind$", party, ignore.case = TRUE), "Independent", party)) %>%
  inner_join(me_fips, by = c("county" = "county_name")) %>% transmute(year = 2018L, county_fips, district, candidate, party, party_group, votes)
stopifnot(!anyNA(y18$votes))
long18 <- finalize_long(y18, "me_2018"); save_long(long18, "he_me_2018")
res18 <- check_long_vs_source(long18, file.path(OUTPUT_DIR, "elect_he_cty_me_2018.rds")); print(res18)
for (n in c("12", "_h", "18")) { l <- get(paste0("long", n)); message("ME ", n, ": rows ", nrow(l), "; NA district ", sum(is.na(l$district)), "; candidates ", n_distinct(l$year, l$district, l$candidate)) }
print(as.data.frame(long18 %>% group_by(candidate, party, party_group) %>% summarise(v = sum(votes), .groups = "drop")))
