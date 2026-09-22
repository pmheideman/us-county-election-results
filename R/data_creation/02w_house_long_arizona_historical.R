## Long tables for Arizona 1990-1998 (01bv, 01br, 01bs, 01bt, 01bu): canvass-derived candidate x county votes.
## 1990/1992/1996: the candidate x county numbers are embedded in the original scripts as R objects; they are EVALUATED from those scripts
##   (only the data-defining top-level assignments, never the parts that write files) so there is one copy of the numbers.
## 1994: the script stored only D/R/L columns per district-county (no names) -> placeholder candidate labels.
## 1996: the script stores surnames only (e.g. SALMON) -> surnames used as the candidate. 1998: full names parsed from the canvass PDF text layer.
source(file.path("R", "data_creation", "02w_common.R"))
DC <- file.path(PROJECT_ROOT, "R", "data_creation")
az_fips <- read_delim(file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  filter(state == "ARIZONA", !is.na(county_fips)) %>% distinct(county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))
## evaluate the named top-level assignments of an original script, in file order, into env `e`
eval_named <- function(file, nms, e = new.env(parent = globalenv())) {
  for (x in parse(file = file.path(DC, file)))
    if (is.call(x) && identical(x[[1]], as.name("<-")) && is.name(x[[2]]) && as.character(x[[2]]) %in% nms) eval(x, e)
  stopifnot(all(nms %in% ls(e))); e
}
finish <- function(df, y, tag) {
  long <- df %>% mutate(year = y) %>% inner_join(az_fips, by = c("county" = "county_name")) %>% select(-county)
  long <- finalize_long(long, tag); save_long(long, paste0("he_", tag))
  message(tag, " long rows: ", nrow(long), " | candidates: ", n_distinct(long$district, long$candidate)); print(check_long_vs_source(long, SRC(tag))); invisible(long)
}
party_lab <- c(D = "Democratic", R = "Republican", SW = "Socialist Workers", L = "Libertarian", I = "Independent", NL = "Natural Law", DEM = "Democratic", REP = "Republican", LBT = "Libertarian", RPA = "Reform")

## ---- 1990 (01bv): CANDS list; write-ins -> OTHER regardless of printed party (as 01bv) ----
e <- eval_named("01bv_house_county_arizona_1990.R", c("COUNTIES", "BALLOTS", "cand", "CANDS"))
d90 <- purrr::map_dfr(e$CANDS, function(cd) { stopifnot(sum(cd$cells) == cd$total)
  tibble(county = names(cd$cells), district = cd$district, candidate = cd$name, party = unname(party_lab[cd$party]),
         party_group = ifelse(cd$writein, "OTHER", ifelse(cd$party == "D", "DEM", ifelse(cd$party == "R", "REP", "OTHER"))), votes = as.numeric(cd$cells)) })
d90$candidate <- sub("\\s*\\(write-in\\)\\s*$", "", d90$candidate, ignore.case = TRUE)
finish(d90, 1990, "az_1990")

## ---- 1992 (01br): canvass tibble with candidate, party (DEM/REP/OTHER) ----
e <- eval_named("01br_house_county_arizona_1992.R", c("cand", "canvass"))
d92 <- e$canvass %>% mutate(pg = party, sfx = sub(".*\\((NL|L|I|Write-in)\\)\\s*$", "\\1", candidate, ignore.case = TRUE),
         sfx = ifelse(sfx == candidate, NA_character_, sfx)) %>%
  transmute(county, district, candidate = trimws(sub("\\s*\\((NL|L|I|Write-in)\\)\\s*$", "", candidate, ignore.case = TRUE)),
            party = ifelse(pg == "DEM", "Democratic", ifelse(pg == "REP", "Republican", ifelse(!is.na(sfx) & sfx %in% names(party_lab), unname(party_lab[sfx]), "Other"))),
            party_group = pg, votes)
finish(d92, 1992, "az_1992")

## ---- 1994 (01bs): only D/R/L columns per district-county; NO candidate names in the script ----
e <- eval_named("01bs_house_county_arizona_1994.R", c("canvass"))
d94 <- e$canvass %>% pivot_longer(c(dem, rep, lib), names_to = "k", values_to = "votes") %>% filter(!(k == "lib" & votes == 0 & district == 3)) %>%
  transmute(county, district, candidate = c(dem = "Democratic candidate", rep = "Republican candidate", lib = "Libertarian candidate")[k],
            party = c(dem = "Democratic", rep = "Republican", lib = "Libertarian")[k], party_group = c(dem = "DEM", rep = "REP", lib = "OTHER")[k], votes)
finish(d94, 1994, "az_1994")

## ---- 1996 (01bt): canvass with surname candidates and party codes ----
e <- eval_named("01bt_house_county_arizona_1996.R", c("cand", "canvass"))
d96 <- e$canvass %>% transmute(county, district, candidate, party_raw = party, votes) %>%
  mutate(party = unname(party_lab[party_raw]), party_group = ifelse(party_raw == "DEM", "DEM", ifelse(party_raw == "REP", "REP", "OTHER"))) %>% select(-party_raw)
finish(d96, 1996, "az_1996")

## ---- 1998 (01bu): parse the canvass PDF text layer exactly as 01bu ----
e <- eval_named("01bu_house_county_arizona_1998.R", c("AZ_COUNTIES", "NUM", "row_re", "hdr_re", "to_n"))
pdf <- file.path(RAW_ROOT, "arizona_historical", "1998", "Canvass1998GE.pdf"); stopifnot(file.exists(pdf))
lines <- system2("pdftotext", c("-layout", shQuote(pdf), "-"), stdout = TRUE)
hdr_idx <- grep(e$hdr_re, lines); stopifnot(length(hdr_idx) == 6); rows <- list()
for (h in hdr_idx) {
  dist <- as.integer(sub(e$hdr_re, "\\1", lines[h])); i <- h + 1
  while (i <= length(lines) && !nzchar(trimws(lines[i]))) i <- i + 1
  while (i <= length(lines) && grepl(e$row_re, lines[i], perl = TRUE)) {
    m <- regmatches(lines[i], regexec(e$row_re, lines[i], perl = TRUE))[[1]]; vals <- e$to_n(strsplit(trimws(m[4]), "\\s+")[[1]]); stopifnot(length(vals) == 16)
    rows[[length(rows) + 1]] <- tibble(district = dist, party_raw = m[2], candidate = trimws(m[3]), county = c(e$AZ_COUNTIES, "TOTAL"), votes = vals); i <- i + 1 } }
d98 <- bind_rows(rows) %>% filter(county != "TOTAL") %>%
  transmute(county, district, candidate = trimws(sub("\\s*\\((?i:write-in)\\)\\s*$", "", sub("\\s*\\*$", "", candidate), perl = TRUE)),
            party = unname(party_lab[party_raw]), party_group = ifelse(party_raw == "DEM", "DEM", ifelse(party_raw == "REP", "REP", "OTHER")), votes)
finish(d98, 1998, "az_1998")
