## Arkansas 2000 U.S. House, county level, from the official Arkansas Secretary of State file R/data/county_house_files/AR_general2000c.zip
## (general2000c.txt, pipe-delimited: Office|Canidate[sic]|Party|<75 county columns "Arkansas County", "Ashley County", ...|), each line ends with a trailing pipe (empty 79th column, dropped).
##
## Layout / gotchas:
##   * one row per office x candidate; a county column is EMPTY when the candidate's race is not on that county's ballot and "0" when it is
##     and the candidate got no votes; split counties therefore have values under several districts (kept by district; shares sum them).
##   * House offices are "U.S. Congress District 01/02/04". District 03 (Asa Hutchinson) has NO row: unopposed, so Arkansas printed no race
##     (no votes are invented). The file prints NO totals, so verification is against Wikipedia and the county presidential totals.
##   * candidate names carry titles ("Congressman Marion Berry", "State Senator Mike Ross"), stripped here.
##   * only Democrat / Republican lines are DEM / REP; the "Write-In" line is OTHER.
## Outputs: R/output/long/he_ar_2000.rds, R/output/elect_he_cty_ar_2000.rds (NEW files; nothing existing is modified).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
save_step <- function(df, name) invisible(df)     # never write shares files through the old helper
RAW <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "arkansas_official"); dir.create(RAW, showWarnings = FALSE, recursive = TRUE)
zipf <- file.path(PROJECT_ROOT, "R", "data", "county_house_files", "AR_general2000c.zip")
utils::unzip(zipf, exdir = RAW, overwrite = FALSE)
x <- read_delim(file.path(RAW, "general2000c.txt"), delim = "|", col_types = cols(.default = "c"), show_col_types = FALSE, progress = FALSE)
stopifnot(ncol(x) == 79, all(is.na(x[[79]]) | !nzchar(x[[79]])))   # every line ends with a trailing "|": an empty 79th column
x <- x[, 1:78]
stopifnot(names(x)[1:3] == c("Office", "Canidate", "Party"))
counties <- sub(" County$", "", names(x)[-(1:3)]); stopifnot(length(counties) == 75)

xw <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  filter(state == "ARKANSAS", !is.na(county_fips)) %>% distinct(county_name, county_fips)
norm <- function(z) gsub("[^A-Z]", "", toupper(z)); xw <- xw %>% mutate(key = norm(county_name)) %>% distinct(key, county_fips)
stopifnot(nrow(xw) == 75, all(norm(counties) %in% xw$key))

h <- x %>% filter(grepl("^U\\.S\\. Congress District", Office))
message("House rows: ", nrow(h), "; districts: ", paste(sort(unique(sub(".*District ", "", h$Office))), collapse = ","))
long_raw <- purrr::map_dfr(seq_len(nrow(h)), function(i) {
  v <- as.character(unlist(h[i, -(1:3)])); keep <- !is.na(v) & nzchar(v)
  tibble(year = 2000L, county = counties[keep], district = sub(".*District ", "", h$Office[i]),
         candidate = sub("^(Congressman|Congresswoman|State Senator|State Representative|Representative|Senator|Sen\\.|Rep\\.)\\s+", "", h$Canidate[i]),
         party = h$Party[i], votes = as.numeric(v[keep]))
})
stopifnot(!anyNA(long_raw$votes))
long_raw <- long_raw %>% mutate(county_fips = xw$county_fips[match(norm(county), xw$key)],
                                party_group = case_when(party == "Democrat" ~ "DEM", party == "Republican" ~ "REP", TRUE ~ "OTHER"))
stopifnot(!anyNA(long_raw$county_fips))
long <- finalize_long(long_raw %>% select(year, county_fips, district, candidate, party, party_group, votes), "ar_2000")
save_long(long, "he_ar_2000")
shares <- derive_shares(long) %>% transmute(state = "ARKANSAS", year, cty_fips, sample, demovote, repuvote, totalvote)
saveRDS(shares, file.path(OUTPUT_DIR, "elect_he_cty_ar_2000.rds"))
check_long_vs_source(long, file.path(OUTPUT_DIR, "elect_he_cty_ar_2000.rds"))

## ---- verification -----------------------------------------------------------------------------------------------------------------------
cat("\ncounties per district (contested districts must show all counties that touch them):\n")
print(as.data.frame(long %>% group_by(district) %>% summarise(counties = n_distinct(county_fips), cands = n_distinct(candidate), votes = sum(votes))))
cat("counties covered:", n_distinct(long$county_fips), "of 75;  split counties (>1 district):", sum(long %>% group_by(county_fips) %>% summarise(d = n_distinct(district)) %>% pull(d) > 1), "\n")
cat("\nstatewide candidate totals:\n"); print(as.data.frame(long %>% group_by(district, candidate, party) %>% summarise(votes = sum(votes), .groups = "drop")))
p <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")) %>% filter(sample == "PE", year == 2000) %>% select(cty_fips, pe = totalvote)
r <- shares %>% left_join(p, by = "cty_fips") %>% mutate(ratio = totalvote / pe)
cat("\nHouse / presidential total 2000 (counties with >=1 contested district):", "min", round(min(r$ratio, na.rm = TRUE), 2), "median", round(median(r$ratio, na.rm = TRUE), 2), "max", round(max(r$ratio, na.rm = TRUE), 2), "\n")
print(as.data.frame(r %>% filter(ratio < 0.5 | ratio > 1.05) %>% select(cty_fips, totalvote, pe, ratio)))
wf <- file.path(PROJECT_ROOT, "R/data/raw_election/wikipedia_house/2000_United_States_House_of_Representatives_elections_in_Arkansas.txt")
if (!file.exists(wf)) system2("curl", c("-sL", "-A", shQuote("Mozilla/5.0 (X11; Linux x86_64) Chrome/124.0"), "-o", shQuote(wf), shQuote("https://en.wikipedia.org/w/index.php?title=2000_United_States_House_of_Representatives_elections_in_Arkansas&action=raw")))
cat("\nwikipedia file: ", file.exists(wf), file.size(wf), "\n")
