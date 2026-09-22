## Indiana 2002 U.S. House by county, from the Indiana Election Division / Secretary of State "2002 General Election - Election Report - State of Indiana"
## (R/data/county_house_files/IN_2002.pdf, 47 pages, native text layer, `pdftotext -layout`): under "United States Representative", a block per district; per candidate a line
## "Name (P)   total [*]" followed by rows of "County votes" pairs (several per line, lines may break in the middle of a candidate's county list, and a district may continue on the next page).
## Checks: county votes per candidate add up to the printed candidate total; county count per district; House total vs the Secretary of State race total in the same report.
## Outputs: R/output/long/he_in_2002.rds, R/output/elect_he_cty_in_2002.rds (NEW files).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
PDF <- file.path(PROJECT_ROOT, "R", "data", "county_house_files", "IN_2002.pdf")
TXT <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "indiana_official", "IN_2002.txt"); dir.create(dirname(TXT), showWarnings = FALSE, recursive = TRUE)
if (!file.exists(TXT) || file.size(TXT) == 0) system2("pdftotext", c("-layout", shQuote(PDF), shQuote(TXT)))
L <- readLines(TXT, warn = FALSE, encoding = "UTF-8")
sec <- function(title) { i <- which(trimws(L) == title); i }               # section headings ("United States Representative", "Secretary of State", ...)
starts <- which(trimws(L) == "United States Representative")
ends <- which(grepl("^\\s*(State Senator|State Representative)\\s*$", L)); stopifnot(length(starts) >= 3)
end_ln <- min(ends[ends > max(starts)]) - 1
rng <- seq(min(starts), end_ln)
DIST <- "^District ([0-9]+)\\s+Votes\\s*$"; CAND <- "^(.+?) \\(([A-Za-z])\\)\\s+([0-9][0-9,]*)( \\*)?\\s*$"
PAIR <- "([A-Z][A-Za-z.' ]*?)\\s{2,}([0-9][0-9,]*)"
cur_d <- NA; cur_c <- NULL; rows <- list(); tot <- list()
for (i in rng) { ln <- L[i]
  if (grepl(DIST, trimws(ln))) { cur_d <- sprintf("%02d", as.integer(sub(DIST, "\\1", trimws(ln)))); cur_c <- NULL; next }
  m <- regmatches(ln, regexec(CAND, trimws(ln)))[[1]]
  if (length(m)) { cur_c <- list(name = m[2], party = m[3], total = as.numeric(gsub(",", "", m[4]))); tot[[length(tot) + 1]] <- data.frame(district = cur_d, candidate = cur_c$name, party = cur_c$party, printed = cur_c$total); next }
  if (is.null(cur_c) || grepl("General Results Page|Election Report|Tuesday,|^\\s*$|United States Representative", ln)) next
  pr <- regmatches(ln, gregexpr(PAIR, ln, perl = TRUE))[[1]]
  for (p in pr) { mm <- regmatches(p, regexec(PAIR, p, perl = TRUE))[[1]]; rows[[length(rows) + 1]] <- data.frame(district = cur_d, candidate = cur_c$name, party = cur_c$party, county = trimws(mm[2]), votes = as.numeric(gsub(",", "", mm[3]))) }
}
raw <- bind_rows(rows); tot <- bind_rows(tot)
chk <- raw %>% group_by(district, candidate, party) %>% summarise(sum = sum(votes), n_cty = n(), .groups = "drop") %>% full_join(tot, by = c("district", "candidate", "party")) %>% mutate(ok = sum == printed)
cat("candidate lines:", nrow(chk), "| county sums equal the printed candidate total:", sum(chk$ok, na.rm = TRUE), "\n"); print(as.data.frame(chk %>% filter(!ok | is.na(ok))))
stopifnot(all(chk$ok))
cat("counties per district:", paste(names(table(raw$district[!duplicated(raw[, c("district", "county")])])), table(raw$district[!duplicated(raw[, c("district", "county")])]), sep = "=", collapse = ", "), "\n")

xw <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  filter(state == "INDIANA", !is.na(county_fips)) %>% distinct(county_name, county_fips)
norm <- function(z) gsub("[^A-Z]", "", toupper(z)); xw <- xw %>% mutate(key = norm(county_name)) %>% distinct(key, county_fips); stopifnot(nrow(xw) == 92)
raw <- raw %>% mutate(county_fips = xw$county_fips[match(norm(county), xw$key)]); stopifnot(!anyNA(raw$county_fips))
PARTY <- c(D = "Democratic", R = "Republican", L = "Libertarian", G = "Green", W = "Write-In", I = "Independent")
raw <- raw %>% transmute(year = 2002L, county_fips, district, candidate, party_group = case_when(party == "D" ~ "DEM", party == "R" ~ "REP", TRUE ~ "OTHER"), party = PARTY[party], votes)     # party_group BEFORE the label overwrites the code
long <- finalize_long(raw, "in_2002"); save_long(long, "he_in_2002")
shares <- derive_shares(long) %>% transmute(state = "INDIANA", year, cty_fips, sample, demovote, repuvote, totalvote); saveRDS(shares, file.path(OUTPUT_DIR, "elect_he_cty_in_2002.rds"))
print(check_long_vs_source(long, file.path(OUTPUT_DIR, "elect_he_cty_in_2002.rds")) %>% select(source, keys_source, matched, mismatched, pass))
cat("counties:", n_distinct(long$county_fips), " split counties:", sum(long %>% group_by(county_fips) %>% summarise(d = n_distinct(district)) %>% pull(d) > 1), "\n")
## sanity vs the Secretary of State race in the same report (statewide race, all counties)
i0 <- which(trimws(L) == "Secretary of State")[1]; i1 <- which(grepl("^\\s*(Auditor of State|Treasurer of State|Clerk of Courts|Clerk of the Supreme Court)", L) & seq_along(L) > i0)[1]
sos <- list(); cn <- NULL
for (i in i0:(i1 - 1)) { ln <- L[i]; m <- regmatches(ln, regexec(CAND, trimws(ln)))[[1]]; if (length(m)) { cn <- m[2]; next }
  if (is.null(cn) || grepl("General Results Page|Election Report|Tuesday,|^\\s*$", ln)) next
  for (p in regmatches(ln, gregexpr(PAIR, ln, perl = TRUE))[[1]]) { mm <- regmatches(p, regexec(PAIR, p, perl = TRUE))[[1]]; sos[[length(sos) + 1]] <- data.frame(county = trimws(mm[2]), votes = as.numeric(gsub(",", "", mm[3]))) } }
sos <- bind_rows(sos) %>% mutate(county_fips = xw$county_fips[match(norm(county), xw$key)]) %>% group_by(county_fips) %>% summarise(sos = sum(votes), .groups = "drop")
r <- shares %>% left_join(sos, by = c("cty_fips" = "county_fips")) %>% mutate(ratio = totalvote / sos)
cat("counties with an SoS total:", sum(!is.na(r$sos)), "; House / SoS total: min", round(min(r$ratio, na.rm = TRUE), 2), "median", round(median(r$ratio, na.rm = TRUE), 2), "max", round(max(r$ratio, na.rm = TRUE), 2), "(split counties have only a part of the vote in the counted districts? no - shares sum all districts)\n")
old <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")) %>% filter(sample == "HE", year == 2002, cty_fips %/% 1000 == 18) %>% select(cty_fips, o_dem = demovote, o_rep = repuvote, o_tot = totalvote)
d <- shares %>% inner_join(old, by = "cty_fips") %>% mutate(same = abs(demovote - o_dem) < 1e-9 & abs(repuvote - o_rep) < 1e-9 & abs(totalvote - o_tot) < 0.5)
cat("panel's existing 2002 Indiana rows:", nrow(old), "; identical:", sum(d$same), "; differing:", sum(!d$same), "\n"); print(as.data.frame(d %>% filter(!same) %>% select(cty_fips, totalvote, o_tot, demovote, o_dem, repuvote, o_rep)))
