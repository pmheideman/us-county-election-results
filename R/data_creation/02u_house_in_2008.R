## Indiana 2008 U.S. House by county, from the Indiana Election Division election report "2008 General Election - Election Report - State of Indiana"
## (R/data/county_house_files/IN_2008.pdf, 104 pages, image-only scan; the general-election "United States Representative" section is PDF pages 66-67, printed as
## "General Results Page 10-11 of 46"): per district, per candidate (party) the county/vote pairs and the printed candidate total.
## The pages were READ FROM 200-dpi IMAGES by hand into R/data/raw_house_county_open_states/indiana_official/in_2008_transcription.csv
## (OCR at 100 dpi was only used to locate the pages). Checks: county votes per candidate add up to the printed candidate total (24 of 24 tie on first reading);
## district totals equal Wikipedia's; county count per district; House total vs the county's presidential total in the panel.
## party_group is computed BEFORE the party code is replaced by its label (bug fixed in 02u_house_in_2002.R and 02u_house_in_2010.R).
## Outputs: R/output/long/he_in_2008.rds, R/output/elect_he_cty_in_2008.rds (NEW files); R/output/in_2008_vs_old_openelections.csv (comparison with the old panel rows).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
tr <- read_csv(file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "indiana_official", "in_2008_transcription.csv"), col_types = cols(district = "c", candidate = "c", party = "c", printed_total = "d", county = "c", votes = "d"))
chk <- tr %>% group_by(district, candidate, party) %>% summarise(sum = sum(votes), printed = first(printed_total), n_cty = n(), .groups = "drop") %>% mutate(ok = sum == printed)
cat("candidate lines:", nrow(chk), "| county sums equal the printed candidate total:", sum(chk$ok), "\n"); stopifnot(all(chk$ok))
cat("counties per district:", paste(names(table(tr$district[!duplicated(tr[, c("district", "county")])])), table(tr$district[!duplicated(tr[, c("district", "county")])]), sep = "=", collapse = ", "), "\n")
xw <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  filter(state == "INDIANA", !is.na(county_fips)) %>% distinct(county_name, county_fips)
norm <- function(z) gsub("[^A-Z]", "", toupper(z)); xw <- xw %>% mutate(key = norm(county_name)) %>% distinct(key, county_fips); stopifnot(nrow(xw) == 92)
tr <- tr %>% mutate(county_fips = xw$county_fips[match(norm(county), xw$key)]); stopifnot(!anyNA(tr$county_fips), n_distinct(tr$county_fips) == 92)
PARTY <- c(D = "Democratic", R = "Republican", L = "Libertarian", `W-R` = "Write-In (Republican)")
raw <- tr %>% transmute(year = 2008L, county_fips, district, candidate, party_group = case_when(party == "D" ~ "DEM", party == "R" ~ "REP", TRUE ~ "OTHER"), party = PARTY[party], votes)   # party_group BEFORE the label overwrites the code
long <- finalize_long(raw, "in_2008")
## hand-restored after finalize_long (which lower-cases nicknames and strips accents / punctuation)
NAMES <- c("Michael A. (mike) Montagano" = "Michael A. (Mike) Montagano", "Andre D. Carson" = "Andr\u00e9 D. Carson"); hit <- long$candidate %in% names(NAMES); long$candidate[hit] <- NAMES[long$candidate[hit]]
long$party[long$party == "Write-In (republican)"] <- "Write-In (Republican)"
save_long(long, "he_in_2008")
shares <- derive_shares(long) %>% transmute(state = "INDIANA", year, cty_fips, sample, demovote, repuvote, totalvote); saveRDS(shares, file.path(OUTPUT_DIR, "elect_he_cty_in_2008.rds"))
print(check_long_vs_source(long, file.path(OUTPUT_DIR, "elect_he_cty_in_2008.rds")) %>% select(source, keys_source, matched, mismatched, pass))
cat("counties:", n_distinct(long$county_fips), " split counties:", sum(long %>% group_by(county_fips) %>% summarise(d = n_distinct(district)) %>% pull(d) > 1), " candidates:", n_distinct(paste(long$district, long$candidate)), "\n")
cat("median Democratic share", round(median(shares$demovote), 3), "| Republican", round(median(shares$repuvote), 3), "| counties with a zero Democratic or Republican share:", sum(shares$demovote == 0 | shares$repuvote == 0), "\n"); stopifnot(median(shares$demovote) > 0.2, median(shares$repuvote) > 0.2)
## district totals vs Wikipedia (its 2008 Indiana page lists the votes per candidate)
wp <- readLines(file.path(PROJECT_ROOT, "R", "data", "raw_election", "wikipedia_house", "2008_United_States_House_of_Representatives_elections_in_Indiana.txt"), warn = FALSE, encoding = "UTF-8")
vt <- as.numeric(gsub("[^0-9]", "", sub("^\\s*\\|\\s*votes\\s*=\\s*", "", grep("^\\s*\\|\\s*votes\\s*=", wp, value = TRUE))))
mine <- tr %>% distinct(district, candidate, printed_total) %>% pull(printed_total)
cat("Wikipedia vote figures found:", length(vt), "; of my ", length(mine), " candidate totals, present on the Wikipedia page:", sum(mine %in% vt), "\n"); print(mine[!mine %in% vt])
## sanity: House total vs the presidential total of the same year in the panel
pe <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")) %>% filter(sample == "PE", year == 2008, cty_fips %/% 1000 == 18) %>% select(cty_fips, pe = totalvote)
r <- shares %>% left_join(pe, by = "cty_fips") %>% mutate(ratio = totalvote / pe)
cat("counties with a 2008 presidential total:", sum(!is.na(r$pe)), "; House / presidential total: min", round(min(r$ratio, na.rm = TRUE), 3), "median", round(median(r$ratio, na.rm = TRUE), 3), "max", round(max(r$ratio, na.rm = TRUE), 3), "\n")
## the old OpenElections-based panel rows
old <- readRDS(file.path(OUTPUT_DIR, "elect_he_cty_in.rds")) %>% filter(year == 2008) %>% select(cty_fips, o_dem = demovote, o_rep = repuvote, o_tot = totalvote)
d <- shares %>% left_join(old, by = "cty_fips") %>% mutate(status = case_when(is.na(o_tot) ~ "missing in old rows", abs(demovote - o_dem) < 1e-9 & abs(repuvote - o_rep) < 1e-9 & abs(totalvote - o_tot) < 0.5 ~ "identical", TRUE ~ "differs"), rel = totalvote / o_tot)
cat("old rows:", nrow(old), "| new 92: identical", sum(d$status == "identical"), ", differs", sum(d$status == "differs"), ", missing in old", sum(d$status == "missing in old rows"), "\n")
split_c <- long %>% group_by(county_fips) %>% summarise(nd = n_distinct(district), .groups = "drop") %>% filter(nd > 1) %>% pull(county_fips)
cat("of the ", length(split_c), " split counties, in the old rows: differs", sum(d$status[d$cty_fips %in% split_c] == "differs"), ", missing", sum(d$status[d$cty_fips %in% split_c] == "missing in old rows"), "\n")
print(as.data.frame(d %>% filter(status == "differs") %>% mutate(share_of_official = round(o_tot / totalvote, 2)) %>% select(cty_fips, totalvote, o_tot, share_of_official, demovote, o_dem) %>% arrange(share_of_official)))
write.csv(d %>% filter(status != "identical"), file.path(OUTPUT_DIR, "in_2008_vs_old_openelections.csv"), row.names = FALSE)
