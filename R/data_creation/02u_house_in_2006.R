## Indiana 2006 U.S. House by county, from the "2006 Indiana Election Report" (Indiana Election Division, printed March 2007;
## R/data/county_house_files/IN_2006_Election_Report.pdf, 110 pages, image-only scans; the general election returns start at pdf page 63 = "General Results Page 1 of 45";
## "United States Representative" is on pdf pages 69-70 = report pages 7-8; the primary returns are pp. 8-62). Same layout as the 2002 and 2010 reports:
## district -> candidate (party) with printed total -> rows of "County votes". Every candidate name, party, printed total and county cell was READ FROM THE PAGE IMAGES
## (200 dpi renders) into R/data/raw_house_county_open_states/indiana_official/in_2006_transcription.csv and in_2006_printed_totals.csv.
## Blank cells are counts not printed (the report prints 0 for real zeros); District 7 write-in John Leroy Plemons (W-I) has no county cell and no total in the report and is not included.
## Checks: county sums == printed candidate totals (23 of 23); county count per district (same 9-district map as 2002/2010); district totals == Wikipedia (2006 elections in Indiana);
## House total vs the Treasurer of State 2006 total per county (same report, page 68; independent statewide race).
## Outputs: R/output/long/he_in_2006.rds, R/output/elect_he_cty_in_2006.rds, R/output/in_2006_vs_old_openelections.csv (NEW files).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "indiana_official")
tr <- read_csv(file.path(DIR, "in_2006_transcription.csv"), col_types = cols(district = "i", .default = "c", votes = "d", year = "i")) %>% mutate(district = sprintf("%02d", district))
printed <- read_csv(file.path(DIR, "in_2006_printed_totals.csv"), col_types = cols(district = "i", .default = "c", printed = "d")) %>% mutate(district = sprintf("%02d", district))
chk <- tr %>% group_by(district, candidate, party) %>% summarise(sum = sum(votes), n_cty = n(), .groups = "drop") %>% left_join(printed, by = c("district", "candidate", "party")) %>% mutate(ok = sum == printed)
cat("candidate lines:", nrow(chk), "| county sums equal the printed candidate total:", sum(chk$ok), "\n"); stopifnot(all(chk$ok), nrow(chk) == nrow(printed))
cty_n <- tr %>% distinct(district, county) %>% count(district); cat("counties per district:", paste(cty_n$district, cty_n$n, sep = "=", collapse = ", "), "\n")
stopifnot(identical(cty_n$n, c(5L, 12L, 8L, 12L, 11L, 19L, 1L, 18L, 20L)))          # same district map as 2002 and 2010
## district totals vs Wikipedia (R, D, other per district; Wikipedia lists no write-ins in districts 5 and 9: 18 and 33 votes)
wp <- tribble(~district, ~rep, ~dem, ~other, "01", 40146, 104195, 5266, "02", 88300, 103561, 0, "03", 95421, 80357, 0, "04", 111057, 66986, 0, "05", 133118, 64362, 7431, "06", 115266, 76812, 0, "07", 64304, 74750, 0, "08", 83704, 131019, 0, "09", 100469, 110454, 9893)
mine <- tr %>% group_by(district) %>% summarise(rep = sum(votes[party == "R"]), dem = sum(votes[party == "D"]), other = sum(votes[party %in% c("L", "I")]), write_in = sum(votes[party == "W"]), .groups = "drop")
w <- wp %>% inner_join(mine, by = "district", suffix = c(".wp", ".me")); cat("district totals equal Wikipedia (R, D, L/I) in", sum(w$rep.wp == w$rep.me & w$dem.wp == w$dem.me & w$other.wp == w$other.me), "of 9 districts; write-ins in this report:", paste(mine$write_in[mine$write_in > 0], collapse = ", "), "\n")
stopifnot(all(w$rep.wp == w$rep.me & w$dem.wp == w$dem.me & w$other.wp == w$other.me))

xw <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  filter(state == "INDIANA", !is.na(county_fips)) %>% distinct(county_name, county_fips)
norm <- function(z) gsub("[^A-Z]", "", toupper(z)); xw <- xw %>% mutate(key = norm(county_name)) %>% distinct(key, county_fips); stopifnot(nrow(xw) == 92)
tr <- tr %>% mutate(county_fips = xw$county_fips[match(norm(county), xw$key)]); stopifnot(!anyNA(tr$county_fips))
PARTY <- c(D = "Democratic", R = "Republican", L = "Libertarian", I = "Independent", W = "Write-In")
raw <- tr %>% transmute(year = 2006L, county_fips, district, candidate, party_group = case_when(party == "D" ~ "DEM", party == "R" ~ "REP", TRUE ~ "OTHER"), party = PARTY[party], votes)     # party_group BEFORE the label overwrites the code
long <- finalize_long(raw, "in_2006")
## finalize_long strips punctuation from names: hand-restore the ones affected
NAMES <- c("Peter J Visclosky" = "Peter J. Visclosky", "Mark J Leyva" = "Mark J. Leyva", "Charles E Barman" = "Charles E. Barman", "Barry A Welsh" = "Barry A. Welsh", "Julia M Carson" = "Julia M. Carson",
           "John N Hostettler" = "John N. Hostettler", "Baron P Hill" = "Baron P. Hill", "D Eric Schansberg" = "D. Eric Schansberg", "Donald W Mantooth" = "Donald W. Mantooth")
key <- function(z) gsub("[^a-z]", "", tolower(z)); nm <- setNames(unname(NAMES), key(names(NAMES))); hit <- key(long$candidate) %in% names(nm); long$candidate[hit] <- nm[key(long$candidate[hit])]
cat("candidates:", paste(unique(long$candidate), collapse = " | "), "\n")
save_long(long, "he_in_2006")
shares <- derive_shares(long) %>% transmute(state = "INDIANA", year, cty_fips, sample, demovote, repuvote, totalvote); saveRDS(shares, file.path(OUTPUT_DIR, "elect_he_cty_in_2006.rds"))
stopifnot(median(shares$demovote) > 0.2, median(shares$repuvote) > 0.2)                     # guards against a party-mapping error (check_long_vs_source cannot see it)
cat("median Democratic share", round(median(shares$demovote), 3), "| median Republican share", round(median(shares$repuvote), 3), "\n")
print(check_long_vs_source(long, file.path(OUTPUT_DIR, "elect_he_cty_in_2006.rds")) %>% select(source, keys_source, matched, mismatched, pass))
split <- long %>% group_by(county_fips) %>% summarise(d = n_distinct(district)) %>% filter(d > 1) %>% pull(county_fips)
cat("counties:", n_distinct(long$county_fips), " split counties (", length(split), "):", paste(sort(split), collapse = ","), "\n")
## House total vs an independent statewide race of the same report: Treasurer of State (pdf page 68; two candidates, all 92 counties). The U.S. Senate race is no check in 2006 (no Democratic candidate:
## Senate totals are 8-35% lower than House totals in every county). The Treasurer page was OCR'd (tesseract --psm 4, in_2006_treasurer_ocr.txt): 92 numbers per candidate in the printed order
## (6 per line, alphabetical), and they add up to the printed statewide totals (Griffin 771,610 exactly; Mourdock 833,531 after ONE misread cell, Cass 9,176 -> 6,176 read from the image).
tl <- readLines(file.path(DIR, "in_2006_treasurer_ocr.txt")); gi <- grep("Griffin", tl)[1]; mi <- grep("ourdock|owrdock", tl)[1]
nums <- function(block) unlist(lapply(block[!grepl("Election Report|Tuesday|Statewide|Treasurer|Griffin|ourdock|owrdock", block)], function(l) as.numeric(gsub(",", "", regmatches(l, gregexpr("(?<![0-9,])[0-9]{1,3}(,[0-9]{3})+|(?<![0-9,])[0-9]{1,3}(?![0-9])", l, perl = TRUE))[[1]]))))
tR <- nums(tl[mi:(gi - 1)]); tD <- nums(tl[gi:length(tl)]); stopifnot(length(tR) == 92, length(tD) == 92)
IN_COUNTIES <- c("Adams","Allen","Bartholomew","Benton","Blackford","Boone","Brown","Carroll","Cass","Clark","Clay","Clinton","Crawford","Daviess","Dearborn","Decatur","DeKalb","Delaware","Dubois","Elkhart","Fayette","Floyd","Fountain","Franklin","Fulton","Gibson","Grant","Greene","Hamilton","Hancock","Harrison","Hendricks","Henry","Howard","Huntington","Jackson","Jasper","Jay","Jefferson","Jennings","Johnson","Knox","Kosciusko","LaGrange","Lake","LaPorte","Lawrence","Madison","Marion","Marshall","Martin","Miami","Monroe","Montgomery","Morgan","Newton","Noble","Ohio","Orange","Owen","Parke","Perry","Pike","Porter","Posey","Pulaski","Putnam","Randolph","Ripley","Rush","St. Joseph","Scott","Shelby","Spencer","Starke","Steuben","Sullivan","Switzerland","Tippecanoe","Tipton","Union","Vanderburgh","Vermillion","Vigo","Wabash","Warren","Warrick","Washington","Wayne","Wells","White","Whitley")
stopifnot(length(IN_COUNTIES) == 92, tR[match("Cass", IN_COUNTIES)] == 9176); tR[match("Cass", IN_COUNTIES)] <- 6176
stopifnot(sum(tR) == 833531, sum(tD) == 771610)
tre <- tibble(cty_fips = xw$county_fips[match(norm(IN_COUNTIES), xw$key)], tre = tR + tD); stopifnot(!anyNA(tre$cty_fips))
r <- shares %>% inner_join(tre, by = "cty_fips") %>% mutate(ratio = totalvote / tre)
cat("House 2006 / Treasurer of State 2006 total per county (", nrow(r), " counties): min", round(min(r$ratio), 3), "median", round(median(r$ratio), 3), "max", round(max(r$ratio), 3), "\n"); print(as.data.frame(r %>% filter(ratio < 0.93 | ratio > 1.07) %>% select(cty_fips, totalvote, tre, ratio)))
write_csv(tibble(district = "n/a (check race)", county = "Cass", candidate = "Richard E. Mourdock (Treasurer of State, used only as the independent county check)", ocr_value = 9176, corrected_value = 6176,
                 how_verified = "tesseract misread 6 as 9; page image (pdf p.68) shows 6,176; with it the 92 county cells add up to the printed statewide total 833,531 (Griffin's 92 cells add up to 771,610 without any correction)"),
          file.path(PROJECT_ROOT, "R", "output", "in_ocr_corrections_2006.csv"))
## comparison with the old OpenElections rows in the panel (partial, 66 counties)
old <- readRDS(file.path(OUTPUT_DIR, "elect_he_cty_in.rds")) %>% filter(year == 2006) %>% select(cty_fips, o_tot = totalvote, o_dem = demovote, o_rep = repuvote)
cmp <- shares %>% left_join(old, by = "cty_fips") %>% mutate(status = case_when(is.na(o_tot) ~ "missing in old data", abs(totalvote - o_tot) < 0.5 & abs(demovote - o_dem) < 1e-9 & abs(repuvote - o_rep) < 1e-9 ~ "identical", TRUE ~ "old differs"))
cat("\nvs the old OpenElections rows for 2006:\n"); print(table(cmp$status)); print(as.data.frame(cmp %>% filter(status == "old differs") %>% mutate(rel = round(o_tot / totalvote, 2)) %>% select(cty_fips, totalvote, o_tot, rel)))
cat("counties missing in the old data:", paste(cmp$cty_fips[cmp$status == "missing in old data"], collapse = ", "), "\n")
write_csv(cmp %>% select(cty_fips, totalvote, o_tot, status), file.path(PROJECT_ROOT, "R", "output", "in_2006_vs_old_openelections.csv"))
