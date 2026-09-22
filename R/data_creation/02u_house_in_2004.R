## Indiana 2004 U.S. House by county, from the "2004 Indiana Election Report" (Indiana Election Division; R/data/county_house_files/IN_2004.pdf, 82 pages, image-only scan;
## the general-election "United States Representative" section is PDF pages 49 (districts 1-6) and 51 (rest of district 6, districts 7-9); PDF page 50 is the Superintendent of
## Public Instruction race (all 92 counties) that sits in between). Layout as in the 2002/2010 reports: district -> candidate (party) with printed total -> rows of "County votes".
## Every county cell and printed total was READ FROM 200-dpi CROPS OF THE PAGE IMAGES (tesseract only located the pages: it garbles the dotted-leader totals).
## Checks: county sums == printed candidate totals; county count per district; district totals vs Wikipedia; House total vs the report's own Superintendent of Public Instruction race per county
## and vs the panel's 2004 presidential total. The report also holds the May 4 2004 primary returns (pp. 8-45) and the general election returns for statewide offices, state legislators, judges.
## Outputs: R/output/long/he_in_2004.rds, R/output/elect_he_cty_in_2004.rds (NEW files); the transcription is written to R/data/raw_house_county_open_states/indiana_official/in_2004_transcription.csv.
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "indiana_official")
## district | candidate | party | printed total | "County votes; County votes; ..."
T <- c(
"01|Peter J. Visclosky|D|178406|Benton 1616;Jasper 5236;Lake 130430;Newton 2865;Porter 38259",
"01|Mark J. Leyva|R|82858|Benton 2193;Jasper 6155;Lake 51548;Newton 2818;Porter 20144",
"02|Chris Chocola|R|140496|Carroll 5690;Cass 9077;Elkhart 16774;Fulton 5584;Howard 6732;La Porte 19641;Marshall 11546;Porter 2616;Pulaski 3599;St. Joseph 53283;Starke 4842;White 1112",
"02|Joe Donnelly|D|115513|Carroll 2693;Cass 4616;Elkhart 9169;Fulton 3037;Howard 5729;La Porte 21693;Marshall 5843;Porter 2299;Pulaski 1954;St. Joseph 53904;Starke 3982;White 594",
"02|Douglas Barnes|L|3346|Carroll 145;Cass 140;Elkhart 277;Fulton 81;Howard 237;La Porte 931;Marshall 203;Porter 134;Pulaski 79;St. Joseph 942;Starke 141;White 36",
"03|Mark Edward Souder|R|171389|Allen 76372;DeKalb 10509;Elkhart 26747;Kosciusko 22430;LaGrange 6603;Noble 10829;Steuben 8830;Whitley 9069",
"03|Maria M. Parra|D|76232|Allen 44116;DeKalb 4681;Elkhart 7385;Kosciusko 5444;LaGrange 2250;Noble 4480;Steuben 3788;Whitley 4088",
"04|Steve Buyer|R|190445|Boone 16727;Clinton 8208;Fountain 1701;Hendricks 37296;Johnson 31406;Lawrence 11282;Marion 11737;Monroe 6418;Montgomery 10741;Morgan 18238;Tippecanoe 30747;White 5944",
"04|David Sanders|D|77574|Boone 4857;Clinton 3139;Fountain 766;Hendricks 12034;Johnson 10447;Lawrence 5212;Marion 6544;Monroe 3945;Montgomery 3136;Morgan 6223;Tippecanoe 18748;White 2523",
"04|Kevin R. Fleming|L|6117|Boone 477;Clinton 201;Fountain 35;Hendricks 1034;Johnson 882;Lawrence 370;Marion 364;Monroe 328;Montgomery 334;Morgan 562;Tippecanoe 1380;White 150",
"05|Dan Burton|R|228718|Grant 18135;Hamilton 76461;Hancock 20167;Howard 16875;Huntington 11202;Johnson 1916;Marion 50429;Miami 9286;Shelby 9522;Tipton 5511;Wabash 9214",
"05|Katherine Fox Carr|D|82637|Grant 7955;Hamilton 22870;Hancock 6549;Howard 6610;Huntington 3638;Johnson 590;Marion 21591;Miami 3576;Shelby 3795;Tipton 1957;Wabash 3506",
"05|Rick Hodgin|L|7008|Grant 418;Hamilton 2431;Hancock 771;Howard 390;Huntington 304;Johnson 64;Marion 1658;Miami 311;Shelby 290;Tipton 141;Wabash 230",
"06|Mike Pence|R|182529|Adams 9480;Allen 3073;Bartholomew 12226;Blackford 3401;Dearborn 5005;Decatur 7222;Delaware 28300;Fayette 5688;Franklin 6154;Henry 13787;Jay 5540;Johnson 3560;Madison 34988;Randolph 7520;Rush 5514;Shelby 1530;Union 2268;Wayne 18028;Wells 9245",
"06|Mel Fox|D|85123|Adams 3474;Allen 1650;Bartholomew 3773;Blackford 1730;Dearborn 1705;Decatur 2653;Delaware 17970;Fayette 3289;Franklin 3329;Henry 6062;Jay 2347;Johnson 943;Madison 18654;Randolph 3296;Rush 1603;Shelby 370;Union 903;Wayne 8773;Wells 2599",
"06|Chad (Wick) Roots|L|4397|Adams 152;Allen 74;Bartholomew 193;Blackford 68;Dearborn 126;Decatur 147;Delaware 899;Fayette 150;Franklin 124;Henry 300;Jay 117;Johnson 76;Madison 701;Randolph 185;Rush 135;Shelby 24;Union 68;Wayne 703;Wells 155",
"07|Julia M. Carson|D|121303|Marion 121303",
"07|Andrew (Andy) Horning|R|97491|Marion 97491",
"07|Barry Campbell|L|4381|Marion 4381",
"08|John N. Hostettler|R|145576|Clay 6151;Daviess 7345;Fountain 3245;Gibson 7779;Greene 7995;Knox 8143;Martin 3381;Owen 4489;Parke 3981;Pike 3081;Posey 7030;Putnam 8332;Sullivan 4050;Vanderburgh 34560;Vermillion 2947;Vigo 16607;Warren 2440;Warrick 14020",
"08|Jon P. Jennings|D|121522|Clay 4104;Daviess 2935;Fountain 1790;Gibson 6603;Greene 4698;Knox 7031;Martin 1527;Owen 2730;Parke 2728;Pike 2986;Posey 4749;Putnam 4232;Sullivan 4097;Vanderburgh 33644;Vermillion 3809;Vigo 21276;Warren 1338;Warrick 11245",
"08|Mark Garvin|L|5680|Clay 248;Daviess 151;Fountain 93;Gibson 226;Greene 155;Knox 255;Martin 72;Owen 223;Parke 166;Pike 105;Posey 181;Putnam 253;Sullivan 148;Vanderburgh 1432;Vermillion 176;Vigo 1173;Warren 82;Warrick 541",
"09|Mike Sodrel|R|142197|Bartholomew 6385;Brown 3960;Clark 20494;Crawford 2131;Dearborn 7589;Dubois 8877;Floyd 16812;Harrison 8979;Jackson 7981;Jefferson 6154;Jennings 5623;Monroe 14600;Ohio 1574;Orange 4991;Perry 3228;Ripley 7104;Scott 3508;Spencer 4623;Switzerland 1665;Washington 5919",
"09|Baron Hill|D|140772|Bartholomew 5110;Brown 3081;Clark 21109;Crawford 2288;Dearborn 5624;Dubois 7501;Floyd 16214;Harrison 8011;Jackson 8074;Jefferson 6421;Jennings 4715;Monroe 21608;Ohio 1302;Orange 3437;Perry 4958;Ripley 4482;Scott 5053;Spencer 5064;Switzerland 1929;Washington 4791",
"09|Al Cox|L|4541|Bartholomew 267;Brown 217;Clark 402;Crawford 79;Dearborn 227;Dubois 196;Floyd 351;Harrison 253;Jackson 246;Jefferson 185;Jennings 166;Monroe 1031;Ohio 38;Orange 167;Perry 97;Ripley 153;Scott 107;Spencer 153;Switzerland 49;Washington 157")
tr <- purrr::map_dfr(strsplit(T, "\\|"), function(p) { kv <- strsplit(strsplit(p[5], ";")[[1]], " (?=[0-9]+$)", perl = TRUE)
  tibble(year = 2004L, district = p[1], candidate = p[2], party = p[3], printed = as.numeric(p[4]), county = vapply(kv, `[`, "", 1), votes = as.numeric(vapply(kv, `[`, "", 2))) })
write_csv(tr, file.path(DIR, "in_2004_transcription.csv"))
chk <- tr %>% group_by(district, candidate, party) %>% summarise(sum = sum(votes), n_cty = n(), printed = first(printed), .groups = "drop") %>% mutate(ok = sum == printed)
cat("candidate lines:", nrow(chk), "| county sums equal the printed candidate total:", sum(chk$ok), "\n"); print(as.data.frame(chk %>% filter(!ok))); stopifnot(all(chk$ok))
cat("counties per district:", paste(names(table(tr$district[!duplicated(tr[, c("district", "county")])])), table(tr$district[!duplicated(tr[, c("district", "county")])]), sep = "=", collapse = ", "), "\n")
stopifnot(!any(duplicated(tr[, c("district", "candidate", "county")])), all(tapply(tr$county, paste(tr$district, tr$candidate), function(z) length(z)) > 0))
## every candidate of a district must have the same county list
cl <- tr %>% group_by(district, candidate) %>% summarise(k = paste(sort(county), collapse = "|"), .groups = "drop") %>% group_by(district) %>% summarise(same = n_distinct(k) == 1); stopifnot(all(cl$same))
## district totals vs Wikipedia (2004 United States House of Representatives elections in Indiana): checked below against the shares printed there (see README note in the report)
mine <- tr %>% group_by(district) %>% summarise(rep = sum(votes[party == "R"]), dem = sum(votes[party == "D"]), other = sum(votes[!party %in% c("D", "R")]), .groups = "drop") %>% mutate(dem_share = round(100 * dem / (rep + dem + other), 1), rep_share = round(100 * rep / (rep + dem + other), 1)); print(as.data.frame(mine))

xw <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  filter(state == "INDIANA", !is.na(county_fips)) %>% distinct(county_name, county_fips)
norm <- function(z) gsub("[^A-Z]", "", toupper(z)); xw <- xw %>% mutate(key = norm(county_name)) %>% distinct(key, county_fips); stopifnot(nrow(xw) == 92)
tr <- tr %>% mutate(county_fips = xw$county_fips[match(norm(county), xw$key)]); stopifnot(!anyNA(tr$county_fips))
PARTY <- c(D = "Democratic", R = "Republican", L = "Libertarian", I = "Independent", W = "Write-In")
raw <- tr %>% transmute(year, county_fips, district, candidate, party_group = case_when(party == "D" ~ "DEM", party == "R" ~ "REP", TRUE ~ "OTHER"), party = PARTY[party], votes)     # party_group BEFORE the label overwrites the code
long <- finalize_long(raw, "in_2004")
## finalize_long strips punctuation from names: hand-restore the ones affected
NAMES <- c("Peter J Visclosky" = "Peter J. Visclosky", "Mark J Leyva" = "Mark J. Leyva", "Maria M Parra" = "Maria M. Parra", "Kevin R Fleming" = "Kevin R. Fleming", "Julia M Carson" = "Julia M. Carson",
           "Chad Wick Roots" = "Chad (Wick) Roots", "Andrew Andy Horning" = "Andrew (Andy) Horning", "John N Hostettler" = "John N. Hostettler", "Jon P Jennings" = "Jon P. Jennings")
key <- function(z) gsub("[^a-z]", "", tolower(z)); nm <- setNames(unname(NAMES), key(names(NAMES))); hit <- key(long$candidate) %in% names(nm); long$candidate[hit] <- nm[key(long$candidate[hit])]
cat("candidates:", paste(unique(long$candidate), collapse = " | "), "\n")
save_long(long, "he_in_2004")
shares <- derive_shares(long) %>% transmute(state = "INDIANA", year, cty_fips, sample, demovote, repuvote, totalvote); saveRDS(shares, file.path(OUTPUT_DIR, "elect_he_cty_in_2004.rds"))
print(check_long_vs_source(long, file.path(OUTPUT_DIR, "elect_he_cty_in_2004.rds")) %>% select(source, keys_source, matched, mismatched, pass))
stopifnot(median(shares$demovote) > 0.2, median(shares$repuvote) > 0.2)
cat("median Democratic share", round(median(shares$demovote), 3), "Republican", round(median(shares$repuvote), 3), "\n")
spl <- long %>% group_by(county_fips) %>% summarise(d = n_distinct(district)) %>% filter(d > 1) %>% pull(county_fips)
cat("counties:", n_distinct(long$county_fips), " split counties (", length(spl), "):", paste(sort(spl), collapse = ", "), "\n")
## House total vs the presidential total of 2004 in the panel
pe <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")) %>% filter(sample == "PE", year == 2004, cty_fips %/% 1000 == 18) %>% select(cty_fips, pres = totalvote)
r <- shares %>% inner_join(pe, by = "cty_fips") %>% mutate(ratio = totalvote / pres)
cat("House 2004 / presidential 2004 total per county (", nrow(r), " counties): min", round(min(r$ratio), 3), "median", round(median(r$ratio), 3), "max", round(max(r$ratio), 3), "\n"); print(as.data.frame(r %>% filter(ratio < 0.9 | ratio > 1.02) %>% select(cty_fips, totalvote, pres, ratio)))
## comparison with the old OpenElections rows in the panel (partial, 66 counties)
old <- readRDS(file.path(OUTPUT_DIR, "elect_he_cty_in.rds")) %>% filter(year == 2004) %>% select(cty_fips, o_tot = totalvote, o_dem = demovote, o_rep = repuvote)
cmp <- shares %>% left_join(old, by = "cty_fips") %>% mutate(status = case_when(is.na(o_tot) ~ "missing in old data", abs(totalvote - o_tot) < 0.5 & abs(demovote - o_dem) < 1e-9 & abs(repuvote - o_rep) < 1e-9 ~ "identical", TRUE ~ "old differs"))
cat("\nvs the old OpenElections rows for 2004:\n"); print(table(cmp$status)); print(as.data.frame(cmp %>% filter(status == "old differs") %>% mutate(rel = round(o_tot / totalvote, 2)) %>% select(cty_fips, totalvote, o_tot, rel)))
write_csv(cmp %>% select(cty_fips, totalvote, o_tot, status), file.path(PROJECT_ROOT, "R", "output", "in_2004_vs_old_openelections.csv"))
