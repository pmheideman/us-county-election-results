## Wisconsin U.S. House, 1998 and 2004 general elections, hand-transcribed from Wisconsin Blue Book scanned page images
## (R/data/county_house_files/wisconsin/1998/ and .../2004/, user-supplied). Closes 2 of Wisconsin's House gap years
## (1990/1992/1994/1996 done separately; 2000/2002/2006-2010 already covered via OpenElections in 01ba/02m; 2016+ via MEDSL).
##
## Source: "VOTE FOR MEMBERS OF THE ... CONGRESS / By District" (1998, Blue Book 1999-2000, pp.862-864) and "DISTRICT VOTE FOR
## MEMBERS OF THE ... U.S. CONGRESS" (2004, Blue Book 2005-2006, pp.906-908) -- one block per congressional district, county rows
## (many counties split across districts as "County (part)"; a county's full total is the sum of its "(part)" rows across every
## district touching it), a printed TOTAL row per district (2004 also prints a "Percent of Total Vote" row as an extra check).
## Every candidate column in every district was hand-verified against its own printed TOTAL before being accepted (see the
## transcribed CSVs) -- both years tie exactly, no residual.
##
## Wisconsin had 9 congressional districts in 1998, reduced to 8 for 2004 (post-2000 redistricting).
##
## Real nuance, 1998 District 2 (Tammy Baldwin's first election, an open seat): three of the four candidates on this table --
## Marc Gumz (Rep.), Josephine W. Musser (Rep.), John Stumpf (Tax.) -- carry a dagger footnote on p.864 ("Write-in candidates"),
## i.e. the state's own Republican nominee had a ballot problem that year and Republicans ran write-in campaigns instead. Kept as
## ordinary candidate rows with their PRINTED party label (ordinary DEM/REP/OTHER classification) -- a printed party affiliation
## is still a real party affiliation even for a write-in candidate, same treatment as every other state in this project.
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)

DIR <- file.path(PROJECT_ROOT, "R", "data", "county_house_files", "wisconsin", "transcribed")
dir.create(DIR, showWarnings = FALSE, recursive = TRUE)

xw <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  filter(state == "WISCONSIN") %>% distinct(county_fips, county_name) %>% mutate(county_name = toupper(trimws(county_name)))
stopifnot(nrow(xw) == 72)
fips_of_county <- function(z) xw$county_fips[match(toupper(trimws(z)), xw$county_name)]

to_party_group <- function(x) { x <- toupper(trimws(x)); case_when(startsWith(x, "DEM") ~ "DEM", startsWith(x, "REP") ~ "REP", TRUE ~ "OTHER") }

## ---- 1998 (Blue Book 1999-2000, pp. 862-864) -------------------------------------------------------------------------------
## One data row per (district, county, candidate); row5() recycles a single candidate/party across a district's county vector.
row5 <- function(district, county, candidate, party, votes) tibble(district = district, county = county, candidate = candidate, party = party, votes = votes)

d1998 <- bind_rows(
  # District 1 -- p.862
  row5(1, c("Green","Jefferson","Kenosha","Racine","Rock","Walworth","Waukesha"), "Lydia Carol Spottswood", "Dem.", c(1598,338,19400,26638,23291,8694,1205)),
  row5(1, c("Green","Jefferson","Kenosha","Racine","Rock","Walworth","Waukesha"), "Paul Ryan", "Rep.", c(2420,526,26003,34081,25309,17279,2857)),
  # District 2 -- p.862 (Gumz/Musser/Stumpf are write-ins, footnoted on p.864; kept as ordinary candidates)
  row5(2, c("Columbia","Dane","Dodge","Green","Iowa","Jefferson","Lafayette","Richland","Sauk"), "Tammy Baldwin", "Dem.", c(6846,91991,1045,2592,2992,304,1899,1962,6746)),
  row5(2, c("Columbia","Dane","Dodge","Green","Iowa","Jefferson","Lafayette","Richland","Sauk"), "Marc Gumz", "Rep.", c(44,10,0,0,0,0,0,3,50)),
  row5(2, c("Columbia","Dane","Dodge","Green","Iowa","Jefferson","Lafayette","Richland","Sauk"), "Josephine W. Musser", "Rep.", c(9693,67739,1947,4017,3928,343,3079,3132,9650)),
  row5(2, c("Columbia","Dane","Dodge","Green","Iowa","Jefferson","Lafayette","Richland","Sauk"), "John Stumpf", "Tax.", c(57,22,1,2,0,0,0,5,16)),
  # District 3 -- p.862
  row5(3, c("Barron","Buffalo","Chippewa","Clark","Crawford","Dunn","Eau Claire","Grant","Jackson","La Crosse","Monroe","Pepin","Pierce","Polk","St. Croix","Trempealeau","Vernon"),
       "Ron Kind", "Dem.", c(8525,2971,136,4334,4531,7173,21786,6852,4911,29696,2954,1539,6322,3334,10072,6755,6365)),
  row5(3, c("Barron","Buffalo","Chippewa","Clark","Crawford","Dunn","Eau Claire","Grant","Jackson","La Crosse","Monroe","Pepin","Pierce","Polk","St. Croix","Trempealeau","Vernon"),
       "Troy A. Brechler", "Rep.", c(3899,1145,68,1653,1318,3169,7461,5446,1281,7200,1042,508,3541,2382,6869,1588,2431)),
  # District 4 -- p.863
  row5(4, c("Milwaukee","Waukesha"), "Jerry Kleczka", "Dem.", c(83361,22480)),
  row5(4, c("Milwaukee","Waukesha"), "Tom Reynolds", "Rep.", c(49073,27593)),
  # District 5 -- p.863
  row5(5, "Milwaukee", "Tom Barrett", "Dem.", 121129),
  row5(5, "Milwaukee", "Jack Melvin", "Rep.", 33506),
  # District 6 -- p.863
  row5(6, c("Adams","Brown","Calumet","Fond du Lac","Green Lake","Juneau","Manitowoc","Marquette","Monroe","Outagamie","Sheboygan","Waupaca","Waushara","Winnebago"),
       "Thomas E. Petri", "Rep.", c(4279,507,9565,23295,4512,4567,18323,3036,5053,5471,8065,12249,6000,39222)),
  row5(6, c("Adams","Brown","Calumet","Fond du Lac","Green Lake","Juneau","Manitowoc","Marquette","Monroe","Outagamie","Sheboygan","Waupaca","Waushara","Winnebago"),
       "Timothy J. Farness", "Tax.", c(277,48,717,1997,171,202,857,113,231,636,646,539,236,4597)),
  # District 7 -- p.863
  row5(7, c("Ashland","Bayfield","Burnett","Chippewa","Clark","Douglas","Eau Claire","Iron","Lincoln","Marathon","Oneida","Polk","Portage","Price","Rusk","Sawyer","Taylor","Washburn","Wood"),
       "David R. Obey", "Dem.", c(3617,4083,3515,10356,2627,8832,195,1603,5367,21750,2906,4024,13509,3633,3602,2787,4078,3107,16022)),
  row5(7, c("Ashland","Bayfield","Burnett","Chippewa","Clark","Douglas","Eau Claire","Iron","Lincoln","Marathon","Oneida","Polk","Portage","Price","Rusk","Sawyer","Taylor","Washburn","Wood"),
       "Scott West", "Rep.", c(1621,1955,2426,6501,1810,3974,110,903,3643,18042,1424,2575,7918,2141,1921,2588,2589,2381,10527)),
  # District 8 -- p.864
  row5(8, c("Brown","Calumet","Door","Florence","Forest","Kewaunee","Langlade","Manitowoc","Marinette","Menominee","Oconto","Oneida","Outagamie","Shawano","Vilas"),
       "Jay Johnson", "Dem.", c(34053,482,4811,701,1716,3401,2996,118,7199,812,5504,3530,19067,5827,3224)),
  row5(8, c("Brown","Calumet","Door","Florence","Forest","Kewaunee","Langlade","Manitowoc","Marinette","Menominee","Oconto","Oneida","Outagamie","Shawano","Vilas"),
       "Mark Green", "Rep.", c(41328,557,5419,903,1631,3890,3371,172,7040,244,6418,4868,23987,7580,5010)),
  # District 9 -- p.864
  row5(9, c("Dodge","Fond du Lac","Jefferson","Ozaukee","Sheboygan","Washington","Waukesha"), "F. James Sensenbrenner, Jr.", "Rep.", c(15670,176,16099,25742,22651,31276,63919)),
  row5(9, c("Dodge","Fond du Lac","Jefferson","Ozaukee","Sheboygan","Washington","Waukesha"), "Anthony E. Deiss", "Tax.", c(2,0,3,0,3,10,6)),
  row5(9, c("Dodge","Fond du Lac","Jefferson","Ozaukee","Sheboygan","Washington","Waukesha"), "Jeffrey M. Gonyo", "Ind.", c(1721,20,1943,2253,2352,3184,4946)),
) %>% mutate(year = 1998L)

## District printed TOTAL rows (p.862-864) -- the tie-check target.
tot1998 <- tribble(
  ~district, ~candidate, ~total,
  1, "Lydia Carol Spottswood", 81164, 1, "Paul Ryan", 108475,
  2, "Tammy Baldwin", 116377, 2, "Marc Gumz", 107, 2, "Josephine W. Musser", 103528, 2, "John Stumpf", 103,
  3, "Ron Kind", 128256, 3, "Troy A. Brechler", 51001,
  4, "Jerry Kleczka", 105841, 4, "Tom Reynolds", 76666,
  5, "Tom Barrett", 121129, 5, "Jack Melvin", 33506,
  6, "Thomas E. Petri", 144144, 6, "Timothy J. Farness", 11267,
  7, "David R. Obey", 115613, 7, "Scott West", 75049,
  8, "Jay Johnson", 93441, 8, "Mark Green", 112418,
  9, "F. James Sensenbrenner, Jr.", 175533, 9, "Anthony E. Deiss", 24, 9, "Jeffrey M. Gonyo", 16419,
)

## ---- 2004 (Blue Book 2005-2006, pp. 906-908) -------------------------------------------------------------------------------
d2004 <- bind_rows(
  # District 1 -- p.906
  row5(1, c("Kenosha","Milwaukee","Racine","Rock","Walworth","Waukesha"), "Jeffrey Chapman Thomas", "Dem.", c(27000,22507,34993,12833,10889,8028)),
  row5(1, c("Kenosha","Milwaukee","Racine","Rock","Walworth","Waukesha"), "Don Bernau", "Lib.", c(647,593,696,324,448,228)),
  row5(1, c("Kenosha","Milwaukee","Racine","Rock","Walworth","Waukesha"), "Paul Ryan", "Rep.", c(43002,44208,62413,26965,29988,26796)),
  row5(1, c("Kenosha","Milwaukee","Racine","Rock","Walworth","Waukesha"), "Norman Aulabaugh", "Ind.", c(622,338,542,2040,527,183)),
  # District 2 -- p.906
  row5(2, c("Columbia","Dane","Green","Jefferson","Rock","Sauk","Walworth"), "Tammy Baldwin", "Dem.", c(14494,181033,9849,12426,22204,8525,3106)),
  row5(2, c("Columbia","Dane","Green","Jefferson","Rock","Sauk","Walworth"), "Dave Magnum", "Rep.", c(14621,88003,8111,11882,14248,6839,2106)),
  # District 3 -- p.906
  row5(3, c("Buffalo","Clark","Crawford","Dunn","Eau Claire","Grant","Iowa","Jackson","Juneau","La Crosse","Lafayette","Monroe","Pepin","Pierce","Richland","St. Croix","Sauk","Trempealeau","Vernon"),
       "Ron Kind", "Dem.", c(4450,5363,5037,12853,34264,11083,5884,6168,4831,38949,3443,10395,2355,11865,3674,20821,5510,9207,8704)),
  row5(3, c("Buffalo","Clark","Crawford","Dunn","Eau Claire","Grant","Iowa","Jackson","Juneau","La Crosse","Lafayette","Monroe","Pepin","Pierce","Richland","St. Croix","Sauk","Trempealeau","Vernon"),
       "Dale W. Schultz", "Rep.", c(2900,3457,3210,9480,19714,13073,6063,3217,6777,21906,4581,8711,1534,9089,5476,19496,8983,4502,5697)),
  # District 4 -- p.907
  row5(4, "Milwaukee", "Colin Hudson", "Con.", 897),
  row5(4, "Milwaukee", "Gwen Moore", "Dem.", 212382),
  row5(4, "Milwaukee", "Gerald H. Boyle", "Rep.", 85928),
  row5(4, "Milwaukee", "Tim Johnson", "Ind.", 3733),
  row5(4, "Milwaukee", "Robert R. Raymond", "Ind.", 1861),
  # District 5 -- p.907
  row5(5, c("Jefferson","Milwaukee","Ozaukee","Washington","Waukesha"), "Bryan Kennedy", "Dem.", c(2127,42780,14706,17705,52066)),
  row5(5, c("Jefferson","Milwaukee","Ozaukee","Washington","Waukesha"), "Tim Peterson", "Lib.", c(139,1521,788,1231,2870)),
  row5(5, c("Jefferson","Milwaukee","Ozaukee","Washington","Waukesha"), "F. James Sensenbrenner, Jr.", "Rep.", c(4376,47296,36157,51108,132216)),
  # District 6 -- p.907
  row5(6, c("Adams","Calumet","Dodge","Fond du Lac","Green Lake","Jefferson","Manitowoc","Marquette","Outagamie","Sheboygan","Waushara","Winnebago"),
       "Jef Hall", "Dem.", c(3999,4721,12711,12337,2423,2774,14100,2571,1886,19012,3271,27404)),
  row5(6, c("Adams","Calumet","Dodge","Fond du Lac","Green Lake","Jefferson","Manitowoc","Marquette","Outagamie","Sheboygan","Waushara","Winnebago"),
       "Tom Petri", "Rep.", c(5874,13774,28072,37950,7169,6550,27166,5031,3735,40099,7988,55212)),
  row5(6, c("Adams","Calumet","Dodge","Fond du Lac","Green Lake","Jefferson","Manitowoc","Marquette","Outagamie","Sheboygan","Waushara","Winnebago"),
       "Carol Ann Rittenhouse", "WG", c(302,388,1541,1176,211,325,856,73,158,1624,345,3019)),
  # District 7 -- p.908
  row5(7, c("Ashland","Barron","Bayfield","Burnett","Chippewa","Clark","Douglas","Iron","Langlade","Lincoln","Marathon","Oneida","Polk","Portage","Price","Rusk","Sawyer","Taylor","Washburn","Wood"),
       "Larry Oftedahl", "Con.", c(59,1252,79,177,1413,265,828,27,153,682,3345,269,461,1313,139,231,90,356,211,1491)),
  row5(7, c("Ashland","Barron","Bayfield","Burnett","Chippewa","Clark","Douglas","Iron","Langlade","Lincoln","Marathon","Oneida","Polk","Portage","Price","Rusk","Sawyer","Taylor","Washburn","Wood"),
       "David R. Obey", "Dem.", c(6008,13608,6370,5514,20868,4057,19063,2459,5482,10859,44401,8091,12868,26875,5626,5200,4829,6417,5555,27156)),
  row5(7, c("Ashland","Barron","Bayfield","Burnett","Chippewa","Clark","Douglas","Iron","Langlade","Lincoln","Marathon","Oneida","Polk","Portage","Price","Rusk","Sawyer","Taylor","Washburn","Wood"),
       "Mike Miles", "WG", c(224,571,236,284,1971,473,1507,62,428,1562,8298,587,893,3764,163,284,192,888,257,3874)),
  # District 8 -- p.908
  row5(8, c("Brown","Calumet","Door","Florence","Forest","Kewaunee","Langlade","Marinette","Menominee","Oconto","Oneida","Outagamie","Shawano","Vilas","Waupaca"),
       "Dottie Le Clair", "Dem.", c(35029,1678,5175,743,1510,2910,757,6596,893,5369,3084,25580,5019,3941,7229)),
  row5(8, c("Brown","Calumet","Door","Florence","Forest","Kewaunee","Langlade","Marinette","Menominee","Oconto","Oneida","Outagamie","Shawano","Vilas","Waupaca"),
       "Mark Green", "Rep.", c(84641,3754,11860,1825,2995,7991,1905,14869,634,13808,6856,54775,14112,9597,18448)),
) %>% mutate(year = 2004L)

tot2004 <- tribble(
  ~district, ~candidate, ~total,
  1, "Jeffrey Chapman Thomas", 116250, 1, "Don Bernau", 2936, 1, "Paul Ryan", 233372, 1, "Norman Aulabaugh", 4252,
  2, "Tammy Baldwin", 251637, 2, "Dave Magnum", 145810,
  3, "Ron Kind", 204856, 3, "Dale W. Schultz", 157866,
  4, "Colin Hudson", 897, 4, "Gwen Moore", 212382, 4, "Gerald H. Boyle", 85928, 4, "Tim Johnson", 3733, 4, "Robert R. Raymond", 1861,
  5, "Bryan Kennedy", 129384, 5, "Tim Peterson", 6549, 5, "F. James Sensenbrenner, Jr.", 271153,
  6, "Jef Hall", 107209, 6, "Tom Petri", 238620, 6, "Carol Ann Rittenhouse", 10018,
  7, "Larry Oftedahl", 12841, 7, "David R. Obey", 241306, 7, "Mike Miles", 26518,
  8, "Dottie Le Clair", 105513, 8, "Mark Green", 248070,
)

## ---- tie-check: every district's county rows must sum EXACTLY to that district's own printed TOTAL --------------------------
check_ties <- function(d, tot, year) {
  chk <- d %>% group_by(district, candidate) %>% summarise(sum_votes = sum(votes), .groups = "drop") %>%
    inner_join(tot, by = c("district", "candidate"))
  stopifnot(nrow(chk) == nrow(tot))                                            # every printed total was matched to a transcribed candidate
  bad <- chk %>% filter(sum_votes != total)
  if (nrow(bad) > 0) { print(as.data.frame(bad)); stop(year, ": ", nrow(bad), " candidate column(s) do not tie to their printed TOTAL") }
  message(year, ": all ", nrow(chk), " candidate columns tie exactly to their printed district TOTAL")
}
check_ties(d1998, tot1998, 1998)
check_ties(d2004, tot2004, 2004)

## ---- build the long table, save the transcribed CSVs for the record ----------------------------------------------------------
write_csv(d1998 %>% select(year, district, county, candidate, party, votes), file.path(DIR, "1998.csv"))
write_csv(d2004 %>% select(year, district, county, candidate, party, votes), file.path(DIR, "2004.csv"))

raw <- bind_rows(d1998, d2004) %>%
  mutate(county_fips = fips_of_county(county), party_group = to_party_group(party), district = norm_district(district)) %>%
  select(year, county_fips, district, candidate, party, party_group, votes)
stopifnot(!anyNA(raw$county_fips))
stopifnot(!anyDuplicated(raw[, c("year", "county_fips", "district", "candidate", "party")]))

for (y in c(1998, 2004)) {
  long <- finalize_long(raw %>% filter(year == y), paste0("wi_", y))
  save_long(long, paste0("he_wi_", y))
  shares <- derive_shares(long) %>% transmute(state = "WISCONSIN", year, cty_fips, sample, demovote, repuvote, totalvote)
  saveRDS(shares, file.path(OUTPUT_DIR, sprintf("elect_he_cty_wi_%d.rds", y)))
  r <- check_long_vs_source(long, file.path(OUTPUT_DIR, sprintf("elect_he_cty_wi_%d.rds", y))); stopifnot(all(r$pass))
  message(y, ": ", nrow(shares), " of 72 WI counties")
}

sanity <- bind_rows(lapply(c(1998, 2004), function(y) readRDS(file.path(OUTPUT_DIR, sprintf("elect_he_cty_wi_%d.rds", y)))))
share <- sanity$repuvote + sanity$demovote
message("Sanity: repuvote+demovote range [", round(min(share), 3), ", ", round(max(share), 3), "]")

## ---- independent check: district winners should match known Wisconsin political history --------------------------------------
for (y in c(1998, 2004)) {
  l <- readRDS(file.path(LONG_DIR, sprintf("he_wi_%d.rds", y)))
  win <- l %>% group_by(district, candidate) %>% summarise(votes = sum(votes), .groups = "drop") %>% group_by(district) %>% slice_max(votes, n = 1) %>% ungroup()
  message(y, " district winners: ", paste(sprintf("D%s=%s", win$district, win$candidate), collapse = "; "))
}
