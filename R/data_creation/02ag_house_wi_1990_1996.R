## Wisconsin U.S. House, 1990/1992/1994/1996 general elections, hand-transcribed from Wisconsin Blue Book scanned page images
## (user-supplied, R/data/county_house_files/wisconsin/<year>/): "VOTE FOR MEMBERS OF THE ___ CONGRESS / By District, November _, ____",
## one table per congressional district (Wisconsin had 9 districts this whole period), county rows (many counties split across districts and
## printed as "County (part)" -- summed together with any other district's row for the same county to get the true county total), then a
## printed TOTAL row. Every district's county rows tie EXACTLY to its own printed TOTAL for every candidate column (verified by hand against
## the source images before this script was written; the tie-check below is a permanent regression guard, not just a one-time check).
##
## 1992 also has a "Special Election, May 4, 1993" table (First District recount, following Les Aspin's resignation to become Secretary of
## Defense) on the same Blue Book page as the 9th district's regular results -- EXCLUDED, not the November general election.
## Party from the printed abbreviation: Dem.->DEM, Rep.->REP, everything else (Ind./Lib./Tax.)->OTHER.
##
## 2000/2002/2006-2010 already covered via OpenElections (01ba/02m); 2016+ via MEDSL. 1998 and 2004 done separately (02ah).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))

xw <- readr::read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  filter(state == "WISCONSIN") %>% distinct(county_fips, county_name) %>% mutate(county_name = toupper(trimws(county_name)))
stopifnot(nrow(xw) == 72)
fips_of_county <- function(z) xw$county_fips[match(toupper(trimws(sub("\\s*\\(PART\\)\\s*$", "", toupper(z)))), xw$county_name)]

party_group_of <- function(p) case_when(p == "Dem." ~ "DEM", p == "Rep." ~ "REP", TRUE ~ "OTHER")

## One row per district block: county column + one column per candidate (named c1, c2, ...); `cands`/`parties` give the real name/party per
## column in order. Returns a long data frame: year, district, county, candidate, party, votes.
block <- function(year, district, cands, parties, tbl) {
  stopifnot(length(cands) == length(parties), ncol(tbl) - 1 == length(cands))
  names(tbl) <- c("county", paste0("c", seq_along(cands)))
  long <- tbl %>% tidyr::pivot_longer(-county, names_to = "cid", values_to = "votes") %>%
    mutate(cid = as.integer(sub("c", "", cid)), candidate = cands[cid], party = parties[cid]) %>% select(-cid)
  total <- long %>% group_by(candidate) %>% summarise(v = sum(votes), .groups = "drop")
  cat(year, "D", district, ": ", paste(sprintf("%s=%d", total$candidate, total$v), collapse = ", "), "\n")
  long %>% mutate(year = year, district = district)
}

raw <- bind_rows(

## ---------------------------------------------------------------- 1990 (p.909-911) ----------------------------------------------------------
block(1990, 1, "Les Aspin", "Dem.", tribble(
  ~county, ~c1,
  "Green (part)", 2233, "Jefferson (part)", 205, "Kenosha", 19996, "Racine", 34596, "Rock", 24401, "Walworth", 12530)),
block(1990, 2, c("Robert W. Kastenmeier", "Scott L. Klug"), c("Dem.", "Rep."), tribble(
  ~county, ~c1, ~c2,
  "Adams (part)", 230, 428, "Columbia", 5281, 8993, "Dane", 63709, 59735, "Dodge (part)", 3914, 6345, "Grant (part)", 754, 812,
  "Green (part)", 1571, 3075, "Iowa", 2271, 3621, "Juneau (part)", 538, 794, "Lafayette", 1850, 3329, "Richland (part)", 443, 703,
  "Sauk", 4595, 9103)),
block(1990, 3, c("James L Ziegeweid", "Steven C. Gunderson"), c("Dem.", "Rep."), tribble(
  ~county, ~c1, ~c2,
  "Barron", 4055, 6202, "Buffalo", 2021, 2256, "Clark (part)", 1851, 3801, "Crawford", 1709, 2720, "Dunn", 4082, 5205,
  "Eau Claire", 11207, 16362, "Grant (part)", 2650, 6080, "Jackson", 2580, 3438, "La Crosse", 10912, 19305, "Pepin", 915, 1092,
  "Pierce", 3371, 5082, "Polk (part)", 2543, 4591, "Richland (part)", 837, 2271, "St. Croix", 5174, 7990, "Trempealeau", 3545, 3766,
  "Vernon", 2957, 4348)),
block(1990, 4, c("Gerald D. Kleczka", "Joseph L. Cook"), c("Dem.", "Rep."), tribble(
  ~county, ~c1, ~c2,
  "Milwaukee (part)", 79737, 29269, "Waukesha (part)", 17244, 13732)),
block(1990, 5, c("Jim Moody", "Donalda Arnell Hammersmith", "Nathaniel J. Stampley"), c("Dem.", "Rep.", "Ind."), tribble(
  ~county, ~c1, ~c2, ~c3,
  "Milwaukee (part)", 77557, 31255, 4968, "Washington (part)", 0, 0, 0)),
block(1990, 6, "Thomas E. Petri", "Rep.", tribble(
  ~county, ~c1,
  "Adams", 2517, "Calumet", 7011, "Fond du Lac (part)", 17727, "Green Lake", 3950, "Juneau", 4800, "Manitowoc", 15201,
  "Marquette", 2677, "Monroe", 6314, "Sheboygan (part)", 7384, "Waupaca", 8340, "Waushara", 3967, "Winnebago", 30102,
  "Wood (part)", 1046)),
block(1990, 7, c("David R. Obey", "John L. McEwen"), c("Dem.", "Rep."), tribble(
  ~county, ~c1, ~c2,
  "Ashland", 3494, 1480, "Bayfield", 3720, 1678, "Burnett", 3059, 1296, "Chippewa", 9858, 4499, "Clark (part)", 2479, 1489,
  "Douglas", 10330, 3612, "Iron", 1606, 760, "Lincoln", 4750, 3334, "Marathon", 18631, 18348, "Oneida (part)", 3228, 1519,
  "Polk (part)", 2110, 909, "Portage", 10111, 4758, "Price", 4000, 2362, "Rusk", 3647, 1487, "Sawyer", 2232, 1822,
  "Taylor", 3364, 2307, "Washburn", 3164, 1647, "Wood (part)", 10286, 7654)),
block(1990, 8, c("Jerome Van Sistine", "Toby Roth"), c("Dem.", "Rep."), tribble(
  ~county, ~c1, ~c2,
  "Brown", 35062, 27389, "Door", 3669, 4989, "Florence", 566, 817, "Forest", 1249, 1757, "Kewaunee", 3751, 3447,
  "Langlade", 2047, 3928, "Marinette", 5184, 7723, "Menominee", 550, 430, "Oconto", 4251, 4919, "Oneida (part)", 2605, 4539,
  "Outagamie", 18027, 24624, "Shawano", 3942, 6399, "Vilas", 2296, 4941)),
block(1990, 9, "F. James Sensenbrenner, Jr.", "Rep.", tribble(
  ~county, ~c1,
  "Dodge (part)", 7288, "Fond du Lac (part)", 1481, "Jefferson (part)", 13313, "Milwaukee (part)", 7938, "Ozaukee", 17429,
  "Sheboygan (part)", 11948, "Washington (part)", 18174, "Waukesha (part)", 40396)),

## ---------------------------------------------------------------- 1992 (p.916-918) ----------------------------------------------------------
block(1992, 1, c("Les Aspin", "Mark W. Neumann", "John Graf"), c("Dem.", "Rep.", "Ind."), tribble(
  ~county, ~c1, ~c2, ~c3,
  "Green (part)", 2878, 2281, 125, "Jefferson (part)", 825, 717, 34, "Kenosha", 35724, 21214, 748, "Racine", 48118, 35065, 545,
  "Rock", 41132, 22854, 2308, "Walworth", 16454, 19428, 582, "Waukesha (part)", 2364, 2793, 49)),
block(1992, 2, c("Ada E. Deer", "Scott Klug", "Joseph E. Schumacher"), c("Dem.", "Rep.", "Ind."), tribble(
  ~county, ~c1, ~c2, ~c3,
  "Columbia", 6954, 16522, 151, "Dane", 81689, 124275, 696, "Dodge (part)", 1408, 3053, 64, "Green (part)", 2510, 6267, 63,
  "Iowa", 3117, 6597, 19, "Jefferson (part)", 300, 564, 11, "Lafayette", 2237, 4847, 5, "Richland", 2890, 5087, 12,
  "Sauk", 7186, 16154, 119)),
block(1992, 3, c("Paul Sacia", "Steven C. Gunderson", "Jay B. Evenson"), c("Dem.", "Rep.", "Ind."), tribble(
  ~county, ~c1, ~c2, ~c3,
  "Barron", 8029, 10069, 112, "Buffalo", 3467, 3106, 94, "Chippewa (part)", 183, 152, 0, "Clark (part)", 2808, 4905, 73,
  "Crawford", 2651, 4712, 36, "Dunn", 7051, 8904, 227, "Eau Claire (part)", 18199, 24784, 1224, "Grant", 6360, 14139, 154,
  "Jackson", 4167, 3741, 106, "La Crosse", 20455, 28145, 914, "Monroe (part)", 2175, 3247, 82, "Pepin", 1578, 1613, 45,
  "Pierce", 6710, 8723, 303, "Polk (part)", 3409, 4633, 98, "St. Croix", 9245, 13874, 897, "Trempealeau", 7213, 5128, 226,
  "Vernon", 4964, 7028, 145)),
block(1992, 4, c("Gerald Kleczka", "John Washburn", "Joseph L. Cook", "Daniel Slak"), c("Dem.", "Lib.", "Rep.", "Ind."), tribble(
  ~county, ~c1, ~c2, ~c3, ~c4,
  "Milwaukee (part)", 137914, 1978, 54225, 2226, "Waukesha (part)", 35568, 510, 30647, 577)),
block(1992, 5, c("Thomas M. Barrett", "Donalda Ann Hammersmith"), c("Dem.", "Rep."), tribble(
  ~county, ~c1, ~c2,
  "Milwaukee (part)", 162344, 71085)),
block(1992, 6, c("Peggy A. Lautenschlager", "Thomas E. Petri"), c("Dem.", "Rep."), tribble(
  ~county, ~c1, ~c2,
  "Adams", 3489, 3789, "Brown (part)", 438, 388, "Calumet (part)", 7242, 8638, "Fond du Lac (part)", 21565, 23227,
  "Green Lake", 3913, 5164, "Juneau", 3410, 6427, "Manitowoc (part)", 21274, 17795, "Marquette", 2811, 3455,
  "Monroe (part)", 4345, 5726, "Outagamie (part)", 5357, 4480, "Sheboygan (part)", 4985, 7115, "Waupaca", 9932, 12191,
  "Waushara", 4165, 5723, "Winnebago", 35306, 39757)),
block(1992, 7, c("David R. Obey", "Dale R. Vannes"), c("Dem.", "Rep."), tribble(
  ~county, ~c1, ~c2,
  "Ashland", 5334, 1986, "Bayfield", 5097, 2218, "Burnett", 4442, 2345, "Chippewa (part)", 15936, 6547, "Clark (part)", 3931, 2199,
  "Douglas", 13660, 4475, "Eau Claire (part)", 297, 136, "Iron", 2485, 827, "Lincoln", 7690, 4929, "Marathon", 29945, 24899,
  "Oneida (part)", 4650, 1804, "Polk (part)", 5754, 3005, "Portage", 20579, 9915, "Price", 4900, 2580, "Rusk", 5146, 2232,
  "Sawyer", 3718, 3055, "Taylor", 5392, 3348, "Washburn", 4272, 2633, "Wood", 22972, 12639)),
block(1992, 8, c("Catherine L. Helms", "Toby Roth"), c("Dem.", "Rep."), tribble(
  ~county, ~c1, ~c2,
  "Brown (part)", 33968, 64268, "Calumet (part)", 485, 1197, "Door", 3736, 9655, "Florence", 703, 1792, "Forest", 1277, 2580,
  "Kewaunee", 3045, 7231, "Langlade", 2212, 7271, "Manitowoc (part)", 128, 220, "Marinette", 4938, 14973, "Menominee", 233, 480,
  "Oconto", 4502, 10199, "Oneida (part)", 3198, 7671, "Outagamie (part)", 16819, 43626, "Shawano", 3924, 12628, "Vilas", 2624, 7913)),
block(1992, 9, c("Ingrid K. Buxton", "Jeffrey Holt Millikin", "F. James Sensenbrenner, Jr.", "David E. Marlow"),
      c("Dem.", "Lib.", "Rep.", "Ind."), tribble(
  ~county, ~c1, ~c2, ~c3, ~c4,
  "Dodge (part)", 9111, 186, 19507, 793, "Fond du Lac (part)", 119, 2, 256, 6, "Jefferson (part)", 8996, 179, 18441, 699,
  "Ozaukee", 9969, 285, 30812, 720, "Sheboygan (part)", 14318, 226, 23877, 525, "Washington", 12625, 494, 32842, 712,
  "Waukesha (part)", 22224, 509, 67163, 1164)),

## ---------------------------------------------------------------- 1994 (p.916-918) ----------------------------------------------------------
block(1994, 1, c("Peter W. Barca", "Edward J. Kozak", "Mark W. Neumann"), c("Dem.", "Lib.", "Rep."), tribble(
  ~county, ~c1, ~c2, ~c3,
  "Green (part)", 1800, 106, 1794, "Jefferson (part)", 299, 28, 427, "Kenosha", 21828, 696, 17552, "Racine", 25865, 931, 29903,
  "Rock", 23985, 624, 18740, "Walworth", 7953, 645, 13278, "Waukesha (part)", 1087, 55, 2243)),
block(1994, 2, c("Thomas C. Hecht", "Scott L. Klug", "John J. Stumpf", "Joseph E. Schumacher"), c("Dem.", "Rep.", "Tax.", "Ind."), tribble(
  ~county, ~c1, ~c2, ~c3, ~c4,
  "Columbia", 3017, 11739, 620, 37, "Dane", 44856, 90303, 1209, 1036, "Dodge (part)", 455, 2038, 72, 31, "Green", 933, 5133, 200, 47,
  "Iowa", 1291, 4754, 42, 24, "Jefferson (part)", 118, 433, 12, 6, "Lafayette", 844, 3922, 31, 15, "Richland", 989, 4481, 53, 23,
  "Sauk", 2903, 10931, 437, 108)),
block(1994, 3, c("Harvey Stower", "Steve Gunderson", "Chuck Lee", "Mark Weinhold"), c("Dem.", "Rep.", "Tax.", "Ind."), tribble(
  ~county, ~c1, ~c2, ~c3, ~c4,
  "Barron", 5018, 5512, 73, 45, "Buffalo", 1775, 2004, 27, 25, "Chippewa (part)", 112, 107, 1, 3, "Clark (part)", 2010, 2921, 107, 44,
  "Crawford", 1947, 2198, 28, 29, "Dunn", 3938, 4840, 171, 213, "Eau Claire (part)", 10066, 15184, 979, 561, "Grant", 3965, 8623, 95, 97,
  "Jackson", 2505, 2963, 115, 42, "La Crosse", 11396, 18319, 491, 686, "Monroe (part)", 1408, 2077, 69, 37, "Pepin", 998, 1190, 50, 28,
  "Pierce", 4083, 5368, 96, 94, "Polk (part)", 3196, 1795, 43, 21, "St. Croix", 6332, 7734, 283, 241, "Trempealeau", 3460, 4065, 76, 53,
  "Vernon", 3549, 4438, 133, 60)),
block(1994, 4, c("Gerald Kleczka", "Tom Reynolds", "James Harold Hause"), c("Dem.", "Rep.", "Tax."), tribble(
  ~county, ~c1, ~c2, ~c3,
  "Milwaukee (part)", 75239, 53033, 1970, "Waukesha (part)", 18550, 25192, 641)),
block(1994, 5, c("Tom Barrett", "Stephen B. Hollingshead", "David J. Schall"), c("Dem.", "Rep.", "Ind."), tribble(
  ~county, ~c1, ~c2, ~c3,
  "Milwaukee (part)", 87806, 51145, 1576)),
block(1994, 6, "Thomas E. Petri", "Rep.", tribble(
  ~county, ~c1,
  "Adams", 3555, "Brown (part)", 385, "Calumet (part)", 7413, "Fond du Lac (part)", 14238, "Green Lake", 4295, "Juneau", 4887,
  "Manitowoc", 17845, "Marquette", 3038, "Monroe (part)", 5052, "Outagamie (part)", 4525, "Sheboygan (part)", 6914, "Waupaca", 9846,
  "Waushara", 4904, "Winnebago", 32487)),
block(1994, 7, c("David R. Obey", "Scott West"), c("Dem.", "Rep."), tribble(
  ~county, ~c1, ~c2,
  "Ashland", 3753, 1346, "Bayfield", 3554, 1644, "Burnett", 2908, 2350, "Chippewa (part)", 9807, 6097, "Clark (part)", 2204, 1923,
  "Douglas", 8866, 3846, "Eau Claire (part)", 148, 100, "Iron", 2141, 831, "Lincoln", 4701, 4678, "Marathon", 16822, 21863,
  "Oneida (part)", 2755, 1772, "Polk (part)", 3330, 2229, "Portage", 9794, 9650, "Price", 3802, 2460, "Rusk", 3346, 1869,
  "Sawyer", 2036, 2130, "Taylor", 2860, 2946, "Washburn", 2760, 2323, "Wood", 11597, 11649)),
block(1994, 8, c("Stan Gruszynski", "Toby Roth"), c("Dem.", "Rep."), tribble(
  ~county, ~c1, ~c2,
  "Brown (part)", 27375, 35151, "Calumet (part)", 377, 631, "Door", 3365, 5702, "Florence", 426, 1418, "Forest", 833, 2050,
  "Kewaunee", 2958, 4143, "Langlade", 1491, 4866, "Manitowoc (part)", 100, 138, "Marinette", 4583, 8992, "Menominee", 336, 448,
  "Oconto", 3377, 6076, "Oneida (part)", 1933, 6079, "Outagamie (part)", 12936, 24625, "Shawano", 3080, 7397, "Vilas", 1895, 6603)),
block(1994, 9, "F. James Sensenbrenner, Jr.", "Rep.", tribble(
  ~county, ~c1,
  "Dodge (part)", 14288, "Fond du Lac (part)", 98, "Jefferson (part)", 13459, "Ozaukee", 15197, "Sheboygan (part)", 19751,
  "Washington", 26918, "Waukesha (part)", 51906)),

## ---------------------------------------------------------------- 1996 (p.881-883) ----------------------------------------------------------
block(1996, 1, c("Lydia C. Spottswood", "Mark W. Neumann"), c("Dem.", "Rep."), tribble(
  ~county, ~c1, ~c2,
  "Green (part)", 2065, 2792, "Jefferson (part)", 687, 796, "Kenosha", 28174, 24878, "Racine", 36417, 39064, "Rock", 32053, 27355,
  "Walworth", 13051, 20179, "Waukesha (part)", 1701, 3344)),
block(1996, 2, c("Paul R. Soglin", "Ben Masel", "Scott L. Klug"), c("Dem.", "Lib.", "Rep."), tribble(
  ~county, ~c1, ~c2, ~c3,
  "Columbia", 6799, 251, 14529, "Dane", 86879, 3405, 101860, "Dodge (part)", 1033, 46, 2810, "Green", 2249, 91, 5653,
  "Iowa", 2739, 73, 5902, "Jefferson (part)", 331, 11, 562, "Lafayette", 1826, 23, 4326, "Richland", 2094, 56, 4960,
  "Sauk", 6517, 270, 13955)),
block(1996, 3, c("Ron Kind", "James E. Harsdorf"), c("Dem.", "Rep."), tribble(
  ~county, ~c1, ~c2,
  "Barron", 8668, 8035, "Buffalo", 2487, 2873, "Chippewa (part)", 152, 115, "Clark (part)", 3085, 3870, "Crawford", 3429, 3096,
  "Dunn", 7702, 6752, "Eau Claire (part)", 21817, 16981, "Grant", 7192, 10843, "Jackson", 3943, 3009, "La Crosse", 26874, 18919,
  "Monroe (part)", 2733, 2466, "Pepin", 1472, 1482, "Pierce", 6664, 7824, "Polk (part)", 3655, 3668, "St. Croix", 10478, 12313,
  "Trempealeau", 6194, 4316, "Vernon", 5422, 5584)),
block(1996, 4, c("Jerry Kleczka", "Tom Reynolds"), c("Dem.", "Rep."), tribble(
  ~county, ~c1, ~c2,
  "Milwaukee (part)", 106357, 63423, "Waukesha (part)", 28113, 35015)),
block(1996, 5, c("Tom Barrett", "Paul D. Melotik", "James D. Soderna"), c("Dem.", "Rep.", "Tax."), tribble(
  ~county, ~c1, ~c2, ~c3,
  "Milwaukee (part)", 141179, 47384, 3696)),
block(1996, 6, c("Al Lindskoog", "James Dean", "Thomas E. Petri", "Timothy Farness"), c("Dem.", "Lib.", "Rep.", "Tax."), tribble(
  ~county, ~c1, ~c2, ~c3, ~c4,
  "Adams", 2556, 83, 4266, 88, "Brown (part)", 141, 14, 566, 12, "Calumet (part)", 2816, 183, 11272, 159,
  "Fond du Lac (part)", 7725, 714, 28354, 408, "Green Lake", 1451, 193, 5807, 42, "Juneau", 1843, 90, 6123, 246,
  "Manitowoc", 9365, 439, 21842, 198, "Marquette", 1662, 69, 3981, 37, "Monroe (part)", 2167, 86, 6350, 88,
  "Outagamie (part)", 2464, 165, 6628, 143, "Sheboygan (part)", 2271, 137, 8826, 130, "Waupaca", 4277, 184, 13374, 161,
  "Waushara", 2045, 98, 5964, 102, "Winnebago", 14594, 2039, 45860, 718)),
block(1996, 7, c("David R. Obey", "Scott West"), c("Dem.", "Rep."), tribble(
  ~county, ~c1, ~c2,
  "Ashland", 4497, 2214, "Bayfield", 4739, 2473, "Burnett", 4133, 2668, "Chippewa (part)", 11616, 9087, "Clark (part)", 2831, 2355,
  "Douglas", 12161, 5964, "Eau Claire (part)", 206, 160, "Iron", 2029, 1271, "Lincoln", 6888, 5428, "Marathon", 26019, 25227,
  "Oneida (part)", 3442, 2216, "Polk (part)", 5106, 2930, "Portage", 16954, 12234, "Price", 4200, 3010, "Rusk", 3829, 2543,
  "Sawyer", 3122, 3081, "Taylor", 4014, 3703, "Washburn", 3789, 2956, "Wood", 17853, 13845)),
block(1996, 8, c("Jay Johnson", "David Prosser"), c("Dem.", "Rep."), tribble(
  ~county, ~c1, ~c2,
  "Brown (part)", 50300, 39928, "Calumet (part)", 738, 683, "Door", 6708, 5812, "Florence", 966, 1047, "Forest", 1904, 1572,
  "Kewaunee", 4692, 3748, "Langlade", 3428, 4593, "Manitowoc (part)", 160, 167, "Marinette", 10043, 7800, "Menominee", 967, 254,
  "Oconto", 7387, 6082, "Oneida (part)", 3747, 6569, "Outagamie (part)", 28003, 26743, "Shawano", 7175, 7688, "Vilas", 3333, 6712)),
block(1996, 9, c("Floyd Brenholt", "F. James Sensenbrenner, Jr."), c("Dem.", "Rep."), tribble(
  ~county, ~c1, ~c2,
  "Dodge (part)", 7383, 17877, "Fond du Lac (part)", 94, 228, "Jefferson (part)", 8382, 17932, "Ozaukee", 8672, 29906,
  "Sheboygan (part)", 11417, 23528, "Washington", 11509, 35680, "Waukesha (part)", 20283, 72759))
)

## ---- district-level tie-check (permanent regression guard) ----------------------------------------------------------------------------------
printed_totals <- tribble(
  ~year, ~district, ~candidate, ~total,
  1990, 1, "Les Aspin", 93961,
  1990, 2, "Robert W. Kastenmeier", 85156, 1990, 2, "Scott L. Klug", 96938,
  1990, 3, "James L Ziegeweid", 60409, 1990, 3, "Steven C. Gunderson", 94509,
  1990, 4, "Gerald D. Kleczka", 96981, 1990, 4, "Joseph L. Cook", 43001,
  1990, 5, "Jim Moody", 77557, 1990, 5, "Donalda Arnell Hammersmith", 31255, 1990, 5, "Nathaniel J. Stampley", 4968,
  1990, 6, "Thomas E. Petri", 111036,
  1990, 7, "David R. Obey", 100069, 1990, 7, "John L. McEwen", 60961,
  1990, 8, "Jerome Van Sistine", 83199, 1990, 8, "Toby Roth", 95902,
  1990, 9, "F. James Sensenbrenner, Jr.", 117967,
  1992, 1, "Les Aspin", 147495, 1992, 1, "Mark W. Neumann", 104352, 1992, 1, "John Graf", 4391,
  1992, 2, "Ada E. Deer", 108291, 1992, 2, "Scott Klug", 183366, 1992, 2, "Joseph E. Schumacher", 1140,
  1992, 3, "Paul Sacia", 108664, 1992, 3, "Steven C. Gunderson", 146903, 1992, 3, "Jay B. Evenson", 4736,
  1992, 4, "Gerald Kleczka", 173482, 1992, 4, "John Washburn", 2488, 1992, 4, "Joseph L. Cook", 84872, 1992, 4, "Daniel Slak", 2803,
  1992, 5, "Thomas M. Barrett", 162344, 1992, 5, "Donalda Ann Hammersmith", 71085,
  1992, 6, "Peggy A. Lautenschlager", 128232, 1992, 6, "Thomas E. Petri", 143875,
  1992, 7, "David R. Obey", 166200, 1992, 7, "Dale R. Vannes", 91772,
  1992, 8, "Catherine L. Helms", 81792, 1992, 8, "Toby Roth", 191704,
  1992, 9, "Ingrid K. Buxton", 77362, 1992, 9, "Jeffrey Holt Millikin", 1881, 1992, 9, "F. James Sensenbrenner, Jr.", 192898, 1992, 9, "David E. Marlow", 4619,
  1994, 1, "Peter W. Barca", 82817, 1994, 1, "Edward J. Kozak", 3085, 1994, 1, "Mark W. Neumann", 83937,
  1994, 2, "Thomas C. Hecht", 55406, 1994, 2, "Scott L. Klug", 133734, 1994, 2, "John J. Stumpf", 2676, 1994, 2, "Joseph E. Schumacher", 1327,
  1994, 3, "Harvey Stower", 65758, 1994, 3, "Steve Gunderson", 89338, 1994, 3, "Chuck Lee", 2837, 1994, 3, "Mark Weinhold", 2279,
  1994, 4, "Gerald Kleczka", 93789, 1994, 4, "Tom Reynolds", 78225, 1994, 4, "James Harold Hause", 2611,
  1994, 5, "Tom Barrett", 87806, 1994, 5, "Stephen B. Hollingshead", 51145, 1994, 5, "David J. Schall", 1576,
  1994, 6, "Thomas E. Petri", 119384,
  1994, 7, "David R. Obey", 97184, 1994, 7, "Scott West", 81706,
  1994, 8, "Stan Gruszynski", 65065, 1994, 8, "Toby Roth", 114319,
  1994, 9, "F. James Sensenbrenner, Jr.", 141617,
  1996, 1, "Lydia C. Spottswood", 114148, 1996, 1, "Mark W. Neumann", 118408,
  1996, 2, "Paul R. Soglin", 110467, 1996, 2, "Ben Masel", 4226, 1996, 2, "Scott L. Klug", 154557,
  1996, 3, "Ron Kind", 121967, 1996, 3, "James E. Harsdorf", 112146,
  1996, 4, "Jerry Kleczka", 134470, 1996, 4, "Tom Reynolds", 98438,
  1996, 5, "Tom Barrett", 141179, 1996, 5, "Paul D. Melotik", 47384, 1996, 5, "James D. Soderna", 3696,
  1996, 6, "Al Lindskoog", 55377, 1996, 6, "James Dean", 4494, 1996, 6, "Thomas E. Petri", 169213, 1996, 6, "Timothy Farness", 2532,
  1996, 7, "David R. Obey", 137428, 1996, 7, "Scott West", 103365,
  1996, 8, "Jay Johnson", 129551, 1996, 8, "David Prosser", 119398,
  1996, 9, "Floyd Brenholt", 67740, 1996, 9, "F. James Sensenbrenner, Jr.", 197910
)
chk <- raw %>% group_by(year, district, candidate) %>% summarise(v = sum(votes), .groups = "drop") %>%
  inner_join(printed_totals, by = c("year", "district", "candidate"))
stopifnot(nrow(chk) == nrow(printed_totals))                                  # every printed total has a matching transcribed candidate
bad <- chk %>% filter(v != total)
cat("district totals checked:", nrow(chk), "| exact ties:", sum(chk$v == chk$total), "\n")
stopifnot(nrow(bad) == 0)

## ---- county FIPS + party_group, build the long table per year --------------------------------------------------------------------------------
raw <- raw %>% mutate(county_fips = fips_of_county(county), party_group = party_group_of(party))
stopifnot(!anyNA(raw$county_fips))
for (y in c(1990, 1992, 1994, 1996)) {
  d <- raw %>% filter(year == y) %>%
    transmute(year, county_fips, district = norm_district(district), candidate, party, party_group, votes) %>%
    group_by(year, county_fips, district, candidate, party, party_group) %>% summarise(votes = sum(votes), .groups = "drop")   # sum "(part)" fragments of the same county+district together (none expected, but safe)
  long <- finalize_long(d, paste0("wi_", y))
  save_long(long, paste0("he_wi_", y))
  shares <- derive_shares(long) %>% transmute(state = "WISCONSIN", year, cty_fips, sample, demovote, repuvote, totalvote)
  saveRDS(shares, file.path(OUTPUT_DIR, sprintf("elect_he_cty_wi_%d.rds", y)))
  r <- check_long_vs_source(long, file.path(OUTPUT_DIR, sprintf("elect_he_cty_wi_%d.rds", y))); stopifnot(all(r$pass))
  message(y, ": ", nrow(shares), " of 72 WI counties")
}

sanity <- bind_rows(lapply(c(1990, 1992, 1994, 1996), function(y) readRDS(file.path(OUTPUT_DIR, sprintf("elect_he_cty_wi_%d.rds", y)))))
share <- sanity$repuvote + sanity$demovote
message("Sanity: repuvote+demovote range [", round(min(share), 3), ", ", round(max(share), 3), "]")
