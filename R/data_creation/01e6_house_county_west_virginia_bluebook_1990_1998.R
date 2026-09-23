## West Virginia House 1990, 1992, 1994, 1996, 1998: the remaining odd-year WV Blue Book editions
## (`WVS_Bluebook_1991/1993/1995/1997/1999.pdf`, scanned/ClearScan-OCR volumes) each carry the PRIOR
## even year's "GENERAL ELECTION RETURNS" section with a clean U.S. House table per district, county
## rows, a printed TOTAL row -- e.g. the 1991 edition covers the November 1990 general election. This
## closes what was, before this session, a complete WV House 1990-1998 gap (all 5 years
## `source_not_found`; OpenElections' WV repo starts at 2000) -- see 01e3/01e4/01e5 for the 2010/2012
## Blue Book work earlier this session.
##
## District maps changed twice across this span: 1990 used the pre-1990-census map (4 districts);
## 1992-1998 used the post-1990-census 3-district map (same map as 2000-2010, confirmed by identical
## county-to-district groupings across all of 1992/1994/1996/1998 and 01az's 2000-2006 builds).
## Two districts were genuinely uncontested by a Republican in some years (no fabricated opponent):
## 1990 CD3 (Wise, no opponent at all), 1992 CD1 (Mollohan, no opponent at all), 1996 CD1 and CD3
## (Mollohan, Rahall, both no opponent at all), 1998 CD1 and CD3 (Mollohan, Rahall, opposed only by a
## Libertarian, not a Republican). These are real `one_party` cases (repuvote = 0), not parsing gaps.
##
## Verified: every single district's county column sums to the book's own printed TOTAL row exactly,
## for all 4 (1990) + 3 + 3 + 3 + 3 = 16 district-year tables. One OCR-garbled county name (1996 CD3's
## last county, printed with no legible name at all before its vote count) was identified as Wyoming
## by elimination (every other CD3 county already accounted for) and confirmed by the district
## total tying out exactly once it was included.
source(file.path("R", "00_setup.R"))
library(readr)

county_fips_crosswalk <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))
wv_fips <- county_fips_crosswalk %>% filter(state == "WEST VIRGINIA") %>% select(county_name, county_fips)
stopifnot(nrow(wv_fips) == 55)

build_year <- function(year, districts, totals) {
  for (i in seq_along(districts)) {
    d <- districts[[i]]; tot <- totals[[i]]
    for (nm in names(tot)) stopifnot(sum(d[[nm]]) == tot[nm])
    message(year, " CD", i, ": ", nrow(d), " counties, ", paste(names(tot), sum_named <- vapply(names(tot), function(nm) sum(d[[nm]]), 0), sep = "=", collapse = ", "), " (all match printed totals)")
  }
  all_d <- bind_rows(districts) %>% mutate(across(-county, ~ tidyr::replace_na(.x, 0)))
  stopifnot(nrow(all_d) == 55, !anyDuplicated(all_d$county))
  vote_cols <- setdiff(names(all_d), "county")
  dem_col <- vote_cols[grepl("^dem", vote_cols)][1]; rep_col <- vote_cols[grepl("^rep", vote_cols)][1]
  other_cols <- setdiff(vote_cols, c(dem_col, rep_col))
  ## NOTE: rep_col is always present (every year here has at least one district with a Republican
  ## column), so a plain column reference is correct -- an `ifelse()` guard here would be a real bug:
  ## with a scalar condition it returns a length-1 result that dplyr recycles, silently overwriting
  ## every row with the FIRST row's value instead of each row's own (caught by a Kanawha 1990 sanity
  ## check: repuvote showed 0.044 for an unopposed race that should have been exactly 0).
  stopifnot(!is.null(rep_col), rep_col %in% names(all_d))
  all_d %>% mutate(dem = .data[[dem_col]], rep = .data[[rep_col]],
                    other = rowSums(across(all_of(other_cols)))) %>%
    left_join(wv_fips, by = c("county" = "county_name")) %>%
    transmute(state = "WEST VIRGINIA", year = !!year, cty_fips = county_fips, sample = "HE",
              totalvote = dem + rep + other, demovote = dem / totalvote, repuvote = rep / totalvote)
}

## ---- 1990 (4 districts; pre-1990-census map) ----------------------------------------------------
d1990_cd1 <- tribble(~county, ~dem, ~rep,
  "BROOKE", 4167, 1485, "DODDRIDGE", 987, 922, "HANCOCK", 6030, 2592, "HARRISON", 14188, 5597,
  "MARION", 11137, 4374, "MARSHALL", 5961, 3001, "OHIO", 8188, 3713, "PLEASANTS", 1166, 697,
  "RITCHIE", 1396, 1034, "TAYLOR", 2769, 1216, "TYLER", 1553, 951, "WETZEL", 3065, 1471, "WOOD", 12242, 8604)
d1990_cd2 <- tribble(~county, ~dem, ~rep,
  "BARBOUR", 3685, 2900, "BERKELEY", 5122, 5502, "FAYETTE", 6150, 3381, "GRANT", 1189, 1369,
  "GREENBRIER", 3908, 3724, "HAMPSHIRE", 2122, 1474, "HARDY", 1695, 1037, "JEFFERSON", 3544, 2872,
  "MINERAL", 5376, 2075, "MONONGALIA", 9475, 8067, "MONROE", 1665, 1538, "MORGAN", 1300, 1625,
  "PENDLETON", 1429, 772, "POCAHONTAS", 1568, 1234, "PRESTON", 3567, 3828, "RANDOLPH", 4210, 2975,
  "SUMMERS", 1837, 1368, "TUCKER", 1454, 957, "UPSHUR", 2365, 3236, "WEBSTER", 1513, 774)
d1990_cd3 <- tribble(~county, ~dem,   # unopposed -- no Republican column in the source
  "BOONE", 3763, "BRAXTON", 2546, "CALHOUN", 2036, "CLAY", 1857, "GILMER", 1471, "JACKSON", 4642,
  "KANAWHA", 32558, "LEWIS", 3627, "LINCOLN", 2475, "MASON", 5926, "NICHOLAS", 4266, "PUTNAM", 6358,
  "ROANE", 2833, "WIRT", 969)
d1990_cd4 <- tribble(~county, ~dem, ~rep,
  "CABELL", 8158, 11301, "LOGAN", 4223, 2045, "MCDOWELL", 4164, 1403, "MERCER", 5315, 6833,
  "MINGO", 3641, 1558, "RALEIGH", 7153, 7704, "WAYNE", 4173, 4035, "WYOMING", 3121, 2067)
wv_1990 <- build_year(1990,
  list(d1990_cd1, d1990_cd2, d1990_cd3, d1990_cd4),
  list(c(dem = 72849, rep = 35657), c(dem = 63174, rep = 50708), c(dem = 75327), c(dem = 39948, rep = 36946)))

## ---- 1992 (3 districts; new post-1990-census map, matches 2000-2010) ----------------------------
d1992_cd1 <- tribble(~county, ~dem,   # Mollohan unopposed
  "BARBOUR", 4714, "BROOKE", 8230, "DODDRIDGE", 1869, "GRANT", 1451, "HANCOCK", 10752, "HARRISON", 25047,
  "MARION", 20628, "MARSHALL", 11580, "MINERAL", 5708, "MONONGALIA", 19727, "OHIO", 14178, "PLEASANTS", 2481,
  "PRESTON", 6364, "RITCHIE", 2793, "TAYLOR", 4810, "TUCKER", 2353, "TYLER", 2956, "WETZEL", 5380, "WOOD", 21903)
d1992_cd2 <- tribble(~county, ~dem, ~rep,
  "BERKELEY", 8937, 8795, "BRAXTON", 4405, 711, "CALHOUN", 2212, 581, "CLAY", 2516, 731, "GILMER", 2240, 625,
  "HAMPSHIRE", 2803, 2276, "HARDY", 2473, 1563, "JACKSON", 7230, 2690, "JEFFERSON", 5947, 4606, "KANAWHA", 54222, 17666,
  "LEWIS", 5061, 1229, "MASON", 8345, 2350, "MORGAN", 1792, 2360, "NICHOLAS", 6411, 1569, "PENDLETON", 1933, 1038,
  "PUTNAM", 10704, 4642, "RANDOLPH", 6155, 1997, "ROANE", 4198, 1253, "UPSHUR", 4663, 1919, "WIRT", 1741, 501)
d1992_cd3 <- tribble(~county, ~dem, ~rep,
  "BOONE", 6516, 1748, "CABELL", 16843, 12196, "FAYETTE", 9604, 4217, "GREENBRIER", 5837, 5348, "LINCOLN", 4976, 2422,
  "LOGAN", 11723, 3034, "MCDOWELL", 7360, 1739, "MERCER", 10614, 8592, "MINGO", 7823, 1701, "MONROE", 2446, 2482,
  "POCAHONTAS", 1936, 1529, "RALEIGH", 15208, 9190, "SUMMERS", 2644, 1878, "WAYNE", 9932, 4701, "WEBSTER", 2500, 773,
  "WYOMING", 6317, 2462)
wv_1992 <- build_year(1992, list(d1992_cd1, d1992_cd2, d1992_cd3),
  list(c(dem = 172924), c(dem = 143988, rep = 59102), c(dem = 122279, rep = 64012)))

## ---- 1994 (same 3-district map) ------------------------------------------------------------------
d1994_cd1 <- tribble(~county, ~dem, ~rep,
  "BARBOUR", 3224, 1268, "BROOKE", 4593, 1489, "DODDRIDGE", 999, 628, "GRANT", 1479, 1238, "HANCOCK", 5751, 2452,
  "HARRISON", 14030, 4082, "MARION", 11624, 2687, "MARSHALL", 6326, 2728, "MINERAL", 3410, 2452, "MONONGALIA", 10925, 5521,
  "OHIO", 8517, 4066, "PLEASANTS", 1462, 722, "PRESTON", 4883, 2248, "RITCHIE", 1697, 942, "TAYLOR", 3190, 797,
  "TUCKER", 1900, 625, "TYLER", 1603, 842, "WETZEL", 3611, 1275, "WOOD", 13953, 7528)
d1994_cd2 <- tribble(~county, ~dem, ~rep,
  "BERKELEY", 5669, 6308, "BRAXTON", 2770, 923, "CALHOUN", 1271, 457, "CLAY", 1560, 663, "GILMER", 1404, 594,
  "HAMPSHIRE", 2314, 1701, "HARDY", 2060, 929, "JACKSON", 4377, 2504, "JEFFERSON", 4404, 3442, "KANAWHA", 31576, 17433,
  "LEWIS", 3220, 1371, "MASON", 4675, 1963, "MORGAN", 1431, 1622, "NICHOLAS", 3949, 1435, "PENDLETON", 1200, 494,
  "PUTNAM", 6493, 4470, "RANDOLPH", 5431, 2057, "ROANE", 2174, 1149, "UPSHUR", 3833, 1810, "WIRT", 946, 366)
d1994_cd3 <- tribble(~county, ~dem, ~rep,
  "BOONE", 3852, 1198, "CABELL", 11891, 9645, "FAYETTE", 6136, 2796, "GREENBRIER", 3994, 3512, "LINCOLN", 3349, 1742,
  "LOGAN", 5967, 1518, "MCDOWELL", 3979, 929, "MERCER", 6967, 5983, "MINGO", 3848, 924, "MONROE", 2016, 1521,
  "POCAHONTAS", 1866, 868, "RALEIGH", 9001, 6027, "SUMMERS", 1673, 898, "WAYNE", 5403, 2978, "WEBSTER", 1623, 476,
  "WYOMING", 3402, 1367)
wv_1994 <- build_year(1994, list(d1994_cd1, d1994_cd2, d1994_cd3),
  list(c(dem = 103177, rep = 43590), c(dem = 90757, rep = 51691), c(dem = 74967, rep = 42382)))

## ---- 1996 (same 3-district map) ------------------------------------------------------------------
d1996_cd1 <- tribble(~county, ~dem,   # Mollohan unopposed
  "BARBOUR", 4405, "BROOKE", 7519, "DODDRIDGE", 1657, "GRANT", 1939, "HANCOCK", 9935, "HARRISON", 22396,
  "MARION", 18330, "MARSHALL", 10557, "MINERAL", 6381, "MONONGALIA", 21270, "OHIO", 14817, "PLEASANTS", 2460,
  "PRESTON", 7943, "RITCHIE", 2560, "TAYLOR", 4485, "TUCKER", 2375, "TYLER", 2672, "WETZEL", 5137, "WOOD", 24496)
d1996_cd2 <- tribble(~county, ~dem, ~rep,
  "BERKELEY", 9809, 8785, "BRAXTON", 3823, 884, "CALHOUN", 2030, 585, "CLAY", 2697, 754, "GILMER", 1826, 648,
  "HAMPSHIRE", 3034, 2606, "HARDY", 2679, 1226, "JACKSON", 6695, 2914, "JEFFERSON", 6834, 4881, "KANAWHA", 50533, 20140,
  "LEWIS", 4576, 1537, "MASON", 7793, 2590, "MORGAN", 2478, 2111, "NICHOLAS", 5933, 1564, "PENDLETON", 2222, 703,
  "PUTNAM", 11655, 5474, "RANDOLPH", 7332, 2568, "ROANE", 3453, 1466, "UPSHUR", 4659, 1945, "WIRT", 1490, 552)
d1996_cd3 <- tribble(~county, ~dem,   # Rahall unopposed; last county's OCR'd name is illegible -- identified as
  "BOONE", 7235, "CABELL", 23241, "FAYETTE", 11928, "GREENBRIER", 9240, "LINCOLN", 6051, "LOGAN", 12117,   # Wyoming
  "MCDOWELL", 7226, "MERCER", 13376, "MINGO", 9269, "MONROE", 3234, "POCAHONTAS", 2504, "RALEIGH", 17469,  # by elimination
  "SUMMERS", 3159, "WAYNE", 9852, "WEBSTER", 2789, "WYOMING", 6860)                                        # (see header note)
wv_1996 <- build_year(1996, list(d1996_cd1, d1996_cd2, d1996_cd3),
  list(c(dem = 171334), c(dem = 141551, rep = 63933), c(dem = 145550)))

## ---- 1998 (same 3-district map; CD1/CD3 opposed only by a Libertarian, not a Republican) --------
d1998_cd1 <- tribble(~county, ~dem, ~lib,
  "BARBOUR", 3198, 284, "BROOKE", 4229, 576, "DODDRIDGE", 1252, 162, "GRANT", 1446, 228, "HANCOCK", 7106, 831,
  "HARRISON", 13005, 1888, "MARION", 10540, 1440, "MARSHALL", 6489, 1060, "MINERAL", 4784, 860, "MONONGALIA", 10100, 4195,
  "OHIO", 8177, 1497, "PLEASANTS", 1916, 253, "PRESTON", 4475, 1342, "RITCHIE", 1780, 217, "TAYLOR", 3210, 369,
  "TUCKER", 1810, 214, "TYLER", 1560, 221, "WETZEL", 3544, 458, "WOOD", 16480, 2918)
d1998_cd2 <- tribble(~county, ~dem, ~rep, ~lib,
  "BERKELEY", 7627, 3697, 469, "BRAXTON", 2599, 471, 66, "CALHOUN", 1385, 285, 39, "CLAY", 1943, 369, 84,
  "GILMER", 1417, 317, 62, "HAMPSHIRE", 3019, 1236, 157, "HARDY", 1959, 504, 74, "JACKSON", 5321, 1453, 329,
  "JEFFERSON", 4871, 2155, 305, "KANAWHA", 33752, 9017, 3298, "LEWIS", 2934, 675, 122, "MASON", 6102, 1365, 294,
  "MORGAN", 2297, 1186, 143, "NICHOLAS", 3005, 572, 245, "PENDLETON", 1401, 310, 36, "PUTNAM", 8058, 2283, 1323,
  "RANDOLPH", 4707, 937, 242, "ROANE", 3092, 816, 123, "UPSHUR", 2824, 1226, 201, "WIRT", 1044, 262, 48)
d1998_cd3 <- tribble(~county, ~dem, ~lib,
  "BOONE", 3560, 403, "CABELL", 13109, 2527, "FAYETTE", 6781, 1059, "GREENBRIER", 4872, 945, "LINCOLN", 2627, 337,
  "LOGAN", 5173, 443, "MCDOWELL", 4299, 506, "MERCER", 8137, 1420, "MINGO", 3604, 281, "MONROE", 2314, 324,
  "POCAHONTAS", 1943, 253, "RALEIGH", 8848, 2123, "SUMMERS", 2168, 332, "WAYNE", 5929, 525, "WEBSTER", 1501, 105,
  "WYOMING", 3949, 613)
wv_1998 <- build_year(1998, list(d1998_cd1, d1998_cd2, d1998_cd3),
  list(c(dem = 105101, lib = 19013), c(dem = 99357, rep = 29136, lib = 7660), c(dem = 78814, lib = 12196)))

wv_bluebook_1990s <- bind_rows(wv_1990, wv_1992, wv_1994, wv_1996, wv_1998)
stopifnot(nrow(wv_bluebook_1990s) == 55 * 5, !anyNA(wv_bluebook_1990s$cty_fips))
sanity <- wv_bluebook_1990s$demovote + wv_bluebook_1990s$repuvote
message("sanity range demovote+repuvote: [", round(min(sanity), 3), ", ", round(max(sanity), 3), "] (< 1 where a Libertarian took votes)")
stopifnot(all(sanity >= 0 & sanity <= 1.001))

for (yr in c(1990, 1992, 1994, 1996, 1998)) {
  out <- wv_bluebook_1990s %>% filter(year == yr) %>% select(-state, -sample) %>% mutate(state = "WEST VIRGINIA", sample = "HE")
  saveRDS(out %>% select(state, year, cty_fips, sample, demovote, repuvote, totalvote), file.path(OUTPUT_DIR, paste0("elect_he_cty_wv_bluebook_", yr, ".rds")))
}
message("Saved elect_he_cty_wv_bluebook_{1990,1992,1994,1996,1998}.rds: 55 counties each")
