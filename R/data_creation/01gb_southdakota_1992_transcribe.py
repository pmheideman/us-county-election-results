"""South Dakota U.S. House (at-large) 1992 by county, transcribed from page 5 of the Secretary of State's scanned "South Dakota General Election November 3, 1992" (1992G.pdf, linked from
https://sdsos.gov/elections-voting/election-resources/election-history/official-election-returns.aspx; saved as R/data/county_house_files/south_dakota/SD_1992_general.pdf). Columns: Tim Johnson (D),
John Timmer (R), Robert J. Newland (L), Ronald Wieczorek (I), Ann Balakier (I). Check: the 66 county rows add up to the printed TOTALS row of every candidate (stop on failure).
Output: R/data/county_house_files/south_dakota/south_dakota_house_county_1992.csv."""
import csv, sys
ROWS = """AURORA 1165 400 13 123 6
BEADLE 6797 1992 88 185 63
BENNETT 799 311 42 33 4
BON HOMME 2671 587 21 76 24
BROOKINGS 8493 3021 100 226 86
BROWN 13875 3601 90 257 168
BRULE 1940 526 18 149 19
BUFFALO 369 89 3 23 6
BUTTE 2100 1248 214 49 25
CAMPBELL 636 341 13 20 4
CHARLES MIX 3060 848 18 136 18
CLARK 1678 583 28 36 18
CLAY 4752 964 58 89 134
CODINGTON 7476 2921 55 275 104
CORSON 677 386 81 29 23
CUSTER 2072 1025 115 44 43
DAVISON 5605 1936 42 511 20
DAY 2822 746 20 63 14
DEUEL 1764 518 30 65 21
DEWEY 1248 394 32 36 19
DOUGLAS 1122 716 8 190 13
EDMUNDS 1598 558 18 32 8
FALL RIVER 2437 985 128 73 41
FAULK 1006 338 16 37 10
GRANT 2744 1171 36 88 36
GREGORY 1805 665 21 79 27
HAAKON 737 508 29 20 4
HAMLIN 1779 829 27 62 23
HAND 1838 573 21 75 10
HANSON 1022 329 13 62 6
HARDING 481 333 32 7 4
HUGHES 5267 2519 62 133 49
HUTCHINSON 2825 1176 14 126 20
HYDE 634 243 16 35 3
JACKSON 696 405 19 15 10
JERAULD 1071 344 2 47 6
JONES 474 269 21 9 3
KINGSBURY 2369 672 27 48 19
LAKE 4119 1324 23 64 42
LAWRENCE 6030 2942 330 161 122
LINCOLN 5436 2224 38 97 36
LYMAN 970 431 17 41 10
MARSHALL 1738 486 16 17 12
MCCOOK 2080 740 21 99 10
MCPHERSON 981 634 19 20 11
MEADE 5997 3337 205 251 166
MELLETTE 595 217 13 17 12
MINER 1141 351 7 56 9
MINNEHAHA 43850 17735 344 658 434
MOODY 2377 640 21 39 24
PENNINGTON 23478 12256 789 629 334
PERKINS 1166 660 60 64 26
POTTER 1138 526 35 48 11
ROBERTS 2735 962 34 110 28
SANBORN 1123 340 12 98 12
SHANNON 1373 130 27 41 34
SPINK 3032 891 26 81 20
STANLEY 977 367 15 22 6
SULLY 654 313 16 20 3
TODD 1268 278 27 22 29
TRIPP 2241 996 38 68 22
TURNER 2984 1195 8 67 23
UNION 3608 1148 46 115 88
WALWORTH 1810 878 69 57 21
YANKTON 6852 2084 60 203 117
ZIEBACH 413 220 24 18 7"""
CANDS = [("Tim Johnson", "D"), ("John Timmer", "R"), ("Robert J. Newland", "L"), ("Ronald Wieczorek", "I"), ("Ann Balakier", "I")]
TOTALS = [230070, 89375, 3931, 6746, 2780]
rows = []
for l in ROWS.split("\n"):
    p = l.split(); rows.append((" ".join(p[:-5]), [int(x) for x in p[-5:]]))
assert len(rows) == 66, len(rows)
sums = [sum(v[k] for _, v in rows) for k in range(5)]
if sums != TOTALS: sys.exit(f"TIE FAILED: county sums {sums} vs printed totals {TOTALS}; differences {[a - b for a, b in zip(sums, TOTALS)]}")
w = csv.writer(open("R/data/county_house_files/south_dakota/south_dakota_house_county_1992.csv", "w", newline="")); w.writerow(["year", "county", "candidate", "party_code", "votes"])
for c, v in rows:
    for (n, p), x in zip(CANDS, v): w.writerow([1992, c, n, p, x])
print(len(rows), "counties; all five candidates tie to the printed totals")
