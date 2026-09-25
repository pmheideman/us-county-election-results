"""Florida U.S. House 1990 and 1994, county level, hand-transcribed from Florida Division of
Elections reports on the Internet Archive's Statistical Reference Index microfiche.

1990: micro_IA40706938_0349 '1990 General Election Results, November 6, 1990' (report dated
12/05/90). 'United States Representative, District 0nn' tables on printed pages 1-8 (scan pages
2-9): county rows, Sub-total, 'Absentees **' (absentee ballots counted pursuant to Rule 1S-2.013),
Total. The Rule 1S-2.013 absentees are broken down BY COUNTY on printed pages 9-14 (scan pages
10-15), so each county's value here = main table + its absentee row.
Checks: county rows sum to the printed Sub-total; absentee rows sum to the printed Absentees line;
Sub-total + Absentees = Total; every candidate Total and every district total ('Total votes cast')
equals the FEC 1990 results (R/data/fec_official/fec1990_house.csv) exactly.

1994: micro_IA40706952_0277 'General Election, Nov. 8, 1994: Official Results' (county reporting
status printout dated Nov 15), 'Race: Representative In Congress District: n' tables on printed
pages 2-7 (scan pages 6-11); counties are 4-letter abbreviations (mapped below).
Checks: county rows sum to the printed Totals. The Clerk of the House 1994 statistics (certified
later) exceed these Totals by a few votes per candidate (late overseas absentee ballots, not broken
down by county in this report): each candidate's Clerk total must be >= ours and within 100 votes
and 0.2%; the differences are printed.

Named write-in candidates (WRI) are kept as party I: the official district totals include them.
Unopposed seats are not on the Florida ballot and have no table: 1990 Districts 8, 10, 12, 13, 16;
1994 Districts 4, 10, 13, 14, 18, 23. Counties that lie only in those districts are absent.

Output: R/data/county_house_files/sri/florida_house_county_1990.csv and florida_house_county_1994_verification.csv (1994 equals the existing OpenElections rows in all 61 counties, so it is kept as a verification copy, not a source)
(year,district,county,candidate,party_code,votes)
"""
import csv, os

HERE = os.path.dirname(os.path.abspath(__file__))
OUT = os.path.join(HERE, "../data/county_house_files/sri")

# ---------------------------------------------------------------- 1990
# district: (candidates [(name, party)], {county: main votes}, {county: absentee votes}, subtotal, absentee line, FEC district total)
D90 = {
    1: ([("Terry Ketchel", "R"), ("Earl Hutto", "D")],
        {"BAY": (13466, 15542), "ESCAMBIA": (30835, 37936), "OKALOOSA": (22294, 17352), "SANTA ROSA": (10262, 12452), "WALTON": (3931, 5072)},
        {"BAY": (10, 8), "ESCAMBIA": (24, 21), "OKALOOSA": (21, 26), "SANTA ROSA": (6, 3), "WALTON": (2, 4)},
        (80788, 88354), (63, 62), 169267),
    2: ([("Bill Grant", "R"), ("Pete Peterson", "D")],
        {"BAKER": (2202, 1831), "BAY": (1216, 979), "BRADFORD": (2705, 2825), "CALHOUN": (1527, 1782), "CLAY": (3902, 2581),
         "COLUMBIA": (4700, 5053), "DIXIE": (1539, 2066), "FRANKLIN": (1181, 1563), "GADSDEN": (3357, 6424), "GILCHRIST": (1247, 1618),
         "GULF": (2068, 2300), "HAMILTON": (1007, 1693), "HOLMES": (2399, 1827), "JACKSON": (5414, 5786), "JEFFERSON": (1413, 2366),
         "LAFAYETTE": (884, 937), "LEON": (23386, 41047), "LEVY": (3316, 3740), "LIBERTY": (781, 1032), "MADISON": (1687, 2748),
         "SUWANNEE": (3602, 3513), "TAYLOR": (2306, 2626), "UNION": (922, 1346), "WAKULLA": (2416, 2985), "WASHINGTON": (2720, 2339)},
        {"BAKER": (0, 0), "BAY": (2, 0), "BRADFORD": (0, 0), "CALHOUN": (0, 0), "CLAY": (9, 1), "COLUMBIA": (3, 2), "DIXIE": (0, 0),
         "FRANKLIN": (0, 0), "GADSDEN": (1, 2), "GILCHRIST": (0, 0), "GULF": (0, 2), "HAMILTON": (0, 0), "HOLMES": (0, 0), "JACKSON": (2, 0),
         "JEFFERSON": (0, 1), "LAFAYETTE": (0, 0), "LEON": (16, 12), "LEVY": (0, 0), "LIBERTY": (0, 0), "MADISON": (2, 3), "SUWANNEE": (2, 0),
         "TAYLOR": (2, 0), "UNION": (0, 1), "WAKULLA": (2, 1), "WASHINGTON": (1, 0)},
        (77897, 103007), (42, 25), 180971),
    3: ([("Rod Sullivan", "R"), ("Charles E. Bennett", "D")],
        {"DUVAL": (27737, 77008), "NASSAU": (3966, 7253)}, {"DUVAL": (22, 19), "NASSAU": (2, 0)},
        (31703, 84261), (24, 19), 116007),
    4: ([("Craig T. James", "R"), ("Reid Hughes", "D"), ("Ken McCarthy", "I")],
        {"CLAY": (11999, 7082, 4), "DUVAL": (23154, 17186, 3), "FLAGLER": (5631, 4839, 1), "PUTNAM": (7678, 8968, 0),
         "ST. JOHNS": (14453, 10193, 0), "VOLUSIA": (57889, 47025, 5)},
        {"CLAY": (69, 19, 0), "DUVAL": (8, 2, 0), "FLAGLER": (3, 0, 0), "PUTNAM": (0, 0, 0), "ST. JOHNS": (2, 1, 0), "VOLUSIA": (9, 5, 0)},
        (120804, 95293, 13), (91, 27, 0), 216228),
    5: ([("Bill McCollum", "R"), ("Bob Fletcher", "D")],
        {"LAKE": (4497, 2435), "ORANGE": (48203, 35112), "SEMINOLE": (41717, 25696)},
        {"LAKE": (0, 0), "ORANGE": (26, 7), "SEMINOLE": (10, 3)},
        (94417, 63243), (36, 10), 157706),
    6: ([("Clifford B. Stearns", "R"), ("Art Johnson", "D"), ("Kim O'Connor", "I")],
        {"ALACHUA": (24400, 23184, 1), "CITRUS": (20498, 13944, 0), "HERNANDO": (22381, 19061, 0), "LAKE": (23697, 12530, 0),
         "MARION": (36743, 17535, 18), "PASCO": (5815, 5246, 1), "PUTNAM": (464, 532, 0), "SUMTER": (4549, 3378, 0)},
        {"ALACHUA": (9, 5, 0), "CITRUS": (14, 0, 0), "HERNANDO": (5, 2, 0), "LAKE": (7, 3, 0), "MARION": (2, 1, 0),
         "PASCO": (2, 0, 0), "PUTNAM": (0, 0, 0), "SUMTER": (2, 0, 0)},
        (138547, 95410, 20), (41, 11, 0), 234029),
    7: ([("Charles Prout", "R"), ("Sam M. Gibbons", "D")],
        {"HILLSBOROUGH": (47754, 99454)}, {"HILLSBOROUGH": (11, 10)}, (47754, 99454), (11, 10), 147229),
    9: ([("Michael Bilirakis", "R"), ("Cheryl Davis Knapp", "D")],
        {"HILLSBOROUGH": (31755, 21070), "PASCO": (49833, 36316), "PINELLAS": (60557, 45109)},
        {"HILLSBOROUGH": (3, 1), "PASCO": (4, 3), "PINELLAS": (11, 4)},
        (142145, 102495), (18, 8), 244666),
    11: ([("Bill Tolley", "R"), ("Jim Bacchus", "D")],
         {"BREVARD": (60968, 70004), "INDIAN RIVER": (12434, 7180), "ORANGE": (27873, 31934), "OSCEOLA": (10641, 11856)},
         {"BREVARD": (31, 12), "INDIAN RIVER": (0, 0), "ORANGE": (22, 5), "OSCEOLA": (1, 0)},
         (111916, 120974), (54, 17), 232961),
    14: ([("Scott Shore", "R"), ("Harry A. Johnston", "D")],
         {"BROWARD": (19257, 39935), "PALM BEACH": (60982, 116115)}, {"BROWARD": (2, 1), "PALM BEACH": (8, 4)},
         (80239, 156050), (10, 5), 236304),
    15: ([("E. Clay Shaw, Jr.", "R"), ("Charles Goodmon", "I")],
         {"BROWARD": (104273, 2374)}, {"BROWARD": (22, 0)}, (104273, 2374), (22, 0), 106669),
    17: ([("Earl Rodney", "R"), ("William \"Bill\" Lehman", "D"), ("Charles Evans", "I")],
         {"MIAMI-DADE": (22027, 79560, 1)}, {"MIAMI-DADE": (2, 9, 0)}, (22027, 79560, 1), (2, 9, 0), 101599),
    18: ([("Ileana Ros-Lehtinen", "R"), ("Bernard \"Bern\" Anscher", "D"), ("Thabo Ntweng", "I")],
         {"MIAMI-DADE": (56354, 36967, 1)}, {"MIAMI-DADE": (10, 11, 0)}, (56354, 36967, 1), (10, 11, 0), 93343),
    19: ([("Bob Allen", "R"), ("Dante B. Fascell", "D")],
         {"MIAMI-DADE": (44314, 76465), "MONROE": (9460, 11212)}, {"MIAMI-DADE": (17, 16), "MONROE": (5, 3)},
         (53774, 87677), (22, 19), 141492),
}
# FEC 1990 candidate totals (fec1990_house.csv) for the D and R nominees, by district
FEC90 = {1: {"R": 80851, "D": 88416}, 2: {"R": 77939, "D": 103032}, 3: {"R": 31727, "D": 84280}, 4: {"R": 120895, "D": 95320},
         5: {"R": 94453, "D": 63253}, 6: {"R": 138588, "D": 95421}, 7: {"R": 47765, "D": 99464}, 9: {"R": 142163, "D": 102503},
         11: {"R": 111970, "D": 120991}, 14: {"R": 80249, "D": 156055}, 15: {"R": 104295}, 17: {"R": 22029, "D": 79569},
         18: {"R": 56364, "D": 36978}, 19: {"R": 53796, "D": 87696}}

rows90 = []
for d, (cands, main, absn, sub, absline, fec_total) in D90.items():
    n = len(cands)
    assert set(main) == set(absn), d
    for j in range(n):
        assert sum(v[j] for v in main.values()) == sub[j], ("1990 sub-total", d, j)
        assert sum(v[j] for v in absn.values()) == absline[j], ("1990 absentees", d, j)
    tot = [sub[j] + absline[j] for j in range(n)]
    assert sum(tot) == fec_total, ("1990 FEC district total", d, sum(tot), fec_total)
    for j, (nm, p) in enumerate(cands):
        if p in FEC90[d]:
            assert tot[j] == FEC90[d][p], ("1990 FEC candidate", d, nm, tot[j], FEC90[d][p])
    for c in main:
        for j, (nm, p) in enumerate(cands):
            rows90.append((1990, d, c, nm, p, main[c][j] + absn[c][j]))

# ---------------------------------------------------------------- 1994
AB = {"ALAC": "ALACHUA", "BAKE": "BAKER", "BAY": "BAY", "BRAD": "BRADFORD", "BREV": "BREVARD", "BROW": "BROWARD", "CALH": "CALHOUN",
      "CITR": "CITRUS", "CLAY": "CLAY", "COLU": "COLUMBIA", "DADE": "MIAMI-DADE", "DESO": "DESOTO", "DIXI": "DIXIE", "DUVA": "DUVAL",
      "ESCA": "ESCAMBIA", "FLAG": "FLAGLER", "FRAN": "FRANKLIN", "GADS": "GADSDEN", "GILC": "GILCHRIST", "GLAD": "GLADES", "GULF": "GULF",
      "HAMI": "HAMILTON", "HARD": "HARDEE", "HEND": "HENDRY", "HERN": "HERNANDO", "HIGH": "HIGHLANDS", "HILL": "HILLSBOROUGH",
      "HOLM": "HOLMES", "INDI": "INDIAN RIVER", "JACK": "JACKSON", "JEFF": "JEFFERSON", "LAFA": "LAFAYETTE", "LAKE": "LAKE", "LEON": "LEON",
      "LEVY": "LEVY", "LIBE": "LIBERTY", "MADI": "MADISON", "MARI": "MARION", "MART": "MARTIN", "MONR": "MONROE", "OKAL": "OKALOOSA",
      "OKEE": "OKEECHOBEE", "ORAN": "ORANGE", "OSCE": "OSCEOLA", "PALM": "PALM BEACH", "PASC": "PASCO", "PINE": "PINELLAS", "POLK": "POLK",
      "PUTN": "PUTNAM", "SANT": "SANTA ROSA", "SEMI": "SEMINOLE", "ST J": "ST. JOHNS", "ST L": "ST. LUCIE", "SUMT": "SUMTER",
      "SUWA": "SUWANNEE", "TAYL": "TAYLOR", "UNIO": "UNION", "VOLU": "VOLUSIA", "WAKU": "WAKULLA", "WALT": "WALTON", "WASH": "WASHINGTON"}
# district: (candidates, {abbrev: votes}, printed Totals); names as in the Clerk of the House 1994 statistics
D94 = {
    1: ([("Vince Whibbs, Jr.", "D"), ("Joe Scarborough", "R"), ("Ralph A. Boone, Jr.", "I"), ("Wayne Robinson", "I")],
        {"BAY": (2979, 7356, 0, 0), "ESCA": (34589, 43242, 89, 9), "HOLM": (1895, 2769, 0, 0), "OKAL": (15234, 32248, 1, 9),
         "SANT": (11848, 20197, 14, 6), "WALT": (3844, 7089, 2, 0)}, (70389, 112901, 106, 24)),
    2: ([("Douglas \"Pete\" Peterson", "D"), ("Carole Griffin", "R"), ("Prescott", "I")],
        {"BAKE": (1264, 1671, 0), "BAY": (12733, 14899, 0), "CALH": (2103, 1422, 0), "COLU": (4189, 4132, 0), "FRAN": (2650, 1231, 0),
         "GADS": (8472, 2498, 0), "GULF": (3378, 1937, 0), "HAMI": (1898, 925, 0), "JACK": (7354, 5205, 0), "JEFF": (2826, 1267, 0),
         "LAFA": (1297, 724, 0), "LEON": (49827, 24040, 7), "LIBE": (1282, 627, 0), "MADI": (2791, 1914, 0), "SUWA": (4752, 4173, 1),
         "TAYL": (3367, 2508, 0), "WAKU": (4027, 2111, 0), "WASH": (3194, 2727, 0)}, (117404, 74011, 8)),
    3: ([("Corrine Brown", "D"), ("Marc Little", "R")],
        {"ALAC": (5008, 3440), "BAKE": (406, 237), "CLAY": (562, 2210), "COLU": (1726, 2386), "DUVA": (29128, 21856), "FLAG": (262, 161),
         "LAKE": (329, 114), "LEVY": (246, 142), "MARI": (2860, 1834), "ORAN": (12380, 5903), "PUTN": (2489, 4625), "SEMI": (2692, 1963),
         "ST J": (1304, 339), "VOLU": (4453, 1685)}, (63845, 46895)),
    5: ([("Karen L. Thurman", "D"), ("Don (Big Daddy) Garlits", "R")],
        {"ALAC": (30646, 16791), "CITR": (21794, 18642), "DIXI": (2339, 1641), "GILC": (1959, 1630), "HERN": (26792, 22033),
         "LEVY": (4932, 3508), "MARI": (5046, 5224), "PASC": (26410, 20005), "SUMT": (5862, 4619)}, (125780, 94093)),
    6: ([("Cliff Stearns", "R"), ("Phil Denton", "I")],
        {"BAKE": (1012, 0), "BRAD": (4820, 34), "CLAY": (25462, 27), "DUVA": (18141, 1082), "LAKE": (46230, 61), "MARI": (42562, 110),
         "PUTN": (8618, 7), "UNIO": (1853, 11)}, (148698, 1332)),
    7: ([("Edward D. Goddard", "D"), ("John L. Mica", "R")],
        {"ORAN": (3572, 8885), "SEMI": (18903, 62711), "VOLU": (25272, 60115)}, (47747, 131711)),
    8: ([("Bill McCollum", "R"), ("Ron Bedell", "I")], {"ORAN": (120808, 431), "OSCE": (10568, 8)}, (131376, 439)),
    9: ([("Michael Bilirakis", "R"), ("Richard Grayson", "I")],
        {"HILL": (37810, 48), "PASC": (43820, 27), "PINE": (95623, 77)}, (177253, 152)),
    11: ([("Sam Gibbons", "D"), ("Mark Sharpe", "R"), ("David Weeks", "I")], {"HILL": (76814, 72119, 9)}, (76814, 72119, 9)),
    12: ([("Robert Connors", "D"), ("Charles T. Canady", "R")],
         {"DESO": (2293, 3753), "HARD": (1627, 3532), "HIGH": (1778, 2833), "HILL": (7506, 18603), "PASC": (2233, 3589),
          "POLK": (41766, 73813)}, (57203, 106123)),
    15: ([("Sue Munsey", "D"), ("Dave Weldon", "R"), ("Jim Owen", "I"), ("Rick Smith", "I")],
         {"BREV": (74544, 82528, 223, 83), "INDI": (14756, 20916, 21, 0), "OSCE": (8313, 9883, 2, 3), "POLK": (2900, 3700, 0, 0)},
         (100513, 117027, 246, 86)),
    16: ([("John Comerford", "D"), ("Mark Adam Foley", "R")],
         {"GLAD": (1008, 1636), "HEND": (1709, 3390), "HIGH": (7561, 12933), "MART": (14764, 26752), "OKEE": (1713, 2758),
          "PALM": (43219, 47140), "ST L": (18672, 28125)}, (88646, 122734)),
    17: ([("Carrie P. Meek", "D"), ("Maureen Coletta", "I")], {"DADE": (75741, 11)}, (75741, 11)),
    19: ([("Harry Johnston", "D"), ("Peter J. Tsakanikas", "R")], {"BROW": (64352, 33895), "PALM": (83239, 41884)}, (147591, 75779)),
    20: ([("Peter Deutsch", "D"), ("Beverly \"Bev\" Kennedy", "R")],
         {"BROW": (92283, 54490), "DADE": (9077, 8239), "MONR": (13255, 9787)}, (114615, 72516)),
    21: ([("Lincoln Diaz-Balart", "R"), ("Laura Garza", "I"), ("Tobias", "I")], {"DADE": (90948, 1, 1)}, (90948, 1, 1)),
    22: ([("Hermine L. Wiener", "D"), ("E. Clay Shaw, Jr.", "R")],
         {"BROW": (26532, 62071), "DADE": (26456, 20373), "PALM": (16227, 37246)}, (69215, 119690)),
}
# Clerk of the House, Statistics of the Congressional Election of November 8, 1994 (Florida): D, R, write-in by district
CLERK94 = {1: (70416, 112974, 130), 2: (117420, 74040, 8), 3: (63855, 46907, 0), 5: (125792, 94097, 0), 6: (0, 148741, 1332),
           7: (47758, 131731, 0), 8: (0, 131393, 439), 9: (0, 177265, 152), 11: (76821, 72129, 9), 12: (57210, 106140, 0),
           15: (100526, 117044, 332), 16: (88653, 122760, 0), 17: (75756, 0, 11), 19: (147595, 75788, 0), 20: (114623, 72525, 0),
           21: (0, 90951, 2), 22: (69221, 119696, 0)}

rows94, diffs = [], []
for d, (cands, cty, printed) in D94.items():
    n = len(cands)
    for j in range(n):
        assert sum(v[j] for v in cty.values()) == printed[j], ("1994 Totals", d, j)
    ours = {"D": 0, "R": 0, "I": 0}
    for j, (nm, p) in enumerate(cands):
        ours[p] += printed[j]
    for k, key in enumerate(("D", "R", "I")):
        cl = CLERK94[d][k]
        diff = cl - ours[key]
        assert 0 <= diff <= 100 and diff <= 0.002 * max(cl, 1) + 1, ("1994 Clerk", d, key, ours[key], cl)
        if diff:
            diffs.append((d, key, ours[key], cl, diff))
    for a, v in cty.items():
        for j, (nm, p) in enumerate(cands):
            rows94.append((1994, d, AB[a], nm, p, v[j]))

cp = {r["county_name"].upper() for r in csv.DictReader(open(os.path.join(HERE, "../data/raw_election/countypres_2000-2024.tab")), delimiter="\t") if r["state"] == "FLORIDA"}
for yr, rows in ((1990, rows90), (1994, rows94)):
    assert {r[2] for r in rows} <= cp, {r[2] for r in rows} - cp
    with open(os.path.join(OUT, f"florida_house_county_{yr}.csv" if yr == 1990 else "florida_house_county_1994_verification.csv"), "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["year", "district", "county", "candidate", "party_code", "votes"])
        w.writerows(rows)
    print(yr, len(rows), "rows,", len({r[2] for r in rows}), "counties,", len({r[1] for r in rows}), "districts")
print("1994 Clerk minus report totals (late absentees):", diffs)
