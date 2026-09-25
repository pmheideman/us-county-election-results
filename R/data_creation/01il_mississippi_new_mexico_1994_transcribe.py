"""Mississippi and New Mexico U.S. House 1994 general election, county level, hand-transcribed
from Internet Archive Statistical Reference Index (SRI) microfiche.

Mississippi: micro_IA40706953_0014, 'Mississippi Summary for General Election: Nov. 8, 1994'
  (Secretary of State Dick Molpus's certification; 'Elections Spreadsheet Summary' pages 4-8,
  scan pages 5-9, one page per congressional district: county, (D), (R), (T) votes with
  percentages, row TOTALS column and a TOTALS row). Local copy R/data/county_house_files/sri/ms1994/.
New Mexico: micro_IA40706953_0112, 'New Mexico Official Returns: 1994 General Election'
  (State Canvassing Board, 'Canvass of Returns of General Election Held on November 8, 1994',
  page 1 of 8, scan page 55: handwritten county columns, one row per candidate, and a
  'Total for each candidate' column). Local copy R/data/county_house_files/sri/nm1994/.

Read by eye from the page images, digit by digit. Checks (the script stops on any failure):
  Mississippi: every county row's candidate votes equal its printed TOTALS; every column equals
  the printed TOTALS row; every candidate total equals the Clerk of the House 'Statistics of the
  Congressional Election of November 8, 1994'.
  New Mexico: every candidate's county values sum to the printed 'Total for each candidate';
  every candidate total equals the Clerk of the House 1994 statistics.
County names are matched to R/data/raw_election/countypres_2000-2024.tab (all 82 / 33 counties).
No district was unopposed in either state. Write-ins were not printed.

Output: R/data/county_house_files/sri/mississippi_house_county_1994.csv and
        R/data/county_house_files/sri/new_mexico_house_county_1994.csv
"""
import csv, os

HERE = os.path.dirname(os.path.abspath(__file__))
OUT = os.path.join(HERE, "../data/county_house_files/sri")
XW = os.path.join(HERE, "../data/raw_election/countypres_2000-2024.tab")

# ---- Mississippi: {district: (candidates [(name, party)], rows [(county, votes..., printed row total)], printed column totals)}
MS = {
    1: ([("William R. Wheeler, Jr.", "D"), ("Roger F. Wicker", "R")], """
Alcorn 2436 4174 6610
Benton 1337 1263 2600
Calhoun 1542 2470 4012
Chickasaw 1891 2385 4276
Choctaw 943 1744 2687
Desoto 4438 12903 17341
Grenada 61 257 318
Itawamba 2781 2945 5726
LaFayette 2834 4266 7100
Lee 4433 11216 15649
Marshall 3550 2918 6468
Monroe 2940 4567 7507
Montgomery 272 550 822
Oktibbeha 233 406 639
Panola 1350 2922 4272
Pontotoc 1600 4074 5674
Prentiss 2213 3614 5827
Tallahatchie 1190 1247 2437
Tate 1751 2999 4750
Tippah 2113 3375 5488
Tishomingo 2939 2362 5301
Union 1760 4155 5915
Webster 1004 2162 3166
Yalobusha 1581 1579 3160""", [47192, 80553]),
    2: ([("Bennie Thompson", "D"), ("Bill Jordan", "R"), ("Vince Thornton", "I")], """
Attala 800 598 119 1517
Bolivar 5604 3828 753 10185
Carroll 1018 1690 414 3122
Claiborne 2136 783 156 3075
Coahoma 3502 2082 672 6256
Grenada 2458 2492 671 5621
Hinds 10761 1823 240 12824
Holmes 3393 1657 184 5234
Humphreys 1810 1185 187 3182
Issaquena 395 336 62 793
Jefferson 1939 405 186 2530
Leake 949 348 67 1364
Leflore 4375 4306 478 9159
Madison 4198 2750 295 7243
Montgomery 1129 898 289 2316
Panola 1945 874 83 2902
Quitman 1471 1100 121 2692
Sharkey 1020 898 106 2024
Sunflower 3393 2792 616 6801
Tallahatchie 1247 559 165 1971
Tunica 954 535 101 1590
Warren 4604 7941 1497 14042
Washington 6027 5849 1396 13272
Yazoo 2886 3541 550 6977""", [68014, 49270, 9408]),
    3: ([("G. V. Montgomery", "D"), ("Dutch Dabbs", "R")], """
Attala 2174 955 3129
Clarke 3639 1507 5146
Clay 3403 1454 4857
Jasper 2637 912 3549
Jones 3190 1672 4862
Kemper 2235 564 2799
Lauderdale 13128 3696 16824
Leake 3064 1103 4167
Lowndes 6287 3494 9781
Madison 3862 4021 7883
Neshoba 3758 1642 5400
Newton 3682 872 4554
Noxubee 1846 561 2407
Oktibbeha 4404 2321 6725
Rankin 14408 10952 25360
Scott 4267 1086 5353
Smith 3334 1074 4408
Wayne 551 208 759
Winston 3294 1732 5026""", [83163, 39826]),
    4: ([("Mike Parker", "D"), ("Mike Wood", "R")], """
Adams 6568 2568 9136
Amite 3674 1080 4754
Copiah 5870 1891 7761
Covington 3131 1481 4612
Franklin 2306 545 2851
Hinds 23782 15475 39257
Jefferson_Davis 2580 753 3333
Jones 5977 4417 10394
Lawrence 2658 860 3518
Lincoln 6113 1656 7769
Marion 4255 1844 6099
Pike 7163 2292 9455
Simpson 3557 1874 5431
Walthall 2269 902 3171
Wilkinson 3036 562 3598""", [82939, 38200]),
    5: ([("Gene Taylor", "D"), ("George Barlos", "R")], """
Forrest 8879 6231 15110
George 2735 1692 4427
Greene 1422 1037 2459
Hancock 5556 2833 8389
Harrison 22427 12354 34781
Jackson 16755 12563 29318
Lamar 4943 4428 9371
Pearl_River 3948 3707 7655
Perry 1677 1065 2742
Stone 2119 1071 3190
Wayne 2718 1594 4312""", [73179, 48575]),
}
MS_CLERK = {1: [47192, 80553], 2: [68014, 49270, 9408], 3: [83163, 39826], 4: [82939, 38200], 5: [73179, 48575]}

# ---- New Mexico: {district: (candidates, {county: [votes per candidate]}, printed candidate totals)}
NM = {
    1: ([("Peter L. Zollinger", "D"), ("Steven H. Schiff", "R")],
        {"Bernalillo": [38773, 109979], "Sandoval": [1440, 3169], "Santa Fe": [90, 475], "Torrance": [896, 3106], "Valencia": [1117, 3267]},
        [42316, 119996]),
    2: ([("Benjamin Anthony Chavez", "D"), ("Joe Skeen", "R"), ("Rex R. Johnson", "I")],
        {"Bernalillo": [200, 189, 28], "Catron": [318, 1066, 96], "Chaves": [4100, 11117, 569], "Cibola": [2230, 2733, 239],
         "De Baca": [313, 694, 38], "Dona Ana": [11040, 19423, 2007], "Eddy": [4795, 10016, 506], "Grant": [3963, 4734, 565],
         "Guadalupe": [1206, 900, 68], "Hidalgo": [693, 1142, 70], "Lea": [2686, 9583, 609], "Lincoln": [1092, 3940, 216],
         "Luna": [1810, 3261, 307], "Otero": [3126, 9499, 537], "Sierra": [1015, 2630, 243], "Socorro": [2373, 3018, 307],
         "Valencia": [4356, 6021, 493]},
        [45316, 89966, 6898]),
    3: ([("Bill Richardson", "D"), ("F. Gregg Bemis, Jr.", "R"), ("Ed D. Nagel", "I")],
        {"Bernalillo": [1260, 1400, 69], "Cibola": [321, 115, 12], "Colfax": [3079, 1537, 72], "Curry": [4981, 5169, 160],
         "Harding": [385, 251, 4], "Los Alamos": [4808, 3815, 366], "McKinley": [9860, 3112, 201], "Mora": [1841, 483, 48],
         "Quay": [2267, 1396, 51], "Rio Arriba": [7744, 1813, 175], "Roosevelt": [2382, 2345, 84], "Sandoval": [9234, 6316, 346],
         "San Juan": [12270, 13560, 613], "San Miguel": [6661, 1404, 171], "Santa Fe": [25756, 8528, 1088],
         "Taos": [6184, 1531, 205], "Union": [867, 740, 32]},
        [99900, 53515, 3697]),
}
NM_CLERK = {1: [42316, 119996], 2: [45316, 89966, 6898], 3: [99900, 53515, 3697]}


def names(state):
    out = {}
    for r in csv.DictReader(open(XW), delimiter="\t"):
        if r["state"] == state and r["county_fips"] not in ("", "NA"):
            out["".join(ch for ch in r["county_name"].upper() if ch.isalpha())] = r["county_name"].upper()
    return out


def key(c):
    return "".join(ch for ch in c.upper() if ch.isalpha())


rows = []
ms_names = names("MISSISSIPPI")
for d, (cands, block, tot) in MS.items():
    n = len(cands)
    cols = [0] * n
    for line in block.strip().splitlines():
        parts = line.split()
        cty, v = parts[0].replace("_", " "), [int(x) for x in parts[1:]]
        assert len(v) == n + 1 and sum(v[:n]) == v[n], ("MS row", d, cty, v)
        assert key(cty) in ms_names, ("MS county", cty)
        for j in range(n):
            cols[j] += v[j]
            rows.append(("mississippi", 1994, d, ms_names[key(cty)], cands[j][0], cands[j][1], v[j]))
    assert cols == tot, ("MS column", d, cols, tot)
    assert tot == MS_CLERK[d], ("MS Clerk", d, tot, MS_CLERK[d])

nm_names = names("NEW MEXICO")
for d, (cands, block, tot) in NM.items():
    cols = [sum(v[j] for v in block.values()) for j in range(len(cands))]
    assert cols == tot, ("NM column", d, cols, tot)
    assert tot == NM_CLERK[d], ("NM Clerk", d, tot, NM_CLERK[d])
    for cty, v in block.items():
        assert key(cty) in nm_names, ("NM county", cty)
        for j, (nm, p) in enumerate(cands):
            rows.append(("new_mexico", 1994, d, nm_names[key(cty)], nm, p, v[j]))

for st, names_ in (("mississippi", ms_names), ("new_mexico", nm_names)):
    rs = [r for r in rows if r[0] == st]
    have = {r[3] for r in rs}
    assert have == set(names_.values()), (st, sorted(set(names_.values()) - have))
    with open(os.path.join(OUT, f"{st}_house_county_1994.csv"), "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["year", "district", "county", "candidate", "party_code", "votes"])
        for r in rs:
            w.writerow(r[1:])
    print(f"{st} 1994: {len(rs)} candidate-county rows, {len(have)} counties; rows, columns and Clerk totals tie")
