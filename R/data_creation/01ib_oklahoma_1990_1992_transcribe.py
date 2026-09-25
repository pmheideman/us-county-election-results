"""Oklahoma U.S. House, county level, 1990 and 1992 general elections, hand-transcribed.

Source: Oklahoma State Election Board, 'State of Oklahoma Election Results and
Statistics' (annual book), as filmed for the Statistical Reference Index
microfiche and scanned by the Internet Archive:
  1990: micro_IA40706939_0247, 'OFFICIAL RETURNS GENERAL ELECTION, NOVEMBER 6, 1990,
        UNITED STATES REPRESENTATIVE', printed pp. 108-110 (scan pages 115-117)
  1992: micro_IA40706946_0152, 'OFFICIAL RETURNS GENERAL ELECTION, NOVEMBER 3, 1992',
        printed pp. 59-61 (scan pages 64-66)
Page images: R/data/county_house_files/oklahoma_microfiche/ok<year>_<page>.png
(extracted from the items' jp2 zips). Clean typewriter print, read by eye.
The primary and runoff tables printed nearby were not used. All six districts
had a general-election contest in both years.

Checks (the script stops unless all pass):
  * each candidate column adds up to the printed TOTAL row;
  * the printed TOTALs add up to the printed TOTAL VOTE;
  * every candidate total equals the FEC's official result
    (R/data/fec_official/fec1990_house.csv, fec1992_house.csv: complete for OK);
  * all 77 counties appear each year ('*' = county divided between districts).

Output: R/data/county_house_files/oklahoma_microfiche/oklahoma_house_county_1990_1992.csv
(year, district, county, candidate, party_code, votes).
"""
import csv, os, re

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.join(HERE, "..")
OUT = os.path.join(ROOT, "data/county_house_files/oklahoma_microfiche/oklahoma_house_county_1990_1992.csv")

# year -> district -> (candidates [(name, party)], county rows {county: [votes...]}, TOTAL row, TOTAL VOTE)
DATA = {
 1990: {
  1: ([("Kurt G. Glassco", "D"), ("James M. Inhofe", "R")],
      {"Creek": [3022, 3559], "Osage": [4575, 4182], "Tulsa": [51314, 67145], "Washington": [610, 732]},
      [59521, 75618], 135139),
  2: ([("Mike Synar", "D"), ("Terry M. Gorham", "R")],
      {"Adair": [2339, 1380], "Cherokee": [5925, 3461], "Craig": [2727, 1467], "Creek": [4624, 2575],
       "Delaware": [3976, 2998], "Haskell": [2869, 965], "McIntosh": [4025, 1682], "Mayes": [6250, 3548],
       "Muskogee": [10834, 6110], "Nowata": [1896, 1215], "Okfuskee": [2047, 937], "Okmulgee": [6879, 2911],
       "Ottawa": [5127, 2756], "Pawnee": [2596, 1779], "Rogers": [8743, 7098], "Sequoyah": [6212, 2381],
       "Tulsa": [6814, 9102], "Wagoner": [6937, 4966]},
      [90820, 57331], 148151),
  3: ([("Bill Brewster", "D"), ("Patrick K. Miller", "R")],
      {"Atoka": [2974, 433], "Bryan": [6434, 916], "Carter": [9720, 2302], "Choctaw": [4028, 535],
       "Coal": [1610, 322], "Hughes": [2959, 652], "Johnston": [2271, 336], "Latimer": [2262, 389],
       "LeFlore": [7995, 1711], "Lincoln": [5387, 2404], "Love": [2055, 293], "McCurtain": [5532, 938],
       "Marshall": [3254, 537], "Murray": [3292, 565], "Payne": [11442, 4844], "Pittsburg": [9220, 2106],
       "Pontotoc": [8081, 1531], "Pottawatomie": [10515, 3619], "Pushmataha": [3018, 499], "Seminole": [5592, 1329]},
      [107641, 26261], 133902),
  4: ([("Dave McCurdy", "D"), ("Howard Bell", "R")],
      {"Cleveland": [31956, 13429], "Comanche": [14754, 6085], "Cotton": [1737, 368], "Garvin": [6125, 1353],
       "Grady": [8234, 2776], "Jackson": [4866, 1130], "Jefferson": [1648, 370], "McClain": [5097, 1607],
       "Oklahoma": [13077, 5707], "Pottawatomie": [764, 240], "Stephens": [10207, 2772], "Tillman": [2414, 395]},
      [100879, 36232], 137111),
  5: ([("Bryce Baggett", "D"), ("Mickey Edwards", "R")],
      {"Canadian": [2775, 7450], "Kay": [5339, 8692], "Logan": [2969, 5781], "Noble": [1159, 2651],
       "Oklahoma": [32217, 79642], "Osage": [881, 1031], "Washington": [4746, 9361]},
      [50086, 114608], 164694),
  6: ([("Glenn English", "D"), ("Robert Burns", "R")],
      {"Alfalfa": [2120, 412], "Beaver": [1541, 702], "Beckham": [4625, 720], "Blaine": [3358, 775],
       "Caddo": [6527, 1137], "Canadian": [8012, 2737], "Cimarron": [944, 379], "Custer": [6922, 1217],
       "Dewey": [1988, 298], "Ellis": [1557, 324], "Garfield": [13728, 3724], "Grant": [2083, 430],
       "Greer": [1902, 246], "Harmon": [1176, 98], "Harper": [1333, 394], "Kingfisher": [3663, 955],
       "Kiowa": [3185, 430], "Major": [2351, 729], "Oklahoma": [26572, 7903], "Roger Mills": [1592, 237],
       "Texas": [3172, 1416], "Washita": [3601, 495], "Woods": [3132, 666], "Woodward": [5016, 1116]},
      [110100, 27540], 137640),
 },
 1992: {
  1: ([("John Selph", "D"), ("James M. Inhofe", "R")],
      {"Tulsa": [102833, 114260], "Wagoner": [3786, 4951]},
      [106619, 119211], 225830),
  2: ([("Mike Synar", "D"), ("Jerry Hill", "R"), ("William S. Vardeman", "I")],
      {"Adair": [3410, 2496, 152], "Cherokee": [8090, 5705, 572], "Craig": [3451, 2217, 195],
       "Creek": [13416, 9345, 837], "Delaware": [5817, 5246, 475], "Haskell": [3713, 1358, 157],
       "McIntosh": [4636, 2571, 274], "Mayes": [7914, 5826, 441], "Muskogee": [14109, 11939, 672],
       "Nowata": [2461, 1624, 147], "Okfuskee": [2752, 1334, 156], "Okmulgee": [9279, 4639, 526],
       "Osage": [7670, 4389, 419], "Ottawa": [6700, 4857, 495], "Pawnee": [348, 347, 32],
       "Rogers": [11702, 13569, 986], "Sequoyah": [7138, 4942, 370], "Wagoner": [5936, 5253, 408]},
      [118542, 87657, 7314], 213513),
  3: ([("Bill K. Brewster", "D"), ("Robert W. Stokes", "R")],
      {"Atoka": [3972, 782], "Bryan": [10074, 2056], "Carter": [13180, 3832], "Choctaw": [4910, 814],
       "Coal": [2048, 432], "Hughes": [4071, 888], "Johnston": [3450, 582], "Latimer": [3641, 792],
       "LeFlore": [11139, 3851], "Lincoln": [7592, 3619], "Love": [2845, 585], "McCurtain": [8111, 2310],
       "Marshall": [4074, 1007], "Murray": [4255, 854], "Pawnee": [3654, 1914], "Payne": [17129, 11038],
       "Pittsburg": [13757, 3485], "Pontotoc": [11481, 2745], "Pottawatomie": [15647, 7445],
       "Pushmataha": [3758, 631], "Seminole": [7146, 2063]},
      [155934, 51725], 207659),
  4: ([("Dave McCurdy", "D"), ("Howard Bell", "R")],
      {"Cleveland": [49106, 24340], "Comanche": [23482, 9202], "Cotton": [2210, 587], "Garvin": [8405, 2250],
       "Grady": [11479, 4560], "Jackson": [6480, 2181], "Jefferson": [2187, 494], "McClain": [7337, 2526],
       "Oklahoma": [12765, 6577], "Stephens": [14381, 4733], "Tillman": [3009, 785]},
      [140841, 58235], 199076),
  5: ([("Laurie Williams", "D"), ("Ernest Jim Istook", "R")],
      {"Canadian": [7698, 9616], "Kay": [11596, 9541], "Logan": [6937, 5919], "Noble": [2756, 2192],
       "Oklahoma": [67422, 82501], "Osage": [2115, 1366], "Washington": [9055, 12102]},
      [107579, 123237], 230816),
  6: ([("Glenn English", "D"), ("Bob Anthony", "R")],
      {"Alfalfa": [1943, 939], "Beaver": [1504, 1138], "Beckham": [5567, 1829], "Blaine": [3460, 1279],
       "Caddo": [8019, 2636], "Canadian": [8517, 4856], "Cimarron": [900, 596], "Custer": [7742, 3391],
       "Dewey": [1971, 654], "Ellis": [1490, 686], "Garfield": [15811, 8089], "Grant": [2068, 799],
       "Greer": [1980, 556], "Harmon": [1173, 265], "Harper": [1211, 667], "Kingfisher": [4032, 1976],
       "Kiowa": [3472, 1041], "Major": [2218, 1325], "Oklahoma": [44518, 22381], "Roger Mills": [1540, 498],
       "Texas": [3694, 2727], "Washita": [3914, 1137], "Woods": [2718, 1729], "Woodward": [5272, 2874]},
      [134734, 64068], 198802),
 },
}

key = lambda s: re.sub(r"[^A-Z]", "", s.upper())
xw = {}
for r in csv.DictReader(open(os.path.join(ROOT, "data/raw_election/countypres_2000-2024.tab")), delimiter="\t"):
    if r["state"] == "OKLAHOMA":
        xw[key(r["county_name"])] = r["county_name"]
assert len(xw) == 77

fec = {}
for yr in DATA:
    for r in csv.DictReader(open(os.path.join(ROOT, f"data/fec_official/fec{yr}_house.csv"))):
        if r["state_po"] == "OK" and r["is_total"] == "False":
            fec[(yr, int(r["district"]), key(r["candidate"].split()[-1]))] = int(float(r["votes"]))

rows = []
for yr, dists in DATA.items():
    seen = set()
    for d, (cands, counties, total, total_vote) in dists.items():
        n = len(cands)
        assert all(len(v) == n for v in counties.values()), (yr, d)
        for j in range(n):
            assert sum(v[j] for v in counties.values()) == total[j], (yr, d, cands[j], sum(v[j] for v in counties.values()), total[j])
        assert sum(total) == total_vote, (yr, d, sum(total), total_vote)
        for j, (name, party) in enumerate(cands):
            assert fec[(yr, d, key(name.split()[-1]))] == total[j], (yr, d, name, fec.get((yr, d, key(name.split()[-1]))), total[j])
        for c, v in counties.items():
            cn = xw[key(c)]
            seen.add(cn)
            for j, (name, party) in enumerate(cands):
                rows.append(dict(year=yr, district=d, county=cn, candidate=name, party_code=party, votes=v[j]))
    assert len(seen) == 77, (yr, len(seen), sorted(set(xw.values()) - seen))

with open(OUT, "w", newline="") as f:
    w = csv.DictWriter(f, fieldnames=["year", "district", "county", "candidate", "party_code", "votes"])
    w.writeheader()
    w.writerows(rows)
print(f"{len(rows)} candidate-county rows; 1990 and 1992: 6 districts each, all columns tie to TOTAL and TOTAL VOTE, all candidate totals equal the FEC, 77 counties each year")
