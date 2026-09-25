"""Ohio U.S. House 1990 general election, county level, hand-transcribed.

Source: Ohio Secretary of State, 'Ohio Election Statistics for 1989-1990', table
'District Vote for Representatives to Congress and Pluralities, General Election,
November 6, 1990', printed pp. 100-103 (Internet Archive SRI microfiche item
micro_IA40706942_0388, scan pages 103-106; local copies in
R/data/county_house_files/sri/oh1990/). Read digit by digit from the page images.

Checks (the script stops on any failure):
  * every candidate's county rows add up to the printed district Totals;
  * the printed Plurality equals winner minus runner-up;
  * every candidate total equals the FEC's 'Federal Elections 90' (parsed below from
    R/data/fec_official/federalelections90.pdf; its OCR prints some commas as '1',
    e.g. '411693' = 41,693, repaired by rule) -- except District 7, where the FEC book
    has Hobson 97,123 / Schira 59,349 against this book's 97,020 / 59,253 (the book's
    county rows add up to its own totals; kept as printed, see FEC_EXCEPTIONS);
  * all 88 counties appear.
One-county districts print only a Totals line (Hamilton, Montgomery, Summit, Cuyahoga
parts). Write-in candidates (D11 Mononen, D15 Buckel, D21 Ware) are excluded from the CSV.
District 3 (Tony P. Hall) was unopposed but on the ballot.

Output: R/data/county_house_files/sri/ohio_house_county_1990.csv
"""
import csv, os, re, subprocess

HERE = os.path.dirname(os.path.abspath(__file__))
OUT = os.path.join(HERE, "../data/county_house_files/sri/ohio_house_county_1990.csv")
FECPDF = os.path.join(HERE, "../data/fec_official/federalelections90.pdf")

# district: (candidates [(name, party)], {county: [votes per candidate]}, printed totals, printed plurality)
D = {
 1: ([("Charles Luken", "D"), ("J. Kenneth Blackwell", "R")], {"HAMILTON": [83932, 80362]}, [83932, 80362], 3570),
 2: ([("Tyrone K. Yates", "D"), ("Willis D. Gradison, Jr.", "R")],
     {"BROWN": [4245, 5461], "CLERMONT": [11673, 24616], "HAMILTON": [41427, 73740]}, [57345, 103817], 46472),
 3: ([("Tony P. Hall", "D")], {"MONTGOMERY": [116797]}, [116797], None),
 4: ([("Thomas E. Burkhart", "D"), ("Michael G. Oxley", "R")],
     {"ALLEN": [11666, 21686], "AUGLAIZE": [4765, 9876], "CRAWFORD": [5568, 10027], "HANCOCK": [8486, 13132],
      "HARDIN": [3487, 6385], "KNOX": [6965, 8620], "RICHLAND": [15565, 19908], "SHELBY": [5239, 9258],
      "WYANDOT": [2726, 5005]}, [64467, 103897], 39430),
 5: ([("P. Scott Mange", "D"), ("Paul E. Gillmor", "R"), ("John E. Jackson", "I")],
     {"DEFIANCE": [3717, 8121, 1001], "ERIE": [7825, 14838, 1164], "FULTON": [898, 4477, 282],
      "HENRY": [2078, 7011, 703], "HURON": [603, 1435, 115], "LORAIN": [2, 0, 3], "OTTAWA": [3576, 9761, 551],
      "PAULDING": [2140, 4269, 500], "PUTNAM": [2936, 8806, 689], "SANDUSKY": [4853, 13436, 1528],
      "SENECA": [4221, 13509, 1111], "WILLIAMS": [2579, 7719, 956], "WOOD": [6265, 20233, 2009]},
     [41693, 113615, 10612], 71922),
 6: ([("Ray Mitchell", "D"), ("Bob McEwen", "R")],
     {"ADAMS": [2396, 6122], "ATHENS": [1038, 1162], "BROWN": [213, 398], "CLERMONT": [349, 700],
      "CLINTON": [2242, 7913], "FAYETTE": [714, 2432], "HIGHLAND": [3015, 8537], "HOCKING": [2955, 4747],
      "JACKSON": [2828, 6633], "MONTGOMERY": [5150, 18076], "PIKE": [3066, 5080], "ROSS": [5310, 13989],
      "SCIOTO": [8415, 15972], "VINTON": [1524, 2377], "WARREN": [8200, 23082]}, [47415, 117220], 69805),
 7: ([("Jack Schira", "D"), ("David L. Hobson", "R")],
     {"CHAMPAIGN": [3832, 6404], "CLARK": [15723, 27506], "FAYETTE": [1061, 2446], "FRANKLIN": [103, 95],
      "GREENE": [14716, 28522], "LOGAN": [4419, 8107], "MADISON": [574, 968], "MARION": [9780, 10202],
      "PICKAWAY": [5358, 6440], "UNION": [3531, 6106], "WYANDOT": [156, 224]}, [59253, 97020], 37767),
 8: ([("Gregory V. Jolivette", "D"), ("John A. Boehner", "R")],
     {"AUGLAIZE": [43, 157], "BUTLER": [33761, 46088], "CHAMPAIGN": [220, 461], "DARKE": [6435, 11262],
      "HAMILTON": [20, 51], "MERCER": [5667, 7826], "MIAMI": [9494, 19703], "PREBLE": [4503, 8056],
      "VAN WERT": [3441, 6351]}, [63584, 99955], 36371),
 9: ([("Marcy Kaptur", "D"), ("Jerry D. Lammers", "R")],
     {"FULTON": [4590, 1853], "LUCAS": [106640, 30481], "WOOD": [6451, 1457]}, [117681, 33791], 83890),
 10: ([("John M. Buchanan", "D"), ("Clarence E. Miller", "R")],
      {"ATHENS": [5909, 7575], "FAIRFIELD": [12293, 22442], "GALLIA": [3756, 7266], "GUERNSEY": [373, 686],
       "LAWRENCE": [7599, 11001], "LICKING": [10173, 13327], "MEIGS": [2632, 5254], "MORGAN": [1607, 3885],
       "MUSKINGUM": [7973, 17609], "NOBLE": [71, 80], "PERRY": [3646, 6621], "WASHINGTON": [5624, 10263]},
      [61656, 106009], 44353),
 11: ([("Dennis E. Eckart", "D"), ("Margaret R. Mueller", "R")],
      {"ASHTABULA": [22079, 10441], "GEAUGA": [15891, 11656], "LAKE": [40643, 18944], "PORTAGE": [28118, 14667],
       "TRUMBULL": [5192, 2664]}, [111923, 58372], 53551),
 12: ([("Mike Gelpi", "D"), ("John R. Kasich", "R")],
      {"DELAWARE": [5176, 20407], "FRANKLIN": [39108, 87716], "LICKING": [4167, 15566], "MORROW": [2333, 6806]},
      [50784, 130495], 79711),
 13: ([("Don J. Pease", "D"), ("William D. Nielson", "R"), ("John Michael Ryan", "I")],
      {"ASHLAND": [7865, 6623, 759], "HURON": [8658, 5508, 944], "LORAIN": [46900, 31822, 5777],
       "MEDINA": [23078, 14131, 2266], "RICHLAND": [4670, 1583, 495], "SUMMIT": [2260, 1258, 265]},
      [93431, 60925, 10506], 32506),
 14: ([("Thomas C. Sawyer", "D"), ("Jean E. Bender", "R")], {"SUMMIT": [97875, 66460]}, [97875, 66460], 31415),
 15: ([("Thomas V. Erney", "D"), ("Chalmers P. Wylie", "R")],
      {"FRANKLIN": [65802, 93542], "MADISON": [2708, 5709]}, [68510, 99251], 30741),
 16: ([("Warner D. Mendenhall", "D"), ("Ralph Regula", "R")],
      {"CARROLL": [930, 1424], "HOLMES": [1952, 4213], "STARK": [55367, 77191], "SUMMIT": [181, 169],
       "WAYNE": [12086, 18100]}, [70516, 101097], 30581),
 17: ([("James A. Traficant, Jr.", "D"), ("Robert R. DeJulio, Jr.", "R")],
      {"COLUMBIANA": [1697, 816], "MAHONING": [75860, 21750], "TRUMBULL": [55650, 15633]}, [133207, 38199], 95008),
 18: ([("Douglas Applegate", "D"), ("John A. Hales", "R")],
      {"BELMONT": [19879, 5411], "CARROLL": [4503, 2102], "COLUMBIANA": [23272, 9933], "COSHOCTON": [7028, 3889],
       "GUERNSEY": [7965, 3104], "HARRISON": [4853, 1526], "JEFFERSON": [24339, 5764], "MONROE": [5193, 1014],
       "NOBLE": [3317, 1390], "TUSCARAWAS": [18877, 6826], "WASHINGTON": [1556, 864]}, [120782, 41823], 78959),
 19: ([("Edward F. Feighan", "D"), ("Susan M. Lawko", "R")],
      {"CUYAHOGA": [123644, 68191], "LAKE": [8211, 3359], "LORAIN": [1096, 765]}, [132951, 72315], 60636),
 20: ([("Mary Rose Oakar", "D"), ("Bill Smith", "R")], {"CUYAHOGA": [109390, 39749]}, [109390, 39749], 69641),
 21: ([("Louis Stokes", "D"), ("Franklin H. Roski", "R")], {"CUYAHOGA": [103338, 25906]}, [103338, 25906], 77432),
}
# the FEC book differs from this book in District 7 only (see docstring)
FEC_EXCEPTIONS = {(7, "David L. Hobson"): 97123, (7, "Jack Schira"): 59349}


def fec_ohio():
    """{district: sorted candidate totals} from the FEC 1990 book's Ohio House section."""
    t = subprocess.run(["pdftotext", "-layout", FECPDF, "-"], capture_output=True, text=True).stdout.splitlines()
    i = next(k for k, l in enumerate(t) if re.match(r"^\s*1 -\s+Luken\s+Thomas", l))
    out, d = {}, None
    for l in t[i:]:
        if re.match(r"^\s*1 -\s+Inhofe", l):  # Oklahoma starts
            break
        m = re.match(r"^\s*(\d+)\s*-\s+", l)
        if m:
            d = int(m.group(1))
        m = re.search(r"(Democratic|Republican|Independent)\s+([\d,]+)\s+[\d.]", l)
        if m and d:
            s = m.group(2)
            if "," not in s and len(s) >= 6 and s[-4] == "1":  # OCR comma read as '1'
                s = s[:-4] + s[-3:]
            out.setdefault(d, []).append(int(s.replace(",", "")))
    return out


fec = fec_ohio()
assert sorted(fec) == list(range(1, 22)), sorted(fec)
rows = []
for d, (cands, cty, tot, plur) in D.items():
    sums = [sum(v[j] for v in cty.values()) for j in range(len(cands))]
    assert all(len(v) == len(cands) for v in cty.values()), d
    assert sums == tot, ("county rows vs printed Totals", d, sums, tot)
    if plur is not None:
        s = sorted(tot, reverse=True)
        assert s[0] - s[1] == plur, ("plurality", d)
    ours = sorted(FEC_EXCEPTIONS.get((d, c[0]), t) for c, t in zip(cands, tot))
    assert ours == sorted(fec[d]), ("FEC", d, ours, sorted(fec[d]))
    for c, v in cty.items():
        for (nm, p), x in zip(cands, v):
            rows.append((1990, d, c, nm, p, x))

counties = {r[2] for r in rows}
xw = set()
with open(os.path.join(HERE, "../data/raw_election/countypres_2000-2024.tab")) as f:
    for r in csv.DictReader(f, delimiter="\t"):
        if r["state"] == "OHIO":
            xw.add(r["county_name"].upper())
assert counties <= xw, counties - xw
assert len(counties) == 88, (len(counties), sorted(xw - counties))
with open(OUT, "w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["year", "district", "county", "candidate", "party_code", "votes"])
    w.writerows(rows)
print(f"{len(rows)} candidate-county rows, {len(D)} districts, {len(counties)} counties; all checks pass")
