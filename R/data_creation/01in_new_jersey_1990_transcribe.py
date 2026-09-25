"""New Jersey U.S. House 1990, county level, hand-transcribed from the NJ Division of Elections'
'Official Results, General Election, November 6, 1990 - House of Representatives'
('Candidates for the Office of House of Representatives', one table per congressional district,
one column per county with '(part)' for split counties, and a Total column).

Source: Internet Archive, Statistical Reference Index microfiche, item micro_IA40706939_0170
('State of New Jersey Results of the General Election Held Nov. 6, 1990'), scan pages 4-11;
local copy R/data/county_house_files/sri/nj1990/. Unlike micro_IA40706939_0171 (D and R only,
by municipality) this lists ALL candidates, minor parties included.

District 1 also had an UNEXPIRED-TERM special election on the same ballot (Andrews v. Mangini,
printed below the regular table on scan page 4); it is excluded - regular election only.

Checks (the script stops unless all pass): every candidate's county values sum to the printed
Total; every printed Total equals the FEC 'Federal Elections 90' result (NJ, regular elections);
all 21 counties appear. No write-in column is printed. Every value was read from the page images
(no value inferred from a check).

Output: R/data/county_house_files/sri/new_jersey_house_county_1990.csv
        (year,district,county,candidate,party_code,votes)
"""
import csv, os

HERE = os.path.dirname(os.path.abspath(__file__))
OUT = os.path.join(HERE, "../data/county_house_files/sri/new_jersey_house_county_1990.csv")

# district: (counties in column order, [(candidate, printed party, code, [county votes], printed total)])
DATA = {
    1: (["BURLINGTON", "CAMDEN", "GLOUCESTER"], [
        ("Robert E. Andrews", "Democratic", "D", [3935, 44698, 23782], 72415),
        ("Daniel J. Mangini", "Republican", "R", [2789, 33265, 21245], 57299),
        ("Jerry Zeldin", "Libertarian Party", "I", [106, 799, 687], 1592),
        ("William Henry Harris", "Populist Party", "I", [29, 795, 242], 1066),
        ("Walter E. Konstanty", "Pride and Honesty", "I", [61, 746, 615], 1422),
    ]),
    2: (["ATLANTIC", "CAPE MAY", "CUMBERLAND", "GLOUCESTER", "SALEM"], [
        ("William J. Hughes", "Democratic", "D", [31297, 21078, 20488, 10517, 14318], 97698),
        # Republican: 'NO NOMINATION MADE' (all zeros)
        ("William A. Kanengiser", "Populist Party", "I", [2807, 2198, 2804, 2073, 3238], 13120),
    ]),
    3: (["MONMOUTH", "OCEAN"], [
        ("Frank Pallone, Jr.", "Democratic", "D", [48864, 29002], 77866),
        ("Paul A. Kapalko", "Republican", "R", [43596, 30100], 73696),
        ("Richard D. McKean", "Independent", "I", [2979, 1398], 4377),
        ("Joseph A. Plonski", "Populist Party", "I", [446, 425], 871),
        ("William Stewart", "Libertarian Party", "I", [626, 1207], 1833),
    ]),
    4: (["BURLINGTON", "MERCER", "MIDDLESEX", "MONMOUTH", "OCEAN"], [
        ("Christopher H. Smith", "Republican", "R", [11330, 43758, 6628, 32607, 5597], 99920),
        ("Mark Setaro", "Democratic", "D", [6324, 26242, 5394, 14897, 2104], 54961),
        ("Joseph J. Notarangelo", "Populist Party", "I", [156, 460, 175, 335, 80], 1206),
        ("Carl Peters", "Libertarian Party", "I", [174, 768, 120, 647, 469], 2178),
        ("J.M. Carter", "God We Trust", "I", [88, 262, 47, 544, 93], 1034),
    ]),
    5: (["BERGEN", "PASSAIC", "SUSSEX"], [
        ("Marge Roukema", "Republican", "R", [81489, 15059, 21553], 118101),
        ("Lawrence Wayne Olsen", "Democratic", "D", [24968, 5074, 4968], 35010),
        ("Mark Richards", "Populist Party", "I", [1436, 552, 1010], 2998),
    ]),
    6: (["MIDDLESEX", "MONMOUTH", "UNION"], [
        ("Bernard J. Dwyer", "Democratic", "D", [47697, 3118, 12930], 63745),
        ('Paul "Daniels" Danielczyk', "Republican", "R", [47740, 3268, 7139], 58147),
        ("Randolph Waller", "Populist Party", "I", [2041, 46, 261], 2348),
        ("Howard F. Schoen", "Libertarian Party", "I", [1297, 90, 395], 1782),
    ]),
    7: (["ESSEX", "MIDDLESEX", "SOMERSET", "UNION"], [
        ("Matthew J. Rinaldo", "Republican", "R", [5061, 3697, 18969, 72339], 100066),
        ("Bruce H. Bergen", "Democratic", "D", [1336, 1164, 5401, 23198], 31099),
        ("Thomas V. Sarnowski", "Populist Party", "I", [93, 164, 1017, 1633], 2907),
    ]),
    8: (["BERGEN", "ESSEX", "MORRIS", "PASSAIC"], [
        ("Robert A. Roe", "Democratic", "D", [849, 18195, 296, 36457], 55797),
        # Republican: 'NO NOMINATION MADE' (all zeros)
        ("Bruce Eden", "Populist Party", "I", [160, 1716, 32, 1655], 3563),
        ("Stephen Sibilia", "Independent Conservative", "I", [341, 4140, 75, 8624], 13180),
    ]),
    9: (["BERGEN", "HUDSON"], [
        ("Robert G. Torricelli", "Democratic", "D", [77835, 4700], 82535),
        ("Peter J. Russo", "Republican", "R", [65204, 4454], 69658),
        ("Chester Grabowski", "Populist Party", "I", [2311, 262], 2573),
    ]),
    10: (["ESSEX", "UNION"], [
        ("Donald M. Payne", "Democratic", "D", [39487, 2619], 42106),
        ("Howard E. Berkeley", "Republican", "R", [7416, 1538], 8954),
        ("George Mehrabian", "Socialist Workers Party", "I", [611, 32], 643),
    ]),
    11: (["ESSEX", "MORRIS", "SUSSEX", "WARREN"], [
        ("Dean A. Gallo", "Republican", "R", [29562, 58555, 2143, 2421], 92681),
        ("Michael Gordon", "Democratic", "D", [19087, 26758, 693, 876], 47414),
        ('Jasper "Jack" Gould', "Populist Party", "I", [804, 2529, 115, 143], 3591),
    ]),
    12: (["HUNTERDON", "MERCER", "MIDDLESEX", "MORRIS", "SOMERSET", "SUSSEX", "WARREN"], [
        ("Dick Zimmer", "Republican", "R", [21534, 6015, 22425, 16807, 25736, 1786, 13548], 107851),
        ("Marguerite Chandler", "Democratic", "D", [7652, 4684, 15124, 5914, 12253, 612, 6017], 52256),
        ("Michael A. Notarangelo", "Populist Party", "I", [251, 31, 550, 224, 191, 25, 139], 1411),
        ("C. Max Kortepeter", "Independent Reform Party", "I", [455, 194, 317, 69, 1256, 16, 124], 2431),
        ("Joan I. Bottcher", '"Back to Basics"', "I", [523, 140, 961, 495, 1486, 127, 709], 4441),
    ]),
    13: (["BURLINGTON", "CAMDEN", "OCEAN"], [
        ("H. James Saxton", "Republican", "R", [36271, 22102, 41315], 99688),
        ("John H. Adler", "Democratic", "D", [30072, 18536, 18979], 67587),
        ("Howard Scott Pearlman", "World Without War", "I", [979, 410, 2742], 4131),
    ]),
    14: (["HUDSON"], [
        ("Frank J. Guarini", "Democratic", "D", [56455], 56455),
        ("Fred J. Theemling, Jr.", "Republican", "R", [24870], 24870),
        ("Louis Vernotico", "Right to Vote", "I", [309], 309),
        ("Donald K. Stoveken, Sr.", "Populist Party", "I", [502], 502),
        ("Jane E. Harris", "Socialist Workers Party", "I", [1318], 1318),
        ("Michael Ziruolo", "Better Affordable Government", "I", [1822], 1822),
    ]),
}

# FEC 'Federal Elections 90' (R/data/fec_official/federalelections90.pdf), New Jersey, regular elections
FEC = {
    1: {"Andrews": 72415, "Mangini": 57299, "Zeldin": 1592, "Konstanty": 1422, "Harris": 1066},
    2: {"Hughes": 97698, "Kanengiser": 13120},
    3: {"Pallone": 77866, "Kapalko": 73696, "McKean": 4377, "Stewart": 1833, "Plonski": 871},
    4: {"Smith": 99920, "Setaro": 54961, "Peters": 2178, "Notarangelo": 1206, "Carter": 1034},
    5: {"Roukema": 118101, "Olsen": 35010, "Richards": 2998},
    6: {"Dwyer": 63745, "Danielczyk": 58147, "Waller": 2348, "Schoen": 1782},
    7: {"Rinaldo": 100066, "Bergen": 31099, "Sarnowski": 2907},
    8: {"Roe": 55797, "Sibilia": 13180, "Eden": 3563},
    9: {"Torricelli": 82535, "Russo": 69658, "Grabowski": 2573},
    10: {"Payne": 42106, "Berkeley": 8954, "Mehrabian": 643},
    11: {"Gallo": 92681, "Gordon": 47414, "Gould": 3591},
    12: {"Zimmer": 107851, "Chandler": 52256, "Bottcher": 4441, "Kortepeter": 2431, "Notarangelo": 1411},
    13: {"Saxton": 99688, "Adler": 67587, "Pearlman": 4131},
    14: {"Guarini": 56455, "Theemling": 24870, "Ziruolo": 1822, "Harris": 1318, "Stoveken": 502, "Vernotico": 309},
}
FEC_DISTRICT_TOTAL = {1: 133794, 2: 110818, 3: 158643, 4: 159299, 5: 156109, 6: 126022, 7: 134072,
                      8: 72540, 9: 154766, 10: 51703, 11: 143686, 12: 168390, 13: 171406, 14: 85276}


def surname(name):
    parts = name.replace(",", " ").replace('"', " ").split()
    parts = [p for p in parts if p not in ("Jr.", "Sr.")]
    return parts[-1]


rows = []
for d, (counties, cands) in DATA.items():
    fec = FEC[d]
    assert len(cands) == len(fec), ("candidate count", d)
    dist_sum = 0
    for name, party, code, votes, total in cands:
        assert len(votes) == len(counties), ("columns", d, name)
        assert sum(votes) == total, ("row sum", d, name, sum(votes), total)
        assert fec[surname(name)] == total, ("FEC", d, name, total, fec[surname(name)])
        dist_sum += total
        rows += [(1990, d, c, name, code, v) for c, v in zip(counties, votes)]
    assert dist_sum == FEC_DISTRICT_TOTAL[d], ("district total", d, dist_sum, FEC_DISTRICT_TOTAL[d])
    assert sum(1 for c in cands if c[2] == "D") <= 1 and sum(1 for c in cands if c[2] == "R") <= 1

counties = {r[2] for r in rows}
assert len(counties) == 21, sorted(counties)
with open(OUT, "w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["year", "district", "county", "candidate", "party_code", "votes"])
    w.writerows(rows)
print(f"{len(rows)} candidate-county rows, 14 districts, 21 counties; row sums, FEC candidate and district totals all tie")
