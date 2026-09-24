"""Michigan U.S. House 1990, county level, hand-transcribed from the Michigan Manual
'Official Canvass of Votes, General Election' (U.S. House of Representatives,
2-year term; printed pp. 886-890), screenshots supplied by the project lead:
R/data/county_house_files/michigan/1990/Screenshot From 2026-09-24 19-07-{10,17,22}.png
(814x656, two book pages each; read after 3x LANCZOS upscaling).

Each row is (county, Total by County, candidate votes..., write-ins). Checks (the
script stops on any failure):
  * every county row's candidate votes + write-ins equal its 'Total by County';
  * every column (incl. Total by County and write-ins) adds up to the printed Totals row;
  * every district's candidate totals equal the FEC's (R/data/fec_official/fec1990_house.csv).
Split counties (e.g. Wayne, Oakland, Macomb, Kent) appear in each district they touch.
Party codes as printed (Dem./Rep./Lib./NPA/Tis./Worw.); in the CSV D = Dem., R = Rep., else I.
Write-ins are used in the checks but not written to the CSV.

Output: R/data/county_house_files/michigan/michigan_house_county_1990.csv
        (year,district,county,candidate,party_code,votes)
"""
import csv, os

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.join(HERE, "..", "..")

# district: (candidates [(name, printed party)], rows [(county, total, *cand votes, writein)], totals row)
D = {
    1: ([("John Conyers, Jr.", "Dem."), ("Ray Shoulders", "Rep."), ("Jonathan Flint", "Lib."), ("Robert Mays", "NPA")],
        [("WAYNE", 85756, 76556, 7298, 764, 1134, 4)],
        (85756, 76556, 7298, 764, 1134, 4)),
    2: ([("Elmer White", "Dem."), ("Carl Pursell", "Rep."), ("Paul Jensen", "Tis.")],
        [("BRANCH", 1135, 361, 759, 15, 0),
         ("HILLSDALE", 9975, 3031, 6798, 146, 0),
         ("JACKSON", 35011, 10713, 23315, 982, 1),
         ("LENAWEE", 12626, 4377, 8075, 174, 0),
         ("WASHTENAW", 52628, 21667, 29759, 1197, 5),
         ("WAYNE", 38391, 9529, 27256, 1605, 1)],
        (149766, 49678, 95962, 4119, 7)),
    3: ([("Howard Wolpe", "Dem."), ("Brad Haskins", "Rep.")],
        [("BARRY", 5753, 3048, 2705, 0),
         ("CALHOUN", 36356, 20469, 15882, 5),
         ("EATON", 30342, 16474, 13868, 0),
         ("INGHAM", 18969, 13403, 5564, 2),
         ("KALAMAZOO", 50970, 28982, 21988, 0)],
        (142390, 82376, 60007, 7)),
    4: ([("JoAnne McFarland", "Dem."), ("Fred Upton", "Rep.")],
        [("ALLEGAN", 16705, 6775, 9930, 0),
         ("BERRIEN", 37791, 16673, 21117, 1),
         ("BRANCH", 8289, 3695, 4594, 0),
         ("CASS", 10355, 4973, 5382, 0),
         ("KALAMAZOO", 9369, 3433, 5936, 0),
         ("OTTAWA", 18663, 6558, 12104, 1),
         ("ST. JOSEPH", 13550, 5417, 8133, 0),
         ("VAN BUREN", 16580, 7925, 8654, 1)],
        (131302, 55449, 75850, 3)),
    5: ([("Thomas Trzybinski", "Dem."), ("Paul Henry", "Rep.")],
        [("ALLEGAN", 6801, 1522, 5278, 1),
         ("BARRY", 8327, 2510, 5816, 1),
         ("IONIA", 6648, 1857, 4791, 0),
         ("KENT", 144678, 34994, 109666, 18),
         ("NEWAYGO", 1044, 287, 757, 0)],
        (167498, 41170, 126308, 20)),
    6: ([("Bob Carr", "Dem.")],
        [("CLINTON", 1087, 1083, 4),
         ("GENESEE", 806, 806, 0),
         ("INGHAM", 43909, 43744, 165),
         ("JACKSON", 1643, 1643, 0),
         ("LIVINGSTON", 14517, 14485, 32),
         ("OAKLAND", 34041, 33998, 43),
         ("SHIAWASSEE", 1788, 1788, 0)],
        (97791, 97547, 244)),
    7: ([("Dale Kildee", "Dem."), ("David Morrill", "Rep.")],
        [("GENESEE", 103548, 75079, 28469, 0),
         ("LAPEER", 18833, 10366, 8466, 1),
         ("OAKLAND", 8684, 4300, 4383, 1),
         ("SANILAC", 164, 69, 95, 0),
         ("SHIAWASSEE", 839, 493, 346, 0)],
        (132068, 90307, 41759, 2)),
    8: ([("Bob Traxler", "Dem."), ("James White", "Rep.")],
        [("ARENAC", 3961, 2453, 1508, 0),
         ("BAY", 33780, 25001, 8778, 1),
         ("GENESEE", 9277, 6076, 3201, 0),
         ("HURON", 10644, 7265, 3379, 0),
         ("LAPEER", 697, 444, 253, 0),
         ("MIDLAND", 1314, 773, 541, 0),
         ("SAGINAW", 55564, 39469, 16093, 2),
         ("ST. CLAIR", 2460, 1268, 1192, 0),
         ("SANILAC", 11706, 6841, 4865, 0),
         ("TUSCOLA", 14762, 9313, 5449, 0)],
        (144165, 98903, 45259, 3)),
    9: ([("Geraldine Greene", "Dem."), ("Guy Vander Jagt", "Rep.")],
        [("BENZIE", 4168, 2205, 1963, 0),
         ("GRAND TRAVERSE", 12400, 6823, 5577, 0),
         ("IONIA", 7637, 3411, 4226, 0),
         ("KENT", 1300, 529, 771, 0),
         ("LAKE", 2859, 1678, 1181, 0),
         ("LEELANAU", 6271, 3460, 2810, 1),
         ("MANISTEE", 7516, 4435, 3081, 0),
         ("MASON", 9219, 4683, 4536, 0),
         ("MONTCALM", 13140, 5501, 7637, 2),
         ("MUSKEGON", 42402, 22030, 20368, 4),
         ("NEWAYGO", 9700, 4146, 5554, 0),
         ("OCEANA", 6545, 3020, 3525, 0),
         ("OTTAWA", 39537, 11683, 27849, 5)],
        (162694, 73604, 89078, 12)),
    10: ([("Joan Dennison", "Dem."), ("Dave Camp", "Rep."), ("Charles Congdon", "Lib.")],
         [("ANTRIM", 164, 56, 106, 2, 0),
          ("CLARE", 7533, 2696, 4689, 139, 9),
          ("CLINTON", 16156, 5827, 10112, 213, 4),
          ("CRAWFORD", 2572, 747, 1774, 51, 0),
          ("GLADWIN", 6266, 2381, 3743, 137, 5),
          ("GRAND TRAVERSE", 5482, 1838, 3586, 58, 0),
          ("GRATIOT", 9679, 2977, 6570, 115, 17),
          ("IOSCO", 242, 104, 131, 7, 0),
          ("ISABELLA", 12993, 4052, 8481, 411, 49),
          ("KALKASKA", 3444, 1182, 2230, 32, 0),
          ("MECOSTA", 8562, 2870, 5600, 82, 10),
          ("MIDLAND", 24706, 5672, 18202, 589, 243),
          ("MISSAUKEE", 3862, 936, 2868, 58, 0),
          ("OGEMAW", 5513, 2189, 3237, 87, 0),
          ("OSCEOLA", 5395, 1756, 3599, 37, 3),
          ("OSCODA", 1186, 461, 704, 21, 0),
          ("ROSCOMMON", 6849, 2603, 4206, 40, 0),
          ("SAGINAW", 9414, 3619, 5600, 195, 0),
          ("SHIAWASSEE", 16239, 6581, 9481, 173, 4),
          ("WEXFORD", 7459, 2376, 5033, 49, 1)],
         (153716, 50923, 99952, 2496, 345)),
    11: ([("Marcia Gould", "Dem."), ("Bob Davis", "Rep.")],
         [("ALCONA", 3094, 1072, 2021, 1),
          ("ALGER", 3437, 1770, 1667, 0),
          ("ALPENA", 8367, 3127, 5239, 1),
          ("ANTRIM", 5760, 1979, 3781, 0),
          ("BARAGA", 2370, 950, 1420, 0),
          ("CHARLEVOIX", 7132, 2311, 4821, 0),
          ("CHEBOYGAN", 6040, 2129, 3911, 0),
          ("CHIPPEWA", 8547, 2969, 5578, 0),
          ("CRAWFORD", 591, 211, 380, 0),
          ("DELTA", 11526, 5079, 6447, 0),
          ("DICKINSON", 8768, 3776, 4992, 0),
          ("EMMET", 7780, 2319, 5461, 0),
          ("GOGEBIC", 5978, 3063, 2915, 0),
          ("GRAND TRAVERSE", 1733, 507, 1226, 0),
          ("HOUGHTON", 9874, 3705, 6169, 0),
          ("IOSCO", 7947, 2996, 4951, 0),
          ("IRON", 4795, 1924, 2871, 0),
          ("KEWEENAW", 833, 357, 476, 0),
          ("LUCE", 1976, 601, 1375, 0),
          ("MACKINAC", 4004, 1310, 2694, 0),
          ("MARQUETTE", 17739, 7969, 9770, 0),
          ("MENOMINEE", 6290, 2683, 3607, 0),
          ("MONTMORENCY", 2398, 784, 1614, 0),
          ("ONTONAGON", 3294, 1234, 2060, 0),
          ("OSCODA", 998, 283, 715, 0),
          ("OTSEGO", 5471, 1789, 3682, 0),
          ("PRESQUE ISLE", 4520, 1535, 2985, 0),
          ("SCHOOLCRAFT", 3054, 1327, 1727, 0)],
         (154316, 59759, 94555, 2)),
    12: ([("David Bonior", "Dem."), ("Jim Dingeman", "Rep."), ("Robert Roddis", "Lib.")],
         [("MACOMB", 115494, 76660, 36936, 1896, 2),
          ("ST. CLAIR", 36331, 21572, 14183, 576, 0)],
         (151825, 98232, 51119, 2472, 2)),
    13: ([("Barbara-Rose Collins", "Dem."), ("Carl Edwards, Sr.", "Rep."), ("Jeff Hampton", "Lib."),
          ("Joyce Griffin", "Worw."), ("Cleve Pulley", "NPA")],
         [("WAYNE", 67824, 54345, 11203, 649, 1090, 530, 7)],
         (67824, 54345, 11203, 649, 1090, 530, 7)),
    14: ([("Dennis Hertel", "Dem."), ("Kenneth McNealy", "Rep."), ("Robert Gale", "Tis."), ("Kenneth Morris", "Lib.")],
         [("MACOMB", 65483, 40304, 22393, 1827, 958, 1),
          ("OAKLAND", 13282, 8229, 4653, 230, 170, 0),
          ("WAYNE", 44656, 29973, 13453, 635, 593, 2)],
         (123421, 78506, 40499, 2692, 1721, 3)),
    15: ([("William Ford", "Dem."), ("Burl Adkins", "Rep."), ("David Hunt", "Lib.")],
         [("WASHTENAW", 21597, 13299, 7740, 558, 0),
          ("WAYNE", 90738, 55443, 33352, 1939, 4)],
         (112335, 68742, 41092, 2497, 4)),
    16: ([("John Dingell", "Dem."), ("Frank Beaumont", "Rep."), ("Roger Pope", "Lib.")],
         [("LENAWEE", 10389, 6134, 4196, 59, 0),
          ("MONROE", 30614, 19323, 11014, 276, 1),
          ("WAYNE", 92611, 63505, 27419, 1684, 3)],
         (133614, 88962, 42629, 2019, 4)),
    17: ([("Sander Levin", "Dem."), ("Blaine Lankford", "Rep.")],
         [("OAKLAND", 76685, 53390, 23289, 6),
          ("WAYNE", 55629, 38815, 16811, 3)],
         (132314, 92205, 40100, 9)),
    18: ([("Walter Briggs IV", "Dem."), ("William Broomfield", "Rep.")],
         [("LIVINGSTON", 9076, 3377, 5699, 0),
          ("MACOMB", 18687, 6730, 11955, 2),
          ("OAKLAND", 163071, 54078, 108975, 18)],
         (190834, 64185, 126629, 20)),
}

# FEC candidate totals (fec1990_house.csv, MI), in the order of the candidate lists above
FEC = {1: [76556, 7298, 764, 1134], 2: [49678, 95962, 4119], 3: [82376, 60007], 4: [55449, 75850],
       5: [41170, 126308], 6: [97547], 7: [90307, 41759], 8: [98903, 45259], 9: [73604, 89078],
       10: [50923, 99952, 2496], 11: [59759, 94555], 12: [98232, 51119, 2472],
       13: [54345, 11203, 649, 1090, 530], 14: [78506, 40499, 2692, 1721], 15: [68742, 41092, 2497],
       16: [88962, 42629, 2019], 17: [92205, 40100], 18: [64185, 126629]}

# cross-check the FEC table above against the parsed FEC file (district totals incl. scattered write-ins)
fec_tot = {}
for r in csv.DictReader(open(os.path.join(ROOT, "R/data/fec_official/fec1990_house.csv"))):
    if r["state_po"] == "MI" and r["is_total"] == "True":
        fec_tot[int(r["district"])] = int(float(r["total"]))

out = []
for d, (cands, rows, tot) in D.items():
    n = len(cands)
    for r in rows:
        assert len(r) == n + 3, (d, r)
        assert sum(r[2:]) == r[1], ("row", d, r[0], sum(r[2:]), r[1])
    for j in range(n + 2):
        assert sum(r[1 + j] for r in rows) == tot[j], ("column", d, j, sum(r[1 + j] for r in rows), tot[j])
    assert list(tot[1:1 + n]) == FEC[d], ("FEC", d, tot[1:1 + n], FEC[d])
    assert tot[0] == fec_tot[d], ("FEC total", d, tot[0], fec_tot[d])
    for r in rows:
        for k, (name, p) in enumerate(cands):
            code = {"Dem.": "D", "Rep.": "R"}.get(p, "I")
            out.append(dict(year=1990, district=d, county=r[0], candidate=name, party_code=code, votes=r[2 + k]))

counties = {o["county"] for o in out}
assert len(counties) == 83, (len(counties), sorted(counties))
with open(os.path.join(ROOT, "R/data/county_house_files/michigan/michigan_house_county_1990.csv"), "w", newline="") as f:
    w = csv.DictWriter(f, fieldnames=["year", "district", "county", "candidate", "party_code", "votes"])
    w.writeheader()
    w.writerows(out)
print(f"{len(D)} districts, {sum(len(v[1]) for v in D.values())} county rows, {len(counties)} counties, "
      f"{len(out)} candidate-county rows; all row, column and FEC checks pass")
