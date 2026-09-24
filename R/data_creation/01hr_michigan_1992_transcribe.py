"""Michigan U.S. House 1992, county level, hand-transcribed.

Source: Michigan Manual 1993-1994 (Legislative Council, State of Michigan),
'Official Canvass of Votes, General Election', U.S. House of Representatives,
printed pp. 854-857 = PDF pp. 904-907 of R/data/county_house_files/michigan/mi_95-96.pdf
(the file name is wrong: the volume is the 1993-1994 Manual, general election of
November 3, 1992). 16 districts.

Keys: (1) the 300 dpi renders of those pages (pdftoppm -r 300 -gray), read digit by
digit; (2) the project lead's screenshots of the same pages
(R/data/county_house_files/michigan/1992/*.png), compared cell by cell. The PDF text
layer holds only headings and county names (no numbers), so it is not a key.

Checks (the script stops on any failure):
  * every county row: candidate votes + write-ins == printed 'Total by County'
  * every column: county rows sum to the printed 'Totals' row
  * district candidate totals == the FEC 1992 book (federalelections92.pdf) where it
    lists them (FEC_1992 below)
  * all 83 Michigan counties appear
One printed cell contradicts the page's own row and column totals and the FEC
(1st District, Kalkaska, Stupak printed 2,914, must be 2,915): corrected in CORRECTIONS.
Party code: Dem. -> D, Rep. -> R, everything else I. Write-ins are used in the
checks but not written to the CSV.

Output: R/data/county_house_files/michigan/michigan_house_county_1992.csv
(year, district, county, candidate, party_code, votes)
"""
import csv, os

HERE = os.path.dirname(os.path.abspath(__file__))
OUT = os.path.join(HERE, "../data/county_house_files/michigan/michigan_house_county_1992.csv")

# district: (candidates [(name, printed party)], rows [(county, total_by_county, votes..., write_in)], totals row)
D = {
    1: ([("Bart Stupak", "Dem."), ("Philip Ruppe", "Rep."), ("Gerald Aydlott", "Lib."), ("Lyman Clark", "NLP")], [
        ("Alger", 4311, 2642, 1559, 68, 41, 1),
        ("Alpena", 13916, 6925, 6778, 112, 101, 0),
        ("Antrim", 8886, 3983, 4595, 247, 61, 0),
        ("Baraga", 3403, 2096, 1251, 37, 18, 1),
        ("Benzie", 6111, 3062, 2874, 96, 78, 1),
        ("Charlevoix", 11175, 5021, 5355, 643, 156, 0),
        ("Cheboygan", 9877, 5137, 4564, 130, 46, 0),
        ("Chippewa", 12377, 6582, 5629, 100, 64, 2),
        ("Crawford", 1340, 693, 608, 25, 14, 0),
        ("Delta", 17168, 11217, 5776, 106, 69, 0),
        ("Dickinson", 12453, 7378, 4969, 65, 41, 0),
        ("Emmet", 12809, 5232, 6862, 483, 226, 6),
        ("Gogebic", 8414, 5369, 2960, 50, 35, 0),
        ("Grand Traverse", 32826, 15065, 16481, 684, 569, 27),
        ("Houghton", 14239, 7866, 6046, 140, 187, 0),
        ("Iron", 6771, 4410, 2266, 48, 45, 2),
        ("Kalkaska", 5702, 2914, 2625, 96, 66, 0),   # Stupak cell misprinted: see CORRECTIONS
        ("Keweenaw", 1137, 709, 410, 6, 12, 0),
        ("Leelanau", 8868, 4101, 4430, 185, 150, 2),
        ("Luce", 2317, 1160, 1130, 17, 10, 0),
        ("Mackinac", 5531, 2787, 2641, 48, 55, 0),
        ("Marquette", 30402, 18811, 10993, 337, 261, 0),
        ("Menominee", 10445, 6593, 3768, 48, 36, 0),
        ("Montmorency", 4260, 2074, 2107, 42, 37, 0),
        ("Ontonagon", 4506, 2777, 1680, 27, 22, 0),
        ("Otsego", 8472, 4086, 4126, 172, 88, 0),
        ("Presque Isle", 7052, 3819, 3103, 65, 65, 0),
        ("Schoolcraft", 3851, 2347, 1470, 17, 17, 0),
    ], (268619, 144857, 117056, 4094, 2570, 42)),
    2: ([("John Miltner", "Dem."), ("Peter Hoekstra", "Rep."), ("Dick Jacobs", "Lib.")], [
        ("Allegan", 29764, 8801, 20225, 728, 10),
        ("Barry", 8073, 3245, 4649, 179, 0),
        ("Lake", 4264, 2343, 1777, 144, 0),
        ("Manistee", 9863, 5003, 4641, 219, 0),
        ("Mason", 12309, 4807, 7035, 467, 0),
        ("Muskegon", 59332, 28381, 30056, 888, 7),
        ("Newaygo", 15286, 5821, 9227, 234, 4),
        ("Oceana", 8685, 3409, 5135, 138, 3),
        ("Ottawa", 88184, 19186, 67310, 1633, 55),
        ("Wexford", 11001, 5269, 5522, 210, 0),
    ], (246761, 86265, 155577, 4840, 79)),
    3: ([("Carol Kooistra", "Dem."), ("Paul Henry", "Rep."), ("Richard Whitelock", "Lib."), ("Susan Normandin", "NLP")], [
        ("Barry", 9828, 4010, 5572, 140, 106, 0),
        ("Ionia", 20141, 8344, 11130, 477, 189, 1),
        ("Kent", 234979, 83573, 145749, 2615, 2933, 109),
    ], (264948, 95927, 162451, 3232, 3228, 110)),
    4: ([("Lisa Donaldson", "Dem."), ("Dave Camp", "Rep."), ("Joan Dennison", "Tis."), ("Gary Bradley", "Lib."), ("Thomas List", "NLP")], [
        ("Clare", 11682, 3955, 7422, 172, 78, 55, 0),
        ("Clinton", 26307, 9786, 15617, 426, 346, 130, 2),
        ("Crawford", 4295, 1380, 2754, 49, 38, 74, 0),
        ("Gladwin", 10225, 3653, 6330, 141, 57, 44, 0),
        ("Gratiot", 14576, 4204, 10038, 167, 111, 56, 0),
        ("Isabella", 20625, 6982, 13132, 234, 178, 98, 1),
        ("Mecosta", 13834, 4805, 8742, 136, 92, 59, 0),
        ("Midland", 37825, 10142, 26986, 343, 227, 127, 0),
        ("Missaukee", 5813, 1432, 4254, 62, 37, 28, 0),
        ("Montcalm", 18930, 8066, 10386, 185, 209, 84, 0),
        ("Ogemaw", 8371, 3378, 4761, 137, 62, 33, 0),
        ("Osceola", 8239, 2607, 5479, 69, 63, 21, 0),
        ("Oscoda", 3619, 1311, 2195, 49, 31, 33, 0),
        ("Roscommon", 10794, 4247, 6281, 154, 60, 50, 2),
        ("Saginaw", 34811, 13016, 20744, 543, 266, 242, 0),
        ("Shiawassee", 21593, 8609, 12216, 477, 172, 113, 6),
    ], (251539, 87573, 157337, 3344, 2027, 1247, 11)),
    5: ([("James Barcia", "Dem."), ("Keith Muxlow", "Rep."), ("Lloyd Clarke", "WORW")], [
        ("Alcona", 4552, 2133, 2379, 40, 0),
        ("Arenac", 6381, 3944, 2388, 49, 0),
        ("Bay", 50817, 38068, 12474, 272, 3),
        ("Genesee", 49028, 32273, 15043, 1712, 0),
        ("Huron", 14518, 7579, 6856, 83, 0),
        ("Iosco", 12302, 6255, 5768, 279, 0),
        ("Lapeer", 13651, 5977, 7241, 433, 0),
        ("Saginaw", 55522, 34715, 19761, 1046, 0),
        ("Sanilac", 16760, 4879, 11767, 112, 2),
        ("Tuscola", 21461, 11795, 9421, 244, 1),
    ], (244992, 147618, 93098, 4270, 6)),
    6: ([("Andy Davis", "Dem."), ("Fred Upton", "Rep.")], [
        ("Allegan", 5757, 2441, 3316, 0),
        ("Berrien", 65994, 22500, 43488, 6),
        ("Cass", 17930, 6851, 11079, 0),
        ("Kalamazoo", 96857, 40146, 56711, 0),   # printed '40.146' (period for comma)
        ("St. Joseph", 21139, 6406, 14731, 2),
        ("Van Buren", 25435, 10676, 14758, 1),
    ], (233112, 89020, 144083, 9)),
    7: ([("Nick Smith", "Rep."), ("Kenneth Proctor", "Lib.")], [   # no Democratic candidate
        ("Barry", 1878, 1704, 174, 0),
        ("Branch", 9307, 8785, 520, 2),
        ("Calhoun", 35594, 29739, 5810, 45),
        ("Eaton", 30843, 25338, 5463, 42),
        ("Hillsdale", 11993, 11120, 861, 12),
        ("Jackson", 40644, 35838, 4784, 22),
        ("Lenawee", 17964, 17500, 444, 20),
        ("Washtenaw", 4645, 3948, 695, 2),
    ], (152868, 133972, 18751, 145)),
    8: ([("Bob Carr", "Dem."), ("Dick Chrysler", "Rep."), ("Michael Marotta", "Lib."), ("Frank McAlpine", "NPA")], [
        ("Genesee", 53005, 27743, 22959, 740, 1562, 1),
        ("Ingham", 131509, 66339, 55540, 2644, 6976, 10),
        ("Livingston", 60163, 23411, 33633, 1109, 2010, 0),
        ("Oakland", 5799, 2641, 2774, 67, 317, 0),
        ("Shiawassee", 7429, 3651, 3296, 73, 409, 0),
        ("Washtenaw", 26802, 11732, 13704, 482, 881, 3),
    ], (284707, 135517, 131906, 5115, 12155, 14)),
    9: ([("Dale Kildee", "Dem."), ("Megan O'Neill", "Rep."), ("Jerome White", "WORL"), ("Key Halverson", "NLP")], [
        ("Genesee", 91987, 67463, 23262, 684, 576, 2),
        ("Lapeer", 18940, 8881, 9582, 231, 245, 1),
        ("Oakland", 138603, 57612, 78954, 957, 1070, 10),
    ], (249530, 133956, 111798, 1872, 1891, 13)),
    10: ([("David Bonior", "Dem."), ("Douglas Carl", "Rep."), ("David Weidner", "Lib.")], [
        ("Macomb", 198382, 103301, 89421, 5656, 4),
        ("St. Clair", 61831, 34892, 25497, 1442, 0),
    ], (260213, 138193, 114918, 7098, 4)),
    11: ([("Walter Briggs", "Dem."), ("Joe Knollenberg", "Rep."), ("Brian Wright", "Lib."), ("Henry Clark", "NLP")], [
        ("Oakland", 228569, 93425, 130811, 2891, 1422, 20),
        ("Wayne", 64529, 24300, 38129, 1253, 847, 0),
    ], (293098, 117725, 168940, 4144, 2269, 20)),
    12: ([("Sander Levin", "Dem."), ("John Pappageorge", "Rep."), ("Charles Hahn", "Lib."), ("R. Montgomery", "NLP")], [
        ("Macomb", 122988, 62818, 57834, 1364, 970, 2),
        ("Oakland", 138361, 74696, 61523, 1387, 754, 1),
    ], (261349, 137514, 119357, 2751, 1724, 3)),
    13: ([("William Ford", "Dem."), ("R. Geake", "Rep."), ("Paul Jensen", "Tis."), ("Larry Roberts", "WORL"), ("Randall Roe", "NPA")], [
        ("Washtenaw", 92812, 56944, 32185, 892, 468, 2315, 8),
        ("Wayne", 153076, 70698, 72984, 2422, 659, 6311, 2),
    ], (245888, 127642, 105169, 3314, 1127, 8626, 10)),
    14: ([("John Conyers, Jr.", "Dem."), ("John Gordon", "Rep."), ("D'Artagnan Collier", "WORL"), ("Richard Miller", "NLP")], [
        ("Wayne", 200879, 165496, 32036, 1296, 2043, 8),
    ], (200879, 165496, 32036, 1296, 2043, 8)),
    15: ([("Barbara-Rose Collins", "Dem."), ("Charles Vincent", "Rep."), ("Jane Meade", "NLP"), ("James Harris, Jr.", "NPA")], [
        ("Wayne", 184964, 148908, 31849, 1496, 2704, 7),
    ], (184964, 148908, 31849, 1496, 2704, 7)),
    16: ([("John Dingell", "Dem."), ("Frank Beaumont", "Rep."), ("Max Siegle", "Tis."), ("Jeff Hampton", "Lib."), ("Martin McLaughlin", "WORL")], [
        ("Monroe", 51514, 33696, 16525, 585, 391, 317, 0),
        ("Wayne", 189422, 123268, 59169, 3463, 1996, 1525, 1),
    ], (240936, 156964, 75694, 4048, 2387, 1842, 1)),
}

# FEC 1992 book (federalelections92.pdf), Michigan House general election, candidate totals by district,
# in the column order above (from pdftotext of the FEC book, 'MICHIGAN', pp. 69-70).
FEC_1992 = {
    1: [144857, 117056, 4094, 2570], 2: [86265, 155577, 4840], 3: [95927, 162451, 3232, 3228],
    4: [87573, 157337, 3344, 2027, 1247], 5: [147618, 93098, 4270], 6: [89020, 144083], 7: [133972, 18751],
    8: [135517, 131906, 5115, 12155], 9: [133956, 111798, 1872, 1891], 10: [138193, 114918, 7098],
    11: [117725, 168940, 4144, 2269], 12: [137514, 119357, 2751, 1724], 13: [127642, 105169, 3314, 1127, 8626],
    14: [165496, 32036, 1296, 2043], 15: [148908, 31849, 1496, 2704], 16: [156964, 75694, 4048, 2387, 1842],
}

# Printed cells that contradict the page's own totals, corrected. Each needs agreement of the printed row total, the printed column total and the FEC.
CORRECTIONS = {
    # printed 2,914; row 'Total by County' 5,702 needs 2,915, the Stupak column total 144,857 needs 2,915, and FEC gives Stupak 144,857
    (1, "Kalkaska", 0): (2914, 2915),
}

MI_COUNTIES = 83
for (d, county, k), (printed, fixed) in CORRECTIONS.items():
    rows = D[d][1]
    i = next(i for i, r in enumerate(rows) if r[0] == county)
    assert rows[i][2 + k] == printed
    rows[i] = rows[i][:2 + k] + (fixed,) + rows[i][3 + k:]
out = []
for d, (cands, rows, tot) in D.items():
    n = len(cands)
    for r in rows:
        assert len(r) == n + 3, (d, r)
        assert sum(r[2:]) == r[1], ("row total", d, r[0], sum(r[2:]), r[1])
    assert len(tot) == n + 2, (d, tot)
    for j in range(1, n + 3):
        assert sum(r[j] for r in rows) == tot[j - 1], ("column total", d, j, sum(r[j] for r in rows), tot[j - 1])
    if d in FEC_1992:
        assert list(tot[1:n + 1]) == FEC_1992[d], ("FEC", d, tot[1:n + 1], FEC_1992[d])
    for r in rows:
        for k, (name, party) in enumerate(cands):
            code = {"Dem.": "D", "Rep.": "R"}.get(party, "I")
            out.append(dict(year=1992, district=d, county=r[0].upper(), candidate=name, party_code=code, votes=r[2 + k]))
counties = {r["county"] for r in out}
assert len(counties) == MI_COUNTIES, len(counties)
with open(OUT, "w", newline="") as f:
    w = csv.DictWriter(f, fieldnames=["year", "district", "county", "candidate", "party_code", "votes"])
    w.writeheader()
    w.writerows(out)
print(f"{len(D)} districts, {len(counties)} counties, {len(out)} candidate-county rows; row and column totals tie; FEC-checked districts: {sorted(FEC_1992)}")
