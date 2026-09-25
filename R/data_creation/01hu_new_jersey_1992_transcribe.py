"""New Jersey U.S. House 1992, county level, hand-transcribed from the NJ Division of Elections
report 'Candidates for the Office of House of Representatives, General Election, November 3, 1992'
(R/data/county_house_files/new_jersey/1992-general-election-results-house.pdf; image-only typescript,
13 PDF pages = printed pp. 6-18, one district per page; downloaded from
https://www.nj.gov/state/elections/election-information-1992.shtml).

Layout: one row per candidate (designation, name, address, party) with one column per county the
district touches ('(part)' for split counties) and a Total column. Read by eye from 300 dpi renders.

Checks (the script stops if any fails):
  * every candidate's county votes add up to the candidate's printed Total;
  * every candidate's Total equals the Clerk of the House 'Statistics of the Presidential and
    Congressional Election of November 3, 1992' (CLERK below, typed from the Clerk's page);
  * all 21 counties appear.
Party code: D / R from the party at the end of the designation line (confirmed against the Clerk),
I for every other ballot line. No write-in column is printed.

Output: R/data/county_house_files/new_jersey/new_jersey_house_county_1992.csv
        (year, district, county, candidate, party_code, votes)
"""
import csv, os

HERE = os.path.dirname(os.path.abspath(__file__))
OUT = os.path.join(HERE, "../data/county_house_files/new_jersey/new_jersey_house_county_1992.csv")

# district: (counties, [(candidate, party_code, [votes per county], printed total), ...])
D = {
    1: (["BURLINGTON", "CAMDEN", "GLOUCESTER"], [
        ("Robert E. Andrews", "D", [8068, 103432, 42025], 153525),
        ("Lee A. Solomon", "R", [3601, 40087, 21435], 65123),
        ("James E. Smith", "I", [144, 2172, 1445], 3761),
        ("Jerry Zeldin", "I", [155, 1511, 975], 2641),
        ("Kenneth L. Lowndes", "I", [74, 1529, 560], 2163),
        ("Nicholas Pastuch", "I", [24, 625, 210], 859),
    ]),
    2: (["ATLANTIC", "BURLINGTON", "CAPE MAY", "CUMBERLAND", "GLOUCESTER", "SALEM"], [
        ("William J. Hughes", "D", [53087, 142, 26911, 23761, 14326, 14238], 132465),
        ("Frank A. LoBiondo", "R", [30396, 183, 18473, 22618, 14315, 12330], 98315),
        ("Roger W. Bacon", "I", [410, 3, 323, 796, 641, 402], 2575),
        ("Joseph Ponczek", "I", [506, 3, 317, 467, 504, 270], 2067),
        ("Andrea Lippi", "I", [537, 2, 275, 317, 283, 191], 1605),
    ]),
    3: (["BURLINGTON", "CAMDEN", "OCEAN"], [
        ("Jim Saxton", "R", [67760, 26084, 57524], 151368),
        ("Timothy E. Ryan", "D", [41245, 17355, 35412], 94012),
        ("Helen L. Radder", "I", [1254, 856, 601], 2711),
        ("Joseph A. Plonski", "I", [170, 226, 1913], 2309),
        ("Michael S. Permuko", "I", [629, 65, 1034], 1728),
        ("James Reilly", "I", [432, 110, 373], 915),
        ("William Donald McMahon", "I", [332, 122, 447], 901),
        ("Anthony J. Verderese", "I", [283, 92, 374], 749),
        ("Martin T. King", "I", [321, 131, 141], 593),
        ("Frank Burke", "I", [175, 106, 231], 512),
    ]),
    4: (["BURLINGTON", "MERCER", "MONMOUTH", "OCEAN"], [
        ("Christopher H. Smith", "R", [18560, 42611, 28400, 59524], 149095),
        ("Brian M. Hughes", "D", [10948, 31344, 10873, 31349], 84514),
        ("Benjamin Grindlinger", "I", [194, 455, 249, 2086], 2984),
        ("Patrick C. Pasculli", "I", [235, 607, 338, 957], 2137),
        ("Agnes A. James", "I", [163, 451, 135, 881], 1630),
        ("Joseph J. Notarangelo", "I", [40, 154, 84, 587], 865),
    ]),
    5: (["BERGEN", "PASSAIC", "SUSSEX", "WARREN"], [
        ("Marge Roukema", "R", [127759, 18863, 25608, 23968], 196198),
        ("Frank R. Lucas", "D", [41245, 6261, 7253, 12820], 67579),
        ("William J. Leonard", "I", [2579, 534, 1394, 1675], 6182),
        ("Michael V. Pierone", "I", [1031, 212, 812, 581], 2636),
        ("George Lahood", "I", [693, 76, 119, 106], 994),
        ("Stuart Bacha", "I", [367, 91, 182, 142], 782),
    ]),
    6: (["MIDDLESEX", "MONMOUTH"], [
        ("Frank Pallone, Jr.", "D", [64622, 53644], 118266),
        ("Joseph M. Kyrillos", "R", [54427, 46522], 100949),
        ("Joseph Spalletta", "I", [1988, 165], 2153),
        ("Bill Stewart", "I", [831, 573], 1404),
        ("Peter Cerrato", "I", [929, 144], 1073),
        ("George P. Predham", "I", [853, 98], 951),
        ("Simone Berg", "I", [308, 305], 613),
        ("Kenneth Matto", "I", [227, 184], 411),
        ("Charles H. Dickson", "I", [169, 104], 273),
    ]),
    7: (["ESSEX", "MIDDLESEX", "SOMERSET", "UNION"], [
        ("Bob Franks", "R", [6685, 25888, 33551, 66050], 132174),
        ("Leonard R. Sendelsky", "D", [3818, 25688, 24703, 51552], 105761),
        ("Eugene J. Gillespie, Jr.", "I", [203, 863, 1040, 1937], 4043),
        ("Bill Campbell", "I", [65, 434, 635, 1478], 2612),
        ("Spencer Layman", "I", [45, 291, 1112, 516], 1964),
        ("John L. Kucek", "I", [30, 203, 256, 355], 844),
        ("Kevin Michael Criss", "I", [139, 145, 120, 280], 684),
    ]),
    8: (["ESSEX", "PASSAIC"], [
        ("Herbert C. Klein", "D", [45262, 51480], 96742),
        ("Joseph L. Bubba", "R", [39560, 45114], 84674),
        ("Gloria J. Kolodziej", "I", [1943, 14227], 16170),
        ("Thomas Caslander", "I", [2605, 311], 2916),
        ("Carmine O. Pellosie", "I", [450, 1685], 2135),
        ("Louis M. Stefanelli", "I", [734, 375], 1109),
        ("Rob Dominianni", "I", [747, 352], 1099),
        ("Jason Redrup", "I", [117, 275], 392),
        ("Gregory E. Dzula", "I", [173, 143], 316),
        ("Neal A. Gorfinkle", "I", [94, 181], 275),
    ]),
    9: (["BERGEN", "HUDSON"], [
        ("Robert G. Torricelli", "D", [117581, 21607], 139188),
        ("Patrick J. Roma", "R", [75961, 12218], 88179),
        ("Peter J. Russo", "I", [4037, 454], 4491),
        ("Gary Novosielski", "I", [1992, 265], 2257),
        ("Joseph D'Alessio", "I", [1015, 591], 1606),
        ("Herbert H. Shaw", "I", [931, 438], 1369),
        ("Daniel M. Karlan", "I", [650, 449], 1099),
        ("Shel Haas", "I", [474, 41], 515),
    ]),
    10: (["ESSEX", "HUDSON", "UNION"], [
        ("Donald M. Payne", "D", [76269, 8822, 32196], 117287),
        ("Alfred D. Palermo", "R", [8757, 3500, 17903], 30160),
        ("Roberto Caraballo", "I", [526, 373, 373], 1272),
        ("William T. Leonard", "I", [476, 155, 282], 913),
    ]),
    11: (["ESSEX", "MORRIS", "PASSAIC", "SOMERSET", "SUSSEX"], [
        ("Dean A. Gallo", "R", [21977, 133990, 1876, 16712, 13610], 188165),
        ("Ona Spiridellis", "D", [8904, 47262, 866, 7532, 4307], 68871),
        ("Richard S. Roth", "I", [161, 2864, 28, 157, 328], 3538),
        ("Barry J. Fitzpatrick", "I", [139, 1625, 40, 324, 999], 3127),
        ("David C. Karlen", "I", [690, 645, 38, 82, 427], 1882),
        ("Howard Safier", "I", [144, 991, 13, 482, 81], 1711),
        ("Richard E. Hrazanek", "I", [87, 857, 12, 139, 47], 1142),
    ]),
    12: (["HUNTERDON", "MERCER", "MIDDLESEX", "MONMOUTH", "SOMERSET"], [
        ("Dick Zimmer", "R", [36956, 29466, 28875, 64165, 14754], 174216),
        ("Frank Abate", "D", [9164, 18327, 19168, 32511, 3865], 83035),
        ("Carl J. Mayer", "I", [1673, 5016, 1838, 1908, 616], 11051),
        ("Carl Peters", "I", [310, 377, 301, 711, 207], 1906),
        ("Edward F. Eggert", "I", [161, 428, 431, 641, 143], 1804),
        ("Compton C. Pakenham", "I", [433, 62, 76, 159, 15], 745),
    ]),
    13: (["ESSEX", "HUDSON", "MIDDLESEX", "UNION"], [
        ("Robert Menendez", "D", [10104, 65071, 11937, 6558], 93670),
        ("Fred J. Theemling, Jr.", "R", [4253, 31213, 6722, 2341], 44529),
        ("Joseph D. Bonacci", "I", [468, 1435, 226, 234], 2363),
        ("Len Flynn", "I", [52, 1307, 148, 32], 1539),
        ("John E. Rummel", "I", [723, 660, 131, 11], 1525),
        ("Jane Harris", "I", [189, 1071, 87, 59], 1406),
        ("Donald K. Stoveken", "I", [82, 512, 65, 23], 682),
    ]),
}

# Clerk of the House, 1992 statistics, New Jersey (candidate totals by district)
CLERK = {
    1: [153525, 65123, 3761, 2641, 2163, 859],
    2: [132465, 98315, 2575, 2067, 1605],
    3: [151368, 94012, 2711, 2309, 1728, 915, 901, 749, 593, 512],
    4: [149095, 84514, 2984, 2137, 1630, 865],
    5: [196198, 67579, 6182, 2636, 994, 782],
    6: [118266, 100949, 2153, 1404, 1073, 951, 613, 411, 273],
    7: [132174, 105761, 4043, 2612, 1964, 844, 684],
    8: [96742, 84674, 16170, 2916, 2135, 1109, 1099, 392, 316, 275],
    9: [139188, 88179, 4491, 2257, 1606, 1369, 1099, 515],
    10: [117287, 30160, 1272, 913],
    11: [188165, 68871, 3538, 3127, 1882, 1711, 1142],
    12: [174216, 83035, 11051, 1906, 1804, 745],
    13: [93670, 44529, 2363, 1539, 1525, 1406, 682],
}

rows = []
for d, (counties, cands) in D.items():
    assert sorted(t for *_, t in cands) == sorted(CLERK[d]), (d, "totals differ from the Clerk")
    assert sum(p == "D" for _, p, _, _ in cands) == 1 and sum(p == "R" for _, p, _, _ in cands) == 1, d
    for name, party, votes, total in cands:
        assert len(votes) == len(counties), (d, name)
        assert sum(votes) == total, (d, name, sum(votes), total)
        for c, v in zip(counties, votes):
            rows.append(dict(year=1992, district=d, county=c, candidate=name, party_code=party, votes=v))
assert len({r["county"] for r in rows}) == 21, sorted({r["county"] for r in rows})
with open(OUT, "w", newline="") as f:
    w = csv.DictWriter(f, fieldnames=["year", "district", "county", "candidate", "party_code", "votes"])
    w.writeheader()
    w.writerows(rows)
print(f"{len(D)} districts, {len(rows)} candidate-county rows; every row ties to its printed total and the Clerk")
