"""New Jersey U.S. House 1994, county level, hand-transcribed.

Source: New Jersey Division of Elections, 'Official List - General Election Returns
for the Office of House of Representative, for election held November 8, 1994'
(R/data/county_house_files/new_jersey/1994-general-election-results-house.pdf,
from nj.gov/state/elections; image-only typescript, 13 pages, one page per
district: a column per county ('(part)' for split counties) and a TOTAL).

Read by eye from 300 dpi renders (pdftoppm -r 300 -gray). Checks (the script
stops on failure): every candidate's county votes add up to that candidate's
printed TOTAL, and every printed TOTAL equals the Clerk of the House
'Statistics of the Congressional Election of November 8, 1994'. All 21 counties
appear. No cell needed to be inferred from the checks.

Party: D for Democrat, R for Republican, I for every other designation
(printed 'Ind' in the Party column); each candidate's ballot designation is
kept in DATA for reference but not written to the CSV.

Output: R/data/county_house_files/new_jersey/new_jersey_house_county_1994.csv
(year, district, county, candidate, party_code, votes)
"""
import csv, os

HERE = os.path.dirname(os.path.abspath(__file__))
OUT = os.path.join(HERE, "../data/county_house_files/new_jersey/new_jersey_house_county_1994.csv")

# district: (counties, [(candidate, party_code, designation, [votes per county], printed TOTAL)])
DATA = {
    1: (["BURLINGTON", "CAMDEN", "GLOUCESTER"], [
        ("Robert E. Andrews", "D", "Democrat", [5409, 72625, 30121], 108155),
        ("James N. Hogan", "R", "Republican", [2214, 23759, 15532], 41505)]),
    2: (["ATLANTIC", "BURLINGTON", "CAPE MAY", "CUMBERLAND", "GLOUCESTER", "SALEM"], [
        ("Louis N. Magazzu", "D", "Democrat", [21155, 34, 10518, 10318, 7617, 6509], 56151),
        ("Frank A. LoBiondo", "R", "Republican", [33399, 234, 22551, 20381, 13621, 12380], 102566)]),
    3: (["BURLINGTON", "CAMDEN", "OCEAN"], [
        ("James B. Smith", "D", "Democratic", [22700, 11437, 20304], 54441),
        ("Jim Saxton", "R", "Republican", [49821, 19368, 46561], 115750),
        ("Arthur Fulvio Croce", "I", "Democracy in Action", [487, 232, 403], 1122),
        ("D. James Hill", "I", "United We Serve", [1724, 286, 1005], 3015)]),
    4: (["BURLINGTON", "MERCER", "MONMOUTH", "OCEAN"], [
        ("Ralph Walsh", "D", "Democrat", [6608, 16132, 6912, 19885], 49537),
        ("Christopher H. Smith", "R", "Republican", [13994, 30214, 19912, 45698], 109818),
        ("Arnold Kokans", "I", "Natural Law Party", [78, 146, 136, 473], 833),
        ("Leonard P. Marshall", "I", "NJ Conservative Party", [226, 450, 206, 697], 1579)]),
    5: (["BERGEN", "PASSAIC", "SUSSEX", "WARREN"], [
        ("Bill Auer", "D", "Democrat", [26563, 3955, 3914, 6843], 41275),
        ("Marge Roukema", "R", "Republican", [90263, 13096, 18036, 18569], 139964),
        ("William J. Leonard", "I", "Independent", [1077, 586, 931, 1152], 3746),
        ("Roger Bacon", "I", "Libertarian Party", [1232, 161, 745, 744], 2882),
        ("Helen Hamilton", "I", "Natural Law Party", [330, 47, 140, 121], 638)]),
    6: (["MIDDLESEX", "MONMOUTH"], [
        ("Frank Pallone, Jr.", "D", "Democrat", [48801, 40121], 88922),
        ("Mike Herson", "R", "Republican", [28622, 26665], 55287),
        ("Charles H. Dickson", "I", "Capitalist Party", [1500, 274], 1774),
        ("Richard Quinn", "I", "Natural Law Party", [215, 333], 548),
        ("Gary J. Rich", "I", "NJ Conservative Party", [435, 365], 800)]),
    7: (["ESSEX", "MIDDLESEX", "SOMERSET", "UNION"], [
        ("Karen Carroll", "D", "Democrat", [2845, 15111, 14794, 31481], 64231),
        ("Bob Franks", "R", "Republican", [5311, 17053, 24343, 52107], 98814),
        ("James J. Cleary", "I", "LaRouche Was Right", [38, 815, 639, 839], 2331),
        ("Claire Greene", "I", "Natural Law Party", [16, 119, 135, 211], 481)]),
    8: (["ESSEX", "PASSAIC"], [
        ("Herb Klein", "D", "Democrat", [31863, 36798], 68661),
        ("Bill Martini", "R", "Republican", [33107, 37387], 70494),
        ("Bernard George", "I", "Conservative", [814, 1399], 2213)]),
    9: (["BERGEN", "HUDSON"], [
        ("Robert G. Torricelli", "D", "Democrat", [83663, 16321], 99984),
        ("Peter J. Russo", "R", "Republican", [49764, 7887], 57651),
        ("Gregory Pason", "I", "Independent", [1233, 257], 1490),
        ("Kenneth Ebel", "I", "Natural Law Party", [553, 210], 763)]),
    10: (["ESSEX", "HUDSON", "UNION"], [
        ("Donald M. Payne", "D", "Democrat", [48107, 6692, 19823], 74622),
        ("Jim Ford", "R", "Republican", [7008, 2315, 12201], 21524),
        ("Rose Monyek", "I", "Inflation Fighting Housewife", [509, 173, 916], 1598),
        ("Maurice Williams", "I", "Socialist Workers Party", [295, 174, 155], 624)]),
    11: (["ESSEX", "MORRIS", "PASSAIC", "SOMERSET", "SUSSEX"], [
        ("Frank Herbert", "D", "Democrat", [8260, 33194, 714, 5020, 3023], 50211),
        ("Rodney P. Frelinghuysen", "R", "Republican", [14747, 90873, 1323, 11843, 9082], 127868),
        ("Stuart Bacha", "I", "Fascist Party", [108, 196, 5, 96, 31], 436),
        ("Mary Frueholz", "I", "LaRouche Was Right", [80, 780, 23, 107, 75], 1065)]),
    12: (["HUNTERDON", "MERCER", "MIDDLESEX", "MONMOUTH", "SOMERSET"], [
        ("Joseph D. Youssouf", "D", "Democrat", [6165, 14247, 13447, 19450, 2668], 55977),
        ("Dick Zimmer", "R", "Republican", [26511, 22908, 20920, 44584, 11016], 125939),
        ("Anthony M. Provenzano", "I", "NJ Conservative Party", [410, 440, 392, 1009, 113], 2364)]),
    13: (["ESSEX", "HUDSON", "MIDDLESEX", "UNION"], [
        ("Robert Menendez", "D", "Democrat", [6778, 49288, 7489, 4133], 67688),
        ("Fernando A. Alonso", "R", "Republican", [3671, 15460, 3559, 1381], 24071),
        ("Herbert H. Shaw", "I", "Politicians Are Crooks", [107, 923, 226, 63], 1319),
        ("Steven Marshall", "I", "Socialist Workers Party", [69, 746, 47, 33], 895),
        ("Frank J. Rubino, Jr.", "I", "We the People", [312, 511, 561, 110], 1494)]),
}

# Clerk of the House, Statistics of the Congressional Election of November 8, 1994 (New Jersey)
CLERK = {
    1: {"James N. Hogan": 41505, "Robert E. Andrews": 108155},
    2: {"Frank A. LoBiondo": 102566, "Louis N. Magazzu": 56151},
    3: {"Jim Saxton": 115750, "James B. Smith": 54441, "D. James Hill": 3015, "Arthur Fulvio Croce": 1122},
    4: {"Christopher H. Smith": 109818, "Ralph Walsh": 49537, "Leonard P. Marshall": 1579, "Arnold Kokans": 833},
    5: {"Marge Roukema": 139964, "Bill Auer": 41275, "William J. Leonard": 3746, "Roger Bacon": 2882, "Helen Hamilton": 638},
    6: {"Mike Herson": 55287, "Frank Pallone, Jr.": 88922, "Gary J. Rich": 800, "Richard Quinn": 548, "Charles H. Dickson": 1774},
    7: {"Bob Franks": 98814, "Karen Carroll": 64231, "Claire Greene": 481, "James J. Cleary": 2331},
    8: {"Bill Martini": 70494, "Herb Klein": 68661, "Bernard George": 2213},
    9: {"Peter J. Russo": 57651, "Robert G. Torricelli": 99984, "Gregory Pason": 1490, "Kenneth Ebel": 763},
    10: {"Jim Ford": 21524, "Donald M. Payne": 74622, "Maurice Williams": 624, "Rose Monyek": 1598},
    11: {"Rodney P. Frelinghuysen": 127868, "Frank Herbert": 50211, "Mary Frueholz": 1065, "Stuart Bacha": 436},
    12: {"Dick Zimmer": 125939, "Joseph D. Youssouf": 55977, "Anthony M. Provenzano": 2364},
    13: {"Fernando A. Alonso": 24071, "Robert Menendez": 67688, "Steven Marshall": 895, "Frank J. Rubino, Jr.": 1494, "Herbert H. Shaw": 1319},
}

NJ_COUNTIES = {"ATLANTIC", "BERGEN", "BURLINGTON", "CAMDEN", "CAPE MAY", "CUMBERLAND", "ESSEX", "GLOUCESTER", "HUDSON",
               "HUNTERDON", "MERCER", "MIDDLESEX", "MONMOUTH", "MORRIS", "OCEAN", "PASSAIC", "SALEM", "SOMERSET",
               "SUSSEX", "UNION", "WARREN"}

rows = []
for d, (counties, cands) in DATA.items():
    assert {c for c, *_ in cands} == set(CLERK[d]), d
    for name, party, _, votes, total in cands:
        assert len(votes) == len(counties), (d, name)
        assert sum(votes) == total, (d, name, sum(votes), total)
        assert total == CLERK[d][name], (d, name, total, CLERK[d][name])
        for county, v in zip(counties, votes):
            rows.append(dict(year=1994, district=d, county=county, candidate=name, party_code=party, votes=v))
assert len(DATA) == 13 and {r["county"] for r in rows} == NJ_COUNTIES
with open(OUT, "w", newline="") as f:
    w = csv.DictWriter(f, fieldnames=["year", "district", "county", "candidate", "party_code", "votes"])
    w.writeheader()
    w.writerows(rows)
print(f"{len(rows)} candidate-county rows, 13 districts, 21 counties; every row ties to its TOTAL and every TOTAL to the Clerk")
