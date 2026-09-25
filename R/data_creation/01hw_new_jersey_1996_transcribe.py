"""New Jersey U.S. House 1996, county level, hand-transcribed.

Source: New Jersey Division of Elections, 'Official List - General Election Returns
for the Office of House of Representatives - For Election Held November 5, 1996'
(printed 01/31/1997; 13 pages, one per district), image-only scan
R/data/county_house_files/new_jersey/1996-general-election-results-house.pdf
(from nj.gov/state/elections/election-information-1996.shtml). Read by eye from
300 dpi renders; tesseract was not used for values.

Checks (the script stops on any failure):
  * each candidate's county votes add up to that candidate's printed TOTAL;
  * every Democratic and Republican TOTAL equals the FEC's 'Federal Elections 96'
    (R/data/fec_official/federalelections96.pdf), and the independents' TOTALs
    equal the FEC figures as well (FEC_OTHER below);
  * all 21 counties appear.
The Clerk of the House 'Statistics of the ... Election of November 5, 1996' differs
by 1-22 votes for some nominees (e.g. Katz 83,890 vs 83,912 here); the FEC and this
Official List agree exactly, so the Official List is kept.

Party: Rep -> R, Dem -> D, Ind -> I. Split counties are marked '(part)' in the source.
Output: R/data/county_house_files/new_jersey/new_jersey_house_county_1996.csv
"""
import csv, os

HERE = os.path.dirname(os.path.abspath(__file__))
OUT = os.path.join(HERE, "../data/county_house_files/new_jersey/new_jersey_house_county_1996.csv")

# district: (counties, [(candidate, party, [votes per county], printed TOTAL)])
D = {
    1: (["BURLINGTON", "CAMDEN", "GLOUCESTER"], [
        ("Mel Suplee", "R", [2572, 26768, 14946], 44286),
        ("Robert E. Andrews", "D", [7227, 104968, 48220], 160415),
        ("Michael Edmondson", "I", [154, 1431, 1083], 2668),
        ("Patricia A. Bily", "I", [74, 1084, 715], 1873),
        ("Norman E. Wahner", "I", [112, 931, 450], 1493)]),
    2: (["ATLANTIC", "BURLINGTON", "CAPE MAY", "CUMBERLAND", "GLOUCESTER", "SALEM"], [
        ("Frank A. LoBiondo", "R", [46037, 230, 28845, 26192, 16582, 15244], 133130),
        ("Ruth Katz", "D", [31587, 75, 13185, 16202, 13273, 9590], 83912),
        ("Andrea Lippi", "I", [236, 3, 301, 140, 239, 165], 1084),
        ("David Rodger Headrick", "I", [299, 1, 150, 416, 278, 295], 1439),
        ("Judith Lee Azaren", "I", [281, 4, 130, 209, 405, 145], 1174)]),
    3: (["BURLINGTON", "CAMDEN", "OCEAN"], [
        ("Jim Saxton", "R", [69238, 26088, 62177], 157503),
        ("John Leonardi", "D", [33377, 15953, 32260], 81590),
        ("Eugene B. Ashworth", "I", [730, 73, 331], 1134),
        ("Agnes A. James", "I", [453, 207, 695], 1355),
        ("Janice Presser", "I", [1543, 567, 927], 3037),
        ("Ken Feduniewicz", "I", [190, 63, 406], 659)]),
    4: (["BURLINGTON", "MERCER", "MONMOUTH", "OCEAN"], [
        ("Christopher H. Smith", "R", [17719, 39251, 27758, 61676], 146404),
        ("Kevin John Meara", "D", [9094, 27875, 10169, 30427], 77565),
        ("Arnold Kokans", "I", [215, 197, 255, 444], 1111),
        ("J. Morgan Strong", "I", [208, 438, 440, 948], 2034),
        ("Robert Figueroa", "I", [448, 1128, 498, 926], 3000)]),
    5: (["BERGEN", "PASSAIC", "SUSSEX", "WARREN"], [
        ("Marge Roukema", "R", [116444, 17677, 23668, 23534], 181323),
        ("Bill Auer", "D", [39410, 6189, 6971, 10386], 62956),
        ("E. Gregory Kresge", "I", [53, 33, 753, 60], 899),
        ("Helen Hamilton", "I", [836, 136, 315, 391], 1678),
        ("Barry Childers", "I", [438, 96, 403, 329], 1266),
        ("Dan Karlan", "I", [662, 218, 674, 564], 2118),
        ("Lorraine L. La Neve", "I", [1380, 923, 1003, 787], 4093)]),
    6: (["MIDDLESEX", "MONMOUTH"], [
        ("Steven J. Corodemus", "R", [40034, 33368], 73402),
        ("Frank Pallone Jr.", "D", [68078, 56557], 124635),
        ("Richard Sorrentino", "I", [811, 698], 1509),
        ("Susan H. Normandin", "I", [489, 758], 1247),
        ("Stefanie C. Trice", "I", [416, 225], 641),
        ("Keith Quarles", "I", [1514, 530], 2044)]),
    7: (["ESSEX", "MIDDLESEX", "SOMERSET", "UNION"], [
        ("Bob Franks", "R", [6327, 23951, 34206, 64333], 128817),
        ("Larry Lerner", "D", [3937, 23113, 23529, 46704], 97283),
        ("Dorothy De Laura", "I", [62, 663, 1050, 2301], 4076),
        ("Robert G. Robertson", "I", [20, 168, 154, 354], 696),
        ("Nicholas W. Gentile", "I", [9, 202, 418, 1064], 1693)]),
    8: (["ESSEX", "PASSAIC"], [
        ("Bill Martini", "R", [41792, 50812], 92604),
        ("William J. Pascrell Jr.", "D", [38454, 60399], 98853),
        ("Jeffrey M. Levine", "I", [509, 1112], 1621)]),
    9: (["BERGEN", "HUDSON"], [
        ("Kathleen A. Donovan", "R", [77476, 11529], 89005),
        ("Steven R. Rothman", "D", [96130, 21516], 117646),
        ("Arthur B. Rosen", "I", [2205, 525], 2730),
        ("Leon Myerson", "I", [812, 737], 1549)]),
    10: (["ESSEX", "HUDSON", "UNION"], [
        ("Vanessa Williams", "R", [6958, 2877, 12251], 22086),
        ("Donald M. Payne", "D", [81578, 12576, 32972], 127126),
        ("Harley Tyler", "I", [140, 107, 945], 1192),
        ("Toni M. Jackson", "I", [163, 185, 308], 656)]),
    11: (["ESSEX", "MORRIS", "PASSAIC", "SOMERSET", "SUSSEX"], [
        ("Rodney P. Frelinghuysen", "R", [17810, 120199, 1580, 17016, 12486], 169091),
        ("Chris Evangel", "D", [10272, 54579, 989, 7780, 5122], 78742),
        ("Victoria S. Spruiell", "I", [65, 1315, 22, 150, 285], 1837),
        ("Ed De Mott", "I", [191, 1899, 76, 256, 448], 2870),
        ("Austin S. Lett", "I", [114, 2003, 25, 208, 268], 2618)]),
    12: (["HUNTERDON", "MERCER", "MIDDLESEX", "MONMOUTH", "SOMERSET"], [
        ("Mike Pappas", "R", [29527, 21067, 21253, 50847, 13117], 135811),
        ("David M. Del Vecchio", "D", [16858, 30086, 27710, 44061, 6879], 125594),
        ("Joseph M. Mercurio", "I", [619, 535, 249, 1163, 84], 2650),
        ("Philip G. Cenicola", "I", [276, 152, 147, 570, 66], 1211),
        ("Virginia A. Flynn", "I", [1014, 670, 973, 1023, 275], 3955)]),
    13: (["ESSEX", "HUDSON", "MIDDLESEX", "UNION"], [
        ("Carlos E. Munoz", "R", [2394, 17645, 3941, 1446], 25426),
        ("Robert Menendez", "D", [14758, 80388, 13076, 7235], 115457),
        ("Mike Buoncristiano", "I", [75, 1732, 247, 40], 2094),
        ("Herbert H. Shaw", "I", [157, 1644, 276, 59], 2136),
        ("William P. Estrada", "I", [59, 522, 69, 70], 720),
        ("Rupert Ravens", "I", [20, 329, 53, 235], 637)]),
}

# FEC 'Federal Elections 96' general-election totals: (district, party) -> votes for D/R nominees
FEC_DR = {
    (1, "R"): 44286, (1, "D"): 160415, (2, "R"): 133130, (2, "D"): 83912, (3, "R"): 157503, (3, "D"): 81590,
    (4, "R"): 146404, (4, "D"): 77565, (5, "R"): 181323, (5, "D"): 62956, (6, "R"): 73402, (6, "D"): 124635,
    (7, "R"): 128817, (7, "D"): 97283, (8, "R"): 92604, (8, "D"): 98853, (9, "R"): 89005, (9, "D"): 117646,
    (10, "R"): 22086, (10, "D"): 127126, (11, "R"): 169091, (11, "D"): 78742, (12, "R"): 135811, (12, "D"): 125594,
    (13, "R"): 25426, (13, "D"): 115457,
}
# FEC totals for independents (checked where the FEC text is legible)
FEC_OTHER = {
    "Patricia A. Bily": 1873, "Norman E. Wahner": 1493, "David Rodger Headrick": 1439, "Judith Lee Azaren": 1174,
    "Eugene B. Ashworth": 1134, "Robert Figueroa": 3000, "Arnold Kokans": 1111, "Lorraine L. La Neve": 4093,
    "Dan Karlan": 2118, "Helen Hamilton": 1678, "Barry Childers": 1266, "Richard Sorrentino": 1509,
    "Susan H. Normandin": 1247, "Dorothy De Laura": 4076, "Nicholas W. Gentile": 1693, "Jeffrey M. Levine": 1621,
    "Arthur B. Rosen": 2730, "Harley Tyler": 1192, "Victoria S. Spruiell": 1837, "Philip G. Cenicola": 1211,
    "William P. Estrada": 720, "Rupert Ravens": 637,
}

rows, counties = [], set()
for d, (cts, cands) in D.items():
    for name, party, votes, total in cands:
        assert len(votes) == len(cts), (d, name)
        assert sum(votes) == total, (d, name, sum(votes), total)
        if party in ("D", "R"):
            assert FEC_DR[(d, party)] == total, (d, name, total, FEC_DR[(d, party)])
        if name in FEC_OTHER:
            assert FEC_OTHER[name] == total, (d, name)
        for c, v in zip(cts, votes):
            rows.append(dict(year=1996, district=d, county=c, candidate=name, party_code=party, votes=v))
            counties.add(c)
    assert sum(1 for _, p, _, _ in cands if p == "D") <= 1 and sum(1 for _, p, _, _ in cands if p == "R") <= 1, d
assert len(D) == 13 and len(counties) == 21, (len(D), len(counties))
with open(OUT, "w", newline="") as f:
    w = csv.DictWriter(f, fieldnames=["year", "district", "county", "candidate", "party_code", "votes"])
    w.writeheader()
    w.writerows(rows)
print(f"1996: {len(D)} districts, {len(counties)} counties, {len(rows)} candidate-county rows; all checks passed")
