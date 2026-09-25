"""New Jersey U.S. House 1998, county level, hand-transcribed from the NJ Division of
Elections 'Official List - Candidate Returns for House of Representatives - For
November 1998 General Election' (dated 12-01-1998; pages 1-14 of 15, one or two
pages per district): R/data/county_house_files/new_jersey/1998-gen-elect-results-us--house.pdf
(image-only scan; rendered at 300 dpi and read by eye).

Each candidate block lists a tally per county (split counties marked '(part)') and a
printed Total. Checks (the script stops on any failure):
  * every candidate's county tallies add up to his or her printed Total;
  * every printed Total equals the Clerk of the House 'Statistics of the Congressional
    Election of November 3, 1998' (R/data/clerk_house_stats/1998Stat.htm);
  * all 21 counties appear.
Party as printed (Republican / Democratic / Independent); in the CSV D, R, else I
(D/R confirmed against the Clerk). No write-in tallies are printed.

Output: R/data/county_house_files/new_jersey/new_jersey_house_county_1998.csv
        (year,district,county,candidate,party_code,votes)
"""
import csv, html, os, re

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.join(HERE, "..", "..")

# district: [(candidate, printed party, [(county, votes), ...], printed Total, Clerk total as read)]; the Clerk totals are also parsed from the page below
D = {
    1: [
        ("Ronald L. Richards", "Republican", [("BURLINGTON", 1797), ("CAMDEN", 16144), ("GLOUCESTER", 9914)], 27855, 27855),
        ("Robert E. Andrews", "Democratic", [("BURLINGTON", 5066), ("CAMDEN", 57593), ("GLOUCESTER", 27620)], 90279, 90279),
        ("Edward \"Rob\" Forchion", "Independent", [("BURLINGTON", 104), ("CAMDEN", 720), ("GLOUCESTER", 433)], 1257, 1257),
        ("David E. West, Jr.", "Independent", [("BURLINGTON", 89), ("CAMDEN", 1033), ("GLOUCESTER", 562)], 1684, 1684),
        ("Joseph W. Stockman", "Independent", [("BURLINGTON", 76), ("CAMDEN", 670), ("GLOUCESTER", 578)], 1324, 1324),
        ("James E. Barber", "Independent", [("BURLINGTON", 29), ("CAMDEN", 615), ("GLOUCESTER", 299)], 943, 943),
    ],
    2: [
        ("Frank A. LoBiondo", "Republican", [("ATLANTIC", 30693), ("BURLINGTON", 171), ("CAPE MAY", 21395), ("CUMBERLAND", 17463), ("GLOUCESTER", 11465), ("SALEM", 12061)], 93248, 93248),
        ("Derek Hunsberger", "Democratic", [("ATLANTIC", 15679), ("BURLINGTON", 54), ("CAPE MAY", 6677), ("CUMBERLAND", 8953), ("GLOUCESTER", 6973), ("SALEM", 5227)], 43563, 43563),
        ("Glenn Campbell", "Independent", [("ATLANTIC", 875), ("BURLINGTON", 9), ("CAPE MAY", 525), ("CUMBERLAND", 502), ("GLOUCESTER", 567), ("SALEM", 477)], 2955, 2955),
        ("Mary A. Whittam", "Independent", [("ATLANTIC", 571), ("BURLINGTON", 2), ("CAPE MAY", 346), ("CUMBERLAND", 214), ("GLOUCESTER", 349), ("SALEM", 266)], 1748, 1748),
    ],
    3: [
        ("Jim Saxton", "Republican", [("BURLINGTON", 44824), ("CAMDEN", 13902), ("OCEAN", 38782)], 97508, 97508),
        ("Steven J. Polansky", "Democratic", [("BURLINGTON", 25976), ("CAMDEN", 10190), ("OCEAN", 19082)], 55248, 55248),
        ("Ken Feduniewicz", "Independent", [("BURLINGTON", 172), ("CAMDEN", 24), ("OCEAN", 89)], 285, 285),
        ("Janice Presser", "Independent", [("BURLINGTON", 1481), ("CAMDEN", 449), ("OCEAN", 597)], 2527, 2527),
        ("James Pircher", "Independent", [("BURLINGTON", 350), ("CAMDEN", 56), ("OCEAN", 202)], 608, 608),
        ("Norman E. Wahner", "Independent", [("BURLINGTON", 346), ("CAMDEN", 107), ("OCEAN", 610)], 1063, 1063),
    ],
    4: [
        ("Christopher H. Smith", "Republican", [("BURLINGTON", 12487), ("MERCER", 22860), ("MONMOUTH", 17777), ("OCEAN", 39867)], 92991, 92991),
        ("Larry Schneider", "Democratic", [("BURLINGTON", 7701), ("MERCER", 18200), ("MONMOUTH", 7209), ("OCEAN", 19171)], 52281, 52281),
        ("Morgan Strong", "Independent", [("BURLINGTON", 144), ("MERCER", 249), ("MONMOUTH", 351), ("OCEAN", 754)], 1498, 1498),
        ("Keith Quarles", "Independent", [("BURLINGTON", 353), ("MERCER", 580), ("MONMOUTH", 385), ("OCEAN", 435)], 1753, 1753),
        ("Nick Mellis", "Independent", [("BURLINGTON", 179), ("MERCER", 358), ("MONMOUTH", 257), ("OCEAN", 260)], 1054, 1054),
    ],
    5: [
        ("Marge Roukema", "Republican", [("BERGEN", 66912), ("PASSAIC", 11359), ("SUSSEX", 12833), ("WARREN", 15200)], 106304, 106304),
        ("Mike Schneider", "Democratic", [("BERGEN", 37557), ("PASSAIC", 4530), ("SUSSEX", 5193), ("WARREN", 8207)], 55487, 55487),
        ("Thomas W. Wright", "Independent", [("BERGEN", 847), ("PASSAIC", 228), ("SUSSEX", 623), ("WARREN", 697)], 2395, 2395),
        ("William Weightman", "Independent", [("BERGEN", 521), ("PASSAIC", 247), ("SUSSEX", 511), ("WARREN", 349)], 1628, 1628),
        ("Helen Hamilton", "Independent", [("BERGEN", 644), ("PASSAIC", 97), ("SUSSEX", 127), ("WARREN", 136)], 1004, 1004),
    ],
    6: [
        ("Michael Ferguson", "Republican", [("MIDDLESEX", 27936), ("MONMOUTH", 27244)], 55180, 55180),
        ("Frank Pallone, Jr.", "Democratic", [("MIDDLESEX", 42969), ("MONMOUTH", 35133)], 78102, 78102),
        ("Steve Nagle", "Independent", [("MIDDLESEX", 778), ("MONMOUTH", 484)], 1262, 1262),
        ("Leonard P. Marshall", "Independent", [("MIDDLESEX", 729), ("MONMOUTH", 448)], 1177, 1177),
        ("Carl J. Mayer", "Independent", [("MIDDLESEX", 890), ("MONMOUTH", 401)], 1291, 1291),
    ],
    7: [
        ("Bob Franks", "Republican", [("ESSEX", 4183), ("MIDDLESEX", 12916), ("SOMERSET", 20567), ("UNION", 40085)], 77751, 77751),
        ("Maryanne S. Connelly", "Democratic", [("ESSEX", 2410), ("MIDDLESEX", 14755), ("SOMERSET", 15860), ("UNION", 32751)], 65776, 65776),
        ("Darren Young", "Independent", [("ESSEX", 69), ("MIDDLESEX", 397), ("SOMERSET", 424), ("UNION", 618)], 1508, 1508),
        ("Richard C. Martin", "Independent", [("ESSEX", 29), ("MIDDLESEX", 245), ("SOMERSET", 441), ("UNION", 2292)], 3007, 3007),
    ],
    8: [
        ("Matthew J. Kirnan", "Republican", [("ESSEX", 21330), ("PASSAIC", 24959)], 46289, 46289),
        ("Bill Pascrell, Jr.", "Democratic", [("ESSEX", 32308), ("PASSAIC", 48760)], 81068, 81068),
        ("Thomas Paine Caslander", "Independent", [("ESSEX", 245), ("PASSAIC", 380)], 625, 625),
        ("Bernard George", "Independent", [("ESSEX", 114), ("PASSAIC", 608)], 722, 722),
        ("Jose L. Aravena", "Independent", [("ESSEX", 150), ("PASSAIC", 168)], 318, 318),
        ("Stephen Spinosa", "Independent", [("ESSEX", 352), ("PASSAIC", 410)], 762, 762),
        ("Jeffrey Levine", "Independent", [("ESSEX", 634), ("PASSAIC", 170)], 804, 804),
    ],
    9: [
        ("Steve Lonegan", "Republican", [("BERGEN", 41653), ("HUDSON", 6164)], 47817, 47817),
        ("Steven R. Rothman", "Democratic", [("BERGEN", 76320), ("HUDSON", 15010)], 91330, 91330),
        ("Michael Perrone, Jr.", "Independent", [("BERGEN", 889), ("HUDSON", 460)], 1349, 1349),
        ("Michael W. Koontz", "Independent", [("BERGEN", 578), ("HUDSON", 108)], 686, 686),
        ("Kenneth Ebel", "Independent", [("BERGEN", 206), ("HUDSON", 71)], 277, 277),
    ],
    10: [
        ("William Stanley Wnuck", "Republican", [("ESSEX", 3608), ("HUDSON", 1002), ("UNION", 6068)], 10678, 10678),
        ("Donald M. Payne", "Democratic", [("ESSEX", 54336), ("HUDSON", 6487), ("UNION", 21421)], 82244, 82244),
        ("Maurice Williams", "Independent", [("ESSEX", 1940), ("HUDSON", 179), ("UNION", 160)], 2279, 2279),
        ("Richard J. Pezzullo", "Independent", [("ESSEX", 732), ("HUDSON", 178), ("UNION", 2383)], 3293, 3293),
    ],
    11: [
        ("Rodney P. Frelinghuysen", "Republican", [("ESSEX", 13748), ("MORRIS", 68792), ("PASSAIC", 1216), ("SOMERSET", 10429), ("SUSSEX", 6725)], 100910, 100910),
        ("John P. Scollo", "Democratic", [("ESSEX", 5698), ("MORRIS", 29577), ("PASSAIC", 893), ("SOMERSET", 5581), ("SUSSEX", 2411)], 44160, 44160),
        ("Austin S. Lett", "Independent", [("ESSEX", 157), ("MORRIS", 1183), ("PASSAIC", 18), ("SOMERSET", 162), ("SUSSEX", 217)], 1737, 1737),
        ("Agnes A. James", "Independent", [("ESSEX", 89), ("MORRIS", 1075), ("PASSAIC", 14), ("SOMERSET", 102), ("SUSSEX", 129)], 1409, 1409),
        ("Stephen A. Bauer", "Independent", [("ESSEX", 107), ("MORRIS", 368), ("PASSAIC", 25), ("SOMERSET", 143), ("SUSSEX", 112)], 755, 755),
    ],
    12: [
        ("Michael Pappas", "Republican", [("HUNTERDON", 19021), ("MERCER", 14188), ("MIDDLESEX", 13991), ("MONMOUTH", 31814), ("SOMERSET", 8207)], 87221, 87221),
        ("Rush Holt", "Democratic", [("HUNTERDON", 11699), ("MERCER", 23240), ("MIDDLESEX", 21925), ("MONMOUTH", 30222), ("SOMERSET", 5442)], 92528, 92528),
        ("Joseph A. Siano", "Independent", [("HUNTERDON", 514), ("MERCER", 313), ("MIDDLESEX", 397), ("MONMOUTH", 746), ("SOMERSET", 155)], 2125, 2125),
        ("Beverly Kidder", "Independent", [("HUNTERDON", 203), ("MERCER", 193), ("MIDDLESEX", 135), ("MONMOUTH", 137), ("SOMERSET", 81)], 749, 749),
        ("Mary Jo Christian", "Independent", [("HUNTERDON", 98), ("MERCER", 196), ("MIDDLESEX", 92), ("MONMOUTH", 175), ("SOMERSET", 17)], 578, 578),
        ("Madelyn R. Hoffman", "Independent", [("HUNTERDON", 354), ("MERCER", 275), ("MIDDLESEX", 237), ("MONMOUTH", 477), ("SOMERSET", 66)], 1409, 1409),
    ],
    13: [
        ("Theresa De Leon", "Republican", [("ESSEX", 2486), ("HUDSON", 8681), ("MIDDLESEX", 2751), ("UNION", 697)], 14615, 14615),
        ("Robert Menendez", "Democratic", [("ESSEX", 7811), ("HUDSON", 49287), ("MIDDLESEX", 8289), ("UNION", 4921)], 70308, 70308),
        ("Susan Anmuth", "Independent", [("ESSEX", 337), ("HUDSON", 288), ("MIDDLESEX", 105), ("UNION", 22)], 752, 752),
        ("Richard G. Rivera", "Independent", [("ESSEX", 167), ("HUDSON", 525), ("MIDDLESEX", 61), ("UNION", 119)], 872, 872),
        ("Richard S. Hester, Sr.", "Independent", [("ESSEX", 155), ("HUDSON", 940), ("MIDDLESEX", 134), ("UNION", 47)], 1276, 1276),
    ],
}

NJ_COUNTIES = {"ATLANTIC", "BERGEN", "BURLINGTON", "CAMDEN", "CAPE MAY", "CUMBERLAND", "ESSEX", "GLOUCESTER", "HUDSON", "HUNTERDON", "MERCER",
               "MIDDLESEX", "MONMOUTH", "MORRIS", "OCEAN", "PASSAIC", "SALEM", "SOMERSET", "SUSSEX", "UNION", "WARREN"}
PARTY = {"Democratic": "D", "Republican": "R"}

# Clerk totals parsed from the cached page (not typed): '<n>. Name, Party votes' per district, New Jersey section
_t = re.sub(r"<[^>]+>", " ", open(os.path.join(ROOT, "R", "data", "clerk_house_stats", "1998Stat.htm"), encoding="latin-1").read())
_t = re.sub(r"\s+", " ", html.unescape(_t).replace("\xa0", " "))
_nj = _t[[m.start() for m in re.finditer("NEW JERSEY For United States Representative", _t)][0]:]
_nj = _nj[:_nj.find("Recapitulation")]
CLERK = {}
_d = None
for tok in re.split(r"(?<![\d,])(\d{1,2})\. (?=[A-Z])", _nj)[1:]:
    if re.fullmatch(r"\d{1,2}", tok):
        _d = int(tok); continue
    for name, votes in re.findall(r"([^,]+?(?:, (?:Jr|Sr)\.)?), (?:Republican|Democrat|Independent) ([\d,]+)", tok):
        CLERK[(_d, name.strip().split()[-1].rstrip(".").lower() if not name.strip().endswith(("Jr.", "Sr.")) else name.strip().split()[-2].rstrip(",").lower())] = int(votes.replace(",", ""))

def clerk_total(d, name):
    last = [w for w in re.sub(r"[\"',.]", " ", name).split() if w not in ("Jr", "Sr")][-1].lower()
    return CLERK[(d, last)]

rows = []
for d, cands in D.items():
    counties = [c for c, _ in cands[0][2]]
    for name, party, tallies, total, clerk in cands:
        assert [c for c, _ in tallies] == counties, (d, name)                  # same county list for every candidate in a district
        assert sum(v for _, v in tallies) == total, (d, name, sum(v for _, v in tallies), total)
        assert total == clerk == clerk_total(d, name), (d, name, total, clerk, clerk_total(d, name))
        for c, v in tallies:
            assert c in NJ_COUNTIES, c
            rows.append(dict(year=1998, district=d, county=c, candidate=name, party_code=PARTY.get(party, "I"), votes=v))
    assert sum(1 for _, p, *_ in cands if p == "Democratic") == 1 and sum(1 for _, p, *_ in cands if p == "Republican") == 1, d
assert len(CLERK) == sum(len(c) for c in D.values()), (len(CLERK), sum(len(c) for c in D.values()))
assert len(D) == 13 and {r["county"] for r in rows} == NJ_COUNTIES

out = os.path.join(ROOT, "R", "data", "county_house_files", "new_jersey", "new_jersey_house_county_1998.csv")
with open(out, "w", newline="") as f:
    w = csv.DictWriter(f, fieldnames=["year", "district", "county", "candidate", "party_code", "votes"])
    w.writeheader(); w.writerows(rows)
print(f"1998: {len(D)} districts, {len(rows)} candidate-county rows, {len({r['county'] for r in rows})} counties; "
      f"every candidate ties to its printed Total and the Clerk; votes {sum(r['votes'] for r in rows):,}")
