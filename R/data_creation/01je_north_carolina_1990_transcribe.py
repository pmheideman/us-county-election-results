"""North Carolina U.S. House, November 6, 1990 general election, county level.

Source: North Carolina Manual 1991-1992 (NC Department of the Secretary of State), 'Abstracts of Votes and Election Results: General Elections,
1986-1990', printed pp. 930-933 (Internet Archive item northcarolinaman19911992nort, leaves n954-n957; also https://digital.ncdcr.gov/Documents/Detail/
north-carolina-manual-1991-1992/4384573). One table per congressional district (CD1-11), county rows ('(Part)' = county split between districts),
columns for 1990, 1988 and 1986; only the 1990 (D) and (R) columns are used. Page images: R/data/county_house_files/north_carolina/leads/
nc_manual_1991_p930..p933_house_1986_1990.png (found by a source-search agent, 2026-09-24); values read by eye from full-resolution crops.

Checks (stop on failure): each candidate column sums to the printed district Totals row; every total equals the FEC 1990 results
(R/data/fec_official/fec1990_house.csv; CD2 Valentine and CD8 Hefner are missing from that csv and are taken from 'Federal Elections 90',
R/data/fec_official/federalelections90.pdf: 130,979 and 98,700); all 100 counties appear.

Minor-candidate check: the FEC lists only the Democratic and Republican candidates in every one of the 11 districts, and in every district D + R
equals the FEC 'Total Votes' - so the Manual's two columns are the complete vote; no candidate is missing.

Output: R/data/county_house_files/north_carolina/north_carolina_house_county_1990.csv (year, district, county, candidate, party_code, votes)
"""
import csv, os

HERE = os.path.dirname(os.path.abspath(__file__))
OUT = os.path.join(HERE, "../data/county_house_files/north_carolina/north_carolina_house_county_1990.csv")

# district: ((D name, R name), [(county, D, R)], printed totals (D, R))
DATA = {
    1: (("Walter B. Jones", "Howard D. Moye"), [
        ("BEAUFORT", 6445, 5861), ("BERTIE", 3937, 1258), ("CAMDEN", 1325, 689), ("CARTERET", 9047, 7329), ("CHOWAN", 2075, 1250),
        ("CRAVEN", 11531, 7382), ("CURRITUCK", 2215, 1414), ("DARE", 4671, 3050), ("GATES", 1969, 629), ("GREENE", 3609, 1164),
        ("HERTFORD", 4770, 1402), ("HYDE", 1130, 576), ("LENOIR", 9673, 5766), ("MARTIN", 4674, 1733), ("NORTHAMPTON", 5258, 1209),
        ("PAMLICO", 2979, 1479), ("PASQUOTANK", 4253, 2680), ("PERQUIMANS", 1800, 1116), ("PITT", 20913, 9748), ("TYRRELL", 851, 379),
        ("WASHINGTON", 2707, 1412)], (105832, 57526)),
    2: (("I.T. (Tim) Valentine, Jr.", "Hal C. Sharpe"), [
        ("CASWELL", 4315, 1717), ("DURHAM", 45067, 15041), ("EDGECOMBE", 14777, 3289), ("GRANVILLE", 8672, 1771), ("HALIFAX", 11896, 2915),
        ("JOHNSTON", 871, 401), ("NASH", 14286, 8612), ("PERSON", 5215, 1811), ("VANCE", 8955, 2085), ("WARREN", 5594, 884),
        ("WILSON", 11331, 5737)], (130979, 44263)),
    3: (("Martin Lancaster", "Don Davis"), [
        ("BLADEN", 5620, 1834), ("DUPLIN", 8011, 3773), ("HARNETT", 7850, 8434), ("JOHNSTON", 12729, 9635), ("JONES", 2024, 936),
        ("LEE", 5074, 4325), ("MOORE", 1678, 1318), ("ONSLOW", 11333, 7114), ("PENDER", 6011, 3073), ("SAMPSON", 9629, 7600),
        ("WAYNE", 13971, 9563)], (83930, 57605)),
    4: (("David E. Price", "John Carrington"), [
        ("CHATHAM", 9252, 5669), ("FRANKLIN", 6898, 4675), ("ORANGE", 26824, 10195), ("RANDOLPH", 10051, 19273), ("WAKE", 86371, 60849)],
        (139396, 100661)),
    5: (("Steve Neal", "Ken Bell"), [
        ("ALEXANDER", 5441, 7062), ("ALLEGHANY", 2735, 1465), ("ASHE", 5122, 4635), ("FORSYTH", 54255, 35334), ("ROCKINGHAM", 16730, 7049),
        ("STOKES", 8671, 5921), ("SURRY", 10468, 7220), ("WILKES", 10392, 10061)], (113814, 78747)),
    6: (("Helen R. Allegrone", "Howard Coble"), [
        ("ALAMANCE", 9911, 24895), ("DAVIDSON", 10971, 27206), ("GUILFORD", 42031, 73291)], (62913, 125392)),
    7: (("Charles G. Rose, III", "Robert C. Anderson"), [
        ("BRUNSWICK", 9872, 7164), ("COLUMBUS", 12351, 3933), ("CUMBERLAND", 32128, 18067), ("NEW HANOVER", 20694, 15102), ("ROBESON", 19901, 5415)],
        (94946, 49681)),
    8: (("W.G. (Bill) Hefner", "Ted Blanton"), [
        ("ANSON", 5185, 1523), ("CABARRUS", 17871, 14334), ("DAVIE", 4006, 6021), ("HOKE", 3505, 1169), ("MONTGOMERY", 4222, 3048),
        ("MOORE", 7427, 9804), ("RICHMOND", 9323, 3295), ("ROWAN", 16896, 15641), ("SCOTLAND", 3631, 1559), ("STANLY", 10073, 8196),
        ("UNION", 13666, 10978), ("YADKIN", 2895, 5284)], (98700, 80852)),
    9: (("David P. McKnight", "J. Alex McMillan"), [
        ("IREDELL", 11060, 18454), ("LINCOLN", 6308, 11929), ("MECKLENBURG", 63073, 100407), ("YADKIN", 361, 1146)], (80802, 131936)),
    10: (("Daniel R. Green, Jr.", "T. Cass Ballenger"), [
        ("AVERY", 868, 2969), ("BURKE", 10415, 13992), ("CALDWELL", 7549, 13190), ("CATAWBA", 14223, 24408), ("CLEVELAND", 10870, 12871),
        ("GASTON", 15474, 30725), ("WATAUGA", 6311, 8245)], (65710, 106400)),
    11: (("James M. Clarke", "Charles H. Taylor"), [
        ("AVERY", 378, 1267), ("BUNCOMBE", 31926, 27379), ("CHEROKEE", 3120, 3460), ("CLAY", 1823, 1964), ("GRAHAM", 1529, 2094),
        ("HAYWOOD", 8481, 7506), ("HENDERSON", 10144, 14139), ("JACKSON", 4699, 4252), ("MACON", 4266, 4649), ("MADISON", 4222, 3363),
        ("MCDOWELL", 5025, 5333), ("MITCHELL", 1501, 3956), ("POLK", 2415, 2934), ("RUTHERFORD", 8441, 8277), ("SWAIN", 1729, 2055),
        ("TRANSYLVANIA", 5653, 5018), ("YANCEY", 3966, 4345)], (99318, 101991)),
}
FEC = {1: (105832, 57526), 2: (130979, 44263), 3: (83930, 57605), 4: (139396, 100661), 5: (113814, 78747), 6: (62913, 125392),
       7: (94946, 49681), 8: (98700, 80852), 9: (80802, 131936), 10: (65710, 106400), 11: (99318, 101991)}
FEC_DISTRICT_TOTAL = {1: 163358, 2: 175242, 3: 141535, 4: 240057, 5: 192561, 6: 188305, 7: 144627, 8: 179552, 9: 212738, 10: 172110, 11: 201309}

# cross-check the FEC figures typed above against the project's parsed FEC csv where it has them
fec_csv = {}
for r in csv.DictReader(open(os.path.join(HERE, "../data/fec_official/fec1990_house.csv"))):
    if r["state_po"] == "NC" and r["district"]:
        if r["is_total"] == "True": assert int(float(r["total"])) == FEC_DISTRICT_TOTAL[int(r["district"])], r
        else: fec_csv.setdefault(int(r["district"]), []).append(int(float(r["votes"])))
for d, vs in fec_csv.items():
    assert all(v in FEC[d] for v in vs), (d, vs)

rows, counties = [], set()
for d, ((dn, rn), cty, (pd, pr)) in DATA.items():
    assert sum(c[1] for c in cty) == pd and sum(c[2] for c in cty) == pr, ("printed totals", d)
    assert (pd, pr) == FEC[d], ("FEC", d)
    assert pd + pr == FEC_DISTRICT_TOTAL[d], ("minor candidates", d)
    for c, dv, rv in cty:
        counties.add(c)
        rows += [(1990, d, c, dn, "D", dv), (1990, d, c, rn, "R", rv)]
assert len(counties) == 100, len(counties)
with open(OUT, "w", newline="") as f:
    w = csv.writer(f); w.writerow(["year", "district", "county", "candidate", "party_code", "votes"]); w.writerows(rows)
print(f"NC 1990: 11 districts, {len(counties)} counties, {len(rows)} rows; columns tie to the printed totals and the FEC; no minor candidates")
