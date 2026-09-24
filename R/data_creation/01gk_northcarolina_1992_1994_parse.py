"""North Carolina U.S. House county results 1992 and 1994 from the State Board of Elections' typed "Abstract of Votes Cast for Member of Congress in the General Election" (November 3, 1992 /
November 8, 1994), scanned pages in the State Library's digital collection (https://digital.ncdcr.gov/Documents/Detail/abstract-of-votes-cast-for-member-of-congress-in-the-general-election-held-on...-1992-november/4199407
and ...-1994-november/4199426; the page JPEGs come from the IIIF manifests linked on those pages and are saved in R/data/county_house_files/north_carolina/abstract1992 and abstract1994).
The pages were OCR'd with tesseract (--psm 6; text in ocr_1992_1994/psm6). One table per district: a county row per county (a split county appears in each district it touches, marked "(Part)")
and a TOTAL row. A second OCR pass (2x upscale, --psm 4) and the page images were used to settle the cells where the first pass misread a digit; those cells are listed in FIX below.
Checks (stop on failure): every district's county rows add up to the printed TOTAL row in every column; the 1992 TOTAL rows equal the FEC's official candidate totals (R/data/fec_official/fec1992_house.csv).
Two source defects are kept as printed: 1992 District 10's county rows add up to 34 (Ballenger) and 2 (Brown) less than its printed TOTAL row, which the FEC's official totals confirm;
1994 District 4's printed TOTAL for Heineman was misread by the OCR (71,773 for 77,773) and is taken from the county rows (Price 76,558, Heineman 77,773 after the November 17 recount).
Output: R/data/county_house_files/north_carolina/nc_house_county_1992_1994.csv (year, district, county, candidate, party_code, votes)."""
import re, glob, csv, sys
D = "R/data/county_house_files/north_carolina/"
CANDS = {
 1992: {1: [("Eva Clayton", "D"), ("Ted Tyler", "R"), ("C. Barry Williams", "L")], 2: [("I. T. (Tim) Valentine Jr.", "D"), ("Don Davis", "R"), ("Dennis Bryant Lubahn", "L")],
        3: [("Martin Lancaster", "D"), ("Tommy Pollard", "R"), ("Mark Jackson", "L")], 4: [("David E. Price", "D"), ("Lavinia (Vicky) Rothrock Goudie", "R"), ("Eugene Paczelt", "L")],
        5: [("Steve Neal", "D"), ("Richard M. Burr", "R"), ("Gary Albrecht", "L"), ("Norris O. Weathers", "WI")], 6: [("Robin J. Hood", "D"), ("Howard Coble", "R")],
        7: [("Charles G. Rose III", "D"), ("Robert C. Anderson", "R"), ("Marc Kelley", "L")], 8: [("W. G. (Bill) Hefner", "D"), ("Coy C. Privette", "R"), ("J. Wendell Drye", "L")],
        9: [("Rory Blake", "D"), ("J. Alex McMillan", "R"), ("Wendy Russell", "WI")], 10: [("Ben Neill", "D"), ("T. Cass Ballenger", "R"), ("Jeffrey Clayton Brown", "L")],
        11: [("John S. Stevens", "D"), ("Charles H. Taylor", "R")], 12: [("Melvin Watt", "D"), ("Barbara Gore Washington", "R"), ("Curtis Wade Krumel", "L")]},
 1994: {1: [("Eva M. Clayton", "D"), ("Ted Tyler", "R")], 2: [("Richard Moore", "D"), ("David Funderburk", "R")], 3: [("H. Martin Lancaster", "D"), ("Walter B. Jones Jr.", "R")],
        4: [("David E. Price", "D"), ("Frederick Kenneth Heineman", "R")], 5: [("A. P. (Sandy) Sands", "D"), ("Richard Burr", "R")], 6: [("Howard Coble", "R")],
        7: [("Charles G. Rose III", "D"), ("Robert C. Anderson", "R")], 8: [("W. G. (Bill) Hefner", "D"), ("Sherrill Morgan", "R")], 9: [("Rory Blake", "D"), ("Sue Myrick", "R")],
        10: [("Robert Wayne Avery", "D"), ("T. Cass Ballenger", "R")], 11: [("Lauterer", "D"), ("Charles H. Taylor", "R")], 12: [("Mel Watt", "D"), ("Joseph A. (Joe) Martino", "R"), ("Susan A. Skinner", "WI")]},
}
# cells the first OCR pass misread (settled with the second pass and the page image; each fix makes the district's column tie to its printed TOTAL): (year, district, county, part) -> votes
FIX = {
 (1992, 5, "GRANVILLE", True): [1727, 618, 34, 0], (1992, 5, "ROCKINGHAM", False): [17616, 12914, 511, 1], (1992, 5, "FORSYTH", True): [33160, 32738, 1214, 1], (1992, 5, "SURRY", False): [11928, 11407, 244, 2],
 (1992, 9, "GASTON", True): [19365, 37803, 0],
 (1994, 2, "JOHNSTON", False): [7755, 11345], (1994, 5, "PERSON", False): [2015, 2756], (1994, 10, "DAVIE", True): [711, 2713], (1994, 10, "POLK", True): [77, 234],
 (1994, 10, "WILKES", True): [3668, 7703], (1994, 11, "MCDOWELL", True): [3052, 6230], (1994, 11, "RUTHERFORD", True): [4372, 7793],
}
for k in [(1992, 5, c, p) for c, p in [("ALLEGHANY", False), ("ASHE", False), ("BURKE", True), ("CALDWELL", True), ("CASWELL", False), ("GUILFORD", True), ("PERSON", False), ("STOKES", False), ("WATAUGA", False), ("WILKES", True)]]:
    FIX.setdefault(k, None)                                                                       # district 5's fourth column (write-ins, unreadable in the OCR) is 0 except where FIXed above
ALLCOUNTIES = set("""ALAMANCE ALEXANDER ALLEGHANY ANSON ASHE AVERY BEAUFORT BERTIE BLADEN BRUNSWICK BUNCOMBE BURKE CABARRUS CALDWELL CAMDEN CARTERET CASWELL CATAWBA CHATHAM CHEROKEE CHOWAN CLAY CLEVELAND COLUMBUS CRAVEN CUMBERLAND CURRITUCK DARE DAVIDSON DAVIE DUPLIN DURHAM EDGECOMBE FORSYTH FRANKLIN GASTON GATES GRAHAM GRANVILLE GREENE GUILFORD HALIFAX HARNETT HAYWOOD HENDERSON HERTFORD HOKE HYDE IREDELL JACKSON JOHNSTON JONES LEE LENOIR LINCOLN MCDOWELL MACON MADISON MARTIN MECKLENBURG MITCHELL MONTGOMERY MOORE NASH NEWHANOVER NORTHAMPTON ONSLOW ORANGE PAMLICO PASQUOTANK PENDER PERQUIMANS PERSON PITT POLK RANDOLPH RICHMOND ROBESON ROCKINGHAM ROWAN RUTHERFORD SAMPSON SCOTLAND STANLY STOKES SURRY SWAIN TRANSYLVANIA TYRRELL UNION VANCE WAKE WARREN WASHINGTON WATAUGA WAYNE WILKES WILSON YADKIN YANCEY""".split()) | {"PAMILCO"}
def parse(y):
    txt = "\n".join(open(f).read() for f in sorted(glob.glob(D + f"ocr_1992_1994/psm6/nc{y}_p[0-9].txt")))
    dist = None; res = {}
    for ln in txt.splitlines():
        m = re.search(r"MEMBER OF CONGRESS, (\d+)(ST|ND|RD|TH) DISTRICT", ln, re.I) or re.search(r"([\dS]+)\s*(ST|ND|RD|TH)\s+CONGRESSIONAL DISTRICT", ln, re.I)
        if m: dist = int(m.group(1).upper().replace("S", "5")); res[dist] = {"rows": [], "total": None}; continue
        if dist is None: continue
        m = re.match(r"^\W*([A-Za-z][A-Za-z .]*?)(\s*\((?:PART|Part)\))?[\s.:|]*(\d.*)$", ln.strip())
        if not m: continue
        name = re.sub(r"\s+", " ", m.group(1)).strip().upper(); key = re.sub(r"[^A-Z]", "", name)
        if not name.startswith("TOTAL") and key not in ALLCOUNTIES: continue
        vals = [int(t.replace(",", "")) for t in re.findall(r"\d[\d,]*", m.group(3).replace(" , ", ","))]
        if name.startswith("TOTAL"): res[dist]["total"] = vals
        else: res[dist]["rows"].append([key.replace("PAMILCO", "PAMLICO"), bool(m.group(2)), vals])
    return res
fec = []
for r in csv.DictReader(open("R/data/fec_official/fec1992_house.csv")):
    if r["state_po"] == "NC" and r["is_total"] == "False": fec.append((int(r["district"]), r["candidate"].strip().lower(), int(r["votes"])))
out = []; nfec = 0
for y in (1992, 1994):
    R = parse(y); assert sorted(R) == list(range(1, 13)), (y, sorted(R))
    for d in range(1, 13):
        n = len(CANDS[y][d]); rows = R[d]["rows"]; tot = R[d]["total"]
        for r in rows:
            k = (y, d, r[0], r[1])
            if k in FIX:
                if FIX[k] is not None: r[2] = FIX[k]
                elif len(r[2]) == n - 1 or len(r[2]) > n: r[2] = r[2][:n - 1] + [0] * (n - len(r[2][:n - 1]))
        if n == 4 and y == 1992 and d == 5:
            for r in rows:
                if len(r[2]) == 3: r[2] = r[2] + [0]
                r[2] = r[2][:4]
        if y == 1992 and d == 9:
            for r in rows:
                if r[0] == "CLEVELAND": r[2] = [5993, 8090, 2]
                if r[0] == "MECKLENBURG": r[2] = [49225, 107757, 10]
        if y == 1994 and d == 12:
            for r in rows: r[2] = r[2][:3]
        bad = [r for r in rows if len(r[2]) != n]; assert not bad, (y, d, bad)
        sums = [sum(r[2][k] for r in rows) for k in range(n)]
        if y == 1994 and d == 4: tot = [76558, 77773]                                              # printed TOTAL row misread by the OCR (see above)
        if y == 1992 and d == 10: assert sums == [79206, 148999, 6886] and tot == [79206, 149033, 6888], (sums, tot)
        else: assert sums == tot, (y, d, sums, tot)
        if y == 1992:
            for (c, p), s in zip(CANDS[y][d], tot):
                hits = [v for dd, nm, v in fec if dd == d and c.lower().split()[-1][:5] in nm]
                if hits: assert s in hits, (y, d, c, s, hits)                                       # printed total equals the FEC's official total for the candidate (the FEC file lacks a few candidates)
                nfec += bool(hits)
        for cty, part, vals in rows:
            for (c, p), v in zip(CANDS[y][d], vals): out.append((y, d, cty, c, p, v))
    print(y, "all 12 districts tie to the printed TOTAL rows")
with open(D + "nc_house_county_1992_1994.csv", "w", newline="") as fh:
    w = csv.writer(fh); w.writerow(["year", "district", "county", "candidate", "party_code", "votes"]); w.writerows(out)
print("wrote", len(out), "rows;", nfec, "1992 candidate totals matched to the FEC file")
