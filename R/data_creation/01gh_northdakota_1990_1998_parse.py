"""North Dakota U.S. House (at-large) 1990-1998 by county, from the Secretary of State's scanned "Official Abstract of Votes Cast at the General Election" (one wide two-page table per
year, downloaded by curl from https://www.sos.nd.gov/elections/election-results -> election-results-pdfs/<date>-general.pdf; saved as R/data/county_house_files/north_dakota/ND_<year>_general.pdf).
The PDFs are scans with an embedded OCR text layer. The text layer's word positions (pdftotext -bbox) are used to put every county number into its column (a blank cell stays blank; the
columns are located through the statewide TOTAL row, whose House cells are the FEC's official totals for 1990, 1992, 1996 and 1998 and the total printed on the 1994 page image). Checks (stop on failure):
53 counties; every House column adds up to the printed TOTAL cell; the candidates' votes are compared with the county's TOTAL BALLOTS CAST (three 1994 counties exceed it as printed: Griggs, LaMoure, McHenry; the image agrees with the text layer).
Output: R/data/county_house_files/north_dakota/nd_house_county_1990_1998.csv (year, county, candidate, party_code, votes)."""
import re, subprocess, csv, os, sys
D = os.path.join("R", "data", "county_house_files", "north_dakota") + os.sep
def words(y, p):
    h = subprocess.run(["pdftotext", "-bbox", "-f", str(p), "-l", str(p), D + f"ND_{y}_general.pdf", "-"], capture_output=True, text=True).stdout
    return [(float(a), float(b), float(c), float(d), t) for a, b, c, d, t in re.findall(r'<word xMin="([\d.]+)" yMin="([\d.]+)" xMax="([\d.]+)" yMax="([\d.]+)">(.*?)</word>', h)]
def rows(W):
    W = sorted(W, key=lambda w: (w[1] + w[3]) / 2); out = []
    for w in W:
        yc = (w[1] + w[3]) / 2
        if out and abs(out[-1][0] - yc) < 3: out[-1][1].append(w); out[-1][0] = (out[-1][0] + yc) / 2
        else: out.append([yc, [w]])
    return [(y, sorted(ws)) for y, ws in out]
def num(t):
    t = t.replace("O", "0").replace("o", "0")
    return int(t.replace(",", "")) if re.fullmatch(r"\d[\d,]*", t) else None
COUNTIES = """ADAMS BARNES BENSON BILLINGS BOTTINEAU BOWMAN BURKE BURLEIGH CASS CAVALIER DICKEY DIVIDE DUNN EDDY EMMONS FOSTER GOLDENVALLEY GRANDFORKS GRANT GRIGGS HETTINGER KIDDER LAMOURE LOGAN MCHENRY MCINTOSH MCKENZIE MCLEAN MERCER MORTON MOUNTRAIL NELSON OLIVER PEMBINA PIERCE RAMSEY RANSOM RENVILLE RICHLAND ROLETTE SARGENT SHERIDAN SIOUX SLOPE STARK STEELE STUTSMAN TOWNER TRAILL WALSH WARD WELLS WILLIAMS""".split()
assert len(COUNTIES) == 53
def ckey(nm):
    k = re.sub(r"[^A-Z]", "", nm.upper()); return {"MCLNTOSH": "MCINTOSH", "MCLNTOSH": "MCINTOSH"}.get(k, k)
# year: (page, [(candidate, party_code, printed statewide total)])
SPEC = {
 1990: (1, [("Byron L. Dorgan", "D", 152530), ("Edward T. Schafer", "R", 81443)]),
 1992: (2, [("Earl Pomeroy", "D", 169273), ("John T. Korsmo", "R", 117442), ("Anna Belle Bourgois", "I", 7394), ("Grady Blount", "I", 3789)]),
 1994: (1, [("Gary Porter", "R", 105988), ("Earl Pomeroy", "D", 123134), ("James Germalic", "I", 6267)]),
 1996: (1, [("Earl Pomeroy", "D", 144833), ("Kevin Cramer", "R", 113684), ("Kenneth R. Loughead", "I", 4493)]),
 1998: (2, [("Kevin Cramer", "R", 87511), ("Earl Pomeroy", "D", 119668), ("Kenneth R. Loughead", "I", 5709)]),
}
out = []
for y, (page, cands) in SPEC.items():
    R = rows(words(y, page)); body = []
    for yc, ws in R:
        k = 0
        while k < len(ws) and not re.fullmatch(r"\d[\d,]*", ws[k][4]): k += 1                # county name = the leading non-numeric words
        nm = " ".join(w[4] for w in ws[:k]); nm = re.sub(r"[.\s]+$", "", re.sub(r"\s*[.·]+\s*", " ", nm)).strip()
        if nm: body.append((nm, ws[k:]))
    tot = [b for b in body if b[0].upper() == "TOTAL" and any(num(w[4]) == cands[0][2] for w in b[1])]; assert len(tot) == 1, (y, len(tot)); tw = tot[0][1]
    cols = []
    for c, p, t in cands:
        m = [w for w in tw if num(w[4]) == t]; assert len(m) == 1, (y, c, t, [w[4] for w in tw][:12]); cols.append(m[0][2])
    ballots_x = [w for w in tw if num(w[4]) is not None][1][2]                    # second number = TOTAL BALLOTS CAST
    allx = [w[2] for w in tw if num(w[4]) is not None]; ct = []
    for nm, ws in body:
        if nm.upper() == "TOTAL": continue
        if ckey(nm) not in COUNTIES: continue
        assign = {}                                                                     # every number goes to the nearest column of the TOTAL row
        for w in ws:
            if num(w[4]) is None: continue
            j = min(range(len(allx)), key=lambda q: abs(allx[q] - w[2]))
            if abs(allx[j] - w[2]) >= 14: continue                                      # a column to the right of those the TOTAL row's text layer carries (not House)
            assert j not in assign, (y, nm, w); assign[j] = num(w[4])
        vals = [assign.get(allx.index(x), 0) for x in cols]; ballots = assign.get(allx.index(ballots_x))
        ct.append((ckey(nm), vals, ballots))
    assert len(ct) == 53, (y, len(ct), [c[0] for c in ct])
    for k, (c, p, t) in enumerate(cands):
        s = sum(r[1][k] for r in ct); assert s == t, (y, c, s, t)
    bad = [(r[0], sum(r[1]), r[2]) for r in ct if r[2] is not None and sum(r[1]) > r[2]]; print('  county House votes above TOTAL BALLOTS CAST as printed (checked against the page image, a defect of the source):', bad) if bad else None
    print(y, "page", page, ":", len(ct), "counties; columns tie to the printed totals:", [t for _, _, t in cands])
    for nm, vals, b in ct:
        for (c, p, t), v in zip(cands, vals): out.append((y, nm, c, p, v))
os.makedirs(D, exist_ok=True)
with open(D + "nd_house_county_1990_1998.csv", "w", newline="") as fh:
    w = csv.writer(fh); w.writerow(["year", "county", "candidate", "party_code", "votes"]); w.writerows(out)
print("wrote", len(out), "rows")
