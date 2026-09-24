"""Oregon U.S. House county results 1990-1998, 2000, 2002, 2004 and 2012 from the Secretary of State's "Official Abstract of Votes" / "Statistical Summary" documents in the State Library of Oregon's
digital collection (https://digitalcollections.library.oregon.gov/nodes/view/88649; item pages 203500 (1990), 203718 (1992), 203908 (1994), 204141 (1996 document, which also contains the 1998 general-election
abstract), 204882 (2000), 205358 (2002), 206356 (2004), 213403 (2012); PDFs fetched from /assets/displaypdf/<id> by curl and saved as R/data/county_house_files/oregon/OR_<year>_abstract.pdf).
Each document has one "Representative in Congress" table per congressional district: county rows (a county split between districts appears in each) and a TOTAL row, with a trailing "Misc." write-in column (not used).
2000-2004, 1998 and 2012 come from the PDFs' text layer (parsed by the functions below). 1990-1996 are old scans: their text layer is read where every candidate column ties to the printed TOTAL row, and the tables whose
OCR misread digits were corrected against a second OCR pass (300 dpi, tesseract --psm 6) and the page images and are typed into MANUAL below (each is checked against its printed TOTAL row here).
Checks (stop on failure): every candidate column adds up to the printed TOTAL row; 36 counties are covered in every year. 1994 District 1 uses the Automatic Recount Certification table.
Output: R/data/county_house_files/oregon/or_house_county.csv (year, district, county, candidate, party_code, votes)."""
import re, csv, difflib, subprocess, os
D = "R/data/county_house_files/oregon/"
def txt(y): return subprocess.run(["pdftotext", "-layout", D + f"OR_{y}_abstract.pdf", "-"], capture_output=True, text=True).stdout.split("\n")
ORC = "BAKER BENTON CLACKAMAS CLATSOP COLUMBIA COOS CROOK CURRY DESCHUTES DOUGLAS GILLIAM GRANT HARNEY HOODRIVER JACKSON JEFFERSON JOSEPHINE KLAMATH LAKE LANE LINCOLN LINN MALHEUR MARION MORROW MULTNOMAH POLK SHERMAN TILLAMOOK UMATILLA UNION WALLOWA WASCO WASHINGTON WHEELER YAMHILL".split()
def county(tok):
    t = re.sub(r"[^A-Za-z]", "", tok).upper()
    m = difflib.get_close_matches(t, ORC, n=1, cutoff=0.7) if t else []
    return m[0] if m else None
def nums(s): return re.sub(r"(?<=\d)[;:.](?=\d{3})", ",", s)
def parse_tables(lines):
    out = []; i = 0; n = len(lines)
    while i < n:
        ln = lines[i]
        m = re.search(r"REPRESENTATIVE IN C\S{1,3}GRESS,?\s*([\dS]+)(ST|ND|RD|TH)\s+DISTRICT", ln, re.I) or re.match(r"\s*([\dSsIi]+)(st|nd|rd|th|\")?\s+Congre\S*\s+\S", ln, re.I) or re.match(r"\s*(\d)(st|nd|rd|th) District", ln)
        if not m: i += 1; continue
        g = m.group(1).upper(); suf = (m.group(2) or "").lower()
        d = 3 if (g == "S" and suf == "rd") else (1 if g == "I" else int(g.replace("S", "5")))
        j = i + 1
        while j < n and not re.match(r"\s*\W*[Cc]?ounty", lines[j]) and j < i + 14: j += 1
        if j >= n or not re.match(r"\s*\W*[Cc]?ounty", lines[j]): i += 1; continue
        rows = []; total = None; k = j + 1
        while k < n and k < j + 60:
            l = lines[k]
            if re.match(r"\s*TOTAL", l, re.I): total = [int(x.replace(",", "")) for x in re.findall(r"\d[\d,]*", nums(l))]; break
            m2 = re.match(r"^\W*([A-Za-z][A-Za-z ]*?)\W*\s{2,}(.*)$", l) if l.strip() else None
            if m2 and county(m2.group(1)): rows.append((county(m2.group(1)), [int(x.replace(",", "")) for x in re.findall(r"\d[\d,]*", nums(m2.group(2)))]))
            k += 1
        out.append(dict(district=d, title=ln.strip(), rows=rows, total=total)); i = k + 1
    return out
def ties(t):
    tot = t["total"]; rows = t["rows"]
    return tot is not None and all(len(r[1]) == len(tot) for r in rows) and [sum(r[1][c] for r in rows) for c in range(len(tot))] == tot
# candidates per year/district, in column order (the trailing Misc. column is not a candidate)
N = {
 1990: {1: [("Les AuCoin", "D"), ("Rick Livingston", "I"), ("Earl Molander", "R")], 2: [("Jim Smiley", "D"), ("Robert F. (Bob) Smith", "R")], 3: [("Phil Mooney", "R"), ("Ron Wyden", "D")], 4: [("Peter DeFazio", "D"), ("Tonie Nathan", "L")], 5: [("Mike Kopetski", "D"), ("Denny Smith", "R")]},
 1992: {1: [("Elizabeth Furse", "D"), ("Tony Meeker", "R")], 2: [("Denzel Ferguson", "D"), ("Robert F. (Bob) Smith", "R")], 3: [("Blair Bobier", "L"), ("Al Ritter", "R"), ("Ron Wyden", "D")], 4: [("Peter DeFazio", "D"), ("Richard L. Schulz", "R")], 5: [("Mike Kopetski", "D"), ("Jim Seagraves", "R")]},
 1994: {1: [("Elizabeth Furse", "D"), ("Brewster Gillett", "A"), ("Daniel E. Wilson", "L"), ("Bill Witt", "R")], 2: [("Wes Cooley", "R"), ("Sue C. Kupillas", "D"), ("Gary L. Sublett II", "L")], 3: [("Mark Brunelle", "I"), ("Everett Hall", "R"), ("Gene Nanni", "L"), ("Ron Wyden", "D")],
        4: [("Peter DeFazio", "D"), ("John D. Newkirk", "R")], 5: [("Jim Bunn", "R"), ("Catherine Webber", "D"), ("Jon E. Zimmer", "L")]},
 1996: {1: [("Elizabeth Furse", "D"), ("David Prine", "S"), ("Richard Johnson", "L"), ("Bill Witt", "R")], 2: [("Mike Dugan", "D"), ("Robert F. (Bob) Smith", "R"), ("Frank Wise", "L")],
        3: [("Joe Keating", "P"), ("Bruce Alexander Knight", "L"), ("Scott Bruun", "R"), ("Earl Blumenauer", "D"), ("Victoria P. Guillebeau", "S")],
        4: [("Peter A. DeFazio", "D"), ("David G. Duemler", "S"), ("William (Bill) Banville", "RF"), ("Allan Opus", "P"), ("Tonie Nathan", "L"), ("John D. Newkirk", "R")], 5: [("Darlene Hooley", "D"), ("Lawrence Knight Duquesne", "L"), ("Trey Smith", "S"), ("Jim Bunn", "R")]},
 1998: {1: [("Molly Bordonaro", "R"), ("David Wu", "D"), ("Michael De Paulo", "L"), ("John F. Hryciuk", "S")], 2: [("Lindsey Bradshaw", "L"), ("Rohn (Grandpa) Webb", "S"), ("Greg Walden", "R"), ("Kevin M. Campbell", "D")],
        3: [("Walter F. (Walt) Brown", "S"), ("Earl Blumenauer", "D"), ("Bruce Alexander Knight", "L")], 4: [("Steve J. Webb", "R"), ("Karl G. Sorg", "S"), ("Peter A. DeFazio", "D")],
        5: [("Jim Burns", "NL"), ("Blaine Thallheimer", "L"), ("Marylin Shannon", "R"), ("Ed Dover", "S"), ("Michael Donnelly", "P"), ("Darlene Hooley", "D")]},
 2000: {1: [("Beth A. King", "L"), ("David Wu", "D"), ("Charles Starr", "R")], 2: [("Greg Walden", "R"), ("Walter Ponsford", "D")], 3: [("Earl Blumenauer", "D"), ("Walter F. (Walt) Brown", "S"), ("Bruce Alexander Knight", "L"), ("Jeffery L. Pollock", "R"), ("Tre Arrow", "P")],
        4: [("Peter A. DeFazio", "D"), ("David Duemler", "S"), ("John Lindsey", "R")], 5: [("Brian J. Boquist", "R"), ("Darlene Hooley", "D")]},
 2002: {1: [("Jim Greenfield", "R"), ("David Wu", "D"), ("Beth A. King", "L")], 2: [("Mike Wood", "L"), ("Greg Walden", "R"), ("Peter Buckley", "D")], 3: [("Kevin Jones", "L"), ("Walter F. (Walt) Brown", "S"), ("David Brownlow", "C"), ("Earl Blumenauer", "D"), ("Sarah Seale", "R")],
        4: [("Peter A. DeFazio", "D"), ("Chris Bigelow", "L"), ("Liz VanLeeuwen", "R")], 5: [("Brian J. Boquist", "R"), ("Darlene Hooley", "D")]},
 2004: {1: [("Goli Ameri", "R"), ("Dean Wolf", "C"), ("David Wu", "D")], 2: [("Jim Lindsay", "L"), ("John C. McColgan", "D"), ("Greg Walden", "R"), ("Jack Alan Brown Jr.", "C")], 3: [("Tami Mars", "R"), ("Dale Winegarden", "C"), ("Earl Blumenauer", "D"), ("Walter F. (Walt) Brown", "S")],
        4: [("Peter A. DeFazio", "D"), ("Jim Feldkamp", "R"), ("Michael Paul Marsh", "C"), ("Jacob Boone", "L")], 5: [("Jerry Defoe", "L"), ("Darlene Hooley", "D"), ("Jim Zupancic", "R"), ("Joseph H. Bitz", "C")]},
 2012: {1: [("Bob Ekstrom", "C"), ("Delinda Morgan", "R"), ("Suzanne Bonamici", "D"), ("Steven Reynolds", "P")], 2: [("Joyce B. Segers", "D"), ("Greg Walden", "R"), ("Joe Tabor", "L")], 3: [("Earl Blumenauer", "D"), ("Woodrow Broadnax", "PG"), ("Michael Cline", "L"), ("Ronald Green", "R")],
        4: [("Peter A. DeFazio", "D"), ("Chuck Huntting", "L"), ("Art Robinson", "R")], 5: [("Kurt Schrader", "D"), ("Fred Thompson", "R"), ("Raymond Baldwin", "C"), ("Christina Jean Lugo", "PG")]},
}
# tables typed from the page images / second OCR pass (candidate columns only, in the order of N): {(year, district): ([county rows...], printed totals)}
def rows(s):
    out = []
    for l in s.strip().split("\n"):
        p = l.split(); k = next(i for i, x in enumerate(p) if re.fullmatch(r"[\d,]+", x)); out.append(("".join(p[:k]).upper(), [int(x.replace(",", "")) for x in p[k:]]))
    return out
MANUAL = {
 (1990, 1): (rows("""Clatsop 8773 789 2726
Columbia 10221 1338 3469
Lincoln 9907 1160 4368
Multnomah 32403 2158 11925
Polk 2886 356 1613
Tillamook 6326 603 2404
Washington 65998 7572 38381
Yamhill 13778 1609 7496"""), [150292, 15585, 72382]),
 (1990, 2): (rows("""Baker 1721 4238
Crook 1455 3764
Deschutes 9420 18621
Gilliam 220 608
Grant 971 2135
Harney 746 2134
HoodRiver 1831 3368
Jackson 16264 31724
Jefferson 1316 2866
Josephine 5939 13055
Klamath 5129 12711
Lake 806 2212
Malheur 2118 5938
Morrow 789 1710
Sherman 274 741
Umatilla 4293 8307
Union 2825 5833
Wallowa 881 2358
Wasco 2976 5184
Wheeler 157 491"""), [60131, 127998]),
 (1990, 3): (rows("""Clackamas 6416 17566
Multnomah 33800 152165"""), [40216, 169731]),
 (1990, 4): (rows("""Benton 4142 802
Coos 18370 2826
Curry 5731 1599
Douglas 27053 4860
Jackson 2709 858
Josephine 2397 621
Lane 89595 12679
Linn 12472 2181
Marion 25 6"""), [162494, 26432]),
 (1990, 5): (rows("""Benton 14792 8707
Clackamas 50125 36874
Linn 8182 8528
Marion 43829 40241
Polk 7682 7300"""), [124610, 101650]),
 (1992, 1): (rows("""Clackamas 9549 9861
Clatsop 8458 7076
Columbia 9625 9285
Multnomah 35917 17813
Washington 76871 78867
Yamhill 12497 18084"""), [152917, 140986]),
 (1992, 2): (rows("""Baker 1917 5287
Crook 1939 4972
Deschutes 15869 26751
Gilliam 245 751
Grant 886 3008
Harney 541 2792
HoodRiver 2548 4691
Jackson 27516 46579
Jefferson 1871 3973
Josephine 9428 19850
Klamath 7531 18953
Lake 846 2891
Malheur 2380 7959
Morrow 950 2421
Sherman 299 801
Umatilla 6848 14691
Union 3613 7456
Wallowa 768 3154
Wasco 3846 6541
Wheeler 195 642"""), [90036, 184163]),
 (1992, 3): (rows("""Clackamas 1439 9255 24173
Multnomah 9974 40980 183855"""), [11413, 50235, 208028]),
 (1992, 4): (rows("""Benton 5172 2260
Coos 20794 8136
Curry 6737 3929
Douglas 28085 16050
Josephine 1570 1202
Lane 111821 34046
Linn 25193 14110"""), [199372, 79733]),
 (1992, 5): (rows("""Benton 19932 8550
Clackamas 55597 32272
Lincoln 12033 5970
Marion 66339 39848
Polk 12451 8240
Tillamook 8091 3104"""), [174443, 97984]),
 (1994, 2): (rows("""Baker 4151 2186 219
Crook 3507 2560 286
Deschutes 18467 16743 1479
Gilliam 536 317 23
Grant 2408 832 112
Harney 2015 771 70
HoodRiver 2949 3466 228
Jackson 34820 26632 2478
Jefferson 2655 2171 214
Josephine 15181 8282 1055
Klamath 14703 5858 848
Lake 2357 780 79
Malheur 5915 2244 175
Morrow 1597 1106 102
Sherman 549 360 33
Umatilla 9966 7150 621
Union 5333 3928 485
Wallowa 2350 981 126
Wasco 4341 4240 397
Wheeler 455 215 33"""), [134255, 90822, 9063]),
 (1996, 3): (rows("""Clackamas 718 573 12880 19177 196
Multnomah 8556 3901 52379 146745 2253"""), [9274, 4474, 65259, 165922, 2449]),
 (1996, 5): (rows("""Benton 15584 374 143 10100
Clackamas 42749 1676 618 41044
Lincoln 11184 619 191 8031
Marion 52449 1852 917 49196
Polk 11841 469 175 11990
Tillamook 5714 201 80 5048"""), [139521, 5191, 2124, 125409]),
}
SRC = {1990: (1990, None), 1992: (1992, None), 1994: (1994, (400, 600)), 1996: (1996, (960, 1140)), 1998: (1996, (4700, 4900)), 2000: (2000, (600, 800)), 2002: (2002, (500, 640)), 2004: (2004, (480, 620)), 2012: (2012, (1051, 1160))}
out = []
for y in sorted(N):
    doc, rng = SRC[y]; L = txt(doc)
    if rng: L = L[rng[0] - 1: rng[1]]
    T = {}
    for t in parse_tables(L):
        if y == 1994 and t["district"] == 1 and "Recount" not in t["title"]: continue                         # Original Certification table; the Automatic Recount Certification is the result
        if t["district"] not in T or ties(t): T[t["district"]] = t
    cov = set()
    for d in range(1, 6):
        cands = N[y][d]
        if (y, d) in MANUAL:
            rws, tot = MANUAL[(y, d)]
            assert len(tot) == len(cands) and all(len(r[1]) == len(cands) for r in rws), (y, d)
            assert [sum(r[1][c] for r in rws) for c in range(len(tot))] == tot, (y, d, "manual block does not tie")
        else:
            t = T[d]; assert ties(t), (y, d, "text layer table does not tie", t["total"])
            k = len(t["total"]); assert k in (len(cands), len(cands) + 1), (y, d, k, len(cands))
            rws = [(c, v[:len(cands)]) for c, v in t["rows"]]; tot = t["total"][:len(cands)]
        for c, v in rws:
            cov.add(c)
            for (nm, p), x in zip(cands, v): out.append((y, d, c, nm, p, x))
    print(y, "all 5 districts tie to the printed TOTAL rows;", len(cov), "counties with House rows")
    assert len(cov) == 36, (y, sorted(set(ORC) - cov))
with open(D + "or_house_county.csv", "w", newline="") as fh:
    w = csv.writer(fh); w.writerow(["year", "district", "county", "candidate", "party_code", "votes"]); w.writerows(out)
print("wrote", len(out), "rows")
