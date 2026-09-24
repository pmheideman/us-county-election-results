"""Montana U.S. House (at-large) county results 1992, 1996 and 1998 from the Secretary of State's official general-election canvass PDFs (text layers), recovered from the Wayback Machine:
https://web.archive.org/web/20090205162642id_/http://sos.mt.gov/Elections/archives/1990s/1992/92GEN/1992gen.pdf, .../1996/96GEN/1996gen.pdf (20090205162706), .../1998/98GEN/1998gen.pdf (20090205162801)
(saved as R/data/county_house_files/montana/MT_<year>_general.pdf). The canvass is one wide table (counties x every contest) printed over several pages side by side:
  1992: county names on every page, U.S. Representative columns (Marlenee R, Williams D, Wilverding L) are the 2nd-4th numbers of the "U.S. Representative At-Large District" block on page 2.
  1996: county names only on pages 1-2; the U.S. House columns are on two later page pairs without county names, matched to the counties by row order: Heaton (NL), Rehberg (R), Shaw (Reform) = the first three numbers
        of pages 5-6 (top line = statewide total), Yellowtail (D) = the first number of pages 7-8.
  1998: county lines are on pages 1-3, Deschamps (D) is their last number; Fellows (L), Sullivan (Reform) and Hill (R) are the first three numbers of the unlabeled rows on pages 4-5 (top line = statewide total).
Checks (stop on failure): 56 counties; each candidate's county values add up to the printed statewide total; for 1996/1998 the row-order alignment is tested by requiring every county's House votes not to exceed its printed total votes cast and to be at least 75% of it.
Output: R/data/county_house_files/montana/montana_house_county_1992_1998.csv (year, county, candidate, party_code, votes)."""
import re, csv, subprocess, sys
D = "R/data/county_house_files/montana/"
def page(y, p): return subprocess.run(["pdftotext", "-layout", "-f", str(p), "-l", str(p), D + f"MT_{y}_general.pdf", "-"], capture_output=True, text=True).stdout.split("\n")
def nums(s): return [int(x.replace(",", "")) for x in re.findall(r"\d[\d,]*", s)]
def numeric_lines(lines): return [nums(l) for l in lines if re.fullmatch(r"[\d,\s]+", l) and l.strip()]
def county_rows(lines):
    out = []
    for l in lines:
        m = re.match(r"^\s*([A-Z][A-Za-z &\.]+?)\s{2,}([\d,%\.\s]+)$", l)
        if m and m.group(1) not in ("TOTAL", "Total"): out.append((m.group(1).strip(), m.group(2)))
    return out
res = []; RATIOS = []
def emit(y, counties, cols, totals, votecast=None):
    assert len(counties) == 56, (y, len(counties))
    for name, party, vals, tot in cols:
        assert len(vals) == 56, (y, name, len(vals))
        if sum(vals) != tot: sys.exit(f"TIE FAILED {y} {name}: counties {sum(vals)} vs printed statewide total {tot}")
        for c, v in zip(counties, vals): res.append((y, c, name, party, v))
    if votecast:
        for i, c in enumerate(counties):
            h = sum(v[i] for _, _, v, _ in cols)
            RATIOS.append((round(h / votecast[i], 3), c))
            if not (0.75 * votecast[i] <= h <= votecast[i]): sys.exit(f"ALIGNMENT CHECK FAILED {y} {c}: House votes {h} vs votes cast {votecast[i]}")
# ---- 1992: county names on the same line
L = page(1992, 2); out = {}
for l in L:
    m = re.match(r"^\s*([A-Z][A-Za-z &\.]+?)\s{2,}([\d,\s]+)$", l)
    if m and m.group(1).strip() not in ("Total",): out[m.group(1).strip()] = nums(m.group(2))
tot = next(nums(l) for l in L if re.match(r"^\s*Total\s", l))
counties = list(out); cols = [(n, p, [out[c][k] for c in counties], tot[k]) for n, p, k in (("Ron Marlenee", "R", 1), ("Pat Williams", "D", 2), ("Jerome J. Wilverding", "L", 3))]
emit(1992, counties, cols, tot)
# ---- 1996
cn = []
for p in (1, 2): cn += [c for c, _ in county_rows(page(1996, p))]
votecast = []
for p in (1, 2): votecast += [nums(r)[-3 if False else -2] for c, r in county_rows(page(1996, p))]   # [precincts, registration, votes cast, turnout%]: votes cast is the 3rd number
votecast = []
for p in (1, 2):
    for c, r in county_rows(page(1996, p)): votecast.append(nums(r)[2])
def stack(y, pages):
    rows = []
    for p in pages: rows += numeric_lines(page(y, p))
    return rows
hr = stack(1996, (5, 6)); dr = stack(1996, (7, 8))
th, td = hr[0], dr[0]; hr, dr = hr[1:], dr[1:]
assert len(hr) == 56 and len(dr) == 56, (len(hr), len(dr))
cols = [("Stephen Heaton", "NL", [r[0] for r in hr], th[0]), ("Dennis Rehberg", "R", [r[1] for r in hr], th[1]), ("Becky Shaw", "Reform", [r[2] for r in hr], th[2]), ("Bill Yellowtail", "D", [r[0] for r in dr], td[0])]
emit(1996, cn, cols, None, votecast)
# ---- 1998
cn = []; d98 = []; votecast = []
for l in page(1998, 1) + page(1998, 2) + page(1998, 3):
    m = re.match(r"^\s*([A-Z][A-Za-z &\.]+?)\s{2,}([\d,%\.\s]+)$", l)
    if m and m.group(1).strip() != "TOTAL": cn.append(m.group(1).strip()); v = nums(m.group(2)); votecast.append(v[2]); d98.append(v[-1])
tot98 = next(nums(m.group(1)) for l in page(1998, 1) if (m := re.match(r"^\s*TOTAL\s+(.*)$", l)))
r2 = stack(1998, (4, 5, 6, 7)); th = r2[0]; r2 = r2[1:]
assert len(r2) >= 56, len(r2)
r2 = r2[:56]
cols = [("Dusty Deschamps", "D", d98, tot98[-1]), ("Mike Fellows", "L", [r[0] for r in r2], th[0]), ("Webb Sullivan", "Reform", [r[1] for r in r2], th[1]), ("Rick Hill", "R", [r[2] for r in r2], th[2])]
emit(1998, cn, cols, None, votecast)
print("lowest House/votes-cast ratios:", sorted(RATIOS)[:6], "highest:", sorted(RATIOS)[-3:])
w = csv.writer(open(D + "montana_house_county_1992_1998.csv", "w", newline="")); w.writerow(["year", "county", "candidate", "party_code", "votes"]); w.writerows(res)
print(len(res), "rows")
