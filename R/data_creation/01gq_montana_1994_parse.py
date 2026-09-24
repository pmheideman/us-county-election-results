"""Montana U.S. House (at-large) 1994 by county, from the Secretary of State's "1994 Statewide General Canvass ... As compiled by Secretary of State Mike Cooney" Lotus 1-2-3 worksheet (94STATE.WK1, inside
94genst.zip inside 94gencnty.zip; recovered from the Wayback Machine capture of sos.mt.gov/Elections/archives/1990s/1994/94GEN/94gencnty.zip, saved as R/data/county_house_files/montana/MT_1994_gencnty.zip).
The worksheet has one row per county and a TOTAL row; U.S. Representative = Cy Jamison (Republican), Steve Kelly (Independent), Pat Williams (Democrat). The worksheet is converted to CSV with gnumeric's
ssconvert. Checks (stop on failure): 56 counties; the county rows add up to the printed TOTAL row for each candidate (148,715 / 32,046 / 171,372); candidates do not exceed each county's total votes cast.
Output: R/data/county_house_files/montana/montana_house_county_1994.csv (year, county, candidate, party_code, votes)."""
import csv, subprocess, os, tempfile
D = "R/data/county_house_files/montana/"
tmp = tempfile.mkdtemp()
subprocess.run(["ssconvert", "-S", "-O", "", D + "mt94/94STATE.WK1", os.path.join(tmp, "s.csv")], check=True, capture_output=True)
rows = list(csv.reader(open(os.path.join(tmp, "s.csv.0"), encoding="utf-8", errors="replace")))
hi = next(i for i, r in enumerate(rows) if "Jamison" in r); h = rows[hi]
cj, ck, cw = h.index("Jamison"), h.index("Kelly"), h.index("Williams")
assert (ck, cw) == (cj + 1, cj + 2)
hdr_party = rows[hi - 2]; assert hdr_party[cj] == "Republican" and hdr_party[ck] == "Independent" and hdr_party[cw] == "Democrat", hdr_party[cj - 1:cw + 1]
ct = []; tot = None
for r in rows[hi + 1:]:
    if not r or not r[0].strip(): continue
    vals = [int(float(r[c])) for c in (cj, ck, cw)]; cast = int(float(r[3]))
    if r[0].strip() == "TOTAL": tot = vals; break
    ct.append((r[0].strip().upper(), vals, cast))
assert len(ct) == 56, len(ct)
assert [sum(c[1][k] for c in ct) for k in range(3)] == tot == [148715, 32046, 171372], tot
assert all(sum(c[1]) <= c[2] for c in ct), [c for c in ct if sum(c[1]) > c[2]]
names = [("Cy Jamison", "R"), ("Steve Kelly", "I"), ("Pat Williams", "D")]
with open(D + "montana_house_county_1994.csv", "w", newline="") as fh:
    w = csv.writer(fh); w.writerow(["year", "county", "candidate", "party_code", "votes"])
    for c, v, _ in ct:
        for (nm, p), x in zip(names, v): w.writerow([1994, c, nm, p, x])
print("56 counties; ties to the printed TOTAL row", tot)
