"""Parse the Ohio Secretary of State's archived U.S. House results pages (Wayback Machine captures of www.sos.state.oh.us/sos/results/, October 2004) into a tidy CSV.
Pages (raw HTML saved under R/data/raw_house_county_open_states/ohio_sos_archive/):
  1996  https://web.archive.org/web/20041025172350/http://www.sos.state.oh.us/sos/results/90/1996/gen/UShouse.htm     "Official Tabulation"
  2000  https://web.archive.org/web/20041025162338/http://www.sos.state.oh.us/sos/results/2000/gen/us_house_of_representatives_00.htm
  2002  https://web.archive.org/web/20041025165504/http://www.sos.state.oh.us/sos/results/2002/gen/2USreps-dist.htm
Each page is a set of tables, one per congressional district: a header row ("DISTRICT 02" + candidate names with the party in parentheses), one row per county (a trailing ** marks a county split between
districts), a TOTAL(S) row and a percentage row. Output: R/data/raw_house_county_open_states/ohio_sos_archive/us_house_county_district.csv (year, district, county, split, candidate, party_code, votes).
Checks: for every district the county rows add up to the printed TOTAL row for every candidate (differences above 10 votes stop the run; smaller ones are printed)."""
import re, csv, html, sys
from html.parser import HTMLParser
D = "R/data/raw_house_county_open_states/ohio_sos_archive/"
FILES = {1996: "us_house_1996_20041025172350.html", 2000: "us_house_2000_20041025162338.html", 2002: "us_house_2002_20041025165504.html"}

class TR(HTMLParser):
    def __init__(self):
        super().__init__(); self.rows = []; self.cur = None; self.cell = None
    def handle_starttag(self, tag, attrs):
        if tag == "tr": self.cur = []
        elif tag in ("td", "th") and self.cur is not None: self.cell = []
        elif tag == "br" and self.cell is not None: self.cell.append(" ")
    def handle_endtag(self, tag):
        if tag in ("td", "th") and self.cell is not None and self.cur is not None:
            self.cur.append(re.sub(r"\s+", " ", html.unescape("".join(self.cell))).strip()); self.cell = None
        elif tag == "tr" and self.cur is not None:
            self.rows.append(self.cur); self.cur = None
    def handle_data(self, d):
        if self.cell is not None: self.cell.append(d)

def num(s):
    s = s.replace(",", "").strip()
    return int(s) if re.fullmatch(r"\d+", s) else None

out = []
for year, f in FILES.items():
    p = TR(); p.feed(open(D + f, errors="replace").read())
    dist = None; cands = None; block = []; pending = False
    def close():
        global block
        if dist is None: return
        tot = [r for r in block if r[0].upper() in ("TOTAL", "TOTALS")]
        cty = [r for r in block if r[0].upper() not in ("TOTAL", "TOTALS") and not r[0].upper().startswith("PERCENT") and any(num(c) is not None for c in r[1:])]
        for k, (nm, party) in cands.items():
            for r in cty:
                v = num(r[k]) if k < len(r) else None
                if v is None: continue
                name = r[0].replace("*", "").strip(); out.append((year, dist, name, "**" in r[0], nm, party, v))
        if tot:
            t = tot[0]
            for k, (nm, party) in cands.items():
                s = sum(num(r[k]) or 0 for r in cty if k < len(r)); tv = num(t[k]) if k < len(t) else None
                if tv is not None and s != tv:
                    print(f"note: {year} district {dist} {nm}: county rows add up to {s}, printed total {tv} (diff {s - tv})")
                    if abs(s - tv) > 10: sys.exit(f"TIE FAILED {year} district {dist} {nm}")
        elif len(cty) > 1: sys.exit(f"no TOTAL row {year} district {dist}")
        block = []
    for r in p.rows:
        if not r: continue
        m = re.match(r"^district\s+0*(\d+)$", r[0].strip(), re.I)
        if m and len(r) == 1:                                     # 2000: "District N" is a row of its own, the candidate names follow in the next row
            close(); dist = int(m.group(1)); cands = None; pending = True; continue
        if pending and r[0] == '' and any(c for c in r):
            cands = {}
            for k, c in enumerate(r):
                if not c: continue
                mm = re.match(r"^(.*?)\s*\(([A-Za-z]+)\)\s*$", c.replace("*", "").strip()); cands[k - 1] = (mm.group(1).strip(), mm.group(2)) if mm else (c.strip(), "")   # data columns start one cell earlier than the names
            pending = False; continue
        if m and sum(1 for c in r[1:] if c) >= 1:
            close(); dist = int(m.group(1)); cands = {}
            for k, c in enumerate(r):
                if k == 0 or not c: continue
                c = c.replace("*", "").strip(); mm = re.match(r"^(.*?)\s*\(([A-Za-z]+)\)\s*$", c)
                cands[k] = (mm.group(1).strip(), mm.group(2)) if mm else (c, "")
            continue
        if r[0] == '' and len(r) > 1 and r[1] != '': r = [r[1], ''] + r[2:]          # 2002: county name sits in the second cell (header: district, blank, candidates...)
        if dist is not None and cands is not None: block.append(r)
    close()
w = csv.writer(open(D + "us_house_county_district.csv", "w", newline=""))
w.writerow(["year", "district", "county", "split", "candidate", "party_code", "votes"]); w.writerows(out)
import collections
print(len(out), collections.Counter(o[0] for o in out))
for y in FILES: print(y, "districts", sorted({o[1] for o in out if o[0] == y}), "counties", len({o[2] for o in out if o[0] == y}))
