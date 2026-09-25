"""Ohio U.S. House 2004 and 2010, county level, from the Ohio Secretary of State's results pages as archived by the Wayback Machine
(links found by the project lead, 2026-09-24):
  2004  https://web.archive.org/web/20150109111817/http://www.sos.state.oh.us/sos/elections/Research/electResultsMain/2004ElectionsResults/04-1102USReps.aspx
  2010  https://web.archive.org/web/20150106175304/http://www.sos.state.oh.us/sos/elections/Research/electResultsMain/2010results/20101102congress.aspx
        ('Amended Official Results')
Raw HTML saved in R/data/raw_house_county_open_states/ohio_sos_archive/ (oh2004_usreps.html, oh2010_congress.html).

Layout: per district a header row ('DISTRICT NUMBER: 1' / 'U.S. Representative - District 01'), a candidate-name row, a party row, one row per county
(' **' marks a county split between districts), a Total row and a percentage row. Checks (stop on failure): every candidate column adds up to its
Total; every candidate with a Democratic or Republican label equals the FEC general-election figure (R/output/fec_congress_rows.rds, parsed from the
FEC workbooks); all 88 counties appear. Write-in candidates (WI / 'Write In') are kept as other candidates, as for 2002.

Output: R/data/raw_house_county_open_states/ohio_sos_archive/us_house_county_district_2004_2010.csv (year, district, county, candidate, party_code, votes)
"""
import csv, html, re, subprocess, json
from html.parser import HTMLParser

D = "R/data/raw_house_county_open_states/ohio_sos_archive/"
FILES = {2004: "oh2004_usreps.html", 2010: "oh2010_congress.html"}


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


def party_code(p):
    p = p.lower()
    return "D" if p.startswith("democrat") else "R" if p.startswith("republican") else "I"


# FEC general-election figures for Ohio 2004/2010 (read from the project's parsed FEC rows)
fec_json = subprocess.run(["Rscript", "-e", 'x <- readRDS("R/output/fec_congress_rows.rds"); x <- subset(x, state_po == "OH" & year %in% c(2004, 2010) & !is.na(last) & !is.na(gen)); '
                           'cat(jsonlite::toJSON(x[, c("year", "district", "last", "party", "gen")]))'], capture_output=True, text=True).stdout
FEC = [x for x in json.loads(fec_json) if str(x["district"]).isdigit()]

out = []
for year, f in FILES.items():
    p = TR(); p.feed(open(D + f, errors="replace").read())
    rows = [[c for c in r] for r in p.rows if any(c for c in r)]
    i = 0; n_dist = 0
    while i < len(rows):
        r = rows[i]
        m = re.search(r"DISTRICT NUMBER:\s*(\d+)|U\.S\. Representative - District (\d+)", " ".join(r))
        if not m:
            i += 1; continue
        dist = int(m.group(1) or m.group(2)); n_dist += 1
        while re.search(r"DISTRICT NUMBER:|U\.S\. Representative - District", " ".join(rows[i + 1])):   # 2010 District 9 prints both header styles
            i += 1
        names = [c for c in rows[i + 1][1:] if c]
        parties = [c for c in rows[i + 2][1:] if c][:len(names)]
        assert len(parties) == len(names), (year, dist, names, parties)
        names = [re.sub(r"^\*", "", re.sub(r"\s*\(WI\)$", "", n)).strip() for n in names]
        j = i + 3; county_rows = []; total = None
        while j < len(rows):
            c0 = rows[j][0].strip()
            vals = [num(c) for c in rows[j][1:1 + len(names)]]
            if c0.lower().startswith("total"):
                total = vals; break
            if c0.lower().startswith("percentage"):   # one-county districts print no Total row (2004 District 10): the county row is the total
                assert len(county_rows) == 1, (year, dist)
                total = list(county_rows[0][1]); break
            assert not re.search(r"DISTRICT", c0, re.I), ("no Total row before the next district", year, dist)
            assert len(vals) == len(names) and all(v is not None for v in vals), ("short/odd county row", year, dist, rows[j])
            county_rows.append((re.sub(r"\s*\*+$", "", c0).strip().upper(), vals)); j += 1
        assert total and all(v is not None for v in total), (year, dist)
        for k, nm in enumerate(names):
            s = sum(v[k] or 0 for _, v in county_rows)
            assert s == total[k], ("column", year, dist, nm, s, total[k])
            pc = party_code(parties[k])
            if pc in ("D", "R"):
                fec = [x for x in FEC if x["year"] == year and int(x["district"]) == dist and str(x["party"])[:1] == pc
                       and x["last"].lower() in nm.lower()]
                assert len(fec) == 1 and fec[0]["gen"] == total[k], ("FEC", year, dist, nm, total[k], fec)
            for cty, v in county_rows:
                out.append(dict(year=year, district=dist, county=cty, candidate=nm, party_code=pc, votes=v[k] or 0))
        i = j + 1
    cties = {r["county"] for r in out if r["year"] == year}
    assert len(cties) == 88, (year, len(cties))
    print(f"{year}: {n_dist} districts, 88 counties, every column ties to its Total, D/R candidates equal the FEC")

with open(D + "us_house_county_district_2004_2010.csv", "w", newline="") as fh:
    w = csv.DictWriter(fh, fieldnames=["year", "district", "county", "candidate", "party_code", "votes"])
    w.writeheader(); w.writerows(out)
print(len(out), "rows")
