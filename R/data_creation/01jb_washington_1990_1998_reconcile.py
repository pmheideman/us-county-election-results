"""Washington U.S. House 1990, 1992, 1994, 1996, 1998, county level, from the Washington Secretary of State's 'Election Search' results database
(1900-2006; still online): per race, 'Show County Breakdown' pages
  https://www.sos.wa.gov/elections/results_report_county_breakdown.aspx?e=<E>&t=<T>
  E: 1990=24, 1992=3, 1994=15, 1996=17, 1998=10; T: CD1=857 CD2=861 CD3=864 CD4=867 CD5=868 CD6=869 CD7=863 CD8=865 CD9=872
Found and downloaded by a source-search agent (2026-09-24): 44 pages in R/data/county_house_files/washington/leads/sos_county_breakdown/, parsed into
wa_house_county_1990_1998_sos_db.csv (year, district, county, candidate, party, votes). Search report: R/data/county_house_files/source_search/WA_1990_1998.md.

Checks (stop on failure): 39 counties each year; every candidate's district total equals the official figure (1990: R/data/fec_official/fec1990_house.csv;
1992-1998: Clerk of the House statistics, R/data/clerk_house_stats/<year>Stat.htm). Party codes come from those official lists, because the SOS database
mislabels some (1998 CD4 Pross coded R, a Democrat; 1998 CD7 Lippmann coded L, Reform). Names: 'Taber Taber' -> Ron Taber; double spaces removed.

Output: R/data/county_house_files/washington/washington_house_county_<year>.csv (year, district, county, candidate, party_code, votes)
"""
import csv, collections, html, re

SRC = "R/data/county_house_files/washington/leads/sos_county_breakdown/wa_house_county_1990_1998_sos_db.csv"
OUT = "R/data/county_house_files/washington/"
letters = lambda s: re.sub(r"[^A-Z]", "", s.upper())
FIX_NAMES = {"Taber Taber": "Ron Taber"}


def official(year):
    """{district: [(name, party_letter, votes)]}"""
    out = collections.defaultdict(list)
    if year == 1990:
        for r in csv.DictReader(open("R/data/fec_official/fec1990_house.csv")):
            if r["state_po"] == "WA" and r["is_total"] != "True" and r["district"]:
                p = r["party"]; out[int(r["district"])].append((r["candidate"], "D" if "Democrat" in p else "R" if "Republican" in p else "I", int(float(r["votes"]))))
        return out
    t = open(f"R/data/clerk_house_stats/{year}Stat.htm", encoding="latin-1").read()
    t = re.sub(r"\s+", " ", html.unescape(re.sub(r"<[^>]+>", " ", t)))
    seg = t[t.find("WASHINGTON For "):]; seg = seg[:seg.find("WEST VIRGINIA")]; seg = seg[seg.find("Representative"):]
    d = None
    for m in re.finditer(r"(?:(\d{1,2})\.\s+)?([A-Z][^,0-9]+?(?:, (?:Jr|Sr|II|III)\.?)?), ([A-Za-z ,.'-]+?) ([\d,]+)(?= |$)", seg):
        if m.group(1): d = int(m.group(1))
        p = m.group(3).split(",")[0].strip()
        out[d].append((m.group(2), "D" if p.startswith("Democrat") else "R" if p.startswith("Republican") else "I", int(m.group(4).replace(",", ""))))
    return out


rows = list(csv.DictReader(open(SRC)))
for year in (1990, 1992, 1994, 1996, 1998):
    rs = [r for r in rows if int(r["year"]) == year]
    assert len({r["county"] for r in rs}) == 39, year
    O = official(year)
    tot = collections.Counter();
    for r in rs:
        r["candidate"] = FIX_NAMES.get(re.sub(r"\s+", " ", r["candidate"]).strip(), re.sub(r"\s+", " ", r["candidate"]).strip())
        tot[(int(r["district"]), r["candidate"])] += int(r["votes"])
    code = {}
    for (d, cand), v in tot.items():
        last = letters(cand.split()[-1])
        hit = [o for o in O[d] if last in letters(o[0]) and o[2] == v]
        assert len(hit) == 1, (year, d, cand, v, O[d])
        code[(d, cand)] = hit[0][1]
    assert sum(len(v) for v in O.values()) == len(tot), (year, "official list has candidates the SOS data lacks")
    with open(OUT + f"washington_house_county_{year}.csv", "w", newline="") as fh:
        w = csv.writer(fh); w.writerow(["year", "district", "county", "candidate", "party_code", "votes"])
        for r in rs:
            w.writerow([year, int(r["district"]), r["county"].upper(), r["candidate"], code[(int(r["district"]), r["candidate"])], int(r["votes"])])
    changed = [(d, c, [r["party"] for r in rs if int(r["district"]) == d and r["candidate"] == c][0], code[(d, c)]) for (d, c) in code
               if {"D": "D", "R": "R"}.get([r["party"] for r in rs if int(r["district"]) == d and r["candidate"] == c][0], "I") != code[(d, c)]]
    print(f"{year}: {len({k[0] for k in tot})} districts, 39 counties, {len(tot)} candidates equal the official totals; party changed from the SOS label: {changed}")
