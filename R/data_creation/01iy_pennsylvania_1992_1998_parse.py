"""Pennsylvania U.S. House 1994, 1996, 1998, county level, from the Pennsylvania Department of State's precinct election returns
(Bureau of Commissions, Elections and Legislation, 'Precinct Election Returns' electronic data files, extracted June 30, 2005; downloaded by the
project lead to R/data/county_house_files/pennsylvania/ElectionReturns_<year>_General_PrecinctReturns.txt with ReadMe files giving the layout).

Comma-delimited, one row per precinct x office x candidate. Used: Election Type G, Office Code USC (Representative in Congress); County Code
(1-67, alphabetical), Candidate District, Party Code, Candidate Number / names, Vote Total. Party OTH = the write-in total ('WRITE-IN'), left out.

The file is a later database extract, not the certified canvass, so every district is checked against the Clerk of the House 'Statistics of the
Congressional Election' (R/data/clerk_house_stats/<year>Stat.htm): each candidate with at least 10% of the district vote must be within 1% of the
Clerk's figure (most differ by a few votes); three districts are allowed up to 3% and documented: 1994 District 7 (Weldon -2.2%, Nichols -1.5%),
1996 District 11 (Kanjorski/Urban -0.8%/-1.0%), 1998 District 3 (Borski +2.6%). Party codes come from the Clerk, because the file records
candidates cross-filed on the Republican AND Democratic lines under one line only (1994 District 9 Shuster appears as DEM; the Clerk lists him
'Republican, Democrat'): a candidate listed first as Republican is coded R, first as Democrat D.

NOT BUILT: 1992. Seven of 21 districts are more than 1% off the Clerk, District 4 and District 10 about 15% short (McDade's Republican-line votes,
about 30,000, are missing), so county shares would be wrong. The 1992 file is kept for reference only.

Output: R/data/county_house_files/pennsylvania/pennsylvania_house_county_<year>.csv (year, district, county, candidate, party_code, votes)
"""
import csv, collections, html, re

D = "R/data/county_house_files/pennsylvania/"
YEARS = (1994, 1996, 1998)
ALLOW_3PCT = {(1994, 7), (1996, 11), (1998, 3)}
COUNTIES = ("ADAMS ALLEGHENY ARMSTRONG BEAVER BEDFORD BERKS BLAIR BRADFORD BUCKS BUTLER CAMBRIA CAMERON CARBON CENTRE CHESTER CLARION CLEARFIELD "
            "CLINTON COLUMBIA CRAWFORD CUMBERLAND DAUPHIN DELAWARE ELK ERIE FAYETTE FOREST FRANKLIN FULTON GREENE HUNTINGDON INDIANA JEFFERSON JUNIATA "
            "LACKAWANNA LANCASTER LAWRENCE LEBANON LEHIGH LUZERNE LYCOMING MCKEAN MERCER MIFFLIN MONROE MONTGOMERY MONTOUR NORTHAMPTON NORTHUMBERLAND "
            "PERRY PHILADELPHIA PIKE POTTER SCHUYLKILL SNYDER SOMERSET SULLIVAN SUSQUEHANNA TIOGA UNION VENANGO WARREN WASHINGTON WAYNE WESTMORELAND "
            "WYOMING YORK").split()
assert len(COUNTIES) == 67
letters = lambda s: re.sub(r"[^A-Z]", "", s.upper())


def clerk(year):
    """{district: [(name, party string, votes)]} from the Clerk's Pennsylvania section."""
    t = open(f"R/data/clerk_house_stats/{year}Stat.htm", encoding="latin-1").read()
    t = re.sub(r"\s+", " ", html.unescape(re.sub(r"<[^>]+>", " ", t)))
    seg = t[t.find("PENNSYLVANIA For "):]; seg = seg[:seg.find("RHODE ISLAND")]; seg = seg[seg.find("Representative"):]
    out, d = collections.defaultdict(list), None
    for m in re.finditer(r"(?:(\d{1,2})\.\s+)?([A-Z][^,0-9]+?(?:, (?:Jr|Sr|II|III)\.?)?), ([A-Za-z ,.'-]+?) ([\d,]+)(?= |$)", seg):
        if m.group(1): d = int(m.group(1))
        out[d].append((m.group(2), m.group(3), int(m.group(4).replace(",", ""))))
    return out


for year in YEARS:
    C = clerk(year)
    agg = collections.Counter(); meta = {}
    for r in csv.reader(open(D + f"ElectionReturns_{year}_General_PrecinctReturns.txt", encoding="latin-1")):
        if len(r) < 17 or r[1] != "G" or r[8] != "USC" or r[9] == "OTH":
            continue
        dist, cand = int(r[5]), r[10]
        agg[(dist, COUNTIES[int(r[2]) - 1], cand)] += int(r[15] or 0)
        meta[(dist, cand)] = (" ".join(x for x in [r[12].title(), r[13].title(), r[11].title(), r[14]] if x), r[11], r[9])
    assert len({k[1] for k in agg}) == 67
    tot = collections.Counter()
    for (d, c, cand), v in agg.items():
        tot[(d, cand)] += v
    code = {}
    for d in sorted({k[0] for k in tot}):
        cands = C[d]; dist_tot = sum(c[2] for c in cands)
        for (dd, cand), v in tot.items():
            if dd != d: continue
            last = letters(meta[(d, cand)][1])
            hit = [c for c in cands if letters(re.sub(r",.*", "", c[0]).split()[-1]) in last or last in letters(c[0])]
            assert len(hit) == 1, (year, d, meta[(d, cand)], [c[0] for c in cands])
            nm, party, cv = hit[0]
            first = party.split(",")[0].strip()
            code[(d, cand)] = "R" if first.startswith("Republican") else "D" if first.startswith("Democrat") else "I"
            if cv >= 0.1 * dist_tot:
                tol = 0.03 if (year, d) in ALLOW_3PCT else 0.01
                assert abs(v - cv) <= tol * cv, (year, d, nm, v, cv)
    with open(D + f"pennsylvania_house_county_{year}.csv", "w", newline="") as fh:
        w = csv.writer(fh); w.writerow(["year", "district", "county", "candidate", "party_code", "votes"])
        for (d, c, cand), v in sorted(agg.items()):
            w.writerow([year, d, c, meta[(d, cand)][0], code[(d, cand)], v])
    relabel = [(k[0], meta[k][0], meta[k][2], code[k]) for k in code if (meta[k][2] == "DEM") != (code[k] == "D") or (meta[k][2] == "REP") != (code[k] == "R")]
    print(f"{year}: {len({k[0] for k in tot})} districts, 67 counties, {len(tot)} candidates within tolerance of the Clerk; party from the Clerk differs from the file for {relabel}")
