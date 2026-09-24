"""Oklahoma U.S. House 1998 by county and district from the State Election Board's own results page as archived by the Wayback Machine:
https://web.archive.org/web/20020721032506/http://www.elections.state.ok.us/98gencon.html ("General Election 11/3/98: Congressional Officers"; the URL in the request was the 2004 capture of the same page).
Saved as R/data/raw_house_county_open_states/oklahoma_archive/98gencon_20020721032506_wayback.html. The page is preformatted text: after the county-by-county U.S. Senator table come six tables (one per congressional
district): two header lines (first names / last names with the party in parentheses), an '=' line, one line per county (a county split between districts appears in each) and a STATE TOTAL line.
Check: county rows add up to the STATE TOTAL line in every column (stop on failure). Output: R/data/raw_house_county_open_states/oklahoma_archive/us_house_1998_county_district.csv."""
import re, csv, html, sys
D = "R/data/raw_house_county_open_states/oklahoma_archive/"
t = open(D + "98gencon_20020721032506_wayback.html", errors="replace").read()
t = html.unescape(re.sub(r"<[^>]+>", "", t))
out = []
for m in re.finditer(r"UNITED STATES REPRESENTATIVE, DISTRICT (\d)\s*\n(.*?)(?=\n\s*Return to Oklahoma|\n\s*UNITED STATES REPRESENTATIVE, DISTRICT|\Z)", t, re.S):
    dist = int(m.group(1)); lines = [l for l in m.group(2).split("\n")]
    ei = next(i for i, l in enumerate(lines) if l.strip().startswith("====="))
    h1 = [x for x in re.split(r"\s{2,}", lines[ei - 2].strip()) if x]; h2 = [x for x in re.split(r"\s{2,}", lines[ei - 1].strip()) if x]
    assert len(h1) == len(h2), (dist, h1, h2)
    cands = []
    for a, b in zip(h1[:-1], h2[:-1]):
        mm = re.match(r"^(.*?)\s*\(([A-Z]+)\)\s*$", b); cands.append((f"{a} {mm.group(1)}".strip(), mm.group(2)))
    n = len(cands); sums = [0] * (n + 1); tot = None
    for l in lines[ei + 1:]:
        if not l.strip(): continue
        mt = re.match(r"^\s*STATE TOTAL:\s*(.*)$", l)
        if mt: tot = [int(x.replace(",", "")) for x in mt.group(1).split()]; continue
        mc = re.match(r"^([A-Za-z' .]+?)\s{2,}([\d,\s]+)$", l)
        if not mc: continue
        v = [int(x.replace(",", "")) for x in mc.group(2).split()]; assert len(v) == n + 1, (dist, l)
        sums = [a + b for a, b in zip(sums, v)]
        if sum(v[:-1]) != v[-1]: sys.exit(f"ROW SUM FAILED district {dist} {l}")
        for (name, party), x in zip(cands, v[:-1]): out.append((1998, dist, mc.group(1).strip(), name, party, x))
    if tot != sums: sys.exit(f"TIE FAILED district {dist}: counties {sums} vs STATE TOTAL {tot}")
w = csv.writer(open(D + "us_house_1998_county_district.csv", "w", newline="")); w.writerow(["year", "district", "county", "candidate", "party_code", "votes"]); w.writerows(out)
print(len(out), "rows;", len({o[2] for o in out}), "counties; all 6 district tables tie to their STATE TOTAL lines")
