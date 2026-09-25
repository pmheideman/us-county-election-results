"""Two House gaps closed from official sources found by a source search (2026-09-24; report R/data/county_house_files/source_search/MS1996_AR2004_DE1998.md).

ARKANSAS 2004: Secretary of State, '2004 General Election and Non-Partisan Judicial Runoff Certification Report' (1,663-page PDF with text layer,
  https://www.sos.arkansas.gov/uploads/elections/2004_General_Election_and_Non-Partisan_Judicial_Runoff_Certification_Report.pdf; local copy
  R/data/county_house_files/arkansas/leads/). County blocks 'U.S. Congress District 0N (<County> County)' list each candidate's votes. District 4
  (Mike Ross) was unopposed and not on the ballot, so its 29 counties have no tally (unopposed_no_ballot). Check: per district, county sums equal
  the statewide certified totals printed at the front of the report, for every candidate. Over/under votes are not candidates.
DELAWARE 1998: Department of Elections, '1998 General Election - Results By District' (Wayback capture; R/data/county_house_files/delaware/leads/),
  U.S. Representative by election district, summed to the 3 counties with the OpenElections election-district-to-county map 1992-2002 (the map
  reproduces the official 2000 county page exactly). Aggregated by the search agent into DE_1998_house_county_aggregated_from_ED.csv. Check: the
  county sums equal the statewide totals on the by-office page (Williams 57,446; Castle 119,811; Bemis 859; Webster 2,411).

Outputs: R/data/county_house_files/arkansas/arkansas_house_county_2004.csv, R/data/county_house_files/delaware/delaware_house_county_1998.csv
"""
import csv, collections, re, subprocess

B = "R/data/county_house_files/"
TITLES = r"^(Congressman|Congresswoman|State Representative|State Senator)\s+"
line_re = re.compile(r"^\s*(.+?) - (Democrat|Republican|Independent|Write-In|Green|Libertarian)\s+([\d,]+)\s+[\d.]+%")

# ---- Arkansas 2004 ----
txt = subprocess.run(["pdftotext", "-layout", B + "arkansas/leads/AR_2004_General_Election_Certification_Report.pdf", "-"],
                     capture_output=True, text=True).stdout.splitlines()
state, county_rows, cur = collections.defaultdict(dict), [], None
for ln in txt:
    m = re.match(r"^U\.S\. Congress District 0(\d)(?: \((.+?) County\))?\s*$", ln.strip())
    if m:
        cur = (int(m.group(1)), m.group(2)); continue
    if cur and not line_re.match(ln) and (re.search(r"\(.+ County\)\s*$", ln.strip()) or re.match(r"^(State |Proposed|Referred|U\.S\. )", ln.strip())):
        cur = None   # next section heading (candidate lines such as 'State Representative Marvin Parks - Republican ...' are not headings)
    if not cur: continue
    m = line_re.match(ln)
    if m and m.group(2) == "Write-In":   # write-ins (District 2: Gabriel, 4 votes statewide) are not listed by county; left out as elsewhere
        continue
    if m:
        name = re.sub(TITLES, "", m.group(1)).strip(); v = int(m.group(3).replace(",", ""))
        pc = {"Democrat": "D", "Republican": "R"}.get(m.group(2), "I")
        if cur[1] is None: state[cur[0]][name] = (pc, v)
        else: county_rows.append((cur[0], cur[1].upper(), name, pc, v))
assert sorted(state) == [1, 2, 3], sorted(state)
# the report prints every county block twice (county section and county summary): keep one copy after checking both agree
seen = {}
for r in county_rows:
    k = r[:3]
    assert seen.get(k, r) == r, ("copies differ", r, seen[k])
    seen[k] = r
county_rows = list(seen.values())
sums = collections.Counter()
for d, c, n, p, v in county_rows: sums[(d, n)] += v
for d, cands in state.items():
    for n, (p, v) in cands.items():
        assert sums[(d, n)] == v, ("AR", d, n, sums[(d, n)], v)
counties = {c for _, c, *_ in county_rows}
assert len(counties) == 46, len(counties)
with open(B + "arkansas/arkansas_house_county_2004.csv", "w", newline="") as f:
    w = csv.writer(f); w.writerow(["year", "district", "county", "candidate", "party_code", "votes"])
    for r in county_rows: w.writerow([2004, *r[:3], r[3], r[4]])
print(f"Arkansas 2004: {len(counties)} counties in districts 1-3, every candidate's county sum equals the certified total; district 4 unopposed")

# ---- Delaware 1998 ----
rows = list(csv.DictReader(open(B + "delaware/leads/DE_1998_house_county_aggregated_from_ED.csv")))
tot = collections.Counter()
for r in rows: tot[r["candidate"]] += int(r["votes"])
assert dict(tot) == {"Dennis E. Williams": 57446, "Michael N. Castle": 119811, "Kim Stanley Bemis": 859, "James P. Webster": 2411}, dict(tot)
assert {r["county"] for r in rows} == {"New Castle", "Kent", "Sussex"}
with open(B + "delaware/delaware_house_county_1998.csv", "w", newline="") as f:
    w = csv.writer(f); w.writerow(["year", "district", "county", "candidate", "party_code", "votes"])
    for r in rows:
        w.writerow([1998, 1, r["county"].upper(), r["candidate"], {"Democrat": "D", "Republican": "R"}.get(r["party"], "I"), int(r["votes"])])
print("Delaware 1998: 3 counties, county sums equal the statewide totals")
