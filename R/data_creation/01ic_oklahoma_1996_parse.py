"""Oklahoma U.S. House 1996, county level, from the State Election Board's 'State of Oklahoma
Election Results and Statistics 1996' (Oklahoma Department of Libraries, Digital Prairie,
collection stgovpub item 13878: https://digitalprairie.ok.gov/digital/collection/stgovpub/id/13878/;
local copy R/data/county_house_files/OK_1996_election_results_statistics.pdf, text layer).

Tables 'UNITED STATES REPRESENTATIVE, DISTRICT n / General Election - November 5, 1996':
county rows with one column per candidate plus TOTAL VOTES, and a STATE TOTAL row.
Stops unless every county row's candidate votes equal its TOTAL VOTES, every candidate
column equals the STATE TOTAL, all 77 counties appear, and every candidate total equals
the FEC's 1996 results. Candidate names and parties are from the table headers.

Output: R/data/county_house_files/oklahoma_house_county_1996.csv (year,district,county,candidate,party_code,votes)
"""
import csv, os, re, subprocess

HERE = os.path.dirname(os.path.abspath(__file__))
D = os.path.join(HERE, "../data/county_house_files")
PDF = os.path.join(D, "OK_1996_election_results_statistics.pdf")

CANDS = {  # district: [(name, party code)] in column order, from the table headers
    1: [("Steve Largent", "R"), ("Randolph John Amen", "D"), ("Karla Condray", "I")],
    2: [("Tom A. Coburn", "R"), ("Glen D. Johnson", "D")],
    3: [("Wes Watkins", "R"), ("Darryl Roberts", "D"), ("Scott Demaree", "I")],
    4: [("J. C. Watts, Jr.", "R"), ("Robert T. Murphy", "I"), ("Ed Crocker", "D")],
    5: [("Ernest Istook", "R"), ("James L. Forsythe", "D"), ("Ava Kennedy", "I")],
    6: [("Frank D. Lucas", "R"), ("Paul M. Barby", "D")],
}
FEC = {  # FEC 'Federal Elections 96' (R/data/fec_official/federalelections96.pdf), Oklahoma, November 5th column
    1: [143415, 57996, 8996], 2: [112273, 90120], 3: [98526, 86647, 6335],
    4: [106923, 4500, 73950], 5: [148362, 57594, 6835], 6: [113499, 64173],
}

text = subprocess.run(["pdftotext", "-layout", PDF, "-"], capture_output=True, text=True).stdout.splitlines()
rows, district, totals = [], None, {}
for i, ln in enumerate(text):
    m = re.search(r"UNITED STATES REPRESENTATIVE, DISTRICT (\d)", ln)
    if m and i + 1 < len(text) and "General Election - November 5, 1996" in text[i + 1]:
        district = int(m.group(1))
        continue
    if district is None:
        continue
    m = re.match(r"^\s*([A-Za-z][A-Za-z .:]*?)\s{2,}([\d,\s]+)$", ln)
    if not m:
        continue
    name = m.group(1).strip().upper()
    nums = [int(x.replace(",", "")) for x in m.group(2).split()]
    n = len(CANDS[district])
    if re.match(r"STATE\s+TOTAL", name):
        assert len(nums) == n + 1, (district, ln)
        totals[district] = nums
        district = None  # table ends at its STATE TOTAL row
        continue
    assert len(nums) == n + 1, (district, ln)
    assert sum(nums[:n]) == nums[n], ("row", district, name, nums)
    rows.append((district, name, nums[:n]))
assert sorted(totals) == [1, 2, 3, 4, 5, 6], sorted(totals)
for d, cands in CANDS.items():
    rs = [r for r in rows if r[0] == d]
    for j in range(len(cands)):
        s = sum(r[2][j] for r in rs)
        assert s == totals[d][j], ("column", d, j, s, totals[d][j])
        assert s == FEC[d][j], ("FEC", d, j, s, FEC[d][j])
counties = {r[1] for r in rows}
assert len(counties) == 77, len(counties)
with open(os.path.join(D, "oklahoma_house_county_1996.csv"), "w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["year", "district", "county", "candidate", "party_code", "votes"])
    for d, c, v in rows:
        for (nm, p), x in zip(CANDS[d], v):
            w.writerow([1996, d, c, nm, p, x])
print(f"{len(rows)} county rows, 77 counties, 6 districts; rows, columns and FEC all tie")
