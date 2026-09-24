"""North Carolina U.S. House county results 1996 and 1998 from the State Board of Elections' archived result files (public S3 bucket dl.ncsbe.gov, not linked from the Historical Election Results page):
  1996  https://s3.amazonaws.com/dl.ncsbe.gov/ENRS/1996_11_05/results_by_contest_19961105.zip -> results_US_House_19961105.pdf (a text PDF printed from the old sboe.state.nc.us "Abstract of Votes Cast in the General
        Election held on November 5, 1996" web page; per district, county rows with D/R/L/NL(/WI) columns and a Total row; a split county appears in each district it touches with only that part's votes)
  1998  https://s3.amazonaws.com/dl.ncsbe.gov/ENRS/1998_11_03/results_by_contest_19981103.zip -> results_US_House_01..12_19981103.pdf (one PDF per district: precinct rows, a "<COUNTY> TOTALS:" line for every county
        and a GRAND TOTALS line)
Saved unzipped under R/data/county_house_files/north_carolina/z96 and z98. The NC readme says the files "are considered public information per NC General Statutes".
1996 candidate names come from the printed column headers (the layout puts them over several lines); columns are matched by party order.
Checks (stop on failure): 1996 county rows add up to the printed Total row of each district in every column; 1998 county TOTALS lines add up to the GRAND TOTALS line of each district.
Output: R/data/county_house_files/north_carolina/nc_house_county_1996_1998.csv (year, district, county, candidate, party_code, votes)."""
import re, csv, subprocess, sys, glob
D = "R/data/county_house_files/north_carolina/"
def pdf(f): return subprocess.run(["pdftotext", "-layout", f, "-"], capture_output=True, text=True).stdout
out = []
NAMES96 = {1: ["Eva M. Clayton", "Ted Tyler", "Todd Murphrey", "Joseph Boxerman"], 2: ["Bob Etheridge", "David Funderburk", "Mark D. Jackson", "Robert Argy Jr."], 3: ["George Parrott", "Walter B. Jones Jr.", "Edward Downey"],
  4: ["David E. Price", "Fred Heineman", "David Allen Walker", "Russell Wollman"], 5: ["Neil Grist Cashion Jr.", "Richard M. Burr", "Barbara J. Howe", "Craig Berg"], 6: ["Mark Costley", "Howard Coble", "Gary Goodson"],
  7: ["Mike McIntyre", "Bill Caster", "Chris Nubel", "Garrison King Frantz"], 8: ["W. G. (Bill) Hefner", "Curtis Blackwood", "Thomas W. Carlisle"], 9: ["Michael C. (Mike) Daisley", "Sue Myrick", "David L. Knight", "Jeannine Austin", "Gene Gay"],
  10: ["Ben Neill", "T. Cass Ballenger", "Richard Kahn"], 11: ["James Mark Ferguson", "Charles H. Taylor", "Phil McCanless", "Milton Burrill"], 12: ["Mel Watt", "Joseph A. (Joe) Martino Jr.", "Roger L. Kohn", "Walter Lewis"]}
cur = None; blocks = []
for l in pdf(D + "z96/results_US_House_19961105.pdf").split("\n"):
    m = re.match(r"^\s*(\d+)(?:st|nd|rd|th) Congressional District", l)
    if m: cur = {"d": int(m.group(1)), "rows": [], "tot": None, "party": None}; blocks.append(cur); continue
    if cur is None: continue
    if re.match(r"^\s*\((D|R|L|NL|WI)\)", l): cur["party"] = re.findall(r"\(([A-Z]+)\)", l); continue
    mt = re.match(r"^\s*Total\s+([\d,\s]+)$", l)
    if mt: cur["tot"] = [int(x.replace(",", "")) for x in mt.group(1).split()]; continue
    mc = re.match(r"^\s*([A-Z][A-Z \.']+?)\s+([\d,]+(?:\s+[\d,]+)*)\s*$", l)
    if mc and cur["party"]: cur["rows"].append((mc.group(1).strip(), [int(x.replace(",", "")) for x in mc.group(2).split()]))
assert len(blocks) == 12
for b in blocks:
    n = len(b["party"]); names = NAMES96[b["d"]]; assert len(names) == n, b["d"]
    sums = [sum(v[k] for c, v in b["rows"] if k < len(v)) for k in range(n)]
    if sums != b["tot"]: sys.exit(f"TIE FAILED 1996 district {b['d']}: {sums} vs {b['tot']}")
    for c, v in b["rows"]:
        for k in range(n):
            x = v[k] if k < len(v) else 0
            out.append((1996, b["d"], c, names[k], b["party"][k], x))
for f in sorted(glob.glob(D + "z98/results_US_House_*_19981103.pdf")):
    dist = int(re.search(r"House_(\d+)_", f).group(1)); t = pdf(f).split("\n")
    cands = []; 
    for l in t:
        for p, nm in re.findall(r"\((D|R|L|NL|WI|IND|I|G|REF|UNAF|CST|LIB)\)\s+([^()]+?)\s*(?=\(|$)", l.strip()): pass
    hdr = [l for l in t[:12] if re.search(r"\([A-Z]+\)", l)]
    for l in hdr:
        for p, nm in re.findall(r"\(([A-Z]+)\)\s+(.+?)(?=\s{2,}|$)", l): cands.append((p, nm.strip().rstrip("_").replace("_", " ")))
    n = len(cands); sums = [0] * n; grand = None
    for l in t:
        mt = re.match(r"^\s*([A-Za-z' \.]+?) TOTALS:\s+([\d,\s]+)$", l)
        if mt and mt.group(1).strip().upper() != "GRAND":
            v = [int(x.replace(",", "")) for x in mt.group(2).split()]; assert len(v) == n, (dist, l)
            sums = [a + b for a, b in zip(sums, v)]
            for (p, nm), x in zip(cands, v): out.append((1998, dist, mt.group(1).strip().upper(), nm, p, x))
        mg = re.match(r"^\s*GRAND TOTALS:\s+([\d,\s]+)$", l)
        if mg: grand = [int(x.replace(",", "")) for x in mg.group(1).split()]
    if sums != grand: sys.exit(f"TIE FAILED 1998 district {dist}: {sums} vs {grand}")
w = csv.writer(open(D + "nc_house_county_1996_1998.csv", "w", newline="")); w.writerow(["year", "district", "county", "candidate", "party_code", "votes"]); w.writerows(out)
print(len(out), "rows; 12 district tables per year tie to their printed totals")
