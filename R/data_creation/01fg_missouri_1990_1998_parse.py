"""Missouri U.S. House county results 1990-1998 from the Official Manual of the State of Missouri (Blue Book) pages saved by the project lead under
R/data/county_house_files/missouri/<year>/bluebook_*_full.jpg (1990 pp. 496-497, 1992 pp. 705-707, 1994 pp. 530-532, 1996 pp. 563-565, 1998 pp. 561-563). Each page prints, per congressional district, a table of the county
(or "part of" county) votes of every candidate and a TOTALS row. The tables were read from the page images into R/data/county_house_files/missouri/transcribed/mo_<year>*.txt (one line per county) and are
parsed here into a tidy CSV, checking that the county rows add up to the printed TOTALS row in every column (stop on failure) except one documented exception.
Conventions: a county split between districts appears once per district ("St. Louis (part of)", "Jackson (part of)"; the 1998 pages print the whole names) and is summed in the build; "Kansas City" (1994-1998
District 5: the Kansas City Board of Election Commissioners' Jackson County portion) is assigned to Jackson County; "St. Louis" without "City" is St. Louis County. The 1992 page 706 heads the first table "District 9"
but it is District 5 (Alan Wheat; District 9 is on the next page).
Documented exception: 1992 District 8, Republican column: the 26 county cells add up to 147,128 but the book (and the FEC) print 147,398 for Bill Emerson (a 270-vote difference, e.g. a transposed digit in one county);
the county cells are used as printed, so Emerson's county total is 270 votes short in that district.
Output: R/data/county_house_files/missouri/missouri_house_county_1990_1998.csv (year, district, county, candidate, party_code, votes)."""
import re, csv, glob, sys
D = "R/data/county_house_files/missouri/transcribed/"
KNOWN = {(1992, 8, 0): 147128}          # (year, district, column) -> county-cell sum that is known to differ from the printed total
out = []; ndist = 0
for f in sorted(glob.glob(D + "mo_*.txt")):
    year = int(re.search(r"mo_(\d{4})", f).group(1)); cands = None; sums = None; dist = None
    for line in open(f):
        l = line.strip()
        if not l or l.startswith("#"): continue
        if l.startswith("DISTRICT"):
            head, names = l.split("|", 1); dist = int(head.split()[1]); ndist += 1; sums = None
            cands = []
            for c in re.split(r"\),\s*", names.strip()):
                c = c if c.endswith(")") else c + ")"
                m = re.match(r"^(.*)\(([^()]*)\)\s*$", c.strip()); cands.append((m.group(1).strip(), m.group(2).strip()))
            continue
        p = [x.strip() for x in l.split("|")]; v = [int(x) for x in p[1:]]
        assert len(v) == len(cands), (f, dist, l)
        if p[0] == "TOTALS":
            for k, (a, b) in enumerate(zip(sums, v)):
                if a != b and KNOWN.get((year, dist, k)) != a: sys.exit(f"TIE FAILED {year} district {dist} column {k}: counties {a} vs printed {b}")
                if a != b: print(f"note: {year} district {dist} {cands[k][0]}: county cells add up to {a}, printed total {b} (diff {a - b})")
            continue
        sums = v if sums is None else [a + b for a, b in zip(sums, v)]
        for (name, party), x in zip(cands, v): out.append((year, dist, p[0], name, party, x))
w = csv.writer(open("R/data/county_house_files/missouri/missouri_house_county_1990_1998.csv", "w", newline=""))
w.writerow(["year", "district", "county", "candidate", "party_code", "votes"]); w.writerows(out)
print(ndist, "district tables,", len(out), "rows")
