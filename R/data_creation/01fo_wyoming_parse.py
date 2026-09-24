"""Wyoming U.S. House county results 1996, 1998, 2006 from the Wyoming Secretary of State's statewide general-election PDFs (text layer), downloaded from
https://sos.wyo.gov/Elections/Docs/<year>/<year>GeneralResults.aspx -> 1996/96Results/96General/96General_SW_Candidate_Judicial_Const.Amendments.pdf, 1998/98Results/98General/Statewide_Issues Abstract.pdf,
2006/06Results/06General/SW_Candidates_Summary.pdf (saved as R/data/county_house_files/wyoming/WY_<year>_general_statewide.pdf). The at-large U.S. Representative table lists the 23 counties and the candidates
(1996/1998 print surnames only: "Cubin (R)"; 2006 prints full names with the party after a dash and splits the columns over two pages: Trauner and Rankin on page 1, Cubin on page 2).
Check: for every candidate the 23 county rows add up to the printed Official Totals / Total row (stop on failure). Output: R/data/county_house_files/wyoming/wyoming_house_county.csv (year, county, candidate, party_code, votes)."""
import re, csv, subprocess, sys
D = "R/data/county_house_files/wyoming/"
def text(y, extra=()): return subprocess.run(["pdftotext", "-layout", *extra, D + f"WY_{y}_general_statewide.pdf", "-"], capture_output=True, text=True).stdout
def rows(lines):
    out = {}; tot = None
    for l in lines:
        m = re.match(r"^\s*([A-Z][A-Za-z ]+?)\s{2,}([\d,\s]+)$", l)
        if not m: continue
        v = [int(x.replace(",", "")) for x in m.group(2).split()]
        if m.group(1).strip() in ("Official Totals", "Total"): tot = v
        else: out[m.group(1).strip()] = v
    return out, tot
res = []
def emit(year, cands, out, tot):
    assert len(out) == 23, (year, len(out))
    for k, (name, party) in enumerate(cands):
        s = sum(v[k] for v in out.values())
        if s != tot[k]: sys.exit(f"TIE FAILED {year} {name}: counties {s} vs printed total {tot[k]}")
        for c, v in out.items(): res.append((year, c, name, party, v[k]))
for y, hdr_re in ((1996, r"Cubin \(R\)\s+Maxfield \(D\)\s+Dawson \(L\)"), (1998, r"Farris \(D\)\s+Richardson \(L\)\s+Cubin \(R\)")):
    L = text(y).split("\n"); i = next(i for i, l in enumerate(L) if re.search(hdr_re, l))
    cands = [(a, b) for a, b in re.findall(r"([A-Za-z]+) \(([A-Z])\)", L[i])]
    out, tot = rows(L[i + 1:i + 40]); emit(y, cands, out, tot)
P1 = text(2006, ("-f", "1", "-l", "1")).split("\n"); P2 = text(2006, ("-f", "2", "-l", "2")).split("\n")
o1, t1 = rows(P1); o2, t2 = rows(P2)
o1 = {c: v[2:] for c, v in o1.items()}; t1 = t1[2:]
o2 = {c: v[:1] for c, v in o2.items()}; t2 = t2[:1]
out = {c: o1[c] + o2[c] for c in o1}; emit(2006, [("Gary Trauner", "D"), ("Thomas R. Rankin", "L"), ("Barbara Cubin", "R")], out, t1 + t2)
w = csv.writer(open(D + "wyoming_house_county.csv", "w", newline="")); w.writerow(["year", "county", "candidate", "party_code", "votes"]); w.writerows(res)
print(len(res), "rows; every candidate's 23 county rows tie to the printed totals")
