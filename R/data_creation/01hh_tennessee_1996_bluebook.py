"""Tennessee U.S. House 1996, county level, from the Tennessee Blue Book 1997-1998
(printed pp. 538-541, PDF pp. 545-548, 'General Election for U.S. House of
Representatives - November 5, 1996'). No other county-level source was found.

Two independent keys: (1) visual transcription of the 300 dpi page images
(microfiche/review/bluebook_1996.json), (2) the PDF's OCR text layer
(01hf_tn_bluebook_1996_textlayer.py). Stops unless every column of every
district ties to its printed TOTAL and every cell the text layer reads cleanly
equals the image reading (the only exceptions allowed are text-layer '00' tokens
that dropped a digit, checked on the page image: D1 Hawkins, D6 Smith, D7
Henderson). Write-Ins are dropped, as for 1998.

Output: tennessee/tennessee_house_county_1996.csv (same layout as the 1998 file)
"""
import csv, json, os

HERE = os.path.dirname(os.path.abspath(__file__))
TN = os.path.join(HERE, "../data/county_house_files/tennessee")
V = json.load(open(os.path.join(TN, "microfiche/review/bluebook_1996.json")))["districts"]
T = json.load(open(os.path.join(TN, "microfiche/review/bluebook_1996_textlayer.json")))
KNOWN_TEXT_DROPS = {("1", "HAWKINS", 7), ("6", "SMITH", 2), ("7", "HENDERSON", 2)}

num = lambda s: int(str(s).replace(",", ""))
out, agree = [], 0
for d, dd in V.items():
    rows, total, cands = dd["rows"], dd["total"], dd["candidates"]
    n = len(cands)
    assert all(len(r["values"]) == n for r in rows) and len(total) == n, d
    for j in range(n):
        assert sum(num(r["values"][j]) for r in rows) == num(total[j]), (d, j)
    trows = {r["county"]: r["values"] for r in T.get(d, {}).get("rows", [])}
    for r in rows:
        t = trows.get(r["county"])
        if t is None or len(t) != n:
            continue
        for j in range(n):
            if t[j] is None:
                continue
            if num(t[j]) == num(r["values"][j]):
                agree += 1
            else:
                assert (d, r["county"], j) in KNOWN_TEXT_DROPS and t[j] == "00", (d, r["county"], j, t[j], r["values"][j])
    for r in rows:
        for j, c in enumerate(cands):
            if c["name"].lower().startswith("write"):
                continue
            out.append(dict(year=1996, district=int(d), county=r["county"], candidate=c["name"],
                            party_code=c["party"] or "I", votes=num(r["values"][j])))
assert len({r["county"] for r in out}) == 95
with open(os.path.join(TN, "tennessee_house_county_1996.csv"), "w", newline="") as f:
    w = csv.DictWriter(f, fieldnames=["year", "district", "county", "candidate", "party_code", "votes"])
    w.writeheader(); w.writerows(out)
print(f"{len(out)} candidate-county rows, {len(V)} districts; {agree} cells confirmed by both keys")
