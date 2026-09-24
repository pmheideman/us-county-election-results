"""Second, independent key for Tennessee House 1996: parse the OCR text layer
of the Tennessee Blue Book 1997-1998 (PDF pp. 545-548, general election
Nov. 5, 1996). The first key is a visual transcription of the page images
(review/bluebook_1996.json); 01hg compares the two cell by cell.

Cells whose text-layer token is not a clean number (the layer garbles the
underlined last row of each table: 'lllliIB', '2D..521') are left null, i.e.
unconfirmed by this key.

Output: microfiche/review/bluebook_1996_textlayer.json
"""
import csv, difflib, json, os, re, subprocess

HERE = os.path.dirname(os.path.abspath(__file__))
TN = os.path.join(HERE, "../data/county_house_files/tennessee")
PDF = os.path.join(TN, "TN BLUE BOOK 1997-1998.pdf")
COUNTIES = sorted({r["county"] for r in csv.DictReader(open(os.path.join(TN, "tennessee_house_county_1998.csv")))})
SQ = {c.replace(" ", ""): c for c in COUNTIES}
NUM = re.compile(r"^\d{1,3}(,\d{3})*$")

text = subprocess.run(["pdftotext", "-f", "545", "-l", "548", "-layout", PDF, "-"],
                      capture_output=True, text=True).stdout
out, d = {}, None
for line in text.splitlines():
    m = re.search(r"DISTRICT\s+(\d)\b", line)
    if m and "TOTAL" not in line.upper():
        d = m.group(1)
        out[d] = {"rows": [], "total": None}
        continue
    if d is None:
        continue
    toks = line.split()
    if len(toks) < 3:
        continue
    # leading name tokens = everything before the first token containing a digit
    k = next((i for i, t in enumerate(toks) if re.search(r"\d", t)), len(toks))
    name = re.sub(r"[^A-Z]", "", "".join(toks[:k]).upper())
    vals = [t.replace(",", "") if NUM.match(t) else None for t in toks[k:]]
    if not name or not vals:
        continue
    if difflib.SequenceMatcher(None, name, "TOTAL").ratio() > 0.6 and len(name) <= 7:
        out[d]["total"] = vals
        continue
    hit = difflib.get_close_matches(name, SQ, n=1, cutoff=0.7)
    if hit:
        out[d]["rows"].append({"county": SQ[hit[0]], "raw": line.strip(), "values": vals})

json.dump(out, open(os.path.join(TN, "microfiche/review/bluebook_1996_textlayer.json"), "w"), indent=1)
for d, v in out.items():
    cells = [x for r in v["rows"] for x in r["values"]]
    print(f"D{d}: {len(v['rows'])} rows, {sum(x is not None for x in cells)}/{len(cells)} clean cells, total parsed: {v['total'] is not None}")
