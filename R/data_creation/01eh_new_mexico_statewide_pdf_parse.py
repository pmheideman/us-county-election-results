## Parses New Mexico's born-digital statewide "Canvass of Returns" PDFs (2004: NM_StatewideGen04.pdf,
## 2006: NM_StatewideGen06.pdf, R/data/county_house_files/) after `pdftotext -layout`. These print
## ONE wide table per office: 33 counties as columns (fixed left-to-right order, reconstructed here
## from the two-line county-name header since some names wrap to a second line to fit the page
## width), one row per candidate, ending in a "TOTAL FOR EACH CANDIDATE" column with a pre-printed
## total used to verify the transcription (every row's county-sum must equal it exactly).
##
## The U.S. House section is isolated by text between the first "UNITED STATES REPRESENTATIVE" and
## the next office title -- which is NOT the same office in every year (2004: "JUSTICE OF THE
## SUPREME COURT"; 2006: "GOVERNOR and LIEUTENANT GOVERNOR", since NM's ballot order after U.S.
## House depends on which offices are up that year) -- see END_MARKERS below; add an entry there
## for any further year run through this parser.
##
## Run as `pdftotext -layout <pdf> <txt>` first, then
## `python3 01eh_new_mexico_statewide_pdf_parse.py <txt> <year> <output_csv>`.
import re, sys, csv

COUNTIES_DISPLAY =["Bernalillo","Catron","Chaves","Cibola","Colfax","Curry","DeBaca","Dona Ana",
    "Eddy","Grant","Guadalupe","Harding","Hidalgo","Lea","Lincoln","Los Alamos","Luna","McKinley",
    "Mora","Otero","Quay","Rio Arriba","Roosevelt","Sandoval","San Juan","San Miguel","Santa Fe",
    "Sierra","Socorro","Taos","Torrance","Union","Valencia"]

def find_county_order(line1, line2):
    positions = []
    for c in COUNTIES_DISPLAY:
        p1 = line1.find(c)
        p2 = line2.find(c)
        pos = p1 if p1 >= 0 else p2
        if pos < 0:
            raise ValueError(f"county not found in header: {c}")
        positions.append((pos, c))
    positions.sort()
    return [c for _, c in positions]

END_MARKERS = {2004: "JUSTICE OF THE SUPREME COURT", 2006: "GOVERNOR and LIEUTENANT GOVERNOR"}

def parse_file(path, year):
    with open(path) as f:
        text = f.read()
    lines = text.split("\n")
    hdr_i = None
    for i, l in enumerate(lines):
        if re.search(r'\bBernalillo\b', l):
            hdr_i = i
            break
    if hdr_i is None:
        raise ValueError("header not found")
    line1 = lines[hdr_i]
    line2 = ""
    for j in range(hdr_i+1, hdr_i+6):
        if re.search(r'\bCurry\b', lines[j]):
            line2 = lines[j]
            break
    order = find_county_order(line1, line2)
    assert len(order) == 33, len(order)

    # isolate just the US HOUSE section: from the first "UNITED STATES REPRESENTATIVE" to the
    # next "JUSTICE OF THE SUPREME COURT" (the office that always immediately follows it in these PDFs)
    end_marker = END_MARKERS[year]
    start = end = None
    for i, l in enumerate(lines):
        if start is None and re.search(r'UNITED STATES REPRESENTATIVE', l, re.IGNORECASE):
            start = i
        elif start is not None and end_marker in l:
            end = i
            break
    if end is None:
        end = len(lines)
    section = lines[start:end]

    rows = []
    current_district = None
    i = 0
    while i < len(section):
        l = section[i]
        if re.search(r'UNITED STATES REPRESENTATIVE', l, re.IGNORECASE):
            m = re.search(r'DISTRICT\s+(\d+)', l, re.IGNORECASE)
            if not m and i+1 < len(section):
                m = re.search(r'DISTRICT\s+(\d+)', section[i+1], re.IGNORECASE)
                if m:
                    i += 1
            current_district = int(m.group(1)) if m else None
        elif current_district is not None:
            m = re.match(r'^\s*([A-Za-z][A-Za-z.,\'\-\s()]*?)\s+([\d\s,]+)\s*$', l)
            if m and re.search(r'\d', m.group(2)):
                name = m.group(1).strip()
                nums = [int(x.replace(",", "")) for x in re.findall(r'[\d,]+', m.group(2))]
                if len(nums) == 34:
                    county_votes = dict(zip(order, nums[:33]))
                    total = nums[33]
                    party = None
                    for k in range(i+1, min(i+4, len(section))):
                        if section[k].strip():
                            party = section[k].strip()
                            break
                    rows.append({"year": year, "district": current_district, "candidate": name,
                                 "party": party, "total_printed": total, **county_votes})
        i += 1
    return rows, order

if __name__ == "__main__":
    path, year, outcsv = sys.argv[1], sys.argv[2], sys.argv[3]
    rows, order = parse_file(path, int(year))
    print(f"parsed {len(rows)} candidate rows")
    for r in rows:
        cty_sum = sum(r[c] for c in order)
        match = "OK" if cty_sum == r["total_printed"] else f"MISMATCH sum={cty_sum}"
        print(r["district"], r["candidate"], r["party"], "printed_total=", r["total_printed"], match)
    if rows:
        keys = list(rows[0].keys())
        with open(outcsv, "w", newline="") as f:
            w = csv.DictWriter(f, fieldnames=keys)
            w.writeheader()
            w.writerows(rows)
