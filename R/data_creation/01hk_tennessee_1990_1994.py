"""Tennessee U.S. House 1990, 1992, 1994, county level, from two state publications:

  * Tennessee Blue Book 1991-1994 (1990: PDF pp. 465-468; 1992: pp. 496-501) and
    1995-1996 (1994: pp. 543-544): typeset county tables, transcribed from 300 dpi
    page images and checked against the PDF text layer -> microfiche/review/bluebook_<yr>.json
  * Secretary of State 'Certification of Election Returns' typescript on SRI microfiche
    (Internet Archive micro_IA40706939_0340 / _0247 / _0275), read by eye with the
    01hb-01hd review tool -> microfiche/review/vision_<yr>.json. Its printed district
    totals are the certified ones and equal the FEC's.

Values are the Blue Book's, except the cells in OVERRIDES where the Blue Book is
wrong (its column does not add up, or it disagrees with the certification and the
FEC) and the microfiche value is taken instead. Stops unless every candidate
column of every district adds up to the certified (microfiche) district total.
Candidate names and parties come from the microfiche district headers. Write-ins
are dropped, as for 1996 and 1998.

Output: tennessee/tennessee_house_county_1990_1994.csv (layout of the 1998 file)
"""
import csv, json, os, re

HERE = os.path.dirname(os.path.abspath(__file__))
TN = os.path.join(HERE, "../data/county_house_files/tennessee")
REV = os.path.join(TN, "microfiche/review")

# (year, district, county, microfiche column index 0-based) -> value
OVERRIDES = {
    # Blue Book prints 13,102 (its table adds up to 61,179); certification and FEC: 14,164 / 62,241
    ("1990", "8", "MADISON", 0): 14164,
    # Blue Book 71 leaves the Hartley column 38 short; microfiche 109 closes it
    ("1992", "1", "JEFFERSON", 3): 109,
    # Blue Book 10,072 (transposed digits) leaves the Davis column 630 short; microfiche 10,702 ties
    ("1992", "7", "MONTGOMERY", 0): 10702,
    # Blue Book 1,256 (transposed) leaves the Ward column 9 short; 1,265 is the only single value that ties
    # (the microfiche cell is blurred: read 1,285, which is 20 over)
    ("1992", "8", "MADISON", 5): 1265,
}
num = lambda s: int(str(s).replace(",", ""))
norm = lambda c: re.sub(r"[^A-Z]", "", c.upper())


def microfiche_districts(yr):
    """{district: {'rows': {county: values}, 'total': values}} from the visual reading."""
    out, d = {}, 1
    for r in json.load(open(os.path.join(REV, f"vision_{yr}.json")))["rows"]:
        if r["kind"] == "ignore":
            continue
        dd = out.setdefault(str(d), {"rows": {}, "total": None})
        if r["kind"] == "total":
            dd["total"] = r["values"]
            d += 1
        else:
            dd["rows"][norm(r["county"])] = r["values"]
    return out


rows_out, used = [], set()
for yr in ("1990", "1992", "1994"):
    bb = json.load(open(os.path.join(REV, f"bluebook_{yr}.json")))["districts"]
    cols_all = json.load(open(os.path.join(REV, f"vision_{yr}.json")))["districts"]
    mf = microfiche_districts(yr)
    n_diff = 0
    for d in sorted(bb, key=int):
        cols = cols_all[d]["columns"]
        cert = mf[d]["total"]
        final = {}
        for r in bb[d]["rows"]:
            c = r["county"]
            vals = []
            for j, v in enumerate(r["values"]):
                if v is None:  # write-ins: not in the Blue Book, dropped
                    vals.append(None)
                    continue
                key = (yr, d, c, j)
                bv = num(v)
                mv = mf[d]["rows"].get(norm(c))
                mv = num(mv[j]) if mv and mv[j] not in (None, "") else None
                if key in OVERRIDES:
                    used.add(key)
                    vals.append(OVERRIDES[key])
                    print(f"  {yr} D{d} {c} col{j+1}: Blue Book {bv} -> {OVERRIDES[key]} (microfiche read {mv})")
                else:
                    vals.append(bv)
                    if mv is not None and mv != bv:
                        n_diff += 1
                        print(f"  {yr} D{d} {c} col{j+1}: Blue Book {bv} kept (blurred microfiche read {mv})")
            final[c] = vals
        for j, col in enumerate(cols):
            if col["party"] == "W" or col["candidate"].lower().startswith("write"):
                continue
            s = sum(v[j] for v in final.values())
            assert s == num(cert[j] or 0), (yr, d, j, s, cert[j])
            if not col["candidate"]:
                assert s == 0, (yr, d, j)
                continue
            for c, v in final.items():
                rows_out.append(dict(year=int(yr), district=int(d), county=c, candidate=col["candidate"],
                                     party_code=col["party"] or "I", votes=v[j]))
    print(f"{yr}: all 9 districts tie to the certified totals; {n_diff} other cells where the blurred microfiche reading differed")
assert used == set(OVERRIDES), set(OVERRIDES) - used
for yr in (1990, 1992, 1994):
    assert len({r["county"] for r in rows_out if r["year"] == yr}) == 95, yr
with open(os.path.join(TN, "tennessee_house_county_1990_1994.csv"), "w", newline="") as f:
    w = csv.DictWriter(f, fieldnames=["year", "district", "county", "candidate", "party_code", "votes"])
    w.writeheader()
    w.writerows(rows_out)
print(len(rows_out), "candidate-county rows")
