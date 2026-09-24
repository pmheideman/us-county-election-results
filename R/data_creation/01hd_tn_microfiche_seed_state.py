"""Seed the review tool's state.json from the layout rows (01hb) plus the
visual readings in review/vision_<year>.json. The visual reading becomes the
draft; the OCR draft is kept as `ocrv` so the tool can ring cells where the two
disagree. Refuses to overwrite a state.json that already has verified rows.
"""
import json, os, sys
import numpy as np

HERE = os.path.dirname(os.path.abspath(__file__))
REVIEW = os.path.join(HERE, "../data/county_house_files/tennessee/microfiche/review")
NCOLS = {"1990": 6, "1992": 7, "1994": 5}

state_f = os.path.join(REVIEW, "state.json")
if os.path.exists(state_f):
    old = json.load(open(state_f))
    if any(r["verified"] for y in old for r in old[y]) and "--force" not in sys.argv:
        sys.exit("state.json has verified rows; not overwriting (use --force)")

rows = json.load(open(os.path.join(REVIEW, "rows.json")))
state = {}
for yr, n in NCOLS.items():
    base = {r["id"]: r for r in rows if r["year"] == yr}
    vf = os.path.join(REVIEW, f"vision_{yr}.json")
    vis = json.load(open(vf))["rows"] if os.path.exists(vf) else []
    out, used = [], set()
    for k, v in enumerate(vis):
        vals = [str(x).replace(",", "") if x is not None else "" for x in v.get("values", [])]
        vals = (vals + [""] * n)[:n]
        if v.get("id") and v["id"] in base:
            r = base[v["id"]]
            used.add(v["id"])
            box, rid, ocr, ocrv = r["box"], r["id"], r["ocr"], r["draft"]
        else:
            same = [r for r in base.values() if r["page"] == v["page"]]
            h = int(np.median([r["box"][3] - r["box"][1] for r in same])) if same else 76
            x0 = same[0]["box"][0] if same else 300
            x1 = same[0]["box"][2] if same else 2700
            box = [x0, int(v["y"]) - 14, x1, int(v["y"]) - 14 + h]
            rid, ocr, ocrv = f"{yr}_v{k:03d}", "(added by visual reading)", None
        out.append(dict(id=rid, page=v["page"], box=box, kind=v.get("kind", "county"),
                        county=v.get("county", "?"), values=vals, draft=list(vals),
                        vision=list(vals), ocrv=ocrv, unsure=v.get("unsure", []),
                        ocr=ocr, verified=False, note=v.get("note", "")))
    for rid, r in base.items():  # layout rows the reader did not mention
        if rid not in used:
            out.append(dict(id=rid, page=r["page"], box=r["box"], kind="ignore" if vis else r["kind"],
                            county=r["county"], values=r["draft"], draft=list(r["draft"]),
                            vision=None, ocrv=r["draft"], unsure=[], ocr=r["ocr"], verified=False,
                            note="not in visual reading" if vis else ""))
    out.sort(key=lambda r: (r["page"], r["box"][1]))
    state[yr] = out
    print(yr, len(out), "rows;", sum(r["vision"] is not None for r in out), "with visual reading;",
          sum(1 for r in out if r["vision"] and r["ocrv"] and any(
              (a or "") != (b or "") for a, b in zip(r["vision"], r["ocrv"]) if b)), "rows with OCR disagreement")
json.dump(state, open(state_f, "w"), indent=0)
