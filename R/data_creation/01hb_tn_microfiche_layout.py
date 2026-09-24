"""Tennessee House 1990/1992/1994 from the SRI microfiche (Internet Archive).

Step 1 of the manual-verification build: locate every county row on the
U.S. House pages, cut a row image for each and store a draft reading. The
verification tool (01hc_tn_microfiche_review.py) serves these rows so each
cell can be confirmed by eye.

Sources (Internet Archive, collection statistical-reference-index):
  1990 micro_IA40706939_0340  Certification...Returns...Nov. 6, 1990 (jp2 pages 7-10)
  1992 micro_IA40706946_0247  Certification of Election Returns, Nov. 3, 1992 (pages 9-13; 14 is the TN Senate)
  1994 micro_IA40706953_0275  Certification of Election Returns, Nov. 8, 1994 (pages 11-14)
Pages are extracted from <id>_jp2.zip to microfiche/pages/tn<year>_<nn>.png.

Method: text lines are found from the page's ink profile (whole-page OCR
misses rows on these scans); each line is OCRed on its own (tesseract --psm 7)
to get the county name and a draft of the numbers. Districts follow the order
of the 'District Total' rows. Drafts are only a starting point: every cell is
confirmed in the review tool.

Output: microfiche/review/rows.json (row boxes refer to pages/<page>_deskew.png)
"""
import csv, json, os, re, difflib, subprocess, tempfile
from concurrent.futures import ThreadPoolExecutor
import numpy as np
from PIL import Image, ImageOps

BASE = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                    "../data/county_house_files/tennessee/microfiche")
PAGES = {"1990": [7, 8, 9, 10], "1992": [9, 10, 11, 12, 13],
         "1994": [11, 12, 13, 14]}
NCOLS = {"1990": 6, "1992": 7, "1994": 5}

COUNTIES = sorted({r["county"] for r in csv.DictReader(open(os.path.join(
    BASE, "../tennessee_house_county_1998.csv")))})
assert len(COUNTIES) == 95, len(COUNTIES)
SQUASHED = {c.replace(" ", ""): c for c in COUNTIES}


def bands(a):
    """Text-line bands (y0, y1, x0, x1) from the ink profile of the paper area."""
    colmean = a.mean(0)
    paper = np.where(colmean > 150)[0]
    px0, px1 = paper.min() + 40, paper.max() - 40
    region = a[:, px0:px1]
    thr = np.percentile(region, 50) - 70  # paper is the median; ink is well below it
    ink = region < thr
    prof = ink.sum(1)
    on = prof >= 4
    out, y = [], 0
    while y < len(on):
        if on[y]:
            y0 = y
            gap = 0
            while y < len(on) and (on[y] or gap < 6):
                gap = 0 if on[y] else gap + 1
                y += 1
            y1 = y - gap
            # touching rows (smudges, descenders) merge into one tall band:
            # split it at the weakest profile rows, one cut per ~row pitch
            pieces = [(y0, y1)]
            while any(b - a > 70 for a, b in pieces):
                a, b = next((a, b) for a, b in pieces if b - a > 70)
                cut = a + 22 + int(np.argmin(prof[a + 22:b - 22]))
                i = pieces.index((a, b))
                pieces[i:i + 1] = [(a, cut), (cut, b)]
            for a, b in pieces:
                if 18 <= b - a <= 70:
                    xs = np.where(ink[a:b].any(0))[0]
                    out.append((a, b, px0 + xs.min(), px0 + xs.max()))
        y += 1
    return out


def deskew(im):
    """Rotate so text lines are horizontal: maximise the sharpness of the
    row ink profile, scored only inside the paper (the black film borders
    would otherwise dominate the score)."""
    a = np.array(im)
    xs = np.where(a.mean(0) > 150)[0]
    ys = np.where(a.mean(1) > 150)[0]
    x0, x1, y0, y1 = xs.min() + 80, xs.max() - 80, ys.min() + 80, ys.max() - 80
    paper = im.crop((x0, y0, x1, y1))
    small = paper.resize((paper.width // 3, paper.height // 3))
    thr = np.percentile(np.array(small), 50) - 60
    best = (-1, 0.0)
    for ang in np.arange(-3, 3.01, 0.1):
        r = np.array(small.rotate(ang, resample=Image.BILINEAR, fillcolor=255))
        w = r.shape[1]
        c = r[:, w // 8: -w // 8]
        prof = (c < thr).sum(1).astype(float)
        best = max(best, (float(np.sum(np.diff(prof) ** 2)), ang))
    return im.rotate(best[1], resample=Image.BICUBIC, fillcolor=255), round(best[1], 1)


def ocr(img):
    img = ImageOps.autocontrast(img, cutoff=1)
    img = img.resize((img.width * 2, img.height * 2), Image.LANCZOS)
    with tempfile.NamedTemporaryFile(suffix=".png") as f:
        img.save(f.name)
        r = subprocess.run(["tesseract", f.name, "-", "--psm", "7"],
                           capture_output=True, text=True)
    return r.stdout.strip()


HEADER = re.compile(r"(?i)tenness|state of|united stat|house of|democr|republ|independ|candidat|write|election|novemb|page|\d\.\s+[A-Z][a-z]|^\s*\d\.\s+\d\.|:\d\d:")


def classify(text):
    """('county', name, rest) | ('total', None, rest) | None"""
    up = text.upper()
    m = re.search(r"TOTA\w*", up)
    if m and re.search(r"D\W?[I1lL]\W?S|STR[I1]CT|[I1]CT\b|DISTR", up[:m.start()]):
        return "total", None, text[m.end():]
    m = re.match(r"^[^A-Za-z]{0,4}([A-Za-z][A-Za-z .'-]*?)\s*(?=[\d,.:;|]|$)", text)
    if not m:
        return None
    name = re.sub(r"[^A-Z]", "", m.group(1).upper())
    if len(name) < 3:
        return None
    hit = difflib.get_close_matches(name, SQUASHED, n=1, cutoff=0.7)
    if not hit:
        return None
    return "county", SQUASHED[hit[0]], text[m.end():]


def nums(rest):
    toks = []
    for t in rest.split():
        t = t.replace("O", "0").replace("o", "0").replace("D", "0")
        t = t.replace("l", "1").replace("I", "1").replace("]", "1").replace("|", "")
        t = re.sub(r"[,.'`]", "", t)
        toks.append(t if re.fullmatch(r"\d{1,7}", t) else "?")
    return toks


def main():
    os.makedirs(os.path.join(BASE, "review"), exist_ok=True)
    rows = []
    for yr, pages in PAGES.items():
        for pn in pages:
            page = f"tn{yr}_{pn:02d}"
            im, ang = deskew(Image.open(os.path.join(BASE, "pages", page + ".png")))
            im.save(os.path.join(BASE, "pages", page + "_deskew.png"))
            print(page, "deskew", ang)
            a = np.array(im)
            bs = bands(a)
            crops = [im.crop((b[2] - 20, b[0] - 10, b[3] + 20, b[1] + 10)) for b in bs]
            with ThreadPoolExecutor(16) as ex:
                texts = list(ex.map(ocr, crops))
            for b, t in zip(bs, texts):
                c = classify(t)
                if not c and len(re.findall(r"\d[\d,]*", t)) >= 2 and not HEADER.search(t) \
                        and b[3] - b[2] > 0.4 * a.shape[1]:
                    c = ("unknown", "?", re.sub(r"^\D*", "", t))
                if c:
                    rows.append(dict(year=yr, page=page, kind=c[0], county=c[1] or "DISTRICT TOTAL",
                                     y=int(b[0]),
                                     ocr=t, draft=nums(c[2]), box=[int(b[2]) - 30, int(b[0]) - 14,
                                                                    int(b[3]) + 30, int(b[1]) + 14]))
    # Same x-range for every row of a page (columns line up in the review
    # tool); gaps in the regular row pitch get placeholder rows (county '?'),
    # which catches rows the ink profile merged on the blurred pages.
    filled = []
    for page in sorted({r["page"] for r in rows}):
        rs = sorted([r for r in rows if r["page"] == page], key=lambda r: r["y"])
        data = [r for r in rs if r["kind"] != "unknown" or r["draft"].count("?") < len(r["draft"])]
        xl = int(np.percentile([r["box"][0] for r in data], 10))
        xr = int(np.percentile([r["box"][2] for r in data], 90))
        diffs = np.diff([r["y"] for r in rs])
        pitch = float(np.median([d for d in diffs if d < 100])) if any(diffs < 100) else 57.0
        for r in rs:
            r["box"][0], r["box"][2] = xl, xr
        for r, nxt in zip(rs, rs[1:] + [None]):
            filled.append(r)
            if nxt is None:
                continue
            gap = nxt["y"] - r["y"]
            n_missing = int(round(gap / pitch)) - 1
            if nxt["kind"] == "total" and gap < 2.6 * pitch:  # separator line before totals
                continue
            if 1 <= n_missing <= 3:
                for k in range(1, n_missing + 1):
                    y = int(r["y"] + k * gap / (n_missing + 1))
                    h = r["box"][3] - r["box"][1]
                    filled.append(dict(year=r["year"], page=page, kind="unknown", county="?",
                                       y=y, ocr="(gap fill)", draft=[],
                                       box=[xl, y - 14, xr, y - 14 + h]))
        print(page, "pitch", round(pitch, 1), "rows", len(rs), "->",
              len([f for f in filled if f["page"] == page]))
    out = []
    for yr in PAGES:
        rs = [r for r in filled if r["year"] == yr]
        k = 1
        for i, r in enumerate(rs):
            r["district"] = k
            if r["kind"] == "total":
                k += 1
            r["id"] = f"{yr}_{i:03d}"
            r["draft"] = r["draft"][:NCOLS[yr]] + [""] * max(0, NCOLS[yr] - len(r["draft"]))
            r["draft"] = ["" if v == "?" else v for v in r["draft"]]
            out.append(r)
        print(yr, len(rs), "rows,", k - 1, "district totals (expect 9);",
              "counties", len({r["county"] for r in rs if r["kind"] == "county"}))
    json.dump(out, open(os.path.join(BASE, "review/rows.json"), "w"), indent=1)


if __name__ == "__main__":
    main()
