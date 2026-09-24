"""Local review tool for the Tennessee House 1990/1992/1994 microfiche.

Serves tn_microfiche_review.html plus the deskewed page scans; every edit is
saved to microfiche/review/state.json (the verified transcription). Rows start
from review/rows.json (01hb_tn_microfiche_layout.py); a saved state wins.

Run:  python3 R/data_creation/01hc_tn_microfiche_review.py   -> http://localhost:8765
"""
import csv, json, os, shutil, time
from PIL import Image
from http.server import ThreadingHTTPServer, SimpleHTTPRequestHandler

HERE = os.path.dirname(os.path.abspath(__file__))
BASE = os.path.join(HERE, "../data/county_house_files/tennessee/microfiche")
REVIEW = os.path.join(BASE, "review")
STATE = os.path.join(REVIEW, "state.json")
NCOLS = {"1990": 6, "1992": 7, "1994": 5}
FEC = os.path.join(HERE, "../data/fec_official")


def fec_totals():
    out = {}
    for yr in NCOLS:
        f = os.path.join(FEC, f"fec{yr}_house.csv")
        if not os.path.exists(f):
            continue
        for r in csv.DictReader(open(f)):
            if r["state_po"] != "TN" or not r["district"]:
                continue
            d = out.setdefault(yr, {}).setdefault(r["district"], [])
            if r["is_total"] == "True":
                d.append({"candidate": "TOTAL", "party": "", "votes": int(float(r["total"]))})
            else:
                d.append({"candidate": r["candidate"], "party": r["party"], "votes": int(float(r["votes"]))})
    return out


def page_columns():
    """x-ranges of the vote columns per page, from the vertical ink profile of
    the detected rows (the n heaviest runs right of the county names). Used
    only to place the input boxes under their column in the review page."""
    cache = os.path.join(REVIEW, "columns.json")
    if os.path.exists(cache):
        return json.load(open(cache))
    import numpy as np
    rows = json.load(open(os.path.join(REVIEW, "rows.json")))
    out = {}
    for page in sorted({r["page"] for r in rows}):
        rs = [r for r in rows if r["page"] == page and r["kind"] in ("county", "total")]
        if len(rs) < 3:
            continue
        im = np.array(Image.open(os.path.join(BASE, "pages", page + "_deskew.png")))
        x0, x1 = rs[0]["box"][0], rs[0]["box"][2]
        prof = np.zeros(x1 - x0)
        for r in rs:
            b = r["box"]
            strip = im[b[1] + 10:b[3] - 10, x0:x1]
            prof += (strip < np.percentile(strip, 50) - 60).any(0)
        on = prof >= max(2, len(rs) * 0.15)
        runs, i = [], 0
        while i < len(on):
            if on[i]:
                j = i
                while j < len(on) and on[j:j + 35].any():
                    j += 1
                runs.append((i, j, float(prof[i:j].sum())))
                i = j
            else:
                i += 1
        n = NCOLS[page[2:6]]
        cols = sorted(sorted(runs[1:], key=lambda r: -r[2])[:n])
        out[page] = [[x0 + a, x0 + b] for a, b, _ in cols]
    for page in {r["page"] for r in rows} - set(out):  # too few rows: borrow the previous page's
        prev = max(p for p in out if p < page and p[:6] == page[:6])
        out[page] = out[prev]
    json.dump(out, open(cache, "w"))
    return out


def initial_state():
    rows = json.load(open(os.path.join(REVIEW, "rows.json")))
    st = {}
    for r in rows:
        st.setdefault(r["year"], []).append(dict(
            id=r["id"], page=r["page"], box=r["box"], kind=r["kind"], county=r["county"],
            values=r["draft"], draft=list(r["draft"]), ocr=r["ocr"], verified=False, note=""))
    return st


class H(SimpleHTTPRequestHandler):
    def log_message(self, *a):
        pass

    def send_json(self, obj):
        b = json.dumps(obj).encode()
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(b)))
        self.end_headers()
        self.wfile.write(b)

    def do_GET(self):
        if self.path in ("/", "/index.html"):
            b = open(os.path.join(HERE, "tn_microfiche_review.html"), "rb").read()
            self.send_response(200)
            self.send_header("Content-Type", "text/html; charset=utf-8")
            self.end_headers()
            self.wfile.write(b)
        elif self.path == "/api/state":
            st = json.load(open(STATE)) if os.path.exists(STATE) else initial_state()
            counties = sorted({r["county"] for r in csv.DictReader(open(
                os.path.join(BASE, "../tennessee_house_county_1998.csv")))})
            sizes = {f[:-11]: Image.open(os.path.join(BASE, "pages", f)).size
                     for f in os.listdir(os.path.join(BASE, "pages")) if f.endswith("_deskew.png")}
            bluebook = {}
            for yr in NCOLS:
                f = os.path.join(REVIEW, f"bluebook_{yr}.json")
                if os.path.exists(f):
                    bluebook[yr] = json.load(open(f))
            self.send_json(dict(state=st, bluebook=bluebook, ncols=NCOLS, fec=fec_totals(), counties=counties, sizes=sizes,
                                columns=page_columns()))
        elif self.path.startswith("/pages/"):
            f = os.path.join(BASE, "pages", os.path.basename(self.path))
            if not os.path.exists(f):
                self.send_error(404)
                return
            self.send_response(200)
            self.send_header("Content-Type", "image/png")
            self.send_header("Cache-Control", "max-age=86400")
            self.end_headers()
            with open(f, "rb") as fh:
                shutil.copyfileobj(fh, self.wfile)
        else:
            self.send_error(404)

    def do_POST(self):
        if self.path != "/api/save":
            self.send_error(404)
            return
        body = self.rfile.read(int(self.headers["Content-Length"]))
        st = json.loads(body)
        tmp = STATE + ".tmp"
        with open(tmp, "w") as f:
            json.dump(st, f, indent=0)
        if os.path.exists(STATE):  # one rolling backup per minute
            shutil.copy(STATE, os.path.join(REVIEW, f"state_backup_{time.strftime('%Y%m%d_%H%M')}.json"))
        os.replace(tmp, STATE)
        self.send_json({"ok": True, "saved": time.strftime("%H:%M:%S")})


if __name__ == "__main__":
    print("Tennessee microfiche review: http://localhost:8765")
    ThreadingHTTPServer(("127.0.0.1", 8765), H).serve_forever()
