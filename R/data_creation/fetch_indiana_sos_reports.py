#!/usr/bin/env python3
"""Download Indiana Secretary of State General Election Reports (1950-2000)
from the Indiana State Library's Indiana Memory / CONTENTdm collection and
assemble each report into a single PDF.

Source collection: "Indiana State Agency Documents" (p16066coll37),
sub-collection "Secretary of State General Election Reports"
  https://cdm16066.contentdm.oclc.org/digital/collection/p16066coll37/

Each report is a CONTENTdm compound object: an ordered list of full-resolution
JPEG2000 page scans (~10 MB each, 400 dpi). This script
  1. lists the reports via the CONTENTdm search API,
  2. reads each report's ordered page list (the compound object's CPD XML),
  3. downloads every page (resumable; skips finished files),
  4. wraps the pages, in order, into one PDF per report with img2pdf. The JP2
     bytes are embedded unchanged (no re-encoding, no quality loss).

It does NO data extraction. Output layout (under --out):
  indiana_election_report_<year>.pdf   assembled reports
  reports.csv                          one row per report (metadata, page count)
  manifest.csv                         one row per page: PDF page number <->
                                       CONTENTdm page id and section title
  pages/<year>/NNN_<id>.jp2            page scans (deleted after a successful
                                       PDF build unless --keep-pages)

Usage:
  python fetch_indiana_sos_reports.py --list
  python fetch_indiana_sos_reports.py                  # everything
  python fetch_indiana_sos_reports.py --years 1950 1952
Re-running is safe: finished PDFs are skipped (use --force to rebuild).
Needs: requests, img2pdf (and pdfinfo from poppler for the page-count check).
"""
import argparse
import csv
import re
import shutil
import subprocess
import sys
import time
import xml.etree.ElementTree as ET
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path

import img2pdf
import requests

BASE = "https://cdm16066.contentdm.oclc.org"
COLLECTION = "p16066coll37"
SEARCH_TERM = "Secretary of State General Election Reports"
TITLE_PREFIX = "Election report of Indiana"
JP2_MAGIC = b"\x00\x00\x00\x0cjP  "
DPI = 400  # per the collection's digitization metadata

DEFAULT_OUT = Path(__file__).resolve().parents[1] / "data" / "indiana_sos_reports"

session = requests.Session()
session.headers["User-Agent"] = "Mozilla/5.0 (research download; contact: pmheideman@gmail.com)"


def get(url, retries=5, **kw):
    for attempt in range(retries):
        try:
            r = session.get(url, timeout=120, **kw)
            r.raise_for_status()
            return r
        except requests.RequestException as e:
            if attempt == retries - 1:
                raise
            wait = 2 ** attempt * 2
            print(f"    retry in {wait}s ({e})", file=sys.stderr)
            time.sleep(wait)


def list_items():
    """All items in the sub-collection search, as [{id, title}]."""
    term = requests.utils.quote(SEARCH_TERM)
    url = (f"{BASE}/digital/api/search/collection/{COLLECTION}/searchterm/{term}"
           f"/field/all/mode/all/conn/and/order/title/ad/asc/maxRecords/100")
    d = get(url).json()
    if d["totalResults"] > len(d["items"]):
        sys.exit(f"search returned {len(d['items'])} of {d['totalResults']} items; add paging")
    return [{"id": it["itemId"], "title": it["title"]} for it in d["items"]]


def item_metadata(item_id):
    d = get(f"{BASE}/digital/api/collections/{COLLECTION}/items/{item_id}/false").json()
    return {f["key"]: f["value"] for f in d["parent"]["fields"]}


def page_list(item_id):
    """Ordered [(page_id, title)] from the compound object's CPD XML, or None."""
    r = get(f"{BASE}/digital/api/collection/{COLLECTION}/id/{item_id}/download")
    if not r.content.lstrip(b"\xef\xbb\xbf").startswith(b"<?xml"):
        return None  # a single image, not a compound object
    root = ET.fromstring(r.content)
    return [(p.findtext("pageptr"), (p.findtext("pagetitle") or "").strip())
            for p in root.findall("page")]


def download_page(page_id, dest, delay):
    if dest.exists():
        return
    part = dest.with_suffix(".part")
    with session.get(f"{BASE}/digital/api/collection/{COLLECTION}/id/{page_id}/download",
                     stream=True, timeout=120) as r:
        r.raise_for_status()
        expected = int(r.headers.get("Content-Length", 0)) or None
        with open(part, "wb") as f:
            for chunk in r.iter_content(1 << 20):
                f.write(chunk)
    size = part.stat().st_size
    with open(part, "rb") as f:
        magic = f.read(len(JP2_MAGIC))
    if magic != JP2_MAGIC or (expected and size != expected):
        part.unlink()
        raise IOError(f"page {page_id}: bad download (size {size}, expected {expected})")
    part.rename(dest)
    time.sleep(delay)


def download_with_retry(page_id, dest, delay, retries=4):
    for attempt in range(retries):
        try:
            return download_page(page_id, dest, delay)
        except (requests.RequestException, IOError) as e:
            if attempt == retries - 1:
                raise
            wait = 2 ** attempt * 3
            print(f"    page {page_id}: {e}; retry in {wait}s", file=sys.stderr)
            time.sleep(wait)


def pdf_pages(pdf):
    try:
        out = subprocess.run(["pdfinfo", str(pdf)], capture_output=True, text=True, check=True).stdout
        return int(re.search(r"Pages:\s+(\d+)", out).group(1))
    except (FileNotFoundError, subprocess.CalledProcessError, AttributeError):
        return None


def read_csv(path):
    if not path.exists():
        return []
    with open(path, newline="") as f:
        return list(csv.DictReader(f))


def write_csv(path, rows, fields):
    rows = sorted(rows, key=lambda r: (int(r["year"]), int(r.get("page", 0))))
    with open(path, "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=fields)
        w.writeheader()
        w.writerows(rows)


REPORT_FIELDS = ["year", "item_id", "title", "identifier", "local_identifier", "pages",
                 "pdf", "source_url", "description"]
PAGE_FIELDS = ["year", "page", "page_id", "page_title", "jp2_bytes", "pdf"]


def process(item, year, out, args, reports, manifest):
    pdf = out / f"indiana_election_report_{year}.pdf"
    pages_dir = out / "pages" / str(year)
    print(f"[{year}] {item['title']} (id {item['id']})")

    pages = page_list(item["id"])
    if not pages:
        print("    not a compound object with pages; skipping", file=sys.stderr)
        return False

    if pdf.exists() and not args.force and pdf_pages(pdf) in (len(pages), None):
        print(f"    already assembled ({len(pages)} pages)")
        return True

    pages_dir.mkdir(parents=True, exist_ok=True)
    files = [pages_dir / f"{n:03d}_{pid}.jp2" for n, (pid, _) in enumerate(pages, 1)]
    with ThreadPoolExecutor(args.workers) as ex:
        futures = [ex.submit(download_with_retry, pid, dest, args.delay)
                   for (pid, _), dest in zip(pages, files)]
        for n, fut in enumerate(futures, 1):
            fut.result()  # re-raises download errors
            print(f"    page {n}/{len(pages)}", end="\r", flush=True)
    print()

    layout = img2pdf.get_fixed_dpi_layout_fun((DPI, DPI))
    tmp = pdf.with_suffix(".pdf.part")
    with open(tmp, "wb") as f:
        f.write(img2pdf.convert([str(p) for p in files], layout_fun=layout))
    got = pdf_pages(tmp)
    if got is not None and got != len(pages):
        tmp.unlink()
        raise RuntimeError(f"{year}: PDF has {got} pages, expected {len(pages)}")
    tmp.rename(pdf)

    meta = item_metadata(item["id"])
    reports[year] = {
        "year": year, "item_id": item["id"], "title": item["title"],
        "identifier": meta.get("identi", ""), "local_identifier": meta.get("local", ""),
        "pages": len(pages), "pdf": pdf.name,
        "source_url": f"{BASE}/digital/collection/{COLLECTION}/id/{item['id']}",
        "description": meta.get("descri", ""),
    }
    manifest[year] = [
        {"year": year, "page": n, "page_id": pid, "page_title": ptitle,
         "jp2_bytes": f.stat().st_size, "pdf": pdf.name}
        for n, ((pid, ptitle), f) in enumerate(zip(pages, files), 1)
    ]
    print(f"    wrote {pdf.name} ({pdf.stat().st_size / 1e6:.0f} MB, {len(pages)} pages)")

    if not args.keep_pages:
        shutil.rmtree(pages_dir)
        try:
            pages_dir.parent.rmdir()  # drop pages/ once empty
        except OSError:
            pass
    return True


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--out", type=Path, default=DEFAULT_OUT, help=f"output dir (default {DEFAULT_OUT})")
    ap.add_argument("--years", type=int, nargs="+", help="only these report years")
    ap.add_argument("--list", action="store_true", help="list available reports and exit")
    ap.add_argument("--keep-pages", action="store_true", help="keep the per-page JP2 files")
    ap.add_argument("--force", action="store_true", help="rebuild PDFs that already exist")
    ap.add_argument("--workers", type=int, default=3, help="parallel page downloads (default 3)")
    ap.add_argument("--delay", type=float, default=0.5, help="seconds each worker waits after a page")
    ap.add_argument("--include-other", action="store_true",
                    help=f"also fetch items not titled '{TITLE_PREFIX}...' (e.g. Legislative ledger, 1969)")
    args = ap.parse_args()

    items = list_items()
    todo = []
    for it in items:
        m = re.search(r"\b(\d{4})\b", it["title"])
        is_report = it["title"].startswith(TITLE_PREFIX)
        if not m or (not is_report and not args.include_other):
            print(f"skipping non-report item: {it['title']} (use --include-other)", file=sys.stderr)
            continue
        if args.years and int(m.group(1)) not in args.years:
            continue
        todo.append((it, int(m.group(1))))

    if args.list:
        for it, y in todo:
            n = len(page_list(it["id"]) or [])
            print(f"{y}  id {it['id']:>5}  {n:>3} pages  {it['title']}")
        return

    args.out.mkdir(parents=True, exist_ok=True)
    reports, manifest, failed = {}, {}, []
    for it, year in todo:
        try:
            process(it, year, args.out, args, reports, manifest)
        except Exception as e:  # keep going; report at the end
            print(f"    FAILED {year}: {e}", file=sys.stderr)
            failed.append(year)

    # merge this run's rows into the on-disk tables (replacing re-run years)
    def merge(name, fields, new_by_year, flat):
        old = [r for r in read_csv(args.out / name) if int(r["year"]) not in new_by_year]
        new = flat(new_by_year)
        write_csv(args.out / name, old + new, fields)
    merge("reports.csv", REPORT_FIELDS, reports, lambda d: list(d.values()))
    merge("manifest.csv", PAGE_FIELDS, manifest, lambda d: [r for rows in d.values() for r in rows])

    if failed:
        sys.exit(f"failed years: {failed} (re-run to resume)")
    print("done")


if __name__ == "__main__":
    main()
