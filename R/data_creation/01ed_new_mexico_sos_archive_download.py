## Downloads New Mexico's 1998 and 2002 "General Election Results by County" per-county HTML pages
## from the SOS's own archive (sos.nm.gov/sos-archive/election-results-archive/), pointed at by the
## user. These pages are Front-Page-era "General Election Results for <OFFICE> / <COUNTY> County"
## tables (candidate, party, votes, percent) -- one physical HTML file per county, all 33 counties,
## generated ~2002/1998 and never touched since.
##
## The archive page itself only embeds a "RealFile" JS widget (a 3rd-party file-browser widget,
## <div class="rf-table" data-account-guid=... data-folder-id=... data-widget-id=...>) -- the actual
## file list/download isn't in the page HTML, it's fetched client-side. Found the real API by pulling
## the widget's own JS (https://prod.realfile.rtsclients.com/js/rf-tables.js and
## https://cdn.rtsclients.com/SDKs/RealFile/JavaScript/rf_sdk.min.js) and reading how it calls itself:
##   GET https://klvg4oyd4j.execute-api.us-west-2.amazonaws.com/prod/GetWidgetFiles
##       ?widgetId=<>&folderId=<>&rootFolderId=<>&accountGUID=<>
##   -> JSON {data: {files: [{name, fileId, metadata: {"5": "<County> (PDF)"}}, ...]}}
##   GET https://api.realfile.rtsclients.com/PublicFiles/<accountGUID>/<fileId>/<name>  -> the file itself
## No auth needed (these are public folders); no bot-blocking. Both years share the same accountGUID.
import json, re, subprocess, os

ACCOUNT = "ee3072ab0d43456cb15a51f7d82c77a2"
API = "https://klvg4oyd4j.execute-api.us-west-2.amazonaws.com/prod/GetWidgetFiles"
DL = "https://api.realfile.rtsclients.com/PublicFiles/{account}/{fileId}/{name}"
FOLDERS = {
    1998: {"folderId": "4d31fd58-b685-4c42-a265-d024f8b2fdfd", "widgetId": "6790ef07-dc2f-4885-ba23-38926ddc828a"},
    2002: {"folderId": "a1544e0f-6c72-4ba1-87b5-7c5cbfc72c00", "widgetId": "fd0fb574-b14b-49b5-9467-cd79f2fca286"},
}
RAW_DIR = os.path.join("R", "data", "raw_house_county_open_states", "new_mexico_sos_archive")
os.makedirs(RAW_DIR, exist_ok=True)

def curl_get(url):
    out = subprocess.run(["curl", "-sL", "-m", "30", "-A", "Mozilla/5.0 (research; county election data project)", url],
                          capture_output=True, text=True)
    return out.stdout

def curl_download(url, dest):
    if os.path.exists(dest) and os.path.getsize(dest) > 0:
        return
    subprocess.run(["curl", "-sL", "-m", "30", "-A", "Mozilla/5.0 (research; county election data project)", "-o", dest, url])

for year, f in FOLDERS.items():
    listing_path = os.path.join(RAW_DIR, f"{year}_listing.json")
    if not os.path.exists(listing_path):
        url = f"{API}?widgetId={f['widgetId']}&folderId={f['folderId']}&rootFolderId={f['folderId']}&accountGUID={ACCOUNT}"
        body = curl_get(url)
        with open(listing_path, "w") as fh:
            fh.write(body)
    data = json.loads(open(listing_path).read())
    files = data["data"]["files"]
    # keep only real per-county files (2002's folder also has one whole-state summary PDF, skip it)
    county_files = [x for x in files if x["name"].lower().endswith((".htm", ".html")) or "(PDF)" in (x["metadata"].get("5") or "")]
    county_files = [x for x in county_files if x["name"].lower() not in ("2002_general_summary.pdf",)]
    print(f"{year}: {len(county_files)} county files")
    ydir = os.path.join(RAW_DIR, str(year))
    os.makedirs(ydir, exist_ok=True)
    manifest = []
    for x in county_files:
        ## some entries omit the space before "(PDF)" (e.g. 2002's "Guadalupe(PDF)") -- strip both
        county_display = re.sub(r"\s*\(PDF\)\s*$", "", x["metadata"].get("5") or x["name"]).strip()
        dest = os.path.join(ydir, f"{re.sub(r'[^A-Za-z]', '_', county_display)}.html")
        url = DL.format(account=ACCOUNT, fileId=x["fileId"], name=x["name"])
        curl_download(url, dest)
        manifest.append({"county": county_display, "file": dest, "url": url})
    with open(os.path.join(RAW_DIR, f"{year}_manifest.json"), "w") as fh:
        json.dump(manifest, fh, indent=2)
    sizes = [os.path.getsize(m["file"]) for m in manifest if os.path.exists(m["file"])]
    print(f"  downloaded: {sum(1 for s in sizes if s > 0)} of {len(manifest)}, sizes range {min(sizes) if sizes else 0}-{max(sizes) if sizes else 0}")
