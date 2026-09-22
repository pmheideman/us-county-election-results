"""Download the Georgia Secretary of State's archived general-election results (Wayback Machine copies from July 2008 of sos.georgia.gov/elections/election_results/), 1990-1998.
Index: https://web.archive.org/web/20080702191734/http://sos.georgia.gov/elections/election_results/default.htm -> 'Results Frames/<year>.html' -> the November general election frame
(<year>_<mmdd>/frame_<mmdd>.html or <year>/Frame_<mmdd>.html) -> federal.htm (statewide offices in ballot order) and one county page per office (0000100.htm, 0000200.htm, ... = 100 x the office's
position in federal.htm) listing every county of the district (a county in 2+ districts has its part in each). Only the U.S. Representative pages are fetched (13 districts at most),
2-14 seconds apart (with retries); cached in R/data/raw_house_county_open_states/georgia_sos_archive/ (re-runs skip existing files)."""
import re, os, subprocess, time, html
D = "R/data/raw_house_county_open_states/georgia_sos_archive"; os.makedirs(D, exist_ok=True)
UA = "Mozilla/5.0 (research; county election data project)"
def get(url, out):
    if os.path.exists(out) and os.path.getsize(out) > 200: return open(out, encoding="utf-8", errors="ignore").read()
    for attempt in range(4):                                  # the Wayback Machine sometimes answers with an empty or error page: retry with growing pauses
        time.sleep(2 + 4 * attempt); subprocess.run(["curl", "-sL", "--compressed", "-m", "90", "-A", UA, "-o", out, url])
        if os.path.exists(out) and os.path.getsize(out) > 800 and b"<title>Wayback Machine" not in open(out, "rb").read(3000): break
    return open(out, encoding="utf-8", errors="ignore").read() if os.path.exists(out) else ""
def text(h):
    h = re.sub(r"<script.*?</script>|<style.*?</style>", "", h, flags=re.S | re.I); t = re.sub(r"<[^>]+>", "\n", h); t = html.unescape(t)
    return re.sub(r"\n\s*\n+", "\n", re.sub(r"[ \t\xa0]+", " ", t))
BASE = "https://web.archive.org"
FRAMES = {1990: ("20080702194526", "1990/Frame_1106.html"), 1992: ("20080702194552", "1992/Frame_1103.html"), 1994: ("20080702194800", "1994/Frame_1108.html"),
          1996: ("20080702194644", "1996_1105/frame_1105.html"), 1998: ("20080702194502", "1998_1103/frame_1103.html")}
ROOT = "http://sos.georgia.gov/elections/election_results/"
def ordinal(n): return "%d%s" % (n, "th" if 10 <= n % 100 <= 20 else {1: "st", 2: "nd", 3: "rd"}.get(n % 10, "th"))
for y, (ts, frame) in FRAMES.items():
    folder = os.path.dirname(frame); prefix = "%s/web/%s/%s%s/" % (BASE, ts, ROOT, folder)
    get("%s/web/%s/%s%s" % (BASE, ts, ROOT, frame), os.path.join(D, "%d_frame.html" % y))
    found = []
    if y <= 1994:                                             # named pages: 1stcong.htm ... 11thcong.htm
        for n in range(1, 14):
            h = get(prefix + "%scong.htm" % ordinal(n), os.path.join(D, "%d_district%02d.html" % (y, n)))
            if h and "CONGRESS" in h.upper() and re.search(r"(?i)precincts|county", h): found.append(n)
            elif os.path.exists(os.path.join(D, "%d_district%02d.html" % (y, n))): os.remove(os.path.join(D, "%d_district%02d.html" % (y, n)))
    else:                                                     # numbered pages 0000100.htm ...: keep those titled 'United States Representative'
        for k in range(1, 26):
            f = os.path.join(D, "%d_page%02d.html" % (y, k)); h = get(prefix + "%07d.htm" % (k * 100), f)
            t = text(h)[:300] if h else ""
            m = re.search(r"UNITED STATES REPRESENTATIVE - (\d+)(?:ST|ND|RD|TH) DISTRICT", t.upper())
            if m: found.append((k, int(m.group(1))))
            else: os.remove(f) if os.path.exists(f) else None
    print(y, "U.S. House pages found:", found)
