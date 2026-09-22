"""Download the Illinois State Board of Elections 'Candidate Totals by County' general-election files (GE<year>Ctytxt.txt, later GE<year>Cty.txt / <year>GECty.txt), 1998-2024.
Page: https://www.elections.il.gov/electionoperations/DownloadVoteTotals.aspx (ASP.NET WebForms). For each year the page's own dropdown is submitted as a postback
(ctl00$ContentPlaceHolder1$ddlCurrent = year) and returns links (elections.il.gov/NewDocDisplay.aspx?<token>) to GE<year>Ctytxt.txt / .xls etc. One postback and one download
per year, 1 second apart; cached in R/data/raw_house_county_open_states/illinois_sbe/ (re-runs skip existing files)."""
import re, html, os, subprocess, time, urllib.parse
D = "R/data/raw_house_county_open_states/illinois_sbe"; os.makedirs(D, exist_ok=True)
URL = "https://www.elections.il.gov/electionoperations/DownloadVoteTotals.aspx"
UA = "Mozilla/5.0 (research; county election data project)"
def curl(args): return subprocess.run(["curl", "-sL", "--compressed", "-m", "90", "-A", UA] + args, capture_output=True)
page = curl([URL]).stdout.decode("utf-8", "ignore")
def hid(n):
    m = re.search(r'name="%s" id="%s" value="([^"]*)"' % (n, n), page); return html.unescape(m.group(1)) if m else ""
TARGETS = [("Cty", r"(GE%dCty(txt)?|%dGECty)\.txt"), ("Tot", r"(GE%dTot(txt)?|%dGETot)\.txt")]      # county file and statewide candidate-totals file (checksum)
for y in range(1998, 2026, 2):
    need = [(k, pat) for k, pat in TARGETS if not (os.path.exists(os.path.join(D, "GE%d%s.txt" % (y, k))) and os.path.getsize(os.path.join(D, "GE%d%s.txt" % (y, k))) > 500)]
    if not need: continue
    data = {"__EVENTTARGET": "ctl00$ContentPlaceHolder1$ddlCurrent", "__EVENTARGUMENT": "", "__LASTFOCUS": "", "__VIEWSTATE": hid("__VIEWSTATE"), "__VIEWSTATEGENERATOR": hid("__VIEWSTATEGENERATOR"),
            "__EVENTVALIDATION": hid("__EVENTVALIDATION"), "ctl00$ContentPlaceHolder1$ddlCurrent": str(y), "ctl00$ContentPlaceHolder1$ddlArchived": ""}
    open("/tmp/il_post.txt", "w").write(urllib.parse.urlencode(data)); time.sleep(1)
    r = curl(["-X", "POST", "--data-binary", "@/tmp/il_post.txt", "-H", "Content-Type: application/x-www-form-urlencoded", URL]).stdout.decode("utf-8", "ignore")
    links = re.findall(r'<a href="([^"]+)"[^>]*>([^<]+)</a>', r)
    for k, pat in need:
        tgt = [(u, t) for u, t in links if re.fullmatch(pat % (y, y), t.strip(), re.I)]
        out = os.path.join(D, "GE%d%s.txt" % (y, k))
        print(y, k, "found" if tgt else "NOT FOUND")
        if tgt: time.sleep(1); curl(["-o", out, html.unescape(tgt[0][0])])
