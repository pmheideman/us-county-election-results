#!/usr/bin/env python3
"""
Downloads the official Clerk of the U.S. House "Statistics of the ... Election" pages
(clerk.house.gov), one per even year -- these give, for every state, every U.S. House district's
candidates with PARTY and district-total votes (not county-level, just district totals). Built to
close the Michigan 1998-2006 gap: OpenElections' MI precinct files for those years have real
candidate names and real county-level vote counts but a genuinely BLANK party column (checked
directly), so this is a name->party lookup, not a vote-count source. Reusable for any other state
with the same "OpenElections has county splits but no party" problem.

URL pattern: https://clerk.house.gov/member_info/electionInfo/<YEAR>/<FILE>Stat.htm, where <FILE>
is the two-digit year for 1998 ("98Stat.htm") and the full 4-digit year from 2000 on
("2000Stat.htm", "2002Stat.htm", ...) -- confirmed by trying both patterns per year.
"""
import os
import sys
import time
import urllib.request

OUT_DIR = "/home/paul/Stats/substack_projects/immigration/R/data/clerk_house_stats"
os.makedirs(OUT_DIR, exist_ok=True)

FILE_TOKEN = {1998: "98", 2000: "2000", 2002: "2002", 2004: "2004", 2006: "2006"}

def fetch(url):
    req = urllib.request.Request(url, headers={"User-Agent": "Mozilla/5.0 (research script)"})
    with urllib.request.urlopen(req, timeout=30) as resp:
        return resp.read()

def main(years):
    for year in years:
        dest = os.path.join(OUT_DIR, f"{year}Stat.htm")
        if os.path.exists(dest) and os.path.getsize(dest) > 0:
            print(f"{year}: cached ({os.path.getsize(dest)} bytes)")
            continue
        url = f"https://clerk.house.gov/member_info/electionInfo/{year}/{FILE_TOKEN[year]}Stat.htm"
        try:
            data = fetch(url)
        except Exception as e:
            print(f"{year}: FAILED ({url}): {e}", file=sys.stderr)
            continue
        with open(dest, "wb") as f:
            f.write(data)
        print(f"{year}: {len(data)} bytes -> {dest}")
        time.sleep(1)

if __name__ == "__main__":
    main([1998, 2000, 2002, 2004, 2006])
