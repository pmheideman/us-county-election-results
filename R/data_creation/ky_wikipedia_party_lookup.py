#!/usr/bin/env python3
"""
Builds a (year, district, candidate_last_name) -> party lookup for Kentucky U.S. House races,
2000-2008, from Wikipedia's raw wikitext ("<year> United States House of Representatives
elections in Kentucky"). Feeds 01g_house_county_kentucky_2000s.R, which has no other way to get
party affiliation for elect.ky.gov's 2000s county-level files (those show candidate NAMES but no
party letter at all in that block).

Why the Infobox, not the "Election box" results template: both appear in these articles, but the
Election box template's exact wording/field order varies by year (found empirically: 2000 has
`candidate = X` then `party = Y`; 2002 has `party = X` then `candidate = Y`; 2008 has no Election
box templates for some districts at all). The `{{Infobox election ...}}` block at the top of each
district section, by contrast, uses a stable `nomineeN`/`partyN` field pattern in every year
checked (2000, 2002, 2004, 2006, 2008) -- simpler and more reliable to parse.

Party is normalized to REP/DEM/OTHER (party string starts with "Republican" -> REP, "Democratic"
-> DEM, anything else -> OTHER) since that's all the downstream vote-share calculation needs.
"""
import re
import sys
import csv

YEARS = [2000, 2002, 2004, 2006, 2008, 2012, 2014, 2016]
WIKI_DIR = "/tmp"  # already-fetched raw wikitext, see conversation
OUT_CSV = "/home/paul/Stats/substack_projects/immigration/R/data/raw_house_county_open_states/kentucky_2000s/ky_wiki_party_lookup.csv"

def normalize_party(raw):
    raw = raw.strip()
    if raw.startswith("Republican"):
        return "REP"
    if raw.startswith("Democratic"):
        return "DEM"
    return "OTHER"

def clean_name(raw):
    # Strip wikilinks [[...]] -> keep display text, bold '''...''', parenthetical suffixes like
    # (incumbent)/(inc.), trailing whitespace.
    raw = raw.strip()
    raw = re.sub(r"\[\[([^\]|]+)\|([^\]]+)\]\]", r"\2", raw)  # [[target|display]] -> display
    raw = re.sub(r"\[\[([^\]]+)\]\]", r"\1", raw)             # [[target]] -> target
    raw = raw.replace("'''", "")
    raw = re.sub(r"\(incumbent[^)]*\)", "", raw, flags=re.IGNORECASE)
    raw = re.sub(r"\(inc\.?\)", "", raw, flags=re.IGNORECASE)
    return raw.strip()

def last_name(full_name):
    toks = [t for t in re.split(r"\s+", full_name.strip()) if t]
    if not toks:
        return ""
    return re.sub(r"[^A-Za-z]", "", toks[-1]).upper()

def extract_infobox_block(text, start_idx):
    """Given the index of '{{Infobox election', return the block up to its matching '}}'."""
    depth = 0
    i = start_idx
    n = len(text)
    block_start = start_idx
    while i < n:
        if text[i:i+2] == "{{":
            depth += 1
            i += 2
            continue
        if text[i:i+2] == "}}":
            depth -= 1
            i += 2
            if depth == 0:
                return text[block_start:i]
            continue
        i += 1
    return text[block_start:]

def parse_year(year):
    path = f"{WIKI_DIR}/wiki_ky_{year}.txt"
    with open(path, encoding="utf-8") as f:
        text = f.read()

    # District section boundaries: "==District N==" or "== District N ==" (spacing varies).
    dist_pat = re.compile(r"^==\s*District\s*(\d+)\s*==", re.MULTILINE)
    matches = list(dist_pat.finditer(text))
    rows = []
    for idx, m in enumerate(matches):
        district = int(m.group(1))
        sec_start = m.end()
        sec_end = matches[idx + 1].start() if idx + 1 < len(matches) else len(text)
        section = text[sec_start:sec_end]

        ib_idx = section.find("{{Infobox election")
        if ib_idx == -1:
            print(f"  WARNING: no Infobox found for {year} district {district}", file=sys.stderr)
            continue
        block = extract_infobox_block(section, ib_idx)

        for n in range(1, 6):
            nominee_m = re.search(rf"\|\s*nominee{n}\s*=\s*(.+)", block)
            party_m = re.search(rf"\|\s*party{n}\s*=\s*(.+)", block)
            if not nominee_m or not party_m:
                break
            name = clean_name(nominee_m.group(1))
            party = normalize_party(party_m.group(1))
            ln = last_name(name)
            if ln:
                rows.append((year, district, ln, name, party))
    return rows

def main():
    import os
    os.makedirs(os.path.dirname(OUT_CSV), exist_ok=True)
    all_rows = []
    for year in YEARS:
        rows = parse_year(year)
        print(f"{year}: {len(rows)} candidate rows across districts")
        all_rows.extend(rows)

    with open(OUT_CSV, "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f)
        w.writerow(["year", "district", "last_name", "full_name", "party"])
        w.writerows(all_rows)
    print(f"\nWrote {len(all_rows)} rows to {OUT_CSV}")

if __name__ == "__main__":
    main()
