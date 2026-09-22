#!/usr/bin/env python3
"""
Parses the Clerk of the House "Statistics of the ... Election" pages (downloaded by
01ds_clerk_house_party_lookup_download.py) into a (year, state, district, candidate, party, votes)
lookup table. Two table markups seen across 1998-2006 (both handled by one generic <tr>/<td>
parser, not a per-year branch): older years (<table border=1>, "<td>N.</td>" with a trailing
period) and 2006+ (<table cellspacing="0">, "<td class=\"first\">N</td>" no period, extra empty
trailing <td>). District number only appears on a candidate's FIRST row within a district; blank
on every row after that -- carried forward.

Name/party split: rsplit on the LAST comma only ("Albert Chia, Jr., Libertarian" -> name "Albert
Chia, Jr.", party "Libertarian" -- a plain split on the first comma would wrongly cut the name).
A row with no comma at all ("Write-in") has no usable candidate name and is dropped.

Currently built for Michigan only (state anchor #22 confirmed for all 5 years); STATE_ANCHORS can
be extended to reuse this same parser for any other state with the same OpenElections-has-county-
splits-but-no-party problem.
"""
import csv
import html
import os
import re
import sys

IN_DIR = "/home/paul/Stats/substack_projects/immigration/R/data/clerk_house_stats"
OUT_CSV = "/home/paul/Stats/substack_projects/immigration/R/data/clerk_house_stats/clerk_house_party_lookup.csv"

STATE_ANCHORS = {"MICHIGAN": "22"}
YEARS = [1998, 2000, 2002, 2004, 2006]


def normalize_party(raw):
    raw = raw.strip()
    if raw.startswith("Democrat"):
        return "DEM"
    if raw.startswith("Republican"):
        return "REP"
    return "OTHER"


def clean_text(raw):
    raw = re.sub(r"<[^>]+>", "", raw)
    raw = html.unescape(raw)
    return raw.strip()


def extract_section(text, anchor):
    # The section starts at this state's <a name="N"> and runs until the NEXT <a name="M">
    # (any state) or end of file.
    pat = re.compile(r'<a\s+name\s*=\s*"?' + re.escape(anchor) + r'"?\s*>', re.IGNORECASE)
    m = pat.search(text)
    if not m:
        return None
    start = m.end()
    nxt = re.search(r'<a\s+name\s*=\s*"?\d+"?\s*>', text[start:], re.IGNORECASE)
    end = start + nxt.start() if nxt else len(text)
    return text[start:end]


def extract_house_block(section):
    m = re.search(r"For United States Representative", section, re.IGNORECASE)
    if not m:
        return None
    return section[m.end():]


def parse_rows(block):
    """Yields (district_or_None, name_party_cell, votes_cell) for every <tr> in block."""
    for tr_m in re.finditer(r"<tr\b.*?</tr>", block, re.IGNORECASE | re.DOTALL):
        tds = re.findall(r"<td\b[^>]*>(.*?)</td>", tr_m.group(0), re.IGNORECASE | re.DOTALL)
        cells = [clean_text(td) for td in tds]
        cells = [c for c in cells if True]  # keep empties (positional)
        if len(cells) < 3:
            continue
        yield cells[0], cells[1], cells[2]


def parse_year_state(year, state):
    path = os.path.join(IN_DIR, f"{year}Stat.htm")
    with open(path, encoding="utf-8", errors="replace") as f:
        text = f.read()
    section = extract_section(text, STATE_ANCHORS[state])
    if section is None:
        print(f"  WARNING: no anchor found for {state} {year}", file=sys.stderr)
        return []
    block = extract_house_block(section)
    if block is None:
        print(f"  WARNING: no 'For United States Representative' block for {state} {year}", file=sys.stderr)
        return []

    rows = []
    current_district = None
    for dist_cell, name_party_cell, votes_cell in parse_rows(block):
        dist_digits = re.sub(r"[^\d]", "", dist_cell)
        if dist_digits:
            current_district = int(dist_digits)
        if current_district is None:
            continue
        if "," not in name_party_cell:
            continue  # e.g. bare "Write-in" with no attributable candidate name
        name, party_raw = name_party_cell.rsplit(",", 1)
        name = name.strip()
        party = normalize_party(party_raw)
        votes_digits = re.sub(r"[^\d]", "", votes_cell)
        votes = int(votes_digits) if votes_digits else None
        if not name or not re.search(r"[A-Za-z]", name):
            # After the last district's real candidates, the House section runs straight into a
            # statewide recapitulation/summary table (no <b> or <a name> boundary of its own) whose
            # rows carry plain numbers in this same cell position, e.g. "54", "1,438" -- a real
            # candidate name always has a letter in it, so this reliably filters those out without
            # needing to find that table's actual start (same "last section has no natural end"
            # bug class as the Kentucky/Ohio parsers; confirmed here in MI 1998 district 16 and MI
            # 2002 district 15, i.e. every year's LAST district, right after its real candidates).
            continue
        rows.append((year, state, current_district, name, party, votes))
    return rows


def main():
    all_rows = []
    for year in YEARS:
        for state in STATE_ANCHORS:
            rows = parse_year_state(year, state)
            print(f"{year} {state}: {len(rows)} candidate rows")
            all_rows.extend(rows)

    with open(OUT_CSV, "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f)
        w.writerow(["year", "state", "district", "candidate", "party", "votes"])
        w.writerows(all_rows)
    print(f"\nWrote {len(all_rows)} rows to {OUT_CSV}")


if __name__ == "__main__":
    main()
