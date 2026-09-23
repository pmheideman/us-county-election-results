## Parses the per-county HTML files downloaded by 01ed_new_mexico_sos_archive_download.py into a
## long candidate-level CSV. Each county file has one "General Election Results for <OFFICE>" block
## per office/district that touches that county; this extracts only the
## "UNITED STATES REPRESENTATIVE - DISTRICT NN" blocks. The HTML is FrontPage-era and not
## well-formed (unclosed/mismatched tags), so this uses a simple positional-cell regex rather than
## a real HTML parser: every `<font color="#800000">...</font>` inside a district's block is a data
## cell, and cells come in groups of 4 (name, party, votes, percent) once empty spacer cells are
## dropped. Run as `python3 01ef_new_mexico_sos_archive_parse.py <year> <input_dir> <output_csv>`.
import re, sys, csv, os, glob

def parse_county_file(path, county, year):
    with open(path, encoding="utf-8", errors="replace") as f:
        text = f.read()
    # isolate each "UNITED STATES REPRESENTATIVE - DISTRICT NN" block, up to the next "</TABLE>"
    rows = []
    for m in re.finditer(r'UNITED STATES REPRESENTATIVE\s*-\s*DISTRICT\s*0?(\d+)', text, re.IGNORECASE):
        district = int(m.group(1))
        block_start = m.end()
        block_end = text.find("</TABLE>", block_start)
        if block_end < 0:
            block_end = len(text)
        block = text[block_start:block_end]
        # every <font color="#800000">...</font> is a data cell (name/party/votes/percent in sequence);
        # empty spacer cells are '<font color="#800000"></font>' or whitespace-only -- filter those out
        cells = re.findall(r'<font color="#800000">\s*([^<]*?)\s*</font>', block, re.IGNORECASE | re.DOTALL)
        cells = [c.strip() for c in cells if c.strip()]
        # cells should now come in groups of 4: name, party, votes, percent
        if len(cells) % 4 != 0:
            print(f"  WARN {county} {year} district {district}: {len(cells)} cells, not a multiple of 4: {cells}")
        for i in range(0, len(cells) - 3, 4):
            name, party, votes, pct = cells[i:i+4]
            votes_num = votes.replace(",", "")
            if not re.match(r'^-?\d+$', votes_num):
                continue
            rows.append({"year": year, "county": county, "district": district, "candidate": name,
                         "party": party, "votes": int(votes_num), "pct_printed": pct})
    return rows

def main(year, indir, outcsv):
    all_rows = []
    for path in sorted(glob.glob(os.path.join(indir, "*.html")) + glob.glob(os.path.join(indir, "*.htm"))):
        county = os.path.splitext(os.path.basename(path))[0].replace("_", " ")
        all_rows.extend(parse_county_file(path, county, year))
    print(f"{year}: {len(all_rows)} rows from {len(set(r['county'] for r in all_rows))} counties")
    keys = ["year", "county", "district", "candidate", "party", "votes", "pct_printed"]
    with open(outcsv, "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=keys)
        w.writeheader()
        w.writerows(all_rows)
    # quick per-district county coverage check
    from collections import defaultdict
    by_dist = defaultdict(set)
    for r in all_rows:
        by_dist[r["district"]].add(r["county"])
    for d in sorted(by_dist):
        print(f"  district {d}: {len(by_dist[d])} counties")

if __name__ == "__main__":
    main(int(sys.argv[1]), sys.argv[2], sys.argv[3])
