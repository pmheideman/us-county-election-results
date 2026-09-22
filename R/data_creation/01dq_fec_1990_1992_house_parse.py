"""Parse the U.S. House general-election district results out of the FEC's scanned 1990 and 1992 books
(federalelections90.pdf, federalelections92.pdf; OCR text layer). Two different, but both clean, layouts:
  1990: "N -   LastName   First MI   Party   votes   pct" per candidate, districts numbered in sequence within a state,
        closed by a "Total Votes:" line (no explicit district total is CALLED OUT beyond that summary line).
  1992: a "DISTRICT N" header line, then "LastName, First   Party   votes   pct" candidate rows, closed by "Total Votes:".
Only the GENERAL election section of each book is used (the House section starts after the Senate section and after any
primary/run-off tables). Output: R/data/fec_official/fec1990_house.csv, fec1992_house.csv
(state_po, district, candidate, party, votes, is_total, total)
"""
import re, csv, subprocess

ST = {"ALABAMA":"AL","ALASKA":"AK","ARIZONA":"AZ","ARKANSAS":"AR","CALIFORNIA":"CA","COLORADO":"CO","CONNECTICUT":"CT","DELAWARE":"DE",
      "AMERICAN SAMOA":None, "GUAM":None, "VIRGIN ISLANDS":None, "PUERTO RICO":None, "NORTHERN MARIANA":None,
      "DISTRICT OF COLUMBIA":"DC","FLORIDA":"FL","GEORGIA":"GA","HAWAII":"HI","IDAHO":"ID","ILLINOIS":"IL","INDIANA":"IN","IOWA":"IA",
      "KANSAS":"KS","KENTUCKY":"KY","LOUISIANA":"LA","MAINE":"ME","MARYLAND":"MD","MASSACHUSETTS":"MA","MICHIGAN":"MI","MINNESOTA":"MN",
      "MISSISSIPPI":"MS","MISSOURI":"MO","MONTANA":"MT","NEBRASKA":"NE","NEVADA":"NV","NEW HAMPSHIRE":"NH","NEW JERSEY":"NJ",
      "NEW MEXICO":"NM","NEW YORK":"NY","NORTH CAROLINA":"NC","NORTH DAKOTA":"ND","OHIO":"OH","OKLAHOMA":"OK","OREGON":"OR",
      "PENNSYLVANIA":"PA","RHODE ISLAND":"RI","SOUTH CAROLINA":"SC","SOUTH DAKOTA":"SD","TENNESSEE":"TN","TEXAS":"TX","UTAH":"UT",
      "VERMONT":"VT","VIRGINIA":"VA","WASHINGTON":"WA","WEST VIRGINIA":"WV","WISCONSIN":"WI","WYOMING":"WY"}
STATE_HDR = re.compile(r"^\s*([A-Z][A-Z .]+?)\s*(\(cont[a-z']*\))?\s*$")


def num(s):
    d = re.sub(r"[^0-9]", "", s)
    return int(d) if d else None


def parse_1990(text):
    out = []
    st = None
    dist_seq = 0
    for ln in text.splitlines():
        raw = ln
        s = ln.strip()
        if not s:
            continue
        m = STATE_HDR.match(ln)
        if m and m.group(1).strip() in ST and not re.search(r"\d", ln):
            code = ST[m.group(1).strip()]
            if code:
                st = code
                dist_seq = 0
            else:
                st = None
            continue
        if st is None:
            continue
        # "N -   Name  First MI   Party   votes   pct"  OR (continuation) "      Name  First MI   Party  votes  pct"
        m = re.match(r"^\s*(\d{1,2})\s*-\s+(.*)$", ln)
        if m:
            dist_seq = int(m.group(1))
            rest = m.group(2)
        else:
            rest = ln
        mt = re.search(r"Total\s+[Vv]otes\s*:\s*([0-9][0-9,]*)", rest)
        if mt:
            out.append({"state_po": st, "district": dist_seq, "candidate": None, "party": None, "votes": None, "is_total": True, "total": num(mt.group(1))})
            continue
        mc = re.match(r"^\s*(?:\d{1,2}\s*-\s*)?([A-Z][A-Za-z'.\-]+)\s+([A-Za-z][A-Za-z .,\"'\-]*?)\s{2,}([A-Za-z][A-Za-z .]*?)\s+([0-9][0-9,]*)\s+[0-9]", rest)
        if mc and dist_seq:
            last, first, party, votes = mc.groups()
            out.append({"state_po": st, "district": dist_seq, "candidate": f"{first.strip()} {last.strip()}", "party": party.strip(), "votes": num(votes), "is_total": False, "total": None})
    return out


def parse_1992(text):
    out = []
    st = None
    district = None
    for ln in text.splitlines():
        s = ln.strip()
        if not s:
            continue
        m = STATE_HDR.match(ln)
        if m and m.group(1).strip() in ST and not re.search(r"\d", ln):
            code = ST[m.group(1).strip()]
            st = code
            district = None
            continue
        if st is None:
            continue
        md = re.match(r"^\s*DISTRICT\s+(\d{1,2})", s.upper())
        if md:
            district = int(md.group(1))
            continue
        mt = re.search(r"Total\s+[Vv]otes\s*:\s*([0-9][0-9,]*)", ln)
        if mt:
            out.append({"state_po": st, "district": district, "candidate": None, "party": None, "votes": None, "is_total": True, "total": num(mt.group(1))})
            continue
        mc = re.match(r"^\s*([A-Za-z][A-Za-z'.\-]+),\s*([A-Za-z][A-Za-z .\"'\-]*?)\s{2,}([A-Za-z][A-Za-z .]*?)\s+([0-9][0-9,]*)\s+[0-9]", ln)
        if mc and district:
            last, first, party, votes = mc.groups()
            out.append({"state_po": st, "district": district, "candidate": f"{first.strip()} {last.strip()}", "party": party.strip(), "votes": num(votes), "is_total": False, "total": None})
    return out


def house_section(path, anchor_re):
    txt = subprocess.run(["pdftotext", "-layout", path, "-"], capture_output=True, text=True).stdout
    ## anchor on "ALABAMA" immediately followed by that year's district-1 marker: this exact sequence is UNIQUE in the book (President and Senate results also
    ## print "ALABAMA" as a running header, and the column header "CANDIDATE NAME / PARTY LABEL / # OF VOTES / PERCENT" repeats on every page of every section,
    ## but only the House section's Alabama block is immediately followed by a district-1 line)
    m = re.search(anchor_re, txt)
    i = m.start() if m else -1
    j = txt.upper().rfind("GUIDE TO PARTY LABELS")   # LAST occurrence: the TOC lists it first
    return txt[i:j if j > i else len(txt)]


def write_csv(rows, path):
    with open(path, "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["state_po", "district", "candidate", "party", "votes", "is_total", "total"])
        for r in rows:
            w.writerow([r["state_po"], r["district"], r["candidate"], r["party"], r["votes"], r["is_total"], r["total"]])


t90 = house_section("R/data/fec_official/federalelections90.pdf", r"ALABAMA\s*\n\s*\n?1\s*-\s+[A-Z]")
r90 = parse_1990(t90)
write_csv(r90, "R/data/fec_official/fec1990_house.csv")
print("1990:", len(r90), "rows,", sum(1 for r in r90 if r["is_total"]), "district totals,", len(set(r["state_po"] for r in r90)), "states")

t92 = house_section("R/data/fec_official/federalelections92.pdf", r"ALABAMA\s*\n\s*DISTRICT\s*1\b")
r92 = parse_1992(t92)
write_csv(r92, "R/data/fec_official/fec1992_house.csv")
print("1992:", len(r92), "rows,", sum(1 for r in r92 if r["is_total"]), "district totals,", len(set(r["state_po"] for r in r92)), "states")
