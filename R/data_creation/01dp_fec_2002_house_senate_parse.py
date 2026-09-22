"""Parse the FEC 2002 House and Senate results workbooks (position-encoded row/column layout, not a normal table) into
(state_po, office, district, candidate, party, general_votes, district_or_state_total) rows for the QA reconciliation.
Output: R/data/fec_official/fec2002_house.csv, fec2002_senate.csv"""
import openpyxl, csv, re

ST = {"ALABAMA":"AL","ALASKA":"AK","ARIZONA":"AZ","ARKANSAS":"AR","CALIFORNIA":"CA","COLORADO":"CO","CONNECTICUT":"CT","DELAWARE":"DE","DISTRICT OF COLUMBIA":"DC","FLORIDA":"FL","GEORGIA":"GA","HAWAII":"HI","IDAHO":"ID","ILLINOIS":"IL","INDIANA":"IN","IOWA":"IA","KANSAS":"KS","KENTUCKY":"KY","LOUISIANA":"LA","MAINE":"ME","MARYLAND":"MD","MASSACHUSETTS":"MA","MICHIGAN":"MI","MINNESOTA":"MN","MISSISSIPPI":"MS","MISSOURI":"MO","MONTANA":"MT","NEBRASKA":"NE","NEVADA":"NV","NEW HAMPSHIRE":"NH","NEW JERSEY":"NJ","NEW MEXICO":"NM","NEW YORK":"NY","NORTH CAROLINA":"NC","NORTH DAKOTA":"ND","OHIO":"OH","OKLAHOMA":"OK","OREGON":"OR","PENNSYLVANIA":"PA","RHODE ISLAND":"RI","SOUTH CAROLINA":"SC","SOUTH DAKOTA":"SD","TENNESSEE":"TN","TEXAS":"TX","UTAH":"UT","VERMONT":"VT","VIRGINIA":"VA","WASHINGTON":"WA","WEST VIRGINIA":"WV","WISCONSIN":"WI","WYOMING":"WY"}

def clean_state(v):
    v = re.sub(r"\s{2,}.*$", "", v).strip().upper()
    return ST.get(v)

def num(v):
    if v is None: return None
    try: return float(str(v).replace(",", ""))
    except ValueError: return None

def parse(path, is_house):
    wb = openpyxl.load_workbook(path, data_only=True); ws = wb.active
    rows = list(ws.iter_rows(values_only=True))
    # find the column holding "GENERAL" 's first "# OF VOTES" sub-column: header row 2 has "GENERAL" (or "GENERAL\n# OF VOTES   %"), row 3 sub-labels
    gen_col = None
    for i in range(min(6, len(rows))):
        for j, v in enumerate(rows[i]):
            if v and "GENERAL" in str(v).upper():
                gen_col = j  # the votes number is in this same column per the samples seen (first numeric col under GENERAL)
    name_col = 0
    for j, v in enumerate(rows[0]):
        pass
    out = []
    st = None; district = None; office = "house" if is_house else "senate"
    for r in rows:
        vals = {j: v for j, v in enumerate(r) if v is not None}
        if not vals: continue
        cols = sorted(vals.keys())
        first_col, first_val = cols[0], vals[cols[0]]
        text = str(first_val).strip()
        if text.upper() in ST or clean_state(text):
            cs = clean_state(text)
            if cs: st = cs; district = None; continue
        if is_house and re.match(r"^DISTRICT\s+(\d+)", text.upper()):
            district = re.match(r"^DISTRICT\s+(\d+)", text.upper()).group(1)
            continue
        if text.upper().startswith("DISTRICT VOTES") or (not is_house and "PARTY VOTES" not in text.upper() and text.upper().startswith("TOTAL")):
            tot = vals.get(gen_col)
            out.append({"state_po": st, "district": district if is_house else "S", "candidate": None, "party": None, "votes": None, "is_total": True, "total": num(tot)})
            continue
        if "PARTY VOTES" in text.upper() or text.upper().startswith("PARTY VOTES"):
            continue
        ## a candidate row: name is in whichever of the first couple columns holds a comma'd "Last, First" or a bare name (col 0 may hold "(I)")
        name = None
        for c in cols:
            v = str(vals[c])
            if v == "(I)": continue
            if re.match(r"^[A-Za-z]", v):
                name = v; break
        if name is None: continue
        ## party = the column right after the name column, if short (<=6 chars, letters)
        party = None
        nc = None
        for c in cols:
            if str(vals[c]) == name: nc = c; break
        for c in cols:
            if c > nc and c <= nc + 20:
                v = str(vals[c])
                if re.match(r"^[A-Z]{1,6}$", v): party = v; break
        votes = num(vals.get(gen_col))
        if votes is None:  # some rows put the general vote a column or two off; take the LAST numeric value on the row as a fallback (percentages are <100)
            nums = [vals[c] for c in cols if isinstance(vals[c], (int, float))]
            big = [n for n in nums if n >= 100]
            votes = big[-1] if big and gen_col not in vals else votes
        out.append({"state_po": st, "district": district if is_house else "S", "candidate": name, "party": party, "votes": votes, "is_total": False, "total": None})
    return out

for path, is_house, outname in [("R/data/fec_official/FederalElections2002_House.xlsx", True, "R/data/fec_official/fec2002_house.csv"),
                                  ("R/data/fec_official/FederalElections2002_Senate.xlsx", False, "R/data/fec_official/fec2002_senate.csv")]:
    rows = parse(path, is_house)
    with open(outname, "w", newline="") as f:
        w = csv.writer(f); w.writerow(["state_po", "district", "candidate", "party", "votes", "is_total", "total"])
        for r in rows: w.writerow([r["state_po"], r["district"], r["candidate"], r["party"], r["votes"], r["is_total"], r["total"]])
    print(outname, len(rows), "rows;", sum(1 for r in rows if r["is_total"]), "totals;", len(set(r["state_po"] for r in rows if r["state_po"])), "states")
