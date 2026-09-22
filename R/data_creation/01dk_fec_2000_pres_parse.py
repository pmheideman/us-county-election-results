"""Parse the state presidential results of FEC 'Federal Elections 2000' (OCR text layer, pages 22-36 of federalelections00.pdf) into a nominee table for the QA reconciliation.
Output: R/data/fec_official/pres2000_nominees.csv (state_po, last, party, votes, state_total). Only Bush (R) and Gore (D) [NY: 'Combined Parties' lines] and the printed 'Total State Votes'."""
import re, csv, subprocess
PDF = "R/data/fec_official/federalelections00.pdf"
txt = subprocess.run(["pdftotext", "-layout", "-f", "22", "-l", "36", PDF, "-"], capture_output=True, text=True).stdout
ST = {"ALABAMA":"AL","ALASKA":"AK","ARIZONA":"AZ","ARKANSAS":"AR","CALIFORNIA":"CA","COLORADO":"CO","CONNECTICUT":"CT","DELAWARE":"DE","DISTRICTOFCOLUMBIA":"DC","FLORIDA":"FL","GEORGIA":"GA","HAWAII":"HI","IDAHO":"ID","ILLINOIS":"IL","INDIANA":"IN","IOWA":"IA","KANSAS":"KS","KENTUCKY":"KY","LOUISIANA":"LA","MAINE":"ME","MARYLAND":"MD","MASSACHUSETTS":"MA","MICHIGAN":"MI","MINNESOTA":"MN","MISSISSIPPI":"MS","MISSOURI":"MO","MONTANA":"MT","NEBRASKA":"NE","NEVADA":"NV","NEWHAMPSHIRE":"NH","NEWJERSEY":"NJ","NEWMEXICO":"NM","NEWYORK":"NY","NORTHCAROLINA":"NC","NORTHDAKOTA":"ND","OHIO":"OH","OKLAHOMA":"OK","OREGON":"OR","PENNSYLVANIA":"PA","RHODEISLAND":"RI","SOUTHCAROLINA":"SC","SOUTHDAKOTA":"SD","TENNESSEE":"TN","TEXAS":"TX","UTAH":"UT","VERMONT":"VT","VIRGINIA":"VA","WASHINGTON":"WA","WESTVIRGINIA":"WV","WISCONSIN":"WI","WYOMING":"WY"}
def num(s): 
    d = re.sub(r"[^0-9]", "", s); return int(d) if d else None
rows, cur = {}, None
for ln in txt.splitlines():
    h = re.sub(r"[^A-Z]", "", ln.replace("(Continued)", "").replace("Continued", "")) if re.fullmatch(r"[\sA-Z().]*(\(Continued\))?\s*", ln) and ln.strip() else None
    if h and h in ST: cur = ST[h]; rows.setdefault(cur, {"total": None}); continue
    if not cur: continue
    ## OCR garbles names ("I3ush", "Ciore"), so lines are classified by the PARTY column: the first R line is Bush, the first D-type line (D, DFL, DNL) is Gore; New York prints "Combined Parties" lines per candidate
    m = re.match(r"\s*(\S.*?)\s{2,}(Combined Parties:|R|D|DFL|DNL|D-NPL)\s+([\d ,\.]+?)\s{2,}[\d .o;%]*$", ln)
    if m and "Total" not in ln:
        if m.group(2) == "Combined Parties:" and not re.match(r"\s*(Bus\s*h|I3ush|Bush|Gore|Ciore)", ln): continue   # New York: other candidates' combined lines
        who = "Bush" if (m.group(2) == "R" or re.match(r"\s*(Bus\s*h|I3ush|Bush)", ln)) else "Gore"
        if m.group(2) == "Combined Parties:" or who not in rows[cur] or rows[cur][who][0] != "Combined Parties:":
            if who not in rows[cur] or m.group(2) == "Combined Parties:": rows[cur][who] = (m.group(2), num(m.group(3)))
        continue
    m = re.search(r"Total\s*S\s*tate\s*Votes:\s*([\d ,]+)", ln)
    if m: rows[cur]["total"] = num(m.group(1))
with open("R/data/fec_official/pres2000_nominees.csv", "w", newline="") as f:
    w = csv.writer(f); w.writerow(["state_po", "last", "party", "votes", "state_total"])
    for st, d in sorted(rows.items()):
        for last, party in (("Bush", "R"), ("Gore", "D")): w.writerow([st, last, party, d.get(last, (None, None))[1], d["total"]])
print(len(rows), "states parsed;", sum(1 for d in rows.values() if "Bush" in d and "Gore" in d and d["total"]), "with Bush, Gore and a total")
