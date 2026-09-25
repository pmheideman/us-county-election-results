"""New Mexico U.S. House 1996, county level -- re-transcription of the State Canvassing Board's
'Canvass of Returns of General Election Held on November 5, 1996' (R/data/county_house_files/
NM_1996 General Summary.pdf, page 1 of 18: one wide handwritten sheet, counties as columns in the
fixed order below, then TOTAL FOR EACH CANDIDATE). Read at 400 dpi, every cell anchored to its
county column header.

Why: the 1996 block of 01ek_house_county_new_mexico_scanned_canvass.R had county-attribution
errors (the same class as its 1994 column shift):
  * the District 3 Richardson/Redmond cells under HARDING (376 / 231) were labelled GUADALUPE, so
    Harding looked absent and Guadalupe's House total ran to 134% of its presidential vote;
  * the District 1 cells under SANTA FE (216 / 723) were labelled SAN MIGUEL;
  * District 1's Green (Uhrich) and Unaffiliated (Turrietta-Koury) candidates were kept for
    Bernalillo only, although the sheet prints them in Sandoval, Santa Fe, Torrance and Valencia
    too (01ek used the Bernalillo cells, 7,060 and 4,056, as their 'totals'; the printed totals
    are 7,694 and 4,459);
  * District 3's Libertarian Ed D. Nagel (4,097) was omitted.

Checks (the script stops unless all pass): each candidate's county cells sum to the printed
TOTAL FOR EACH CANDIDATE; every total equals the Clerk of the House 'Statistics of the
Presidential and Congressional Election of November 5, 1996'; each county's House total (summed
over districts) is 0.8-1.05 of its 1996 presidential total in the panel (R/output/elect_cty_final.rds,
sample PE); all 33 counties appear.

Output: R/data/county_house_files/sri/new_mexico_house_county_1996.csv
"""
import csv, os, subprocess

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.join(HERE, "../..")
OUT = os.path.join(ROOT, "R/data/county_house_files/sri/new_mexico_house_county_1996.csv")

# column order on the sheet (sheet spelling -> countypres spelling where it differs)
COLS = ["BERNALILLO", "CATRON", "CHAVES", "CIBOLA", "COLFAX", "CURRY", "DE BACA", "DONA ANA", "EDDY",
        "GRANT", "GUADALUPE", "HARDING", "HIDALGO", "LEA", "LINCOLN", "LOS ALAMOS", "LUNA", "MCKINLEY",
        "MORA", "OTERO", "QUAY", "RIO ARRIBA", "ROOSEVELT", "SANDOVAL", "SAN JUAN", "SAN MIGUEL",
        "SANTA FE", "SIERRA", "SOCORRO", "TAOS", "TORRANCE", "UNION", "VALENCIA"]

# (district, candidate, party code, {county: votes}, printed TOTAL FOR EACH CANDIDATE)
ROWS = [
    (1, "John Wertheim", "D", {"BERNALILLO": 65190, "SANDOVAL": 2420, "SANTA FE": 216, "TORRANCE": 1609, "VALENCIA": 2200}, 71635),
    (1, "Steven H. Schiff", "R", {"BERNALILLO": 99577, "SANDOVAL": 3000, "SANTA FE": 723, "TORRANCE": 2773, "VALENCIA": 3217}, 109290),
    (1, "John A. Uhrich", "I", {"BERNALILLO": 7060, "SANDOVAL": 335, "SANTA FE": 43, "TORRANCE": 129, "VALENCIA": 127}, 7694),
    (1, "Betty Turrietta-Koury", "I", {"BERNALILLO": 4056, "SANDOVAL": 127, "SANTA FE": 22, "TORRANCE": 92, "VALENCIA": 162}, 4459),
    (2, "E. Shirley Baca", "D", {"BERNALILLO": 307, "CATRON": 460, "CHAVES": 6310, "CIBOLA": 3072, "DE BACA": 369, "DONA ANA": 21877,
        "EDDY": 7393, "GRANT": 5903, "GUADALUPE": 1057, "HIDALGO": 853, "LEA": 4901, "LINCOLN": 2243, "LUNA": 3049, "OTERO": 5778,
        "SIERRA": 2290, "SOCORRO": 2999, "VALENCIA": 6054}, 74915),
    (2, "Joe Skeen", "R", {"BERNALILLO": 206, "CATRON": 1037, "CHAVES": 12044, "CIBOLA": 2954, "DE BACA": 715, "DONA ANA": 21154,
        "EDDY": 11419, "GRANT": 4837, "GUADALUPE": 661, "HIDALGO": 1087, "LEA": 9434, "LINCOLN": 4113, "LUNA": 3188, "OTERO": 10415,
        "SIERRA": 2535, "SOCORRO": 3146, "VALENCIA": 6146}, 95091),
    (3, "Bill Richardson", "D", {"BERNALILLO": 2051, "CIBOLA": 472, "COLFAX": 3566, "CURRY": 6598, "HARDING": 376, "LOS ALAMOS": 5559,
        "MCKINLEY": 11654, "MORA": 1911, "QUAY": 2789, "RIO ARRIBA": 8721, "ROOSEVELT": 3328, "SANDOVAL": 12152, "SAN JUAN": 16962,
        "SAN MIGUEL": 8009, "SANTA FE": 31499, "TAOS": 8019, "UNION": 928}, 124594),
    (3, "Bill Redmond", "R", {"BERNALILLO": 1877, "CIBOLA": 179, "COLFAX": 1437, "CURRY": 5574, "HARDING": 231, "LOS ALAMOS": 4004,
        "MCKINLEY": 3398, "MORA": 404, "QUAY": 1318, "RIO ARRIBA": 2232, "ROOSEVELT": 2405, "SANDOVAL": 7495, "SAN JUAN": 14511,
        "SAN MIGUEL": 1267, "SANTA FE": 8091, "TAOS": 1481, "UNION": 676}, 56580),
    (3, "Ed D. Nagel", "I", {"BERNALILLO": 79, "CIBOLA": 15, "COLFAX": 90, "CURRY": 147, "HARDING": 1, "LOS ALAMOS": 289,
        "MCKINLEY": 182, "MORA": 27, "QUAY": 40, "RIO ARRIBA": 178, "ROOSEVELT": 74, "SANDOVAL": 451, "SAN JUAN": 743,
        "SAN MIGUEL": 173, "SANTA FE": 1257, "TAOS": 323, "UNION": 28}, 4097),
]
# Clerk of the House, Statistics of the Presidential and Congressional Election of November 5, 1996 (New Mexico)
CLERK = {"John Wertheim": 71635, "Steven H. Schiff": 109290, "John A. Uhrich": 7694, "Betty Turrietta-Koury": 4459,
         "E. Shirley Baca": 74915, "Joe Skeen": 95091, "Bill Richardson": 124594, "Bill Redmond": 56580, "Ed D. Nagel": 4097}

for d, cand, p, cells, tot in ROWS:
    assert set(cells) <= set(COLS), (cand, set(cells) - set(COLS))
    assert sum(cells.values()) == tot, ("printed total", cand, sum(cells.values()), tot)
    assert tot == CLERK[cand], ("Clerk", cand, tot, CLERK[cand])

# county names and FIPS from the MEDSL crosswalk
fips = {}
with open(os.path.join(ROOT, "R/data/raw_election/countypres_2000-2024.tab")) as f:
    for r in csv.DictReader(f, delimiter="\t"):
        if r["state"] == "NEW MEXICO" and r["county_fips"] not in ("", "NA"):
            fips[r["county_name"].upper()] = int(float(r["county_fips"]))
assert set(COLS) <= set(fips), set(COLS) - set(fips)
house = {c: 0 for c in COLS}
for d, cand, p, cells, tot in ROWS:
    for c, v in cells.items():
        house[c] += v
assert all(v > 0 for v in house.values()), [c for c, v in house.items() if v == 0]   # all 33 counties

# turnout check against the 1996 presidential totals in the panel
pe = subprocess.run(["Rscript", "-e", 'p <- readRDS("R/output/elect_cty_final.rds"); x <- p[p$sample == "PE" & p$year == 1996 & p$cty_fips %/% 1000 == 35, ]; '
                     'write.csv(x[, c("cty_fips", "totalvote")], stdout(), row.names = FALSE)'], capture_output=True, text=True, cwd=ROOT).stdout
pe = {int(float(r["cty_fips"])): float(r["totalvote"]) for r in csv.DictReader(pe.splitlines())}
bad = []
for c in COLS:
    r = house[c] / pe[fips[c]]
    if not 0.8 <= r <= 1.05:
        bad.append((c, house[c], pe[fips[c]], round(r, 3)))
assert not bad, bad

with open(OUT, "w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["year", "district", "county", "candidate", "party_code", "votes"])
    for d, cand, p, cells, tot in ROWS:
        for c in COLS:
            if c in cells:
                w.writerow([1996, d, c, cand, p, cells[c]])
print(f"New Mexico 1996: 9 candidates, 33 counties; printed totals, Clerk and turnout checks pass. "
      f"House/president ratio range {min(house[c] / pe[fips[c]] for c in COLS):.3f}-{max(house[c] / pe[fips[c]] for c in COLS):.3f}")
