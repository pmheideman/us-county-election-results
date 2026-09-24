"""Montana U.S. House 1990 by county, transcribed from the Secretary of State's "Report of the Official Canvass of the Vote Cast at the General Election Held in the State of Montana, November 6, 1990"
(scanned, rotated county sheet, 2 pages; Wayback Machine copy of sos.mt.gov/Elections/archives/1990s/1990/1990_General_Statewide.pdf, saved as R/data/county_house_files/montana/MT_1990_general.pdf).
Montana had two districts in 1990: Western (Pat Williams D, Brad Johnson R; counties marked * on the sheet) and Eastern (Don Burris D, Ron Marlenee R). The sheet prints each county's pair only in its own
district's columns (dots in the other), and the printed numbers sit one line above the county name, so the pairs are read in order and assigned to the Western (19 counties marked *) and Eastern (37) county
lists in alphabetical order. Checks (stop on failure): the four columns add up to the printed TOTALS row (100,409 / 63,837 / 56,739 / 96,449) and every county's two-candidate sum is at most its Total Vote Cast
and at least 80% of it (the Total Vote Cast of the first 32 counties is typed below from the sheet).
Output: R/data/county_house_files/montana/montana_house_county_1990.csv (year, district, county, candidate, party_code, votes)."""
import csv
W = "Beaverhead Broadwater DeerLodge Flathead Gallatin Glacier Granite Jefferson Lake LewisClark Lincoln Madison Mineral Missoula Park Powell Ravalli Sanders SilverBow".split()
Wv = [(1414,1902),(779,813),(3592,698),(11891,10181),(10263,8900),(2062,1013),(586,647),(2007,1383),(4300,3504),(13520,6631),(3581,3034),(1237,1392),(1023,392),(19146,9435),(3294,2757),(1491,1053),(5476,5503),(2413,1477),(12334,3122)]
E = "BigHorn Blaine Carbon Carter Cascade Chouteau Custer Daniels Dawson Fallon Fergus Garfield GoldenValley Hill JudithBasin Liberty McCone Meagher Musselshell Petroleum Phillips Pondera PowderRiver Prairie Richland Roosevelt Rosebud Sheridan Stillwater SweetGrass Teton Toole Treasure Valley Wheatland Wibaux Yellowstone".split()
Ev = [(1819,1826),(893,1638),(1487,2436),(158,682),(10988,17003),(723,2259),(1587,3065),(372,906),(1616,2704),(380,1155),(1648,4012),(160,670),(143,390),(2950,3590),(369,956),(299,846),(410,945),(255,618),(743,1212),(75,187),(601,1811),(903,1923),(266,697),(245,482),(1310,2938),(1467,2001),(1355,1807),(793,1717),(1083,1928),(397,1182),(858,2066),(774,1776),(140,361),(1385,2361),(317,736),(167,508),(17603,25055)]
CAST = {"Beaverhead":3401,"BigHorn":3839,"Blaine":2583,"Broadwater":1650,"Carbon":4042,"Carter":876,"Cascade":28656,"Chouteau":3050,"Custer":4820,"Daniels":1316,"Dawson":4451,"DeerLodge":4389,"Fallon":1622,"Fergus":6068,"Flathead":22625,"Gallatin":19816,"Garfield":858,"Glacier":3175,"GoldenValley":546,"Granite":1277,"Hill":6754,"Jefferson":3466,"JudithBasin":1370,"Lake":7953,"LewisClark":20667,"Liberty":1187,"Lincoln":6869,"Madison":2698,"McCone":1380,"Meagher":899,"Mineral":1467,"Missoula":29026}
assert len(W) == len(Wv) == 19 and len(E) == len(Ev) == 37
assert [sum(v[k] for v in Wv) for k in (0, 1)] == [100409, 63837] and [sum(v[k] for v in Ev) for k in (0, 1)] == [56739, 96449]
for c, v in list(zip(W, Wv)) + list(zip(E, Ev)):
    if c in CAST: assert 0.80 * CAST[c] <= sum(v) <= CAST[c], (c, v, CAST[c])
def cn(c): return "LEWIS & CLARK" if c == "LewisClark" else c.upper()
with open("R/data/county_house_files/montana/montana_house_county_1990.csv", "w", newline="") as fh:
    w = csv.writer(fh); w.writerow(["year", "district", "county", "candidate", "party_code", "votes"])
    for c, (d, r) in [(c, (1, r)) for c, r in zip(W, Wv)]:
        w.writerow([1990, 1, cn(c), "Pat Williams", "D", r[0]]); w.writerow([1990, 1, cn(c), "Brad Johnson", "R", r[1]])
    for c, r in zip(E, Ev):
        w.writerow([1990, 2, cn(c), "Don Burris", "D", r[0]]); w.writerow([1990, 2, cn(c), "Ron Marlenee", "R", r[1]])
print("56 counties; columns tie to the printed TOTALS row")
