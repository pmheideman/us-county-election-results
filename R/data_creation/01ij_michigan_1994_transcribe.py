"""Michigan U.S. House 1994, county level, hand-transcribed from the Michigan Department of
State, Bureau of Elections 'Election Results, General Election, November 8, 1994' (report
CL75.RPT, dated Sat, Nov 26, 1994), pages 13-19: '<n> Congressional District 2 Year Term'
tables with County, Total by County, one column per candidate and Write-In (SCAT).

Source: Internet Archive, Statistical Reference Index microfiche, item micro_IA40706952_0453
('State of Michigan General Election, Nov. 8, 1994'), scan pages 15-21; local copy
R/data/county_house_files/sri/mi1994/. Values read by eye from the page images.

Checks (the script stops unless all hold): every county row's candidate votes plus write-ins
equal its printed Total by County; every column equals the printed TOTALS row; every
candidate total equals the Clerk of the House 'Statistics of the Congressional Election of
November 8, 1994' (Michigan). All 83 counties appear.

Output: R/data/county_house_files/sri/michigan_house_county_1994.csv
        (year,district,county,candidate,party_code,votes; write-ins excluded)
"""
import csv, os

HERE = os.path.dirname(os.path.abspath(__file__))
OUT = os.path.join(HERE, "../data/county_house_files/sri/michigan_house_county_1994.csv")

# district: (candidates [(name, printed party)], rows [(county, total_by_county, [candidate votes], write_in)], printed TOTALS row [total, cands..., write_in])
D = {
    1: ([("Bart Stupak", "DEM"), ("Gil Ziegler", "REP"), ("Michael McPeak", "NLP")], [
        ("ALGER", 3367, [2175, 1173, 19], 0), ("ALPENA", 10578, [6436, 4055, 87], 0), ("ANTRIM", 7811, [3558, 4184, 69], 0),
        ("BARAGA", 2780, [1693, 1077, 10], 0), ("BENZIE", 5084, [2432, 2616, 35], 1), ("CHARLEVOIX", 8821, [4256, 4369, 196], 0),
        ("CHEBOYGAN", 8063, [4333, 3690, 40], 0), ("CHIPPEWA", 10392, [6539, 3768, 85], 0), ("CRAWFORD", 1061, [492, 527, 42], 0),
        ("DELTA", 13962, [9926, 3980, 55], 1), ("DICKINSON", 9721, [6241, 3459, 21], 0), ("EMMET", 10632, [5277, 5132, 223], 0),
        ("GOGEBIC", 6338, [4367, 1911, 50], 10), ("GRAND TRAVERSE", 26497, [11884, 13874, 698], 41), ("HOUGHTON", 11144, [5982, 5076, 85], 1),
        ("IRON", 5353, [3734, 1590, 29], 0), ("KALKASKA", 4669, [2292, 2338, 39], 0), ("KEWEENAW", 994, [550, 433, 11], 0),
        ("LEELANAU", 8202, [3690, 4406, 104], 2), ("LUCE", 1924, [1132, 785, 7], 0), ("MACKINAC", 4683, [2656, 1987, 40], 0),
        ("MARQUETTE", 21319, [14499, 6645, 175], 0), ("MENOMINEE", 7672, [4700, 2955, 17], 0), ("MONTMORENCY", 3531, [1857, 1659, 15], 0),
        ("ONTONAGON", 3473, [2108, 1341, 24], 0), ("OTSEGO", 6981, [3415, 3463, 103], 0), ("PRESQUE ISLE", 5550, [3201, 2244, 105], 0),
        ("SCHOOLCRAFT", 2947, [2008, 923, 15], 1)],
        [213549, 121433, 89660, 2399, 57]),
    2: ([("Marcus Hoover", "DEM"), ("Peter Hoekstra", "REP"), ("Lu Wiggins", "NLP")], [
        ("ALLEGAN", 23876, [4582, 18996, 295], 3), ("BARRY", 6631, [1734, 4833, 64], 0), ("LAKE", 3191, [1238, 1871, 82], 0),
        ("MANISTEE", 7666, [2369, 5262, 35], 0), ("MASON", 10140, [2593, 7398, 148], 1), ("MUSKEGON", 44776, [16717, 27753, 303], 3),
        ("NEWAYGO", 12412, [3209, 9050, 153], 0), ("OCEANA", 6959, [1861, 5068, 30], 0), ("OTTAWA", 69654, [9477, 59479, 676], 22),
        ("WEXFORD", 8882, [2317, 6454, 106], 5)],
        [194187, 46097, 146164, 1892, 34]),
    3: ([("Betsy Flory", "DEM"), ("Vernon Ehlers", "REP"), ("Barrie Konicov", "LIB"), ("Susan Normandin", "NLP")], [
        ("BARRY", 7741, [2185, 5351, 151, 54], 0), ("IONIA", 15689, [4358, 10997, 239, 95], 0), ("KENT", 161647, [37037, 120363, 2570, 1666], 11)],
        [185077, 43580, 136711, 2960, 1815, 11]),
    4: ([("Damion Frasier", "DEM"), ("Dave Camp", "REP"), ("Michael Lee", "NLP")], [
        ("CLARE", 8852, [2242, 6420, 190], 0), ("CLINTON", 22705, [5886, 16437, 381], 1), ("CRAWFORD", 3261, [706, 2469, 86], 0),
        ("GLADWIN", 7990, [1912, 5915, 163], 0), ("GRATIOT", 11198, [2565, 8525, 108], 0), ("ISABELLA", 15362, [4170, 10915, 277], 0),
        ("MECOSTA", 9954, [2582, 7211, 160], 1), ("MIDLAND", 29287, [5993, 23028, 266], 0), ("MISSAUKEE", 4769, [778, 3882, 109], 0),
        ("MONTCALM", 14552, [3934, 10512, 106], 0), ("OGEMAW", 6655, [1789, 4752, 114], 0), ("OSCEOLA", 6874, [1600, 5240, 34], 0),
        ("OSCODA", 2942, [628, 2217, 97], 0), ("ROSCOMMON", 9283, [2258, 6957, 67], 1), ("SAGINAW", 26818, [8173, 18234, 401], 10),
        ("SHIAWASSEE", 18032, [5328, 12462, 238], 4)],
        [198534, 50544, 145176, 2797, 17]),
    5: ([("James Barcia", "DEM"), ("William Anderson", "REP"), ("Susan Arnold", "NLP"), ("Larry Fairchild", "NPA")], [
        ("ALCONA", 3921, [2023, 1849, 23, 26], 0), ("ARENAC", 4945, [3368, 1525, 23, 29], 0), ("BAY", 39247, [29995, 8874, 149, 226], 3),
        ("GENESEE", 37503, [24880, 11173, 714, 735], 1), ("HURON", 11993, [7826, 4028, 52, 86], 1), ("IOSCO", 10027, [6190, 3412, 211, 214], 0),
        ("LAPEER", 10709, [5472, 4567, 341, 329], 0), ("SAGINAW", 44559, [30372, 12497, 553, 1095], 42), ("SANILAC", 12880, [5570, 7102, 111, 97], 0),
        ("TUSCOLA", 17406, [10760, 6315, 146, 185], 0)],
        [193190, 126456, 61342, 2323, 3022, 47]),
    6: ([("David Taylor", "DEM"), ("Fred Upton", "REP"), ("E. Berker", "NLP")], [
        ("ALLEGAN", 4616, [1186, 3395, 35], 0), ("BERRIEN", 43113, [10237, 32318, 534], 24), ("CASS", 12475, [3599, 8766, 110], 0),
        ("KALAMAZOO", 71445, [19380, 51251, 814], 0), ("ST. JOSEPH", 14689, [3034, 11574, 78], 3), ("VAN BUREN", 19628, [4912, 14619, 96], 1)],
        [165966, 42348, 121923, 1667, 28]),
    7: ([("Kim McCaughtry", "DEM"), ("Nick Smith", "REP"), ("Kenneth Proctor", "LIB"), ("Scott Williamson", "NLP")], [
        ("BARRY", 2573, [771, 1743, 42, 17], 0), ("BRANCH", 11512, [3019, 8373, 77, 41], 2), ("CALHOUN", 39885, [14497, 24164, 696, 519], 9),
        ("EATON", 35506, [11824, 22555, 870, 251], 6), ("HILLSDALE", 11969, [2530, 9243, 158, 38], 0), ("JACKSON", 44554, [12923, 30355, 1021, 255], 0),
        ("LENAWEE", 25828, [9738, 15725, 297, 68], 0), ("WASHTENAW", 5673, [2024, 3463, 150, 34], 2)],
        [177500, 57326, 115621, 3311, 1223, 19]),
    8: ([("Bob Mitchell", "DEM"), ("Dick Chrysler", "REP"), ("Gerald Turcotte Jr", "LIB"), ("Susan McPeak", "NLP")], [
        ("GENESEE", 39402, [19238, 19191, 509, 462], 2), ("INGHAM", 95583, [49020, 42952, 2052, 1559], 0),
        ("LIVINGSTON", 45885, [14822, 29281, 1152, 630], 0), ("OAKLAND", 4409, [1655, 2606, 100, 47], 1),
        ("SHIAWASSEE", 5876, [2707, 3007, 89, 68], 5), ("WASHTENAW", 21326, [7941, 12626, 446, 310], 3)],
        [212481, 95383, 109663, 4348, 3076, 11]),
    9: ([("Dale Kildee", "DEM"), ("Megan O'Neill", "REP"), ("Karen Blasdell", "NLP")], [
        ("GENESEE", 66022, [49167, 16110, 741], 4), ("LAPEER", 14944, [6157, 8412, 375], 0), ("OAKLAND", 108597, [41772, 64626, 2124], 75)],
        [189563, 97096, 89148, 3240, 79]),
    10: ([("David Bonior", "DEM"), ("Donald Lobsinger", "REP")], [
        ("MACOMB", 148193, [91894, 56253], 46), ("ST. CLAIR", 47629, [29982, 17609], 38)],
        [195822, 121876, 73862, 84]),
    11: ([("Mike Breshgold", "DEM"), ("Joe Knollenberg", "REP"), ("John Hocking", "NLP")], [
        ("OAKLAND", 178306, [55040, 121028, 2149], 89), ("WAYNE", 48578, [14128, 33668, 779], 3)],
        [226884, 69168, 154696, 2928, 92]),
    12: ([("Sander Levin", "DEM"), ("John Pappageorge", "REP"), ("Eric Anderson", "NLP"), ("Jerome White", "NPA")], [
        ("MACOMB", 93527, [46786, 45400, 654, 687], 0), ("OAKLAND", 105491, [56722, 47362, 686, 699], 22)],
        [199018, 103508, 92762, 1340, 1386, 22]),
    13: ([("Lynn Rivers", "DEM"), ("John Schall", "REP"), ("Craig Seymour", "LIB"), ("Gail Petrosoff", "NLP"), ("Helen Halyard", "NPA")], [
        ("WASHTENAW", 63446, [39126, 22563, 1072, 230, 442], 13), ("WAYNE", 109232, [50447, 55345, 2114, 376, 946], 4)],
        [172678, 89573, 77908, 3186, 606, 1388, 17]),
    14: ([("John Conyers Jr.", "DEM"), ("Richard Fournier", "REP"), ("Richard Miller", "NLP")], [
        ("WAYNE", 157631, [128463, 26215, 2953], 0)],
        [157631, 128463, 26215, 2953, 0]),
    15: ([("Barbara-Rose Collins", "DEM"), ("John Savage II", "REP"), ("Henry Clark", "NLP"), ("Cynthia Jaquith", "NPA"), ("Larry Roberts", "NPA")], [
        ("WAYNE", 142014, [119442, 20074, 848, 987, 654], 9)],
        [142014, 119442, 20074, 848, 987, 654, 9]),
    16: ([("John Dingell", "DEM"), ("Ken Larkin", "REP"), ("Noha Hamze", "NLP")], [
        ("MONROE", 37171, [21241, 15727, 203], 0), ("WAYNE", 141809, [84608, 55432, 1765], 4)],
        [178980, 105849, 71159, 1968, 4]),
}
# Clerk of the House, Statistics of the Congressional Election of November 8, 1994 (Michigan), in the column order above
CLERK = {1: [121433, 89660, 2399], 2: [46097, 146164, 1892], 3: [43580, 136711, 2960, 1815], 4: [50544, 145176, 2797],
         5: [126456, 61342, 2323, 3022], 6: [42348, 121923, 1667], 7: [57326, 115621, 3311, 1223], 8: [95383, 109663, 4348, 3076],
         9: [97096, 89148, 3240], 10: [121876, 73862], 11: [69168, 154696, 2928], 12: [103508, 92762, 1340, 1386],
         13: [89573, 77908, 3186, 606, 1388], 14: [128463, 26215, 2953], 15: [119442, 20074, 848, 987, 654], 16: [105849, 71159, 1968]}

out, counties = [], set()
for d, (cands, rows, tot) in D.items():
    n = len(cands)
    for c, t, v, w in rows:
        assert len(v) == n, (d, c)
        assert sum(v) + w == t, ("row", d, c, sum(v) + w, t)
        counties.add(c)
    assert sum(r[1] for r in rows) == tot[0], ("total col", d)
    for j in range(n):
        assert sum(r[2][j] for r in rows) == tot[1 + j], ("col", d, j)
    assert sum(r[3] for r in rows) == tot[-1], ("write-in col", d)
    assert tot[1:1 + n] == CLERK[d], ("clerk", d)
    for c, t, v, w in rows:
        for (nm, p), x in zip(cands, v):
            out.append([1994, d, c, nm, {"DEM": "D", "REP": "R"}.get(p, "I"), x])
assert len(counties) == 83, len(counties)
with open(OUT, "w", newline="") as f:
    wr = csv.writer(f)
    wr.writerow(["year", "district", "county", "candidate", "party_code", "votes"])
    wr.writerows(out)
print(f"{len(out)} candidate-county rows, 16 districts, 83 counties; rows, columns and Clerk totals all tie")
