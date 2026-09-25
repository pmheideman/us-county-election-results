"""Ohio U.S. House 1992, county level, from the Ohio Secretary of State's 'Ohio Election Statistics
for 1991-92', table 'District Vote for Representatives to Congress and Pluralities, General
Election, November 3, 1992' (printed pp. 123-126).

Source: Internet Archive, Statistical Reference Index microfiche, item micro_IA40706946_0141
(page images micro_IA40706946_0141_0129..0132.jp2, fetched singly from the item's jp2.zip; local
copies R/data/county_house_files/sri/oh1992/p0129-p0132). Hand-transcribed from the page images.

Checks (the script stops on any failure):
  * every candidate column adds up to the printed district Totals row;
  * the printed Plurality equals the winner's total minus the runner-up's;
  * every candidate total equals the Clerk of the House 'Statistics of the Presidential and
    Congressional Election of November 3, 1992' (Ohio), write-ins included;
  * all 88 Ohio counties appear.
Parties follow the Clerk (District 1: Grote and Berns are Independents; the book prints no party
over them). Write-in candidates (WI) are used in the checks but not written to the CSV.

Output: R/data/county_house_files/sri/ohio_house_county_1992.csv (year,district,county,candidate,party_code,votes)
"""
import csv, os

HERE = os.path.dirname(os.path.abspath(__file__))
OUT = os.path.join(HERE, "../data/county_house_files/sri/ohio_house_county_1992.csv")

# district: (candidates [(name, party code; W = write-in)], county rows {county: [votes per candidate]},
#            printed Totals, printed Plurality, Clerk totals)
D = {
 1: ([("David Mann", "D"), ("Jim Berns", "I"), ("Steve Grote", "I"), ("Dennis L. Mayberry", "W"), ("Brian Ali Taylor", "W")],
     {"Hamilton": [120190, 12734, 101498, 6, 5]},
     [120190, 12734, 101498, 6, 5], 18692, [120190, 12734, 101498, None, None]),
 2: ([("Thomas R. Chandler", "D"), ("Willis D. Gradison Jr.", "R"), ("Emily Roughen Wood", "W")],
     {"Adams": [4166, 5365, 0], "Brown": [5986, 7799, 4], "Clermont": [19888, 40206, 0],
      "Hamilton": [36854, 108921, 3], "Warren": [9030, 15429, 0]},
     [75924, 177720, 7], 101796, [75924, 177720, 7]),
 3: ([("Tony P. Hall", "D"), ("Pete Davis", "R"), ("Ira T. McDonald", "W")],
     {"Montgomery": [146072, 98733, 6]},
     [146072, 98733, 6], 47339, [146072, 98733, 6]),
 4: ([("Raymond M. Ball", "D"), ("Michael G. Oxley", "R"), ("James R. Stahl", "W")],
     {"Allen": [16057, 30654, 10], "Auglaize": [4821, 10479, 0], "Crawford": [6795, 13166, 0],
      "Hancock": [11981, 16567, 464], "Hardin": [4555, 8189, 1], "Knox": [3170, 6418, 0],
      "Logan": [2372, 5520, 0], "Marion": [10581, 15419, 0], "Morrow": [4691, 6859, 3],
      "Richland": [24140, 27434, 3], "Wyandot": [3445, 6641, 5]},
     [92608, 147346, 486], 54738, [92608, 147346, 486]),
 5: ([("Paul E. Gillmor", "R")],
     {"Defiance": [12626], "Erie": [24199], "Henry": [10548], "Huron": [15578], "Lorain": [10387],
      "Mercer": [1888], "Ottawa": [10068], "Paulding": [6692], "Putnam": [13135], "Sandusky": [19457],
      "Seneca": [20154], "Van Wert": [10863], "Williams": [12876], "Wood": [19389]},
     [187860], None, [187860]),
 6: ([("Ted Strickland", "D"), ("Bob McEwen", "R")],
     {"Athens": [16184, 8652], "Clinton": [4565, 10067], "Gallia": [6893, 5916], "Highland": [5803, 9262],
      "Hocking": [5525, 4680], "Jackson": [5806, 6726], "Lawrence": [15292, 10753], "Meigs": [5214, 4727],
      "Pike": [5812, 5374], "Ross": [7386, 9405], "Scioto": [19493, 13982], "Vinton": [2426, 2659],
      "Warren": [6494, 16260], "Washington": [15827, 10789]},
     [122720, 119252], 3468, [122720, 119252]),
 7: ([("Clifford S. Heskett", "D"), ("Dave Hobson", "R")],
     {"Champaign": [4099, 10945], "Clark": [17226, 42547], "Fairfield": [15088, 30453], "Fayette": [2193, 6889],
      "Greene": [14555, 41285], "Logan": [2081, 7124], "Pickaway": [4940, 9812], "Ross": [2782, 4803],
      "Union": [3273, 10337]},
     [66237, 164195], 97958, [66237, 164195]),
 8: ([("Fred Sennet", "D"), ("John A. Boehner", "R")],
     {"Auglaize": [973, 3336], "Butler": [31427, 89375], "Darke": [5268, 17849], "Mercer": [3677, 10969],
      "Miami": [9959, 28733], "Montgomery": [395, 862], "Preble": [4897, 12062], "Shelby": [5437, 13176]},
     [62033, 176362], 114329, [62033, 176362]),
 9: ([("Marcy Kaptur", "D"), ("Ken D. Brown", "R"), ("Ed Howard", "I"), ("Mary Ann Haupricht", "W")],
     {"Fulton": [11960, 5749, 654, 1], "Lucas": [145003, 40424, 9382, 47], "Ottawa": [5057, 1594, 236, 1],
      "Wood": [16859, 5244, 890, 1]},
     [178879, 53011, 11162, 50], 125868, [178879, 53011, 11162, 50]),
 10: ([("Mary Rose Oakar", "D"), ("Martin R. Hoke", "R"), ("Guy Templeton Black", "W"), ("David Lee Rock", "W"), ("Peter Thierjung", "W")],
      {"Cuyahoga": [103788, 136433, 6, 0, 12]},
      [103788, 136433, 6, 0, 12], 32645, [103788, 136433, None, None, None]),
 11: ([("Louis Stokes", "D"), ("Beryl E. Rothschild", "R"), ("Ed Gudenas", "I"), ("Gerald Henley", "I"), ("Ronald Parks", "W")],
      {"Cuyahoga": [154718, 43866, 19773, 5267, 0]},
      [154718, 43866, 19773, 5267, 0], 110852, [154718, 43866, 19773, 5267, None]),
 12: ([("Bob Fitrakis", "D"), ("John R. Kasich", "R")],
      {"Delaware": [5978, 29730], "Franklin": [55174, 115327], "Licking": [7609, 25240]},
      [68761, 170297], 101536, [68761, 170297]),
 13: ([("Sherrod Brown", "D"), ("Margaret R. Mueller", "R"), ("Mark Miller", "I"), ("Werner J. Lange", "I"), ("Tom Lawson", "I")],
      {"Cuyahoga": [3360, 3273, 636, 95, 76], "Geauga": [15447, 15579, 2600, 927, 820],
       "Lorain": [59851, 27874, 7515, 1195, 1438], "Medina": [28038, 23427, 4730, 687, 914],
       "Portage": [11876, 8990, 2487, 384, 867], "Summit": [9091, 6237, 1465, 187, 348],
       "Trumbull": [6823, 3509, 887, 369, 256]},
      [134486, 88889, 20320, 3844, 4719], 45597, [134486, 88889, 20320, 3844, 4719]),
 14: ([("Thomas C. Sawyer", "D"), ("Robert Morgan", "R")],
      {"Portage": [23679, 10645], "Stark": [1200, 685], "Summit": [140456, 67329]},
      [165335, 78659], 86676, [165335, 78659]),
 15: ([("Richard Cordray", "D"), ("Deborah Pryce", "R"), ("Linda Reidelbach", "I"), ("Larry W. Oliver", "W")],
      {"Franklin": [89931, 103021, 41933, 2], "Madison": [4329, 6647, 2548, 0], "Pickaway": [647, 722, 425, 0]},
      [94907, 110390, 44906, 2], 15483, [94907, 110390, 44906, 2]),
 16: ([("Warner D. Mendenhall", "D"), ("Ralph Regula", "R")],
      {"Ashland": [6811, 12939], "Holmes": [2275, 6321], "Knox": [3925, 5092], "Stark": [62521, 108084],
       "Wayne": [14692, 26053]},
      [90224, 158489], 68265, [90224, 158489]),
 17: ([("James A. Traficant, Jr.", "D"), ("Salvatore Pansino", "R")],
      {"Columbiana": [35900, 9919], "Mahoning": [106174, 18847], "Trumbull": [74429, 11977]},
      [216503, 40743], 175760, [216503, 40743]),
 18: ([("Douglas Applegate", "D"), ("Bill Ress", "R")],
      {"Belmont": [26590, 6248], "Carroll": [7900, 4030], "Columbiana": [429, 104], "Coshocton": [9734, 5238],
       "Guernsey": [11465, 4389], "Harrison": [5815, 1838], "Jefferson": [30294, 7715], "Licking": [13317, 11134],
       "Monroe": [6089, 1149], "Morgan": [2906, 2902], "Muskingum": [17137, 14586], "Noble": [3860, 1672],
       "Perry": [6851, 5002], "Tuscarawas": [23802, 11222]},
      [166189, 77229], 88960, [166189, 77229]),
 19: ([("Eric D. Fingerhut", "D"), ("Robert A. Gardner", "R"), ("Don Mackle", "W"), ("Allan D. Mononen", "W")],
      {"Ashtabula": [21886, 19437, 5, 7], "Cuyahoga": [70709, 54619, 0, 0], "Lake": [45870, 50550, 0, 0]},
      [138465, 124606, 5, 7], 13859, [138465, 124606, None, None]),
}
# Clerk write-in totals per district (the Clerk reports write-ins as one number)
CLERK_WRITEIN = {1: 11, 2: 7, 3: 6, 4: 486, 9: 50, 10: 18, 15: 2, 19: 12}

rows, counties = [], set()
for d, (cands, cty, tot, plur, clerk) in D.items():
    n = len(cands)
    for c, v in cty.items():
        assert len(v) == n, (d, c)
    sums = [sum(v[j] for v in cty.values()) for j in range(n)]
    assert sums == tot, ("column totals", d, sums, tot)
    real = sorted((t for (nm, p), t in zip(cands, tot) if p != "W"), reverse=True)
    if plur is not None:
        assert real[0] - real[1] == plur, ("plurality", d, real[:2], plur)
    for (nm, p), t, ct in zip(cands, tot, clerk):
        if ct is not None:
            assert t == ct, ("Clerk", d, nm, t, ct)
    wi = sum(t for (nm, p), t in zip(cands, tot) if p == "W")
    assert wi == CLERK_WRITEIN.get(d, 0), ("Clerk write-ins", d, wi)
    for c, v in cty.items():
        counties.add(c.upper())
        for (nm, p), x in zip(cands, v):
            if p != "W":
                rows.append([1992, d, c.upper(), nm, p, x])

assert len(counties) == 88, len(counties)
with open(OUT, "w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["year", "district", "county", "candidate", "party_code", "votes"])
    w.writerows(rows)
print(f"{len(rows)} candidate-county rows, 19 districts, {len(counties)} counties; column totals, pluralities and Clerk totals all tie")
