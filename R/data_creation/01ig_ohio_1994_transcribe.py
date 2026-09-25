"""Ohio U.S. House 1994, county level, hand-transcribed from 'Ohio Election Statistics for
1993-94' (Ohio Secretary of State Bob Taft), 'District Vote for Representative to Congress,
General Election, Nov. 8, 1994', printed pp. 137-140.

Source: Internet Archive, Statistical Reference Index microfiche, item micro_IA40706953_0173
(https://archive.org/details/micro_IA40706953_0173); jp2 scan pages 142-145; local copy
R/data/county_house_files/sri/oh1994/. Values read by eye from the page images.

Layout: one table per district, one column per candidate (WI = write-in), county rows
(** = only part of the county is in the district) and a printed Total row. Single-county
districts (1, 3, 10, 11) print only the county total.

Checks (the script stops unless all pass):
  * every candidate column adds up to the printed district Total;
  * every non-write-in candidate total equals the Clerk of the House 'Statistics of the
    Congressional Election of November 8, 1994' (clerk.house.gov 94Stat.htm), and each
    district's write-in columns add up to the Clerk's write-in figure;
  * all 88 counties appear.
Write-in columns are used in the checks but left out of the CSV.

Output: R/data/county_house_files/sri/ohio_house_county_1994.csv
        (year, district, county, candidate, party_code, votes)
"""
import csv, os

HERE = os.path.dirname(os.path.abspath(__file__))
OUT = os.path.join(HERE, "../data/county_house_files/sri/ohio_house_county_1994.csv")

# district: (candidates [(name, party)], rows {county: [votes per candidate]}, printed totals)
D = {
    1: ([("Steve Chabot", "R"), ("David Mann", "D")],
        {"HAMILTON": [92997, 72822]}, [92997, 72822]),
    2: ([("Les Mann", "D"), ("Rob Portman", "R")],
        {"ADAMS": [2668, 5974], "BROWN": [3289, 8048], "CLERMONT": [9856, 35002],
         "HAMILTON": [23014, 85212], "WARREN": [4903, 15892]}, [43730, 150128]),
    3: ([("Tony P. Hall", "D"), ("David A. Westbrock", "R")],
        {"MONTGOMERY": [105342, 72314]}, [105342, 72314]),
    4: ([("Michael G. Oxley", "R")],
        {"ALLEN": [25904], "AUGLAIZE": [10275], "CRAWFORD": [11663], "HANCOCK": [16910],
         "HARDIN": [7224], "KNOX": [5554], "LOGAN": [5123], "MARION": [13908],
         "MORROW": [7089], "RICHLAND": [29760], "WYANDOT": [6431]}, [139841]),
    5: ([("Paul E. Gillmor", "R"), ("Jarrod Tudor", "D")],
        {"DEFIANCE": [8161, 2990], "ERIE": [15369, 7943], "HENRY": [7966, 1943],
         "HURON": [11821, 4821], "LORAIN": [7378, 3932], "MERCER": [1364, 583],
         "OTTAWA": [7033, 2451], "PAULDING": [4751, 2045], "PUTNAM": [10090, 2521],
         "SANDUSKY": [14871, 5401], "SENECA": [14120, 4722], "VAN WERT": [7901, 2458],
         "WILLIAMS": [8790, 2357], "WOOD": [16264, 5168]}, [135879, 49335]),
    6: ([("Frank A. Cremeans", "R"), ("Ted Strickland", "D")],
        {"ATHENS": [6017, 10336], "CLINTON": [6669, 4537], "GALLIA": [5634, 4679],
         "HIGHLAND": [5780, 5066], "HOCKING": [3418, 4135], "JACKSON": [5123, 4390],
         "LAWRENCE": [9702, 8615], "MEIGS": [3572, 3712], "PIKE": [3673, 5165],
         "ROSS": [5535, 6496], "SCIOTO": [11228, 13635], "VINTON": [1897, 2385],
         "WARREN": [12480, 4916], "WASHINGTON": [10535, 9794]}, [91263, 87861]),
    7: ([("Dave Hobson", "R")],
        {"CHAMPAIGN": [8569], "CLARK": [35128], "FAIRFIELD": [27631], "FAYETTE": [5254],
         "GREENE": [36928], "LOGAN": [5468], "PICKAWAY": [8252], "ROSS": [4480],
         "UNION": [8414]}, [140124]),
    8: ([("John A. Boehner", "R"), ("Rogers H. Campbell", "WI")],
        {"AUGLAIZE": [3292, 3], "BUTLER": [73635, 62], "DARKE": [15032, 3],
         "MERCER": [9102, 7], "MIAMI": [25088, 4], "MONTGOMERY": [848, 0],
         "PREBLE": [9816, 4], "SHELBY": [11525, 4]}, [148338, 87]),
    9: ([("Marcy Kaptur", "D"), ("R. Randy Whitman", "R")],
        {"FULTON": [8102, 4903], "LUCAS": [95754, 28750], "OTTAWA": [3747, 1216],
         "WOOD": [10517, 3796]}, [118120, 38665]),
    10: ([("Francis E. Gaul", "D"), ("Martin R. Hoke", "R"), ("Joseph J. Jacobs, Jr.", "I")],
         {"CUYAHOGA": [70918, 95226, 17495]}, [70918, 95226, 17495]),
    11: ([("Roni L. McCann", "WI"), ("Louis Stokes", "D"), ("James J. Sykora", "R")],
         {"CUYAHOGA": [3, 114220, 33705]}, [3, 114220, 33705]),
    12: ([("Edward S. Brown", "WI"), ("John B. Hurd", "WI"), ("John R. Kasich", "R"),
          ("Greg Powers", "WI"), ("Cynthia L. Ruccia", "D"), ("John Yiamouyianni", "WI")],
         {"DELAWARE": [12, 28, 22556, 1, 7478, 86], "FRANKLIN": [31, 73, 72872, 27, 42854, 122],
          "LICKING": [10, 16, 19180, 1, 6962, 36]}, [53, 117, 114608, 29, 57294, 244]),
    13: ([("Sherrod Brown", "D"), ("Howard Mason", "I"), ("John Michael Ryan", "I"),
          ("Gregory A. White", "R")],
         {"CUYAHOGA": [2757, 195, 93, 2859], "GEAUGA": [11991, 966, 265, 15946],
          "LORAIN": [35853, 2373, 882, 30658], "MEDINA": [20620, 2297, 476, 21012],
          "PORTAGE": [9446, 975, 333, 7814], "SUMMIT": [7300, 498, 203, 5359],
          "TRUMBULL": [5180, 473, 178, 2774]}, [93147, 7777, 2430, 86422]),
    14: ([("William L. Goff", "WI"), ("Thomas C. Sawyer", "D"), ("Lynn Slaby", "R")],
         {"PORTAGE": [0, 14298, 10256], "STARK": [0, 688, 765], "SUMMIT": [24, 81288, 78085]},
         [24, 96274, 89106]),
    15: ([("Bill Buckel", "D"), ("Ronald D. Dempsey", "WI"), ("Lawrence W. Oliver", "WI"),
          ("Deborah Pryce", "R"), ("John E. Roessler", "WI"), ("John J. Wing", "WI")],
         {"FRANKLIN": [43453, 246, 5, 104768, 22, 3], "MADISON": [2659, 27, 0, 7217, 0, 0],
          "PICKAWAY": [368, 1, 0, 927, 0, 0]}, [46480, 274, 5, 112912, 22, 3]),
    16: ([("J. Michael Finn", "D"), ("Ralph Regula", "R")],
         {"ASHLAND": [3815, 10461], "HOLMES": [1384, 5068], "KNOX": [2412, 4320],
          "STARK": [31038, 94105], "WAYNE": [7132, 23368]}, [45781, 137322]),
    17: ([("Mike G. Meister", "R"), ("James A. Traficant, Jr.", "D")],
         {"COLUMBIANA": [11205, 22201], "MAHONING": [17868, 73701], "TRUMBULL": [14417, 53102]},
         [43490, 149004]),  # the last digit of the printed 149,004 is smudged; the column sum and the Clerk give 149,004
    18: ([("Greg L. DiDonato", "D"), ("Bob Ney", "R")],
         {"BELMONT": [8506, 16934], "CARROLL": [4422, 4833], "COLUMBIANA": [306, 218],
          "COSHOCTON": [4245, 6795], "GUERNSEY": [4513, 7057], "HARRISON": [3036, 2845],
          "JEFFERSON": [15520, 13186], "LICKING": [8576, 10661], "MONROE": [2166, 3714],
          "MORGAN": [1501, 3621], "MUSKINGUM": [9758, 16245], "NOBLE": [1452, 3417],
          "PERRY": [4121, 5256], "TUSCARAWAS": [19804, 8333]}, [87926, 103115]),
    19: ([("Jerome A. Brentar", "I"), ("Eric D. Fingerhut", "D"), ("Steven C. LaTourette", "R"),
          ("Ronald E. Young", "I")],
         {"ASHTABULA": [715, 14109, 13939, 2766], "CUYAHOGA": [2575, 51743, 43430, 3559],
          "LAKE": [1890, 23849, 42628, 5039]}, [5180, 89701, 99997, 11364]),
}

# Clerk of the House, 1994 statistics (94Stat.htm): candidate totals by district (non-write-in), and write-in totals
CLERK = {
    1: {"Steve Chabot": 92997, "David Mann": 72822}, 2: {"Rob Portman": 150128, "Les Mann": 43730},
    3: {"David A. Westbrock": 72314, "Tony P. Hall": 105342}, 4: {"Michael G. Oxley": 139841},
    5: {"Paul E. Gillmor": 135879, "Jarrod Tudor": 49335},
    6: {"Frank A. Cremeans": 91263, "Ted Strickland": 87861}, 7: {"Dave Hobson": 140124},
    8: {"John A. Boehner": 148338}, 9: {"R. Randy Whitman": 38665, "Marcy Kaptur": 118120},
    10: {"Martin R. Hoke": 95226, "Francis E. Gaul": 70918, "Joseph J. Jacobs, Jr.": 17495},
    11: {"James J. Sykora": 33705, "Louis Stokes": 114220},
    12: {"John R. Kasich": 114608, "Cynthia L. Ruccia": 57294},
    13: {"Gregory A. White": 86422, "Sherrod Brown": 93147, "Howard Mason": 7777, "John Michael Ryan": 2430},
    14: {"Lynn Slaby": 89106, "Thomas C. Sawyer": 96274}, 15: {"Deborah Pryce": 112912, "Bill Buckel": 46480},
    16: {"Ralph Regula": 137322, "J. Michael Finn": 45781},
    17: {"Mike G. Meister": 43490, "James A. Traficant, Jr.": 149004},
    18: {"Bob Ney": 103115, "Greg L. DiDonato": 87926},
    19: {"Steven C. LaTourette": 99997, "Eric D. Fingerhut": 89701, "Ronald E. Young": 11364, "Jerome A. Brentar": 5180},
}
CLERK_WRITEIN = {8: 87, 11: 3, 12: 443, 14: 24, 15: 304}

out = []
for d, (cands, rows, tot) in D.items():
    for j, (nm, p) in enumerate(cands):
        s = sum(v[j] for v in rows.values())
        assert s == tot[j], (d, nm, s, tot[j])
        if p != "WI":
            assert CLERK[d][nm] == s, (d, nm, s, CLERK[d][nm])
    wi = sum(tot[j] for j, (_, p) in enumerate(cands) if p == "WI")
    assert wi == CLERK_WRITEIN.get(d, 0), (d, wi)
    assert set(CLERK[d]) == {nm for nm, p in cands if p != "WI"}, d
    for c, v in rows.items():
        for (nm, p), x in zip(cands, v):
            if p != "WI":
                out.append([1994, d, c, nm, p, x])
counties = {r[2] for r in out}
assert len(counties) == 88, (len(counties), sorted(counties))
with open(OUT, "w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["year", "district", "county", "candidate", "party_code", "votes"])
    w.writerows(out)
print(f"{len(out)} candidate-county rows, 19 districts, 88 counties; columns tie to printed totals and to the Clerk")
