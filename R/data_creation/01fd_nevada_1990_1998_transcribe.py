"""Nevada U.S. House county results 1990-1998, transcribed from the Nevada Secretary of State's official abstracts (downloaded from
https://www.nvsos.gov/elections/election-information/previous-elections/election-results, saved as R/data/county_house_files/nevada/NV_<year>_*.pdf):
  1990  "General Election Returns by County, November 6, 1990"  (PDF page 13, scanned; landscape table rotated)
  1992  "General Election Returns by County, November 3, 1992"  (PDF page 20, scanned)
  1994  "General Election Returns" (PDF page 11; OCR'd text layer too noisy, read from the page image)
  1996  "1996 Official General Election Returns"                (PDF page 13, scanned)
  1998  "1998 Official General Election Returns"                (PDF text layer)
The tables have 17 county columns (Carson City, Churchill, Clark, Douglas, Elko, Esmeralda, Eureka, Humboldt, Lander, Lincoln, Lyon, Mineral, Nye, Pershing, Storey, Washoe, White Pine) and a Total.
District 1 is entirely in Clark County (one value); district 2 has all 17 counties (Clark's and Washoe's are the district-2 parts of those counties).
Every transcribed row is checked: the 17 county cells add up to the printed Total (stop on failure); 1992 needed FEC-assisted digit corrections (see below). Output: R/data/county_house_files/nevada/nevada_house_county_1990_1998.csv."""
import csv, sys
COUNTIES = ["Carson City","Churchill","Clark","Douglas","Elko","Esmeralda","Eureka","Humboldt","Lander","Lincoln","Lyon","Mineral","Nye","Pershing","Storey","Washoe","White Pine"]
# (year, district, candidate, party, [17 county values] or {"Clark": v}, printed total)
D1 = lambda v: {"Clark": v}
ROWS = [
 (1990,1,"James H. Bilbray","D",D1(84650),84650),(1990,1,"Bob Dickinson","R",D1(47377),47377),(1990,1,"William (Bill) Moore","L",D1(5825),5825),
 (1990,2,"Dan Becan","L",[849,518,1689,630,346,48,27,175,119,64,609,189,494,115,125,5973,150],12120),
 (1990,2,"Barbara F. Vucanovich","R",[9004,3195,16473,7038,5931,366,527,2691,1176,1121,4284,1454,3296,851,705,43056,2340],103508),
 (1990,2,"Jane Wisdom","D",[4872,1939,12533,2865,2046,108,119,805,347,317,2191,873,1953,357,431,27039,786],59581),
 (1992,1,"James H. Bilbray","D",D1(128278),128278),(1992,1,"Scott A. Kjar","L",D1(8993),8993),(1992,1,"J. Coy Pettyjohn","R",D1(84217),84217),
 (1992,2,"Dan Becan","L",[476,147,1347,353,144,26,7,76,41,25,227,64,206,26,68,4277,42],7552),
 (1992,2,"Don Golden","POP",[211,61,879,166,65,3,3,25,22,8,118,19,114,6,25,1109,26],2860),
 (1992,2,"Daniel M. Hansen","IA",[936,503,1996,728,571,28,23,170,122,89,618,108,407,77,121,6546,242],13285),
 (1992,2,"Pete Sferrazza","D",[7691,2634,32502,5384,3816,225,203,1622,777,606,3912,1249,3822,666,669,49882,1539],117199),
 (1992,2,"Barbara F. Vucanovich","R",[8696,4044,34061,7898,7201,292,448,2662,1242,1077,4182,1170,3284,782,588,50147,1801],129575),
 (1994,1,"James H. Bilbray","D",D1(72333),72333),(1994,1,"John Ensign","R",D1(73769),73769),(1994,1,"Gary Wood","L",D1(6065),6065),
 (1994,2,"Lois Avery","NL",[551,196,1242,362,131,9,7,103,50,35,238,75,146,19,48,3454,59],6725),
 (1994,2,"Janet Greeson","D",[5186,1515,17837,3328,1496,98,106,830,406,333,2344,727,1968,340,472,27685,719],65390),
 (1994,2,"Thomas F. Jefferson","IA",[677,404,1851,542,679,27,43,150,101,106,492,94,533,73,93,3559,191],9615),
 (1994,2,"Barbara F. Vucanovich","R",[9463,4649,39900,9005,7583,397,504,3202,1533,1357,5069,1513,4377,1042,966,49419,2223],142202),
 (1996,1,"Bob Coffin","D",D1(75081),75081),(1996,1,"James Dan","L",D1(3341),3341),(1996,1,"Richard Eidson","NL",D1(3127),3127),(1996,1,"John Ensign","R",D1(86472),86472),(1996,1,"Ted Gunderson","IA",D1(4572),4572),
 (1996,2,"Lois Avery","NL",[254,95,1875,176,192,11,14,56,24,32,114,29,173,18,25,1477,63],4628),
 (1996,2,"Jim Gibbons","R",[10957,5279,45071,10764,7884,344,453,2924,1463,1077,5742,1176,5193,1006,997,60012,1968],162310),
 (1996,2,"Dan Hansen","IA",[543,302,2361,423,574,37,30,201,111,132,395,64,302,47,58,2993,207],8780),
 (1996,2,"Louis R. Tomburello","L",[194,58,1254,149,166,14,9,33,26,15,100,22,203,11,22,1419,37],3732),
 (1996,2,"Thomas \"Spike\" Wilson","D",[6668,1886,28862,4186,2709,132,160,1307,564,469,3133,981,3111,464,545,41470,1095],97742),
 (1998,1,"Shelley Berkley","D",D1(79315),79315),(1998,1,"Jim Burns","L",D1(5292),5292),(1998,1,"Don Chairez","R",D1(73540),73540),(1998,1,"Jess Howe","IA",D1(2935),2935),
 (1998,2,"Jim Gibbons","R",[12943,6106,62874,11636,8347,414,547,3492,1726,1482,7282,1580,6975,1242,1259,71279,2439],201623),
 (1998,2,"Christopher Horne","IA",[1399,500,8639,1103,648,47,46,290,95,194,779,276,686,101,127,5455,353],20738),
 (1998,2,"Louis R. Tomburello","L",[896,221,5878,644,459,35,34,183,107,85,541,96,798,81,141,8183,179],18561),
 (1998,2,"Robert W. Winquist","NL",[410,195,2391,338,268,13,13,94,62,58,269,83,418,45,62,3004,118],7841),
]
# 1992 is a fax-quality scan in which the digits 5 and 6 are easily confused (several PRINTED TOTALS were first misread that way too: Becan 7,552, Vucanovich 129,575). One cell was resolved with the FEC's
# official statewide total (R/data/fec_official/fec1992_house.csv): Sferrazza / Clark 32,502 (first read 32,602) so that his cells add up to the printed total and FEC figure 117,199. All other rows tie
# to the printed totals as read and agree with the FEC (minor candidate Golden: cells 2,860 as printed, FEC 2,850, left as read).
out = []
for y, d, name, party, v, tot in ROWS:
    if isinstance(v, dict):
        vals = {c: 0 for c in COUNTIES}; vals.update(v)
    else:
        assert len(v) == 17, (y, d, name, len(v)); vals = dict(zip(COUNTIES, v))
    if sum(vals.values()) != tot: sys.exit(f"SUM CHECK FAILED {y} CD{d} {name}: cells {sum(vals.values())} vs printed total {tot}")
    for c, x in vals.items():
        if x: out.append((y, d, name, party, c, x))
w = csv.writer(open("R/data/county_house_files/nevada/nevada_house_county_1990_1998.csv", "w", newline=""))
w.writerow(["year","district","candidate","party","county","votes"]); w.writerows(out)
print(len(ROWS), "candidate rows,", len(out), "county rows; all row sums tie to the printed totals")
