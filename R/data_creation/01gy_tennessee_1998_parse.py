"""Tennessee U.S. House county results, November 3, 1998, from the Secretary of State's archived "U.S. House of Representatives, November 3, 1998 - General Election" county report (image-only PDF,
3 pages, https://tnelections.tnsosfiles.com/sharetngov/archived/election/results/1998-11/us-house.pdf, downloaded by the project lead in a browser because the file host refuses curl; saved as
R/data/county_house_files/tennessee/us-house.pdf). The pages were OCR'd (tesseract --psm 6, 200 dpi; text in ocr_1998/) and the county rows below were keyed from that text, with misread digits corrected
against the page images (District 4 Moore 441, District 7 printed total 91,503, District 8 Madison 11,615 differ from the raw OCR). Each district table lists counties (a county split between districts appears in each) with candidate columns 1-5 and a Write-In's column (not used) and a DISTRICT TOTAL row.
Checks (stop on failure): every district's county rows add up to its printed DISTRICT TOTAL in every candidate column. Output: R/data/county_house_files/tennessee/tennessee_house_county_1998.csv."""
import csv, re
def rows(s):
    out = []
    for l in s.strip().split("\n"):
        p = l.split(); k = next(i for i, x in enumerate(p) if re.fullmatch(r"[\d,]+", x)); out.append(("".join(p[:k]), [int(x.replace(",", "")) for x in p[k:]]))
    return out
D = {
 1: ([("Kay C. White", "D"), ("William L. (Bill) Jenkins", "R")], [30710, 68904], """CARTER 2728 6290
COCKE 1031 2814
GREENE 2844 6149
HANCOCK 200 721
HAWKINS 2527 6925
JEFFERSON 1498 3801
JOHNSON 637 1865
KNOX 203 359
SEVIER 2720 6979
SULLIVAN 10190 20166
UNICOI 1117 1960
WASHINGTON 5015 10875"""),
 2: ([("John J. Duncan Jr.", "R"), ("George Njezic", "I"), ("Greg Samples", "I"), ("Robert O. Watson", "I")], [90860, 2920, 4332, 4372], """BLOUNT 16974 456 714 526
BRADLEY 6272 136 399 407
KNOX 50334 1927 2560 2243
LOUDON 5777 152 262 214
MCMINN 7310 156 249 583
MONROE 4193 93 148 399"""),
 3: ([("James M. Lewis Jr.", "D"), ("Zach Wamp", "R"), ("Richard M. Sims", "I")], [37144, 75100, 1468], """ANDERSON 4853 10298 219
BLEDSOE 1108 1722 37
BRADLEY 1914 6063 117
GRUNDY 1158 715 11
HAMILTON 17392 40586 673
MARION 2844 2327 74
MEIGS 555 821 24
MORGAN 1110 1425 26
POLK 1402 1681 39
ROANE 3616 7735 204
SEQUATCHIE 758 1203 22
VANBUREN 434 524 22"""),
 4: ([("Jerry W. Cooper", "D"), ("Van Hilleary", "R")], [42627, 62829], """BEDFORD 2158 3012
CAMPBELL 2658 4036
CLAIBORNE 1405 2512
COFFEE 4189 4997
CUMBERLAND 3573 6836
FENTRESS 693 1412
FRANKLIN 3338 3191
GILES 2054 1893
GRAINGER 781 1543
HAMBLEN 3248 7235
HARDIN 1575 2618
KNOX 660 1617
LAWRENCE 3358 4780
LINCOLN 2550 2904
MOORE 349 441
PICKETT 483 810
RHEA 1258 3190
SCOTT 1104 2096
UNION 720 1376
WARREN 3963 2658
WAYNE 837 1622
WHITE 1673 2050"""),
 5: ([("Bob Clement", "D"), ("Al Borgman", "I"), ("William M. Lancaster", "I"), ("Gary I. Worden", "I")], [74611, 4983, 6162, 4345], """DAVIDSON 69658 4807 5527 4091
ROBERTSON 4953 176 635 254"""),
 6: ([("Bart Gordon", "D"), ("Walt Massey", "R")], [75055, 62277], """CANNON 1530 663
CLAY 1227 527
DAVIDSON 597 926
DEKALB 1983 809
JACKSON 2149 514
MACON 1767 1099
MARSHALL 2636 1360
OVERTON 2237 777
PUTNAM 9226 5286
RUTHERFORD 16336 13020
SMITH 2843 917
SUMNER 11918 11078
TROUSDALE 1035 314
WILLIAMSON 9919 16767
WILSON 9652 8220"""),
 7: ([("Ed Bryant", "R")], [91503], """CHEATHAM 3834
CHESTER 1203
DECATUR 1579
DICKSON 2805
FAYETTE 4013
HARDEMAN 1673
HENDERSON 2985
HICKMAN 2001
LEWIS 1271
MAURY 6447
MCNAIRY 2601
MONTGOMERY 17537
PERRY 464
ROBERTSON 1002
SHELBY 42088"""),
 8: ([("John S. Tanner", "D")], [76803], """BENTON 2949
CARROLL 4408
CROCKETT 1367
DYER 3593
GIBSON 8474
HAYWOOD 2041
HENRY 6062
HOUSTON 847
HUMPHREYS 3100
LAKE 595
LAUDERDALE 2172
MADISON 11615
OBION 5123
SHELBY 13264
STEWART 1747
TIPTON 5046
WEAKLEY 4400"""),
 9: ([("Harold E. Ford Jr.", "D"), ("Claude Burdikoff", "I"), ("Johnny Kelly", "I"), ("Gwendolyn L. Moore", "I"), ("Greg Voehringer", "I")], [75428, 18078, 775, 932, 567], """SHELBY 75428 18078 775 932 567"""),
}
out = []; ok = True
for d, (cands, tot, txt) in D.items():
    rs = rows(txt); assert all(len(v) == len(cands) for _, v in rs), (d, [r for r in rs if len(r[1]) != len(cands)])
    sums = [sum(v[k] for _, v in rs) for k in range(len(cands))]
    if sums != tot: print("MISMATCH district", d, "sums", sums, "printed", tot, "diff", [s - t for s, t in zip(sums, tot)]); ok = False; continue
    for c, v in rs:
        for (nm, p), x in zip(cands, v): out.append((1998, d, c, nm, p, x))
assert ok, "fix the districts above"
with open("R/data/county_house_files/tennessee/tennessee_house_county_1998.csv", "w", newline="") as fh:
    w = csv.writer(fh); w.writerow(["year", "district", "county", "candidate", "party_code", "votes"]); w.writerows(out)
print("all 9 districts tie to their printed DISTRICT TOTAL;", len(set((r[2]) for r in out)), "counties")
