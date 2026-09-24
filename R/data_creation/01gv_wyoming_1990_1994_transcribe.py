"""Wyoming U.S. House (at-large) 1990, 1992 and 1994 by county, hand-transcribed from the "Official Vote - General Election" (1990, 1992) and "Official Summary of General Election" (1994) tables in the
Secretary of State's "Wyoming Official Directory and Election Returns" volumes, as scanned in the Internet Archive's Statistical Reference Index microfiche collection (items micro_IA40706939_0468 (1990,
leaf n194), micro_IA40706946_0378 (1992, n164), micro_IA40706953_0409 (1994, n168); page images saved in R/data/county_house_files/wyoming/ia_sri). Checks (stop on failure): the 23 county rows add up to the
printed Official Totals of every candidate (the 1990 and 1992 totals also equal the FEC's official results). Output: R/data/county_house_files/wyoming/wyoming_house_county_1990_1994.csv (year, county, candidate, party_code, votes)."""
import csv
C = "Albany BigHorn Campbell Carbon Converse Crook Fremont Goshen HotSprings Johnson Laramie Lincoln Natrona Niobrara Park Platte Sheridan Sublette Sweetwater Teton Uinta Washakie Weston".split()
T = {
 1990: ([("Craig Thomas", "R"), ("Pete Maxfield", "D")], [87078, 70977], """4150 5111
2640 1425
5618 2745
2831 3119
2382 1515
1733 651
5948 5500
2439 2294
1280 847
1695 757
12088 11661
2874 1825
11694 9935
808 405
5111 2698
2081 1586
5088 4334
1498 711
5092 7048
3412 2014
3009 2407
1953 1468
1654 921"""),
 1992: ([("Craig Thomas", "R"), ("Jon Herschler", "D"), ("Craig McCune", "L")], [113882, 77418, 5677], """6322 6050 354
3036 1368 133
7426 3143 314
3245 3253 150
2852 1835 90
1902 673 80
8267 5001 404
3425 1651 116
1420 883 55
2057 897 103
17492 13195 1124
3751 1797 182
13842 11318 697
859 383 25
7623 3227 237
2361 1453 177
5891 5219 370
1704 792 86
7051 7340 387
4739 3149 242
4117 2538 192
2474 1348 79
2026 905 80"""),
 1994: ([("Barbara Cubin", "R"), ("Bob Schuster", "D"), ("Dave Dawson", "L")], [104426, 81022, 10749], """5467 5757 689
3062 1406 159
6676 3564 584
2996 3264 364
2677 1725 315
1842 677 135
7648 5268 881
3175 1860 220
1236 738 84
2153 778 110
14623 14166 1549
3828 2133 264
12817 11507 2047
835 358 83
6947 3350 503
2503 1580 266
6304 4084 658
1903 856 126
5835 8778 826
3575 3881 286
3780 3277 343
2449 1258 122
2095 757 135"""),
}
out = []
for y, (cands, tot, txt) in T.items():
    rows = [[int(x) for x in l.split()] for l in txt.split("\n")]
    assert len(rows) == 23 and all(len(r) == len(cands) for r in rows), y
    sums = [sum(r[k] for r in rows) for k in range(len(cands))]
    assert sums == tot, (y, sums, tot)
    for c, r in zip(C, rows):
        for (nm, p), v in zip(cands, r): out.append((y, c, nm, p, v))
    print(y, "23 counties tie to the printed Official Totals", tot)
with open("R/data/county_house_files/wyoming/wyoming_house_county_1990_1994.csv", "w", newline="") as fh:
    w = csv.writer(fh); w.writerow(["year", "county", "candidate", "party_code", "votes"]); w.writerows(out)
