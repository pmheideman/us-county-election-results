## Delaware U.S. Representative (at-large) 1990, 1992, 1994, 1996, county level, hand-transcribed from the Department of Elections' scanned "Official Results of General Election for Statewide
## Offices" tables (R/data/county_house_files/delaware/DE_<year>_general.pdf; PDF page 3 in 1990, page 4 in 1992, 1994 and 1996). The table columns are City of Wilmington, Rural New Castle County,
## Kent, Sussex, Total; New Castle County = Wilmington + Rural New Castle. Every candidate row is checked against its printed total, and each column against the printed "Total Votes Cast".
## Writes R/data/county_house_files/delaware/delaware_house_county_1990_1996.csv (year, county, candidate, party_code, votes).
import csv, os
T = {  # year: [(candidate, party, [wilm, rural_nc, kent, sussex], printed_total)], plus printed column totals
 1990: ([("Thomas R. Carper","D",[12781,66632,15676,21185],116274),("Ralph O. Williams","R",[3210,33457,8591,12779],58037),("Richard Cohen","L",[297,1967,479,378],3121)], [16288,102056,24746,34342,177432]),
 1992: ([("S.B. Woo","D",[15350,66366,17399,18311],117426),("Michael N. Castle","R",[9363,93036,20460,30178],153037),("Peggy Schmitt","L",[345,3480,905,931],5661)], [25058,162882,38764,49420,276124]),
 1994: ([("Carol Ann DeSantis","D",[7344,28623,7232,8604],51803),("Michael N. Castle","R",[8513,80867,20349,28231],137960),("Donald M. Hockmuth","K",[163,797,226,219],1405),("Danny Ray Beaver","L",[225,2688,556,400],3869)], [16245,112975,28363,37454,195037]),
 1996: ([("Dennis E. Williams","D",[10899,40960,9653,11741],73253),("Michael N. Castle","R",[10521,108979,27978,38098],185576),("George A. Jurgensen","L",[234,2782,501,483],4000),("Robert E. Mattson","V",[72,640,127,148],987),("Felicia B. Johnson","P",[175,1676,573,585],3009)], [21901,155037,38832,51055,266825]),
}
out = []
for y, (cands, coltot) in T.items():
    for c, p, v, t in cands:
        assert sum(v) == t, (y, c, sum(v), t)
    for k in range(4):
        assert sum(c[2][k] for c in cands) == coltot[k], (y, k, sum(c[2][k] for c in cands), coltot[k])
    assert sum(c[3] for c in cands) == coltot[4], (y, "grand")
    for c, p, v, t in cands:
        for cty, n in (("NEW CASTLE", v[0] + v[1]), ("KENT", v[2]), ("SUSSEX", v[3])):
            out.append((y, cty, c, p, n))
    print(y, "ties:", len(cands), "candidates,", coltot[4], "votes")
f = os.path.join("R", "data", "county_house_files", "delaware", "delaware_house_county_1990_1996.csv")
os.makedirs(os.path.dirname(f), exist_ok=True)
with open(f, "w", newline="") as fh:
    w = csv.writer(fh); w.writerow(["year", "county", "candidate", "party_code", "votes"]); w.writerows(out)
print("wrote", len(out), "rows")
