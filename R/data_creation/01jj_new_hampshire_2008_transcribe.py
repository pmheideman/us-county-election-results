"""New Hampshire U.S. House 2008, county level, from the 'State of New Hampshire Manual for the General Court' 2009 (No. 61, NH Department of
State), 'U.S. HOUSE: FIRST DISTRICT' (printed p. 416) and 'SECOND DISTRICT' (pp. 417-418), General Election Nov. 4, 2008.
Source: UNH Scholars Repository https://scholars.unh.edu/court/61/ (PDF article=1060; curl gets 403, the Wayback copy
https://web.archive.org/web/20240606110014id_/https://scholars.unh.edu/cgi/viewcontent.cgi?article=1060&context=court downloads); the three
pages are saved as R/data/county_house_files/new_hampshire/leads/NH_manual_2009_house_2008_general_pp416-418_UNHpdfleaf434-436.pdf.
Found by a source-search agent (R/data/county_house_files/source_search/NH_1994_2008.md).

Values read by eye from 300 dpi renders of the page images (the PDF text layer was used only to locate rows: it drops or garbles some cells, e.g.
Gilford 2,299 -> '239', Kingston 1,526 -> '1326', Hudson 5,559 -> '5459', the Somersworth wards). A dash in the book is 0.

The tables are by town/ward (alphabetical, not grouped by county); city wards roll up to the city and towns are summed to the 10 counties with
R/data/raw_house_county_open_states/new_hampshire/nh_town_county_crosswalk.csv (same key normalisation as 01bp/01iq) plus the unincorporated
places in EXTRA. Places listed under 'No votes cast' in the book are omitted. Scatter/write-ins are not printed (FEC: 198 in CD1, 229 in CD2).

Checks (asserted): every candidate column sums to the printed 'Totals' row; every candidate total equals the FEC 2008 figure
(R/output/fec_congress_rows.rds: Shea-Porter 176,435, Bradley 156,338, Kingsbury 8,100; Hodes 188,332, Horn 138,222, Lapointe 7,121);
every town maps to a county; all 10 counties present. The panel had no New Hampshire 2008 House rows before this build.

Output: R/data/county_house_files/new_hampshire/new_hampshire_house_county_2008.csv (year,district,county,candidate,party_code,votes)
"""
import csv, os, re
from collections import defaultdict

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.join(HERE, "..", "data")
CW = os.path.join(ROOT, "raw_house_county_open_states/new_hampshire/nh_town_county_crosswalk.csv")
OUT = os.path.join(ROOT, "county_house_files/new_hampshire")

EXTRA = {"GREENSGRANT": "COOS", "PINKHAMSGT": "COOS", "PINKHAMSGRANT": "COOS", "WENTWORTHSLOC": "COOS", "WENTWORTHSLOCATION": "COOS",
         "HALESLOC": "CARROLL", "HALESLOCATION": "CARROLL", "HARTSLOC": "CARROLL", "HARTSLOCATION": "CARROLL", "WATERVILLEVAL": "GRAFTON",
         "WATERVILLEVALLEY": "GRAFTON"}

def key(x):
    x = x.upper().replace("*", "")
    x = re.sub(r"\s+(WARD|WD|W)\s*[0-9A-H]+\s*$", "", x)   # 'Dover Ward 1' -> DOVER
    return re.sub(r"[^A-Z0-9&]", "", x)

TOWN_COUNTY = {key(r["town"]): r["county"].upper() for r in csv.DictReader(open(CW))}
TOWN_COUNTY.update(EXTRA)

YEAR = 2008
RACES = {  # district: (candidates [(name, party code)], printed totals, FEC totals, block)
    1: ([("Carol Shea-Porter", "D"), ("Jeb Bradley", "R"), ("Robert Kingsbury", "I")], [176435, 156338, 8100], [176435, 156338, 8100], "D08_1"),
    2: ([("Paul Hodes", "D"), ("Jennifer Horn", "R"), ("Chester L. Lapointe II", "I")], [188332, 138222, 7121], [188332, 138222, 7121], "D08_2"),
}
DATA = {
    "D08_1": """
Albany|244|163|15
Alton|1252|1803|58
Auburn|1226|1756|64
Barnstead|1188|1117|46
Barrington|2537|1955|118
Bartlett|1002|754|0
Bedford|4619|7300|268
Belmont|1648|1625|78
Brentwood|996|1139|44
Brookfield|192|258|6
Candia|1043|1337|64
Center Harbor|337|365|17
Chatham|88|114|5
Chester|1099|1464|66
Conway|2959|1895|166
Danville|998|1120|83
Deerfield|1128|1335|79
Derry|7184|7271|422
Dover Ward 1|1655|670|59
Dover Ward 2|1540|711|54
Dover Ward 3|1713|1270|51
Dover Ward 4|1677|1126|44
Dover Ward 5|1473|862|52
Dover Ward 6|1525|1051|61
Durham|4715|1866|153
East Kingston|627|724|34
Eaton|171|109|0
Effingham|409|376|33
Epping|1705|1447|78
Exeter|4910|3260|163
Farmington|1567|1266|102
Freedom|434|449|14
Fremont|959|1176|74
Gilford|2073|2299|62
Gilmanton|1041|1005|38
Goffstown|3987|4528|202
Greenland|1221|981|25
Hale's Location|36|87|1
Hampstead|2075|2746|120
Hampton|4945|4184|173
Hampton Falls|555|833|35
Hart's Location|13|16|0
Hooksett|3174|3594|124
Jackson|410|250|13
Kensington|661|621|43
Kingston|1526|1604|97
Laconia Ward 1|717|791|19
Laconia Ward 2|684|495|34
Laconia Ward 3|615|528|20
Laconia Ward 4|655|499|30
Laconia Ward 5|612|451|20
Laconia Ward 6|733|803|21
Lee|1591|843|52
Londonderry|5751|6466|273
Madbury|640|429|0
Madison|815|544|31
Manchester Ward 1|2751|2459|88
Manchester Ward 2|2516|1956|94
Manchester Ward 3|1809|996|81
Manchester Ward 4|1837|1214|122
Manchester Ward 5|1632|925|93
Manchester Ward 6|2433|2183|92
Manchester Ward 7|2004|1490|77
Manchester Ward 8|2368|2264|97
Manchester Ward 9|2210|1611|90
Manchester Ward 10|2144|1655|106
Manchester Ward 11|1645|1060|71
Manchester Ward 12|2122|1862|111
Meredith|1797|1857|100
Merrimack|6889|6991|443
Middleton|427|375|31
Milton|1168|1000|59
Moultonborough|1243|1649|75
New Castle|368|385|8
New Durham|687|801|35
New Hampton|590|607|32
Newfields|520|491|20
Newington|251|282|10
Newmarket|3080|1776|134
Newton|1166|1051|89
North Hampton|1401|1357|52
Northwood|1164|1103|72
Nottingham|1364|1222|75
Ossipee|962|1090|84
Plaistow|1754|1989|123
Portsmouth Ward 1|1487|555|41
Portsmouth Ward 2|2110|710|63
Portsmouth Ward 3|927|498|23
Portsmouth Ward 4|1945|1109|61
Portsmouth Ward 5|1972|828|57
Raymond|2367|2415|172
Rochester Ward 1|1469|1170|48
Rochester Ward 2|1225|1062|45
Rochester Ward 3|1341|1132|43
Rochester Ward 4|1237|894|35
Rochester Ward 5|1334|1046|35
Rochester Ward 6|1187|728|34
Rollinsford|885|596|30
Rye|1896|1728|69
Sandown|1415|1614|101
Sandwich|553|406|21
Seabrook|1960|1867|161
Somersworth Ward 1|785|475|26
Somersworth Ward 2|622|371|27
Somersworth Ward 3|630|378|14
Somersworth Ward 4|724|394|26
Somersworth Ward 5|495|250|16
South Hampton|251|233|18
Strafford|1251|1059|33
Stratham|2322|2151|69
Tamworth|872|641|54
Tuftonboro|645|912|37
Wakefield|1141|1369|95
Wolfeboro|1735|2315|78
""",
    "D08_2": """
Acworth|339|179|9
Alexandria|443|367|27
Allenstown|1182|830|52
Alstead|624|362|5
Amherst|3402|3493|147
Andover|761|541|11
Antrim|825|585|45
Ashland|578|470|28
Atkinson|1832|2177|85
Bath|231|235|16
Bennington|379|332|22
Benton|81|81|8
Berlin Ward 1|701|233|28
Berlin Ward 2|739|255|22
Berlin Ward 3|910|348|21
Berlin Ward 4|702|202|18
Bethlehem|825|474|21
Boscawen|903|751|41
Bow|2479|2109|57
Bradford|551|398|19
Bridgewater|327|333|8
Bristol|852|695|46
Brookline|1131|1484|91
Cambridge|2|1|0
Campton|1017|678|52
Canaan|1091|620|43
Canterbury|925|520|29
Carroll|262|194|9
Charlestown|1564|786|125
Chesterfield|1267|795|45
Chichester|731|706|14
Claremont Ward 1|1063|495|31
Claremont Ward 2|1293|780|21
Claremont Ward 3|1157|601|29
Clarksville|73|71|5
Colebrook|514|408|29
Columbia|164|142|8
Concord Ward 1|1478|816|33
Concord Ward 2|1265|696|35
Concord Ward 3|1077|472|15
Concord Ward 4|1486|545|38
Concord Ward 5|1756|823|36
Concord Ward 6|1078|458|38
Concord Ward 7|1544|733|24
Concord Ward 8|1320|702|27
Concord Ward 9|1046|592|35
Concord Ward 10|1800|1121|23
Cornish|668|380|16
Croydon|187|187|4
Dalton|240|226|16
Danbury|324|270|12
Deering|530|437|33
Dixville|12|7|1
Dorchester|89|101|7
Dublin|574|411|16
Dummer|81|87|2
Dunbarton|768|797|23
Easton|111|66|6
Ellsworth|24|27|2
Enfield|1508|791|34
Epsom|1229|1116|52
Errol|72|88|9
Fitzwilliam|692|503|48
Francestown|519|481|22
Franconia|480|233|16
Franklin Ward 1|735|545|10
Franklin Ward 2|573|394|15
Franklin Ward 3|779|520|14
Gilsum|233|178|11
Gorham|1027|414|21
Goshen|233|167|11
Grafton|350|286|32
Grantham|1166|739|18
Greenfield|480|372|34
Green's Grant|1|0|0
Greenville|510|374|25
Groton|150|132|7
Hancock|714|438|21
Hanover|5438|1382|92
Harrisville|496|181|15
Haverhill|946|903|54
Hebron|171|221|5
Henniker|1437|892|76
Hill|297|267|4
Hillsborough|1561|1116|66
Hinsdale|1222|460|42
Holderness|721|500|26
Hollis|2228|2469|110
Hopkinton|2268|1429|47
Hudson|5559|5535|238
Jaffrey|1570|1147|49
Jefferson|274|304|6
Keene Ward 1|1911|490|90
Keene Ward 2|1797|567|96
Keene Ward 3|1582|663|51
Keene Ward 4|1626|782|55
Keene Ward 5|1693|803|57
Lancaster|874|695|37
Landaff|129|89|6
Langdon|221|153|8
Lebanon Ward 1|1443|691|29
Lebanon Ward 2|1368|562|33
Lebanon Ward 3|1587|711|31
Lempster|308|266|15
Lincoln|432|301|18
Lisbon|410|310|11
Litchfield|1912|2261|79
Littleton|1558|1098|43
Loudon|1419|1262|61
Lyman|152|134|12
Lyme|837|277|11
Lyndeborough|487|455|39
Marlborough|768|340|23
Marlow|265|157|13
Mason|338|413|37
Milan|417|281|15
Milford|3761|3510|142
Millsfield|3|11|1
Monroe|230|232|13
Mont Vernon|706|691|30
Nashua Ward 1|2807|2287|119
Nashua Ward 2|2327|1962|110
Nashua Ward 3|2347|1698|121
Nashua Ward 4|1789|880|95
Nashua Ward 5|2580|2239|126
Nashua Ward 6|2232|1453|110
Nashua Ward 7|2018|1473|113
Nashua Ward 8|2275|1790|127
Nashua Ward 9|2631|2174|112
Nelson|296|144|10
New Boston|1421|1496|65
New Ipswich|898|1502|44
New London|1510|1243|35
Newbury|702|606|13
Newport|1689|1104|52
Northfield|1187|970|47
Northumberland|640|315|21
Orange|86|60|6
Orford|452|233|16
Pelham|2902|3246|162
Pembroke|2030|1502|48
Peterborough|2360|1326|56
Piermont|246|178|11
Pinkham's Grant|4|1|0
Pittsburg|209|276|0
Pittsfield|878|881|20
Plainfield|986|404|17
Plymouth|2345|985|127
Randolph|156|77|3
Richmond|345|280|24
Rindge|1484|1733|70
Roxbury|99|45|2
Rumney|436|413|0
Salem|6732|6766|250
Salisbury|372|383|13
Sanbornton|954|767|29
Sharon|148|95|1
Shelburne|146|109|2
Springfield|389|329|19
Stark|151|115|5
Stewartstown|176|163|11
Stoddard|373|302|11
Stratford|182|96|9
Sugar Hill|242|150|9
Sullivan|224|146|10
Sunapee|1040|926|30
Surry|274|208|11
Sutton|656|472|20
Swanzey|2102|1392|90
Temple|450|367|15
Thornton|739|558|30
Tilton|970|794|27
Troy|529|397|39
Unity|417|307|14
Walpole|1307|803|33
Warner|993|626|27
Warren|235|196|11
Washington|293|283|11
Waterville Valley|105|89|5
Weare|2164|2143|140
Webster|542|457|29
Wentworth|260|216|25
Wentworth's Location|10|12|1
Westmoreland|639|395|18
Whitefield|594|438|25
Wilmot|498|302|12
Wilton|1203|931|41
Winchester|1146|581|82
Windham|2969|4149|123
Windsor|65|50|4
Woodstock|425|266|12
""",
}

def rows(block):
    out = []
    for line in DATA[block].strip().splitlines():
        town, *vals = line.split("|")
        out.append((town, [int(v) for v in vals]))
    return out

agg = defaultdict(int)
for d, (cands, printed, fec, block) in RACES.items():
    rs = rows(block)
    assert all(len(v) == len(cands) for _, v in rs), d
    for j in range(len(cands)):
        s = sum(v[j] for _, v in rs)
        assert s == printed[j], ("printed total", d, cands[j][0], s, printed[j])
        assert s == fec[j], ("FEC", d, cands[j][0], s, fec[j])
    for town, v in rs:
        county = TOWN_COUNTY.get(key(town))
        assert county, ("no county for town", town)
        for (name, party), x in zip(cands, v):
            agg[(d, county, name, party)] += x
counties = {c for (_, c, _, _) in agg}
assert len(counties) == 10, sorted(counties)
with open(os.path.join(OUT, f"new_hampshire_house_county_{YEAR}.csv"), "w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["year", "district", "county", "candidate", "party_code", "votes"])
    for (d, c, name, party), x in sorted(agg.items()):
        w.writerow([YEAR, d, c, name, party, x])
print(YEAR, "districts", sorted(RACES), "counties", len(counties), "rows", len(agg), "- all checks pass")
