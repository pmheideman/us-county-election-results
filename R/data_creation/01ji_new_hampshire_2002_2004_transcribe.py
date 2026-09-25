"""New Hampshire U.S. House 2002 and 2004, county level, from the 'State of New Hampshire Manual for
the General Court' (NH Department of State):
  2002: 2003 Manual (No. 58), 'U.S. House: First District' (printed p. 313) and 'Second District'
        (pp. 314-317); Internet Archive manualforgeneral58newh, leaves 331-335.
  2004: 2005 Manual (No. 59), 'U.S. House: First District' (p. 395) and 'Second District'
        (pp. 396-399); Internet Archive manualforgeneral59newh, leaves 413-416 plus leaf n416 for
        p. 399 (Temple-Woodstock and the District 2 Totals, missing from the downloaded extract).
Local copies: R/data/county_house_files/new_hampshire/leads/ (source search:
R/data/county_house_files/source_search/NH_1994_2008.md). Values read by eye from 500 dpi renders.

The tables are by town/ward (alphabetical, no county column); towns are summed to the 10 counties
with R/data/raw_house_county_open_states/new_hampshire/nh_town_county_crosswalk.csv (key
normalisation as 01bp/01iq; city wards roll up to the city) plus the unincorporated places in EXTRA.
Places listed under 'No votes cast' are omitted; a dash is 0. Scattering is not printed by town.

Checks (asserted): every candidate column sums to the printed 'Totals' row; every candidate total
equals the Clerk of the House statistics (R/data/clerk_house_stats/2002Stat.htm, 2004Stat.htm);
every town maps to a county; all 10 counties appear.

Output: R/data/county_house_files/new_hampshire/new_hampshire_house_county_<year>.csv
        (year,district,county,candidate,party_code,votes)
"""
import csv, os, re
from collections import defaultdict

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.join(HERE, "..", "data")
CW = os.path.join(ROOT, "raw_house_county_open_states/new_hampshire/nh_town_county_crosswalk.csv")
OUT = os.path.join(ROOT, "county_house_files/new_hampshire")

EXTRA = {"GREENSGRANT": "COOS", "PINKHAMSGT": "COOS", "PINKHAMSGRANT": "COOS", "WENTWORTHSLOC": "COOS",
         "WENTWORTHSLOCATION": "COOS", "HALESLOC": "CARROLL", "HALESLOCATION": "CARROLL", "HARTSLOC": "CARROLL",
         "HARTSLOCATION": "CARROLL", "WATERVILLEVAL": "GRAFTON", "WATERVILLEVALLEY": "GRAFTON"}

def key(x):
    x = x.upper().replace("*", "")
    x = re.sub(r"\s+(WARD|WD|W)\s*[0-9A-H]+\s*$", "", x)
    return re.sub(r"[^A-Z0-9&]", "", x)

TOWN_COUNTY = {key(r["town"]): r["county"].upper() for r in csv.DictReader(open(CW))}
TOWN_COUNTY.update(EXTRA)

# year -> district -> (candidates [(name, party code)], printed totals, Clerk totals, data block)
RACES = {
    2002: {
        1: ([("Jeb Bradley", "R"), ("Martha Fuller Clark", "D"), ("Dan Belforti", "I")], [128993, 85426, 7387], [128993, 85426, 7387], "D02_1"),
        2: ([("Charles Bass", "R"), ("Katrina Swett", "D"), ("Rosalie T. Babiarz", "I")], [125804, 90479, 5051], [125804, 90479, 5051], "D02_2"),
    },
    2004: {
        1: ([("Jeb Bradley", "R"), ("Justin Nadeau", "D")], [204836, 118226], [204836, 118226], "D04_1"),
        2: ([("Charles F. Bass", "R"), ("Paul W. Hodes", "D"), ("Richard B. Kahn", "I")], [191188, 125280, 11311], [191188, 125280, 11311], "D04_2"),
    },
}
DATA = {
    "D02_1": """
Albany|116|112|8
Alton|1442|445|66
Auburn|1360|561|62
Barrington|1472|1233|136
Barnstead|834|613|63
Bartlett|733|555|45
Bedford|6383|2256|165
Belmont|1354|669|74
Brentwood|750|474|42
Brookfield|231|81|6
Candia|1232|570|58
Center Harbor|329|164|21
Chatham|69|37|5
Chester|1096|451|54
Conway|1598|1329|68
Danville|829|448|71
Deerfield|1030|601|72
Derry|5101|2966|269
Dover Ward 1|599|761|61
Dover Ward 2|587|810|63
Dover Ward 3|1093|881|66
Dover Ward 4|914|883|62
Dover Ward 5|645|629|37
Dover Ward 6|886|735|44
Durham|1341|2364|81
East Kingston|485|259|30
Eaton|98|92|3
Effingham|264|162|23
Epping|1138|713|86
Exeter|2805|2645|129
Farmington|979|628|100
Freedom|397|262|12
Fremont|761|324|48
Gilford|2206|950|62
Gilmanton|783|483|47
Goffstown|3705|1998|191
Greenland|835|604|54
Hale's Location|61|9|1
Hampstead|2112|951|109
Hampton|3647|2614|216
Hampton Falls|692|289|22
Hart's Location|12|9|0
Hooksett|2811|1342|150
Jackson|289|256|10
Kensington|491|366|35
Kingston|1253|742|93
Laconia Ward 1|854|383|24
Laconia Ward 2|528|306|22
Laconia Ward 3|471|333|13
Laconia Ward 4|557|320|30
Laconia Ward 5|392|283|30
Laconia Ward 6|583|276|26
Lee|783|942|54
Londonderry|4893|2535|228
Madbury|380|354|24
Madison|461|361|23
Manchester Ward 1|2350|1653|90
Manchester Ward 2|1774|1234|81
Manchester Ward 3|765|800|79
Manchester Ward 4|1083|881|102
Manchester Ward 5|866|828|57
Manchester Ward 6|1729|1113|106
Manchester Ward 7|1353|1033|108
Manchester Ward 8|1917|1149|101
Manchester Ward 9|1506|1119|97
Manchester Ward 10|1571|1136|109
Manchester Ward 11|1018|716|88
Manchester Ward 12|1454|847|75
Meredith|1679|780|58
Merrimack|5608|3491|358
Middleton|240|178|25
Milton|733|493|68
Moultonborough|1547|580|90
New Castle|386|267|18
New Durham|589|285|40
New Hampton|501|271|31
Newfields|428|270|26
Newington|281|149|10
Newmarket|1450|1542|100
Newton|739|539|86
North Hampton|1231|832|53
Northwood|755|535|64
Nottingham|923|650|67
Ossipee|861|535|33
Plaistow|1515|805|105
Portsmouth Ward 1|552|773|65
Portsmouth Ward 2|649|1303|57
Portsmouth Ward 3|429|534|27
Portsmouth Ward 4|1101|1018|65
Portsmouth Ward 5|707|1172|52
Raymond|1670|905|151
Rye|1677|1125|73
Rochester Ward 1|888|664|72
Rochester Ward 2|955|607|55
Rochester Ward 3|891|691|57
Rochester Ward 4|788|597|52
Rochester Ward 5|817|592|54
Rochester Ward 6|696|544|45
Rollinsford|531|440|24
Sandown|1058|509|73
Sandwich|397|357|20
Seabrook|1356|895|95
Somersworth Ward 1|437|373|32
Somersworth Ward 2|304|297|24
Somersworth Ward 3|356|321|20
Somersworth Ward 4|381|368|28
Somersworth Ward 5|211|271|18
South Hampton|211|151|19
Strafford|841|611|56
Stratham|1795|1135|79
Tamworth|508|479|26
Tuftonboro|863|303|27
Wakefield|1028|477|50
Wolfeboro|2294|784|52
""",
    "D02_2": """
Acworth|204|164|14
Alexandria|350|171|22
Allenstown|784|585|36
Alstead|357|375|16
Amherst|3315|1624|88
Andover|469|386|27
Antrim|567|344|21
Ashland|443|218|0
Atkinson|1741|1004|74
Bath|223|103|6
Bennington|327|157|14
Benton|73|25|2
Berlin Ward 1|410|453|10
Berlin Ward 2|453|437|16
Berlin Ward 3|513|537|18
Berlin Ward 4|291|447|12
Bethlehem|495|344|12
Boscawen|685|427|41
Bow|2094|1411|38
Bradford|313|231|15
Bridgewater|293|152|5
Bristol|744|303|33
Brookline|1061|606|55
Cambridge|1|0|0
Campton|598|337|29
Canaan|562|447|41
Canterbury|583|486|27
Carroll|215|111|5
Charlestown|740|618|34
Chesterfield|694|624|24
Chichester|663|330|26
Claremont Ward 1|553|558|20
Claremont Ward 2|808|716|23
Claremont Ward 3|632|583|24
Clarksville|66|20|2
Colebrook|463|145|19
Columbia|134|51|1
Concord Ward 1|817|711|29
Concord Ward 2|600|588|30
Concord Ward 3|592|528|21
Concord Ward 4|692|871|27
Concord Ward 5|845|841|30
Concord Ward 6|503|542|39
Concord Ward 7|922|883|33
Concord Ward 8|651|638|25
Concord Ward 9|582|521|24
Concord Ward 10|1240|956|27
Cornish|372|356|16
Croydon|188|83|4
Dalton|206|107|5
Danbury|259|114|18
Deering|397|243|14
Dixville|17|1|0
Dorchester|84|38|3
Dublin|464|341|9
Dummer|86|40|4
Dunbarton|758|361|13
Easton|76|67|4
Ellsworth|24|15|1
Enfield|809|645|44
Epsom|1038|561|38
Errol|94|27|1
Fitzwilliam|416|355|19
Francestown|481|290|28
Franconia|314|207|8
Franklin Ward 1|505|337|16
Franklin Ward 2|406|277|11
Franklin Ward 3|484|370|19
Gilsum|143|115|7
Gorham|534|622|17
Goshen|171|124|5
Grafton|243|148|40
Grantham|770|501|17
Greenfield|314|224|23
Greenville|266|213|19
Green's Grant|0|2|0
Groton|131|41|4
Hancock|566|366|13
Hanover|1406|2791|63
Harrisville|208|293|13
Haverhill|858|358|32
Hebron|233|71|3
Henniker|995|731|47
Hill|220|134|5
Hillsborough|1074|578|48
Hinsdale|480|518|19
Holderness|599|334|24
Hollis|2098|1184|77
Hopkinton|1692|1130|58
Hudson|3958|2711|150
Jaffrey|1056|783|45
Jefferson|282|104|7
Keene Ward 1|380|648|35
Keene Ward 2|587|886|50
Keene Ward 3|716|759|36
Keene Ward 4|814|837|35
Keene Ward 5|919|879|33
Lancaster|733|389|24
Landaff|88|47|1
Langdon|146|103|6
Lebanon Ward 1|719|768|32
Lebanon Ward 2|613|740|29
Lebanon Ward 3|679|660|31
Lempster|202|116|8
Lincoln|366|138|11
Lisbon|296|113|11
Litchfield|1630|777|50
Littleton|1372|564|17
Loudon|1185|591|38
Lyman|107|61|2
Lyme|350|482|8
Lyndeborough|438|215|20
Marlborough|350|417|19
Marlow|153|165|13
Mason|309|193|10
Milan|256|180|4
Milford|2896|1677|106
Millsfield|10|4|0
Monroe|235|85|14
Mont Vernon|604|350|19
Nashua Ward 1|1994|1638|84
Nashua Ward 2|1673|1146|55
Nashua Ward 3|1491|1286|76
Nashua Ward 4|704|837|41
Nashua Ward 5|1775|1467|79
Nashua Ward 6|1204|1248|73
Nashua Ward 7|896|836|43
Nashua Ward 8|1720|1404|69
Nashua Ward 9|1765|1325|68
Nelson|129|154|6
New Boston|1250|618|45
New Ipswich|1126|436|41
New London|1473|640|25
Newbury|567|323|24
Newport|1101|684|41
Northfield|838|469|40
Northumberland|341|294|9
Orange|56|52|4
Orford|253|178|10
Pelham|2136|1369|96
Pembroke|1531|945|45
Peterborough|1509|1175|46
Piermont|191|104|4
Pinkham's Grant|0|2|0
Pittsburg|248|65|3
Pittsfield|852|469|33
Plainfield|449|515|23
Plymouth|868|718|85
Randolph|96|113|3
Richmond|186|199|0
Rindge|1031|559|43
Roxbury|48|42|5
Rumney|423|175|14
Salem|4977|3656|220
Salisbury|321|181|13
Sanbornton|741|432|27
Sharon|99|67|3
Shelburne|112|63|5
Springfield|277|166|13
Stark|98|63|2
Stewartstown|142|50|4
Stoddard|283|163|8
Stratford|84|92|7
Sugar Hill|243|135|3
Sullivan|133|109|6
Sunapee|1052|477|32
Surry|168|143|13
Sutton|469|333|25
Swanzey|1147|985|39
Temple|349|206|18
Thornton|486|277|22
Tilton|643|403|30
Troy|292|317|14
Unity|236|178|13
Walpole|761|709|37
Warner|684|467|42
Warren|225|79|20
Washington|250|138|11
Waterville Valley|84|61|3
Weare|1779|859|86
Webster|425|262|14
Wentworth|191|59|7
Wentworth's Location|3|7|0
Westmoreland|379|342|22
Whitefield|463|249|27
Wilmot|328|226|18
Wilton|875|589|61
Winchester|547|510|35
Windham|2927|1389|103
Windsor|44|24|1
Woodstock|274|164|6
""",
    "D04_1": """
Albany|214|165
Alton|2301|675
Auburn|2075|765
Barrington|2542|1648
Barnstead|1498|741
Bartlett|1118|752
Bedford|7884|2670
Belmont|2344|898
Brentwood|1305|664
Brookfield|325|115
Candia|1724|671
Center Harbor|485|202
Chatham|125|56
Chester|1839|645
Conway|2876|2017
Danville|1465|642
Deerfield|1592|714
Derry|9147|5026
Dover Ward 1|984|1303
Dover Ward 2|1009|1276
Dover Ward 3|1721|1206
Dover Ward 4|1388|1292
Dover Ward 5|1164|1108
Dover Ward 6|1444|1051
Durham|2249|3364
East Kingston|811|329
Eaton|147|120
Effingham|486|219
Epping|1932|1063
Exeter|4408|3449
Farmington|1796|972
Freedom|645|293
Fremont|1366|568
Gilford|3184|1215
Gilmanton|1364|600
Goffstown|5834|2617
Greenland|1229|777
Hale's Location|90|16
Hampstead|3280|1411
Hampton|5449|3605
Hampton Falls|967|364
Hart's Location|20|9
Hooksett|4424|1820
Jackson|383|321
Kensington|766|445
Kingston|2117|979
Laconia Ward 1|1121|420
Laconia Ward 2|773|380
Laconia Ward 3|757|407
Laconia Ward 4|803|433
Laconia Ward 5|682|364
Laconia Ward 6|1093|425
Lee|1176|1162
Londonderry|8008|3630
Madbury|530|467
Madison|806|513
Manchester Ward 1|3298|2044
Manchester Ward 2|2670|1719
Manchester Ward 3|1367|1366
Manchester Ward 4|1864|1376
Manchester Ward 5|1461|1218
Manchester Ward 6|2625|1450
Manchester Ward 7|2154|1348
Manchester Ward 8|3033|1429
Manchester Ward 9|2324|1523
Manchester Ward 10|2385|1460
Manchester Ward 11|1752|1100
Manchester Ward 12|2475|1424
Meredith|2424|1026
Merrimack|9307|4680
Middleton|511|250
Milton|1299|705
Moultonborough|2184|706
New Castle|498|270
New Durham|960|407
New Hampton|803|390
Newfields|583|346
Newington|362|176
Newmarket|2339|2224
Newton|1326|833
North Hampton|1806|1007
Northwood|1329|763
Nottingham|1509|775
Ossipee|1472|544
Plaistow|2629|1279
Portsmouth Ward 1|876|1149
Portsmouth Ward 2|986|1818
Portsmouth Ward 3|717|745
Portsmouth Ward 4|1562|1562
Portsmouth Ward 5|1107|1621
Raymond|3111|1470
Rye|2136|1495
Rochester Ward 1|1555|882
Rochester Ward 2|1461|701
Rochester Ward 3|1482|760
Rochester Ward 4|1322|845
Rochester Ward 5|1365|765
Rochester Ward 6|1077|743
Rollinsford|813|623
Sandown|1855|838
Sandwich|533|458
Seabrook|2522|1513
Somersworth Ward 1|737|537
Somersworth Ward 2|550|417
Somersworth Ward 3|554|477
Somersworth Ward 4|663|548
Somersworth Ward 5|457|371
South Hampton|299|218
Strafford|1384|788
Stratham|2583|1437
Tamworth|917|561
Tuftonboro|1231|364
Wakefield|1783|686
Wolfeboro|3184|937
""",
    "D04_2": """
Acworth|263|219|27
Alexandria|554|225|34
Allenstown|1295|766|72
Alstead|544|454|41
Amherst|4324|2319|255
Andover|727|555|34
Antrim|804|523|55
Ashland|677|301|43
Atkinson|2692|1236|148
Bath|307|137|19
Bennington|443|254|25
Benton|126|33|10
Berlin Ward 1|628|477|30
Berlin Ward 2|701|451|37
Berlin Ward 3|820|516|26
Berlin Ward 4|566|462|23
Bethlehem|759|548|46
Boscawen|965|611|42
Bow|2765|1738|110
Bradford|541|393|29
Bridgewater|418|186|15
Bristol|1036|472|57
Brookline|1668|735|107
Cambridge|1|1|0
Campton|993|559|53
Canaan|902|670|67
Canterbury|767|703|42
Carroll|255|157|21
Charlestown|1276|897|79
Chesterfield|1149|871|49
Chichester|847|484|54
Claremont Ward 1|929|691|53
Claremont Ward 2|1231|874|65
Claremont Ward 3|1079|667|61
Clarksville|116|26|3
Colebrook|764|204|30
Columbia|282|55|6
Concord Ward 1|1266|987|61
Concord Ward 2|957|794|51
Concord Ward 3|725|776|22
Concord Ward 4|880|1229|78
Concord Ward 5|910|1889|52
Concord Ward 6|739|869|53
Concord Ward 7|1009|1268|42
Concord Ward 8|998|864|63
Concord Ward 9|831|785|45
Concord Ward 10|1562|1336|51
Cornish|543|451|24
Croydon|268|108|15
Dalton|343|135|14
Danbury|388|239|28
Deering|594|325|41
Dixville|22|3|1
Dorchester|119|57|5
Dublin|593|445|37
Dummer|136|43|7
Dunbarton|937|499|33
Easton|93|86|6
Ellsworth|44|11|4
Enfield|1278|930|73
Epsom|1459|772|55
Errol|148|31|7
Fitzwilliam|713|498|54
Francestown|594|395|32
Franconia|414|284|20
Franklin Ward 1|821|443|38
Franklin Ward 2|611|388|31
Franklin Ward 3|786|488|35
Gilsum|244|164|18
Gorham|953|614|34
Goshen|253|138|18
Grafton|377|201|23
Grantham|940|762|37
Greenfield|471|311|39
Greenville|498|355|51
Green's Grant|1|2|0
Groton|188|92|15
Hancock|642|516|34
Hanover|1887|4045|263
Harrisville|322|366|21
Haverhill|1418|509|81
Hebron|291|115|8
Henniker|1276|1039|81
Hill|333|179|14
Hillsborough|1629|906|88
Hinsdale|892|803|75
Holderness|785|425|37
Hollis|2970|1484|184
Hopkinton|1977|1669|74
Hudson|6986|3691|450
Jaffrey|1776|1002|98
Jefferson|426|136|22
Keene Ward 1|843|1127|129
Keene Ward 2|1003|1301|95
Keene Ward 3|1088|1131|89
Keene Ward 4|1278|1142|77
Keene Ward 5|1297|1195|98
Lancaster|1141|480|52
Landaff|161|63|3
Langdon|215|111|13
Lebanon Ward 1|1001|1080|74
Lebanon Ward 2|957|1042|72
Lebanon Ward 3|978|897|74
Lempster|303|165|13
Lincoln|557|227|30
Lisbon|502|184|20
Litchfield|2740|1073|154
Littleton|2061|817|81
Loudon|1721|843|74
Lyman|208|75|16
Lyme|459|660|24
Lyndeborough|583|341|37
Marlborough|561|552|26
Marlow|230|194|16
Mason|473|269|0
Milan|468|232|21
Milford|4404|2380|252
Millsfield|15|0|0
Monroe|394|105|20
Mont Vernon|807|454|52
Nashua Ward 1|2977|2029|203
Nashua Ward 2|2615|1608|171
Nashua Ward 3|2339|1838|196
Nashua Ward 4|1295|1440|157
Nashua Ward 5|2816|1838|181
Nashua Ward 6|2106|1743|130
Nashua Ward 7|2042|1415|140
Nashua Ward 8|2181|1653|174
Nashua Ward 9|2761|1818|162
Nelson|200|208|7
New Boston|1809|818|100
New Ipswich|1678|568|99
New London|1756|1013|69
Newbury|777|433|37
Newport|1771|929|112
Northfield|1382|726|85
Northumberland|658|333|35
Orange|81|64|6
Orford|374|288|21
Pelham|3906|1867|252
Pembroke|2165|1239|84
Peterborough|2002|1556|120
Piermont|251|155|20
Pinkham's Grant|0|16|1
Pittsburg|409|88|13
Pittsfield|1204|581|57
Plainfield|641|692|36
Plymouth|1585|1525|209
Randolph|123|131|12
Richmond|319|243|18
Rindge|1793|1017|3
Roxbury|71|74|7
Rumney|636|278|31
Salem|8507|4467|534
Salisbury|455|264|17
Sanbornton|1035|606|38
Sharon|128|98|6
Shelburne|198|79|3
Springfield|429|224|24
Stark|192|85|7
Stewartstown|292|92|25
Stoddard|362|239|16
Stratford|174|108|8
Sugar Hill|204|171|11
Sullivan|220|143|11
Sunapee|1329|642|53
Surry|277|159|20
Sutton|628|457|42
Swanzey|1954|1341|92
Temple|476|304|19
Thornton|778|246|48
Tilton|1071|607|61
Troy|591|389|43
Unity|456|233|32
Walpole|1173|897|63
Warner|870|692|52
Warren|351|130|16
Washington|378|179|29
Waterville Valley|110|75|6
Weare|2849|1234|165
Webster|629|351|25
Wentworth|331|127|16
Wentworth's Location|14|9|3
Westmoreland|536|426|24
Whitefield|743|352|40
Wilmot|432|337|20
Wilton|1242|865|89
Winchester|966|745|114
Windham|4591|1853|229
Windsor|73|43|8
Woodstock|443|225|31
""",
}

def rows(block):
    out = []
    for line in DATA[block].strip().splitlines():
        town, *vals = line.split("|")
        out.append((town, [int(v) for v in vals]))
    return out

for year, districts in RACES.items():
    agg = defaultdict(int)
    for d, (cands, printed, clerk, block) in districts.items():
        rs = rows(block)
        assert all(len(v) == len(cands) for _, v in rs), (year, d)
        for j in range(len(cands)):
            s = sum(v[j] for _, v in rs)
            assert s == printed[j], ("printed total", year, d, cands[j][0], s, printed[j])
            assert s == clerk[j], ("Clerk", year, d, cands[j][0], s, clerk[j])
        for town, v in rs:
            county = TOWN_COUNTY.get(key(town))
            assert county, ("no county for town", town, key(town))
            for (name, party), x in zip(cands, v):
                agg[(d, county, name, party)] += x
    counties = {c for (_, c, _, _) in agg}
    assert len(counties) == 10, (year, sorted(counties))
    with open(os.path.join(OUT, f"new_hampshire_house_county_{year}.csv"), "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["year", "district", "county", "candidate", "party_code", "votes"])
        for (d, c, name, party), x in sorted(agg.items()):
            w.writerow([year, d, c, name, party, x])
    print(year, "districts", sorted(districts), "counties", len(counties), "rows", len(agg), "- all checks pass")
