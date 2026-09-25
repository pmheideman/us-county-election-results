"""New Hampshire U.S. House 1990 and 1992, county level, from the 'State of New Hampshire Manual
for the General Court' (NH Department of State) on Internet Archive SRI microfiche:
  1990: 1991 Manual, micro_IA40706939_0148, 'U.S. House: First District' (printed p. 242, scan 260)
        and 'Second District' (pp. 243-244, scans 261-262), General Election Nov. 6, 1990.
  1992: 1993 Manual, micro_IA40706946_0051, 'U.S. House: First District' (pp. 384-385, scans
        426-427) and 'Second District' (pp. 386-388, scans 428-430), General Election Nov. 3, 1992.
Local copies: R/data/county_house_files/sri/nh_manual/. Values read by eye from the page images.

The tables are by town/ward (alphabetical, not grouped by county); towns are summed to the 10
counties with R/data/raw_house_county_open_states/new_hampshire/nh_town_county_crosswalk.csv
(same key normalisation as 01bp) plus the unincorporated places listed in EXTRA. Places listed
under 'No votes cast' in the book are omitted. Scatter/write-ins are not printed in these tables.

Checks (asserted): every candidate column sums to the printed 'Totals' row; every candidate total
equals the FEC 'Federal Elections 90/92' figure. The 1990 District 2 Swett total prints with a
smudged last digit; the column sum (74,866) equals the FEC. For 1992 District 2 the Clerk of the
House lists Hatch 91,126 and Bingham 657; the Manual and the FEC both give 91,127 and 658.

1994 not built: the 1995 Manual (micro_IA40706953_0081) returns HTTP 500 for its metadata, images
and PDF on archive.org (2026-09-24), and the 1994 results book micro_IA40706953_0082 lacks the
District 1 page from Laconia Ward 4 to Somersworth Ward 3.

Output: R/data/county_house_files/sri/new_hampshire_house_county_<year>.csv (year,district,county,candidate,party_code,votes)
"""
import csv, os, re
from collections import defaultdict

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.join(HERE, "..", "data")
CW = os.path.join(ROOT, "raw_house_county_open_states/new_hampshire/nh_town_county_crosswalk.csv")
OUT = os.path.join(ROOT, "county_house_files/sri")

EXTRA = {"GREENSGRANT": "COOS", "PINKHAMSGT": "COOS", "WENTWORTHSLOC": "COOS", "HALESLOC": "CARROLL",
         "HARTSLOC": "CARROLL", "WATERVILLEVAL": "GRAFTON"}

def key(x):
    x = x.upper().replace("*", "")
    x = re.sub(r"\s+(WARD|WD|W)\s*[0-9A-H]+\s*$", "", x)   # 'Dover W1', 'Concord W A'
    return re.sub(r"[^A-Z0-9&]", "", x)

TOWN_COUNTY = {key(r["town"]): r["county"].upper() for r in csv.DictReader(open(CW))}
TOWN_COUNTY.update(EXTRA)

# year -> district -> (candidates [(name, party code)], printed totals, FEC totals, town rows)
RACES = {
    1990: {
        1: ([("William H. Zeliff, Jr.", "R"), ("Joseph F. Keefe", "D")], [81684, 66176], [81684, 66176], "D90_1"),
        2: ([("Chuck Douglas", "R"), ("Richard N. Swett", "D")], [67225, 74866], [67225, 74866], "D90_2"),
    },
    1992: {
        1: ([("William H. Zeliff, Jr.", "R"), ("Bob Preston", "D"), ("Knox Bickford", "I"), ("Richard P. Bosa", "I"),
             ("Linda Spitzfaden", "I")], [135936, 108578, 5633, 3537, 1997], [135936, 108578, 5633, 3537, 1997], "D92_1"),
        2: ([("Bill Hatch", "R"), ("Dick Swett", "D"), ("John A. Lewicke", "I"), ("James J. Bingham", "I")],
            [91127, 157328, 5977, 658], [91127, 157328, 5977, 658], "D92_2"),
    },
}
DATA = {
    "D90_1": """
Albany|64|78
Alton|807|339
Auburn|606|416
Barnstead|500|402
Barrington|732|674
Bartlett|444|387
Bedford|2934|1529
Belmont|697|528
Brentwood|399|263
Brookfield|143|79
Candia|692|382
Canterbury|266|420
Center Harbor|197|144
Chatham|50|24
Chester|555|207
Chichester|302|296
Conway|1142|1101
Danville|408|182
Deerfield|576|355
Derry|3308|1977
Dover W1|448|528
Dover W2|235|439
Dover W3|587|664
Dover W4|681|696
Dover W5|497|530
Dover W6|417|442
Durham|855|1283
East Kingston|236|147
Eaton|61|63
Effingham|182|108
Epping|696|457
Epsom|529|377
Exeter|1801|1697
Farmington|566|473
Freedom|276|153
Fremont|413|222
Gilford|1352|832
Gilmanton|434|379
Goffstown|2293|1559
Greenland|477|396
Hale's Loc.|2|0
Hampstead|1216|501
Hampton|2050|1965
Hampton Falls|442|217
Hart's Loc.|10|5
Hooksett|1466|980
Jackson|250|160
Kensington|247|261
Kingston|875|432
Laconia W1|612|352
Laconia W2|350|288
Laconia W3|462|415
Laconia W4|326|255
Laconia W5|262|309
Laconia W6|573|372
Lee|433|583
Londonderry|2960|1607
Loudon|518|419
Madbury|233|251
Madison|284|226
Manchester W1|1594|1465
Manchester W2|1330|1253
Manchester W3|723|804
Manchester W4|718|811
Manchester W5|592|693
Manchester W6|1273|1332
Manchester W7|830|879
Manchester W8|1394|1238
Manchester W9|1033|956
Manchester W10|1143|1092
Manchester W11|680|644
Manchester W12|1136|1013
Meredith|1006|523
Merrimack|3045|2106
Middleton|109|93
Milton|457|371
Moultonborough|951|340
New Castle|234|170
New Durham|291|197
Newfields|147|139
New Hampton|324|201
Newington|117|127
Newmarket|749|942
Newton|475|335
Northfield|436|429
North Hampton|797|625
Northwood|490|354
Nottingham|457|382
Ossipee|545|281
Pittsfield|455|322
Plaistow|900|483
Portsmouth W1|299|535
Portsmouth W2|460|940
Portsmouth W3|348|453
Portsmouth W4|695|796
Portsmouth W5|559|842
Raymond|1040|629
Rochester W1|570|612
Rochester W2|483|458
Rochester W3|751|762
Rochester W4|476|548
Rochester W5|527|561
Rollinsford|294|412
Rye|1070|891
Sanbornton|411|408
Sandown|574|322
Sandwich|322|303
Seabrook|973|704
Somersworth W1|248|322
Somersworth W2|221|276
Somersworth W3|231|295
Somersworth W4|212|325
Somersworth W5|154|299
South Hampton|180|139
Strafford|413|375
Stratham|821|660
Tamworth|364|344
Tilton|468|392
Tuftonboro|610|202
Wakefield|679|336
Wolfeboro|1371|609
""",
    "D90_2": """
Acworth|112|135
Alexandria|176|136
Allenstown|486|627
Alstead|183|288
Amherst|1728|1132
Andover|280|320
Antrim|449|341
Ashland|281|234
Atkinson|977|477
Bath|140|83
Bennington|175|129
Benton|55|16
Berlin W1|255|487
Berlin W2|318|462
Berlin W3|418|534
Berlin W4|216|454
Bethlehem|219|287
Boscawen|304|509
Bow|955|1123
Bradford|224|263
Bridgewater|175|121
Bristol|496|310
Brookline|435|357
Campton|366|324
Canaan|274|387
Carroll|132|68
Charlestown|532|628
Chesterfield|423|443
Claremont W1|210|366
Claremont W2|714|1116
Claremont W3|438|797
Clarksville|34|21
Colebrook|266|162
Columbia|57|43
Concord W A|524|916
Concord W B|390|820
Concord W C|360|852
Concord W D|306|698
Concord W E|262|626
Concord W F|499|990
Concord W G|430|671
Concord W H|656|1073
Cornish|217|291
Croydon|97|69
Dalton|128|74
Danbury|171|124
Deering|230|164
Dixville|16|7
Dorchester|66|54
Dublin|229|282
Dummer|64|52
Dunbarton|254|298
Easton|61|44
Ellsworth|12|14
Enfield|389|589
Errol|61|42
Fitzwilliam|286|236
Francestown|268|240
Franconia|185|156
Franklin W1|303|313
Franklin W2|227|321
Franklin W3|280|387
Gilsum|85|100
Gorham|422|548
Goshen|88|121
Grafton|111|113
Grantham|302|222
Greenfield|213|175
Green's Grant|1|0
Greenville|209|194
Groton|61|49
Hancock|307|314
Hanover|782|1549
Harrisville|123|218
Haverhill|644|336
Hebron|124|67
Henniker|423|603
Hill|108|136
Hillsborough|603|496
Hinsdale|307|377
Holderness|370|251
Hollis|1128|900
Hopkinton|850|1161
Hudson|2015|2422
Jaffrey|689|555
Jefferson|196|109
Keene W1|237|518
Keene W2|370|742
Keene W3|393|759
Keene W4|436|737
Keene W5|515|898
Lancaster|451|404
Landaff|65|30
Langdon|91|57
Lebanon W1|355|550
Lebanon W2|321|527
Lebanon W3|323|502
Lempster|186|119
Lincoln|217|144
Lisbon|231|152
Litchfield|644|583
Littleton|804|557
Lyman|79|51
Lyme|176|321
Lyndeborough|214|147
Marlborough|234|315
Marlow|81|134
Mason|201|141
Milan|153|167
Milford|1619|1095
Millsfield|4|3
Monroe|235|112
Mont Vernon|319|226
Nashua W1|1245|1621
Nashua W2|1177|1319
Nashua W3|916|1196
Nashua W4|327|548
Nashua W5|1156|1452
Nashua W6|817|1282
Nashua W7|806|1061
Nashua W8|1056|1162
Nashua W9|1102|1169
Nelson|69|134
New Boston|548|430
Newbury|267|228
New Ipswich|552|328
New London|869|605
Newport|724|758
Northumberland|233|376
Orange|36|46
Orford|148|165
Pelham|1272|950
Pembroke|775|984
Peterborough|904|855
Piermont|97|94
Pinkham's Gt.|0|4
Pittsburg|109|72
Plainfield|226|429
Plymouth|580|598
Randolph|72|108
Richmond|122|118
Rindge|513|357
Roxbury|23|28
Rumney|312|221
Salem|3292|3409
Salisbury|103|196
Sharon|59|51
Shelburne|65|70
Springfield|139|131
Stark|40|55
Stewartstown|73|31
Stoddard|95|121
Stratford|67|50
Sugar Hill|118|113
Sullivan|89|100
Sunapee|551|480
Surry|100|117
Sutton|236|273
Swanzey|585|828
Temple|197|150
Thornton|301|237
Troy|187|247
Unity|132|167
Walpole|521|497
Warner|333|492
Warren|135|80
Washington|126|131
Waterville Val.|40|28
Weare|823|704
Webster|161|260
Wentworth|130|82
Wentworth's Loc.|6|8
Westmoreland|189|253
Whitefield|281|231
Wilmot|168|172
Wilton|469|392
Winchester|257|423
Windham|1336|842
Windsor|17|13
Woodstock|162|144
""",
    "D92_1": """
Albany|139|96|12|7|2
Alton|1373|536|74|25|20
Auburn|1173|707|47|26|1
Barrington|1407|1214|67|58|18
Barnstead|909|570|67|19|27
Bartlett|929|535|16|21|0
Bedford|5055|2526|145|61|64
Belmont|1376|883|55|49|28
Brentwood|600|461|30|17|7
Brookfield|194|106|13|5|0
Candia|1105|685|54|33|8
Center Harbor|336|198|6|8|2
Chatham|73|48|3|2|0
Chester|799|477|42|11|0
Conway|2284|1466|111|69|61
Danville|612|384|28|29|2
Deerfield|839|612|43|19|8
Derry|6503|4397|372|206|176
Dover W1|926|916|28|20|4
Dover W2|604|836|32|25|15
Dover W3|1164|1040|41|32|11
Dover W4|1064|936|35|21|11
Dover W5|840|892|35|13|13
Dover W6|798|895|19|12|0
Durham|2329|2534|78|61|13
East Kingston|393|288|11|19|4
Eaton|92|93|5|4|4
Effingham|256|162|9|25|2
Epping|1088|1017|60|45|12
Epsom|982|503|39|21|11
Exeter|2862|2830|151|96|59
Farmington|1072|876|37|46|9
Freedom|410|224|10|7|1
Fremont|685|463|51|31|4
Gilford|2250|1091|50|46|47
Gilmanton|779|472|46|19|24
Goffstown|3732|2714|146|74|92
Greenland|785|643|23|12|2
Hale's Loc.|1|0|0|0|0
Hampstead|2182|1227|97|43|64
Hampton|2852|4349|113|58|16
Hampton Falls|555|433|19|10|2
Hart's Loc.|18|11|0|0|0
Hooksett|2609|1721|75|66|10
Jackson|357|152|9|9|2
Kensington|423|417|29|14|9
Kingston|1415|1034|65|46|11
Laconia W1|894|462|26|24|16
Laconia W2|528|377|11|13|6
Laconia W3|663|466|21|14|17
Laconia W4|479|400|15|8|16
Laconia W5|410|374|20|11|14
Laconia W6|915|508|31|19|25
Lee|899|793|31|25|27
Londonderry|5350|3216|256|139|137
Madbury|424|316|16|10|5
Madison|541|344|23|18|4
Manchester W1|2315|1869|63|32|15
Manchester W2|1894|1704|61|46|16
Manchester W3|1161|1352|60|32|18
Manchester W4|1203|1445|50|32|13
Manchester W5|1152|1221|50|25|8
Manchester W6|1857|1881|60|37|17
Manchester W7|1330|1472|43|31|17
Manchester W8|1905|1674|78|31|24
Manchester W9|1664|1595|53|33|23
Manchester W10|1742|1557|52|46|10
Manchester W11|1118|1183|29|28|15
Manchester W12|1525|1672|56|27|15
Meredith|1793|768|53|41|29
Merrimack|6128|3866|294|107|152
Middleton|212|212|9|10|1
Milton|789|627|35|36|12
Moultonborough|1412|538|39|31|11
New Castle|334|236|6|7|2
New Durham|555|289|29|19|3
New Hampton|474|286|21|18|8
Newfields|249|247|11|6|1
Newington|230|167|11|5|1
Newmarket|1357|1716|78|48|20
Newton|806|686|32|41|10
North Hampton|1143|1165|52|14|5
Northwood|801|581|41|33|9
Nottingham|749|610|31|34|16
Ossipee|877|717|25|49|7
Pittsfield|767|523|26|24|0
Plaistow|1771|1234|67|65|5
Portsmouth W1|586|909|38|16|23
Portsmouth W2|1010|1416|82|20|53
Portsmouth W3|726|823|24|24|34
Portsmouth W4|1420|1491|52|25|41
Portsmouth W5|1125|1296|77|20|45
Raymond|1868|1291|103|66|12
Rye|1643|1304|81|23|42
Rochester W1|1119|906|29|21|8
Rochester W2|1219|883|20|18|11
Rochester W3|1218|972|31|30|4
Rochester W4|1115|1111|33|30|5
Rochester W5|1012|937|28|22|8
Rollinsford|602|638|29|16|2
Sandown|1031|692|44|63|11
Sandwich|439|312|15|10|2
Seabrook|1228|1910|176|41|4
Somersworth W1|494|439|18|0|2
Somersworth W2|401|466|8|14|3
Somersworth W3|458|460|18|12|2
Somersworth W4|469|481|9|7|2
Somersworth W5|261|366|5|14|1
South Hampton|257|189|11|11|1
Strafford|758|603|45|24|4
Stratham|1546|1249|55|16|3
Tamworth|626|484|25|33|4
Tuftonboro|762|325|24|32|5
Wakefield|971|634|34|28|8
Wolfeboro|1927|972|56|92|31
""",
    "D92_2": """
Acworth|134|270|7|1
Alexandria|204|269|11|1
Allenstown|547|1351|40|4
Alstead|284|484|13|2
Amherst|2387|2542|116|10
Andover|319|659|42|2
Antrim|414|595|25|3
Ashland|329|495|16|4
Atkinson|1408|1376|90|1
Bath|165|203|7|2
Bennington|215|311|20|0
Benton|61|35|3|0
Berlin W1|213|958|7|2
Berlin W2|259|892|10|2
Berlin W3|355|988|9|1
Berlin W4|165|970|11|3
Bethlehem|286|584|16|5
Boscawen|385|932|31|2
Bow|1137|2055|44|1
Bradford|247|454|25|3
Bridgewater|214|258|11|1
Bristol|530|620|21|3
Brookline|598|715|43|3
Cambridge|1|3|0|0
Campton|424|675|22|6
Canaan|393|914|14|3
Canterbury|250|768|18|4
Carroll|160|190|9|0
Charlestown|539|1359|38|5
Chesterfield|591|975|27|4
Chichester|287|556|34|2
Claremont W1|445|1090|20|3
Claremont W2|634|1229|12|5
Claremont W3|449|1247|22|1
Clarksville|47|76|1|1
Colebrook|346|486|15|2
Columbia|112|135|6|0
Concord W1|372|1124|35|7
Concord W2|362|1108|32|3
Concord W3|312|925|25|7
Concord W4|378|1501|46|14
Concord W5|462|1349|32|1
Concord W6|337|1182|41|13
Concord W7|520|1450|28|7
Concord W8|371|1050|33|6
Concord W9|466|987|26|5
Concord W10|579|1437|29|3
Cornish|244|599|13|3
Croydon|101|144|3|0
Dalton|145|241|11|1
Danbury|154|273|10|3
Deering|286|394|28|1
Dixville|15|12|3|0
Dorchester|68|101|3|0
Dublin|337|492|24|0
Dummer|65|110|1|1
Dunbarton|333|558|33|2
Easton|63|69|6|0
Ellsworth|13|24|3|1
Enfield|399|1233|23|0
Errol|63|95|3|0
Fitzwilliam|393|529|20|0
Francestown|313|438|21|4
Franconia|230|280|10|3
Franklin W1|333|690|11|2
Franklin W2|262|668|15|0
Franklin W3|328|723|17|5
Gilsum|108|204|4|0
Gorham|407|1165|20|2
Goshen|115|219|2|1
Grafton|142|263|16|1
Grantham|416|541|13|1
Greenfield|261|357|23|1
Greenville|279|454|26|0
Groton|76|116|2|0
Hancock|392|539|20|4
Hanover|1128|3272|81|14
Harrisville|152|328|9|4
Haverhill|771|817|13|1
Hebron|158|116|6|0
Henniker|542|1198|52|3
Hill|123|261|8|6
Hillsborough|707|1091|54|4
Hinsdale|457|910|25|5
Holderness|417|516|23|3
Hollis|1528|1873|101|5
Hopkinton|898|1916|58|5
Hudson|3477|5082|310|57
Jaffrey|913|1254|47|6
Jefferson|185|299|5|0
Keene W1|400|1258|36|2
Keene W2|511|1420|33|2
Keene W3|524|1331|28|0
Keene W4|605|1426|31|0
Keene W5|668|1509|35|3
Lancaster|601|890|15|4
Landaff|68|93|3|1
Langdon|113|164|5|0
Lebanon W1|563|1137|16|3
Lebanon W2|477|1168|21|0
Lebanon W3|508|1102|27|4
Lempster|209|217|9|0
Lincoln|238|411|11|1
Lisbon|259|375|7|1
Litchfield|1018|1453|82|4
Littleton|969|1316|37|2
Loudon|551|1208|45|3
Lyman|73|133|8|0
Lyme|260|638|8|0
Lyndeborough|278|345|27|0
Marlborough|276|636|16|2
Marlow|99|258|13|0
Mason|236|272|48|1
Milan|164|398|17|1
Milford|2380|2845|172|13
Millsfield|7|2|0|0
Monroe|226|184|6|0
Mont Vernon|426|494|41|5
Nashua W1|1854|2702|235|9
Nashua W2|1660|2432|85|7
Nashua W3|1310|2212|106|13
Nashua W4|681|1536|78|8
Nashua W5|1506|2333|90|7
Nashua W6|1173|2377|102|10
Nashua W7|1305|2058|104|10
Nashua W8|1525|2315|104|6
Nashua W9|1505|2398|86|11
Nelson|96|224|10|3
New Boston|720|973|59|1
New Ipswich|835|712|56|3
New London|1073|1183|17|2
Newbury|378|352|18|0
Newport|766|1482|45|2
Northfield|538|960|31|2
Northumberland|229|800|14|0
Orange|46|88|2|0
Orford|205|329|6|0
Pelham|1944|2455|169|31
Pembroke|959|2010|55|0
Peterborough|1123|1662|55|22
Piermont|144|205|3|0
Pinkham's Gt.|1|14|2|0
Pittsburg|197|207|7|2
Plainfield|274|780|8|0
Plymouth|736|1510|29|8
Randolph|61|193|7|0
Richmond|167|236|14|0
Rindge|860|778|35|4
Roxbury|38|74|3|0
Rumney|321|384|13|3
Salem|4238|7495|323|59
Salisbury|206|357|14|1
Sanbornton|393|758|36|5
Sharon|81|85|4|1
Shelburne|69|114|7|0
Springfield|191|253|4|1
Stark|53|139|3|2
Stewartstown|127|141|6|0
Stoddard|156|233|17|1
Stratford|70|204|4|0
Sugar Hill|133|166|5|4
Sullivan|85|204|8|0
Sunapee|654|931|35|3
Surry|136|221|8|0
Sutton|299|507|22|2
Swanzey|937|1735|48|3
Temple|240|293|23|4
Thornton|338|473|21|2
Tilton|527|885|39|3
Troy|257|512|29|0
Unity|154|363|20|1
Walpole|626|1121|22|3
Warner|441|755|33|2
Warren|161|183|0|0
Washington|194|230|8|1
Waterville Val.|64|75|4|1
Weare|1172|1623|86|0
Webster|209|455|12|2
Wentworth|154|152|21|2
Wentworth's Loc.|12|11|0|0
Westmoreland|275|492|19|1
Whitefield|301|535|11|0
Wilmot|209|307|16|0
Wilton|692|895|86|3
Winchester|451|885|34|6
Windham|2167|2193|137|19
Windsor|29|53|1|0
Woodstock|199|341|7|3
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
    for d, (cands, printed, fec, block) in districts.items():
        rs = rows(block)
        assert all(len(v) == len(cands) for _, v in rs), (year, d)
        for j in range(len(cands)):
            s = sum(v[j] for _, v in rs)
            assert s == printed[j], ("printed total", year, d, cands[j][0], s, printed[j])
            assert s == fec[j], ("FEC", year, d, cands[j][0], s, fec[j])
        for town, v in rs:
            county = TOWN_COUNTY.get(key(town))
            assert county, ("no county for town", town)
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
