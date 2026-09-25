"""New Hampshire U.S. House 1994 and 1998, county level, from the 'State of New Hampshire Manual for the General Court'
(NH Department of State), tables 'U.S. HOUSE: FIRST DISTRICT' / 'SECOND DISTRICT', General Election results by town/ward:
  1994: 1995 Manual (No. 54), printed pp. 276-281 - Internet Archive book scan manualforgeneral54newh, leaves 288-293
        (microfiche copy micro_IA40706953_0081, leaves 283-288, as backup).
  1998: 1999 Manual (No. 56), printed pp. 300-302 - Internet Archive book scan manualforgeneral56newh, leaves 316-318.
Local copies: R/data/county_house_files/new_hampshire/leads/ (found by a source-search agent, 2026-09-24; report
R/data/county_house_files/source_search/NH_1994_2008.md). Values read by eye from 400/600 dpi page images (the PDF text
layer drops columns).

Towns/wards are summed to the 10 counties with R/data/raw_house_county_open_states/new_hampshire/nh_town_county_crosswalk.csv
and the key normalisation of 01iq (wards rolled up to their city), plus the unincorporated places in EXTRA. Places listed
under 'No votes cast' are omitted. Scatter/write-ins are not printed in these tables.

Checks (asserted):
  * every candidate column sums to the printed 'Totals' row - EXCEPT 1998 District 2, where the printed town rows add up to
    Bass 85,746 and Rauh 72,321 against printed Totals of 85,740 and 72,217 (+6 and +104; Werme ties). Every town cell of
    that district was re-read at 600 dpi and matches the page, the town list is the same as in 1994, and the printed Totals
    equal the Clerk of the House figures, so the difference is inside the source; the town rows are kept as printed and the
    allowed differences are stated exactly in KNOWN_ROW_TOTAL_DIFF.
  * every printed Total equals the Clerk of the House statistics (R/data/clerk_house_stats/1994Stat.htm, 1998Stat.htm),
    except 1994 District 2 Swett: book 74,242, Clerk 74,243 (one vote; the book's town rows tie to the book's total).
  * every town maps to a county; all 10 counties appear each year.

Output: R/data/county_house_files/new_hampshire/new_hampshire_house_county_<year>.csv (year,district,county,candidate,party_code,votes)
"""
import csv, os, re
from collections import defaultdict

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.join(HERE, "..", "data")
CW = os.path.join(ROOT, "raw_house_county_open_states/new_hampshire/nh_town_county_crosswalk.csv")
OUT = os.path.join(ROOT, "county_house_files/new_hampshire")

EXTRA = {"GREENSGRANT": "COOS", "PINKHAMSGT": "COOS", "PINKHAMSGRANT": "COOS", "WENTWORTHSLOC": "COOS", "WENTWORTHSLOCATION": "COOS",
         "HALESLOC": "CARROLL", "HALESLOCATION": "CARROLL", "HARTSLOC": "CARROLL", "HARTSLOCATION": "CARROLL", "WATERVILLEVAL": "GRAFTON"}

def key(x):
    x = x.upper().replace("*", "")
    x = re.sub(r"\s+(WARD|WD|W)\s*[0-9A-H]+\s*$", "", x)   # 'Dover Ward 1', 'Dover Wd 1'
    return re.sub(r"[^A-Z0-9&]", "", x)

TOWN_COUNTY = {key(r["town"]): r["county"].upper() for r in csv.DictReader(open(CW))}
TOWN_COUNTY.update(EXTRA)

# year -> district -> (candidates [(name, party code)], printed totals, Clerk totals, town block)
RACES = {
    1994: {
        1: ([("William H. Zeliff, Jr.", "R"), ("Bill Verge", "D"), ("Paul Lannon", "I"), ("Scott Tosti", "I"), ("Merle Braley", "I")],
            [97017, 42481, 3548, 4203, 573], [97017, 42481, 3548, 4203, 573], "D94_1"),
        2: ([("Charles F. Bass", "R"), ("Dick Swett", "D"), ("John Lewicke", "I"), ("Linda Spitzfaden", "I")],
            [83121, 74242, 2986, 1223], [83121, 74243, 2986, 1223], "D94_2"),
    },
    1998: {
        1: ([("John E. Sununu", "R"), ("Peter Flood", "D")], [104430, 51783], [104430, 51783], "D98_1"),
        2: ([("Charles F. Bass", "R"), ("Mary Rauh", "D"), ("Paula Werme", "I")], [85740, 72217, 3338], [85740, 72217, 3338], "D98_2"),
    },
}
KNOWN_CLERK_DIFF = {(1994, 2, "Dick Swett"): 1}                          # Clerk minus printed total
KNOWN_ROW_TOTAL_DIFF = {(1998, 2, "Charles F. Bass"): 6, (1998, 2, "Mary Rauh"): 104}   # town sum minus printed total

DATA = {
    "D94_1": """
Albany|98|42|7|10|0
Alton|967|202|31|20|4
Auburn|773|258|37|58|1
Barrington|998|483|54|48|4
Barnstead|636|231|27|22|5
Bartlett|611|227|21|30|3
Bedford|3834|972|96|93|11
Belmont|1092|350|37|30|9
Brentwood|457|201|13|32|3
Brookfield|154|48|5|12|0
Candia|806|225|33|40|4
Center Harbor|252|82|7|6|1
Chatham|53|19|4|1|1
Chester|632|167|35|37|4
Conway|1439|596|72|30|7
Danville|445|150|21|43|0
Deerfield|589|268|19|42|3
Derry|3839|1374|199|218|23
Dover Ward 1|674|346|16|27|4
Dover Ward 2|464|336|19|16|4
Dover Ward 3|850|470|25|26|1
Dover Ward 4|642|362|22|28|3
Dover Ward 5|544|367|24|22|3
Dover Ward 6|629|313|17|24|2
Durham|1022|1089|34|47|8
East Kingston|296|87|7|29|0
Eaton|88|32|10|3|4
Effingham|144|61|2|14|3
Epping|853|331|31|90|10
Epsom|709|254|21|19|3
Exeter|2258|1226|68|106|8
Farmington|656|260|26|15|4
Freedom|297|137|4|8|0
Fremont|428|136|28|69|8
Gilford|1599|467|30|38|8
Gilmanton|584|206|19|16|7
Goffstown|2839|943|78|110|37
Greenland|614|220|17|19|1
Hale's Location|5|0|0|0|0
Hampstead|1298|392|24|44|3
Hampton|2779|1402|90|105|7
Hampton Falls|565|148|18|15|2
Hart's Location|15|3|0|1|0
Hooksett|1878|598|48|75|8
Jackson|275|90|5|6|2
Kensington|300|191|17|29|5
Kingston|910|328|30|67|9
Laconia Ward 1|668|181|23|4|0
Laconia Ward 2|556|177|18|9|9
Laconia Ward 3|562|194|14|10|12
Laconia Ward 4|372|171|15|16|8
Laconia Ward 5|405|171|19|10|3
Laconia Ward 6|501|156|21|12|0
Lee|570|435|33|38|3
Londonderry|3168|1071|125|148|12
Madbury|294|187|6|9|0
Madison|365|159|23|15|3
Manchester Ward 1|1866|950|47|43|5
Manchester Ward 2|1350|811|65|42|4
Manchester Ward 3|876|561|46|45|8
Manchester Ward 4|725|635|28|30|2
Manchester Ward 5|760|525|31|15|7
Manchester Ward 6|1455|858|52|57|4
Manchester Ward 7|1021|575|28|35|9
Manchester Ward 8|1433|848|49|51|9
Manchester Ward 9|1245|599|50|50|1
Manchester Ward 10|1302|756|46|33|8
Manchester Ward 11|845|452|46|29|2
Manchester Ward 12|1219|543|36|19|6
Meredith|1287|325|35|30|3
Merrimack|4085|1528|185|184|20
Middleton|134|68|6|4|1
Milton|513|197|23|28|4
Moultonborough|1097|226|46|11|5
New Castle|297|137|5|6|2
New Durham|398|107|17|13|4
New Hampton|398|129|17|9|4
Newfields|219|118|7|19|0
Newington|177|51|6|5|2
Newmarket|1012|618|47|61|7
Newton|445|242|19|40|6
North Hampton|1052|404|25|39|3
Northwood|531|188|31|20|6
Nottingham|538|233|20|39|6
Ossipee|585|218|18|28|5
Pittsfield|553|193|21|18|3
Plaistow|959|510|35|42|2
Portsmouth Ward 1|355|339|19|12|4
Portsmouth Ward 2|497|679|30|23|7
Portsmouth Ward 3|384|338|10|15|6
Portsmouth Ward 4|859|493|16|29|6
Portsmouth Ward 5|636|661|35|18|4
Raymond|1114|347|48|347|18
Rochester Ward 1|932|383|36|33|6
Rochester Ward 2|1000|379|26|42|8
Rochester Ward 3|1007|420|24|36|7
Rochester Ward 4|957|441|31|17|5
Rochester Ward 5|840|384|29|18|3
Rollinsford|452|260|23|26|3
Rye|1272|559|42|34|7
Sandown|696|255|35|50|8
Sandwich|349|238|17|8|1
Seabrook|1141|503|36|45|12
Somersworth Ward 1|349|158|9|18|3
Somersworth Ward 2|330|155|8|14|0
Somersworth Ward 3|304|134|10|14|1
Somersworth Ward 4|325|184|44|13|3
Somersworth Ward 5|195|139|6|14|1
South Hampton|184|91|12|10|0
Strafford|539|234|32|27|4
Stratham|1207|502|42|46|2
Tamworth|428|252|21|19|5
Tuftonboro|641|122|18|19|1
Wakefield|867|225|28|33|4
Wolfeboro|1429|409|29|67|9
""",
    "D94_2": """
Acworth|168|121|7|6
Alexandria|231|122|21|1
Allenstown|557|545|18|15
Alstead|301|290|14|9
Amherst|1959|1278|38|12
Andover|354|268|29|3
Antrim|416|309|19|15
Ashland|332|196|14|6
Atkinson|935|690|28|8
Bath|151|103|4|3
Bennington|200|164|16|6
Benton|60|18|2|0
Berlin Ward 1|252|520|11|6
Berlin Ward 2|280|532|18|3
Berlin Ward 3|375|576|7|8
Berlin Ward 4|209|494|7|7
Bethlehem|305|257|12|7
Boscawen|432|387|10|9
Bow|1368|1259|26|5
Bradford|296|271|17|9
Bridgewater|241|138|9|1
Bristol|563|296|15|10
Brookline|543|351|24|8
Campton|417|304|20|13
Canaan|425|406|23|8
Canterbury|344|448|11|5
Carroll|144|84|4|0
Charlestown|593|618|18|15
Chesterfield|526|433|11|4
Chichester|358|274|18|5
Claremont Ward 1|455|625|24|5
Claremont Ward 2|698|702|16|6
Claremont Ward 3|499|663|21|10
Clarksville|52|22|0|0
Colebrook|393|218|8|7
Columbia|82|49|2|2
Concord Ward 1|429|506|13|8
Concord Ward 2|422|519|9|6
Concord Ward 3|351|548|15|3
Concord Ward 4|459|806|17|9
Concord Ward 5|539|882|14|0
Concord Ward 6|326|527|19|11
Concord Ward 7|604|888|22|7
Concord Ward 8|415|452|16|6
Concord Ward 9|438|531|12|6
Concord Ward 10|672|876|27|10
Cornish|304|311|13|11
Croydon|149|67|5|2
Dalton|135|106|4|2
Danbury|181|115|11|0
Deering|299|149|10|3
Dixville|13|5|0|0
Dorchester|74|41|3|1
Dublin|345|248|10|13
Dummer|58|43|0|2
Dunbarton|380|254|16|4
Easton|65|32|3|6
Ellsworth|22|7|2|2
Enfield|536|535|23|9
Errol|73|23|1|0
Fitzwilliam|294|219|15|5
Francestown|316|247|9|6
Franconia|205|185|5|3
Franklin Ward 1|363|267|9|3
Franklin Ward 2|275|278|7|8
Franklin Ward 3|382|300|6|8
Gilsum|128|71|0|0
Gorham|420|505|10|6
Goshen|155|135|5|0
Grafton|179|97|11|4
Grantham|446|320|6|3
Greenfield|207|169|11|8
Greenville|245|177|15|7
Groton|92|36|4|2
Hancock|376|347|12|8
Hanover|933|1964|26|11
Harrisville|159|204|7|6
Haverhill|674|351|15|3
Hebron|169|55|9|1
Henniker|623|608|38|12
Hill|143|84|7|6
Hillsborough|703|433|22|15
Hinsdale|388|334|4|2
Holderness|369|258|13|4
Hollis|1292|966|31|12
Hopkinton|1122|1117|33|14
Hudson|2456|1988|76|30
Jaffrey|716|594|28|19
Jefferson|220|119|4|2
Keene Ward 1|374|503|18|7
Keene Ward 2|471|632|24|12
Keene Ward 3|530|644|22|7
Keene Ward 4|570|711|17|7
Keene Ward 5|643|743|12|7
Lancaster|575|416|14|4
Landaff|59|45|4|0
Langdon|121|64|3|0
Lebanon Ward 1|498|633|18|3
Lebanon Ward 2|440|570|17|7
Lebanon Ward 3|508|626|17|8
Lempster|255|90|15|3
Lincoln|227|158|8|2
Lisbon|238|116|12|0
Litchfield|812|453|21|8
Littleton|953|533|20|14
Loudon|594|447|18|9
Lyman|85|55|5|1
Lyme|263|411|6|0
Lyndeborough|265|127|13|7
Marlborough|270|309|12|3
Marlow|150|113|6|2
Mason|204|130|21|4
Milan|166|134|6|3
Milford|1883|1263|81|19
Millsfield|6|1|0|0
Monroe|218|112|4|4
Mont Vernon|384|226|25|5
Nashua Ward 1|1388|1356|42|14
Nashua Ward 2|1082|989|32|9
Nashua Ward 3|1011|1037|42|12
Nashua Ward 4|435|670|22|0
Nashua Ward 5|1042|926|29|14
Nashua Ward 6|826|1134|44|12
Nashua Ward 7|887|927|33|13
Nashua Ward 8|961|1057|25|7
Nashua Ward 9|998|980|47|10
Nelson|97|120|5|4
New Boston|710|408|27|8
New Ipswich|666|357|56|14
New London|1056|648|16|7
Newbury|392|205|11|1
Newport|1060|748|37|17
Northfield|511|366|23|6
Northumberland|247|346|16|6
Orange|46|52|1|0
Orford|192|136|5|5
Pelham|1289|1027|47|29
Pembroke|995|850|33|13
Peterborough|1039|875|26|28
Piermont|125|100|3|1
Pinkham's Grant|0|4|0|0
Pittsburg|194|69|0|1
Plainfield|352|383|12|4
Plymouth|616|593|33|15
Randolph|86|110|3|3
Richmond|158|110|20|4
Rindge|618|411|17|6
Roxbury|42|28|0|1
Rumney|327|192|20|10
Salem|2957|3187|90|46
Salisbury|225|156|9|7
Sanbornton|444|342|22|7
Sharon|80|46|5|1
Shelburne|80|71|1|0
Springfield|207|144|8|3
Stark|59|52|2|4
Stewartstown|100|57|6|0
Stoddard|133|84|9|2
Stratford|80|77|2|1
Sugar Hill|138|114|4|3
Sullivan|84|86|12|2
Sunapee|796|438|34|6
Surry|131|94|3|0
Sutton|301|232|21|8
Swanzey|898|731|27|6
Temple|226|146|17|4
Thornton|346|188|22|11
Tilton|510|342|22|12
Troy|279|227|11|5
Unity|233|156|8|5
Walpole|648|613|14|9
Warner|454|435|18|5
Warren|152|68|10|4
Washington|195|93|4|6
Waterville Valley|54|49|0|0
Weare|1122|650|41|12
Webster|264|224|5|4
Wentworth|162|55|12|4
Wentworth's Location|9|6|0|0
Westmoreland|315|265|10|2
Whitefield|321|263|8|10
Wilmot|206|173|5|6
Wilton|574|469|40|10
Winchester|465|360|9|11
Windham|1588|977|56|18
Windsor|22|27|1|0
Woodstock|200|136|7|3
""",
    "D98_1": """
Albany|124|51
Alton|1016|290
Auburn|938|309
Barrington|1208|684
Barnstead|679|301
Bartlett|635|287
Bedford|4366|1262
Belmont|977|337
Brentwood|495|257
Brookfield|167|55
Candia|909|291
Center Harbor|241|131
Chatham|61|21
Chester|730|246
Conway|1418|673
Danville|582|204
Deerfield|823|339
Manchester Wd 3|1002|667
Manchester Wd 4|1109|611
Manchester Wd 5|894|493
Manchester Wd 6|1904|897
Manchester Wd 7|1255|581
Manchester Wd 8|1869|785
Manchester Wd 9|1520|667
Manchester Wd 10|1573|719
Manchester Wd 11|1080|468
Manchester Wd 12|1466|596
Meredith|1280|483
Merrimack|3895|2460
Middleton|195|107
Milton|530|281
Moultonborough|1180|331
New Castle|383|163
New Durham|388|173
Derry|3713|1757
Dover Wd 1|797|433
Dover Wd 2|421|384
Dover Wd 3|913|591
Dover Wd 4|760|462
Dover Wd 5|536|420
Dover Wd 6|676|408
Durham|1065|1144
East Kingston|392|150
Eaton|76|52
Effingham|246|117
Epping|818|356
Epsom|772|337
Exeter|2334|1517
Farmington|696|375
Freedom|366|138
Fremont|493|146
Gilford|1502|521
Gilmanton|569|270
Goffstown|3064|1208
Greenland|719|294
Hale's Location|23|4
Hampstead|1439|564
Hampton|2845|1719
Hampton Falls|549|191
New Hampton|383|194
Newfields|310|182
Newington|239|89
Newmarket|1062|839
Newton|473|305
North Hampton|1002|474
Northwood|555|269
Nottingham|595|427
Ossipee|740|283
Pittsfield|572|260
Plaistow|1071|464
Portsmouth Wd 1|414|460
Portsmouth Wd 2|576|846
Portsmouth Wd 3|414|326
Portsmouth Wd 4|905|664
Portsmouth Wd 5|661|697
Raymond|1243|505
Rye|1367|731
Rochester Wd 1|842|444
Rochester Wd 2|919|421
Rochester Wd 3|936|446
Rochester Wd 4|873|488
Rochester Wd 5|765|386
Rollinsford|491|335
Sandown|736|308
Hart's Location|13|5
Hooksett|1919|727
Jackson|266|142
Kensington|316|232
Kingston|1002|413
Laconia Wd 1|656|223
Laconia Wd 2|462|173
Laconia Wd 3|433|187
Laconia Wd 4|427|157
Laconia Wd 5|367|177
Laconia Wd 6|424|172
Lee|608|582
Londonderry|3662|1449
Madbury|300|238
Madison|431|203
Manchester Wd 1|2160|1075
Manchester Wd 2|1610|853
Sandwich|334|289
Seabrook|1015|626
Somersworth Wd 1|401|218
Somersworth Wd 2|306|176
Somersworth Wd 3|287|216
Somersworth Wd 4|348|230
Somersworth Wd 5|227|164
South Hampton|160|114
Strafford|510|254
Stratham|1416|617
Tamworth|517|278
Tuftonboro|580|168
Wakefield|804|301
Wolfeboro|1649|503
""",
    "D98_2": """
Acworth|163|135|12
Alexandria|207|124|15
Allenstown|477|472|11
Alstead|242|309|12
Amherst|2077|1347|67
Andover|314|345|20
Antrim|389|299|15
Ashland|276|184|19
Atkinson|1190|602|36
Bath|162|77|10
Bennington|195|153|14
Benton|44|13|2
Berlin Wd 1|473|337|9
Berlin Wd 2|517|293|14
Berlin Wd 3|637|434|16
Berlin Wd 4|455|332|16
Francestown|307|259|5
Franconia|277|162|11
Franklin Wd 1|361|298|11
Franklin Wd 2|285|235|7
Franklin Wd 3|372|312|11
Gilsum|115|87|6
Gorham|550|423|16
Goshen|114|109|4
Grafton|160|135|17
Grantham|486|376|11
Greenfield|194|180|12
Greenville|207|172|20
Green's Grant|1|0|0
Groton|69|32|2
Hancock|414|340|16
Hanover|1007|1952|22
Bethlehem|342|252|13
Boscawen|460|434|12
Bow|1494|1099|41
Bradford|251|277|16
Bridgewater|195|106|5
Bristol|479|281|26
Brookline|629|474|37
Campton|361|263|18
Canaan|462|394|16
Canterbury|332|462|14
Carroll|175|74|7
Charlestown|616|571|24
Chesterfield|575|493|18
Chichester|351|287|18
Claremont Wd 1|454|517|16
Claremont Wd 2|656|563|19
Claremont Wd 3|467|595|14
Clarksville|47|15|3
Colebrook|363|102|21
Columbia|91|35|2
Concord Wd 1|498|552|20
Concord Wd 2|435|429|17
Concord Wd 3|389|553|20
Concord Wd 4|472|796|28
Harrisville|139|244|6
Haverhill|768|252|27
Hebron|153|68|4
Henniker|614|684|34
Hill|133|83|3
Hillsborough|669|509|21
Hinsdale|413|383|10
Holderness|375|263|9
Hollis|1301|920|46
Hopkinton|1020|1047|34
Hudson|2598|1942|125
Jaffrey|808|638|18
Jefferson|239|81|9
Keene Wd 1|345|481|12
Keene Wd 2|464|654|20
Keene Wd 3|575|645|19
Keene Wd 4|591|705|24
Keene Wd 5|720|749|15
Lancaster|619|306|17
Landaff|62|24|0
Langdon|124|89|4
Lebanon Wd 1|499|587|13
Lebanon Wd 2|501|551|19
Lebanon Wd 3|522|524|8
Concord Wd 5|557|784|13
Concord Wd 6|334|515|26
Concord Wd 7|613|861|20
Concord Wd 8|420|478|17
Concord Wd 9|378|516|5
Concord Wd 10|777|911|23
Cornish|337|326|11
Croydon|108|63|3
Dalton|156|56|4
Danbury|172|117|12
Deering|245|194|24
Dixville|20|1|0
Dorchester|55|23|2
Dublin|313|287|6
Dummer|74|22|1
Dunbarton|409|316|10
Easton|66|42|3
Ellsworth|21|8|1
Enfield|556|547|16
Errol|67|19|1
Fitzwilliam|310|300|19
Lempster|165|94|6
Lincoln|219|128|12
Lisbon|215|83|6
Litchfield|920|594|33
Littleton|1078|400|19
Loudon|673|478|29
Lyman|80|51|4
Lyme|251|432|4
Lyndeborough|242|196|9
Marlborough|269|360|18
Marlow|112|133|7
Mason|184|139|18
Milan|229|106|2
Milford|1859|1358|102
Millsfield|7|0|0
Monroe|272|74|8
Mont Vernon|367|266|23
Nashua Wd 1|1451|1316|59
Nashua Wd 2|1146|999|39
Nashua Wd 3|1001|1025|56
Nashua Wd 4|448|617|32
Nashua Wd 5|1072|1003|44
Nashua Wd 6|861|1049|46
Nashua Wd 7|870|879|31
Nashua Wd 8|1002|963|39
Nashua Wd 9|1100|1033|38
Nelson|96|126|5
New Boston|730|480|32
New Ipswich|701|316|19
New London|1080|687|15
Newbury|394|271|14
Newport|875|702|23
Northfield|508|431|21
Northumberland|285|219|10
Orange|41|57|2
Orford|190|153|4
Pelham|1390|897|76
Pembroke|917|764|32
Peterborough|1114|961|64
Stewartstown|96|34|4
Stoddard|178|140|5
Stratford|69|57|2
Sugar Hill|151|106|7
Sullivan|109|88|11
Sunapee|733|527|9
Surry|139|117|2
Sutton|300|286|11
Swanzey|854|805|31
Temple|227|157|11
Thornton|281|221|17
Tilton|468|291|16
Troy|237|215|13
Unity|194|146|12
Walpole|640|620|16
Warner|410|460|18
Warren|165|49|7
Washington|188|102|6
Piermont|132|84|7
Pittsburg|175|49|2
Plainfield|358|454|8
Plymouth|573|502|31
Randolph|101|100|6
Richmond|142|136|16
Rindge|664|460|29
Roxbury|36|49|1
Rumney|284|143|14
Salem|3533|2636|133
Salisbury|204|183|10
Sanbornton|475|367|21
Sharon|83|64|1
Shelburne|94|44|5
Springfield|198|151|6
Stark|78|29|1
Waterville Valley|56|41|0
Weare|965|675|58
Webster|262|244|8
Wentworth|126|57|10
Wentworth's Location|6|4|0
Westmoreland|271|272|13
Whitefield|348|189|14
Wilmot|208|218|10
Wilton|549|498|37
Winchester|382|405|24
Windham|1784|894|62
Windsor|25|33|0
Woodstock|210|112|14
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
        for j, (name, _) in enumerate(cands):
            s = sum(v[j] for _, v in rs)
            assert s - printed[j] == KNOWN_ROW_TOTAL_DIFF.get((year, d, name), 0), ("printed total", year, d, name, s, printed[j])
            assert clerk[j] - printed[j] == KNOWN_CLERK_DIFF.get((year, d, name), 0), ("Clerk", year, d, name, printed[j], clerk[j])
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
