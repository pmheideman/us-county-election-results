"""New York U.S. House 1990, 1992, 1994, county level, from the New York State Board of Elections'
'General Election Vote' books (Internet Archive, Statistical Reference Index microfiche):
  1990  micro_IA40706939_0195  'November 6, 1990 General Election Vote' - Representative in Congress,
        scan pages 11-18 (34 districts; counties as rows, candidate lines as columns)
  1992  micro_IA40706946_0089  'NYS Board of Elections - Congressional Vote - Nov. 3, 1992', pages C-1..C-8
        (scan pages 22-29; 31 districts)
  1994  micro_IA40706953_0122  '1994 General Election Vote' - Representative in Congress, scan pages 27-34
        (31 districts)
Local copies of the page images: R/data/county_house_files/sri/ny/ (jp2 zips).

Values are hand-transcribed from the page images, one row per party LINE (New York fusion voting:
a candidate may appear on several lines - DEM, REP, CON, LIB, RTL, IDN/INN, ...). For each district:
per-line county values, the line's district total (multi-county districts), the 'Blank, Void and
Scattering' row and the printed TOTAL row.

Checks (the script stops on any failure):
  * every line's county values add up to its printed district total (multi-county districts);
  * every county column (lines + blank/void/scattering) adds up to the printed county TOTAL;
  * every candidate's district total, summed over all his/her lines, equals the official result:
    1992 and 1994 the Clerk of the House 'Statistics of the ... Election' (every per-line value and the
    scattering value also appear there), 1990 the FEC 'Federal Elections 90' combined candidate totals
    (FEC misspells Bellitto 'Sellitto'); OFFICIAL below;
  * all 62 counties appear each year.
Known source inconsistency (not a transcription issue, allowed explicitly): 1994 District 29's printed
Blank/Void total (7,079) and district TOTAL (193,783) leave out Erie's 4,598 blank votes (the Erie
column itself ties); the Clerk repeats the printed figures. Blank/void votes are not output.

Faint cells resolved by the row/column ties (and confirmed on enhanced crops): 1992 D27 Erie column and
Wyoming CON/RTL/Blank, D26 Orange column, D31 Cattaraugus REP 17,130, D25 Onondaga/Cayuga cells;
1994 D25 Onondaga CCP 1,166, D26 Ulster REP 27,509, D29 Niagara DEM 32,972; 1990 D24 (faint page).

Party code (project rule from 01o_house_county_new_york.R, DEM wins when a candidate carries both
DEM and REP lines, e.g. Charles Rangel 1990): D if the candidate is on the DEM line, else R if on
the REP line, else I. The vote is the candidate's sum over all lines.

Output: R/data/county_house_files/sri/new_york_house_county_<year>.csv (year,district,county,candidate,party_code,votes)
"""
import csv, os, re
from collections import defaultdict

HERE = os.path.dirname(os.path.abspath(__file__))
OUT = os.path.join(HERE, "../data/county_house_files/sri")
DATA = {
"1990": """
D 1 | SUFFOLK
DEM | George J. Hochbrueckner | 72937
REP | Francis W. Creighton | 46380
CON | Clayton Baldwin, Jr. | 6883
RTL | Peter J. O'Hara | 5111
TXB | George J. Hochbrueckner | 2274
BLANK | 9595
TOTAL | 143180
D 2 | SUFFOLK
DEM | Thomas J. Downey | 56722
REP | John W. Bugler | 31808
CON | Dominic A. Curcio | 8150
RTL | John W. Bugler | 5051
BLANK | 8274
TOTAL | 110005
D 3 | NASSAU, SUFFOLK
DEM | Robert J. Mrazek | 37620 32498 70118
REP | Robert Previdi | 25717 19982 45699
CON | Robert Previdi | 6831 6559 13390
RTL | Francis A. Dreger | 2531 2384 4915
LIB | Robert J. Mrazek | 1437 1474 2911
BLANK | 5396 3015 8411
TOTAL | 79532 65912 145444
D 4 | NASSAU
DEM | Francis T. Goban | 41308
REP | Norman F. Lent | 63838
CON | Norman F. Lent | 15466
RTL | John J. Dunkle | 6706
LIB | Ben-Zion J. Heyman | 2343
BLANK | 10643
TOTAL | 140304
D 5 | NASSAU
DEM | Mark S. Epstein | 51738
REP | Raymond J. McGrath | 59568
CON | Raymond J. McGrath | 12380
RTL | Edward K. Kitt | 6000
LIB | Mark S. Epstein | 2182
BLANK | 10059
TOTAL | 141927
D 6 | QUEENS
DEM | Floyd H. Flake | 42499
REP | William Sampol | 13224
RTL | John Cronin | 3111
LIB | Floyd H. Flake | 1807
BLANK | 22917
TOTAL | 83558
D 7 | QUEENS
DEM | Gary L. Ackerman | 48159
LIB | Gary L. Ackerman | 2932
BLANK | 32347
TOTAL | 83438
D 8 | BRONX, NASSAU, QUEENS
DEM | James H. Scheuer | 12874 4614 36064 53552
REP | Gustave Reifenkugel | 1902 1023 11459 14384
CON | Gustave Reifenkugel | 936 247 6079 7262
LIB | James H. Scheuer | 470 248 2126 2844
BLANK | 7907 636 14909 23452
TOTAL | 24089 6768 70637 101494
D 9 | QUEENS
DEM | Thomas J. Manton | 35177
REP | Ann Pfoser Darby | 13330
CON | Thomas V. Ognibene | 6137
BLANK | 19981
TOTAL | 74625
D 10 | KINGS
DEM | Charles E. Schumer | 58673
REP | Patrick J. Kinsella | 9199
CON | Patrick J. Kinsella | 5764
LIB | Charles E. Schumer | 2795
BLANK | 15397
TOTAL | 91828
D 11 | KINGS
DEM | Edolphus Towns | 34711
CON | Ernest Johnson | 1676
LIB | Edolphus Towns | 1575
NAP | Lorraine Stevens | 1094
BLANK | 22345
TOTAL | 61401
D 12 | KINGS
DEM | Major R. Owens | 39103
CON | Joseph Caesar | 1159
LIB | Major R. Owens | 1467
NAP | Mamie Moore | 1021
BLANK | 15365
TOTAL | 58115
D 13 | KINGS
DEM | Stephen J. Solarz | 45412
REP | Edwin Ramos | 7954
CON | Edwin Ramos | 3603
LIB | Stephen J. Solarz | 2034
BLANK | 17908
TOTAL | 76911
D 14 | RICHMOND, KINGS
DEM | Anthony J. Pocchia | 26568 6170 32738
REP | Susan Molinari | 32673 11598 44271
CON | Susan Molinari | 10393 3952 14345
RTL | Christine Sacchi | 3625 745 4370
LIB | Anthony J. Pocchia | 1523 364 1887
BLANK | 12438 5968 18406
TOTAL | 87220 28797 116017
D 15 | NEW YORK
DEM | Frances L. Reiter | 30519
REP | Bill Green | 48505
CON | Michael T. Berns | 3654
LIB | Frances L. Reiter | 2945
BVP | Bill Green | 4414
BLANK | 17485
TOTAL | 107522
D 16 | NEW YORK
DEM | Charles B. Rangel | 50421
REP | Charles B. Rangel | 3440
LIB | Charles B. Rangel | 2021
NAP | Alvaader Frazier | 1592
BLANK | 18635
TOTAL | 76109
D 17 | BRONX, NEW YORK
DEM | Ted Weiss | 13981 59469 73450
REP | William W. Koeppel | 2805 12414 15219
CON | Mark Goret | 736 2192 2928
LIB | Ted Weiss | 663 5048 5711
NAP | John Patterson | 211 876 1087
BLANK | 4684 12861 17545
TOTAL | 23080 92860 115940
D 18 | BRONX
DEM | Jose E. Serrano | 36652
REP | Joseph Chiavaro | 1189
CON | Anna Johnson | 717
LIB | Jose E. Serrano | 1372
NAP | Mary Rivera | 866
BLANK | 16030
TOTAL | 56826
D 19 | BRONX, WESTCHESTER
DEM | Eliot L. Engel | 31281 12647 43928
REP | William J. Gouldman | 8703 8432 17135
CON | Kevin Brawley | 4736 3715 8451
RTL | Kevin Brawley | 1613 1804 3417
LIB | Eliot L. Engel | 1375 455 1830
BLANK | 15554 5230 20784
TOTAL | 63262 32283 95545
D 20 | WESTCHESTER
DEM | Nita M. Lowey | 82203
REP | Glenn D. Bellitto | 35575
CON | John M. Schafer | 8610
RTL | John M. Schafer | 4420
BLANK | 13819
TOTAL | 144627
D 21 | PUTNAM, DUTCHESS, ORANGE, WESTCHESTER
DEM | Richard L. Barbuto | 4818 11591 6499 11220 34128
REP | Hamilton Fish, Jr. | 13413 28800 16152 21563 79928
CON | Hamilton Fish, Jr. | 4571 6502 3640 5225 19938
RTL | Richard S. Curtin | 1214 1918 1224 1569 5925
BLANK | 2248 3823 3551 4165 13787
TOTAL | 26264 52634 31066 43742 153706
D 22 | ROCKLAND, ORANGE, SULLIVAN, WESTCHESTER
DEM | John G. Dow | 17055 8157 2732 9090 37034
REP | Benjamin A. Gilman | 46657 29573 6715 12550 95495
RTL | Margaret M. Beirne | 3770 1591 452 843 6656
BLANK | 10292 5349 1416 4025 21082
TOTAL | 77774 44670 11315 26508 160267
D 23 | ALBANY, SCHENECTADY, MONTGOMERY, RENSSELAER
DEM | Michael R. McNulty | 66078 23970 4274 8170 102492
REP | Margaret B. Buhrmaster | 35357 23497 2621 4285 65760
CON | Michael R. McNulty | 8385 4195 540 1627 14747
BLANK | 5963 3748 1111 1309 12131
TOTAL | 115783 55410 8546 15391 195130
D 24 | COLUMBIA, GREENE, SARATOGA, WARREN, WASHINGTON, DUTCHESS, RENSSELAER
DEM | Bob Lawrence | 6384 4636 16544 5431 4802 5947 12927 56671
REP | Gerald B. Solomon | 9869 7752 28902 10666 9284 8429 17610 92512
CON | Gerald B. Solomon | 2810 1766 6548 1615 2026 2214 5006 21985
RTL | Gerald B. Solomon | 771 562 2000 482 615 689 1590 6709
BLANK | 1389 1351 3012 1127 1119 1812 2920 12730
TOTAL | 21223 16067 57006 19321 17846 19091 40053 190607
D 25 | CHENANGO, CORTLAND, OTSEGO, SCHOHARIE, DELAWARE, MADISON, MONTGOMERY, ONEIDA, TOMPKINS
REP | Sherwood L. Boehlert | 9271 7332 10344 5191 6957 2048 4235 41426 4544 91348
LIB | William L. Griffen | 1508 2368 2311 890 1402 336 613 7247 806 17481
BLANK | 4307 3467 6061 4456 3970 1702 4760 27247 3594 59564
TOTAL | 15086 13167 18716 10537 12329 4086 9608 75920 8944 168393
D 26 | CLINTON, ESSEX, FRANKLIN, FULTON, HAMILTON, HERKIMER, JEFFERSON, LEWIS, ST. LAWRENCE
REP | David O'B. Martin | 10611 7289 7655 7419 1698 10859 15315 5334 16686 82866
CON | David O'B. Martin | 1855 1201 1160 1982 498 1858 2413 659 2848 14474
BLANK | 8218 5777 5498 7623 1079 10023 8107 2678 10358 59361
TOTAL | 20684 14267 14313 17024 3275 22740 25835 8671 29892 156701
D 27 | ONONDAGA, MADISON
DEM | Peggy L. Murray | 45882 4161 50043
REP | James T. Walsh | 74528 8914 83442
CON | James T. Walsh | 10158 1620 11778
RTL | Stephen K. Hoff | 2786 311 3097
LIB | Peggy L. Murray | 2146 249 2395
BLANK | 10258 1591 11849
TOTAL | 145758 16846 162604
D 28 | BROOME, TIOGA, ULSTER, DELAWARE, SULLIVAN, TOMPKINS
DEM | Matt McHugh | 44836 7773 29370 1424 4701 9711 97815
REP | Seymour Krieger | 22405 5644 16962 1201 3573 3292 53077
BLANK | 6266 1669 10244 519 1372 1084 21154
TOTAL | 73507 15086 56576 3144 9646 14087 172046
D 29 | CAYUGA, OSWEGO, SENECA, WAYNE, MONROE, ONEIDA
DEM | Alton F. Eber | 5234 6477 1993 4211 14909 2011 34835
REP | Frank Horton | 12559 19643 5529 15505 30652 5217 89105
CON | Peter DeMauro | 2131 2708 755 2299 4128 578 12599
RTL | Donald M. Peters | 649 928 568 836 1526 371 4878
BLANK | 3192 4980 1177 2010 5287 1552 18198
TOTAL | 23765 34736 10022 24861 56502 9729 159615
D 30 | GENESEE, LIVINGSTON, MONROE, ONTARIO
DEM | Louise M. Slaughter | 7650 2999 80349 6282 97280
REP | John M. Regan, Jr. | 5930 1841 38044 3202 49017
CON | John M. Regan, Jr. | 1346 409 10767 762 13284
RTL | John M. Regan, Jr. | 864 261 3801 307 5233
BLANK | 1083 304 7053 555 8995
TOTAL | 16873 5814 140014 11108 173809
D 31 | WYOMING, CATTARAUGUS, ERIE, LIVINGSTON, ONTARIO
DEM | Kevin P. Gaughan | 2929 977 52536 3234 5752 65428
REP | Bill Paxon | 5639 1399 48255 6129 9476 70898
CON | Bill Paxon | 996 203 8335 1011 1412 11957
RTL | Bill Paxon | 683 199 5570 387 543 7382
LIB | Kevin P. Gaughan | 223 68 3214 176 219 3900
BLANK | 588 184 5638 929 2437 9776
TOTAL | 11058 3030 123548 11866 19839 169341
D 32 | NIAGARA, ORLEANS, ERIE, MONROE
DEM | John J. LaFalce | 25415 4944 20239 15054 65652
REP | Michael T. Waring | 16898 3461 10910 7784 39053
CON | Kenneth J. Kowalski | 6336 744 2869 1843 11792
RTL | Kenneth J. Kowalski | 2559 331 1453 718 5061
LIB | John J. LaFalce | 1074 249 922 470 2715
BLANK | 4440 984 2909 4332 12665
TOTAL | 56722 10713 39302 30201 136938
D 33 | ERIE
DEM | Henry J. Nowak | 81609
REP | Thomas K. Kepfer | 18181
CON | Louis P. Corrigan, Jr. | 6460
LIB | Henry J. Nowak | 3296
BLANK | 16287
TOTAL | 125833
D 34 | ALLEGANY, CHAUTAUQUA, CHEMUNG, SCHUYLER, STEUBEN, YATES, CATTARAUGUS, TOMPKINS
DEM | Joseph P. Leahey | 2358 12835 7091 1452 4964 1498 6490 733 37421
REP | Amo Houghton | 7513 20160 16238 2804 15912 3725 9706 1070 77128
CON | Amo Houghton | 1193 4355 1757 594 2434 697 1529 144 12703
LIB | Nevin K. Eklund | 168 724 225 86 220 77 275 32 1807
BLANK | 1174 3586 2185 462 2083 857 1991 385 12723
TOTAL | 12406 41660 27496 5398 25613 6854 19991 2364 141782
""",
"1992": """
D 1 | SUFFOLK
DEM | George J. Hochbrueckner | 111908
REP | Edward P. Romaine | 87248
CON | Edward P. Romaine | 11785
RTL | Edward P. Romaine | 11010
LIF | George J. Hochbrueckner | 6032
BLANK | 29060
TOTAL | 257043
D 2 | SUFFOLK
DEM | Thomas J. Downey | 91320
REP | Rick A. Lazio | 94208
CON | Rick A. Lazio | 15178
LIF | Thomas J. Downey | 5008
BLANK | 27283
TOTAL | 232997
D 3 | NASSAU
DEM | Steve A. Orlins | 116915
REP | Peter T. King | 108574
CON | Peter T. King | 16153
RTL | Louis P. Roccanova | 6888
LIB | Ben-Zion J. Heyman | 3092
BLANK | 42017
TOTAL | 293639
D 4 | NASSAU
DEM | Philip M. Schiliro | 97007
REP | David A. Levy | 98723
CON | David A. Levy | 11987
RTL | Vincent P. Garbitelli | 9548
LIB | Philip M. Schiliro | 3379
BLANK | 41168
TOTAL | 261812
D 5 | NASSAU, SUFFOLK, QUEENS
DEM | Gary L. Ackerman | 30421 25533 49999 105953
REP | Allan E. Binder | 19532 35541 27810 82883
CON | Allan E. Binder | 1745 5479 4800 12024
RTL | Andrew J. Duff | 938 3072 1438 5448
LIB | Gary L. Ackerman | 1137 998 2388 4523
BLANK | 10069 12646 23009 45724
TOTAL | 63842 83269 109444 256555
D 6 | QUEENS
DEM | Floyd H. Flake | 96972
REP | Dianand D. Bhagwandin | 18725
CON | Dianand D. Bhagwandin | 3962
BLANK | 40569
TOTAL | 160228
D 7 | BRONX, QUEENS
DEM | Thomas J. Manton | 16838 55442 72280
REP | Dennis C. Shea | 13061 33157 46218
CON | Dennis C. Shea | 2535 5886 8421
BLANK | 13759 30446 44205
TOTAL | 46193 124931 171124
D 8 | KINGS, NEW YORK
DEM | Jerrold Nadler | 29491 102681 132172
REP | David L. Askren | 8857 16691 25548
CON | Margaret V. Byrnes | 2019 3161 5180
LIB | Jerrold Nadler | 961 5163 6124
NAP | Arthur R. Block | 194 1030 1224
BLANK | 21376 35961 57337
TOTAL | 62898 164687 227585
D 9 | KINGS, QUEENS
DEM | Charles E. Schumer | 71980 39444 111424
CON | Alice G. Gaffney | 7009 7976 14985
LIB | Charles E. Schumer | 3329 1792 5121
BLANK | 41606 40068 81674
TOTAL | 123924 89280 213204
D 10 | KINGS
DEM | Edolphus Towns | 93801
CON | Owen Augustin | 4315
LIB | Edolphus Towns | 3708
BLANK | 58291
TOTAL | 160115
D 11 | KINGS
DEM | Major R. Owens | 76724
CON | Michael Gaffney | 4287
LIB | Major R. Owens | 3304
NAP | Ernest N. Foster | 1179
BLANK | 42657
TOTAL | 128151
D 12 | KINGS, NEW YORK, QUEENS
DEM | Nydia Velazquez | 30953 15680 9293 55926
REP | Angel Diaz | 5761 2727 3800 12288
CON | Angel Diaz | 830 262 443 1535
RTL | Angel Diaz | 582 291 280 1153
LIB | Ruben Franco | 718 472 366 1556
NAP | Rafael Mendez | 263 208 138 609
BLANK | 17035 7910 6473 31418
TOTAL | 56142 27550 20793 104485
D 13 | RICHMOND, KINGS
DEM | Sal F. Albanese | 42176 26562 68738
REP | Susan Molinari | 70693 21451 92144
CON | Susan Molinari | 12676 3083 15759
RTL | Kathleen M. Murphy | 8260 2565 10825
LIB | Sal F. Albanese | 2942 1840 4782
BLANK | 15568 9326 24894
TOTAL | 152315 64827 217142
D 14 | KINGS, NEW YORK, QUEENS
DEM | Carolyn B. Maloney | 5340 82849 8870 97059
REP | Bill Green | 3491 83710 4833 92034
LIB | Carolyn B. Maloney | 378 3778 437 4593
INN | Bill Green | 225 4772 184 5181
INF | Abraham J. Hirschfeld | 163 2665 142 2970
BLANK | 3494 28309 5080 36883
TOTAL | 13091 206083 19546 238720
D 15 | BRONX, NEW YORK
DEM | Charles B. Rangel | 0 101229 101229
CON | Jose A. Suero | 0 3425 3425
LIB | Charles B. Rangel | 0 3782 3782
NAP | Jessie Fields | 0 1337 1337
INF | Jose A. Suero | 0 920 920
BLANK | 0 43665 43665
TOTAL | 0 154358 154358
D 16 | BRONX
DEM | Jose E. Serrano | 80927
REP | Michael Walters | 6741
CON | Michael Walters | 1234
LIB | Jose E. Serrano | 4295
BLANK | 38885
TOTAL | 132082
D 17 | BRONX, WESTCHESTER
DEM | Eliot L. Engel | 75661 19097 94758
REP | Martin Richman | 12476 4035 16511
CON | Kevin Brawley | 2435 708 3143
RTL | Martin J. O'Grady | 2302 765 3067
LIB | Eliot L. Engel | 2731 579 3310
NLP | Nana LaLuz | 1419 173 1592
BLANK | 36625 7351 43976
TOTAL | 133649 32708 166357
D 18 | BRONX, QUEENS, WESTCHESTER
DEM | Nita M. Lowey | 2293 28237 85311 115841
REP | Joseph J. DioGuardi | 3199 9858 61019 74076
CON | Joseph J. DioGuardi | 681 1328 9018 11027
RTL | Joseph J. DioGuardi | 212 613 6759 7584
BLANK | 1949 12816 19764 34529
TOTAL | 8334 52852 181871 243057
D 19 | PUTNAM, DUTCHESS, ORANGE, WESTCHESTER
DEM | Neil McCarthy | 12273 22925 5479 52177 92854
REP | Hamilton Fish, Jr. | 19031 37257 9678 53081 119047
CON | Hamilton Fish, Jr. | 4854 5780 1628 8301 20563
BLANK | 7177 8378 2713 16559 34827
TOTAL | 43335 74340 19498 130118 267291
D 20 | ROCKLAND, ORANGE, SULLIVAN, WESTCHESTER
DEM | Jonathan L. Levine | 30204 17315 2535 16772 66826
REP | Benjamin A. Gilman | 71053 52940 6772 19536 150301
RTL | Robert F. Garrison | 4763 3778 408 1255 10204
BLANK | 17047 11870 1894 6869 37680
TOTAL | 123067 85903 11609 44432 265011
D 21 | ALBANY, SCHENECTADY, MONTGOMERY, RENSSELAER, SARATOGA
DEM | Michael R. McNulty | 85697 33520 8711 19446 1945 149319
REP | Nancy Norman | 42888 22504 5752 11299 1402 83845
CON | Michael R. McNulty | 7380 5457 1122 2722 371 17052
RTL | William J. Donnelly | 3574 2046 550 1384 169 7723
LIB | Nancy Norman | 3918 2030 397 899 95 7339
BLANK | 14014 9628 3289 4320 477 31728
TOTAL | 157471 75185 19821 40070 4459 297006
D 22 | COLUMBIA, GREENE, WARREN, WASHINGTON, DUTCHESS, ESSEX, RENSSELAER, SARATOGA, SCHOHARIE
DEM | David Roberts | 8924 5782 8851 7747 11237 3725 10428 28051 2151 86896
REP | Gerald B.H. Solomon | 13759 10744 15115 12982 16341 5932 17413 41771 2852 136909
CON | Gerald B.H. Solomon | 2058 1434 1508 1427 2160 559 2244 4324 371 16085
RTL | Gerald B.H. Solomon | 1112 964 1130 1058 1551 485 1596 3300 246 11442
BLANK | 3485 2487 2350 2222 5455 3107 4015 9159 884 33164
TOTAL | 29338 21411 28954 25436 36744 13808 35696 86605 6504 284496
D 23 | CHENANGO, MADISON, ONEIDA, OTSEGO, BROOME, DELAWARE, HERKIMER, MONTGOMERY, SCHOHARIE
DEM | Paula DiPerna | 4645 7598 26136 7998 2010 4054 6468 947 1979 61835
REP | Sherwood L. Boehlert | 12707 13951 65459 13580 4635 9639 13911 1916 3976 139774
CON | Geoffrey P. Grace | 808 1045 2705 997 248 961 724 97 426 8011
RTL | Randall A. Terry | 757 1157 3655 834 483 640 764 142 256 8688
NLP | Ted F. Janowski | 121 193 546 113 51 91 160 32 47 1354
BLANK | 2843 5370 11188 3528 1478 2798 4047 831 1135 33218
TOTAL | 21881 29314 109689 27050 8905 18183 26074 3965 7819 252880
D 24 | CLINTON, FRANKLIN, FULTON, HAMILTON, JEFFERSON, LEWIS, OSWEGO, ST. LAWRENCE, ESSEX, HERKIMER
DEM | Margaret M. Ravenscroft | 8182 5023 5613 532 6026 1993 9396 9007 1090 813 47675
REP | John M. McHugh | 13742 7345 8520 1242 22671 6421 27250 22079 2470 1668 113408
CON | Morrison J. Hosley, Jr. | 2391 1828 2489 1210 2360 651 3170 2607 406 372 17484
RTL | Morrison J. Hosley, Jr. | 1497 990 1111 380 1297 436 1765 1417 209 177 9279
LIB | Stephen Burke | 613 447 357 52 529 121 771 1343 78 63 4374
VRP | John M. McHugh | 624 368 650 62 2088 318 2415 2088 127 109 8849
BLANK | 5286 2729 4508 415 2792 1283 6632 4328 999 1005 29977
TOTAL | 32335 18730 23248 3893 37763 11223 51399 42869 5379 4207 231046
D 25 | CORTLAND, ONONDAGA, BROOME, CAYUGA, TIOGA
DEM | Rhea Jezer | 7882 84267 1011 7485 777 101422
REP | James T. Walsh | 9212 98137 2089 8534 1310 119282
CON | James T. Walsh | 1183 12528 360 1566 157 15794
CSP | Rhea Jezer | 468 4767 66 518 69 5888
BLANK | 2358 16816 940 2963 570 23647
TOTAL | 21103 216515 4466 21066 2883 266033
D 26 | ULSTER, TIOGA, BROOME, TOMPKINS, DELAWARE, ORANGE, DUTCHESS, SULLIVAN
DEM | Maurice D. Hinchey | 39367 6187 32823 15839 689 6805 2058 8995 112763
REP | Bob Moppert | 23975 10300 41392 8226 1161 5866 1685 5784 98389
CON | Bob Moppert | 4806 992 3524 760 196 819 219 1033 12349
RTL | Mary C. Dixon | 2298 652 1671 565 84 793 192 566 6821
LIB | Maurice D. Hinchey | 3080 385 1439 821 63 418 94 494 6794
BLANK | 8014 1856 7498 3882 407 3342 921 3704 29624
TOTAL | 81540 20372 88347 30093 2600 18043 5169 20576 266740
D 27 | GENESEE, LIVINGSTON, ONTARIO, WAYNE, WYOMING, ERIE, SENECA, CAYUGA, MONROE
DEM | W. Douglas Call | 12177 8613 14570 9805 4571 27925 3062 612 8571 89906
REP | Bill Paxon | 9768 12883 21119 19020 8419 38849 4456 626 11857 126997
CON | Bill Paxon | 1293 1783 2825 2708 1200 5010 570 108 1889 17386
RTL | Bill Paxon | 1195 977 1544 1686 1082 4318 489 67 855 12213
BLANK | 2052 2804 5184 6411 1245 7406 2159 559 3017 30837
TOTAL | 26485 27060 45242 39630 16517 83508 10736 1972 26189 277339
D 28 | MONROE
DEM | Louise M. Slaughter | 140908
REP | William P. Polito | 93806
CON | William P. Polito | 18467
EJP | Keith R. T. Perez | 1897
BLANK | 20248
TOTAL | 275326
D 29 | NIAGARA, ORLEANS, ERIE, MONROE
DEM | John J. LaFalce | 42119 6933 54139 17567 120758
REP | William E. Miller, Jr. | 35864 6009 30155 13266 85294
CON | William E. Miller, Jr. | 5276 783 4241 2437 12737
RTL | Kenneth J. Kowalski | 2888 475 2595 1409 7367
LIB | John J. LaFalce | 2505 555 3465 947 7472
EJP | John A. Basar, Jr. | 692 136 705 297 1830
BLANK | 8144 2023 12530 6371 29068
TOTAL | 97488 16914 107830 42294 264526
D 30 | ERIE
DEM | Dennis T. Gorski | 102519
REP | Jack Quinn | 114921
CON | Dennis T. Gorski | 8926
RTL | Mary F. Refermat | 6025
CCP | Jack Quinn | 10813
BLANK | 21893
TOTAL | 265097
D 31 | ALLEGANY, CATTARAUGUS, CHAUTAUQUA, CHEMUNG, SCHUYLER, STEUBEN, YATES, CAYUGA, SENECA, TOMPKINS
DEM | Joseph P. Leahey | 2994 9081 15712 6885 1651 5806 2126 3253 1059 3443 52010
REP | Amo Houghton | 11395 17130 31410 24344 4538 26828 5289 5253 2147 5424 133758
CON | Amo Houghton | 1448 2436 4501 2541 718 2798 777 897 254 568 16938
RTL | Gretchen S. McManus | 929 1800 2262 1610 346 2286 452 529 178 456 10848
BLANK | 2232 4989 9398 4461 1045 4079 1531 3185 743 1977 33640
TOTAL | 18998 35436 63283 39841 8298 41797 10175 13117 4381 11868 247194
""",
"1994": """
D 1 | SUFFOLK
DEM | George J. Hochbrueckner | 78692
REP | Michael P. Forbes | 72045
CON | Michael P. Forbes | 10146
RTL | Michael P. Forbes | 8300
LIB | George J. Hochbrueckner | 1454
FUP | Michael Strong | 1603
BLANK | 11793
TOTAL | 184033
D 2 | SUFFOLK
DEM | James L. Manfre | 40358
REP | Rick A. Lazio | 86857
CON | Rick A. Lazio | 13250
RTL | Alice Cort Ross | 5567
LIB | James L. Manfre | 744
BLANK | 15770
TOTAL | 162546
D 3 | NASSAU
DEM | Norma Grill | 77774
REP | Peter T. King | 98628
CON | Peter T. King | 16608
LIB | John A. DePrima | 1522
BLANK | 24240
TOTAL | 218772
D 4 | NASSAU
DEM | Philip M. Schiliro | 65286
REP | Daniel Frisa | 87815
CON | David A. Levy | 15173
LIB | Robert S. Berkowitz | 1409
RTL | Vincent P. Garbitelli | 5280
BLANK | 21727
TOTAL | 196690
D 5 | NASSAU, SUFFOLK, QUEENS
DEM | Gary L. Ackerman | 25902 21742 41937 89581
REP | Grant M. Lally | 14500 27293 21872 63665
CON | Grant M. Lally | 1658 5008 3553 10219
LIB | Gary L. Ackerman | 1008 762 2545 4315
RTL | Edward Elkowitz | 514 1665 683 2862
BLANK | 3545 4194 12394 20133
TOTAL | 47127 60664 82984 190775
D 6 | QUEENS
DEM | Floyd H. Flake | 68596
REP | Denny D. Bhagwandin | 13956
CON | Denny D. Bhagwandin | 2719
BLANK | 26719
TOTAL | 111990
D 7 | BRONX, QUEENS
DEM | Thomas J. Manton | 15260 43675 58935
CON | Robert E. Hurley | 2132 6566 8698
BLANK | 16734 39518 56252
TOTAL | 34126 89759 123885
D 8 | KINGS, NEW YORK
DEM | Jerrold L. Nadler | 24494 78774 103268
REP | David L. Askren | 7742 13390 21132
CON | Margaret V. Byrnes | 1309 1699 3008
LIB | Jerrold L. Nadler | 902 5776 6678
BLANK | 13775 13364 27139
TOTAL | 48222 113003 161225
D 9 | QUEENS, KINGS
DEM | Charles E. Schumer | 33755 57028 90783
REP | James P. McCall | 14730 15641 30371
CON | James P. McCall | 2830 2679 5509
LIB | Charles E. Schumer | 1829 2527 4356
BLANK | 13822 13966 27788
TOTAL | 66966 91841 158807
D 10 | KINGS
DEM | Edolphus Towns | 74264
REP | Amelia Smith Parker | 7995
CON | Mildred K. Mahoney | 1489
LIB | Edolphus Towns | 2762
BLANK | 29682
TOTAL | 116192
D 11 | KINGS
DEM | Major R. Owens | 59850
REP | Gary S. Popkin | 6311
CON | Michael Gaffney | 1150
LIB | Major R. Owens | 2095
LBT | Gary S. Popkin | 294
BLANK | 22551
TOTAL | 92251
D 12 | KINGS, NEW YORK, QUEENS
DEM | Nydia M. Velazquez | 20819 10654 5849 37322
CON | Genevieve R. Brennan | 1244 514 989 2747
LIB | Nydia M. Velazquez | 1623 643 341 2607
PHA | Eric Ruano-Melendez | 272 179 138 589
BLANK | 12142 6133 5897 24172
TOTAL | 36100 18123 13214 67437
D 13 | RICHMOND, KINGS
DEM | Tyrone G. Butler | 21806 10254 32060
REP | Susan Molinari | 60244 23265 83509
CON | Susan Molinari | 9899 3083 12982
LIB | Tyrone G. Butler | 1371 506 1877
RTL | Elisa Disimone | 3665 990 4655
BLANK | 13429 10055 23484
TOTAL | 110414 48153 158567
D 14 | KINGS, NEW YORK, QUEENS
DEM | Carolyn B. Maloney | 4536 85462 6797 96795
REP | Charles Millard | 2565 43814 3911 50290
LIB | Charles Millard | 151 3628 208 3987
TBA | Thomas K. Leighton | 45 492 29 566
IDN | Carolyn B. Maloney | 155 1332 197 1684
BLANK | 1739 11415 2871 16025
TOTAL | 9191 146143 14013 169347
D 15 | NEW YORK
DEM | Charles B. Rangel | 74566
LIB | Charles B. Rangel | 3264
RTL | Jose Suero | 2013
IFP | Jose Suero | 799
BLANK | 27960
TOTAL | 108602
D 16 | BRONX
DEM | Jose E. Serrano | 57157
LIB | Jose E. Serrano | 1415
CON | Michael Walters | 2257
BLANK | 24690
TOTAL | 85519
D 17 | BRONX, WESTCHESTER
DEM | Eliot L. Engel | 58122 12364 70486
REP | Edward T. Marshall | 12469 4427 16896
CON | Kevin Brawley | 1655 532 2187
LIB | Eliot L. Engel | 2454 381 2835
RTL | Ann M. Noonan | 1754 321 2075
BLANK | 19354 5154 24508
TOTAL | 95808 23179 118987
D 18 | BRONX, QUEENS, WESTCHESTER
DEM | Nita M. Lowey | 2439 23040 66184 91663
REP | Andrew C. Hartzell, Jr. | 2082 6975 46579 55636
CON | Andrew C. Hartzell, Jr. | 447 946 8488 9881
RTL | Florence T. O'Grady | 110 425 2338 2873
BLANK | 1327 7483 13687 22497
TOTAL | 6405 38869 137276 182550
D 19 | PUTNAM, DUTCHESS, ORANGE, WESTCHESTER
DEM | Hamilton Fish, Jr. | 9739 17833 4427 38697 70696
REP | Sue W. Kelly | 17223 29418 8010 45522 100173
CON | Joseph J. DioGuardi | 2446 3474 746 7221 13887
RTL | Joseph J. DioGuardi | 1084 1627 502 2661 5874
ATP | Catherine Portman-Laux | 440 365 180 694 1679
BLANK | 2750 3909 1139 8582 16380
TOTAL | 33682 56626 15004 103377 208689
D 20 | ROCKLAND, ORANGE, SULLIVAN, WESTCHESTER
DEM | Gregory B. Julian | 24664 12786 1845 13050 52345
REP | Benjamin A. Gilman | 55265 43705 6128 15236 120334
RTL | Lois M. Colandrea | 2616 2111 214 671 5612
BLANK | 11089 7502 904 5423 24918
TOTAL | 93634 66104 9091 34380 203209
D 21 | ALBANY, SCHENECTADY, MONTGOMERY, RENSSELAER, SARATOGA
DEM | Michael R. McNulty | 75720 30760 7207 16550 1679 131916
REP | Joseph A. Gomez | 35086 18197 4556 9675 1231 68745
CON | Michael R. McNulty | 6582 4896 1327 2704 379 15888
RTL | Timothy J. Wood | 1942 1021 305 742 115 4125
BLANK | 9749 6053 2602 2635 292 21331
TOTAL | 129079 60927 15997 32306 3696 242005
D 22 | COLUMBIA, GREENE, WARREN, WASHINGTON, DUTCHESS, ESSEX, RENSSELAER, SARATOGA, SCHOHARIE
DEM | L. Robert Lawrence, Jr. | 6782 4220 5490 4611 7049 2219 7604 17700 1389 57064
REP | Gerald B.H. Solomon | 13186 10733 13731 12067 15357 5778 16150 39534 3043 129579
CON | Gerald B.H. Solomon | 2807 1791 1907 1936 2546 559 3250 5777 557 21130
RTL | Gerald B.H. Solomon | 659 602 644 643 892 386 1009 2013 160 7008
BLANK | 1908 1662 1405 1371 3057 2385 2301 5633 413 20135
TOTAL | 25342 19008 23177 20628 28901 11327 30314 70657 5562 234916
D 23 | CHENANGO, MADISON, ONEIDA, OTSEGO, BROOME, DELAWARE, HERKIMER, MONTGOMERY, SCHOHARIE
DEM | Charles W. Skeele, Jr. | 2973 5222 17881 4712 1174 2753 4189 593 1289 40786
REP | Sherwood L. Boehlert | 11574 13035 54655 13385 4614 9737 11429 1826 4231 124486
RTL | Donald J. Thomas | 789 1154 5918 732 404 579 1321 88 231 11216
BLANK | 2291 4229 10873 2849 1098 2170 4195 774 721 29200
TOTAL | 17627 23640 89327 21678 7290 15239 21134 3281 6472 205688
D 24 | CLINTON, FRANKLIN, FULTON, HAMILTON, JEFFERSON, LEWIS, OSWEGO, ST. LAWRENCE, ESSEX, HERKIMER
DEM | Danny M. Francis | 6373 3876 3542 471 4593 1389 6032 6335 910 511 34032
REP | John M. McHugh | 13541 7957 9817 2076 19686 5755 24184 20839 2503 1920 108278
CON | John M. McHugh | 1459 789 1506 294 3064 774 5264 2628 237 352 16367
BLANK | 4026 2587 4044 600 2537 1163 5712 3884 870 779 26202
TOTAL | 25399 15209 18909 3441 29880 9081 41192 33686 4520 3562 184879
D 25 | CORTLAND, ONONDAGA, BROOME, CAYUGA, TIOGA
DEM | Rhea Jezer | 5715 69066 688 5727 514 81710
REP | James T. Walsh | 8219 76981 2077 7706 1380 96363
CON | James T. Walsh | 1374 14059 271 1722 160 17586
CCP | Rhea Jezer | 412 1166 63 446 56 2143
BLANK | 977 9123 519 1054 272 11945
TOTAL | 16697 170395 3618 16655 2382 209747
D 26 | ULSTER, TIOGA, BROOME, TOMPKINS, DELAWARE, ORANGE, DUTCHESS, SULLIVAN
DEM | Maurice D. Hinchey | 27098 5763 33908 10137 641 5138 1583 6584 90852
REP | Bob Moppert | 27509 8056 25712 6683 1034 5821 1606 6496 82917
CON | Bob Moppert | 5216 863 2456 863 139 717 210 863 11327
RTL | Thomas F. Kovach | 1029 526 2283 238 82 275 86 253 4772
LIB | Maurice D. Hinchey | 1911 304 1227 581 54 203 72 288 4640
BLANK | 3091 921 3474 899 237 1882 475 1295 12274
TOTAL | 65854 16433 69060 19401 2187 14036 4032 15779 206782
D 27 | GENESEE, LIVINGSTON, ONTARIO, WAYNE, WYOMING, ERIE, SENECA, CAYUGA, MONROE
DEM | William A. Long, Jr. | 4450 4480 8371 5203 2405 20526 1921 370 4434 52160
REP | Bill Paxon | 11950 13096 19860 17691 8471 35165 4340 715 11304 122592
CON | Bill Paxon | 2008 2121 3705 4140 1152 4040 875 170 2633 20844
RTL | Bill Paxon | 1051 762 1223 1418 723 2806 380 52 759 9174
BLANK | 1582 1590 2911 2714 718 3046 952 295 1996 15804
TOTAL | 21041 22049 36070 31166 13469 65583 8468 1602 21126 220574
D 28 | MONROE
DEM | Louise M. Slaughter | 110987
REP | Renee Forgensi Davison | 61321
CON | Renee Forgensi Davison | 17195
IFP | John A. Clendenin | 6464
BLANK | 11307
TOTAL | 207274
D 29 | NIAGARA, ORLEANS, ERIE, MONROE
DEM | John J. LaFalce | 32972 5836 43931 14880 97619
REP | William E. Miller, Jr. | 30868 5208 23031 10183 69290
CON | William E. Miller, Jr. | 4022 993 3183 2867 11065
RTL | Patrick Murty | 1463 167 1116 550 3296
LIB | John J. LaFalce | 1536 449 2277 1172 5434
BLANK | 3497 803 4598 2779 7079
TOTAL | 74358 13456 78136 32431 193783
D 30 | ERIE
DEM | David A. Franczyk | 58577
REP | Jack Quinn | 106551
CON | Jack Quinn | 18187
LIB | David A. Franczyk | 2815
BLANK | 16096
TOTAL | 202226
D 31 | ALLEGANY, CATTARAUGUS, CHAUTAUQUA, CHEMUNG, SCHUYLER, STEUBEN, YATES, CAYUGA, SENECA, TOMPKINS
REP | Amo Houghton | 8763 13692 24273 19319 3891 20333 4724 5224 1978 4701 106898
CON | Amo Houghton | 1006 2207 3208 1762 592 2302 872 1177 441 713 14280
RTL | Gretchen S. McManus | 2151 4134 3941 3088 757 5108 662 987 300 619 21747
BLANK | 3801 8493 17828 6654 1404 5835 1879 2900 873 3369 53036
TOTAL | 15721 28526 49250 30823 6644 33578 8137 10288 3592 9402 195961
""",
}

# Official candidate totals (all lines), keyed by district then candidate, as checked against the Clerk (1992, 1994) / FEC (1990)
OFFICIAL = {
 "1990": {
  "1": {
   "Clayton Baldwin, Jr.": 6883,
   "Francis W. Creighton": 46380,
   "George J. Hochbrueckner": 75211,
   "Peter J. O'Hara": 5111
  },
  "2": {
   "Dominic A. Curcio": 8150,
   "John W. Bugler": 36859,
   "Thomas J. Downey": 56722
  },
  "3": {
   "Francis A. Dreger": 4915,
   "Robert J. Mrazek": 73029,
   "Robert Previdi": 59089
  },
  "4": {
   "Ben-Zion J. Heyman": 2343,
   "Francis T. Goban": 41308,
   "John J. Dunkle": 6706,
   "Norman F. Lent": 79304
  },
  "5": {
   "Edward K. Kitt": 6000,
   "Mark S. Epstein": 53920,
   "Raymond J. McGrath": 71948
  },
  "6": {
   "Floyd H. Flake": 44306,
   "John Cronin": 3111,
   "William Sampol": 13224
  },
  "7": {
   "Gary L. Ackerman": 51091
  },
  "8": {
   "Gustave Reifenkugel": 21646,
   "James H. Scheuer": 56396
  },
  "9": {
   "Ann Pfoser Darby": 13330,
   "Thomas J. Manton": 35177,
   "Thomas V. Ognibene": 6137
  },
  "10": {
   "Charles E. Schumer": 61468,
   "Patrick J. Kinsella": 14963
  },
  "11": {
   "Edolphus Towns": 36286,
   "Ernest Johnson": 1676,
   "Lorraine Stevens": 1094
  },
  "12": {
   "Joseph Caesar": 1159,
   "Major R. Owens": 40570,
   "Mamie Moore": 1021
  },
  "13": {
   "Edwin Ramos": 11557,
   "Stephen J. Solarz": 47446
  },
  "14": {
   "Anthony J. Pocchia": 34625,
   "Christine Sacchi": 4370,
   "Susan Molinari": 58616
  },
  "15": {
   "Bill Green": 52919,
   "Frances L. Reiter": 33464,
   "Michael T. Berns": 3654
  },
  "16": {
   "Alvaader Frazier": 1592,
   "Charles B. Rangel": 55882
  },
  "17": {
   "John Patterson": 1087,
   "Mark Goret": 2928,
   "Ted Weiss": 79161,
   "William W. Koeppel": 15219
  },
  "18": {
   "Anna Johnson": 717,
   "Jose E. Serrano": 38024,
   "Joseph Chiavaro": 1189,
   "Mary Rivera": 866
  },
  "19": {
   "Eliot L. Engel": 45758,
   "Kevin Brawley": 11868,
   "William J. Gouldman": 17135
  },
  "20": {
   "Glenn D. Bellitto": 35575,
   "John M. Schafer": 13030,
   "Nita M. Lowey": 82203
  },
  "21": {
   "Hamilton Fish, Jr.": 99866,
   "Richard L. Barbuto": 34128,
   "Richard S. Curtin": 5925
  },
  "22": {
   "Benjamin A. Gilman": 95495,
   "John G. Dow": 37034,
   "Margaret M. Beirne": 6656
  },
  "23": {
   "Margaret B. Buhrmaster": 65760,
   "Michael R. McNulty": 117239
  },
  "24": {
   "Bob Lawrence": 56671,
   "Gerald B. Solomon": 121206
  },
  "25": {
   "Sherwood L. Boehlert": 91348,
   "William L. Griffen": 17481
  },
  "26": {
   "David O'B. Martin": 97340
  },
  "27": {
   "James T. Walsh": 95220,
   "Peggy L. Murray": 52438,
   "Stephen K. Hoff": 3097
  },
  "28": {
   "Matt McHugh": 97815,
   "Seymour Krieger": 53077
  },
  "29": {
   "Alton F. Eber": 34835,
   "Donald M. Peters": 4878,
   "Frank Horton": 89105,
   "Peter DeMauro": 12599
  },
  "30": {
   "John M. Regan, Jr.": 67534,
   "Louise M. Slaughter": 97280
  },
  "31": {
   "Bill Paxon": 90237,
   "Kevin P. Gaughan": 69328
  },
  "32": {
   "John J. LaFalce": 68367,
   "Kenneth J. Kowalski": 16853,
   "Michael T. Waring": 39053
  },
  "33": {
   "Henry J. Nowak": 84905,
   "Louis P. Corrigan, Jr.": 6460,
   "Thomas K. Kepfer": 18181
  },
  "34": {
   "Amo Houghton": 89831,
   "Joseph P. Leahey": 37421,
   "Nevin K. Eklund": 1807
  }
 },
 "1992": {
  "1": {
   "Edward P. Romaine": 110043,
   "George J. Hochbrueckner": 117940
  },
  "2": {
   "Rick A. Lazio": 109386,
   "Thomas J. Downey": 96328
  },
  "3": {
   "Ben-Zion J. Heyman": 3092,
   "Louis P. Roccanova": 6888,
   "Peter T. King": 124727,
   "Steve A. Orlins": 116915
  },
  "4": {
   "David A. Levy": 110710,
   "Philip M. Schiliro": 100386,
   "Vincent P. Garbitelli": 9548
  },
  "5": {
   "Allan E. Binder": 94907,
   "Andrew J. Duff": 5448,
   "Gary L. Ackerman": 110476
  },
  "6": {
   "Dianand D. Bhagwandin": 22687,
   "Floyd H. Flake": 96972
  },
  "7": {
   "Dennis C. Shea": 54639,
   "Thomas J. Manton": 72280
  },
  "8": {
   "Arthur R. Block": 1224,
   "David L. Askren": 25548,
   "Jerrold Nadler": 138296,
   "Margaret V. Byrnes": 5180
  },
  "9": {
   "Alice G. Gaffney": 14985,
   "Charles E. Schumer": 116545
  },
  "10": {
   "Edolphus Towns": 97509,
   "Owen Augustin": 4315
  },
  "11": {
   "Ernest N. Foster": 1179,
   "Major R. Owens": 80028,
   "Michael Gaffney": 4287
  },
  "12": {
   "Angel Diaz": 14976,
   "Nydia Velazquez": 55926,
   "Rafael Mendez": 609,
   "Ruben Franco": 1556
  },
  "13": {
   "Kathleen M. Murphy": 10825,
   "Sal F. Albanese": 73520,
   "Susan Molinari": 107903
  },
  "14": {
   "Abraham J. Hirschfeld": 2970,
   "Bill Green": 97215,
   "Carolyn B. Maloney": 101652
  },
  "15": {
   "Charles B. Rangel": 105011,
   "Jessie Fields": 1337,
   "Jose A. Suero": 4345
  },
  "16": {
   "Jose E. Serrano": 85222,
   "Michael Walters": 7975
  },
  "17": {
   "Eliot L. Engel": 98068,
   "Kevin Brawley": 3143,
   "Martin J. O'Grady": 3067,
   "Martin Richman": 16511,
   "Nana LaLuz": 1592
  },
  "18": {
   "Joseph J. DioGuardi": 92687,
   "Nita M. Lowey": 115841
  },
  "19": {
   "Hamilton Fish, Jr.": 139610,
   "Neil McCarthy": 92854
  },
  "20": {
   "Benjamin A. Gilman": 150301,
   "Jonathan L. Levine": 66826,
   "Robert F. Garrison": 10204
  },
  "21": {
   "Michael R. McNulty": 166371,
   "Nancy Norman": 91184,
   "William J. Donnelly": 7723
  },
  "22": {
   "David Roberts": 86896,
   "Gerald B.H. Solomon": 164436
  },
  "23": {
   "Geoffrey P. Grace": 8011,
   "Paula DiPerna": 61835,
   "Randall A. Terry": 8688,
   "Sherwood L. Boehlert": 139774,
   "Ted F. Janowski": 1354
  },
  "24": {
   "John M. McHugh": 122257,
   "Margaret M. Ravenscroft": 47675,
   "Morrison J. Hosley, Jr.": 26763,
   "Stephen Burke": 4374
  },
  "25": {
   "James T. Walsh": 135076,
   "Rhea Jezer": 107310
  },
  "26": {
   "Bob Moppert": 110738,
   "Mary C. Dixon": 6821,
   "Maurice D. Hinchey": 119557
  },
  "27": {
   "Bill Paxon": 156596,
   "W. Douglas Call": 89906
  },
  "28": {
   "Keith R. T. Perez": 1897,
   "Louise M. Slaughter": 140908,
   "William P. Polito": 112273
  },
  "29": {
   "John A. Basar, Jr.": 1830,
   "John J. LaFalce": 128230,
   "Kenneth J. Kowalski": 7367,
   "William E. Miller, Jr.": 98031
  },
  "30": {
   "Dennis T. Gorski": 111445,
   "Jack Quinn": 125734,
   "Mary F. Refermat": 6025
  },
  "31": {
   "Amo Houghton": 150696,
   "Gretchen S. McManus": 10848,
   "Joseph P. Leahey": 52010
  }
 },
 "1994": {
  "1": {
   "George J. Hochbrueckner": 80146,
   "Michael P. Forbes": 90491,
   "Michael Strong": 1603
  },
  "2": {
   "Alice Cort Ross": 5567,
   "James L. Manfre": 41102,
   "Rick A. Lazio": 100107
  },
  "3": {
   "John A. DePrima": 1522,
   "Norma Grill": 77774,
   "Peter T. King": 115236
  },
  "4": {
   "Daniel Frisa": 87815,
   "David A. Levy": 15173,
   "Philip M. Schiliro": 65286,
   "Robert S. Berkowitz": 1409,
   "Vincent P. Garbitelli": 5280
  },
  "5": {
   "Edward Elkowitz": 2862,
   "Gary L. Ackerman": 93896,
   "Grant M. Lally": 73884
  },
  "6": {
   "Denny D. Bhagwandin": 16675,
   "Floyd H. Flake": 68596
  },
  "7": {
   "Robert E. Hurley": 8698,
   "Thomas J. Manton": 58935
  },
  "8": {
   "David L. Askren": 21132,
   "Jerrold L. Nadler": 109946,
   "Margaret V. Byrnes": 3008
  },
  "9": {
   "Charles E. Schumer": 95139,
   "James P. McCall": 35880
  },
  "10": {
   "Amelia Smith Parker": 7995,
   "Edolphus Towns": 77026,
   "Mildred K. Mahoney": 1489
  },
  "11": {
   "Gary S. Popkin": 6605,
   "Major R. Owens": 61945,
   "Michael Gaffney": 1150
  },
  "12": {
   "Eric Ruano-Melendez": 589,
   "Genevieve R. Brennan": 2747,
   "Nydia M. Velazquez": 39929
  },
  "13": {
   "Elisa Disimone": 4655,
   "Susan Molinari": 96491,
   "Tyrone G. Butler": 33937
  },
  "14": {
   "Carolyn B. Maloney": 98479,
   "Charles Millard": 54277,
   "Thomas K. Leighton": 566
  },
  "15": {
   "Charles B. Rangel": 77830,
   "Jose Suero": 2812
  },
  "16": {
   "Jose E. Serrano": 58572,
   "Michael Walters": 2257
  },
  "17": {
   "Ann M. Noonan": 2075,
   "Edward T. Marshall": 16896,
   "Eliot L. Engel": 73321,
   "Kevin Brawley": 2187
  },
  "18": {
   "Andrew C. Hartzell, Jr.": 65517,
   "Florence T. O'Grady": 2873,
   "Nita M. Lowey": 91663
  },
  "19": {
   "Catherine Portman-Laux": 1679,
   "Hamilton Fish, Jr.": 70696,
   "Joseph J. DioGuardi": 19761,
   "Sue W. Kelly": 100173
  },
  "20": {
   "Benjamin A. Gilman": 120334,
   "Gregory B. Julian": 52345,
   "Lois M. Colandrea": 5612
  },
  "21": {
   "Joseph A. Gomez": 68745,
   "Michael R. McNulty": 147804,
   "Timothy J. Wood": 4125
  },
  "22": {
   "Gerald B.H. Solomon": 157717,
   "L. Robert Lawrence, Jr.": 57064
  },
  "23": {
   "Charles W. Skeele, Jr.": 40786,
   "Donald J. Thomas": 11216,
   "Sherwood L. Boehlert": 124486
  },
  "24": {
   "Danny M. Francis": 34032,
   "John M. McHugh": 124645
  },
  "25": {
   "James T. Walsh": 113949,
   "Rhea Jezer": 83853
  },
  "26": {
   "Bob Moppert": 94244,
   "Maurice D. Hinchey": 95492,
   "Thomas F. Kovach": 4772
  },
  "27": {
   "Bill Paxon": 152610,
   "William A. Long, Jr.": 52160
  },
  "28": {
   "John A. Clendenin": 6464,
   "Louise M. Slaughter": 110987,
   "Renee Forgensi Davison": 78516
  },
  "29": {
   "John J. LaFalce": 103053,
   "Patrick Murty": 3296,
   "William E. Miller, Jr.": 80355
  },
  "30": {
   "David A. Franczyk": 61392,
   "Jack Quinn": 124738
  },
  "31": {
   "Amo Houghton": 121178,
   "Gretchen S. McManus": 21747
  }
 }
}

ALLOWED_ROW_MISMATCH = {("1994", 29, "BLANK"), ("1994", 29, "TOTAL")}


def parse(txt):
    ds, cur = [], None
    for ln in txt.strip().splitlines():
        p = [x.strip() for x in ln.split("|")]
        if p[0].startswith("D "):
            cur = dict(d=int(p[0][2:]), counties=[c.strip() for c in p[1].split(",")], lines=[])
            ds.append(cur)
        elif p[0] in ("BLANK", "TOTAL"):
            cur[p[0].lower()] = [int(x) for x in p[1].split()]
        else:
            cur["lines"].append((p[0], p[1], [int(x) for x in p[2].split()]))
    return ds


counties_all = set()
with open(os.path.join(HERE, "../data/raw_election/countypres_2000-2024.tab")) as f:
    for r in csv.DictReader(f, delimiter="\t"):
        if r["state"] == "NEW YORK":
            counties_all.add(re.sub(r"[^A-Z]", "", r["county_name"].upper()))
assert len(counties_all) == 62, len(counties_all)

for yr, txt in DATA.items():
    ds = parse(txt)
    rows = []
    for d in ds:
        n = len(d["counties"]); w = n + 1 if n > 1 else 1
        allrows = [(p, v) for p, _, v in d["lines"]] + [("BLANK", d["blank"]), ("TOTAL", d["total"])]
        for p, v in allrows:
            assert len(v) == w, (yr, d["d"], p, v)
            if n > 1 and sum(v[:n]) != v[n]:
                assert (yr, d["d"], p) in ALLOWED_ROW_MISMATCH, ("row", yr, d["d"], p, v)
        for j in range(w):
            assert sum(v[j] for _, _, v in d["lines"]) + d["blank"][j] == d["total"][j], ("column", yr, d["d"], j)
        cand = defaultdict(lambda: [0] * n); lines = defaultdict(set)
        for p, c, v in d["lines"]:
            lines[c].add(p)
            for j in range(n):
                cand[c][j] += v[j]
        tot = {c: sum(v) for c, v in cand.items()}
        assert tot == OFFICIAL[yr][str(d["d"])], ("official", yr, d["d"], tot)
        for c, v in cand.items():
            code = "D" if "DEM" in lines[c] else "R" if "REP" in lines[c] else "I"
            for j, cty in enumerate(d["counties"]):
                if v[j] > 0:
                    rows.append((int(yr), d["d"], cty, c, code, v[j]))
    seen = {re.sub(r"[^A-Z]", "", r[2]) for r in rows}
    assert seen == counties_all, (yr, counties_all - seen)
    with open(os.path.join(OUT, f"new_york_house_county_{yr}.csv"), "w", newline="") as f:
        wr = csv.writer(f)
        wr.writerow(["year", "district", "county", "candidate", "party_code", "votes"])
        wr.writerows(rows)
    print(yr, len(ds), "districts,", len(rows), "candidate-county rows, all checks pass")
