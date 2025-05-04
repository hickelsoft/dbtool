unit Deutschland_Feiertage;

// Basierend auf:
// https://www.swissdelphicenter.ch/de/showcode.php?id=1278
// Angepasst durch Daniel Marschall, ViaThinkSoft,
// um alle Bundesländer korrekt zu behandeln
// Aktuelle Version unter https://www.viathinksoft.de/codelib/207
// Version: 14.08.2023

// Hinweise:
// - Diese Unit arbeitet mit Jahreszahlen nach 1584
// - Die Verwendung dieses Codes erfolgt unter Ausschluss jeglicher Gewährleistung!
// Es kann einige wenige Gemeinden oder Ortsteile geben, bei denen Feiertage anders behandelt werden

interface

uses
  Windows, SysUtils;

type
  TFeiertag = record
    Date: TDateTime;
    Name: string;
  end;

  TFeiertagTable = array of TFeiertag;

function IstDeutscherFeiertag(tag: TDateTime; plz: integer): boolean;
function DeutschlandFeiertage(Jahr: Word; plz: integer): TFeiertagTable;

implementation

uses
  DateUtils;

{$REGION 'Bundesländer'}
// Wie wurden die Bundesländer extrahiert?
// 1. Spalten von https://cebus.net/de/plz-bundesland.htm in Plaintext-Datei kopiert
// 2. Whitespace mit Tab ersetzen, mit TextPad RegEx:
// Suche:   " "
// Ersetze: "\t"
// 3. In Excel einfügen
// 4. In Excel sortieren nach Name (Spalte 2), Markierung erweitern
// 5. PLZ-Range (erste Spalte) aus Excel in TextPad kopieren
// 6. Delphi Code mit folgendem TextPad RegEx erzeugen:
// Suche:   "(.+)\-(.+)"
// Ersetze: "    \(\(plz >= \1\) and \(plz <= \2\)\) or // \1-\2"

function IstBadenWuerttemberg(plz: integer): boolean;
begin
  // Extrahiert von https://cebus.net/de/plz-bundesland.htm
  result := ((plz >= 63928) and (plz <= 63928)) or // 63928-63928
    ((plz >= 64754) and (plz <= 64754)) or // 64754-64754
    ((plz >= 68001) and (plz <= 68312)) or // 68001-68312
    ((plz >= 68520) and (plz <= 68549)) or // 68520-68549
    ((plz >= 68701) and (plz <= 69234)) or // 68701-69234
    ((plz >= 69240) and (plz <= 69429)) or // 69240-69429
    ((plz >= 69434) and (plz <= 69434)) or // 69434-69434
    ((plz >= 69435) and (plz <= 69469)) or // 69435-69469
    ((plz >= 69489) and (plz <= 69502)) or // 69489-69502
    ((plz >= 69510) and (plz <= 69514)) or // 69510-69514
    ((plz >= 70001) and (plz <= 74592)) or // 70001-74592
    ((plz >= 74594) and (plz <= 76709)) or // 74594-76709
    ((plz >= 77601) and (plz <= 79879)) or // 77601-79879
    ((plz >= 88001) and (plz <= 88099)) or // 88001-88099
    ((plz >= 88147) and (plz <= 88147)) or // 88147-88147
    ((plz >= 88181) and (plz <= 89079)) or // 88181-89079
    ((plz >= 89081) and (plz <= 89085)) or // 89081-89085
    ((plz >= 89090) and (plz <= 89198)) or // 89090-89198
    ((plz >= 89501) and (plz <= 89619)) or // 89501-89619
    ((plz >= 97861) and (plz <= 97877)) or // 97861-97877
    ((plz >= 97893) and (plz <= 97896)) or // 97893-97896
    ((plz >= 97897) and (plz <= 97900)) or // 97897-97900
    ((plz >= 97911) and (plz <= 97999)); // 97911-97999
end;

function IstBayern(plz: integer): boolean;
begin
  // Extrahiert von https://cebus.net/de/plz-bundesland.htm
  result := ((plz >= 63701) and (plz <= 63774)) or // 63701-63774
    ((plz >= 63776) and (plz <= 63928)) or // 63776-63928
    ((plz >= 63930) and (plz <= 63939)) or // 63930-63939
    ((plz >= 74594) and (plz <= 74594)) or // 74594-74594
    ((plz >= 80001) and (plz <= 87490)) or // 80001-87490
    ((plz >= 87493) and (plz <= 87561)) or // 87493-87561
    ((plz >= 87571) and (plz <= 87789)) or // 87571-87789
    ((plz >= 88101) and (plz <= 88146)) or // 88101-88146
    ((plz >= 88147) and (plz <= 88179)) or // 88147-88179
    ((plz >= 89081) and (plz <= 89081)) or // 89081-89081
    ((plz >= 89087) and (plz <= 89087)) or // 89087-89087
    ((plz >= 89201) and (plz <= 89449)) or // 89201-89449
    ((plz >= 90001) and (plz <= 96489)) or // 90001-96489
    ((plz >= 97001) and (plz <= 97859)) or // 97001-97859
    ((plz >= 97888) and (plz <= 97892)) or // 97888-97892
    ((plz >= 97896) and (plz <= 97896)) or // 97896-97896
    ((plz >= 97901) and (plz <= 97909)); // 97901-97909
end;

function IstBerlin(plz: integer): boolean;
begin
  // Extrahiert von https://cebus.net/de/plz-bundesland.htm
  result := ((plz >= 10001) and (plz <= 14330)); // 10001-14330
end;

function IstBrandenburg(plz: integer): boolean;
begin
  // Extrahiert von https://cebus.net/de/plz-bundesland.htm
  result := ((plz >= 01941) and (plz <= 01998)) or // 01941-01998
    ((plz >= 03001) and (plz <= 03253)) or // 03001-03253
    ((plz >= 04891) and (plz <= 04938)) or // 04891-04938
    ((plz >= 14401) and (plz <= 14715)) or // 14401-14715
    ((plz >= 14723) and (plz <= 16949)) or // 14723-16949
    ((plz >= 17258) and (plz <= 17258)) or // 17258-17258
    ((plz >= 17261) and (plz <= 17291)) or // 17261-17291
    ((plz >= 17309) and (plz <= 17309)) or // 17309-17309
    ((plz >= 17321) and (plz <= 17321)) or // 17321-17321
    ((plz >= 17326) and (plz <= 17326)) or // 17326-17326
    ((plz >= 17335) and (plz <= 17335)) or // 17335-17335
    ((plz >= 17337) and (plz <= 17337)) or // 17337-17337
    ((plz >= 19307) and (plz <= 19357)); // 19307-19357
end;

function IstBremen(plz: integer): boolean;
begin
  // Extrahiert von https://cebus.net/de/plz-bundesland.htm
  result := ((plz >= 27501) and (plz <= 27580)) or // 27501-27580
    ((plz >= 28001) and (plz <= 28779)); // 28001-28779
end;

function IstHamburg(plz: integer): boolean;
begin
  // Extrahiert von https://cebus.net/de/plz-bundesland.htm
  result := ((plz >= 20001) and (plz <= 21037)) or // 20001-21037
    ((plz >= 21039) and (plz <= 21170)) or // 21039-21170
    ((plz >= 22001) and (plz <= 22113)) or // 22001-22113
    ((plz >= 22115) and (plz <= 22143)) or // 22115-22143
    ((plz >= 22145) and (plz <= 22145)) or // 22145-22145
    ((plz >= 22147) and (plz <= 22786)) or // 22147-22786
    ((plz >= 27499) and (plz <= 27499)); // 27499-27499
end;

function IstHessen(plz: integer): boolean;
begin
  // Extrahiert von https://cebus.net/de/plz-bundesland.htm
  result := ((plz >= 34001) and (plz <= 34329)) or // 34001-34329
    ((plz >= 34355) and (plz <= 34355)) or // 34355-34355
    ((plz >= 34356) and (plz <= 34399)) or // 34356-34399
    ((plz >= 34441) and (plz <= 36399)) or // 34441-36399
    ((plz >= 37194) and (plz <= 37195)) or // 37194-37195
    ((plz >= 37201) and (plz <= 37299)) or // 37201-37299
    ((plz >= 55240) and (plz <= 55252)) or // 55240-55252
    ((plz >= 59969) and (plz <= 59969)) or // 59969-59969
    ((plz >= 60001) and (plz <= 63699)) or // 60001-63699
    ((plz >= 63776) and (plz <= 63776)) or // 63776-63776
    ((plz >= 64201) and (plz <= 64753)) or // 64201-64753
    ((plz >= 64754) and (plz <= 65326)) or // 64754-65326
    ((plz >= 65327) and (plz <= 65391)) or // 65327-65391
    ((plz >= 65392) and (plz <= 65556)) or // 65392-65556
    ((plz >= 65583) and (plz <= 65620)) or // 65583-65620
    ((plz >= 65627) and (plz <= 65627)) or // 65627-65627
    ((plz >= 65701) and (plz <= 65936)) or // 65701-65936
    ((plz >= 68501) and (plz <= 68519)) or // 68501-68519
    ((plz >= 68601) and (plz <= 68649)) or // 68601-68649
    ((plz >= 69235) and (plz <= 69239)) or // 69235-69239
    ((plz >= 69430) and (plz <= 69431)) or // 69430-69431
    ((plz >= 69434) and (plz <= 69434)) or // 69434-69434
    ((plz >= 69479) and (plz <= 69488)) or // 69479-69488
    ((plz >= 69503) and (plz <= 69509)) or // 69503-69509
    ((plz >= 69515) and (plz <= 69518)); // 69515-69518
end;

function IstMecklenburgVorpommern(plz: integer): boolean;
begin
  // Extrahiert von https://cebus.net/de/plz-bundesland.htm
  result := ((plz >= 17001) and (plz <= 17256)) or // 17001-17256
    ((plz >= 17258) and (plz <= 17259)) or // 17258-17259
    ((plz >= 17301) and (plz <= 17309)) or // 17301-17309
    ((plz >= 17309) and (plz <= 17321)) or // 17309-17321
    ((plz >= 17321) and (plz <= 17322)) or // 17321-17322
    ((plz >= 17328) and (plz <= 17331)) or // 17328-17331
    ((plz >= 17335) and (plz <= 17335)) or // 17335-17335
    ((plz >= 17337) and (plz <= 19260)) or // 17337-19260
    ((plz >= 19273) and (plz <= 19273)) or // 19273-19273
    ((plz >= 19273) and (plz <= 19306)) or // 19273-19306
    ((plz >= 19357) and (plz <= 19417)) or // 19357-19417
    ((plz >= 23921) and (plz <= 23999)); // 23921-23999
end;

function IstNiedersachsen(plz: integer): boolean;
begin
  // Extrahiert von https://cebus.net/de/plz-bundesland.htm
  result := ((plz >= 19271) and (plz <= 19273)) or // 19271-19273
    ((plz >= 21202) and (plz <= 21449)) or // 21202-21449
    ((plz >= 21522) and (plz <= 21522)) or // 21522-21522
    ((plz >= 21601) and (plz <= 21789)) or // 21601-21789
    ((plz >= 26001) and (plz <= 27478)) or // 26001-27478
    ((plz >= 27607) and (plz <= 27809)) or // 27607-27809
    ((plz >= 28784) and (plz <= 29399)) or // 28784-29399
    ((plz >= 29431) and (plz <= 31868)) or // 29431-31868
    ((plz >= 34331) and (plz <= 34353)) or // 34331-34353
    ((plz >= 34355) and (plz <= 34355)) or // 34355-34355
    ((plz >= 37001) and (plz <= 37194)) or // 37001-37194
    ((plz >= 37197) and (plz <= 37199)) or // 37197-37199
    ((plz >= 37401) and (plz <= 37649)) or // 37401-37649
    ((plz >= 37689) and (plz <= 37691)) or // 37689-37691
    ((plz >= 37697) and (plz <= 38479)) or // 37697-38479
    ((plz >= 38501) and (plz <= 38729)) or // 38501-38729
    ((plz >= 48442) and (plz <= 48465)) or // 48442-48465
    ((plz >= 48478) and (plz <= 48480)) or // 48478-48480
    ((plz >= 48486) and (plz <= 48488)) or // 48486-48488
    ((plz >= 48497) and (plz <= 48531)) or // 48497-48531
    ((plz >= 49001) and (plz <= 49459)) or // 49001-49459
    ((plz >= 49551) and (plz <= 49849)); // 49551-49849
end;

function IstNordrheinWestfalen(plz: integer): boolean;
begin
  // Extrahiert von https://cebus.net/de/plz-bundesland.htm
  result := ((plz >= 32001) and (plz <= 33829)) or // 32001-33829
    ((plz >= 34401) and (plz <= 34439)) or // 34401-34439
    ((plz >= 37651) and (plz <= 37688)) or // 37651-37688
    ((plz >= 37692) and (plz <= 37696)) or // 37692-37696
    ((plz >= 40001) and (plz <= 48432)) or // 40001-48432
    ((plz >= 48466) and (plz <= 48477)) or // 48466-48477
    ((plz >= 48481) and (plz <= 48485)) or // 48481-48485
    ((plz >= 48489) and (plz <= 48496)) or // 48489-48496
    ((plz >= 48541) and (plz <= 48739)) or // 48541-48739
    ((plz >= 49461) and (plz <= 49549)) or // 49461-49549
    ((plz >= 50101) and (plz <= 51597)) or // 50101-51597
    ((plz >= 51601) and (plz <= 53359)) or // 51601-53359
    ((plz >= 53581) and (plz <= 53604)) or // 53581-53604
    ((plz >= 53621) and (plz <= 53949)) or // 53621-53949
    ((plz >= 57001) and (plz <= 57489)) or // 57001-57489
    ((plz >= 58001) and (plz <= 59966)) or // 58001-59966
    ((plz >= 59969) and (plz <= 59969)); // 59969-59969
end;

function IstRheinlandPfalz(plz: integer): boolean;
begin
  // Extrahiert von https://cebus.net/de/plz-bundesland.htm
  result := ((plz >= 51598) and (plz <= 51598)) or // 51598-51598
    ((plz >= 53401) and (plz <= 53579)) or // 53401-53579
    ((plz >= 53614) and (plz <= 53619)) or // 53614-53619
    ((plz >= 54181) and (plz <= 55239)) or // 54181-55239
    ((plz >= 55253) and (plz <= 56869)) or // 55253-56869
    ((plz >= 57501) and (plz <= 57648)) or // 57501-57648
    ((plz >= 65326) and (plz <= 65326)) or // 65326-65326
    ((plz >= 65391) and (plz <= 65391)) or // 65391-65391
    ((plz >= 65558) and (plz <= 65582)) or // 65558-65582
    ((plz >= 65621) and (plz <= 65626)) or // 65621-65626
    ((plz >= 65629) and (plz <= 65629)) or // 65629-65629
    ((plz >= 66461) and (plz <= 66509)) or // 66461-66509
    ((plz >= 66841) and (plz <= 67829)) or // 66841-67829
    ((plz >= 76711) and (plz <= 76891)); // 76711-76891
end;

function IstSaarland(plz: integer): boolean;
begin
  // Extrahiert von https://cebus.net/de/plz-bundesland.htm
  result := ((plz >= 66001) and (plz <= 66459)) or // 66001-66459
    ((plz >= 66511) and (plz <= 66839)); // 66511-66839
end;

function IstSachsen(plz: integer): boolean;
begin
  // Extrahiert von https://cebus.net/de/plz-bundesland.htm
  result := ((plz >= 01001) and (plz <= 01936)) or // 01001-01936
    ((plz >= 02601) and (plz <= 02999)) or // 02601-02999
    ((plz >= 04001) and (plz <= 04579)) or // 04001-04579
    ((plz >= 04641) and (plz <= 04889)) or // 04641-04889
    ((plz >= 07919) and (plz <= 07919)) or // 07919-07919
    ((plz >= 07919) and (plz <= 07919)) or // 07919-07919
    ((plz >= 07951) and (plz <= 07951)) or // 07951-07951
    ((plz >= 07952) and (plz <= 07952)) or // 07952-07952
    ((plz >= 07982) and (plz <= 07982)) or // 07982-07982
    ((plz >= 07985) and (plz <= 07985)) or // 07985-07985
    ((plz >= 08001) and (plz <= 09669)); // 08001-09669
end;

function IstSachsenAnhalt(plz: integer): boolean;
begin
  // Extrahiert von https://cebus.net/de/plz-bundesland.htm
  result := ((plz >= 06001) and (plz <= 06548)) or // 06001-06548
    ((plz >= 06601) and (plz <= 06928)) or // 06601-06928
    ((plz >= 14715) and (plz <= 14715)) or // 14715-14715
    ((plz >= 29401) and (plz <= 29416)) or // 29401-29416
    ((plz >= 38481) and (plz <= 38489)) or // 38481-38489
    ((plz >= 38801) and (plz <= 39649)); // 38801-39649
end;

function IstSchleswigHolstein(plz: integer): boolean;
begin
  // Extrahiert von https://cebus.net/de/plz-bundesland.htm
  result := ((plz >= 21039) and (plz <= 21039)) or // 21039-21039
    ((plz >= 21451) and (plz <= 21521)) or // 21451-21521
    ((plz >= 21524) and (plz <= 21529)) or // 21524-21529
    ((plz >= 22113) and (plz <= 22113)) or // 22113-22113
    ((plz >= 22145) and (plz <= 22145)) or // 22145-22145
    ((plz >= 22145) and (plz <= 22145)) or // 22145-22145
    ((plz >= 22801) and (plz <= 23919)) or // 22801-23919
    ((plz >= 24001) and (plz <= 25999)) or // 24001-25999
    ((plz >= 27483) and (plz <= 27498)); // 27483-27498
end;

function IstThueringen(plz: integer): boolean;
begin
  // Extrahiert von https://cebus.net/de/plz-bundesland.htm
  result := ((plz >= 04581) and (plz <= 04639)) or // 04581-04639
    ((plz >= 06551) and (plz <= 06578)) or // 06551-06578
    ((plz >= 07301) and (plz <= 07919)) or // 07301-07919
    ((plz >= 07919) and (plz <= 07919)) or // 07919-07919
    ((plz >= 07920) and (plz <= 07950)) or // 07920-07950
    ((plz >= 07952) and (plz <= 07952)) or // 07952-07952
    ((plz >= 07953) and (plz <= 07980)) or // 07953-07980
    ((plz >= 07985) and (plz <= 07985)) or // 07985-07985
    ((plz >= 07985) and (plz <= 07989)) or // 07985-07989
    ((plz >= 36401) and (plz <= 36469)) or // 36401-36469
    ((plz >= 37301) and (plz <= 37359)) or // 37301-37359
    ((plz >= 96501) and (plz <= 96529)) or // 96501-96529
    ((plz >= 98501) and (plz <= 99998)); // 98501-99998
end;

{$ENDREGION}
{$REGION 'Feiertage in Bundesländern in vereinzelten Teilen'}

function BayernHatMariaeHimmelfahrt(plz: integer): boolean;
begin
  // Daten extrahiert aus
  // - https://www.statistik.bayern.de/statistik/gebiet_bevoelkerung/zensus/himmelfahrt/index.php
  // (1704 Gemeinden mit Feiertag und 352 Gemeinden ohne Feiertag)
  // und
  // - https://www.destatis.de/DE/Themen/Laender-Regionen/Regionales/Gemeindeverzeichnis/_inhalt.html#124272
  result := (plz <> 92262) and // Birgland
    (plz <> 92265) and // Edelsfeld
    (plz <> 92275) and // Hirschbach
    (plz <> 92278) and // Illschwang
    (plz <> 92281) and // Königstein, M
    (plz <> 92268) and // Etzelwang
    (plz <> 92259) and // Neukirchen b.Sulzbach-Rosenberg
    (plz <> 91249) and // Weigendorf
    (plz <> 95514) and // Neustadt am Kulm, St
    (plz <> 95444) and // Bayreuth
    (plz <> 96450) and // Coburg
    (plz <> 95028) and // Hof
    (plz <> 95460) and // Bad Berneck i.Fichtelgebirge, St
    (plz <> 91282) and // Betzenstein, St
    (plz <> 95463) and // Bindlach
    (plz <> 95493) and // Bischofsgrün
    (plz <> 95473) and // Creußen, St
    (plz <> 95488) and // Eckersdorf
    (plz <> 95517) and // Emtmannsberg
    (plz <> 95482) and // Gefrees, St
    (plz <> 95494) and // Gesees
    (plz <> 95496) and // Glashütten
    (plz <> 95497) and // Goldkronach, St
    (plz <> 95473) and // Haag
    (plz <> 95500) and // Heinersreuth
    (plz <> 95503) and // Hummeltal
    (plz <> 95511) and // Mistelbach
    (plz <> 95490) and // Mistelgau
    (plz <> 91287) and // Plech, M
    (plz <> 95473) and // Prebitz
    (plz <> 91289) and // Schnabelwaid, M
    (plz <> 95517) and // Seybothenreuth
    (plz <> 95469) and // Speichersdorf
    (plz <> 95485) and // Warmensteinach
    (plz <> 95466) and // Weidenberg, M
    (plz <> 96482) and // Ahorn
    (plz <> 96487) and // Dörfles-Esbach
    (plz <> 96237) and // Ebersdorf b.Coburg
    (plz <> 96269) and // Großheirath
    (plz <> 96271) and // Grub a.Forst
    (plz <> 96274) and // Itzgrund
    (plz <> 96486) and // Lautertal
    (plz <> 96484) and // Meeder
    (plz <> 96465) and // Neustadt b.Coburg, GKSt
    (plz <> 96489) and // Niederfüllbach
    (plz <> 96476) and // Bad Rodach, St
    (plz <> 96472) and // Rödental, St
    (plz <> 96242) and // Sonnefeld
    (plz <> 96253) and // Untersiemau
    (plz <> 96279) and // Weidhausen b.Coburg
    (plz <> 96479) and // Weitramsdorf
    (plz <> 91349) and // Egloffstein, M
    (plz <> 91322) and // Gräfenberg, St
    (plz <> 91355) and // Hiltpoltstein, M
    (plz <> 91338) and // Igensdorf, M
    (plz <> 91346) and // Wiesenttal, M
    (plz <> 95138) and // Bad Steben, M
    (plz <> 95180) and // Berg
    (plz <> 95182) and // Döhlau
    (plz <> 95183) and // Feilitzsch
    (plz <> 95185) and // Gattendorf
    (plz <> 95179) and // Geroldsgrün
    (plz <> 95233) and // Helmbrechts, St
    (plz <> 95188) and // Issigau
    (plz <> 95189) and // Köditz
    (plz <> 95176) and // Konradsreuth
    (plz <> 95191) and // Leupoldsgrün
    (plz <> 95192) and // Lichtenberg, St
    (plz <> 95213) and // Münchberg, St
    (plz <> 95119) and // Naila, St
    (plz <> 95145) and // Oberkotzau, M
    (plz <> 95194) and // Regnitzlosau
    (plz <> 95111) and // Rehau, St
    (plz <> 95197) and // Schauenstein, St
    (plz <> 95126) and // Schwarzenbach a.d.Saale, St
    (plz <> 95131) and // Schwarzenbach a.Wald, St
    (plz <> 95152) and // Selbitz, St
    (plz <> 95234) and // Sparneck, M
    (plz <> 95236) and // Stammbach, M
    (plz <> 95183) and // Töpen
    (plz <> 95183) and // Trogen
    (plz <> 95237) and // Weißdorf
    (plz <> 95239) and // Zell im Fichtelgebirge, M
    (plz <> 96328) and // Küps, M
    (plz <> 96337) and // Ludwigsstadt, St
    (plz <> 96268) and // Mitwitz, M
    (plz <> 96277) and // Schneckenlohe
    (plz <> 96355) and // Tettau, M
    (plz <> 96364) and // Marktrodach, M
    (plz <> 96369) and // Weißenbrunn
    (plz <> 95356) and // Grafengehaig, M
    (plz <> 95358) and // Guttenberg
    (plz <> 95499) and // Harsdorf
    (plz <> 95502) and // Himmelkron
    (plz <> 95359) and // Kasendorf, M
    (plz <> 95361) and // Ködnitz
    (plz <> 95326) and // Kulmbach, GKSt
    (plz <> 95336) and // Mainleus, M
    (plz <> 95512) and // Neudrossenfeld
    (plz <> 95339) and // Neuenmarkt
    (plz <> 95365) and // Rugendorf
    (plz <> 95349) and // Thurnau, M
    (plz <> 95367) and // Trebgast
    (plz <> 95369) and // Untersteinach
    (plz <> 95339) and // Wirsberg, M
    (plz <> 96197) and // Wonsees, M
    (plz <> 96247) and // Michelau i.OFr.
    (plz <> 96257) and // Redwitz a.d.Rodach
    (plz <> 95680) and // Bad Alexandersbad
    (plz <> 95659) and // Arzberg, St
    (plz <> 95186) and // Höchstädt i.Fichtelgebirge
    (plz <> 95691) and // Hohenberg a.d.Eger, St
    (plz <> 95158) and // Kirchenlamitz, St
    (plz <> 95168) and // Marktleuthen, St
    (plz <> 95615) and // Marktredwitz, GKSt
    (plz <> 95195) and // Röslau
    (plz <> 95706) and // Schirnding, M
    (plz <> 95173) and // Schönwald, St
    (plz <> 95100) and // Selb, GKSt
    (plz <> 95707) and // Thiersheim, M
    (plz <> 95199) and // Thierstein, M
    (plz <> 95709) and // Tröstau
    (plz <> 95163) and // Weißenstadt, St
    (plz <> 95632) and // Wunsiedel, St
    (plz <> 91522) and // Ansbach
    (plz <> 91052) and // Erlangen
    (plz <> 90762) and // Fürth
    (plz <> 90403) and // Nürnberg
    (plz <> 91126) and // Schwabach
    (plz <> 91587) and // Adelshofen
    (plz <> 91572) and // Bechhofen, M
    (plz <> 91590) and // Bruckberg
    (plz <> 91592) and // Buch a.Wald
    (plz <> 91596) and // Burk
    (plz <> 91598) and // Colmberg, M
    (plz <> 91599) and // Dentlein a.Forst, M
    (plz <> 91583) and // Diebach
    (plz <> 90599) and // Dietenhofen, M
    (plz <> 91550) and // Dinkelsbühl, GKSt
    (plz <> 91601) and // Dombühl, M
    (plz <> 91725) and // Ehingen
    (plz <> 91555) and // Feuchtwangen, St
    (plz <> 91604) and // Flachslanden, M
    (plz <> 91607) and // Gebsattel
    (plz <> 91726) and // Gerolfingen
    (plz <> 91608) and // Geslau
    (plz <> 91560) and // Heilsbronn, St
    (plz <> 91610) and // Insingen
    (plz <> 91731) and // Langfurth
    (plz <> 91611) and // Lehrberg, M
    (plz <> 91578) and // Leutershausen, St
    (plz <> 91586) and // Lichtenau, M
    (plz <> 91732) and // Merkendorf, St
    (plz <> 91614) and // Mönchsroth
    (plz <> 91564) and // Neuendettelsau
    (plz <> 91616) and // Neusitz
    (plz <> 91617) and // Oberdachstetten
    (plz <> 91620) and // Ohrenbach
    (plz <> 91580) and // Petersaurach
    (plz <> 91740) and // Röckingen
    (plz <> 91541) and // Rothenburg ob der Tauber, GKSt
    (plz <> 91622) and // Rügland
    (plz <> 91623) and // Sachsen b.Ansbach
    (plz <> 91583) and // Schillingsfürst, St
    (plz <> 91625) and // Schnelldorf
    (plz <> 91626) and // Schopfloch, M
    (plz <> 91628) and // Steinsfeld
    (plz <> 91743) and // Unterschwaningen
    (plz <> 91717) and // Wassertrüdingen, St
    (plz <> 91746) and // Weidenbach, M
    (plz <> 91629) and // Weihenzell
    (plz <> 91744) and // Weiltingen, M
    (plz <> 91631) and // Wettringen
    (plz <> 91632) and // Wieseth
    (plz <> 91635) and // Windelsbach
    (plz <> 91575) and // Windsbach, St
    (plz <> 91749) and // Wittelshofen
    (plz <> 91637) and // Wörnitz
    (plz <> 91086) and // Aurachtal
    (plz <> 91083) and // Baiersdorf, St
    (plz <> 91054) and // Buckenhof
    (plz <> 90542) and // Eckental, M
    (plz <> 90562) and // Heroldsberg, M
    (plz <> 90562) and // Kalchreuth
    (plz <> 91475) and // Lonnerstadt, M
    (plz <> 91096) and // Möhrendorf
    (plz <> 96172) and // Mühlhausen, M
    (plz <> 91097) and // Oberreichenbach
    (plz <> 91080) and // Spardorf
    (plz <> 91080) and // Uttenreuth
    (plz <> 91487) and // Vestenbergsgreuth, M
    (plz <> 91085) and // Weisendorf, M
    (plz <> 90614) and // Ammerndorf, M
    (plz <> 90556) and // Cadolzburg, M
    (plz <> 90613) and // Großhabersdorf
    (plz <> 90579) and // Langenzenn, St
    (plz <> 90522) and // Oberasbach, St
    (plz <> 90587) and // Obermichelbach
    (plz <> 90617) and // Puschendorf
    (plz <> 90574) and // Roßtal, M
    (plz <> 90556) and // Seukendorf
    (plz <> 90547) and // Stein, St
    (plz <> 90587) and // Tuchenbach
    (plz <> 90587) and // Veitsbronn
    (plz <> 91452) and // Wilhermsdorf, M
    (plz <> 90513) and // Zirndorf, St
    (plz <> 91236) and // Alfeld
    (plz <> 90518) and // Altdorf b.Nürnberg, St
    (plz <> 90559) and // Burgthann
    (plz <> 91238) and // Engelthal
    (plz <> 90537) and // Feucht, M
    (plz <> 91230) and // Happurg
    (plz <> 91235) and // Hartenstein
    (plz <> 91239) and // Henfenfeld
    (plz <> 91217) and // Hersbruck, St
    (plz <> 91241) and // Kirchensittenbach
    (plz <> 91207) and // Lauf a.d.Pegnitz, St
    (plz <> 91227) and // Leinburg
    (plz <> 91238) and // Offenhausen
    (plz <> 91242) and // Ottensoos
    (plz <> 91224) and // Pommelsbrunn
    (plz <> 91244) and // Reichenschwand
    (plz <> 90552) and // Röthenbach a.d.Pegnitz, St
    (plz <> 90607) and // Rückersdorf
    (plz <> 90571) and // Schwaig b.Nürnberg
    (plz <> 90592) and // Schwarzenbruck
    (plz <> 91235) and // Velden, St
    (plz <> 91247) and // Vorra
    (plz <> 90610) and // Winkelhaid
    (plz <> 91438) and // Bad Windsheim, St
    (plz <> 91460) and // Baudenbach, M
    (plz <> 91593) and // Burgbernheim, St
    (plz <> 96152) and // Burghaslach, M
    (plz <> 91462) and // Dachsbach, M
    (plz <> 91456) and // Diespeck
    (plz <> 91463) and // Dietersheim
    (plz <> 91448) and // Emskirchen, M
    (plz <> 91465) and // Ergersheim
    (plz <> 91605) and // Gallmersgarten
    (plz <> 91466) and // Gerhardshofen
    (plz <> 97258) and // Gollhofen
    (plz <> 91468) and // Gutenstetten
    (plz <> 91469) and // Hagenbüchach
    (plz <> 97258) and // Hemmersheim
    (plz <> 91471) and // Illesheim
    (plz <> 97258) and // Ippesheim, M
    (plz <> 91472) and // Ipsheim, M
    (plz <> 91474) and // Langenfeld
    (plz <> 91613) and // Marktbergel, M
    (plz <> 91459) and // Markt Erlbach, M
    (plz <> 91478) and // Markt Nordheim, M
    (plz <> 91480) and // Markt Taschendorf, M
    (plz <> 91481) and // Münchsteinach
    (plz <> 90616) and // Neuhof a.d.Zenn, M
    (plz <> 91413) and // Neustadt a.d.Aisch, St
    (plz <> 97258) and // Oberickelsheim
    (plz <> 91619) and // Obernzenn, M
    (plz <> 97215) and // Simmershofen
    (plz <> 91484) and // Sugenheim, M
    (plz <> 90619) and // Trautskirchen
    (plz <> 91486) and // Uehlfeld, M
    (plz <> 97215) and // Uffenheim, St
    (plz <> 97215) and // Weigenheim
    (plz <> 91489) and // Wilhelmsdorf
    (plz <> 91186) and // Büchenbach
    (plz <> 91166) and // Georgensgmünd
    (plz <> 91126) and // Kammerstein
    (plz <> 90596) and // Schwanstetten, M
    (plz <> 91126) and // Rednitzhembach
    (plz <> 91189) and // Rohr
    (plz <> 91154) and // Roth, St
    (plz <> 91177) and // Thalmässing, M
    (plz <> 90530) and // Wendelstein, M
    (plz <> 91720) and // Absberg, M
    (plz <> 91793) and // Alesheim
    (plz <> 91735) and // Muhr a.See
    (plz <> 91790) and // Bergen
    (plz <> 91790) and // Burgsalach
    (plz <> 91723) and // Dittenheim
    (plz <> 91796) and // Ettenstatt
    (plz <> 91710) and // Gunzenhausen, St
    (plz <> 91729) and // Haundorf
    (plz <> 91719) and // Heidenheim, M
    (plz <> 91798) and // Höttingen
    (plz <> 91799) and // Langenaltheim
    (plz <> 91801) and // Markt Berolzheim, M
    (plz <> 91802) and // Meinheim
    (plz <> 91790) and // Nennslingen, M
    (plz <> 91788) and // Pappenheim, St
    (plz <> 91738) and // Pfofeld
    (plz <> 91805) and // Polsingen
    (plz <> 91807) and // Solnhofen
    (plz <> 91741) and // Theilenhofen
    (plz <> 91757) and // Treuchtlingen, St
    (plz <> 91781) and // Weißenburg i.Bay., GKSt
    (plz <> 91747) and // Westheim
    (plz <> 97779) and // Geroda, M
    (plz <> 97799) and // Zeitlofs, M
    (plz <> 97633) and // Aubstadt
    (plz <> 97633) and // Höchheim
    (plz <> 97645) and // Ostheim v.d.Rhön, St
    (plz <> 97647) and // Sondheim v.d.Rhön
    (plz <> 97647) and // Willmars
    (plz <> 97486) and // Königsberg i.Bay., St
    (plz <> 96126) and // Maroldsweisach, M
    (plz <> 96184) and // Rentweinsdorf, M
    (plz <> 96190) and // Untermerzbach
    (plz <> 96126) and // Ermershausen
    (plz <> 97355) and // Abtswind, M
    (plz <> 97320) and // Albertshofen
    (plz <> 97320) and // Buchbrunn
    (plz <> 97355) and // Castell
    (plz <> 97318) and // Kitzingen, GKSt
    (plz <> 97355) and // Kleinlangheim, M
    (plz <> 97350) and // Mainbernheim, St
    (plz <> 97320) and // Mainstockheim
    (plz <> 97340) and // Marktbreit, St
    (plz <> 97348) and // Markt Einersheim, M
    (plz <> 97342) and // Marktsteft, St
    (plz <> 97340) and // Martinsheim
    (plz <> 97342) and // Obernbreit, M
    (plz <> 97348) and // Rödelsee
    (plz <> 97355) and // Rüdenhausen, M
    (plz <> 97340) and // Segnitz
    (plz <> 97355) and // Wiesenbronn
    (plz <> 97907) and // Hasloch
    (plz <> 97785) and // Mittelsinn
    (plz <> 97846) and // Partenstein
    (plz <> 97525) and // Schwebheim
    (plz <> 97526) and // Sennfeld
    (plz <> 97237) and // Altertheim
    (plz <> 97234) and // Reichenberg, M
    (plz <> 97280) and // Remlingen, M
    (plz <> 97286) and // Sommerhausen, M
    (plz <> 97292) and // Uettingen
    (plz <> 97286) and // Winterhausen, M
    (plz <> 89431) and // Bächingen a.d.Brenz
    (plz <> 87761) and // Lauben
    (plz <> 87766) and // Memmingerberg
    (plz <> 87789) and // Woringen
    (plz <> 86733) and // Alerheim
    (plz <> 86736) and // Auhausen
    (plz <> 86738) and // Deiningen
    (plz <> 86739) and // Ederheim
    (plz <> 86735) and // Forheim
    (plz <> 86655) and // Harburg (Schwaben), St
    (plz <> 86745) and // Hohenaltheim
    (plz <> 86751) and // Mönchsdeggingen
    (plz <> 86753) and // Möttingen
    (plz <> 86720) and // Nördlingen, GKSt
    (plz <> 86732) and // Oettingen i.Bay., St
    (plz <> 86759); // Wechingen
end;

function ThueringenHatFronleichnam(plz: integer): boolean;
begin
  (*
    Laut https://www.dgb.de/gesetzliche-feiertage-deutschland :
    In Thüringen nur im im Landkreis Eichsfeld
    sowie in folgenden Gemeinden des Unstrut-Hainich-Kreises und des Wartburgkreises:
    Anrode (nur in den Ortsteilen Bickenriede und Zella),
    Brunnhartshausen (nur in den Ortsteilen Föhlritz und Steinberg),
    Buttlar,
    Dünwald (nur in den Ortsteilen Beberstedt und Hüpstedt),
    Geisa,
    Rodeberg (nur im Ortsteil Struth),
    Schleid,
    Südeichsfeld und
    Zella/Rhön.
  *)

  result :=

  // Landkreis Eichsfeld
  // https://home.meinestadt.de/kreis-eichsfeld/postleitzahlen
    (plz = 37318) or // Wüstheuterode, Kreis Eichsfeld, Thüringen
    (plz = 37308) or // Volkerode, Kreis Eichsfeld, Thüringen
    (plz = 37339) or // Ferna, Kreis Eichsfeld, Thüringen
    (plz = 37339) or // Leinefelde-Worbis, Kreis Eichsfeld, Thüringen
    (plz = 37327) or // (ebenso)
    (plz = 37339) or // Breitenworbis, Kreis Eichsfeld, Thüringen
    (plz = 37355) or // (ebenso)
    (plz = 37359) or // Küllstedt, Kreis Eichsfeld, Thüringen
    (plz = 37327) or // Wingerode, Kreis Eichsfeld, Thüringen
    (plz = 37327) or // Niederorschel, Kreis Eichsfeld, Thüringen
    (plz = 37355) or // (ebenso)
    (plz = 37351) or // Silberhausen, Kreis Eichsfeld, Thüringen
    (plz = 37355) or // Kleinbartloff, Kreis Eichsfeld, Thüringen
    (plz = 37345) or // Am Ohmberg, Kreis Eichsfeld, Thüringen

  // Gemeinden des Unstrut-Hainich-Kreises und des Wartburgkreises (PLZ von Google, Wikipedia, etc.):
  // TODO: Anrode (nur in den Ortsteilen Bickenriede und Zella),
  // TODO: Brunnhartshausen (nur in den Ortsteilen Föhlritz und Steinberg),
    (plz = 36419) or // Buttlar
  // TODO: Dünwald (nur in den Ortsteilen Beberstedt und Hüpstedt)
    (plz = 36419) or // Geisa
  // TODO: Rodeberg (nur im Ortsteil Struth)
    (plz = 36419) or // Schleid
    (plz = 99988) or // Südeichsfeld
    (plz = 36452); // Zella/Rhön
end;

function SachsenHatFronleichnam(plz: integer): boolean;
begin
  (*
    Laut https://www.dgb.de/gesetzliche-feiertage-deutschland:
    In Sachsen nur in folgenden katholisch geprägten Gemeinden des sorbischen Siedlungsgebietes im Landkreis Bautzen:
    Bautzen (nur in den Ortsteilen Bolbritz und Salzenforst),
    Crostwitz,
    Göda (nur im Ortsteil Prischwitz),
    Großdubrau (nur im Ortsteil Sdier),
    Hoyerswerda (nur im Ortsteil Dörgenhausen),
    Königswartha (nicht im Ortsteil Wartha),
    Nebelschütz,
    Neschwitz (nur in den Ortsteilen Neschwitz und Saritsch),
    Panschwitz-Kuckau,
    Puschwitz,
    Räckelwitz,
    Radibor,
    Ralbitz-Rosenthal,
    Wittichenau
  *)

  result :=
  // (PLZ von Google, Wikipedia, etc.)
  // TODO: Bautzen (nur in den Ortsteilen Bolbritz und Salzenforst)
    (plz = 01920) or // Crostwitz
  // TODO: Göda (nur im Ortsteil Prischwitz)
  // TODO: Großdubrau (nur im Ortsteil Sdier)
  // TODO: Hoyerswerda (nur im Ortsteil Dörgenhausen)
  // TODO: Königswartha (nicht im Ortsteil Wartha)
    (plz = 01920) or // Nebelschütz
  // TODO: Neschwitz (nur in den Ortsteilen Neschwitz und Saritsch)
    (plz = 01906) or // Panschwitz-Kuckau
    (plz = 01920) or // (ebenso)
    (plz = 02699) or // Puschwitz
    (plz = 01920) or // Räckelwitz
    (plz = 02627) or // Radibor
    (plz = 02694) or // (ebenso)
    (plz = 01920) or // Ralbitz-Rosenthal
    (plz = 02997); // Wittichenau
end;

function IstAugsburgStadtgebiet(plz: integer): boolean;
begin
  // Extrahiert von https://www.suche-postleitzahl.org/augsburg-plz-86150-86199.35f2
  result := (plz = 86159) or // Antonsviertel
    (plz = 86199) or // Bergheim
    (plz = 86156) or // Bärenkeller
    (plz = 86169) or // Firnhaberau
    (plz = 86199) or // Göggingen
    (plz = 86169) or // Hammerschmiede
    (plz = 86179) or // Haunstetten-Siebenbrunn
    (plz = 86199) or (plz = 86159) or // Hochfeld
    (plz = 86161) or //
    (plz = 86163) or // Hochzoll
    (plz = 86150) or // Innenstadt
    (plz = 86152) or //
    (plz = 86153) or //
    (plz = 86159) or //
    (plz = 86161) or //
    (plz = 86199) or // Inningen
    (plz = 86156) or // Kriegshaber
    (plz = 86157) or //
    (plz = 86165) or // Lechhausen
    (plz = 86167) or //
    (plz = 86169) or //
    (plz = 86154) or // Oberhausen
    (plz = 86156) or //
    (plz = 86156) or // Pfersee
    (plz = 86157) or //
    (plz = 86161) or // Spickel-Herrenbach
    (plz = 86159) or // Universitätsviertel
    (plz = 86161);
end;

{$ENDREGION}
{$REGION 'Spezielle Datumsberechnungen'}

function DritterMittwochImNovember(Jahr: integer): TDateTime;
begin
  result := EncodeDate(Jahr, 11, 1);
  result := result + ((11 - DayOfWeek(result)) mod 7) + 14;
end;

function Ostersonntag(Jahr: integer): TDateTime;
var
  A, B, C, D, E, F, G, H, I, K, L, M, N, P: Word;
  tag, Monat: Word;
begin
  A := Jahr mod 19;
  B := Jahr div 100;
  C := Jahr mod 100;
  D := B div 4;
  E := B mod 4;
  F := (B + 8) div 25;
  G := (B - F + 1) div 3;
  H := (19 * A + B - D - G + 15) mod 30;
  I := C div 4;
  K := C mod 4;
  L := (32 + 2 * E + 2 * I - H - K) mod 7;
  M := (A + 11 * H + 22 * L) div 451;
  N := (H + L - 7 * M + 114) div 31;
  P := (H + L - 7 * M + 114) mod 31 + 1;
  tag := P;
  Monat := N;
  result := EncodeDate(Jahr, Monat, tag);
end;

{$ENDREGION}

function IstDeutscherFeiertag(tag: TDateTime; plz: integer): boolean;
var
  ht: TFeiertagTable;
  H: TFeiertag;
begin
  ht := DeutschlandFeiertage(YearOf(tag), plz);
  for H in ht do
  begin
    if SameDate(tag, H.Date) then
    begin
      result := true;
      exit;
    end;
  end;
  result := false;
end;

function DeutschlandFeiertage(Jahr: Word; plz: integer): TFeiertagTable;

// Funktion, um einen Feiertag über seinen Tag\Monat hinzuzufügen
  procedure AddFeiertag(DD, MM: Word; HDName: string); overload;
  begin
    SetLength(result, High(result) + 2);
    with result[High(result)] do
    begin
      Date := EncodeDate(Jahr, MM, DD);
      Name := HDName;
    end;
  end;

// Funktion, um den Feiertag über die Datumsseriennummer hinzuzufügen
  procedure AddFeiertag(HDDate: TDateTime; HDName: string); overload;
  begin
    SetLength(result, High(result) + 2);
    with result[High(result)] do
    begin
      Date := HDDate;
      Name := HDName;
    end;
  end;

begin
  // siehe https://www.dgb.de/gesetzliche-feiertage-deutschland

  AddFeiertag(1, 1, 'Neujahr');

  if IstBadenWuerttemberg(plz) or IstBayern(plz) or IstSachsenAnhalt(plz) then
  begin
    AddFeiertag(6, 1, 'Heilige Drei Könige');
  end;

  if IstBerlin(plz) then
  begin
    AddFeiertag(8, 3, 'Internationaler Frauentag');
  end;

  AddFeiertag(Ostersonntag(Jahr) - 2, 'Karfreitag');

  if IstBrandenburg(plz) then
  begin
    AddFeiertag(Ostersonntag(Jahr), 'Ostersonntag');
  end;

  AddFeiertag(Ostersonntag(Jahr) + 1, 'Ostermontag');

  AddFeiertag(1, 5, 'Tag der Arbeit');

  AddFeiertag(Ostersonntag(Jahr) + 39, 'Christi Himmelfahrt');

  if IstBrandenburg(plz) then
  begin
    AddFeiertag(Ostersonntag(Jahr) + 49, 'Pfingstsonntag');
  end;

  AddFeiertag(Ostersonntag(Jahr) + 50, 'Pfingstmontag');

  if IstBadenWuerttemberg(plz) or IstBayern(plz) or IstHessen(plz) or
    IstNordrheinWestfalen(plz) or IstRheinlandPfalz(plz) or IstSaarland(plz) or
    (IstSachsen(plz) and SachsenHatFronleichnam(plz)) or
    (IstThueringen(plz) and ThueringenHatFronleichnam(plz)) then
  begin
    AddFeiertag(Ostersonntag(Jahr) + 60, 'Fronleichnam');
  end;

  if IstAugsburgStadtgebiet(plz) then
  begin
    (*
      Laut https://www.dgb.de/gesetzliche-feiertage-deutschland:
      - Nur im Stadtgebiet von Augsburg (nicht jedoch im angrenzenden Umland).
    *)
    AddFeiertag(8, 8, 'Augsburger Friedensfest');
  end;

  if IstSaarland(plz) or (IstBayern(plz) and BayernHatMariaeHimmelfahrt(plz))
  then
  begin
    AddFeiertag(15, 8, 'Mariä Himmelfahrt');
  end;

  if IstThueringen(plz) then
  begin
    AddFeiertag(20, 9, 'Weltkindertag');
  end;

  AddFeiertag(3, 10, 'Tag der deutschen Einheit');

  if IstBrandenburg(plz) or IstBremen(plz) or IstHamburg(plz) or
    IstMecklenburgVorpommern(plz) or IstNiedersachsen(plz) or IstSachsen(plz) or
    IstSachsenAnhalt(plz) or IstSchleswigHolstein(plz) or IstThueringen(plz)
  then
  begin
    AddFeiertag(31, 10, 'Reformationstag');
  end;

  if IstBadenWuerttemberg(plz) or IstBayern(plz) or IstNordrheinWestfalen(plz)
    or IstRheinlandPfalz(plz) or IstSaarland(plz) then
  begin
    AddFeiertag(1, 11, 'Allerheiligen');
  end;

  if IstSachsen(plz) then
  begin
    // Ermittelt den 3. Mitwoch im November
    AddFeiertag(DritterMittwochImNovember(Jahr), 'Buß- und Bettag');
  end;

  // AddFeiertag(24, 12, 'Heiligabend');

  AddFeiertag(25, 12, '1. Weihnachtsfeiertag');

  AddFeiertag(26, 12, '2. Weihnachtsfeiertag');

  // AddFeiertag(31, 12, 'Silvester');
end;

end.
