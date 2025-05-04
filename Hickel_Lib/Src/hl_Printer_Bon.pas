unit hl_Printer_Bon;

interface

uses
  SysUtils, hl_Printer_Neu;

const
  // EPSON+Samsung
  COMMAND_EPSON_CASHDRAWER1_50MS_PULS = 'A';
  COMMAND_EPSON_CASHDRAWER1_100MS_PULS = 'B';
  COMMAND_EPSON_CASHDRAWER1_150MS_PULS = 'C';
  COMMAND_EPSON_CASHDRAWER1_200MS_PULS = 'D';
  COMMAND_EPSON_CASHDRAWER1_250MS_PULS = 'E';
  COMMAND_EPSON_CASHDRAWER2_50MS_PULS = 'a';
  COMMAND_EPSON_CASHDRAWER2_100MS_PULS = 'b';
  COMMAND_EPSON_CASHDRAWER2_150MS_PULS = 'c';
  COMMAND_EPSON_CASHDRAWER2_200MS_PULS = 'd';
  COMMAND_EPSON_CASHDRAWER2_250MS_PULS = 'e';
  COMMAND_EPSON_BON_SCHNEIDEN = 'P';

  // Bixolon SRP-350plusIII
  // siehe https://www.manualslib.com/manual/1150493/Bixolon-Srp-350plusiii.html?page=42
  COMMAND_BIXOLON_CASHDRAWER1_NO_FEED_50MS_PULS = 'F';
  COMMAND_BIXOLON_CASHDRAWER2_NO_FEED_50MS_PULS = 'f';

type
  ThlPrinterBon = class(ThlPrinterNeu)
    procedure BondruckerControlCode(aCommand: Char; aPrinterName: string;
      aSchachtIndex: integer);
  end;

function hlPrinterBon: ThlPrinterBon;
function SethlPrinterBon(NewPrinter: ThlPrinterBon): ThlPrinterBon;

implementation

uses
  StrUtils;

threadvar _hlPrinterBon: ThlPrinterBon;

procedure ThlPrinterBon.BondruckerControlCode(aCommand: Char;
  aPrinterName: string; aSchachtIndex: integer);
var
  iNoFeedNoCutSchachtIndex: integer;
begin
  if aPrinterName = '' then
    exit;
  if aPrinterName = 'Bildschirm' then
    exit; // Bildschirmdruck. Nichts machen

  Refresh; // Muss sein, da sonst nach dem CRPE1-Druck des kassenbons diese Funktionen nicht mehr laufen......
  PrinterName := aPrinterName;

  if aCommand in ['a' .. 'e', 'A' .. 'E'] then
    Title := 'CORAplus Kassenlade öffnen'
  else if aCommand = 'P' then
    Title := 'CORAplus Bon schneiden'
  else
    Title := 'CORAplus Bondrucker Befehl';

  iNoFeedNoCutSchachtIndex := PaperSources.IndexOf('Document[NoFeed,NoCut]');
  // ACHTUNG! DIESE SCHÄCHTE GIBT AB TREIBER VERSION 5 NICHT MEHR!!!
  if iNoFeedNoCutSchachtIndex >= 0 then
  // Drucken ohne "Schnipsel" ist daher nur noch über EpsStmApi.dll möglich (das ab Treiber 6 als separater Download erhältlich ist)
    PaperSourceIndex := iNoFeedNoCutSchachtIndex // (Ticket 58437)
  else
    PaperSourceIndex := aSchachtIndex;

  BeginDoc;
  // Orientation := poPortrait;
  // Canvas.Font.Size := 9;
  // Canvas.Font.Color := clBlack;
  // Canvas.Font.Style := [];

  if ContainsText(PrinterName, 'Samsung') or ContainsText(PrinterName, 'Bixolon')
  then
    Canvas.Font.Name :=
      'FontControl' { Control Function Font Name für Bixolon / Samsung SRP 350 }
  else if ContainsText(PrinterName, 'Epson') then
    Canvas.Font.Name :=
      'control' { Control Function Font Name für Epson TM-T88 }
  else
    Canvas.Font.Name := 'control'; { Keine Ahnung }

  // Anm.: Die Commands (a..e, A..E, P) für Samsung sind die gleichen wie beim Epson, siehe
  // ftp://ftp.t-innova.com/Drivers/Impresoras/Samsung/v3.1.5/Manual/Windows%20Driver%20Manual.pdf
  Canvas.TextOut(0, 0, aCommand);
  EndDoc;
end;

function hlPrinterBon: ThlPrinterBon;
begin
  if (_hlPrinterBon = nil) then
    _hlPrinterBon := ThlPrinterBon.Create;
  Result := _hlPrinterBon;
end;

function SethlPrinterBon(NewPrinter: ThlPrinterBon): ThlPrinterBon;
begin
  Result := _hlPrinterBon;
  _hlPrinterBon := NewPrinter;
end;

initialization

finalization

FreeAndNil(_hlPrinterBon);

end.
