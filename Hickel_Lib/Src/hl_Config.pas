unit hl_Config;

interface

uses Windows, SysUtils, ActiveX, Registry, hl_Exceptions, hl.Utils;

type
  // TODO: Alle CORA-Dinge sollen eigentlich in HCL und nicht in HL!
  ThlConfig = class(TObject)
  private
    _ServerPfad: string;
    // _ServerUpdateProtokollPfad: string;

    _ClientBinPfad: string;
    _ClientConfigPfad: string;
    _ClientConfigUserPfad: string;

    _ClientHT630ExportPfad: string;
    _ClientHT630PtcommPfad: string;
    _ClientHT630COmmNr: Integer;

    _ClientFibuExportPfad: string;
    _ClientLastschriftExportPfad: string;

    _ClientBrauMeldPfad : string;
    _ClientTrackPilotPfad : string;

    _VerwaltungAutoAnmeldungName: string;
    _VerwaltungAutoAnmeldungPasswort: string;

    _KasseAutoAnmeldungName: string;
    _KasseAutoAnmeldungPasswort: string;

    _TapiDeviceId: integer; // deprecated;
    _TapiDeviceName: string;
    _TapiVorwahl: string;

    _ClientMowisExportPfad: string;

    _ClientOctopusPfad: string;
    _ClientGefako2GPlusPfad: string;
    _ClientKollexPfad: string;
    _ServerEDIOrdersStandardPfad: string;
    _ClientWebshopWirDuzenPfad: string;
    _ClientDurstPfad: string;
    _ClientStammsortimentsbereinigungPfad: string;
    _ClientMisExportPfad: string;

    mExeDirectory: string;

    function isDBUpdater: boolean;
    function GetClientTrackPilotPfad: string;
    function GetServerConfigPfad: string;

  protected
    FSpecialLogWarningGiven: boolean;

    function GetServerUpdateProtokollPfad: string;
    function GetClientLogPfad: string;
    function GetClientFibuExportPfad: string;
    function GetClientLastschriftExportPfad: string;
  public
    constructor Create(aExeDirectory: string);

    function GetServerLogPath(title: string): string;

    property ServerPfad: string read _ServerPfad;
    property ServerUpdateProtokollPfad: string read GetServerUpdateProtokollPfad;
    property ClientBinPfad: string read _ClientBinPfad;
    property ClientLogPfad: string read GetClientLogPfad;
    property ClientConfigPfad: string read _ClientConfigPfad;
    property ClientConfigUserPfad: string read _ClientConfigUserPfad;

    // TODO: das sollte eigentlich in hcl
    property ClientHT630ExportPfad: string read _ClientHT630ExportPfad;
    property ClientHT630PtcommPfad: string read _ClientHT630PtcommPfad;
    property ClientHT630COmmNr: integer read _ClientHT630COmmNr;

    property ClientFibuExportPfad: string read GetClientFibuExportPfad;
    property ClientLastschriftExportPfad: string read GetClientLastschriftExportPfad;
    property ClientBrauMeldPfad : string read _ClientBrauMeldPfad;
    property ClientTrackPilotPfad : string read GetClientTrackPilotPfad;

    property VerwaltungAutoAnmeldungName: string read _VerwaltungAutoAnmeldungName;
    property VerwaltungAutoAnmeldungPasswort: string read _VerwaltungAutoAnmeldungPasswort;

    property KasseAutoAnmeldungName: string read _KasseAutoAnmeldungName;
    property KasseAutoAnmeldungPasswort: string read _KasseAutoAnmeldungPasswort;

    property TapiDeviceId: Integer read _TapiDeviceId; // deprecated;
    property TapiDeviceName: string read _TapiDeviceName;
    property TapiVorwahl: string read _TapiVorwahl;

    property MowisExportPfad: string read _ClientMowisExportPfad;

    property ClientOctopusPfad: string read _ClientOctopusPfad;
    property ClientGefako2GPlusPfad: string read _ClientGefako2GPlusPfad;
    property ClientKollexPfad: string read _ClientKollexPfad;
    property ServerEDIOrdersStandardPfad: string read _ServerEDIOrdersStandardPfad;
    property ClientWebshopWirDuzenPfad: string read _ClientWebshopWirDuzenPfad;
    property ClientDurstPfad: string read _ClientDurstPfad;
    property ClientStammsortimentsbereinigungPfad: string read _ClientStammsortimentsbereinigungPfad;
    property ClientMisExportPfad: string read _ClientMisExportPfad;

    property ServerConfigPfad: string read GetServerConfigPfad;

    property ExeDirectory: string read mExeDirectory;

    procedure Reload; virtual;

  end;

implementation

uses
  HsTools, MessaBox, XMLDoc, classes, IniFiles, hl.Utils.StringStreamEx;

procedure RemoveLastPart(var s: string);
begin
  repeat
    Delete(s, length(s),1)
  until (s[length(s)] = '\') or (s = '');
end;

function _PathCor(path: string): string;
var
  test: string;
begin
  result := IncludeTrailingPathDelimiter(path);

  // Es gibt an vielen Stellen des Programms Fehler, wenn der Pfad relativ ist
  // z.B. bei einem Kunden bei Brauereimeldungen, da wird der Pfad irgendwie mittendrin geändert zu Microsoft Office! (Wird irgendwo CWD geändert?)
  if Copy(result, 1, 12) = '..\..\..\..\' then
    test := ThlUtils.GetParentDir(ThlUtils.GetParentDir(ThlUtils.GetParentDir(ThlUtils.GetParentDir(ExtractFilePath(ParamStr(0)))))) + Copy(result,13,Length(result)-12)
  else if Copy(result, 1, 9) = '..\..\..\' then
    test := ThlUtils.GetParentDir(ThlUtils.GetParentDir(ThlUtils.GetParentDir(ExtractFilePath(ParamStr(0))))) + Copy(result,10,Length(result)-9)
  else if Copy(result, 1, 6) = '..\..\' then
    test := ThlUtils.GetParentDir(ThlUtils.GetParentDir(ExtractFilePath(ParamStr(0)))) + Copy(result,7,Length(result)-6)
  else if Copy(result, 1, 3) = '..\' then
    test := ThlUtils.GetParentDir(ExtractFilePath(ParamStr(0))) + Copy(result,4,Length(result)-3);
  if DirectoryExists(test) then result := test;
end;

constructor ThlConfig.Create(aExeDirectory: string);
begin
  inherited Create;
  mExeDirectory := aExeDirectory;
  Reload;
end;

function ThlConfig.GetServerConfigPfad: string;
begin
  result := IncludeTrailingPathDelimiter(_ServerPfad) + 'Config\';
end;

function ThlConfig.GetServerLogPath(title: string): string;
begin
  result := IncludeTrailingPathDelimiter(ServerPfad) + 'Logs\';
  if title <> '' then
  begin
    result := result + title + '\';
    // TODO: prüfen ob title ein gültiger Dateiname ist
  end;
  if not ForceDirectories(result) then
  begin
    if not FSpecialLogWarningGiven then
    begin
      FSpecialLogWarningGiven := true;
      HsShowMessage('Kann Protokoll-Datei nicht auf Server-Verzeichnis schreiben. Log-Datei wird auf lokales Verzeichnis geschrieben.', 'Bereinigungs-Assistent', mbsExclamation, mbbOk);
    end;
    result := IncludeTrailingPathDelimiter('Log');
    if not ForceDirectories(result) then
    begin
      result := '';
    end;
  end;
end;


procedure ThlConfig.Reload;
var
  x: TXmlDocument;
  aComponent: TComponent;
  ss: TStringStream;
  _ClientConfigPfadOld: string;
  _ClientConfigPfadNew: string;

begin
  inherited Create;

  // (Ticket 32858) Damit SystemDBUpdates auch im UNC-Verzeichnis aufgerufen werden kann... (warum auch immer...)
  if UpperCase(GetDeepestDir(IncludeTrailingPathDelimiter(mExeDirectory))) = 'DOWNLOADS' then
  begin
    _ServerPfad := IncludeTrailingPathDelimiter(mExeDirectory);
    RemoveLastPart(_ServerPfad);
    exit;
  end;

  _ClientBinPfad := IncludeTrailingPathDelimiter(mExeDirectory);

  _ClientConfigPfadOld := _ClientBinPfad + 'Config\';
  _ClientConfigPfadNew := _ClientBinPfad + '..\Config\';
  if DirectoryExists(_ClientConfigPfadNew) then
  begin
    _ClientConfigPfad := _ClientConfigPfadNew;
  end
  else
  begin
    _ClientConfigPfad := _ClientConfigPfadOld;
  end;

  if not FileExists(_ClientConfigPfad + 'Hickel.Config.Xml') then
  begin
    ExitCode := 1; // damit die ALU einen Fehler an HS meldet!
    HSShowMessage('Konfigurations-Ordner wurde nicht gefunden. CORAplus kann nicht starten.', 'CORAplus Fehler', mbsStop);
    Halt;
  end;
  if not (DirectoryExists(_ClientConfigPfad)) then mkdir(_ClientConfigPfad);

  aComponent := TComponent.Create(nil);
  x := TXMLDocument.Create(aComponent);
  try
    ss := TStringStream.Create;
    try
      ss.LoadFromFile(_ClientConfigPfad + 'Hickel.Config.Xml');
      x.LoadFromStream(ss); // DM 18.04.2017: Ich verwende nicht x.LoadFromFile, da TXMLDocument nicht in der Lage ist, einen UNC Pfad zu öffnen! (Die Datei-Öffnungs-Routine scheint nicht in Delphi geschrieben zu sein)
      x.Active := true;
    finally
      FreeAndNil(ss);
    end;

    _ServerPfad := _PathCor(x.ChildNodes['Hickel'].ChildNodes ['Config'].childNodes['Server'].Text);

    if isDBUpdater then
    begin
      _ClientBinPfad := '';
    end
    else // DM 18.04.2017: Der DB-Updater soll unabhängig von BAP-Einstellungen sein, daher nicht laden
    begin
      _VerwaltungAutoAnmeldungName := x.ChildNodes['Hickel'].ChildNodes ['Config'].childNodes['VName'].Text;
      _VerwaltungAutoAnmeldungPasswort := x.ChildNodes['Hickel'].ChildNodes ['Config'].childNodes['VPasswort'].Text;

      _KasseAutoAnmeldungName := x.ChildNodes['Hickel'].ChildNodes ['Config'].childNodes['KName'].Text;
      _KasseAutoAnmeldungPasswort := x.ChildNodes['Hickel'].ChildNodes ['Config'].childNodes['KPasswort'].Text;

      if x.ChildNodes['Hickel'].ChildNodes ['Config'].childNodes['TapiDeviceId'].Text = '' then
        _TapiDeviceId := -1
      else
        _TapiDeviceId := StrToInt (x.ChildNodes['Hickel'].ChildNodes ['Config'].childNodes['TapiDeviceId'].Text);
      _TapiDeviceName := x.ChildNodes['Hickel'].ChildNodes ['Config'].childNodes['TapiDeviceName'].Text;
      if x.ChildNodes['Hickel'].ChildNodes ['Config'].childNodes['TapiAmt'] = nil then // Eingeführt mit 6.5.64
        _TapiVorwahl := ''
      else
        _TapiVorwahl := x.ChildNodes['Hickel'].ChildNodes ['Config'].childNodes['TapiAmt'].Text;

      _ClientFibuExportPfad := _PathCor(x.ChildNodes['Hickel'].ChildNodes ['Fibu'].childNodes['ExportPfad'].Text);
      _ClientLastschriftExportPfad := _PathCor(x.ChildNodes['Hickel'].ChildNodes ['Lastschrift'].childNodes['ExportPfad'].Text);
      _ClientHT630ExportPfad := _PathCor(x.ChildNodes['Hickel'].ChildNodes ['HT630'].childNodes['ExportPfad'].Text);
      _ClientHT630PtcommPfad := _PathCor(x.ChildNodes['Hickel'].ChildNodes ['HT630'].childNodes['PTCOMMPfad'].Text);

      if (Not TryStrToInt( x.ChildNodes['Hickel'].ChildNodes ['HT630'].childNodes['COM-Nr'].Text, _ClientHT630COmmNr)) then _ClientHT630COmmNr := 1;

      _ClientBrauMeldPfad := _PathCor(x.ChildNodes['Hickel'].ChildNodes ['BrauMeld'].childNodes['ExportPfad'].Text);

      _ClientMowisExportPfad := _PathCor(x.ChildNodes['Hickel'].ChildNodes ['Mowis'].childNodes['ExportPfad'].Text);

      // TODO: nicht hartkodiert, sondern irgendwo konfigurierbar speichern!
      _ClientOctopusPfad := _PathCor('..\InOut\_Octopus');
      _ClientGefako2GPlusPfad := _PathCor('..\InOut\2G+'); // siehe PHP Script für 2G+ Bestell-Import
      _ClientKollexPfad := _PathCor('..\InOut\_Kollex');
      _ServerEDIOrdersStandardPfad := _PathCor(_ServerPfad + 'EDI Orders\');
      _ClientWebshopWirDuzenPfad := _PathCor('..\InOut\_WebshopExtern');
      _ClientDurstPfad := _PathCor('..\InOut\_Durststrecke');
      _ClientStammsortimentsbereinigungPfad := _PathCor('..\InOut\_Stammsortimentsbereinigung');
      _ClientMisExportPfad := _PathCor('..\InOut\_MIS');
      _ClientTrackPilotPfad := _PathCor ('..\InOut\TrackPilot');
    end;
  finally
    FreeAndNil(x);
    FreeAndNil(aComponent);
  end;

  _ClientConfigUserPfad := _ClientConfigPfad + 'User\'; if (not (DirectoryExists(_ClientConfigUserPfad))) then mkdir(_ClientConfigUserPfad);

  if not DirectoryExists(_ServerPfad) then
  begin
    ExitCode := 1; // damit die ALU einen Fehler an HS meldet!
    // TODO: als stacktrace melden
    HSShowMessage(_ClientConfigPfad + 'Hickel.Config.xml: ServerPfad "' + _ServerPfad + '" nicht existent oder Zugriff verweigert!', 'CORAplus Fehler', mbsStop);
    Halt;
  end;

  // TODO: Fehlermeldung und Programmabbruch anstelle seltsame Abstürze, wenn EXE nicht existiert.
end;

function ThlConfig.GetServerUpdateProtokollPfad: string;
var
  s: string;
begin
  s := ServerPfad + 'Logs\UpdateProtokolle\';
  if not DirectoryExists(s) then mkdir(s);
  result := s;
end;

function ThlConfig.isDBUpdater: boolean;
begin
  result := (UpperCase(ExtractFileName(ParamStr(0))) = 'SYSTEM_DBUPDATES.EXE') or (UpperCase(ExtractFileName(ParamStr(0))) = 'SYSTEM_DBUPDATES64.EXE');
end;

function ThlConfig.GetClientLogPfad: string;
var
  s: string;
begin
  if isDBUpdater then
    s := ServerPfad + 'Logs\UpdateProtokolle\'
  else if DirectoryExists(ClientBinPfad + '..\Log\') then
    s := ClientBinPfad + '..\Log\'
  else
    s := ClientBinPfad + 'Log\';
  if not DirectoryExists(s) then mkdir(s);
  result := s;
end;

function ThlConfig.GetClientTrackPilotPfad: string;
begin
  if ( copy (_ClientTrackPilotPfad, length (_ClientTrackPilotPfad), 1) <> '\') then _ClientTrackPilotPfad := _ClientTrackPilotPfad + '\';
  if (not DirectoryExists(_ClientTrackPilotPfad)) then mkdir (_ClientTrackPilotPfad);
  result := _ClientTrackPilotPfad;
end;

function ThlConfig.GetClientFibuExportPfad;
begin
  if ( copy (_ClientFibuExportPfad, length (_ClientFibuExportPfad), 1) <> '\') then _ClientFibuExportPfad := _ClientFibuExportPfad + '\';
  if (not DirectoryExists(_ClientFibuExportPfad)) then mkdir (_ClientFibuExportPfad);
  result := _ClientFibuExportPfad;
end;

function ThlConfig.GetClientLastschriftExportPfad;
begin
  if ( copy (_ClientLastschriftExportPfad, length (_ClientLastschriftExportPfad), 1) <> '\') then _ClientLastschriftExportPfad := _ClientLastschriftExportPfad + '\';
  if (not DirectoryExists(_ClientLastschriftExportPfad)) then mkdir (_ClientLastschriftExportPfad);
  result := _ClientLastschriftExportPfad;
end;

initialization
  // Wird verwendet für MSXML
  CoInitialize (nil); // steht in uses:ActiveX

finalization
  CoUninitialize;
end.
