unit Globals;

{$INCLUDE 'Globals.inc'}

interface

uses
  Windows, Controls, Graphics, SysUtils, Classes;

var
  clTableText: TColor = $00000000; // RGB($00, $00, $00)
  clTableBackground: TColor = $00FFFFFF; // RGB($FF, $FF, $FF)
  clTableZebra: TColor = $00E0FFFF; // RGB($FF, $FF, $E0)
  clActiveRecord: TColor = $00DED3D6; // RGB($D6, $D3, $DE)
  clActiveField: TColor = $0080FFFF; // RGB($FF, $FF, $80)
  clNullField: TColor = $00DDDDFF; // Schweinchen Rosa

const
  ConfigRegKey = '\Software\HickelSOFT\DBTool'; // do not localize

procedure FixSpeedButtonColors(control: TWinControl; color: TColor);
function OEM2Ansi(Value: AnsiString): AnsiString;
function GetSQLServerDBListFilename: string;
function GetMySQLDBListFilename: string;

// Diese Funktionen sind zur Unterscheidung von HickelSOFT-Produkten:
function Modus_HsInfo2_Verzeichnis: boolean;
function Modus_CORA_Verzeichnis: boolean;

implementation

uses
  Buttons, Dialogs, hl.Utils, HS_Auth;

procedure FixSpeedButtonColors(control: TWinControl; color: TColor);
var
  i: integer;
begin
  for i := 0 to control.ControlCount - 1 do
  begin
    if control.Controls[i].ClassType = TSpeedButton then
    begin
      if TSpeedButton(control.Controls[i]).Flat then
      begin
        if TSpeedButton(control.Controls[i]).Down then
        begin
          TSpeedButton(control.Controls[i]).Font.color := clWindowText;
        end
        else
        begin
          TSpeedButton(control.Controls[i]).Font.color := color;
        end
      end
    end
  end
end;

function OEM2Ansi(Value: AnsiString): AnsiString;
begin
  // http://www.delphipraxis.net/152163-console-oem-nach-ansi.html
  Result := Value;
  OEMToCharA(PAnsiChar(Value), PAnsiChar(Result));
end;

function GetSQLServerDBListFilename: string;
var
  outDir: string;
begin
  // Das ist der Standard von CORAplus
  // Bitte Synchron halten zwischen DBTool (Globals.pas) und CORA Debugger (DebugMain.pas)
  if Modus_CORA_Verzeichnis then
  begin
    Result := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) +
      '..\Config\DbToolDatenbanken.txt';
    if FileExists(Result) then
      exit;
  end;

  // Backwards compatibility to HS-Info 1.0
  if IstHickelSoftTestPC then
  begin
    Result := 'C:\HS-Service\DbToolDatenbanken.txt';
    if FileExists(Result) then
      exit;
  end;

  // Das ist der Standard von DBTool Vollversion
  outDir := SysUtils.GetEnvironmentVariable('APPDATA');
  // das ist das Appdata\Roaming Verzeichnis // do not localize
  if outDir <> '' then
    outDir := IncludeTrailingPathDelimiter(outDir) +
      'HickelSOFT\DBTool\Config\';
  // ForceDirectories(outDir);
  Result := IncludeTrailingPathDelimiter(outDir) + 'SQLServerList.txt';
end;

function GetMySQLDBListFilename: string;
var
  outDir: string;
begin
  outDir := SysUtils.GetEnvironmentVariable('APPDATA');
  // das ist das Appdata\Roaming Verzeichnis // do not localize
  if outDir <> '' then
    outDir := IncludeTrailingPathDelimiter(outDir) +
      'HickelSOFT\DBTool\Config\';
  // ForceDirectories(outDir);
  Result := outDir + 'MySQLServerList.txt';
end;

function Modus_HsInfo2_Verzeichnis: boolean;
begin
  Result := FileExists(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))
    ) + 'HsInfo2.exe') or // do not localize
    FileExists(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) +
    'HsInfo32.exe') or // do not localize
    FileExists(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) +
    'HsInfo64.exe'); // do not localize
end;

function Modus_CORA_Verzeichnis: boolean;
begin
  Result := FileExists(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))
    ) + 'CORA_Verwaltung.exe') or // do not localize
    FileExists(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) +
    'CORA_Verwaltung64.exe'); // do not localize
end;

end.
