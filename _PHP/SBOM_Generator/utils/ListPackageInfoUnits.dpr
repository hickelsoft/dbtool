program ListPackageInfoUnits;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.IOUtils,
  System.JSON,
  System.Classes,
  Winapi.Windows;

type
  TUnitList = class
  private
    FUnits: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddUnit(const Name: string);

    function ToJSONArray: TJSONArray;
    function ToJSON: string;

    property Units: TStringList read FUnits;
  end;

constructor TUnitList.Create;
begin
  inherited Create;

  FUnits := TStringList.Create;

  // Keine doppelten Units
  FUnits.Sorted := True;
  FUnits.Duplicates := dupIgnore;
end;

destructor TUnitList.Destroy;
begin
  FUnits.Free;
  inherited;
end;

procedure TUnitList.AddUnit(const Name: string);
begin
  if Name <> '' then
    FUnits.Add(Name);
end;

function TUnitList.ToJSONArray: TJSONArray;
var
  UnitName: string;
begin
  Result := TJSONArray.Create;

  for UnitName in FUnits do
    Result.Add(UnitName);
end;

function TUnitList.ToJSON: string;
var
  JSONArray: TJSONArray;
begin
  JSONArray := ToJSONArray;
  try
    Result := JSONArray.ToJSON;
  finally
    JSONArray.Free;
  end;
end;

// WICHTIG:
// TPackageInfoProc ist ein "normaler" Prozedurtyp.
// Deshalb darf hier keine Klassenmethode verwendet werden.
procedure PackageInfoCallback(
  const Name: string;
  NameType: TNameType;
  Flags: Byte;
  Param: Pointer);
var
  UnitList: TUnitList;
begin
  if NameType <> ntContainsUnit then
    Exit;

  UnitList := TUnitList(Param);

  if Assigned(UnitList) then
    UnitList.AddUnit(Name);
end;

procedure ProcessBPL(const FileName: string);
var
  Module: HMODULE;
  PackageFlags: Integer;
  UnitList: TUnitList;
begin
  Writeln;
  Writeln('"', ExtractFileName(FileName), '": ');

  Module := LoadLibraryEx(
    PChar(FileName),
    0,
    LOAD_LIBRARY_AS_DATAFILE
  );

  if Module = 0 then
  begin
    Writeln('  FEHLER: BPL konnte nicht geladen werden.');
    Writeln('  Windows-Fehler: ', GetLastError);
    Exit;
  end;

  UnitList := TUnitList.Create;
  try
    PackageFlags := 0;

    try
      GetPackageInfo(
        Module,
        UnitList,
        PackageFlags,
        PackageInfoCallback
      );
      WriteLn(UnitList.ToJSON);
    except
      on E: EAbort do
      begin
        Abort;
      end;
      on E: Exception do
      begin
        Writeln(
          '  FEHLER beim Lesen der PACKAGEINFO: ',
          E.ClassName,
          ': ',
          E.Message
        );
      end;
    end;

  finally
    UnitList.Free;
    FreeLibrary(Module);
  end;
end;

procedure ProcessDirectory(const dir: string);
var
  Files: TArray<string>;
  FileName: string;
  first: boolean;
begin
  if not TDirectory.Exists(dir) then
  begin
    Writeln('Verzeichnis nicht gefunden:');
    Writeln(dir);
    Exit;
  end;

//  Writeln('Suche BPL-Dateien in:');
//  Writeln(BPLDirectory);
//  Writeln;

  Files := TDirectory.GetFiles(
    dir,
    '*.bpl',
    TSearchOption.soTopDirectoryOnly
  );

//  Writeln('Gefundene BPLs: ', Length(Files));

  WriteLn('{');

  first := true;
  for FileName in Files do
  begin
    if first then
      first := false
    else
      WriteLn(',');
    ProcessBPL(FileName);
  end;
  WriteLn('}');
end;

begin
  try
    if ParamCount = 1 then
      ProcessDirectory(ParamStr(1))
    else
      WriteLn('Syntax: ' + ExtractFileName(ParamStr(0)) + ' delphi-bin-dir')

//    Writeln;
//    Writeln('Fertig.');
  except
    on E: EAbort do
    begin
      Abort;
    end;
    on E: Exception do
    begin
      Writeln;
      Writeln('FATALER FEHLER:');
      Writeln(E.ClassName, ': ', E.Message);
    end;
  end;

//  Writeln;
//  Writeln('ENTER zum Beenden...');
//  Readln;
end.
