unit hl_Log;

interface

uses
  SysUtils, Windows;

type
  ThlLog = class(TObject)
  private
    _FileName: string;
  public
    constructor Create(logfile: string=''; noAutoDetect: boolean=false);
    procedure Write(text: string);  overload;
    procedure Delete;
    property FileName: string read _FileName write _FileName;

    class function DefaultLogDir: string;
  end;

implementation

uses
  SyncObjs, Classes;

var
  CSLog: TCriticalSection;

class function ThlLog.DefaultLogDir: string;
var
  exePath: string;
  slTmp: TStringList;
const
  HINWEIS_TXT = '__HINWEIS__.TXT';
begin
  {$REGION 'Deprecated log dir Warnung einfügen'}
  result := '';
  exePath := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
  (* if Assigned(hclConfig) then
  begin
    result := IncludeTrailingPathDelimiter(hclCore.Config.ClientLogPfad);
  end
  else
  *)
  if DirectoryExists(exePath + '..\Log') then
  begin
    result := exePath + '..\Log\';
  end
  else if DirectoryExists(exePath + 'Log') then
  begin
    result := exePath + 'Log\';
  end
  else if DirectoryExists('..\Log') then
  begin
    result := '..\Log\';
  end
  else if DirectoryExists('Log') then
  begin
    result := 'Log\';
  end;
  if not FileExists(result + HINWEIS_TXT) then
  begin
    try
      slTmp := TStringList.Create;
      try
        slTmp.Text := 'Wichtiger Hinweis: Logs werden ab sofort pro Windows User gespeichert unter: '+#13#10+
                      'C:\Users\...\AppData\Local\HickelSOFT\Logs\';
        slTmp.SaveToFile(result + HINWEIS_TXT);
      finally
        FreeAndNil(slTmp);
      end;
    except
      on E: EAbort do
      begin
        Abort;
      end;
      on E: Exception do
      begin
        // ignore
      end;
    end;
  end;
  {$ENDREGION}

  result := SysUtils.GetEnvironmentVariable('APPDATA');
  if result <> '' then
  begin
    result := IncludeTrailingPathDelimiter(result) + '..\Local\HickelSOFT\Logs\';
    ForceDirectories(result);
  end
  else
    result := exePath; // In das EXE-Verzeichnis schreiben
end;

procedure ThlLog.Delete;
begin
  CSLog.Enter;
  try
    if FileExists(_FileName) then SysUtils.DeleteFile(_FileName);
  finally
    CSLog.Leave;
  end;
end;

constructor ThlLog.Create(logfile: string=''; noAutoDetect: boolean=false);
begin
  CSLog.Enter;
  try
    if logfile = '' then
    begin
      logfile := ExtractFileName(ChangeFileExt(ParamStr(0), '.log'));
    end;

    if not noAutoDetect and
       (Pos('\\', logfile) = 0) and // UNC Pfad
       (Pos('..', logfile) = 0) and // Relativer Pfad außerhalb bin\
       (Pos(':',  logfile) = 0) and // Absoluter Pfad
       not FileExists(logfile) then
    begin
      _FileName := DefaultLogDir + logfile;
    end
    else
    begin
      _FileName := logfile;
    end;
  finally
    CSLog.Leave;
  end;
end;

procedure ThlLog.Write(text: string);
var
  logFile: TextFile;
begin
  CSLog.Enter;
  try
    try
      AssignFile(logFile, _filename);
      try
        // TODO: Wenn die Datei sehr groß wird, dann wird Append() sehr, sehr langsam! Deshalb sollte mit StringStreams gearbeitet werden. (Siehe SCC3)
        if not FileExists(_filename) then
          Rewrite(logFile)
        else
          Append(logFile);
        Writeln(logFile, DateTimeToStr(Now) + ' - ' + text);
      finally
        CloseFile(logFile);
      end;
    except
      // DM 29.02.2016
      // Große Ausnahme: Wenn hier was fehlschlägt, ignorieren wir es.
      // Denn Fehler in der Fehlerbehandlungsroutine (mit Logs) sind tödlich.
      on E: EAbort do
      begin
        Abort;
      end;
      on E: Exception do
      begin
        // ignore
      end;
    end;
  finally
    CSLog.Leave;
  end;
end;

initialization
  CSLog := TCriticalSection.Create;

finalization
  FreeAndNil(CSLog);

end.
