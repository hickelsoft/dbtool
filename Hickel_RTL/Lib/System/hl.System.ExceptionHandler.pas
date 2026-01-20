unit hl.System.ExceptionHandler;

interface

uses
  SysUtils, Forms;

type
  ThlExceptionHandler = class(TObject)
  private
    class var FExceptionHandler: TExceptionEvent;
  protected
    class procedure DoException(Sender: TObject; E: Exception);
  public
    class procedure ManualDoException(E: Exception);
    class procedure ErstelleStacktrace(E: Exception);
    class procedure Init(AExceptionHandler: TExceptionEvent = nil);
    class procedure Uninit;
  end;

implementation

uses
  hl_Log, StrUtils, hl_ExceptionLogger, MessaBox, Windows, HS_Mitteilung;

class procedure ThlExceptionHandler.ErstelleStacktrace(E: Exception);
var
  hlExceptionLog: ThlLog;
  logfile: string;
begin
  try
    // logfile := ChangeFileExt(ExtractFileName(ParamStr(0)), '_Exception.txt');

    // Dateiname, z.B. EXCEPTION__2016_04_08__08_00_00__123__CORA_Verwaltung__EOleException.txt
    logfile := 'EXCEPTION__' + formatdatetime('yyyy_mm_dd__hh_nn_ss__zzz', Now)
      + '__' + ChangeFileExt(ExtractFileName(ParamStr(0)), '') + '__' +
      E.ClassName + '.txt';
    hlExceptionLog := ThlLog.Create(logfile);
    try
      ThlExceptionLogger.LogException(E, hlExceptionLog);
      THsMitteilung.SendeStacktraceAnHickelSOFT(hlExceptionLog.FileName);
    finally
      FreeAndNil(hlExceptionLog);
    end;
  except
    // DM 29.02.2016
    // Große Ausnahme: Wenn hier was fehlschlägt, ignorieren wir es.
    // Denn Fehler in der Fehlerbehandlungsroutine (mit Logs) ist tödlich.
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

class procedure ThlExceptionHandler.Init(AExceptionHandler
  : TExceptionEvent = nil);
begin
  Application.OnException := DoException;
  FExceptionHandler := AExceptionHandler;
end;

class procedure ThlExceptionHandler.ManualDoException(E: Exception);
begin
  DoException(nil, E);
end;

class procedure ThlExceptionHandler.Uninit;
begin
  Application.OnException := nil;
end;

class procedure ThlExceptionHandler.DoException(Sender: TObject; E: Exception);
begin
  // Application.ShowException(E);  // Das wäre die Standardbehandlung!

  // EAbort Exceptions verwenden wir, wenn wir z.B. eine laufende Auswertung oder
  // eine ausstehende Datenbankabfrage kontrolliert abbrechen möchten. Diese Exception
  // unterbricht den Programmablauf, aber sie soll dem Benutzer nicht angezeigt werden.
  if E is EAbort then
    Exit;

  if not(E is EHlBedienfehler) then
    ErstelleStacktrace(E);

  if Assigned(FExceptionHandler) then
    FExceptionHandler(nil, E)
  else if ContainsText(ExtractFileName(ParamStr(0)), 'CORA') then
    TMessageBox.ZeigeException(E)
  else if Assigned(Screen) and Assigned(Screen.ActiveForm) then
    MessageBox(Screen.ActiveForm.Handle, PChar(E.Message),
      PChar(Application.Title), MB_ICONERROR or MB_OK)
  else
    MessageBox(0, PChar(E.Message), PChar(Application.Title), MB_TASKMODAL or
      MB_ICONERROR or MB_OK);
end;

end.
