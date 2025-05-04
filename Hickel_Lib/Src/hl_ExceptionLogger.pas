unit hl_ExceptionLogger;

// TODO: Eine API zur Verfügung stellen, mit der Anwendungen eigene Felder
// zum Stacktrace hinzufügen können (z.B. MandantNr etc).

interface

uses
  Classes, SysUtils, hl_Log, hl.Utils, hl_Exceptions;

type
  EHlBedienfehler = class(ThlException);
  // für diese Exceptions wird kein Stacktrace erstellt

  ThlExceptionLogger = record
    /// <summary>Speichert alle notwendigen Informationen über eine Exception, inklusive CallStack (benötigt JEDI).</summary>
    /// <remarks>Damit der Call-Stack funktioniert, muss "Mapping" in den Linker-Einstellungen auf Detailliert gestellt werden. Weitere Informationen im Ordner "Dokumentation Entwicklung".</remarks>
    /// <author>Daniel Marschall</author>
    class procedure LogException(e: Exception;
      hlExceptionLog: ThlLog = nil); static;
  end;

implementation

uses
  // Delphi 2007:
  // Für die Unit JclDebug muss JEDI Installiert sein (VCL_JEDI\Install.bat ausführen und Anweisungen folgen).
  // Manchmal, wenn er hier hängt, muss auch einfach nur Delphi neu gestartet werden.
  // Delphi 11:
  // JEDI über GetIt PackageManager installieren,
  // danach install.bat aufrufen über c:\Users\dmarschall.HICKELSOFT\Documents\Embarcadero\Studio\22.0\CatalogRepository\JEDICodeLibraryJCL-2022.02\
  JclDebug, Windows, Dateutils;

var
  UniqueProcessGUID: TGUID;
  StartTime: TDateTime;

  { ThlExceptionLogger }

class procedure ThlExceptionLogger.LogException(e: Exception;
  hlExceptionLog: ThlLog = nil);
var
  slCallStack: TStringList;
  i: integer;
  tempAssign: boolean;
  slText: TStringList;
  m: integer;
  UniqueExceptionGUID: TGUID;
  dateidatum: TDateTime;
  sDateiDatum: string;
begin
  if e is EAbort then
    exit;
  if e is EHlBedienfehler then
    exit;

  tempAssign := not Assigned(hlExceptionLog);
  if tempAssign then
  begin
    hlExceptionLog := ThlLog.Create;
  end;
  try
    CreateGUID(UniqueExceptionGUID);

    if FileAge(ParamStr(0), dateidatum) then
    begin
      sDateiDatum := DateTimeToStr(dateidatum);
    end
    else
    begin
      sDateiDatum := '???';
    end;

    hlExceptionLog.
      Write('------------------------------------ A U S N A H M E F E H L E R ----------------------------------');
    // TODO: Stacktrace-Dateiname auch mitangeben
    hlExceptionLog.Write('Fehlerzeitp.: ' + DateTimeToStr(Now));
    hlExceptionLog.Write('');
    hlExceptionLog.Write('Anwendung:    ' + ParamStr(0));
    hlExceptionLog.Write('Dateiversion: ' + ThlUtils.GetFileVersion
      (ParamStr(0)));
    hlExceptionLog.Write('Dateidatum:   ' + sDateiDatum);
    hlExceptionLog.Write('');
    hlExceptionLog.Write('Prozess ID:   ' + IntToStr(GetCurrentProcessId));
    hlExceptionLog.Write('Thread ID:    ' + IntToStr(GetCurrentThreadId));
    hlExceptionLog.Write('Prozess GUID: ' + GUIDToString(UniqueProcessGUID));
    hlExceptionLog.Write('Except. GUID: ' + GUIDToString(UniqueExceptionGUID));
    hlExceptionLog.Write('');

    m := SecondsBetween(Now, StartTime);
    hlExceptionLog.Write('Laufzeit:     ' + Format('%2.2d:%2.2d:%2.2d',
      [m div 3600, m mod 3600 div 60, m mod 60]));

    for i := 1 to ParamCount do
    begin
      hlExceptionLog.Write('Parameter:    ' + ParamStr(i));
    end;

    hlExceptionLog.Write('');
    hlExceptionLog.Write('Fehlerklasse: ' + e.ClassName);
    slText := TStringList.Create;
    try
      slText.Text := e.Message;
      for i := 0 to slText.Count - 1 do
      begin
        if i = 0 then
          hlExceptionLog.Write('Fehlertext:   ' + slText.Strings[i])
        else
          hlExceptionLog.Write('              ' + slText.Strings[i]);
      end;
    finally
      FreeAndNil(slText);
    end;
    hlExceptionLog.Write('');
    slCallStack := TStringList.Create;
    try
      // Siehe Beschreibung im "Dokumentation Entwicklung" Verzeichnis
      JclLastExceptStackListToStrings(slCallStack, False, True, True, False);
      for i := 0 to slCallStack.Count - 1 do
      begin
        if i = 0 then
          hlExceptionLog.Write('Aufruf-Stack: ' + slCallStack.Strings[i])
        else
          hlExceptionLog.Write('              ' + slCallStack.Strings[i]);
      end;
    finally
      FreeAndNil(slCallStack);
    end;
    hlExceptionLog.
      Write('---------------------------------------------------------------------------------------------------');
    hlExceptionLog.Write('');
  finally
    if tempAssign then
    begin
      FreeAndNil(hlExceptionLog);
    end;
  end;
end;

initialization

Include(JclStackTrackingOptions, stRawMode); // TODO: mehr Optionen?
Include(JclStackTrackingOptions, stStaticModuleList);
JclStartExceptionTracking;

CreateGUID(UniqueProcessGUID);
StartTime := Now;

finalization

JclStopExceptionTracking;

end.
