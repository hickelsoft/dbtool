unit hl.System.SingleInstance;

interface

uses
  Forms, Windows, Messages, HsTools;

/// <summary>Sorgt dafür, dass die Anwendung nur einmal geöffnet ist, und holt die Anwendung andernfalls in den Vordergrund.</summary>
/// <remarks><b>WICHTIG!</b><br />
/// 1. Diese Funktion muss aufgerufen werden, NACHDEM Application.Initialize, Application.MainFormOnTaskbar und Application.Title gesetzt wurden!<br />
/// 2. Application.Titel muss gesetzt sein, und muss einzigartig sein.</remarks>
procedure HsSemaphoreCheck;

implementation

uses
  hl.System.SingleInstance.DummyForm, Registry, SysUtils, MessaBox,
  hl_KeyValue_Args, hl_WinMessages, hl.Utils.Crc32;

var
  mHandle: THandle;

procedure CloseSemaphoreHandleIfExists;
begin
  if mHandle <> 0 then CloseHandle(mHandle);
end;

procedure HsSemaphoreCheck;
var
  hWndAndereInstanz: HWND;
  Unique_AppTitle: string;
  reg: TRegistry;
  Mandant: string;
begin
  if Application.Title = '' then
  begin
    HsShowMessage('Interner Fehler! Application.Title muss gesetzt sein, um HsSemaphoreCheck verwenden zu können!');
    Exit;
  end;

  {$REGION 'Debug'}
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if reg.OpenKeyReadOnly('Software\HickelSOFT\CORAplus\Debug') then
    begin
      if reg.ValueExists('IgnoreMutex') and reg.ReadBool('IgnoreMutex') then exit;
      reg.CloseKey;
    end;
  finally
    FreeAndNil(reg);
  end;
  {$ENDREGION}

  if Pos('CORA',UpperCase(ParamStr(0))) > 0 then
  begin
    {$REGION 'ThclCore.StandardMandant nachbauen'}
    Mandant := hl_GetParamValueByKey('MANDANT');
    if Mandant = '' then Mandant := '1';
    {$ENDREGION}

    Unique_AppTitle := Application.Title + ' Mandant ' + Mandant;
  end
  else
  begin
    Unique_AppTitle := Application.Title;
  end;

  SetWindowText(Application.Handle, PChar(Unique_AppTitle));

  CloseSemaphoreHandleIfExists;
  mHandle := CreateMutex(nil, True, PChar('urn:oid:1.3.6.1.4.1.56776.151.3:' + Unique_AppTitle));
  if GetLastError = ERROR_ALREADY_EXISTS then
  begin
    // Wir ändern unseren Application.Title, damit wir uns nicht selbst finden.
    // Dieser Titel soll aber trotzdem für den Benutzer sinnvoll erscheinen, da dieser Titel
    // z.B. bei etwaigen ShowMessage() Fenstern oder in der Taskleiste erscheint.
    Application.Title := Unique_AppTitle + ' ';

    // Das Fenster der anderen Instanz finden.
    // Wichtig: Das funktioniert erst nach Application.Initialize!
    hWndAndereInstanz := FindWindow('TApplication', PChar(Unique_AppTitle));
    if hWndAndereInstanz <> 0 then
    begin
      // Falls minimiert, wiederherstellen
      (*
      if IsIconic(hWndAndereInstanz) then
        ShowWindow(hWndAndereInstanz, {SW_SHOW}SW_RESTORE);
      *)
      SendMessage(hWndAndereInstanz, WM_SYSCOMMAND, SC_RESTORE, 0);

      // Die Anwendung in den Vordergrund bringen
      SetForegroundWindow(hWndAndereInstanz);

      // Für HS-Info 2.0, weil der im Tray hockt und dann der obige Code nicht geht
      // Achtung: Der Delphi 11 Debugger fängt die WindowsMessages ab!!!
      ThswmOpenApp.Send(CalculateCRC32File(ParamStr(0)));
    end
    else
    begin
      // Dies passiert, wenn das Fenster der anderen Anwendung NICHT gefunden wurde.
      // Das sollte eigentlich nicht passieren, aber wir versuchen trotzdem, die Situation gekonnt zu überspielen.
      HsShowMessage(Application.Title + ' ist bereits auf diesem PC gestartet.');
    end;

    // Uns selbst schließen
    {$REGION 'Bitdefender-Besänftigung'}
    // Wir verwenden ein Dummy-Form, da Bitdefender bei der Verhaltensanalyse
    // einen Virus vermutet, wenn die Anwendung sich schließt, ohne vorher
    // eine GUI ausgegeben zu haben.
    // Achtung: Das Dummy Form darf nicht Border=none und Size=1x1 haben, sonst
    // akzeptiert Bitdefender das nicht als "richtiges" Fenster...
    Application.CreateForm(TDummyForm, DummyForm);
    Application.Run;
    {$ENDREGION}
    Halt;
  end;
end;

initialization

finalization
  CloseSemaphoreHandleIfExists;
end.
