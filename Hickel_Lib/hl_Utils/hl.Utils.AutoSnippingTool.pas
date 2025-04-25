unit hl.Utils.AutoSnippingTool;

interface

procedure ScreenshotInDateiablageErstellen(Pfad: string);

implementation

uses
  Windows, SysUtils, Graphics, Forms, Clipbrd, ShellAPI, TlHelp32,
  hl.Utils, PngImage;

function KillTask(ExeFileName: string): boolean;
const
  PROCESS_TERMINATE = $0001;
var
  ContinueLoop: BOOL;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
begin
  Result := false;
  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  FProcessEntry32.dwSize := SizeOf(FProcessEntry32);
  ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);

  while Integer(ContinueLoop) <> 0 do
  begin
    if ((UpperCase(ExtractFileName(FProcessEntry32.szExeFile)) =
      UpperCase(ExeFileName)) or (UpperCase(FProcessEntry32.szExeFile) =
      UpperCase(ExeFileName))) then
      Result := Result or (Integer(TerminateProcess(
                        OpenProcess(PROCESS_TERMINATE,
                                    BOOL(0),
                                    FProcessEntry32.th32ProcessID),
                                    0)) <> 0);
     ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
  end;
  CloseHandle(FSnapshotHandle);
end;

function processExists(exeFileName: string): Boolean;
var
  ContinueLoop: BOOL;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
begin
  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  FProcessEntry32.dwSize := SizeOf(FProcessEntry32);
  ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);
  Result := False;
  while Integer(ContinueLoop) <> 0 do
  begin
    if ((UpperCase(ExtractFileName(FProcessEntry32.szExeFile)) =
      UpperCase(ExeFileName)) or (UpperCase(FProcessEntry32.szExeFile) =
      UpperCase(ExeFileName))) then
    begin
      Result := True;
    end;
    ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
  end;
  CloseHandle(FSnapshotHandle);
end;

procedure SendAltNToSnippingTool;
var
  hwndSnippingTool: hWnd;
  i: integer;
  IsWin11SnippingTool: boolean;
begin
  IsWin11SnippingTool := IsWindows11; // should not be neccessary. Mainly that the compiler does not complain

  i := 0;
  repeat
    hwndSnippingTool := FindWindow('XamlWindow', 'Snipping Tool');
    if hwndSnippingTool <> 0 then
    begin
      IsWin11SnippingTool := true;
    end
    else
    begin
      hwndSnippingTool := FindWindow('Microsoft-Windows-SnipperToolbar', 'Snipping Tool');
      if hwndSnippingTool <> 0 then
      begin
        IsWin11SnippingTool := false;
      end;
    end;
    Sleep(10);
    Inc(i);
    if i = 100 then exit;
  until hwndSnippingTool <> 0;

  if IsWin11SnippingTool then
  begin
    hwndSnippingTool := FindWindowEx(hwndSnippingTool, 0, 'Windows.UI.Composition.DesktopWindowContentBridge', 'DesktopWindowXamlSource');
    if hwndSnippingTool = 0 then exit;
  end;

  // Bring specified window into focus
  SetForegroundWindow(hwndSnippingTool);

  keybd_event(VK_MENU,$b8,0 , 0); //Alt Press
  keybd_event(Ord('N'),$8f,0 , 0); // Tab Press
  keybd_event(Ord('N'),$8f, KEYEVENTF_KEYUP,0); // Tab Release
  keybd_event(VK_MENU,$b8,KEYEVENTF_KEYUP,0); // Alt Release
end;

var
  SnippingWatcherRunning: TGUID;
procedure ScreenshotInDateiablageErstellen(Pfad: string);
var
  img: TPicture;
  clip: TClipBoard;
  g: TGUID;
  bmp: TBitmap;
  png: TPngObject;
  i: integer;
  clipHandle: THandle;
begin
  ChangeFSRedirection(true);
  try
    {$IF CompilerVersion > 20.0} // Version geraten
    g := TGUID.NewGuid;
    {$ELSE}
    CreateGUID(g);
    {$IFEND}
    SnippingWatcherRunning := g; // bricht dann einen bestehenden Lauf ab

    i := 0;
    while not KillTask('SnippingTool.exe') and (i<10) do
    begin
      Sleep(100);
      Inc(i);
    end;

    i := 1;
    while i=1 do
    begin
      try
        Clipboard.Clear;
        i := 0;
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

    // TODO: Prüfen ob SnippingTool.exe verfügbar ist (bei Windows Server muss man "Desktopdarstellung" Feature installieren)

    //if Screen.MonitorCount = 1 then
    //begin
      Application.Minimize;
    //end;

    ShellExecute64(0, 'open', 'SnippingTool.exe', '', '', SW_NORMAL);
    SendAltNToSnippingTool;
    while True do
    begin
      if Application.Terminated then exit;
      {$IF CompilerVersion > 20.0} // Version geraten
      if g <> SnippingWatcherRunning then exit;
      {$ELSE}
      if not IsEqualGuid(g, SnippingWatcherRunning) then exit;
      {$IFEND}
      if not processExists('SnippingTool.exe') then break;

      clipHandle := 0;
      try
        clip := Clipboard;
        if not Assigned(clip) then
        begin
          Sleep(100);
          Application.ProcessMessages;
          continue;
        end;
        if not clip.HasFormat(CF_BITMAP) then
        begin
          FreeAndNil(clip);
          Application.ProcessMessages;
          continue;
        end;
        clipHandle := clip.GetAsHandle(CF_Bitmap);
      except
        on E: EAbort do
        begin
          Abort;
        end;
        on E: Exception do
        begin
          Sleep(100);
          Application.ProcessMessages;
          continue;
        end;
      end;

      img := TPicture.create;
      try
        img.LoadFromClipboardFormat(CF_Bitmap,clipHandle,0);
        bmp := TBitmap.Create;
        try
          bmp.Width := img.Width;
          bmp.Height := img.Height;
          bmp.Canvas.Draw(0, 0, img.Graphic);
          png := TPngObject.Create;
          try
            png.Assign(bmp);
            png.SaveToFile(Pfad + '\Screenshot_' + FormatDateTime('yyyy-mm-dd-hh-nn-ss', Now) + '.png');
          finally
            FreeAndNil(png);
          end;
        finally
          FreeAndNil(bmp);
        end;

        i := 1;
        while i=1 do
        begin
          try
            clip.Clear;
            i := 0;
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
      finally
        FreeAndNil(img);
      end;

      Sleep(100);
      Application.ProcessMessages;
      break;
    end;

    Sleep(750); // Bei Windows 11 aus irgendeinem Grund wichtig
    i := 0;
    while not KillTask('SnippingTool.exe') and (i<10) do
    begin
      Sleep(100);
      Inc(i);
    end;

    //if Screen.MonitorCount = 1 then
    //begin
      Application.Restore;
    //end;

    Application.BringToFront;
  finally
    ChangeFSRedirection(false);
  end;
end;

end.
