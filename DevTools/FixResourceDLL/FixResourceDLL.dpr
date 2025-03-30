program FixResourceDLL;

(*

This tool will fix incomplete Delphi Resource DLLs by
adding .dfm files which were not detected by the localization
tool included in Delphi. An example for a .dfm file which is not
detected is TwwMemoDlg from the InfoPower VCL. It doesn't
even get detected if you include it in the base project!

*)

uses
  Windows,
  SysUtils;

{$R *.res}

{$APPTYPE CONSOLE}

var
  hQuelleDLL: HMODULE;
  hUpdate: THandle;

function EnumResNamesProc(hModule: HMODULE; lpType, lpName: PChar; lParam: LONG_PTR): BOOL; stdcall;
var
  resHandle: HRSRC;
  resGlobal: HGLOBAL;
  resPointer: Pointer;
  resSize: DWORD;
begin
  resHandle := FindResource(hModule, lpName, lpType);
  if resHandle = 0 then
  begin
    Writeln('Fehler beim Abrufen der Ressource.');
    Exit(True); // Continue enumeration
  end;

  resSize := SizeofResource(hModule, resHandle);
  resGlobal := LoadResource(hModule, resHandle);
  resPointer := LockResource(resGlobal);

  if (resPointer <> nil) and (resSize > 0) then
  begin
    if not UpdateResource(hUpdate, lpType, lpName, 0, resPointer, resSize) then
    begin
      if IS_INTRESOURCE(lpName) then
        Writeln('Error inserting resource (numeric): ', DWORD(lpName))
      else
        Writeln('Error inserting resource (string): ', lpName);
    end
    else
    begin
      (*
      if IS_INTRESOURCE(lpName) then
        Writeln('Inserted resource (numeric): ', DWORD(lpName))
      else
        Writeln('Inserted resource (string): ', lpName);
      *)
    end;
  end;

  Result := True; // Continue enumeration
end;

// Diese Funktion wird für jeden Ressourcentyp aufgerufen
function EnumResTypesProc(hModule: HMODULE; lpType: PChar; lParam: LONG_PTR): BOOL; stdcall;
begin
  (*
  if IS_INTRESOURCE(lpType) then
    Writeln('Resource type (numeric): ', DWORD(lpType))
  else
    Writeln('Resource type (string): ', lpType);
  *)
  EnumResourceNames(hModule, lpType, @EnumResNamesProc, lParam);
  Result := True; // Continue enumeration
end;

procedure InsertResources(const QuelleDLL, ZielEXE: string);
begin
  hQuelleDLL := LoadLibraryEx(PChar(QuelleDLL), 0, LOAD_LIBRARY_AS_DATAFILE);
  if hQuelleDLL = 0 then
  begin
    Writeln('Cannot load source file', QuelleDLL, GetLastError);
    Exit;
  end;

  hUpdate := BeginUpdateResource(PChar(ZielEXE), False);
  if hUpdate = 0 then
  begin
    Writeln('BeginUpdateResource error: ', ZielExe, GetLastError);
    FreeLibrary(hQuelleDLL);
    Exit;
  end;

  if not EnumResourceTypes(hQuelleDLL, @EnumResTypesProc, 0) then
  begin
    Writeln('EnumResourceTypes error: ', ZielExe, GetLastError);
  end;

  if not EndUpdateResource(hUpdate, False) then
  begin
    Writeln('EndUpdateResource error: ', ZielExe, GetLastError);
  end;

  FreeLibrary(hQuelleDLL);
end;

var
  i: integer;
begin
  try
    if ParamCount = 0 then
    begin
      WriteLn('Syntax: ', ExtractFileName(ParamStr(0)), ' IncompleteResFile.enu [IncompleteResFile.xyz [...]]');
      ExitCode := 2;
      Exit;
    end;

    for i := 1 to ParamCount do
    begin
      // Copy incomplete .ENU file to .TMP file
      CopyFile(PChar(ParamStr(i)), PChar(ChangeFileExt(ParamStr(i),'.tmp')), false);

      // Copy all resources from German .EXE file to .TMP file
      InsertResources(PChar(ChangeFileExt(ParamStr(i),'.exe')), PChar(ChangeFileExt(ParamStr(i),'.tmp')));

      // Copy (and replace existing) resources from English .ENU file to .TMP file
      InsertResources(PChar(ParamStr(i)), PChar(ChangeFileExt(ParamStr(i),'.tmp')));

      // Move .TMP file to .ENU file which is now complete
      MoveFileEx(PChar(ChangeFileExt(ParamStr(i),'.tmp')), PChar(ParamStr(i)), MOVEFILE_REPLACE_EXISTING);
    end;
  except
    on E: EAbort do
    begin
      exit;
    end;
    on E: Exception do
    begin
      Writeln('Error: ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
