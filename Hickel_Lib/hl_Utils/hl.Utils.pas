unit hl.Utils;

interface

uses
  Windows, Messages, Forms, Classes, SysUtils, Math, Controls, ShellAPI, ShlObj, ActiveX,
  {$IF CompilerVersion >= 20.0}IOUtils,{$IFEND}
  StdCtrls, ZLib, DBCtrls, ADODB, SHFolder, ComCtrls, Dialogs;

type
  /// <summary>Statische Klasse mit Hilfsfunktionen. Die Klasse ist eine Art Namespace. Es wurde auf prozedurale Entwicklung verzichtet, um die XML-Dokumentation zu ermöglichen</summary>
  ThlUtils = class(TObject)
  public
    /// <summary>
    ///  PHP like implode function. Returns a string containing a string representation of all the array elements in the same order, with the glue string between each element.
    /// </summary>
    /// <see>http://users.atw.hu/delphicikk/listaz.php?id=1622&oldal=45</see>
    class function implode(const glue: string; const pieces: array of string): string; static;
    class function GermanDayOfWeek(date: TDate): integer; static;

    class function InBlöckeAufspalten(s: string; blockgröße: integer): string; static;

    /// <see>http://delphi.about.com/od/adptips2005/qt/leadingzero.htm</see>
    class function AddLeadingZeroes(const aNumber, Length: integer): string; static;

    class function ShellExecuteWait64(aWnd: HWND; Operation: string; ExeName: string; Params: string; WorkingDirectory: string; ncmdShow: Integer; wait: boolean): Integer;
    class function ShellExecuteWait(aWnd: HWND; Operation: string; ExeName: string; Params: string; WorkingDirectory: string; ncmdShow: Integer; wait: boolean): Integer;
    class function Find3264ExeCandidate(ExeName: string): string;

    class function GetUserDir: string; static;
    class function GetUserDocumentsDir: string; static;

    class function AdvSelectDirectory(const Caption: string; const Root: WideString; var Directory: string;
                                           EditBox: Boolean = False; ShowFiles: Boolean = False; AllowCreateDirs: Boolean = True): Boolean;

    class procedure DeleteDirectory(const DirName: string); static;

    class procedure ProcessMessages(PleaseCancel: PBoolean=nil);

    class procedure ListFiles(Directory: string; list: TStrings; recursive: boolean);

    class procedure ChecksummenErzeugen_SFV(const DirName: string; SFVFileName: string='Checksums.sfv');
    class procedure ChecksummenErzeugen_MD5(const DirName: string; SFVFileName: string='Checksums.md5');

    class function GetParentDir(const dir: string): string;

    class function PathsEqual(const pathA, pathB: string): boolean;

    class function DirectoryEmpty(const dir: string): boolean;

    class function GetFileSize(const filename: string): int64;

    class function ValidWinFileName(Filename: String; islong: Boolean=true): Boolean;

    class function Cut(s: string; delim: char; piece: integer): string; static;
    class function nullenRechtsEntfernen(s: string): string; static;
    class function AmiFloat(d: double): string; static;

    class function GetFileVersion(FileName: string): string;

    class function GetComputerName: string;
    class function GetWindowsUserName: String;
    class function GetDomainName: string;

    class procedure TryFreeDiskSpace(level: integer=0);
    class function DeleteFiles(const AFile: string): boolean;

    class function processExists(exeFileName: string): Boolean;

    class function GetInside(s, delimA, delimB: string): string;
    class function FileIsReadable(filename: string): boolean;
    class function EinigeDateienNichtLesbar(filemask: string): boolean;
    class function AbsToRel(const AbsPath, BasePath: string): string;
    class function RelToAbs(const RelPath, BasePath: string): string;
    class function GetModificationTimeOfFile(const AFileName: String): TDateTime; static;

    class function FileGetContents(const filename: string): string; static;

    class function DaysAge(const filename: string): integer; static;

    class function KillTask(ExeFileName: string): Integer;

    class procedure RequeryAndGotoSameSpot(ds: TAdoQuery);
  end;

function hclStrToStr (aValue: string): string; // TODO: ersetzen durch hlString.toSQLString
function FloatToStrForSQL (aValue: extended; NKStellen: integer): string; overload;
function FloatToStrForSQL (aValue: extended): string;overload;
function BoolToStrForSQL (aValue: Boolean): string;
function hclBoolToStr (aValue: Boolean): string;

function FRound( Value: extended; iAnzahl: integer): extended;
function Runden05 (aValue: double): double;
function Runden10 (aValue: double): double;
function hlFieldNameAnpassen(x: string): string;
function CheckEmpty (aValue: string): string;
function Split (aLine, aSplitString: string): TStringList; overload;
procedure Split(Delimiter: Char; Str: string; ListOfStrings: TStrings); overload;

function CheckAscii(sValue: ansistring): ansistring;

function GetNextPossibleForm(ctrl: TComponent): TForm;

procedure LoadStringListFromResource(const ResName: string; SL: TStrings; ResType: PChar=RT_RCDATA; otherHInstance: HMODULE=0);

function GetTempDir: string;

procedure CropFrame(frame: TFrame; onlyVisible: boolean; rightPadding, bottomPadding: integer);

function StringXorCrypt(const text, key:Ansistring):Ansistring;
procedure SaveStrToFile(const FileName, SourceString : string);
function LoadFileToStr(const FileName: TFileName): AnsiString;
Function DeCompressAnsiString(const Input: AnsiString): AnsiString;
Function CompressAnsiString(const Input: AnsiString): AnsiString;

function EnterToTabAllowed(f: TForm): boolean;

function RandomString(strlength: integer): string;

procedure SplitChar(Delimiter: Char; Str: string; ListOfStrings: TStrings);
procedure SplitText(Delimiter: string; Str: string; ListOfStrings: TStrings);

function IsDirectoryWriteable(const AName: string): Boolean;

function FindAWindow(WinCaption: string; WinClassName: string): THandle;
function ProcessIDFromAppname32(appname: string): DWORD;
function KillProcess(Exename: string): Boolean; overload;
function KillProcess(PID: DWord): Boolean; overload;

function ZahlEingetippt(Key: char): boolean;
function NichtZahlEingetippt(Key: char): boolean;

function GetCharFromVirtualKey(Key: Word): char;

procedure GetUsbDrives(List: TStrings);

procedure MoveFilesWildcards(Source, Target: string);
procedure DeleteFilesWildcards(Source: string; recyclebin: boolean);

function IsWindows10: boolean;
function IsWindows11: boolean;

function Crw13_IstInstalliert: boolean;
function IsVCRuntime2022_64Bit_Installed: Boolean;

function IsWow64: Boolean;
function ChangeFSRedirection(bDisable: Boolean): Boolean;
function FixedDrive(Drive: char): Boolean;
function CopyFolder(const SrcFolder
                    , DestFolder: String
                    ; iFileOp: Integer
                    ; OverWrite: Boolean
                    ; ShowDialog: Boolean): Boolean;
function OpenPropertyPage(AFileOrFolder: string): boolean;
function ExpandEnvStrings(const AString: String): String;

function DateTimeToUTC(const Local: TDateTime): TDateTime;
function UTCToLocalDateTime(const UTC: TDateTime): TDateTime;
procedure SetFileCreationTime(const FileName: string; const DateTime: TDateTime);

function FileSize(const aFilename: String): Int64;
function CleanFileName(const InputString: string): string;

function FilesAreEqual(const File1, File2: TFileName): Boolean;

function GetDesktopFolder: string;
function CreateDesktopShellLink(const TargetName, Args, ALinkName, AIconName: string; AIconIndex: Integer): Boolean;

procedure FilePutContentsA(filename, binary: AnsiString);
procedure FilePutContentsW(filename, binary: WideString);

procedure CopyFiles(Source, Target: string); // Kann auch mit WildCards umgehen!

procedure AnDenAnfangScrollen(riched: TCustomRichEdit);
procedure AnsEndeScrollen(riched: TCustomRichEdit);

function DirectoryExistsAndIsNotEmpty(dirname: string): Boolean;

function GetBuildTimestamp(const ExeFile: string): TDateTime;
function GetOwnBuildTimestamp: TDateTime;

function RichTextToPlainText(richText: string): string;

function ShellExecute64(hWnd: HWND; Operation, FileName, Parameters, Directory: PChar; ShowCmd: Integer): HINST;

function ExpandEnvStr(const szInput: string): string;
function RunCMD(cmdLine: string; WindowMode: integer): boolean;

function GetFileModDate(filename: string): TDateTime;

function Rot13char(c: Char): Char;

function Firewall_IstPortFreigeschaltet(portno: integer): boolean;
Procedure AddLANRule(Name: string; port: integer);
procedure DateiUndDruckerFreigabeAktivieren(Enable: boolean);

function hl_GetMachineId: string;

procedure SecureDeleteFile(filename: string);

function PadLeft(const Str: string; Ch: Char; Count: Integer): string;
function PadRight(const Str: string; Ch: Char; Count: Integer): string;
function NewGUID: TGUID;
function NewGUIDString: string;
function IsEmptyGuidString(GuidStr: string): boolean;
function IsEqualGUID(const guid1, guid2: TGUID): Boolean;
function StrgGedrueckt(Key, Buchstabe: char): boolean;
function StrgShiftGedrueckt(Key, Buchstabe: char): boolean;
function GetDirSize(dir: string; subdir: Boolean): Int64;

function GetRouterMac(debug: boolean=false): string;

function DayOfWeekGerman(ADate: TDateTime): Integer;
function WeekOfDate(A: TDateTime): Integer;
procedure SetGermanLocale;
function MyDocumentsPath: string;
procedure HideFile(fname: string);
function GetWindowsDisplayUserName: string;

function IsUserAdmin: Boolean;

procedure MDI_Form_BringToFront(frm: TForm);
function DaysHumanReadable(days: integer): string;
function WindowsBits: integer;
procedure BmpToPngFile(bmpFile, pngFile: string);
function Hash_djb2(str: string): string;
function GetDeepestDir(const aFilename: string): string;

procedure EmptyKeyQueue;
procedure EmptyMouseQueue;

function GetWinDir: string;

function DarkModeIsEnabled: boolean;

function IsStrANumber(const S: string): Boolean;

function GetSysDir: string;
function Rfc3339ToDatetime(rfc: string): TDatetime;

function IsOdbcDriverInstalled(const DriverName: string): Boolean;

type
  TSenderlessNotifyEvent = procedure of object;

implementation

uses
  FormatSettingsCompat, hl.Utils.CRC32, hl.Utils.MD5, StrUtils, TlHelp32, Registry, ComObj, DateUtils,
  hl.Utils.WmiUtils, WinSock, PngImage, Graphics;

class function ThlUtils.processExists(exeFileName: string): Boolean;
var
  ContinueLoop: BOOL;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
begin
  // Quelle: https://stackoverflow.com/questions/876224/how-to-check-if-a-process-is-running-using-delphi?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa
  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  try
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
  finally
    CloseHandle(FSnapshotHandle);
  end;
end;

class function ThlUtils.AdvSelectDirectory(const Caption: string; const Root: WideString; var Directory: string;
                                           EditBox: Boolean = False; ShowFiles: Boolean = False; AllowCreateDirs: Boolean = True): Boolean;
  // Quelle: http://www.swissdelphicenter.ch/de/showcode.php?id=1802
  
  {
    Dieser Code zeigt den SelectDirectory-Dialog mit zusätzlichen Erweiterungen:
    - eine Edit-Box, wo der Benutzer den Verzeichnisnamen eingeben kann,
    - auch Dateien können in der Liste angezeigt werden,
    - eine Schaltfläche zum Erstellen neuer Verzeichnisse.
  }

  // callback function that is called when the dialog has been initialized
  //or a new directory has been selected

  // Callback-Funktion, die aufgerufen wird, wenn der Dialog initialisiert oder
  //ein neues Verzeichnis selektiert wurde
  function SelectDirCB(Wnd: HWND; uMsg: UINT; lParam, lpData: lParam): Integer;
    stdcall;
//  var
//    PathName: array[0..MAX_PATH] of Char;
  begin
    case uMsg of
      BFFM_INITIALIZED: SendMessage(Wnd, BFFM_SETSELECTION, Ord(True), Integer(lpData));
      // include the following comment into your code if you want to react on the
      //event that is called when a new directory has been selected
      // binde den folgenden Kommentar in deinen Code ein, wenn du auf das Ereignis
      //reagieren willst, das aufgerufen wird, wenn ein neues Verzeichnis selektiert wurde
      {BFFM_SELCHANGED:
      begin
        SHGetPathFromIDList(PItemIDList(lParam), @PathName);
        // the directory "PathName" has been selected
        // das Verzeichnis "PathName" wurde selektiert
      end;}
    end;
    Result := 0;
  end;
var
  WindowList: Pointer;
  BrowseInfo: TBrowseInfo;
  Buffer: PChar;
  RootItemIDList, ItemIDList: PItemIDList;
  ShellMalloc: IMalloc;
  IDesktopFolder: IShellFolder;
  Eaten, Flags: LongWord;
const
  // necessary for some of the additional expansions
  // notwendig für einige der zusätzlichen Erweiterungen
  BIF_USENEWUI = $0040;
  BIF_NOCREATEDIRS = $0200;
begin
  Result := False;
  if not DirectoryExists(Directory) then
    Directory := '';
  FillChar(BrowseInfo, SizeOf(BrowseInfo), 0);
  if (ShGetMalloc(ShellMalloc) = S_OK) and (ShellMalloc <> nil) then
  begin
    Buffer := ShellMalloc.Alloc(MAX_PATH);
    try
      RootItemIDList := nil;
      if Root <> '' then
      begin
        SHGetDesktopFolder(IDesktopFolder);
        IDesktopFolder.ParseDisplayName(Application.Handle, nil,
          POleStr(Root), Eaten, RootItemIDList, Flags);
      end;
      OleInitialize(nil);
      with BrowseInfo do
      begin
        hwndOwner := Application.Handle;
        pidlRoot := RootItemIDList;
        pszDisplayName := Buffer;
        lpszTitle := PChar(Caption);
        // defines how the dialog will appear:
        // legt fest, wie der Dialog erscheint:
        ulFlags := BIF_RETURNONLYFSDIRS or BIF_USENEWUI or
          BIF_EDITBOX * Ord(EditBox) or BIF_BROWSEINCLUDEFILES * Ord(ShowFiles) or
          BIF_NOCREATEDIRS * Ord(not AllowCreateDirs);
        lpfn    := @SelectDirCB;
        if Directory <> '' then
          lParam := Integer(PChar(Directory));
      end;
      WindowList := DisableTaskWindows(0);
      try
        ItemIDList := ShBrowseForFolder(BrowseInfo);
      finally
        EnableTaskWindows(WindowList);
      end;
      Result := ItemIDList <> nil;
      if Result then
      begin
        ShGetPathFromIDList(ItemIDList, Buffer);
        ShellMalloc.Free(ItemIDList);
        Directory := Buffer;
      end;
    finally
      ShellMalloc.Free(Buffer);
    end;
  end;
end;

class function ThlUtils.GetUserDir: string;
const
  SHGFP_TYPE_CURRENT = 0;
var
  path: array [0..MAX_PATH] of char;
begin
  SHGetFolderPath(0, CSIDL_PROFILE, 0, SHGFP_TYPE_CURRENT, @path[0]);
  Result:= IncludeTrailingPathDelimiter(path);
end;

class function ThlUtils.GetUserDocumentsDir: string;
Var
  PI : PItemIDList;
  A  : array[0..200] of Char;
begin
  SHGetSpecialFolderLocation(0, CSIDL_PERSONAL, PI);
  SHGetPathFromIDList(PI, A);
  result:= A;
end;

class function ThlUtils.AddLeadingZeroes(const aNumber, Length : integer) : string;
begin
   result := SysUtils.Format('%.*d', [Length, aNumber]) ;
end;

// DayOfWeek():       1=So, ..., 7=Sa
// GermanDayOfWeek(): 0=Mo, ..., 6=So
class function ThlUtils.GermanDayOfWeek(date: TDate): integer;
begin
  result := (DayOfWeek(date)+5) mod 7;
end;

class function ThlUtils.implode(const glue: string; const pieces: array of string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(Pieces) do
    Result := Result + Glue + Pieces[I];
  Delete(Result, 1, Length(Glue));
end;

class function ThlUtils.InBlöckeAufspalten(s: string; blockgröße: integer): string;
var
  i: integer;
begin
  if blockgröße <= 0 then
  begin
    raise Exception.Create('Ungültige Blockgröße');
  end;
  result := '';
  i := 1;
  s := Trim(s);
  while i < Length(s) do
  begin
    result := result + copy(s, i, blockgröße) + ' ';
    inc(i, blockgröße);
  end;
  result := Trim(result);
end;

class function ThlUtils.KillTask(ExeFileName: string): Integer;
const
  PROCESS_TERMINATE = $0001;
var
  ContinueLoop: BOOL;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
begin
  // https://stackoverflow.com/questions/43774320/how-to-kill-a-process-by-name/43775788
  
  Result := 0;
  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  try
    FProcessEntry32.dwSize := SizeOf(FProcessEntry32);
    ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);

    while Integer(ContinueLoop) <> 0 do
    begin
      if ((UpperCase(ExtractFileName(FProcessEntry32.szExeFile)) =
        UpperCase(ExeFileName)) or (UpperCase(FProcessEntry32.szExeFile) =
        UpperCase(ExeFileName))) then
        Result := Integer(TerminateProcess(
                          OpenProcess(PROCESS_TERMINATE,
                                      BOOL(0),
                                      FProcessEntry32.th32ProcessID),
                                      0));
       ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
    end;
  finally
    CloseHandle(FSnapshotHandle);
  end;
end;

// Returns Windows Error Code (i.e. 0=success), NOT the ShellExecute() code (>32 = success)
class function ThlUtils.ShellExecuteWait(aWnd: HWND; Operation: string; ExeName: string; Params: string; WorkingDirectory: string; ncmdShow: Integer; wait: boolean): Integer;

  function _ShellExecuteWait(aWnd: HWND; Operation, FileName, Parameters, Directory: string; ShowCmd: Integer; wait: boolean): Integer;
  var
    Info: TShellExecuteInfo;
    pInfo: PShellExecuteInfo;
    exitCode: DWord; // Achtung: Muss DWORD sein (Ticket 38498)
    wdir: PChar;
  begin
    pInfo := @Info;
    ZeroMemory(pInfo, SizeOf(Info));
    if Directory = '' then wdir := nil else wdir := PChar(Directory);
    with Info do
    begin
      cbSize       := SizeOf(Info);
      fMask        := SEE_MASK_NOCLOSEPROCESS;
      wnd          := aWnd;
      lpVerb       := PChar(Operation);
      lpFile       := PChar(FileName);
      lpParameters := PChar(Parameters + #0);
      lpDirectory  := wdir;
      nShow        := ShowCmd;
      hInstApp     := 0;
    end;

    if not ShellExecuteEx(pInfo) then
    begin
      result := -GetLastError;
      exit;
    end;

    try
      if not wait then
      begin
        result := 0;
        exit;
      end;

      repeat
        exitCode := WaitForSingleObject(Info.hProcess, 100);
        Sleep(50);
        if Windows.GetCurrentThreadId = System.MainThreadID then
          Application.ProcessMessages;
        if Assigned(Application) and Application.Terminated then Abort;
      until (exitCode <> WAIT_TIMEOUT);

      if not GetExitCodeProcess(Info.hProcess, exitCode) then
      begin
        result := -GetLastError;
        exit;
      end;

      result := exitCode;
    finally
      if Info.hProcess <> 0 then
        CloseHandle(Info.hProcess);
    end;
  end;

  function _CreateProcess(Operation, FileName, Parameters, Directory: string; ShowCmd: Integer; wait: boolean): Integer;
  var
      StartupInfo: TStartupInfo;
      ProcessInformation: TProcessInformation;
      Res: Bool;
      lpExitCode: DWORD;
      ExeAndParams: string;
      wdir: PChar;
  begin
      FillChar(StartUpInfo, sizeof(tstartupinfo), 0);
      with StartupInfo do
      begin
          cb := SizeOf(TStartupInfo);
          lpReserved := nil;
          lpDesktop := nil;
          lpTitle := nil;
          dwFlags := STARTF_USESHOWWINDOW;
          wShowWindow := ncmdShow;
          cbReserved2 := 0;
          lpReserved2 := nil;
      end;
      ExeAndParams := '"' + ExeName + '" ' + params;
      if Directory = '' then wdir := nil else wdir := PChar(Directory);
      Res := CreateProcess(PChar(ExeName), PChar(ExeAndParams), nil, nil, True,
          CREATE_DEFAULT_ERROR_MODE
          or NORMAL_PRIORITY_CLASS, nil, wdir, StartupInfo, ProcessInformation);
      try
        if not Res then
        begin
          result := -GetLastError;
          exit;
        end;
        if not Wait then
        begin
          Result := 0;
          exit;
        end;
        while True do
        begin
            GetExitCodeProcess(ProcessInformation.hProcess, lpExitCode);
            if lpExitCode <> STILL_ACTIVE then
                Break;
            Sleep(50);
            if Windows.GetCurrentThreadId = System.MainThreadID then
              Application.ProcessMessages;
            if Assigned(Application) and Application.Terminated then Abort;
        end;
        Result := Integer(lpExitCode);
      finally
        if ProcessInformation.hProcess <> 0 then
          CloseHandle(ProcessInformation.hProcess);
        if ProcessInformation.hThread <> 0 then
          CloseHandle(ProcessInformation.hThread);
      end;
  end;

begin
  if not SameText(Operation, 'open') then
  begin
    result := _ShellExecuteWait(awnd, PChar(Operation), PChar(ExeName), PChar(Params), PChar(WorkingDirectory), ncmdShow, wait);
    exit;
  end;

  result := _CreateProcess(Operation, ExeName, Params, WorkingDirectory, ncmdshow, wait);
  if (result = -193) then  // Fehler 193 = Keine zulässige Win32-Anwendung (z.B. "Hallo.txt")
  begin
    result := _ShellExecuteWait(awnd, PChar(Operation), PChar(ExeName), PChar(Params), PChar(WorkingDirectory), ncmdShow, wait);
  end;
end;

class function ThlUtils.Find3264ExeCandidate(ExeName: string): string;
var
  testExe: string;
begin
  // siehe auch HSRun_Program.cs
  if WindowsBits = 32 then
  begin
    testExe := ExeName;
    testExe := StringReplace(testExe, '64.exe', '.exe', [rfIgnoreCase]);
    if FileExists(testExe) then ExeName := testExe; // Variante 1 (CORA_Verwaltung.exe)
    testExe := StringReplace(testExe, '.exe', '32.exe', [rfIgnoreCase]);
    if FileExists(testExe) then ExeName := testExe; // Veriante 2 (DBTool32.exe), derzeit nicht mehr im Einsatz (DBTool32.exe heißt nun wieder DBTool.exe)
  end
  else
  begin
    testExe := ExeName;
    testExe := StringReplace(testExe, '32.exe', '.exe', [rfIgnoreCase]);
    testExe := StringReplace(testExe, '.exe', '64.exe', [rfIgnoreCase]);
    if FileExists(testExe) then ExeName := testExe; // Variante 1+2
  end;
  result := ExeName;
end;

class function ThlUtils.ShellExecuteWait64(aWnd: HWND; Operation, ExeName,
  Params, WorkingDirectory: string; ncmdShow: Integer; wait: boolean): Integer;
begin
  ExeName := Find3264ExeCandidate(ExeName);
  ChangeFSRedirection(true);
  try
    // Warum FS Redirection deaktivieren? (Nur bei 32 Bit EXE auf Win64 OS)
    // Mit der WoW64-Weiterleitung würde ein 32-Bit Prozess bevorzugt
    // 32-Bit Anwendungen öffnen. Das heißt z.B. kein Snipping-Tool!
    result := ShellExecuteWait(aWnd, Operation, ExeName, Params, WorkingDirectory, ncmdShow, wait);
  finally
    ChangeFSRedirection(false);
  end;
end;

class function ThlUtils.PathsEqual(const pathA, pathB: string): boolean;

  function __CanonizePath(const path: string): string;
  begin
    result := UpperCase(IncludeTrailingPathDelimiter(path));
  end;

begin
  result := __CanonizePath(pathA) = __CanonizePath(pathB);
  // TODO: Zukünftig auch UNC unterstützen
end;

class procedure ThlUtils.ProcessMessages(PleaseCancel: PBoolean);
begin
  if Windows.GetCurrentThreadId <> System.MainThreadID then exit;

  Application.ProcessMessages;
  if Assigned(Application) then
  begin
    if Application.Terminated then Abort;
  end;
  if Assigned(PleaseCancel) then
  begin
    if PleaseCancel^ then Abort;
  end;
end;

class function ThlUtils.DaysAge(const filename: string): integer;
begin
  result := Trunc(Now-FileDateToDateTime(FileAge(filename)));
end;

class procedure ThlUtils.DeleteDirectory(const DirName: string);
var
  FileOp: TSHFileOpStruct;
begin
  // http://stackoverflow.com/questions/11798783/delete-all-files-and-folders-recursively-using-delphi
  FillChar(FileOp, SizeOf(FileOp), 0);
  FileOp.wFunc := FO_DELETE;
  FileOp.pFrom := PChar(DirName+#0); //double zero-terminated
  FileOp.fFlags := FOF_SILENT or FOF_NOERRORUI or FOF_NOCONFIRMATION;
  SHFileOperation(FileOp);
end;

class function ThlUtils.DirectoryEmpty(const dir: string): boolean;
var
  SR: TSearchRec;
  i: Integer;
begin
  // http://www.swissdelphicenter.ch/de/showcode.php?id=2413
  Result := False;
  FindFirst(IncludeTrailingPathDelimiter(dir) + '*', faAnyFile, SR);
  for i := 1 to 2 do
    if (SR.Name = '.') or (SR.Name = '..') then
      Result := FindNext(SR) <> 0;
  FindClose(SR);
end;

// Quelle: https://www.viathinksoft.de/?page=codelib&showid=72
class procedure ThlUtils.ListFiles(Directory: string; list: TStrings; recursive: boolean);
var
  SR: TSearchRec;
begin
  Directory := IncludeTrailingPathDelimiter(Directory);

  if Application.Terminated then exit;
  if Windows.GetCurrentThreadId = System.MainThreadID then
    Application.ProcessMessages;

  if FindFirst(Directory+'*', faAnyFile, SR) = 0 then;
  begin
    repeat
      if (SR.Name <> '.') and (SR.Name <> '..') then
      begin
        if SR.Attr and faDirectory = faDirectory then
        begin
          if recursive then
          begin
            ListFiles(Directory + SR.Name, list, recursive);
          end;
        end
        else
        begin
          list.Add(Directory + SR.Name);
        end;
      end;
    until FindNext(SR) <> 0;
  end;
  FindClose(SR);
end;

class procedure ThlUtils.ChecksummenErzeugen_SFV(const DirName: string; SFVFileName: string='Checksums.sfv');
var
  slFiles: TStrings;
  sChkFile, sOutFile, sHash: string;
  fOut: TextFile;
  sComment: string;
  dirLen: Integer;
begin
  sOutFile := IncludeTrailingPathDelimiter(DirName) + SFVFileName;
  if FileExists(sOutFile) then
  begin
    if not DeleteFile(sOutFile) then
    begin
      raise Exception.CreateFmt('Konnte Datei %s nicht löschen!', [sOutFile]);
    end;
  end;
  AssignFile(fOut, sOutFile);
  slFiles := TStringList.Create;
  try
    Rewrite(fOut);
    sComment := '; Prüfsummen-Datei';
    if Assigned(Application) then
    begin
      sComment := sComment + ', erstellt mit ' + Application.Title;
    end;
    Writeln(fOut, sChkFile, sComment);
    ThlUtils.ListFiles(DirName, slFiles, true);
    for sChkFile in slFiles do
    begin
      if sChkFile = sOutFile then Continue;
      sHash := IntToHex(CalculateCRC32File(sChkFile), 8);
      dirLen := Length(IncludeTrailingPathDelimiter(DirName));
      Writeln(fOut, Copy(sChkFile, dirLen+1, Length(sChkFile)-dirLen) + ' ' + sHash);
    end;
  finally
    CloseFile(fOut);
    FreeAndNil(slFiles);
  end;
end;

class procedure ThlUtils.ChecksummenErzeugen_MD5(const DirName: string; SFVFileName: string='Checksums.md5');
var
  slFiles: TStrings;
  sChkFile, sOutFile, sHash: string;
  fOut: TextFile;
  sComment: string;
  dirLen: Integer;
begin
  sOutFile := IncludeTrailingPathDelimiter(DirName) + SFVFileName;
  if FileExists(sOutFile) then
  begin
    if not DeleteFile(sOutFile) then
    begin
      raise Exception.CreateFmt('Konnte Datei %s nicht löschen!', [sOutFile]);
    end;
  end;
  AssignFile(fOut, sOutFile);
  slFiles := TStringList.Create;
  try
    Rewrite(fOut);
    sComment := '; Prüfsummen-Datei';
    if Assigned(Application) then
    begin
      sComment := sComment + ', erstellt mit ' + Application.Title;
    end;
    Writeln(fOut, sChkFile, sComment);
    ThlUtils.ListFiles(DirName, slFiles, true);
    for sChkFile in slFiles do
    begin
      if sChkFile = sOutFile then Continue;

      // Hier gibt es ein Problem: Wenn QuickSFV eine SFV-Datei öffnet, verändert er
      // diese Datei (Cache?). Somit wäre eine MD5 Datei, die die SFV-Datei mit-prüft
      // ungültig.
      if LowerCase(ExtractFileExt(sChkFile)) = '.sfv' then Continue;
      
      sHash := LowerCase(MD5File(sChkFile));
      dirLen := Length(IncludeTrailingPathDelimiter(DirName));
      Writeln(fOut, sHash + ' *' + Copy(sChkFile, dirLen+1, Length(sChkFile)-dirLen));
    end;
  finally
    CloseFile(fOut);
    FreeAndNil(slFiles);
  end;
end;

class function ThlUtils.GetFileSize(const filename: string): int64;
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    try
      Result := FileStream.Size;
    except
      on E: EAbort do
      begin
        Abort;
      end;
      on E: Exception do
      begin
        Result := 0;
      end;
    end;
  finally
    FreeAndNil(FileStream);
  end;
end;

class function ThlUtils.GetParentDir(const dir: string): string;
begin
  result := ExtractFilePath(ExcludeTrailingPathDelimiter(dir));
end;

class function ThlUtils.ValidWinFileName(Filename: String; islong: Boolean=true): Boolean;
const
  { for short 8.3 file names }
  ShortForbiddenChars : set of Char = [';', '=', '+', '<', '>', '|',
                                       '"', '[', ']', '\', '/', ''''];
  { for long file names }
  LongForbiddenChars  : set of Char = ['<', '>', '|', '"', '\', '/', ':', '*', '?'];
var
  I: integer;
begin
  Result := Filename <> '';
  if islong then
  begin
    for I := 1 to Length(Filename) do
      Result := Result and not (Filename[I] in LongForbiddenChars);
  end
  else
  begin
    for I := 1 to Length(Filename) do
      Result := Result and not (Filename[I] in ShortForbiddenChars);
  end;
end;

class function ThlUtils.nullenRechtsEntfernen(s: string): string;
begin
  result := s;
  while (result <> '') and (result[Length(result)] = '0') do
  begin
    result := Copy(Result, 1, Length(result)-1);
  end;
  if (RightStr(result, 1) = '.') or (RightStr(result, 1) = ',') then
  begin
    result := Copy(result, 1, Length(result)-1);
  end;
end;

class function ThlUtils.Cut(s: string; delim: char; piece: integer): string;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Delimiter := delim;
    sl.StrictDelimiter := true;
    sl.DelimitedText := s;
    if piece >= sl.Count then
      result := ''
    else
      result := sl.Strings[piece];
  finally
    FreeAndNil(sl);
  end;
end;

class function ThlUtils.AmiFloat(d: double): string;
begin
  result := FloatToStr(d); // TODO: Rundung notwendig?
  result := StringReplace(result, '.', '', [rfReplaceAll]);
  result := StringReplace(result, ',', '.', []);
end;

class procedure ThlUtils.TryFreeDiskSpace(level: integer=0);
begin
  if level >= 0 then
  begin
    DeleteFiles('C:\windows\logs\cbs\*.*');
    DeleteFiles('C:\windows\temp\cab_*_*');
    DeleteFiles('C:\windows\$NtServicePackUninstall*');
    DeleteFiles('C:\windows\$NtUninstall*');
  end
  else if level >= 1 then
  begin
    DeleteFiles('C:\windows\temp\*.*');
    // TODO: auch %appdata%\..\Local\Temp löschen
  end;
end;

class function ThlUtils.DeleteFiles(const AFile: string): boolean;
var
  sh: SHFileOpStruct;
begin
  ZeroMemory(@sh, SizeOf(sh));
  with sh do
  begin
    Wnd := Application.Handle;
    wFunc := FO_DELETE;
    pFrom := PChar(AFile +#0);
    fFlags := FOF_SILENT or FOF_NOCONFIRMATION;
  end;
  result := SHFileOperation(sh) = 0;
end;

// TODO: Prozedurale Methoden in ThlUtils einbinden oder in hl.Utils.*.pas splitten

{$REGION 'Noch prozedurale Utils'}

function GetNextPossibleForm(ctrl: TComponent): TForm;
begin
  result := nil;

  while not (ctrl is TForm) do
  begin
    if TComponent(ctrl).Owner = nil then Exit;
    ctrl := TComponent(ctrl).Owner;
  end;

  if ctrl = nil then exit;

  result := ctrl as TForm;
end;

(*
procedure Split(Delimiter: Char; Str: string; ListOfStrings: TStrings) ;
begin
  // TODO: in HL verschieben
  ListOfStrings.Clear;
  ListOfStrings.Delimiter       := Delimiter;
  ListOfStrings.StrictDelimiter := True; // Requires D2006 or newer.
  ListOfStrings.DelimitedText   := Str;
end;
*)

function Split (aLine, aSplitString: string): TStringList ;
var
  l: TStringList;
  s, s1: string;
  p: integer;

begin
  s := aLine;
  l:= TSTringList.Create;
  l.Clear;

  while length (s) > 0 do
  begin
    p := pos (aSplitString, s);
    if p > 0 then
    begin
      s1 := copy (s, 1, p-1);
      s := copy (s, p + length (aSplitString), 10000);
      l.Add(s1);
    end                                                              
    else
    begin
      l.Add(s);
      s := '';
    end;
  end;
  result := l;
end;

function CheckEmpty (aValue: string): string;
var
  s: string;
begin
  s := aValue;
  if s = '' then s := '0';
  result := s;
end;
function CheckAscii(sValue: ansistring): ansistring;
var
   pcBuffer: pansiChar;
   pcResult: pansiChar;

begin
   pcBuffer := AllocMem(2048);
   pcResult := AllocMem(2048);
   try
     StrPCopy(pcBuffer, sValue);
     CharToOemA(pcBuffer, pcResult);
     result := StrPas(pcResult);
   finally
     FreeMem(pcResult);
     FreeMem(pcBuffer);
   end;
end;
function FloatToStrForSQL (aValue: extended; NKStellen: integer): string;
begin
   result := format ('%.' + intToStr (NKStellen) + 'f', [aValue]);
   if pos (',', result) > 0 then result [pos (',', result)] := '.';
   if SameText(result, 'NAN') then result := 'NULL'; // Ticket 55743
end;
function FloatToStrForSQL (aValue: extended): string;
begin
   result := FloatToStr (aValue);
   if pos (',', result) > 0 then result [pos (',', result)] := '.';
   if SameText(result, 'NAN') then result := 'NULL'; // Ticket 55743
end;
function BoolToStrForSQL (aValue: Boolean): string;
begin
  if (aValue) then result := '1' else result := '0';
end;
function FRound( Value: extended; iAnzahl: integer ): extended;
var
   iCounter : Byte;
   sMultiplier : string [15];
begin
   sMultiplier := '1';
   for iCounter := 1 to iAnzahl do sMultiplier := sMultiplier + '0';
   Value := Value * StrToFloat (sMultiplier);
   if (copy (FloatToStr (Frac (Value)),1,3) = '0,5') or
      (copy (FloatToStr (Frac (Value)),1,4) = '0,49') then
      Value := Value + 0.1;
  result := trunc (Value) + trunc (frac(value) * 2 );
   result := result / StrToFloat (sMultiplier);
end;
function Runden05 (aValue: double): double;
begin
  result := Ceil(aValue*20-0.000001)/20; // Ticket 49669
(*
var
   iValue : int64;
   test1: double;
   i1, i2 : integer;
begin
  test1 := aValue * 100000;
  iValue := trunc (test1);

  if (iValue = 0) then
  begin
    result := 0;
    exit;
  end;

  i1 := iValue mod 1000;
  i2 := iValue div 1000;
  if i1 > 0 then i2 := i2 + 1;
  i1 := i2 mod 10;
  if ((i1 >= 1) and (i1 <= 5)) or (i1 = 0)  then i2 := i2 + 5 - i1;
  if ((i1 >= 6) and (i1 <= 9)) then i2 := i2 + 10 - i1;

  result := i2 / 100;
*)
end;
function Runden10 (aValue: double): double;
begin
  result := Ceil(aValue*10-0.000001)/10; // Ticket 49669
(*
var
   iValue : int64;
   test1: double;
   i1, i2 : integer;
begin
  test1 := aValue * 100000;
  iValue := trunc (test1);

  if (iValue = 0) then
  begin
    result := 0;
    exit;
  end;

  i1 := iValue mod 1000;            
  i2 := iValue div 1000;
  if i1 > 0 then i2 := i2 + 1;
  i1 := i2 mod 10;
  if ((i1 >= 1) and (i1 <= 9)) or (i1 = 0) then i2 := i2 + 10 - i1;

  result := i2 / 100;
*)
end;
function hlFieldNameAnpassen(x: string): string;
var
   i: integer;
   r: string;
   c: char;

begin
   r := '';
   for i := 1 to length(x) do
   begin
      c := x[i];
      if c in ['/', ' ', '-', '%'] then r := r + '_'
      else if c in ['ä', 'Ä'] then r := r + 'AE'
      else if c in ['ö', 'Ö'] then r := r + 'OE'
      else if c in ['ü', 'Ü'] then r := r + 'UE'
      else if c = 'ß' then r := r + 'SS'
      else r := r + upcase(c);
   end;
   result := r;
end;

function hclStrToStr (aValue: string): string;
var
  s : string;
begin
  s := aValue;
  if copy (s, length(s), 1) = ':' then s := copy (s, 1, length(s) -1);
  if copy (s, length(s), 1) = '''' then s := copy (s, 1, length(s) -1);
  s := StringReplace (s, ':', '::', [rfReplaceAll]);
  s := StringReplace (s, '''', '''''', [rfReplaceAll]);
  result := s;
end;

function hclBoolToStr (aValue: Boolean): string;
begin
  if aValue = True then result := 'JA' else result := 'NEIN';
end;

class function ThlUtils.GetFileVersion(FileName: string): string;
var
   dwCoraVersionInfoSize: DWORD;
   dwBuffer: DWORD;
   iBuffer: uint;
   pVersionInfoBuffer: pChar;
   pVersionBuffer: pointer;

begin
   dwCoraVersionInfoSize := GetFileVersionInfoSize(pChar(ParamStr(0)), dwBuffer) + 1;
   pVersionInfoBuffer := AllocMem(dwCoraVersionInfoSize);
   try
     GetFileVersionInfo(pChar(FileName), 0, dwCoraVersionInfoSize, pVersionInfoBuffer);
     iBuffer := 255;

     // 407 = Deutsch
     if VerQueryValue(pVersionInfoBuffer, '\\StringFileInfo\\040704E4\\FileVersion', pVersionBuffer, iBuffer) then
     begin
       Result := StrPas(pChar(pVersionBuffer));
       exit;
     end;
     if VerQueryValue(pVersionInfoBuffer, '\\StringFileInfo\\040704B0\\FileVersion', pVersionBuffer, iBuffer) then
     begin
       Result := StrPas(pChar(pVersionBuffer));
       exit;
     end;

     // 409 = Englisch
     // TODO: Bei Delphi 11 bleibt die Spracheinstellung in den Versioninfos nicht bei Deutsch, sondern geht immer wieder zurück auf Englisch
     if VerQueryValue(pVersionInfoBuffer, '\\StringFileInfo\\040904E4\\FileVersion', pVersionBuffer, iBuffer) then
     begin
       Result := StrPas(pChar(pVersionBuffer));
       exit;
     end;
     if VerQueryValue(pVersionInfoBuffer, '\\StringFileInfo\\040904B0\\FileVersion', pVersionBuffer, iBuffer) then
     begin
       Result := StrPas(pChar(pVersionBuffer));
       exit;
     end;


     Result := '???'
   finally
     FreeMem(pVersionInfoBuffer);
   end;
end;
class function ThlUtils.GetComputerName : string;
var
   ComputerName: String;
   nsize: Dword;
begin
  nsize := 25;
  SetLength(ComputerName,nsize);
  if Windows.GetComputerName(PChar(ComputerName),nsize) then
  begin
     SetLength(ComputerName,nsize);
     result := ComputerName;
  end
  else result := '';
end;

procedure LoadStringListFromResource(const ResName: string; SL: TStrings; ResType: PChar=RT_RCDATA; otherHInstance: HMODULE=0);
var
  RS: TResourceStream;
begin
  if otherHInstance = 0 then otherHInstance := HInstance;
  RS := TResourceStream.Create(otherHInstance, ResName, ResType);
  try
    SL.LoadFromStream(RS);
  finally
    FreeAndNil(rs);
  end;
end;

function GetTempDir: string;
var
  Dir: string;
  Len: DWord;
begin
  SetLength(Dir,MAX_PATH);
  Len:=GetTempPath(MAX_PATH, PChar(Dir));
  if Len>0 then
  begin
    SetLength(Dir,Len);
    Result:=Dir;
  end
  else
    RaiseLastOSError;
  result := IncludeTrailingPathDelimiter(result);
end;

procedure CropFrame(frame: TFrame; onlyVisible: boolean; rightPadding, bottomPadding: integer);
var
  i: integer;
  c: TControl;
  maxRight: integer;
  maxBottom: integer;
begin
  maxRight := 0;
  maxBottom := 0;
  for i := 0 to frame.ControlCount - 1 do
  begin
    c := frame.Controls[i];
    if onlyVisible then
    begin
      if not c.Visible then continue;
      if (c is TLabel) and (TLabel(c).Caption = '') then continue;
    end;
    maxRight := Max(maxRight, c.Left + c.Width);
    maxBottom := Max(maxBottom, c.Top + c.Height);
  end;
  Inc(maxRight, rightPadding);
  Inc(maxBottom, bottomPadding);
  frame.ClientWidth := maxRight;
  frame.ClientHeight := maxBottom;
end;

Function CompressAnsiString(const Input: AnsiString): AnsiString;
{$IF CompilerVersion > 20.0} // Ich weiß nicht genau, ab welcher Version CompressBuf durch ZCompress ersetzt wurde... Ich habe daher irgendeine Version eingetragen
var
  InBytes: TBytes;
  OutBytes: TBytes;
begin
  SetLength(InBytes, Length(Input));
  Move(Input[1], InBytes[0], Length(Input));

  ZCompress(InBytes, outBytes);

  SetLength(Result, Length(outBytes));
  Move(outBytes[0], Result[1], Length(outBytes));
{$ELSE}
var
  Buffer: Pointer;
  BufSize: Integer;
begin
  Buffer := nil;
  try
    CompressBuf(@Input[1], Length(Input), Buffer, BufSize);
    SetLength(Result, BufSize);
    Move(Buffer^, Result[1], BufSize);
  finally
    If Buffer <> nil Then FreeMem(Buffer);
  end;
  {$IFEND}

  if DeCompressAnsiString(Result) <> Input then
    raise Exception.Create('Komprimierung stimmt nicht überein.');
end;

Function DeCompressAnsiString(const Input: AnsiString): AnsiString;
{$IF CompilerVersion > 20.0} // Ich weiß nicht genau, ab welcher Version DeCompressBuf durch ZDecompress ersetzt wurde... Ich habe daher irgendeine Version eingetragen
var
  InBytes: TBytes;
  OutBytes: TBytes;
begin
  SetLength(InBytes, Length(Input));
  Move(Input[1], InBytes[0], Length(Input));

  ZDecompress(InBytes, outBytes);

  SetLength(Result, Length(outBytes));
  Move(outBytes[0], Result[1], Length(outBytes));
{$ELSE}
var
  Buffer: Pointer;
  BufSize: Integer;
begin
  Buffer := nil;
  try
    DeCompressBuf(@Input[1], Length(Input), 0, Buffer, BufSize);
    SetLength(Result, BufSize);
    Move(Buffer^, Result[1], BufSize);
  finally
    If Buffer <> nil then FreeMem(Buffer);
  end;
  {$IFEND}
end;

function LoadFileToStr(const FileName: TFileName): AnsiString;
var
  FileStream : TFileStream;
begin
  FileStream:= TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
     if FileStream.Size>0 then
     begin
      SetLength(Result, FileStream.Size);
      FileStream.Read(Pointer(Result)^, FileStream.Size);
     end;
    finally
     FreeAndNil(FileStream);
    end;
end;

procedure SaveStrToFile(const FileName, SourceString : string);
var
  Stream : TFileStream;
begin
  Stream:= TFileStream.Create(FileName, fmCreate);
  try
    Stream.WriteBuffer(Pointer(SourceString)^, Length(SourceString));
  finally
    FreeAndNil(Stream);
  end;
end;

function StringXorCrypt(const text, key:Ansistring):Ansistring;
var
   i, j, keylen : Integer;
begin
   SetLength(Result, length(text));
   j := 1;
   keylen := Length(key);
   for i := 1 to Length(text) do
   begin
      Result[i] := AnsiChar(ord(text[i]) xor ord(key[j]));
      Inc(j);
      if j > keylen then j := 1;
   end;
end;

procedure Split(Delimiter: Char; Str: string; ListOfStrings: TStrings) ;
begin
   ListOfStrings.Clear;
   ListOfStrings.Delimiter       := Delimiter;
   ListOfStrings.StrictDelimiter := True; // Requires D2006 or newer.
   ListOfStrings.DelimitedText   := Str;
end;

function EnterToTabAllowed(f: TForm): boolean;
begin
  // Tastenfunktionen außerhalb des Grids zum Weiterschalten ( Tab-Ersatz )
  result := (f.ActiveControl <> nil) and
            (f.ActiveControl.classname <> 'TwwDBGrid') and
            (f.ActiveControl.classname <> 'TMemo') and
            (f.ActiveControl.classname <> 'TwwDBRichEdit') and
            ((f.ActiveControl.parent = nil) or (f.ActiveControl.parent.classname <> 'TwwDBGrid')) and
            not (f.Activecontrol is TDBMemo);
end;

function RandomString(strlength: integer): string;
var
  temp : integer;
begin
  // https://www.delphi-treff.de/tipps-tricks/object-pascal/strings/zufallstring-generieren/
  randomize;
  repeat
    temp := random(122); //ggf. erhöhen
    if temp in [48..57{0-1}, 65..90{A-Z}, 97..122{a-z}] then
    //Kann um beliebige ASCII-Zeichen erweitert werden,
    //ggf. den Wert in Random hochsetzen
      result := result + Chr(temp);
  until length(result) = strlength;
end;

{$ENDREGION}

class function ThlUtils.GetInside(s, delimA, delimB: string): string;
var
  pa, pb: integer;
begin
  pa := Pos(delimA, s);
  if pa = 0 then pa := 1;
  inc(pa, Length(delimA));
  s := Copy(s, pa, Length(s)-pa+1);

  pb := Pos(delimB, s);
  if pb = 0 then pb := Length(s);
  s := Copy(s, 1, pb-1);

  result := s;
end;

class function ThlUtils.FileGetContents(const filename: string): string;
{$IF CompilerVersion < 20.0} // Version geraten
var
  FileStream : TFileStream;
{$IFEND}
begin
  {$IF CompilerVersion >= 20.0} // Version geraten
  result := TFile.ReadAllText(filename);
  {$ELSE}
  FileStream:= TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
   try
     if FileStream.Size>0 then
     begin
      SetLength(Result, FileStream.Size);
      FileStream.Read(Pointer(Result)^, FileStream.Size);
     end;
    finally
     FreeAndNil(FileStream);
   end;
  {$IFEND}
end;

class function ThlUtils.FileIsReadable(filename: string): boolean;
var
  ss: TFileStream;
begin
  try
    ss := TFileStream.Create(filename, fmOpenRead);
    FreeAndNil(ss);
    result := true;
  except
    on E: EAbort do
    begin
      Abort;
    end;
    on E: Exception do
    begin
      // Application.MessageBox(PChar(e.Message), '', 0);
      result := false;
    end;
  end;
end;

class function ThlUtils.EinigeDateienNichtLesbar(filemask: string): boolean;
var
  SR: TSearchRec;
begin
  result := false;
  if FindFirst(filemask, faAnyFile, SR) = 0 then
  begin
    repeat
        if not ThlUtils.FileIsReadable(ExtractFilePath(filemask) + '\' + SR.Name) then
        begin
          result := true;
        end;
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;
end;

class function ThlUtils.GetWindowsUserName: String;
var
  nSize: DWord;
begin
  nSize := 1024;
  SetLength(Result, nSize);
  if GetUserName(PChar(Result), nSize) then
    SetLength(Result, nSize-1)
  else
    RaiseLastOSError;
end;

{$IFDEF UNICODE}
function PathRelativePathTo(pszPath: PChar; pszFrom: PChar; dwAttrFrom: DWORD; pszTo: PChar; dwAtrTo: DWORD): LongBool; stdcall; external 'shlwapi.dll' name 'PathRelativePathToW';
{$ELSE}
function PathRelativePathTo(pszPath: PChar; pszFrom: PChar; dwAttrFrom: DWORD; pszTo: PChar; dwAtrTo: DWORD): LongBool; stdcall; external 'shlwapi.dll' name 'PathRelativePathToA';
{$ENDIF}

class function ThlUtils.AbsToRel(const AbsPath, BasePath: string): string;
var
  Path: array[0..MAX_PATH-1] of char;
begin
  PathRelativePathTo(@Path[0], PChar(BasePath), FILE_ATTRIBUTE_DIRECTORY, PChar(AbsPath), 0);
  result := Path;
end;

{$IFDEF UNICODE}
function PathCanonicalize(lpszDst: PChar; lpszSrc: PChar): LongBool; stdcall; external 'shlwapi.dll' name 'PathCanonicalizeW';
{$ELSE}
function PathCanonicalize(lpszDst: PChar; lpszSrc: PChar): LongBool; stdcall; external 'shlwapi.dll' name 'PathCanonicalizeA';
{$ENDIF}

class function ThlUtils.RelToAbs(const RelPath, BasePath: string): string;
var
  Dst: array[0..MAX_PATH-1] of char;
begin
  if Copy(RelPath, 1, 2) = '\\' then
    result := RelPath
  else if Copy(RelPath, 2, 2) = ':\' then
    result := RelPath
  else
  begin
    PathCanonicalize(@Dst[0], PChar(IncludeTrailingPathDelimiter(BasePath) + RelPath));
    result := Dst;
  end;
end;

class function ThlUtils.GetModificationTimeOfFile(const AFileName: String): TDateTime;
var
  SR: TSearchRec;
  SystemTime: TSystemTime;
  NewModificationTime: TFileTime;
begin
  Result:=0;
  if FindFirst(AFileName, faAnyFile, SR)=0 then
  try
   IF (Windows.FileTimeToLocalFiletime(SR.FindData.ftLastWriteTime, NewModificationTime) and
           Windows.FileTimeToSystemTime(NewModificationTime, SystemTime)) Then
    Result:=Encodedate(SystemTime.wYear,
                       SystemTime.wMonth,
                       SystemTime.wDay) +
            Encodetime(SystemTime.wHour,
                       SystemTime.wMinute,
                       SystemTime.wSecond,
                       SystemTime.wMilliseconds);
  finally
   SysUtils.FindClose(SR)
  end;
end;

procedure SplitChar(Delimiter: Char; Str: string; ListOfStrings: TStrings); // TODO: Redundant? (Split-Funktion)
begin
   ListOfStrings.Clear;
   ListOfStrings.Delimiter       := Delimiter;
   ListOfStrings.StrictDelimiter := True; // Requires D2006 or newer.
   ListOfStrings.DelimitedText   := Str;
end;

procedure SplitText(Delimiter: string; Str: string; ListOfStrings: TStrings); // TODO: Redundant? (Split-Funktion)
begin
  SplitChar(#1, StringReplace(Str,Delimiter,#1,[rfIgnoreCase,rfReplaceAll]), ListOfStrings);
end;

function IsDirectoryWriteable(const AName: string): Boolean;
var 
  FileName: String; 
  H: HFILE;
begin
  // https://stackoverflow.com/questions/3599256/how-can-i-use-delphi-to-test-if-a-directory-is-writeable 
  FileName := IncludeTrailingPathDelimiter(AName) + 'chk.tmp';
  H := CreateFile(PChar(FileName), GENERIC_READ or GENERIC_WRITE, 0, nil,
    CREATE_NEW, FILE_ATTRIBUTE_TEMPORARY or FILE_FLAG_DELETE_ON_CLOSE, 0);
  Result := H <> INVALID_HANDLE_VALUE;
  if Result then CloseHandle(H);
end;

class procedure THlUtils.RequeryAndGotoSameSpot(ds: TAdoQuery);
var
  cdis: boolean;
  i: integer;
begin
  cdis := ds.ControlsDisabled;
  ds.DisableControls;
  try
    ds.Prior;

    i := 0;
    while not ds.Bof do
    begin
      ds.Prior;
      Inc(i);
    end;

    ds.Requery;

    while i > 0 do
    begin
      Dec(i);
      ds.Next;
    end;
  finally
    if not cdis then ds.EnableControls
  end;
end;

const
  NERR_Success = 0;

function NetWkstaGetInfo(ServerName: LPWSTR; Level: DWORD;
  BufPtr: Pointer): Longint; stdcall;
  external 'netapi32.dll' Name 'NetWkstaGetInfo';

type
  WKSTA_INFO_100 = record
    wki100_platform_id: DWORD;
    wki100_computername: LPWSTR;
    wki100_langroup: LPWSTR;
    wki100_ver_major: DWORD;
    wki100_ver_minor: DWORD;
  end;
  LPWKSTA_INFO_100 = ^WKSTA_INFO_100;

  _USER_INFO_0 = record
    usri0_name: LPWSTR;
  end;

function GetNetParam(AParam: Integer): string;
var
  PBuf: LPWKSTA_INFO_100;
  Res: LongInt;
begin
  Result := '';
  Res := NetWkstaGetInfo(nil, 100, @PBuf);
  if Res = NERR_Success then
  begin
    case AParam of
      0: Result := string(PBuf^.wki100_computername);
      1: Result := string(PBuf^.wki100_langroup);
    end;
 end;
end;

(*
function GetComputerName: string;
begin
  Result := GetNetParam(0);
end;
*)

class function ThlUtils.GetDomainName: string;
begin
  // https://delphidabbler.github.io/delphi-tips/tips/62.html
  Result := GetNetParam(1);
end;

Function ProcessIDFromAppname32(appname: string): DWORD;
{ Take only the application filename, not full path! }
Var
	snapshot: THandle;
	processEntry : TProcessEntry32;
Begin
	Result := 0;
	appName := UpperCase(appname);
	snapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
	If snapshot <> 0 Then
	try
	  processEntry.dwSize := Sizeof(processEntry);
	  if Process32First(snapshot, processEntry) Then
	  repeat
      if Pos(appname, UpperCase(ExtractFilename(StrPas(processEntry.szExeFile)))) > 0 then
      begin
        Result:= processEntry.th32ProcessID;
        Break;
      end;
	  until not Process32Next(snapshot, processEntry);
	finally
	  CloseHandle(snapshot);
	End;
End;



type
  PFindWindowStruct = ^TFindWindowStruct;
  TFindWindowStruct = record
    Caption: string;
    ClassName: String;
    WindowHandle: THandle;
  end;

function EnumWindowsProc(hWindow: hWnd; lParam: LongInt): boolean; stdcall;
var lpBuffer: PChar;
    WindowCaptionFound: boolean;
    ClassNameFound: boolean;
begin
  GetMem(lpBuffer, 255);
  result:=true;
  WindowCaptionFound:=false;
  ClassNameFound:=false;
  try
    if GetWindowText(hWindow, lpBuffer,255)>0 then
      if Pos(PFindWindowStruct(lParam).Caption, StrPas(lpBuffer))>0
      then WindowCaptionFound:=true;
    if PFindWindowStruct(lParam).ClassName='' then
      ClassNameFound:=true
      else if GetClassName(hWindow, lpBuffer, 255)>0 then
        if Pos(PFindWindowStruct(lParam).ClassName, StrPas(lpBuffer))>0
        then ClassNameFound:=true;
    if (WindowCaptionFound and ClassNameFound) then begin
      PFindWindowStruct(lParam).WindowHandle:=hWindow;
      result:=false;
    end;
  finally
    FreeMem(lpBuffer, sizeof(lpBuffer^));
  end;
end;

function FindAWindow(WinCaption: string; WinClassName: string): THandle;
var WindowInfo: TFindWindowStruct;
begin
  with WindowInfo do begin
    caption := WinCaption;
    className := WinClassName;
    WindowHandle := 0;
    EnumWindows(@EnumWindowsProc, LongInt(@WindowInfo));
    result := WindowHandle;
  end;
end;

function ZahlEingetippt(Key: char): boolean;
begin
  result := (ord (Key) >= 48) and (ord (Key) <= 57);
end;
function NichtZahlEingetippt(Key: char): boolean;
begin
  result := ((ord(Key)>=32) and (ord(Key)<=47)) or (ord(Key) >= 58);
  // TODO <= 122?
end;

{$REGION 'Prozess-Kill-Funktionen'}

Function EnumThreadProc(wnd: HWND; var appHwnd: HWND): LongBool; stdcall;
Var
  buf: array [0..128] of Char;
Begin
  if (GetClassname(wnd, buf, sizeof(buf)) > 0) and
     (StrComp(buf, 'TApplication') = 0) then
  begin
    appHwnd := Wnd;
    Result := False;
  end
  else
  begin
    Result := LongBool(1);
  end;
End;

Function FindApplicationWindow(forThreadID: DWORD): HWND;
Begin
  Result := 0;
  EnumThreadWindows(forThreadID, @EnumThreadProc, lparam(@result));
End;

function KillProcess(PID: DWord): boolean; overload;
var
  hProcess: THandle;
begin
  hProcess := OpenProcess(PROCESS_TERMINATE, False, PID);
  Result := hProcess <> 0;
  if Result then
  begin
    Result := TerminateProcess(hProcess, 0);
  end;
end;

function KillProcess(Exename: string): Boolean; overload;
var
  pid: DWord;
begin
  pid := ProcessIDFromAppname32(Exename);
  if pid <= 0 then
  begin
    result := false;
    exit;
  end
  else
  begin
    result := KillProcess(pid);
  end;
end;

{$ENDREGION}

function GetCharFromVirtualKey(Key: Word): char;
var
  keyboardState: TKeyboardState;
  asciiResult: Integer;
  s: string;
begin
  GetKeyboardState(keyboardState) ;

  SetLength(s, 2);
  asciiResult := ToAscii(key, MapVirtualKey(key, 0), keyboardState, @s[1], 0) ;
  case asciiResult of
    0: s := '';
    1: SetLength(s, 1) ;
    2:;
    else
      s := '';
  end;

  if s = '' then
    result := #0
  else
    result := s[1];
end;

procedure GetUsbDrives(List: TStrings);
  // http://ahmoremore.blogspot.com/2018/05/delphi-how-to-get-list-of-usb-removable.html

  {$MINENUMSIZE 4}
  const
    IOCTL_STORAGE_QUERY_PROPERTY =  $002D1400;

  type
    STORAGE_QUERY_TYPE = (PropertyStandardQuery = 0, PropertyExistsQuery, PropertyMaskQuery, PropertyQueryMaxDefined);
    TStorageQueryType = STORAGE_QUERY_TYPE;

    STORAGE_PROPERTY_ID = (StorageDeviceProperty = 0, StorageAdapterProperty);
    TStoragePropertyID = STORAGE_PROPERTY_ID;

    STORAGE_PROPERTY_QUERY = packed record
      PropertyId: STORAGE_PROPERTY_ID;
      QueryType: STORAGE_QUERY_TYPE;
      AdditionalParameters: array [0..9] of AnsiChar;
    end;
    TStoragePropertyQuery = STORAGE_PROPERTY_QUERY;

    STORAGE_BUS_TYPE = (BusTypeUnknown = 0, BusTypeScsi, BusTypeAtapi, BusTypeAta, BusType1394, BusTypeSsa, BusTypeFibre,
      BusTypeUsb, BusTypeRAID, BusTypeiScsi, BusTypeSas, BusTypeSata, BusTypeMaxReserved = $7F);
    TStorageBusType = STORAGE_BUS_TYPE;

    STORAGE_DEVICE_DESCRIPTOR = packed record
      Version: DWORD;
      Size: DWORD;
      DeviceType: Byte;
      DeviceTypeModifier: Byte;
      RemovableMedia: Boolean;
      CommandQueueing: Boolean;
      VendorIdOffset: DWORD;
      ProductIdOffset: DWORD;
      ProductRevisionOffset: DWORD;
      SerialNumberOffset: DWORD;
      BusType: STORAGE_BUS_TYPE;
      RawPropertiesLength: DWORD;
      RawDeviceProperties: array [0..0] of AnsiChar;
    end;
    TStorageDeviceDescriptor = STORAGE_DEVICE_DESCRIPTOR;

  function GetBusType(Drive: AnsiChar): TStorageBusType;
  var
    H: THandle;
    Query: TStoragePropertyQuery;
    dwBytesReturned: DWORD;
    Buffer: array [0..1023] of Byte;
    sdd: TStorageDeviceDescriptor absolute Buffer;
    OldMode: UINT;
  begin
    Result := BusTypeUnknown;

    OldMode := SetErrorMode(SEM_FAILCRITICALERRORS);
    try
      H := CreateFile(PChar(Format('\\.\%s:', [AnsiLowerCase(Drive)])), 0, FILE_SHARE_READ or FILE_SHARE_WRITE, nil,
        OPEN_EXISTING, 0, 0);
      if H <> INVALID_HANDLE_VALUE then
      begin
        try
          dwBytesReturned := 0;
          FillChar(Query, SizeOf(Query), 0);
          FillChar(Buffer, SizeOf(Buffer), 0);
          sdd.Size := SizeOf(Buffer);
          Query.PropertyId := StorageDeviceProperty;
          Query.QueryType := PropertyStandardQuery;
          if DeviceIoControl(H, IOCTL_STORAGE_QUERY_PROPERTY, @Query, SizeOf(Query), @Buffer, SizeOf(Buffer), dwBytesReturned, nil) then
            Result := sdd.BusType;
        finally
          CloseHandle(H);
        end;
      end;
    finally
      SetErrorMode(OldMode);
    end;
  end;

var
  DriveBits: set of 0..25;
  I: Integer;
  Drive: AnsiChar;
begin
  List.BeginUpdate;
  try
    Cardinal(DriveBits) := GetLogicalDrives;

    for I := 0 to 25 do
      if I in DriveBits then
      begin
        Drive := AnsiChar(Ord('a') + I);
        if GetBusType(Drive) = BusTypeUsb then
          List.Add(Drive);
      end;
  finally
    List.EndUpdate;
  end;
end;

procedure MoveFilesWildcards(Source, Target: string);
var
  FO: TShFileOpStruct;
begin
   FillChar(FO,SizeOf(FO),#0);
   //FO.Wnd := Application.MainForm.Handle;
   FO.wFunc := FO_MOVE;
   FO.pFrom := PChar(Source + #0);
   FO.pTo := PChar(Target + #0);
   ShFileOperation(FO); // TODO: check result
end;

procedure DeleteFilesWildcards(Source: string; recyclebin: boolean);
var
  FO: TShFileOpStruct;
begin
   FillChar(FO,SizeOf(FO),#0);
   //FO.Wnd := Application.MainForm.Handle;
   FO.wFunc := FO_DELETE;
   FO.pFrom := PChar(Source + #0);
   FO.fFlags := FOF_NOCONFIRMATION;
   if recyclebin then FO.fFlags := FO.fFlags or FOF_ALLOWUNDO;
   ShFileOperation(FO); // TODO: check result
end;

function IsWindows10: boolean;
var
  reg: TRegistry;
begin
  result := false;

  if IsWow64 then
    reg := TRegistry.Create(KEY_READ OR KEY_WOW64_64KEY)
  else
    reg := TRegistry.Create(KEY_READ);
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    if reg.OpenKeyReadOnly('SOFTWARE\Microsoft\Windows NT\CurrentVersion') then
    begin
      try
        result := reg.ReadInteger('CurrentMajorVersionNumber') = 10;
      except
        on E: EAbort do
        begin
          Abort;
        end;
        on E: Exception do
        begin
          // Windows Server 2012 Standard hat diesen Registry Key nicht. Es baut aber auf Win7 auf.
          result := false;
        end;
      end;
      reg.CloseKey;
    end;
  finally
    FreeAndNil(reg);
  end;
end;

function IsWindows11: boolean;
var
  reg: TRegistry;
begin
  result := false;

  if IsWow64 then
    reg := TRegistry.Create(KEY_READ OR KEY_WOW64_64KEY)
  else
    reg := TRegistry.Create(KEY_READ);
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    if reg.OpenKeyReadOnly('SOFTWARE\Microsoft\Windows NT\CurrentVersion') then
    begin
      try
        // CurrentMajorVersionNumber oder ProductName dürfen wir nicht verwenden!
        // diese sind nämlich bei Windows 11 derzeit noch auf "10" !
        //result := reg.ReadInteger('CurrentMajorVersionNumber') = 11;
        result := (StrToInt(reg.ReadString('CurrentBuild')) >= 22000) and
                  (StrToInt(reg.ReadString('CurrentBuildNumber')) >= 22000);
      except
        on E: EAbort do
        begin
          Abort;
        end;
        on E: Exception do
        begin
          result := false;
        end;
      end;
      reg.CloseKey;
    end;
  finally
    FreeAndNil(reg);
  end;
end;

function Crw13_IstInstalliert: boolean;
var
  testFile: string;
begin
  if WindowsBits = 64 then
  begin
    testFile := 'C:\Program Files (x86)\SAP BusinessObjects\Crystal Reports for .NET Framework 4.0\Common\SAP BusinessObjects Enterprise XI 4.0\win32_x86\crpe32.dll';
    if not FileExists(testFile) then begin result := false; Exit; end;
    // sic! Die 64 Bit crpe32.dll liegt wirklich in "Program Files (x86)" !
    testFile := 'C:\Program Files (x86)\SAP BusinessObjects\Crystal Reports for .NET Framework 4.0\Common\SAP BusinessObjects Enterprise XI 4.0\win64_x64\crpe32.dll';
    if not FileExists(testFile) then begin result := false; Exit; end;
    result := true;
  end
  else
  begin
    testFile := 'C:\Program Files\SAP BusinessObjects\Crystal Reports for .NET Framework 4.0\Common\SAP BusinessObjects Enterprise XI 4.0\win32_x86\crpe32.dll';
    if not FileExists(testFile) then begin result := false; Exit; end;
    result := true;
  end;
end;

function IsVCRuntime2022_64Bit_Installed: Boolean;
var
  Reg: TRegistry;
begin
  Result := False;
  Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;

    // Prüfe den Standardpfad für 64-Bit Runtime
    if Reg.OpenKeyReadOnly('SOFTWARE\Microsoft\VisualStudio\14.0\VC\Runtimes\x64') then
    begin
      Result := Reg.ValueExists('Installed') and (Reg.ReadInteger('Installed') = 1);
      Reg.CloseKey;
    end;

  finally
    Reg.Free;
  end;
end;

function IsWow64: Boolean;
{$IFDEF WIN64}
begin
  // Native 64 Bit App means OS and CPU is 64 Bit, too.
  result := false;
{$ELSE}
type
  TIsWow64Process = function( // Type of IsWow64Process API fn
    Handle: Windows.THandle; var Res: Windows.BOOL
  ): Windows.BOOL; stdcall;
var
  IsWow64Result: Windows.BOOL;      // Result from IsWow64Process
  IsWow64Process: TIsWow64Process;  // IsWow64Process fn reference
begin
  // Try to load required function from kernel32
  IsWow64Process := Windows.GetProcAddress(
    Windows.GetModuleHandle('kernel32'), 'IsWow64Process'
  );
  if Assigned(IsWow64Process) then
  begin
    // Function is implemented: call it
    if not IsWow64Process(
      Windows.GetCurrentProcess, IsWow64Result
    ) then
      raise SysUtils.Exception.Create('IsWow64: bad process handle');
    // Return result of function
    Result := IsWow64Result;
  end
  else
    // Function not implemented: can't be running on Wow64
    Result := False;
{$ENDIF}
end;

function ChangeFSRedirection(bDisable: Boolean): Boolean;
type
  TWow64DisableWow64FsRedirection = Function(Var Wow64FsEnableRedirection: LongBool): LongBool; StdCall;
  TWow64EnableWow64FsRedirection = Function(var Wow64FsEnableRedirection: LongBool): LongBool; StdCall;
var
  hHandle: THandle;
  Wow64DisableWow64FsRedirection: TWow64DisableWow64FsRedirection;
  Wow64EnableWow64FsRedirection: TWow64EnableWow64FsRedirection;
  Wow64FsEnableRedirection: LongBool;
begin

  {$IFDEF Win64}
  result := false;
  exit;
  {$ENDIF}

  // https://www.delphipraxis.net/155861-windows-7-64bit-redirection.html

  Result := false;

  try
    hHandle := GetModuleHandle('kernel32.dll');
    @Wow64EnableWow64FsRedirection := GetProcAddress(hHandle, 'Wow64EnableWow64FsRedirection');
    @Wow64DisableWow64FsRedirection := GetProcAddress(hHandle, 'Wow64DisableWow64FsRedirection');

    if bDisable then
    begin
     if (hHandle <> 0) and (@Wow64DisableWow64FsRedirection <> nil) then
     begin
       Result := Wow64DisableWow64FsRedirection(Wow64FsEnableRedirection);
     end;
    end else
    begin
     if (hHandle <> 0) and (@Wow64EnableWow64FsRedirection <> nil) then
     begin
       Result := Wow64EnableWow64FsRedirection(Wow64FsEnableRedirection);
       Result := True;
     end;
    end;
  except
    on E: EAbort do
    begin
      Abort;
    end;
  end;
end;

function FixedDrive(Drive: char): Boolean;
begin
  Result := (Windows.GetDriveType(PChar(Drive + ':\')) = Windows.DRIVE_FIXED);
end;

function CopyFolder(const SrcFolder
                    , DestFolder: String
                    ; iFileOp: Integer
                    ; OverWrite: Boolean
                    ; ShowDialog: Boolean): Boolean;
{
     Copies or moves ...\SrcFolder to ...\DestFolder\SrcFolder\*.*

     Example:
       Copy C:\AFolder\SubFolder and it's contents to
            C:\AnotherFolder\SubFolder
            and prompt to replace existing files.

       CopyFolder('C:\AFolder\SubFolder', 'C:\AnotherFolder', FOF_COPY, 
true);

}
var
   // declare structure
   MyFOStruct: TSHFileOpStruct;
   Src,
   Dest:       String;
   ResultVal:  Integer;
begin
   result := false;

   Src := SrcFolder;
   Dest := DestFolder;

   if (Src = '') or
      ( (iFileOp <> FO_DELETE) and (Dest = '') ) or
      (CompareText(Src, Dest) = 0) then
        exit;

   if Src[Length(Src)] = '\' then
     SetLength( Src, Length(Src) -1 );
   Src := Src +#0#0;

   if (Dest <> '') and (Dest[Length(Dest)] = '\') then
     SetLength( Dest, Length(Dest) -1 );
   Dest := Dest + #0#0;

   // zero structure
   // ! Mandatory in XP
   FillChar( MyFOStruct, SizeOf(MyFOStruct), 0 );

   // Fill in structure
   with MyFOStruct do begin
     Wnd := 0;

     // specify a copy operation
     wFunc := iFileOp;
     pFrom := @Src[1];
     pTo := @Dest[1];

     // set the flags
     fFlags := FOF_ALLOWUNDO or FOF_NOCONFIRMMKDIR;

     if not OverWrite then fFlags := fFlags or FOF_RENAMEONCOLLISION;
     if not ShowDialog then fFlags := fFlags or FOF_SILENT;
   end;

   Screen.Cursor := crHourGlass;
   try
     MyFOStruct.fAnyOperationsAborted := False;
     MyFOStruct.hNameMappings := nil;
     Resultval := ShFileOperation(MyFOStruct);
     Result := (ResultVal = 0);
   finally
     Screen.Cursor := crDefault;
   end;

end;

function OpenPropertyPage(AFileOrFolder: string): boolean;
var
  ShExecInfo: TShellExecuteInfo;
begin
  ZeroMemory(@ShExecInfo, SizeOf(ShExecInfo));
  ShExecInfo.cbSize := sizeof(ShExecInfo);
  ShExecInfo.fMask := SEE_MASK_INVOKEIDLIST;
  ShExecInfo.lpVerb := 'properties';
  ShExecInfo.lpFile := PChar(AFileOrFolder);
  ShExecInfo.lpParameters := '';
  ShExecInfo.lpDirectory := '';
  ShExecInfo.nShow := SW_SHOW;
  ShExecInfo.hInstApp := 0;
  Result := ShellExecuteEx(@ShExecInfo);
end;

function ExpandEnvStrings(const AString: String): String;
var
  bufsize: Integer;
begin
  bufsize := ExpandEnvironmentStrings(PChar(AString), nil, 0);
  SetLength(result, bufsize);
  ExpandEnvironmentStrings(PChar(AString), PChar(result), bufsize);
  result := TrimRight(result);
end;

// include winapi methods
// https://www.delphipraxis.net/207745-utc-local-time.html
function TzSpecificLocalTimeToSystemTime(lpTimeZoneInformation: PTimeZoneInformation; var lpLocalTime, lpUniversalTime: TSystemTime): BOOL; stdcall; external kernel32 name 'TzSpecificLocalTimeToSystemTime';
function SystemTimeToTzSpecificLocalTime(lpTimeZoneInformation: PTimeZoneInformation; var lpUniversalTime, lpLocalTime: TSystemTime): BOOL; stdcall; external kernel32 name 'SystemTimeToTzSpecificLocalTime';

// convert local time to UTC
// https://www.delphipraxis.net/207745-utc-local-time.html
function DateTimeToUTC(const Local: TDateTime): TDateTime;
var
  TZI: TTimeZoneInformation;
  LocalTime,
  UniversalTime: TSystemTime;
begin
  GetTimeZoneInformation(TZI);
  DateTimeToSystemTime(Local, LocalTime);
  TzSpecificLocalTimeToSystemTime(@TZI, LocalTime, UniversalTime);
  Result := SystemTimeToDateTime(UniversalTime);
end;

// convert UTC to local time
// https://www.delphipraxis.net/207745-utc-local-time.html
function UTCToLocalDateTime(const UTC: TDateTime): TDateTime;
var
  TZI: TTimeZoneInformation;
  LocalTime,
  UniversalTime: TSystemTime;
begin
  GetTimeZoneInformation(TZI);
  DateTimeToSystemTime(UTC, UniversalTime);
  SystemTimeToTzSpecificLocalTime(@TZI, UniversalTime, LocalTime);
  Result := SystemTimeToDateTime(LocalTime);
end;

procedure SetFileCreationTime(const FileName: string; const DateTime: TDateTime);
const
  FILE_WRITE_ATTRIBUTES = $0100;
var
  Handle: THandle;
  SystemTime: TSystemTime;
  FileTime: TFileTime;
begin
  // https://stackoverflow.com/questions/8446208/delphi-6-how-can-i-change-created-filedate-file-creation-date/8446338#8446338
  // Modified
  Handle := CreateFile(PChar(FileName), FILE_WRITE_ATTRIBUTES,
    FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING,
    FILE_ATTRIBUTE_NORMAL, 0);
  if Handle=INVALID_HANDLE_VALUE then
    RaiseLastOSError;
  try
    DateTimeToSystemTime(DateTimeToUTC(DateTime), SystemTime); // DateTime zu DateTimeToUTC(DateTime) geändert, DM 30.03.2023
    if not SystemTimeToFileTime(SystemTime, FileTime) then
      RaiseLastOSError;
    if not SetFileTime(Handle, @FileTime, @FileTime, @FileTime) then // Modification + AccessTime hinzugefügt, nicht nur Create
      RaiseLastOSError;
  finally
    CloseHandle(Handle);
  end;
end;

function FileSize(const aFilename: String): Int64;
var
  info: TWin32FileAttributeData;
begin
  result := -1;

  FillChar(info, SizeOf(info), 0);
  if GetFileAttributesEx(PChar(aFileName), GetFileExInfoStandard, @info) then
  begin
    Int64Rec(Result).Hi := info.nFileSizeHigh;
    Int64Rec(Result).Lo := info.nFileSizeLow;
  end;
end;

function CleanFileName(const InputString: string): string;
var
  i: integer;
  ResultWithSpaces: string;
begin

  ResultWithSpaces := InputString;

  for i := 1 to Length(ResultWithSpaces) do
  begin
    // These chars are invalid in file names.
    case ResultWithSpaces[i] of 
      '/', '\', ':', '*', '?', '"', '<', '>', '|', ' ', #$D, #$A, #9:
        // Use a * to indicate a duplicate space so we can remove
        // them at the end.
        {$WARNINGS OFF} // W1047 Unsafe code 'String index to var param'
        if (i > 1) and
          ((ResultWithSpaces[i - 1] = ' ') or (ResultWithSpaces[i - 1] = '*')) then
          ResultWithSpaces[i] := '*'
        else
          ResultWithSpaces[i] := ' ';

        {$WARNINGS ON}
    end;
  end;

  // A * indicates duplicate spaces.  Remove them.
  result := ReplaceStr(ResultWithSpaces, '*', '');

  // Also trim any leading or trailing spaces
  result := Trim(Result);

  (*
  if result = '' then
  begin
    raise(Exception.Create('Resulting FileName was empty Input string was: '
      + InputString));
  end;
  *)
end;

function FilesAreEqual(const File1, File2: TFileName): Boolean;
const
  BlockSize = 65536;
var
  fs1, fs2: TFileStream;
  L1, L2: Integer;
  B1, B2: array[1..BlockSize] of Byte;
begin
  Result := False;
  fs1 := TFileStream.Create(File1, fmOpenRead or fmShareDenyWrite);
  try    
    fs2 := TFileStream.Create(File2, fmOpenRead or fmShareDenyWrite);
    try      
      if fs1.Size = fs2.Size then 
      begin
        while fs1.Position < fs1.Size do 
        begin          
          L1 := fs1.Read(B1[1], BlockSize);
          L2 := fs2.Read(B2[1], BlockSize);
          if L1 <> L2 then 
          begin            
            Exit;
          end;
          if not CompareMem(@B1[1], @B2[1], L1) then Exit;        
        end;        
        Result := True;
      end;
    finally      
      FreeAndNil(fs2);
    end;  
  finally    
    FreeAndNil(fs1);
  end;
end;

function GetDesktopFolder: string;
var
  PIDList: PItemIDList;
  Buffer: array [0..MAX_PATH-1] of Char;
begin
  Result := '';
  SHGetSpecialFolderLocation(Application.Handle, CSIDL_DESKTOP, PIDList);
  if Assigned(PIDList) then
    if SHGetPathFromIDList(PIDList, Buffer) then
      Result := Buffer;
end;

function CreateDesktopShellLink(const TargetName, Args, ALinkName, AIconName: string; AIconIndex: Integer): Boolean;
var
  IObject: IUnknown;
  ISLink: IShellLink;
  IPFile: IPersistFile;
  PIDL: PItemIDList;
  LinkName: WideString;
  InFolder: array [0..MAX_PATH-1] of Char;
begin
  Result := False;

  IObject := CreateComObject(CLSID_ShellLink);
  ISLink := IObject as IShellLink;
  IPFile := IObject as IPersistFile;

  with ISLink do
  begin
    //SetDescription('Description ...');
    SetPath(PChar(TargetName));
    SetArguments(PChar(Args));
    SetIconLocation(PChar(AIconName), AIconIndex);
    SetWorkingDirectory(PChar(ExtractFilePath(TargetName)));
  end;

  SHGetSpecialFolderLocation(0, CSIDL_DESKTOPDIRECTORY, PIDL);
  SHGetPathFromIDList(PIDL, InFolder) ;

  LinkName := IncludeTrailingPathDelimiter(GetDesktopFolder) + ALinkName + '.lnk';

  Deletefile(LinkName);

  if not FileExists(LinkName) then
    if IPFile.Save(PWideChar(LinkName), False) = S_OK then
      Result := True;
end;

procedure FilePutContentsA(filename, binary: AnsiString);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(filename, fmCreate);
  try
    fs.WriteBuffer(binary[1], Length(binary));
  finally
    FreeAndNil(fs);
  end;
end;

procedure FilePutContentsW(filename, binary: WideString);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(filename, fmCreate);
  try
    fs.WriteBuffer(binary[1], Length(binary));
  finally
    FreeAndNil(fs);
  end;
end;

procedure CopyFiles(Source, Target: string); // Kann auch mit WildCards umgehen!
var
  FO: TShFileOpStruct;
begin
   FillChar(FO,SizeOf(FO),#0);
   FO.Wnd := 0;
   FO.wFunc := FO_COPY;
   FO.pFrom := PChar(Source + #0);
   FO.pTo := PChar(Target + #0);
   FO.fFlags := FOF_NOCONFIRMATION or FOF_SILENT;
   ShFileOperation(FO); // TODO: check result
end;

procedure AnDenAnfangScrollen(riched: TCustomRichEdit);
begin
  riched.SelStart := 0;
  riched.Perform(EM_SCROLLCARET, 0, 0);
end;

procedure AnsEndeScrollen(riched: TCustomRichEdit);
begin
  riched.SelStart := riched.GetTextLen;
  riched.Perform(EM_SCROLLCARET, 0, 0);
end;

function DirectoryExistsAndIsNotEmpty(dirname: string): Boolean;
var
  SR: TSearchRec;
begin
  Result := false;
  if FindFirst(IncludeTrailingPathDelimiter(dirname) + '*', faAnyFile, SR) = 0 then
  begin
    repeat
      if (Copy(SR.Name,1,1) <> '.') and (UpperCase(SR.Name) <> 'THUMBS.DB') then
      begin
        Result := true;
        Break;
      end;
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;
end;

function GetBuildTimestamp(const ExeFile: string): TDateTime;
var
  fs: TFileStream;
  unixTime: integer;
  peOffset: Integer;
begin
  try
    fs := TFileStream.Create(ExeFile, fmOpenRead or fmShareDenyNone);
    try
      fs.Seek($3C, soFromBeginning);
      fs.Read(peOffset, 4);

      fs.Seek(peOffset+8, soFromBeginning);
      fs.Read(unixTime, 4);

      {$IF CompilerVersion >= 20.0} // geraten
      result := UnixToDateTime(unixTime, false);
      {$ELSE}
      result := UnixToDateTime(unixTime);
      {$IFEND}
    finally
      FreeAndNil(fs);
    end;
  except
    on E: EAbort do
    begin
      Abort;
    end;
    on E: Exception do
    begin
      // Sollte nicht passieren
      if not FileAge(ExeFile, result) then
        raise Exception.CreateFmt('GetBuildTimestamp(%s) fehlgeschlagen', [ExeFile]);
    end;
  end;
end;

function GetOwnBuildTimestamp: TDateTime;
begin
  result := GetBuildTimestamp(ParamStr(0));
end;

// TODO: CODE DUPLIKATE
//       hl.Utils.pas (RichTextToPlainText)
//       hl.Datenbank.CSVExporter.pas (RichTextToPlainText)
//       hcl.Utils.Rtf.pas (ThclUtilsRtf.RtfToPlainText)
function RichTextToPlainText(richText: string): string;
var
  RichEdit1: TRichEdit;
  ss: TStringStream;
begin
  if Copy(richText, 1, 5) <> '{\rtf' then
  begin
    result := richText;
    exit;
  end;
  RichEdit1 := TRichEdit.Create(Application.MainForm);
  try
    // Wenn Visible=true oder Parent=nil, dann geht es nicht...
    RichEdit1.Width := 0;
    RichEdit1.Height := 0;
    RichEdit1.Parent := Application.MainForm;

    // RichEdit1.Text := richText;
    ss := TStringStream.Create(richtext);
    try
      RichEdit1.Lines.LoadFromStream(ss);
    finally
      FreeAndNil(ss);
    end;

    RichEdit1.PlainText := true;
    result := Trim(RichEdit1.Text);
  finally
    FreeAndNil(RichEdit1);
  end;
  if Copy(result, 1, 5) = '{\rtf' then
  begin
    ShowMessage('RTF-To-Text ist fehlgeschlagen!');
  end;
end;

function ExpandEnvStr(const szInput: string): string;
// http://stackoverflow.com/a/2833147/3544341
const
  MAXSIZE = 32768;
begin
  SetLength(Result, MAXSIZE);
  SetLength(Result, ExpandEnvironmentStrings(pchar(szInput),
    @Result[1],length(Result)));
end;

function ShellExecute64(hWnd: HWND; Operation, FileName, Parameters, Directory: PChar; ShowCmd: Integer): HINST;
var
  testExe: string;
begin
  testExe := ThlUtils.Find3264ExeCandidate(FileName);
  ChangeFSRedirection(true);
  try
    // Warum FS Umleitung deaktivieren? (Nur 32 Bit EXE auf 64 Bit OS)
    // Mit der WoW64-Weiterleitung würde ein 32-Bit Prozess bevorzugt
    // 32-Bit Anwendungen öffnen. Das heißt z.B. kein Snipping-Tool!
    result := ShellExecute(hWnd, Operation, PChar(testExe), Parameters, Directory, ShowCmd);
  finally
    ChangeFSRedirection(false);
  end;
end;

function RunCMD(cmdLine: string; WindowMode: integer): boolean;
var
  si: TStartupInfo;
  pi: TProcessInformation;
  sei: TShellExecuteInfo;
  err: Integer;
begin
  ChangeFSRedirection(true);
  try
    // We need a function which does following:
    // 1. Replace the Environment strings, e.g. %SystemRoot%  --> ExpandEnvStr
    // 2. Runs EXE files with parameters (e.g. "cmd.exe /?")  --> WinExec
    // 3. Runs EXE files without path (e.g. "calc.exe")       --> WinExec
    // 4. Runs EXE files without extension (e.g. "calc")      --> WinExec
    // 5. Runs non-EXE files (e.g. "Letter.doc")              --> ShellExecute
    // 6. Commands with white spaces (e.g. "C:\Program Files\xyz.exe") must be enclosed in quotes.

    cmdLine := ExpandEnvStr(cmdLine);
    {$IFDEF UNICODE}
    UniqueString(cmdLine);
    {$ENDIF}

    ZeroMemory(@si, sizeof(si));
    si.cb := sizeof(si);
    si.dwFlags := STARTF_USESHOWWINDOW;
    si.wShowWindow := WindowMode;

    if CreateProcess(nil, PChar(cmdLine), nil, nil, False, 0, nil, nil, si, pi) then
    begin
      CloseHandle(pi.hThread);
      CloseHandle(pi.hProcess);
      result := True;
      Exit;
    end;

    err := GetLastError;
    if (err = ERROR_BAD_EXE_FORMAT) or
       (err = ERROR_BAD_FORMAT) or
       (err = ERROR_FILE_NOT_FOUND{kommt bei URI z.B. 'ms-screensketch:'}) then
    begin
      ZeroMemory(@sei, sizeof(sei));
      sei.cbSize := sizeof(sei);
      sei.lpFile := PChar(cmdLine);
      sei.nShow := WindowMode;

      result := ShellExecuteEx(@sei);
    end
    else
    begin
      result := err = 0;
    end;
  finally
    ChangeFSRedirection(false);
  end;
end;

function GetFileModDate(filename: string): TDateTime;
var
  F : TSearchRec;
begin
  FindFirst(filename,faAnyFile,F);
  Result := FileDateToDateTime(F.Time);
  //if you really wanted an Int, change the return type and use this line:
  //Result := F.Time;
  FindClose(F);
end;

function Rot13char(c: Char): Char;
begin
  Result := c;
  if (c >= 'a') and (c <= 'm') or (c >= 'A') and (c <= 'M') then
    Result := Chr(ord(c) + 13)
  else if (c >= 'n') and (c <= 'z') or (c >= 'N') and (c <= 'Z') then
    Result := Chr(ord(c) - 13);
end;

{$REGION 'Firewall-Funktionen'}

function Firewall_IstPortFreigeschaltet(portno: integer): boolean;
var
  reg: TRegistry;
  sl: TStringList;
  s, s2: string;
begin
  result := false;

  reg := TRegistry.Create;
  sl := TStringList.Create;
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    if reg.OpenKeyReadOnly('SYSTEM\CurrentControlSet\Services\SharedAccess\Parameters\FirewallPolicy\FirewallRules') then
    begin
      reg.GetValueNames(sl);
      for s in sl do
      begin
        // Computer\HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Services\SharedAccess\Parameters\FirewallPolicy\FirewallRules
        // v2.30|Action=Allow|Active=TRUE|Dir=In|Protocol=6|LPort=11000|Name=CORAplus Socket-Dienst Port 11000|
        s2 := UpperCase(reg.ReadString(s));
        if (Pos('|ACTION=ALLOW|', s2) > 0) and
           (Pos('|ACTIVE=TRUE|', s2) > 0) and
           (Pos('|DIR=IN|', s2) > 0) and
           (Pos('|LPORT='+IntToStr(portno)+'|', s2) > 0) then
        begin
          result := true;
          Exit;
        end;
      end;
      reg.CloseKey;
    end;
  finally
    FreeAndNil(reg);
    FreeAndNil(sl);
  end;
end;

Procedure AddLANRule(Name: string; port: integer);
const
  NET_FW_IP_PROTOCOL_TCP = 6;
  NET_FW_ACTION_ALLOW = 1;
var
  CurrentProfiles : OleVariant;
  fwPolicy2       : OleVariant;
  RulesObject     : OleVariant;
  NewRule         : OleVariant;
begin
  Name := StringReplace(Name, '"', '', [rfReplaceAll]);
  Name := StringReplace(Name, '''', '', [rfReplaceAll]);

  if Firewall_IstPortFreigeschaltet(port) then exit;

  {$REGION 'PowerShell (bevorzugt)'}
  // Wir machen es so, weil wir dann automatisch Admin-Rechte einfordern, und weil Windows Server kein FwPolicy2 hat!
  // Hinweis: ShellExecuteWait funktioniert nicht. Nach dem UAC Dialog springt die Funktion zurück zum Programmcode
  if ShellExecute64(0, 'runas', 'powershell', PChar('-command "New-NetFirewallRule -DisplayName '''+Name+''' -Direction Inbound -Action Allow -Protocol TCP -LocalPort '+IntToStr(Port)+'"'), '', SW_HIDE) > 32 then
  begin
    // ShellExecute64() wird erfolgreich beendet, wenn UAC angenommen wurde, NICHT wenn der Ziel-Prozess beendet wurde!
    while ThlUtils.ProcessExists('powershell.exe') and
          not Firewall_IstPortFreigeschaltet(port) do
    begin
      Sleep(100);
    end;
  end;
  if Firewall_IstPortFreigeschaltet(port) then exit;
  {$ENDREGION}

  {$REGION 'netsh (deprecated)'}
  // netsh ist veraltet und wird in Kürze entfernt, daher haben wir zuerst PowerShell probiert
  // Hinweis: ShellExecuteWait funktioniert nicht. Nach dem UAC Dialog springt die Funktion zurück zum Programmcode
  if ShellExecute64(0, 'runas', 'netsh', PChar('advfirewall firewall add rule name="'+Name+'" dir=in action=allow protocol=TCP localport='+IntToStr(Port)), '', SW_NORMAL) > 32 then
  begin
    // ShellExecute64() wird erfolgreich beendet, wenn UAC angenommen wurde, NICHT wenn der Ziel-Prozess beendet wurde!
    while ThlUtils.ProcessExists('netsh.exe') and
          not Firewall_IstPortFreigeschaltet(port) do
    begin
      Sleep(100);
    end;
  end;
  if Firewall_IstPortFreigeschaltet(port) then exit;
  {$ENDREGION}

  {$REGION 'ActiveX/OLE'}

  // Sollte es jetzt immer noch nicht geklappt haben, probieren wir es noch einmal über OLE
  // Vorsicht: ActiveX muss initialisiert sein (CoInitialize) und man braucht Adminrechte!

  // https://theroadtodelphi.com/2013/11/21/using-the-windows-firewall-with-advanced-security-scripting-api-and-delphi/#Adding%20a%20Protocol%20Rule

  // Create the FwPolicy2 object.
  fwPolicy2   := CreateOleObject('HNetCfg.FwPolicy2');
  RulesObject := fwPolicy2.Rules;
  CurrentProfiles := fwPolicy2.CurrentProfileTypes;

  //Create a Rule Object.
  NewRule := CreateOleObject('HNetCfg.FWRule');

  NewRule.Name := Name;
  //NewRule.Description := 'Allow incoming network traffic coming from LAN interface type';
  NewRule.Protocol := NET_FW_IP_PROTOCOL_TCP;
  NewRule.LocalPorts := port;
  //NewRule.Interfacetypes := 'LAN';
  NewRule.Enabled := True;
  //NewRule.Grouping := 'My Group';
  //NewRule.Profiles := CurrentProfiles;
  NewRule.Action := NET_FW_ACTION_ALLOW;

  //Add a new rule
  RulesObject.Add(NewRule);

  if Firewall_IstPortFreigeschaltet(port) then exit;
  {$ENDREGION}

  raise Exception.CreateFmt('Port %d konnte nicht freigeschaltet werden!', [port]);
end;

procedure DateiUndDruckerFreigabeAktivieren(Enable: boolean);
var
  sTmp: string;
const
  GROUP_DE = 'Datei- und Druckerfreigabe';
  GROUP_EN = 'File And Printer Sharing';
begin
  // https://www.c-sharpcorner.com/article/how-to-enable-or-disable-file-and-printer-sharing-in-windows-102/

  {$REGION 'PowerShell (bevorzugt)'}
  if Enable then sTmp := 'True' else sTmp := 'False';
  ShellExecute64(0, 'runas', 'powershell', PChar('-command "Set-NetFirewallRule -DisplayGroup '''+GROUP_DE+''' -Enabled '+sTmp+' -Profile Any"'), '', SW_HIDE);
  ShellExecute64(0, 'runas', 'powershell', PChar('-command "Set-NetFirewallRule -DisplayGroup '''+GROUP_EN+''' -Enabled '+sTmp+' -Profile Any"'), '', SW_HIDE);
  {$ENDREGION}

  {$REGION 'netsh (deprecated)'}
  if Enable then sTmp := 'Yes' else sTmp := 'No';
  ShellExecute64(0, 'runas', 'netsh', PChar('advfirewall firewall set rule group="'+GROUP_DE+'" new enable='+sTmp), '', SW_HIDE);
  ShellExecute64(0, 'runas', 'netsh', PChar('advfirewall firewall set rule group="'+GROUP_EN+'" new enable='+sTmp), '', SW_HIDE);
  {$ENDREGION}
end;

{$ENDREGION}

function hl_GetMachineId: string;
var
  reg: TRegistry;
  ProductId: string;
  MachineGuid: string;
  i: Integer;
begin
  if IsWow64 then
    reg := TRegistry.Create(KEY_READ OR KEY_WOW64_64KEY)
  else
    reg := TRegistry.Create(KEY_READ);
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;

    ProductId := '???';
    if reg.OpenKeyReadOnly('SOFTWARE\Microsoft\Windows NT\CurrentVersion') then
    begin
      ProductId := reg.ReadString('ProductId');
      reg.CloseKey;
    end;

    MachineGuid := '???';
    if reg.OpenKeyReadOnly('SOFTWARE\Microsoft\Cryptography') then
    begin
      MachineGuid := reg.ReadString('MachineGuid');
      reg.CloseKey;
    end;

    ProductId := UpperCase(ProductId);
    MachineGuid := UpperCase(MachineGuid);

    Result := ProductId+'.'+MachineGuid+'.'+UpperCase(MD5String(LowerCase(MD5String('ProductId'+ProductId))+LowerCase(MD5String('MachineGuid'+MachineGuid))));
    for i := 1 to Length(Result) do
    begin
      Result[i] := Rot13char(Result[i]);
    end;
    Result := AnsiReverseString(Result);
  finally
    FreeAndNil(reg);
  end;
end;

procedure SecureDeleteFile(filename: string);
var
  sl: TStringList;
  muellName: string;
  siz: integer;
begin
  sl := TStringList.Create;
  try
    siz := FileSize(filename);
    if siz > 0 then
      sl.Text := StringOfChar('@',siz); // TODO: Das ist recht langsam. Besser wäre ein FileStream
    sl.SaveToFile(filename); // Datei überschreiben, damit Undelete tools nichts wiederherstellen können
  finally
    FreeAndNil(sl);
  end;
  
  muellName := IncludeTrailingPathDelimiter(ExtractFilePath(filename))+RandomString(10)+'.tmp';
  
  if RenameFile(filename, muellName) then
    DeleteFile(muellname)
  else
    DeleteFile(filename);
end;

function PadLeft(const Str: string; Ch: Char; Count: Integer): string;
begin
  // https://stackoverflow.com/questions/1679360/quick-padding-of-a-string-in-delphi
  if Length(Str) < Count then
  begin
    Result := StringOfChar(Ch, Count);
    Move(Str[1], Result[Count - Length(Str) + 1], Length(Str) * SizeOf(Char));
  end
  else Result := Str;
end;

function PadRight(const Str: string; Ch: Char; Count: Integer): string;
begin
  // https://stackoverflow.com/questions/1679360/quick-padding-of-a-string-in-delphi
  if Length(Str) < Count then
  begin
    Result := StringOfChar(Ch, Count);
    Move(Str[1], Result[1], Length(Str) * SizeOf(Char));
  end
  else Result := Str;
end;

function NewGUID: TGUID;
begin
  CreateGUID(result);
end;

function NewGUIDString: string;
begin
  result := GUIDToString(NewGUID);
end;

function IsEmptyGuidString(GuidStr: string): boolean;
begin
	result :=
		(GuidStr = '') or
		(GuidStr = '00000000-0000-0000-0000-000000000000') or
		(GuidStr = '{00000000-0000-0000-0000-000000000000}');
end;

{$IFDEF MSWINDOWS}
function IsEqualGUID; external 'ole32.dll' name 'IsEqualGUID';
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
function IsEqualGUID(const guid1, guid2: TGUID): Boolean;
var
  a, b: PIntegerArray;
begin
  a := PIntegerArray(@guid1);
  b := PIntegerArray(@guid2);
  Result := (a^[0] = b^[0]) and (a^[1] = b^[1]) and (a^[2] = b^[2]) and (a^[3 ] = b^[3]);
end;
{$ENDIF LINUX}

function StrgGedrueckt(Key, Buchstabe: char): boolean;
begin
  // Bei OnKeyPress wird Strg+A als Key=#1 , Strg+B als Key=#2 übergeben usw.
  // Vorsicht: Key=#13 ist aber beispielsweise ENTER und nicht Strg+M !
  // if Ord(Key) < Ord('A') then Key := Chr(Ord(Key) - 1 + Ord('A'));
  if Ord(Key) <= 26 then Key := Chr(Ord(Key) - 1 + Ord('A'));

  // Bei OnKeyDown / OnKeyPress:
  // Hier ist Strg+A wie folgt:  ssCtrl in ShiftState und Key='A'
  result := (GetKeyState(VK_CONTROL)    <{down}0) and
            (GetKeyState(VK_SHIFT)      >={up} 0) and
            (GetKeyState(VK_MENU{Alt!}) >={up} 0) and  // bei VK_MENU handelt es sich um die Alt-Taste, nicht um die Menü-Taste (= VK_APPS)
            (Key = UpCase(Buchstabe));
end;

function StrgShiftGedrueckt(Key, Buchstabe: char): boolean;
begin
  // Bei OnKeyPress wird Strg+A als Key=#1 , Strg+B als Key=#2 übergeben usw.
  // Vorsicht: Key=#13 ist aber beispielsweise ENTER und nicht Strg+M !
  if Ord(Key) < Ord('A') then Key := Chr(Ord(Key) - 1 + Ord('A'));

  // Bei OnKeyDown / OnKeyPress:
  // Hier ist Strg+A wie folgt:  ssShift in ShiftState and ssCtrl in ShiftState and Key='A'
  result := (GetKeyState(VK_CONTROL)    <{down}0) and
            (GetKeyState(VK_SHIFT)      <{down}0) and
            (GetKeyState(VK_MENU{Alt!}) >={up} 0) and  // bei VK_MENU handelt es sich um die Alt-Taste, nicht um die Menü-Taste (= VK_APPS)
            (Key = UpCase(Buchstabe));
end;

function GetDirSize(dir: string; subdir: Boolean): Int64;
var
  rec: TSearchRec;
  found: Integer;
begin
  Result := 0;
  if not DirectoryExists(dir) then exit;
  dir := IncludeTrailingPathDelimiter(dir);
  found := FindFirst(dir + '*.*', faAnyFile, rec);
  while found = 0 do
  begin
    Inc(Result, rec.Size);
    if subdir and (rec.Attr and faDirectory > 0) and (rec.Name[1] <> '.') then
      Inc(Result, GetDirSize(dir + rec.Name, True));
    found := FindNext(rec);
  end;
  FindClose(rec);
end;

function SendArp(DestIP,SrcIP:ULONG;pMacAddr:pointer;PhyAddrLen:pointer) : DWord; StdCall; external 'iphlpapi.dll' name 'SendARP';

function GetRouterMac(debug: boolean=false): string;

  function GetMacAddr(const IPAddress: string; var ErrCode : DWORD): string;
  var
    MacAddr    : Array[0..5] of Byte;
    DestIP     : ULONG;
    PhyAddrLen : ULONG;
    WSAData    : TWSAData;
  begin
    // https://stackoverflow.com/questions/4550672/delphi-get-mac-of-router
    Result    :='';
    WSAStartup($0101, WSAData);
    try
      ZeroMemory(@MacAddr,SizeOf(MacAddr));
      DestIP    :=inet_addr(PAnsiChar(AnsiString(IPAddress)));
      PhyAddrLen:=SizeOf(MacAddr);
      ErrCode   :=SendArp(DestIP,0,@MacAddr,@PhyAddrLen);
      if ErrCode = S_OK then
       Result:=Format('%2.2x-%2.2x-%2.2x-%2.2x-%2.2x-%2.2x',[MacAddr[0], MacAddr[1],MacAddr[2], MacAddr[3], MacAddr[4], MacAddr[5]])
    finally
      WSACleanup;
    end;
  end;

var
  gateway: string;
  ec: DWORD;
  macrouter: string;
  sl: TStringList;
  serr: string;
const
  DELIM = ',';
begin
  result := '';

  gateway := GetWMIarray('.', 'root\CIMV2', 'Win32_NetworkAdapterConfiguration', 'DefaultIPGateway', DELIM);
  gateway := StringReplace(gateway,'{','',[rfReplaceAll]);
  gateway := StringReplace(gateway,'}','',[rfReplaceAll]);

  sl := TStringList.Create;
  try
    sl.Delimiter := DELIM;
    sl.DelimitedText := gateway;
    if sl.Count = 0 then
    begin
      if debug then
        macrouter := 'ERR_NO_ADAPTERS'
      else
        macrouter := '';
    end
    else
    begin
      try
        macrouter := GetMacAddr(sl[0],ec);
      except
        on E: EAbort do
        begin
          Abort;
        end;
        on E: Exception do
        begin
          if debug then
            macrouter := 'ERR_EXCEPT_'+E.Message
          else
            macrouter := '';
        end;
      end;

      if ec = ERROR_BAD_NET_NAME then
        serr := 'ERROR_BAD_NET_NAME'
      else if ec = ERROR_BUFFER_OVERFLOW then
        serr := 'ERROR_BUFFER_OVERFLOW'
      else if ec = ERROR_GEN_FAILURE then
        serr := 'ERROR_GEN_FAILURE'
      else if ec = ERROR_INVALID_PARAMETER then
        serr := 'ERROR_INVALID_PARAMETER'
      else if ec = ERROR_INVALID_USER_BUFFER then
        serr := 'ERROR_INVALID_USER_BUFFER'
      else if ec = 1168(*ERROR_NOT_FOUND*) then
        serr := 'ERROR_NOT_FOUND'
      else if ec = ERROR_NOT_SUPPORTED then
        serr := 'ERROR_NOT_SUPPORTED'
      else if ec = ERROR_NETWORK_UNREACHABLE then // not documented in MSDN WinApi
        serr := 'ERROR_NETWORK_UNREACHABLE'
      else if ec <> S_OK then
        serr := 'ERROR_' + IntToStr(ec);

      if ec <> 0 then
      begin
        if debug then
          macrouter := serr
        else
          macrouter := '';
      end;
    end;
  finally
    FreeAndNil(sl);
  end;

  result := macrouter;
end;

function DayOfWeekGerman(ADate: TDateTime): Integer;
begin
  ADate := Trunc(ADate); // hinzugefügt DM 17.08.2022

  // https://www.delphipraxis.net/86137-kalenderwoche-ermitteln.html
  Result := DayOfWeek(ADate) -1;
  if Result <= 0 then Result := 7;
end;

function WeekOfDate(A: TDateTime): Integer;
var
  Day: Integer;
  Y,M,D: Word;
begin
  A := Trunc(A); // hinzugefügt DM 17.08.2022

  // https://www.delphipraxis.net/86137-kalenderwoche-ermitteln.html
  Day := DayOfWeekGerman(A);
  DecodeDate(A +4 -Day, Y, M, D);
  Result := Round(((A +8 -Day) - EncodeDate(Y, 1, 1)) / 7);
end;

procedure SetGermanLocale;
begin
  // Damit CORA auch auf ausländischen Rechnern korrekt läuft, hier die
  // Deutschen Locale

  FormatSettings.CurrencyString := '€';
  FormatSettings.CurrencyFormat := 3;
  FormatSettings.NegCurrFormat := 8;
  FormatSettings.ThousandSeparator := '.';
  FormatSettings.DecimalSeparator := ',';
  FormatSettings.CurrencyDecimals := 2;
  FormatSettings.DateSeparator := '.';
  FormatSettings.ShortDateFormat := 'dd.MM.yyyy';
  FormatSettings.LongDateFormat := 'dddd, d. MMMM yyyy';
  FormatSettings.TimeSeparator := ':';
  FormatSettings.TimeAMString := '';
  FormatSettings.TimePMString := '';
  FormatSettings.ShortTimeFormat := 'hh:mm';
  FormatSettings.LongTimeFormat := 'hh:mm:ss';

  FormatSettings.ShortMonthNames[1] := 'Jan';
  FormatSettings.ShortMonthNames[2] := 'Feb';
  FormatSettings.ShortMonthNames[3] := 'Mrz';
  FormatSettings.ShortMonthNames[4] := 'Apr';
  FormatSettings.ShortMonthNames[5] := 'Mai';
  FormatSettings.ShortMonthNames[6] := 'Jun';
  FormatSettings.ShortMonthNames[7] := 'Jul';
  FormatSettings.ShortMonthNames[8] := 'Aug';
  FormatSettings.ShortMonthNames[9] := 'Sep';
  FormatSettings.ShortMonthNames[10] := 'Okt';
  FormatSettings.ShortMonthNames[11] := 'Nov';
  FormatSettings.ShortMonthNames[12] := 'Dez';

  FormatSettings.LongMonthNames[1] := 'Januar';
  FormatSettings.LongMonthNames[2] := 'Februar';
  FormatSettings.LongMonthNames[3] := 'März';
  FormatSettings.LongMonthNames[4] := 'April';
  FormatSettings.LongMonthNames[5] := 'Mai';
  FormatSettings.LongMonthNames[6] := 'Juni';
  FormatSettings.LongMonthNames[7] := 'Juli';
  FormatSettings.LongMonthNames[8] := 'August';
  FormatSettings.LongMonthNames[9] := 'September';
  FormatSettings.LongMonthNames[10] := 'Oktober';
  FormatSettings.LongMonthNames[11] := 'November';
  FormatSettings.LongMonthNames[12] := 'Dezember';

  FormatSettings.ShortDayNames[1] := 'So';
  FormatSettings.ShortDayNames[2] := 'Mo';
  FormatSettings.ShortDayNames[3] := 'Di';
  FormatSettings.ShortDayNames[4] := 'Mi';
  FormatSettings.ShortDayNames[5] := 'Do';
  FormatSettings.ShortDayNames[6] := 'Fr';
  FormatSettings.ShortDayNames[7] := 'Sa';

  FormatSettings.LongDayNames[1] := 'Sonntag';
  FormatSettings.LongDayNames[2] := 'Montag';
  FormatSettings.LongDayNames[3] := 'Dienstag';
  FormatSettings.LongDayNames[4] := 'Mittwoch';
  FormatSettings.LongDayNames[5] := 'Donnerstag';
  FormatSettings.LongDayNames[6] := 'Freitag';
  FormatSettings.LongDayNames[7] := 'Samstag';

  SysLocale.DefaultLCID := 1031;
  SysLocale.PriLangID := 7;
  SysLocale.SubLangID := 1;
  SysLocale.FarEast := false;
  SysLocale.MiddleEast := true;

  FormatSettings.TwoDigitYearCenturyWindow := 50;
  FormatSettings.ListSeparator := ';';
end;

function MyDocumentsPath: string;
var
  Allocator: IMalloc;
  SpecialDir: PItemIdList;
  FBuf: array[0..MAX_PATH] of Char;
begin
  result := '';
  if SHGetMalloc(Allocator) = NOERROR then
  begin
    SHGetSpecialFolderLocation(0, CSIDL_PERSONAL, SpecialDir);
    SHGetPathFromIDList(SpecialDir, @FBuf[0]);
    Allocator.Free(SpecialDir);
    result := string(FBuf);
  end;
end;

procedure HideFile(fname: string);
begin
  FileSetAttr(fname, FileGetAttr(fname) or faHidden);
end;

{$IFDEF UNICODE}
function GetUserNameEx(NameFormat: DWORD; lpNameBuffer: LPWSTR; nSize: PULONG): DWORD;
          stdcall; external 'secur32.dll' Name 'GetUserNameExW';
{$ELSE}
function GetUserNameEx(NameFormat: DWORD; lpNameBuffer: LPSTR; nSize: PULONG): DWORD;
          stdcall; external 'secur32.dll' Name 'GetUserNameExA';
{$ENDIF}

function GetWindowsDisplayUserName: string;
const
  cnMaxUserNameLen = 254;
  EXTENDED_NAME_FORMAT_NameUnknown = 0;
  EXTENDED_NAME_FORMAT_NameFullyQualifiedDN = 1;
  EXTENDED_NAME_FORMAT_NameSamCompatible = 2;
  EXTENDED_NAME_FORMAT_NameDisplay = 3;
  EXTENDED_NAME_FORMAT_NameUniqueId = 6;
  EXTENDED_NAME_FORMAT_NameCanonical = 7;
  EXTENDED_NAME_FORMAT_NameUserPrincipal = 8;
  EXTENDED_NAME_FORMAT_NameCanonicalEx = 9;
  EXTENDED_NAME_FORMAT_NameServicePrincipal = 10;
  EXTENDED_NAME_FORMAT_NameDnsDomain = 12;
  EXTENDED_NAME_FORMAT_NameGivenName = 13;
  EXTENDED_NAME_FORMAT_NameSurname = 14;
var
  sUserName     : string;
  dwUserNameLen : DWord;
begin
  // https://www.viathinksoft.de/codelib/209
  dwUserNameLen := cnMaxUserNameLen-1;
  SetLength( sUserName, cnMaxUserNameLen );
  if GetUserNameEx(
    EXTENDED_NAME_FORMAT_NameDisplay,
    PChar( sUserName ),
    @dwUserNameLen ) = 0 then
  begin
    Result := '';
    RaiseLastOsError;
  end;

  // There is probably a bug in Win10/Win11 in GetUserNameExW
  // When the attribute does not exist (e.g. the user has no display name),
  // then GetUserNameEx!=0 (success), but nSize stays untouched, not changed to 0!!!
  if dwUserNameLen = cnMaxUserNameLen-1 then dwUserNameLen := 0;

  SetLength( sUserName, dwUserNameLen );
  Result := sUserName;
end;

// https://www.delphipraxis.net/189175-feststellen-ob-als-administrator-ausgefuehrt.html
Const
 SECURITY_NT_AUTHORITY: TSIDIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 5));
 SECURITY_BUILTIN_DOMAIN_RID = $00000020;
 DOMAIN_ALIAS_RID_ADMINS = $00000220;
 DOMAIN_ALIAS_RID_USERS = $00000221;
 DOMAIN_ALIAS_RID_GUESTS = $00000222;
 DOMAIN_ALIAS_RID_POWER_USERS= $00000223;
function CheckTokenMembership(TokenHandle: THandle; SidToCheck: PSID; var IsMember: BOOL): BOOL; stdcall; external advapi32;
function IsUserAdmin: Boolean;
var
  IsMember : BOOL;
  AdministratorsGroup: PSID;
begin
  Win32Check( AllocateAndInitializeSid(
    {} SECURITY_NT_AUTHORITY,
    {} 2, // 2 sub-authorities
    {} SECURITY_BUILTIN_DOMAIN_RID, // sub-authority 0
    {} DOMAIN_ALIAS_RID_ADMINS, // sub-authority 1
    {} 0, 0, 0, 0, 0, 0, // sub-authorities 2-7 not passed
    {} AdministratorsGroup ) );
  try
    Win32Check( CheckTokenMembership( 0, AdministratorsGroup, IsMember ) );
  finally
    FreeSid( AdministratorsGroup );
  end;
  Result := IsMember;
end;

procedure MDI_Form_BringToFront(frm: TForm);
begin
  // Anmerkung DM: Das ist bestimmt alles doppelt gemoppelt. Sollte man nochmal genau prüfen!
  if frm.WindowState = wsMinimized then
    frm.WindowState := wsNormal;
  frm.Show;
  LockWindowUpdate(frm.Handle);
  frm.BringToFront;
  LockWindowUpdate(0);
  // if frm.WindowState = wsMinimized then frm.WindowState := wsNormal;
  if frm.WindowState = wsMinimized then ShowWindow(frm.Handle, SW_RESTORE);
//  Application.ProcessMessages;
end;

function DaysHumanReadable(days: integer): string;
begin
  if days >= 2*12*30 then
    result := IntToStr(days div (30*12)) + ' Jahren'
  else if days >= 12*30 then
    result := '1 Jahr'

  else if days >= 2*30 then
    result := IntToStr(days div 30) + ' Monaten'
  else if days >= 2*30 then
    result := '1 Monat'

  else if days >= 2 then
    result := IntToStr(days) + ' Tagen'
  else
    result := '1 Tag';
end;

function WindowsBits: integer;
begin
  // Gibt 32 oder 64 aus, je nachdem ob WINDOWS 32 oder 64 Bit ist.
	// NICHT die Anwendung, sondern WINDOWS!
  {$IFDEF WIN64}
  result := 64;
  {$ELSE}
  if IsWow64 then
    result := 64
  else
    result := 32;
  {$ENDIF}
end;

procedure BmpToPngFile(bmpFile, pngFile: string);
var
  bmp: TBitmap;
  png: TPNGObject;
begin
  bmp := TBitmap.Create;
  png := TPNGObject.Create;
  try
    bmp.LoadFromFile(bmpFile);
    png.Assign(bmp);
    png.SaveToFile(pngFile);
  finally
    FreeAndNil(bmp);
    FreeAndNil(png);
  end;
end;

function Hash_djb2(str: string): string;

  function _Hash_djb2(AData: Pointer; ADataLength: Integer): Cardinal;
  var
    i: integer;
  begin
    // https://helloacm.com/simple-and-fast-hash-functions-in-delphi/
    {$OVERFLOWCHECKS OFF}
    Result := 5381;
    for i := 1 to ADataLength do
    begin
      Result := ((Result shl 5) + Result) + PByte(AData)^;
      AData  := Pointer(NativeUInt(AData) + 1);
    end;
    {$OVERFLOWCHECKS ON}
  end;

begin
  result := Format('%x',[
    _Hash_djb2(PChar(str), Length(str)*sizeof(char))
  ]);
end;

function GetDeepestDir(const aFilename: string): string;
begin
  result := ExtractFileName(ExtractFileDir(afilename));
end;

procedure EmptyKeyQueue;
var
  Msg: TMsg;
begin
  // https://www.swissdelphicenter.ch/en/showcode.php?id=1066
  while PeekMessage(Msg, 0, WM_KEYFIRST, WM_KEYLAST,
    PM_REMOVE or PM_NOYIELD) do;
end;

procedure EmptyMouseQueue;
var
  Msg: TMsg;
begin
  // https://www.swissdelphicenter.ch/en/showcode.php?id=1066
  while PeekMessage(Msg, 0, WM_MOUSEFIRST, WM_MOUSELAST,
    PM_REMOVE or PM_NOYIELD) do;
end;

function GetWinDir: string;
var
  dir: array [0..MAX_PATH] of Char;
begin
  // https://www.swissdelphicenter.ch/en/showcode.php?id=144
  GetWindowsDirectory(dir, MAX_PATH);
  Result := StrPas(dir);
end;

function DarkModeIsEnabled: boolean;  // https://github.com/checkdigits/delphidarkmode/blob/master/WindowsDarkMode.pas
{$IFDEF MSWINDOWS}
const
  TheKey   = 'Software\Microsoft\Windows\CurrentVersion\Themes\Personalize\';
  TheValue = 'AppsUseLightTheme';
var
  Reg: TRegistry;
{$ENDIF}
begin

  Result := False;  // There is no dark side - the Jedi are victorious!

// This relies on a registry setting only available on MS Windows
// If the developer has somehow managed to get to this point then tell
// them not to do this!
{$IFNDEF MSWINDOWS}
{$MESSAGE WARN '"DarkModeIsEnabled" will only work on MS Windows targets'}
{$ELSE}
  Reg    := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.KeyExists(TheKey) then
      if Reg.OpenKey(TheKey, False) then
      try
        if Reg.ValueExists(TheValue) then
          Result := Reg.ReadInteger(TheValue) = 0;
      finally
        Reg.CloseKey;
      end;
  finally
    FreeAndNil(Reg);
  end;
{$ENDIF}
end;

function IsStrANumber(const S: string): Boolean;
var
  P: PChar;
begin
  P      := PChar(S);
  Result := False;
  while P^ <> #0 do
  begin
    if not (P^ in ['0'..'9']) then Exit;
    Inc(P);
  end;
  Result := True;
end;

function GetSysDir: string;
var
  Buffer: array[0..MAX_PATH] of Char;
begin
  GetSystemDirectory(Buffer, MAX_PATH - 1);
  SetLength(Result, StrLen(Buffer));
  Result := Buffer;
end;

function Rfc3339ToDatetime(rfc: string): TDatetime;
var
  jahr, monat, tag, stunde, minute, sekunde, off_h, off_m: integer;
  utc: TDateTime;
begin
  // 2020-07-27T10:03:33+0000
  // 123456789012345678901234
  jahr    := StrToInt(Copy(rfc,  1, 4));
  monat   := StrToInt(Copy(rfc,  6, 2));
  tag     := StrToInt(Copy(rfc,  9, 2));
  stunde  := StrToInt(Copy(rfc, 12, 2));
  minute  := StrToInt(Copy(rfc, 15, 2));
  sekunde := StrToInt(Copy(rfc, 18, 2));
  off_h   := StrToInt(Copy(rfc, 21, 2));
  off_m   := StrToInt(Copy(rfc, 23, 2));
  utc     := EncodeDateTime(jahr, monat, tag, stunde-off_h, minute-off_m, sekunde, 0);
  result  := UTCToLocalDateTime(utc);
end;

function IsOdbcDriverInstalled(const DriverName: string): Boolean;
const
  RootKeys: array[0..1] of string = (
    'SOFTWARE\ODBC\ODBCINST.INI\ODBC Drivers',          // 64-Bit Treiber
    'SOFTWARE\WOW6432Node\ODBC\ODBCINST.INI\ODBC Drivers' // 32-Bit Treiber (auf 64-Bit Windows)
  );
var
  Reg: TRegistry;
  DriversKey: string;
  i: Integer;
begin
  Result := False;
  Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;

    // Prüfe beide Pfade (64-Bit und 32-Bit)
    for i := 0 to High(RootKeys) do
    begin
      if Reg.OpenKeyReadOnly(RootKeys[i]) then
      begin
        if Reg.ValueExists(DriverName) then
        begin
          Result := True;
          Exit; // Falls gefunden, sofort beenden
        end;
        Reg.CloseKey;
      end;
    end;
  finally
    Reg.Free;
  end;
end;

end.
