unit hl.Utils;

interface

uses
  Windows, Messages, Forms, Classes, SysUtils, Math, Controls, ShellAPI, ShlObj,
  ActiveX, DB,
{$IF CompilerVersion >= 20.0}IOUtils, {$IFEND}
  StdCtrls, ZLib, DBCtrls, ADODB, SHFolder, ComCtrls, Dialogs;

type
  /// <summary>Statische Klasse mit Hilfsfunktionen. Die Klasse ist eine Art Namespace. Es wurde auf prozedurale Entwicklung verzichtet, um die XML-Dokumentation zu ermöglichen</summary>
  ThlUtils = class(TObject)
  public
    /// <summary>
    /// PHP like implode function. Returns a string containing a string representation of all the array elements in the same order, with the glue string between each element.
    /// </summary>
    /// <see>http://users.atw.hu/delphicikk/listaz.php?id=1622&oldal=45</see>
    class function implode(const glue: string; const pieces: array of string)
      : string; static;
    class function GermanDayOfWeek(date: TDate): integer; static;

    class function InBlöckeAufspalten(s: string; blockgröße: integer)
      : string; static;

    /// <see>http://delphi.about.com/od/adptips2005/qt/leadingzero.htm</see>
    class function AddLeadingZeroes(const aNumber, Length: integer)
      : string; static;

    class function ShellExecuteWait64(aWnd: HWND; Operation: string;
      ExeName: string; Params: string; WorkingDirectory: string;
      ncmdShow: integer; wait: boolean): integer;
    class function ShellExecuteWait(aWnd: HWND; Operation: string;
      ExeName: string; Params: string; WorkingDirectory: string;
      ncmdShow: integer; wait: boolean): integer;
    class function Find3264ExeCandidate(ExeName: string): string;

    class function GetUserDir: string; static;
    class function GetUserDocumentsDir: string; static;

    class function AdvSelectDirectory(const Caption: string;
      const Root: WideString; var Directory: string; EditBox: boolean = False;
      ShowFiles: boolean = False; AllowCreateDirs: boolean = True): boolean;

    class procedure DeleteDirectory(const DirName: string); static;

    class procedure ProcessMessages(PleaseCancel: PBoolean = nil);

    class procedure ListFiles(Directory: string; list: TStrings;
      recursive: boolean);

    class procedure ChecksummenErzeugen_SFV(const DirName: string;
      SFVFileName: string = 'Checksums.sfv');
    class procedure ChecksummenErzeugen_MD5(const DirName: string;
      SFVFileName: string = 'Checksums.md5');

    class function GetParentDir(const dir: string): string;

    class function PathsEqual(const pathA, pathB: string): boolean;

    class function DirectoryEmpty(const dir: string): boolean;

    class function GetFileSize(const filename: string): int64;

    class function ValidWinFileName(filename: String;
      islong: boolean = True): boolean;

    class function Cut(s: string; delim: char; piece: integer): string; static;
    class function nullenRechtsEntfernen(s: string): string; static;
    class function AmiFloat(d: double): string; static;

    class function GetFileVersion(filename: string): string;

    class function GetComputerName: string;
    class function GetWindowsUserName: String;
    class function GetDomainName: string;

    class procedure TryFreeDiskSpace(level: integer = 0);
    class function DeleteFiles(const AFile: string): boolean;

    class function processExists(exeFileName: string): boolean;

    class function GetInside(s, delimA, delimB: string): string;
    class function FileIsReadable(filename: string): boolean;
    class function EinigeDateienNichtLesbar(filemask: string): boolean;
    class function AbsToRel(const AbsPath, BasePath: string): string;
    class function RelToAbs(const RelPath, BasePath: string): string;
    class function GetModificationTimeOfFile(const AFileName: String)
      : TDateTime; static;

    class function FileGetContents(const filename: string): string; static;

    class function DaysAge(const filename: string): integer; static;

    class function KillTask(exeFileName: string): integer;

    class procedure RequeryAndGotoSameSpot(ds: TAdoQuery);
  end;

function FloatToStrForSQL(aValue: extended; NKStellen: integer): string; overload;
function FloatToStrForSQL(aValue: extended): string; overload;

function FloatToStrForCRW(aValue: extended; NKStellen: integer): string; overload;
function FloatToStrForCRW(aValue: extended): string; overload;

function BoolToStrForSQL(aValue: boolean): string;
function hclBoolToStr(aValue: boolean): string;

function FRound(Value: extended; iAnzahl: integer): extended;
function Runden05(aValue: double): double;
function Runden10(aValue: double): double;
function hlFieldNameAnpassen(x: string): string;
function CheckEmpty(aValue: string): string;
function Split(aLine, aSplitString: string): TStringList; overload;
procedure Split(Delimiter: char; Str: string; ListOfStrings: TStrings);
  overload;

function CheckAscii(sValue: ansistring): ansistring;

function GetNextPossibleForm(ctrl: TComponent): TForm;

procedure LoadStringListFromResource(const ResName: string; SL: TStrings;
  ResType: PChar = RT_RCDATA; otherHInstance: HMODULE = 0);

function GetTempDir: string;

procedure CropFrame(frame: TFrame; onlyVisible: boolean;
  rightPadding, bottomPadding: integer);

function StringXorCrypt(const text, key: ansistring): ansistring;
procedure SaveStrToFile(const filename, SourceString: string);
function LoadFileToStr(const filename: TFileName): ansistring;
Function DeCompressAnsiString(const Input: ansistring): ansistring;
Function CompressAnsiString(const Input: ansistring): ansistring;

function EnterToTabAllowed(f: TForm): boolean;

function RandomString(strlength: integer): string;

procedure SplitChar(Delimiter: char; Str: string; ListOfStrings: TStrings);
procedure SplitText(Delimiter: string; Str: string; ListOfStrings: TStrings);

function IsDirectoryWriteable(const AName: string): boolean;

function FindAWindow(WinCaption: string; WinClassName: string): THandle;
function ProcessIDFromAppname32(appname: string): DWORD;
function KillProcess(ExeName: string): boolean; overload;
function KillProcess(PID: DWORD): boolean; overload;

function ZahlEingetippt(key: char): boolean;
function NichtZahlEingetippt(key: char): boolean;

function GetCharFromVirtualKey(key: Word): char;

procedure GetUsbDrives(list: TStrings);

procedure MoveFilesWildcards(Source, Target: string);
procedure DeleteFilesWildcards(Source: string; recyclebin: boolean);

function IsWindows10: boolean;
function IsWindows11: boolean;

function Crw11_IstInstalliert: boolean;
function Crw13_IstInstalliert: boolean;

function IsVCRuntime2022_32Bit_Installed: boolean;
function IsVCRuntime2022_32Bit_Version: string;

function IsVCRuntime2022_64Bit_Installed: boolean;
function IsVCRuntime2022_64Bit_Version: string;

function IsWow64: boolean;
function ChangeFSRedirection(bDisable: boolean): boolean;
function FixedDrive(Drive: char): boolean;
function CopyFolder(const SrcFolder, DestFolder: String; iFileOp: integer;
  OverWrite: boolean; ShowDialog: boolean): boolean;
function OpenPropertyPage(AFileOrFolder: string): boolean;
function ExpandEnvStrings(const AString: String): String;

function DateTimeToUTC(const Local: TDateTime): TDateTime;
function UTCToLocalDateTime(const UTC: TDateTime): TDateTime;
procedure SetFileCreationTime(const filename: string;
  const DateTime: TDateTime);

function FileSize(const AFileName: String): int64;
function CleanFileName(const InputString: string): string;

function FilesAreEqual(const File1, File2: TFileName): boolean;

function GetDesktopFolder: string;
function CreateDesktopShellLink(const TargetName, Args, ALinkName,
  AIconName: string; AIconIndex: integer): boolean;

procedure FilePutContentsA(filename, binary: ansistring);
procedure FilePutContentsW(filename, binary: WideString);

procedure CopyFiles(Source, Target: string); // Kann auch mit WildCards umgehen!

procedure AnDenAnfangScrollen(riched: TCustomRichEdit);
procedure AnsEndeScrollen(riched: TCustomRichEdit);

function DirectoryExistsAndIsNotEmpty(DirName: string): boolean;

function GetBuildTimestamp(const ExeFile: string): TDateTime;
function GetOwnBuildTimestamp: TDateTime;

function RichTextToPlainText(richText: string): string;

function ShellExecute64(HWND: HWND; Operation, filename, Parameters,
  Directory: PChar; ShowCmd: integer): HINST;

function ExpandEnvStr(const szInput: string): string;
function RunCMD(cmdLine: string; WindowMode: integer): boolean;

function GetFileModDate(filename: string): TDateTime;

function Rot13char(c: char): char;

function Firewall_IstPortFreigeschaltet(portno: integer): boolean;
Procedure AddLANRule(Name: string; port: integer);
procedure DateiUndDruckerFreigabeAktivieren(Enable: boolean);

function hl_GetMachineId: string;

procedure SecureDeleteFile(filename: string);

function PadLeft(const Str: string; Ch: char; Count: integer): string;
function PadRight(const Str: string; Ch: char; Count: integer): string;
function NewGUID: TGUID;
function NewGUIDString: string;
function IsEmptyGuidString(GuidStr: string): boolean;
function IsEqualGUID(const guid1, guid2: TGUID): boolean;
function StrgGedrueckt(key, Buchstabe: char): boolean;
function StrgShiftGedrueckt(key, Buchstabe: char): boolean;
function GetDirSize(dir: string; subdir: boolean): int64;

function GetRouterMac(debug: boolean = False): string;

function DayOfWeekGerman(ADate: TDateTime): integer;
function WeekOfDate(A: TDateTime): integer;
procedure SetGermanLocale;
function MyDocumentsPath: string;
procedure HideFile(fname: string);
function GetWindowsDisplayUserName: string;

function IsUserAdmin: boolean;

procedure MDI_Form_BringToFront(frm: TForm);
function DaysHumanReadable(days: integer): string;
function WindowsBits: integer;
procedure BmpToPngFile(bmpFile, pngFile: string);
function Hash_djb2(Str: string): string;
function GetDeepestDir(const AFileName: string): string;

procedure EmptyKeyQueue;
procedure EmptyMouseQueue;

function GetWinDir: string;

function DarkModeIsEnabled: boolean;

function IsStrANumber(const s: string): boolean;

function GetSysDir: string;
function Rfc3339ToDatetime(rfc: string): TDateTime;

function IsOdbcDriverInstalled(const DriverName: string): boolean;

function Make_EditDisplayFormat(nachkommastellen: integer;
  istEditFormat: boolean): string;

function WindowsVersionString: string;

function HasReadAccessToFile(const FileName: string): Boolean;
function HasWriteAccessToFile(const FileName: string): Boolean;

procedure SleepWithMessages(Milliseconds: Integer);

type
  TSenderlessNotifyEvent = procedure of object;

implementation

uses
  FormatSettingsCompat, hl.Utils.CRC32, hl.Utils.MD5, StrUtils, TlHelp32,
  Registry, ComObj, DateUtils,
  hl.Utils.WmiUtils, WinSock, PngImage, Graphics;

resourcestring
  StrUngültigeBlockgröße = 'Ungültige Blockgröße';
  StrKonnteDateiSNich = 'Konnte Datei %s nicht löschen!';
  StrPrüfsummenDatei = 'Prüfsummen-Datei';
  StrErstelltMitS = 'erstellt mit %s';
  StrJA = 'JA';
  StrNEIN = 'NEIN';
  StrKomprimierungStimmt = 'Komprimierung stimmt nicht überein.';
  StrIsWow64BadProcess = 'IsWow64: bad process handle';
  StrGetBuildTimestamps = 'GetBuildTimestamp(%s) fehlgeschlagen';
  StrRTFToTextIstFehl = 'RTF-To-Text ist fehlgeschlagen!';
  StrPortDKonnteNicht = 'Port %d konnte nicht freigeschaltet werden!';
  StrDJahren = '%d Jahren';
  Str1Jahr = '1 Jahr';
  StrDMonaten = '%d Monaten';
  Str1Monat = '1 Monat';
  StrDTagen = '%d Tagen';
  Str1Tag = '1 Tag';

class function ThlUtils.processExists(exeFileName: string): boolean;
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
    while integer(ContinueLoop) <> 0 do
    begin
      if ((UpperCase(ExtractFileName(FProcessEntry32.szExeFile))
        = UpperCase(exeFileName)) or (UpperCase(FProcessEntry32.szExeFile)
        = UpperCase(exeFileName))) then
      begin
        Result := True;
      end;
      ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
    end;
  finally
    CloseHandle(FSnapshotHandle);
  end;
end;

class function ThlUtils.AdvSelectDirectory(const Caption: string;
  const Root: WideString; var Directory: string; EditBox: boolean = False;
  ShowFiles: boolean = False; AllowCreateDirs: boolean = True): boolean;
// Quelle: http://www.swissdelphicenter.ch/de/showcode.php?id=1802

{
  Dieser Code zeigt den SelectDirectory-Dialog mit zusätzlichen Erweiterungen:
  - eine Edit-Box, wo der Benutzer den Verzeichnisnamen eingeben kann,
  - auch Dateien können in der Liste angezeigt werden,
  - eine Schaltfläche zum Erstellen neuer Verzeichnisse.
}

// callback function that is called when the dialog has been initialized
// or a new directory has been selected

// Callback-Funktion, die aufgerufen wird, wenn der Dialog initialisiert oder
// ein neues Verzeichnis selektiert wurde
  function SelectDirCB(Wnd: HWND; uMsg: UINT; lParam, lpData: lParam)
    : integer; stdcall;
  // var
  // PathName: array[0..MAX_PATH] of Char;
  begin
    case uMsg of
      BFFM_INITIALIZED:
        SendMessage(Wnd, BFFM_SETSELECTION, Ord(True), integer(lpData));
      // include the following comment into your code if you want to react on the
      // event that is called when a new directory has been selected
      // binde den folgenden Kommentar in deinen Code ein, wenn du auf das Ereignis
      // reagieren willst, das aufgerufen wird, wenn ein neues Verzeichnis selektiert wurde
      { BFFM_SELCHANGED:
        begin
        SHGetPathFromIDList(PItemIDList(lParam), @PathName);
        // the directory "PathName" has been selected
        // das Verzeichnis "PathName" wurde selektiert
        end; }
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
        IDesktopFolder.ParseDisplayName(Application.Handle, nil, POleStr(Root),
          Eaten, RootItemIDList, Flags);
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
        ulFlags := BIF_RETURNONLYFSDIRS or BIF_USENEWUI or BIF_EDITBOX *
          Ord(EditBox) or BIF_BROWSEINCLUDEFILES * Ord(ShowFiles) or
          BIF_NOCREATEDIRS * Ord(not AllowCreateDirs);
        lpfn := @SelectDirCB;
        if Directory <> '' then
          lParam := integer(PChar(Directory));
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
  path: array [0 .. MAX_PATH] of char;
begin
  SHGetFolderPath(0, CSIDL_PROFILE, 0, SHGFP_TYPE_CURRENT, @path[0]);
  Result := IncludeTrailingPathDelimiter(path);
end;

class function ThlUtils.GetUserDocumentsDir: string;
Var
  PI: PItemIDList;
  A: array [0 .. 200] of char;
begin
  SHGetSpecialFolderLocation(0, CSIDL_PERSONAL, PI);
  ShGetPathFromIDList(PI, A);
  Result := A;
end;

class function ThlUtils.AddLeadingZeroes(const aNumber,
  Length: integer): string;
begin
  Result := SysUtils.Format('%.*d', [Length, aNumber]);
end;

// DayOfWeek():       1=So, ..., 7=Sa
// GermanDayOfWeek(): 0=Mo, ..., 6=So
class function ThlUtils.GermanDayOfWeek(date: TDate): integer;
begin
  Result := (DayOfWeek(date) + 5) mod 7;
end;

class function ThlUtils.implode(const glue: string;
  const pieces: array of string): string;
var
  I: integer;
begin
  Result := '';
  for I := 0 to High(pieces) do
    Result := Result + glue + pieces[I];
  Delete(Result, 1, Length(glue));
end;

class function ThlUtils.InBlöckeAufspalten(s: string;
  blockgröße: integer): string;
var
  I: integer;
begin
  if blockgröße <= 0 then
  begin
    raise Exception.Create(StrUngültigeBlockgröße);
  end;
  Result := '';
  I := 1;
  s := Trim(s);
  while I < Length(s) do
  begin
    Result := Result + copy(s, I, blockgröße) + ' ';
    inc(I, blockgröße);
  end;
  Result := Trim(Result);
end;

class function ThlUtils.KillTask(exeFileName: string): integer;
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

    while integer(ContinueLoop) <> 0 do
    begin
      if ((UpperCase(ExtractFileName(FProcessEntry32.szExeFile))
        = UpperCase(exeFileName)) or (UpperCase(FProcessEntry32.szExeFile)
        = UpperCase(exeFileName))) then
        Result := integer(TerminateProcess(OpenProcess(PROCESS_TERMINATE,
          BOOL(0), FProcessEntry32.th32ProcessID), 0));
      ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
    end;
  finally
    CloseHandle(FSnapshotHandle);
  end;
end;

// Returns Windows Error Code (i.e. 0=success), NOT the ShellExecute() code (>32 = success)
class function ThlUtils.ShellExecuteWait(aWnd: HWND; Operation: string;
  ExeName: string; Params: string; WorkingDirectory: string; ncmdShow: integer;
  wait: boolean): integer;

  function _ShellExecuteWait(aWnd: HWND; Operation, filename, Parameters,
    Directory: string; ShowCmd: integer; wait: boolean): integer;
  var
    Info: TShellExecuteInfo;
    pInfo: PShellExecuteInfo;
    exitCode: DWORD; // Achtung: Muss DWORD sein (Ticket 38498)
    wdir: PChar;
  begin
    pInfo := @Info;
    ZeroMemory(pInfo, SizeOf(Info));
    if Directory = '' then
      wdir := nil
    else
      wdir := PChar(Directory);
    with Info do
    begin
      cbSize := SizeOf(Info);
      fMask := SEE_MASK_NOCLOSEPROCESS;
      Wnd := aWnd;
      lpVerb := PChar(Operation);
      lpFile := PChar(filename);
      lpParameters := PChar(Parameters + #0);
      lpDirectory := wdir;
      nShow := ShowCmd;
      hInstApp := 0;
    end;

    if not ShellExecuteEx(pInfo) then
    begin
      Result := -GetLastError;
      exit;
    end;

    try
      if not wait then
      begin
        Result := 0;
        exit;
      end;

      repeat
        exitCode := WaitForSingleObject(Info.hProcess, 100);
        Sleep(50);
        if Windows.GetCurrentThreadId = System.MainThreadID then
          Application.ProcessMessages;
        if Assigned(Application) and Application.Terminated then
          Abort;
      until (exitCode <> WAIT_TIMEOUT);

      if not GetExitCodeProcess(Info.hProcess, exitCode) then
      begin
        Result := -GetLastError;
        exit;
      end;

      Result := exitCode;
    finally
      if Info.hProcess <> 0 then
        CloseHandle(Info.hProcess);
    end;
  end;

  function _CreateProcess(Operation, filename, Parameters, Directory: string;
    ShowCmd: integer; wait: boolean): integer;
  var
    StartupInfo: TStartupInfo;
    ProcessInformation: TProcessInformation;
    Res: BOOL;
    lpExitCode: DWORD;
    ExeAndParams: string;
    wdir: PChar;
  begin
    FillChar(StartupInfo, SizeOf(TStartupInfo), 0);
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
    ExeAndParams := '"' + ExeName + '" ' + Params;
    if Directory = '' then
      wdir := nil
    else
      wdir := PChar(Directory);
    Res := CreateProcess(PChar(ExeName), PChar(ExeAndParams), nil, nil, True,
      CREATE_DEFAULT_ERROR_MODE or NORMAL_PRIORITY_CLASS, nil, wdir,
      StartupInfo, ProcessInformation);
    try
      if not Res then
      begin
        Result := -GetLastError;
        exit;
      end;
      if not wait then
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
        if Assigned(Application) and Application.Terminated then
          Abort;
      end;
      Result := integer(lpExitCode);
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
    Result := _ShellExecuteWait(aWnd, PChar(Operation), PChar(ExeName),
      PChar(Params), PChar(WorkingDirectory), ncmdShow, wait);
    exit;
  end;

  Result := _CreateProcess(Operation, ExeName, Params, WorkingDirectory,
    ncmdShow, wait);
  if (Result = -193) then
  // Fehler 193 = Keine zulässige Win32-Anwendung (z.B. "Hallo.txt")
  begin
    Result := _ShellExecuteWait(aWnd, PChar(Operation), PChar(ExeName),
      PChar(Params), PChar(WorkingDirectory), ncmdShow, wait);
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
    if FileExists(testExe) then
      ExeName := testExe; // Variante 1 (CORA_Verwaltung.exe)
    testExe := StringReplace(testExe, '.exe', '32.exe', [rfIgnoreCase]);
    if FileExists(testExe) then
      ExeName := testExe;
    // Veriante 2 (DBTool32.exe), derzeit nicht mehr im Einsatz (DBTool32.exe heißt nun wieder DBTool.exe)
  end
  else
  begin
    testExe := ExeName;
    testExe := StringReplace(testExe, '32.exe', '.exe', [rfIgnoreCase]);
    testExe := StringReplace(testExe, '.exe', '64.exe', [rfIgnoreCase]);
    if FileExists(testExe) then
      ExeName := testExe; // Variante 1+2
  end;
  Result := ExeName;
end;

class function ThlUtils.ShellExecuteWait64(aWnd: HWND;
  Operation, ExeName, Params, WorkingDirectory: string; ncmdShow: integer;
  wait: boolean): integer;
begin
  ExeName := Find3264ExeCandidate(ExeName);
  ChangeFSRedirection(True);
  try
    // Warum FS Redirection deaktivieren? (Nur bei 32 Bit EXE auf Win64 OS)
    // Mit der WoW64-Weiterleitung würde ein 32-Bit Prozess bevorzugt
    // 32-Bit Anwendungen öffnen. Das heißt z.B. kein Snipping-Tool!
    Result := ShellExecuteWait(aWnd, Operation, ExeName, Params,
      WorkingDirectory, ncmdShow, wait);
  finally
    ChangeFSRedirection(False);
  end;
end;

class function ThlUtils.PathsEqual(const pathA, pathB: string): boolean;

  function __CanonizePath(const path: string): string;
  begin
    Result := UpperCase(IncludeTrailingPathDelimiter(path));
  end;

begin
  Result := __CanonizePath(pathA) = __CanonizePath(pathB);
  // TODO: Zukünftig auch UNC unterstützen
end;

class procedure ThlUtils.ProcessMessages(PleaseCancel: PBoolean);
begin
  if Windows.GetCurrentThreadId <> System.MainThreadID then
    exit;

  Application.ProcessMessages;
  if Assigned(Application) then
  begin
    if Application.Terminated then
      Abort;
  end;
  if Assigned(PleaseCancel) then
  begin
    if PleaseCancel^ then
      Abort;
  end;
end;

class function ThlUtils.DaysAge(const filename: string): integer;
begin
  Result := Trunc(Now - FileDateToDateTime(FileAge(filename)));
end;

class procedure ThlUtils.DeleteDirectory(const DirName: string);
var
  FileOp: TSHFileOpStruct;
begin
  // http://stackoverflow.com/questions/11798783/delete-all-files-and-folders-recursively-using-delphi
  FillChar(FileOp, SizeOf(FileOp), 0);
  FileOp.wFunc := FO_DELETE;
  FileOp.pFrom := PChar(DirName + #0); // double zero-terminated
  FileOp.fFlags := FOF_SILENT or FOF_NOERRORUI or FOF_NOCONFIRMATION;
  SHFileOperation(FileOp);
end;

class function ThlUtils.DirectoryEmpty(const dir: string): boolean;
var
  SR: TSearchRec;
  I: integer;
begin
  // http://www.swissdelphicenter.ch/de/showcode.php?id=2413
  Result := False;
  FindFirst(IncludeTrailingPathDelimiter(dir) + '*', faAnyFile, SR);
  for I := 1 to 2 do
    if (SR.Name = '.') or (SR.Name = '..') then
      Result := FindNext(SR) <> 0;
  FindClose(SR);
end;

// Quelle: https://www.viathinksoft.de/?page=codelib&showid=72
class procedure ThlUtils.ListFiles(Directory: string; list: TStrings;
  recursive: boolean);
var
  SR: TSearchRec;
begin
  Directory := IncludeTrailingPathDelimiter(Directory);

  if Application.Terminated then
    exit;
  if Windows.GetCurrentThreadId = System.MainThreadID then
    Application.ProcessMessages;

  if FindFirst(Directory + '*', faAnyFile, SR) = 0 then;
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

class procedure ThlUtils.ChecksummenErzeugen_SFV(const DirName: string;
  SFVFileName: string = 'Checksums.sfv');
var
  slFiles: TStrings;
  sChkFile, sOutFile, sHash: string;
  fOut: TextFile;
  sComment: string;
  dirLen: integer;
begin
  sOutFile := IncludeTrailingPathDelimiter(DirName) + SFVFileName;
  if FileExists(sOutFile) then
  begin
    if not DeleteFile(sOutFile) then
    begin
      raise Exception.CreateFmt(StrKonnteDateiSNich, [sOutFile]);
    end;
  end;
  AssignFile(fOut, sOutFile);
  slFiles := TStringList.Create;
  try
    Rewrite(fOut);
    sComment := '; ' + StrPrüfsummenDatei;
    if Assigned(Application) then
    begin
      sComment := sComment + ', ' + Format(StrErstelltMitS,
        [Application.Title]);
    end;
    Writeln(fOut, sChkFile, sComment);
    ThlUtils.ListFiles(DirName, slFiles, True);
    for sChkFile in slFiles do
    begin
      if sChkFile = sOutFile then
        Continue;
      sHash := IntToHex(CalculateCRC32File(sChkFile), 8);
      dirLen := Length(IncludeTrailingPathDelimiter(DirName));
      Writeln(fOut, copy(sChkFile, dirLen + 1, Length(sChkFile) - dirLen) +
        ' ' + sHash);
    end;
  finally
    CloseFile(fOut);
    FreeAndNil(slFiles);
  end;
end;

class procedure ThlUtils.ChecksummenErzeugen_MD5(const DirName: string;
  SFVFileName: string = 'Checksums.md5');
var
  slFiles: TStrings;
  sChkFile, sOutFile, sHash: string;
  fOut: TextFile;
  sComment: string;
  dirLen: integer;
begin
  sOutFile := IncludeTrailingPathDelimiter(DirName) + SFVFileName;
  if FileExists(sOutFile) then
  begin
    if not DeleteFile(sOutFile) then
    begin
      raise Exception.CreateFmt(StrKonnteDateiSNich, [sOutFile]);
    end;
  end;
  AssignFile(fOut, sOutFile);
  slFiles := TStringList.Create;
  try
    Rewrite(fOut);
    sComment := '; ' + StrPrüfsummenDatei;
    if Assigned(Application) then
    begin
      sComment := sComment + ', ' + Format(StrErstelltMitS,
        [Application.Title]);
    end;
    Writeln(fOut, sChkFile, sComment);
    ThlUtils.ListFiles(DirName, slFiles, True);
    for sChkFile in slFiles do
    begin
      if sChkFile = sOutFile then
        Continue;

      // Hier gibt es ein Problem: Wenn QuickSFV eine SFV-Datei öffnet, verändert er
      // diese Datei (Cache?). Somit wäre eine MD5 Datei, die die SFV-Datei mit-prüft
      // ungültig.
      if LowerCase(ExtractFileExt(sChkFile)) = '.sfv' then
        Continue;

      sHash := LowerCase(MD5File(sChkFile));
      dirLen := Length(IncludeTrailingPathDelimiter(DirName));
      Writeln(fOut, sHash + ' *' + copy(sChkFile, dirLen + 1,
        Length(sChkFile) - dirLen));
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
  FileStream := TFileStream.Create(filename, fmOpenRead or fmShareDenyNone);
  try
    try
      Result := FileStream.Size;
    except
      on E: EAbort do
      begin
        Result := 0;
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
  Result := ExtractFilePath(ExcludeTrailingPathDelimiter(dir));
end;

class function ThlUtils.ValidWinFileName(filename: String;
  islong: boolean = True): boolean;
const
  { for short 8.3 file names }
  ShortForbiddenChars: set of char = [';', '=', '+', '<', '>', '|', '"', '[',
    ']', '\', '/', ''''];
  { for long file names }
  LongForbiddenChars: set of char = ['<', '>', '|', '"', '\', '/', ':',
    '*', '?'];
var
  I: integer;
begin
  Result := filename <> '';
  if islong then
  begin
    for I := 1 to Length(filename) do
      Result := Result and not(filename[I] in LongForbiddenChars);
  end
  else
  begin
    for I := 1 to Length(filename) do
      Result := Result and not(filename[I] in ShortForbiddenChars);
  end;
end;

class function ThlUtils.nullenRechtsEntfernen(s: string): string;
begin
  Result := s;
  while (Result <> '') and (Result[Length(Result)] = '0') do
  begin
    Result := copy(Result, 1, Length(Result) - 1);
  end;
  if (RightStr(Result, 1) = '.') or (RightStr(Result, 1) = ',') then
  begin
    Result := copy(Result, 1, Length(Result) - 1);
  end;
end;

class function ThlUtils.Cut(s: string; delim: char; piece: integer): string;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Delimiter := delim;
    SL.StrictDelimiter := True;
    SL.DelimitedText := s;
    if piece >= SL.Count then
      Result := ''
    else
      Result := SL.Strings[piece];
  finally
    FreeAndNil(SL);
  end;
end;

class function ThlUtils.AmiFloat(d: double): string;
begin
  Result := FloatToStr(d); // TODO: Rundung notwendig?
  Result := StringReplace(Result, '.', '', [rfReplaceAll]);
  Result := StringReplace(Result, ',', '.', []);
end;

class procedure ThlUtils.TryFreeDiskSpace(level: integer = 0);
var
  tmp: string;
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
    tmp := SysUtils.GetEnvironmentVariable('APPDATA');
    if tmp <> '' then
    begin
      tmp := IncludeTrailingPathDelimiter(tmp) + '..\Local\Temp\*.*';
      tmp := StringReplace(tmp, 'Roaming\..\', '', [rfIgnoreCase]);
      DeleteFiles(tmp);
    end;
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
    pFrom := PChar(AFile + #0);
    fFlags := FOF_SILENT or FOF_NOCONFIRMATION;
  end;
  Result := SHFileOperation(sh) = 0;
end;

// TODO: Prozedurale Methoden in ThlUtils einbinden oder in hl.Utils.*.pas splitten

{$REGION 'Noch prozedurale Utils'}

function GetNextPossibleForm(ctrl: TComponent): TForm;
begin
  Result := nil;

  while not(ctrl is TForm) do
  begin
    if TComponent(ctrl).Owner = nil then
      exit;
    ctrl := TComponent(ctrl).Owner;
  end;

  if ctrl = nil then
    exit;

  Result := ctrl as TForm;
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

function Split(aLine, aSplitString: string): TStringList;
var
  l: TStringList;
  s, s1: string;
  p: integer;

begin
  s := aLine;
  l := TStringList.Create;
  l.Clear;

  while Length(s) > 0 do
  begin
    p := pos(aSplitString, s);
    if p > 0 then
    begin
      s1 := copy(s, 1, p - 1);
      s := copy(s, p + Length(aSplitString), 10000);
      l.Add(s1);
    end
    else
    begin
      l.Add(s);
      s := '';
    end;
  end;
  Result := l;
end;

function CheckEmpty(aValue: string): string;
var
  s: string;
begin
  s := aValue;
  if s = '' then
    s := '0';
  Result := s;
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
    Result := StrPas(pcResult);
  finally
    FreeMem(pcResult);
    FreeMem(pcBuffer);
  end;
end;

function FloatToStrForCRW(aValue: extended; NKStellen: integer): string;
begin
  Result := Format('%.' + intToStr(NKStellen) + 'f', [aValue]);
  if pos(',', Result) > 0 then
    Result[pos(',', Result)] := '.';
  if SameText(Result, 'NAN') then
  begin
    Result := 'NULL' // Ticket 55743
  end;
end;

function FloatToStrForCRW(aValue: extended): string;
begin
  Result := FloatToStr(aValue);
  if pos(',', Result) > 0 then
    Result[pos(',', Result)] := '.';
  if SameText(Result, 'NAN') then
  begin
    Result := 'NULL' // Ticket 55743
  end;
end;

function FloatToStrForSQL(aValue: extended; NKStellen: integer): string;
begin
  Result := Format('%.' + intToStr(NKStellen) + 'f', [aValue]);
  if pos(',', Result) > 0 then
    Result[pos(',', Result)] := '.';
  if SameText(Result, 'NAN') then
  begin
    Result := 'NULL' // Ticket 55743
  end
  else
  begin
    // Das ist wichtig, denn bei folgendem Ausdruck wird der SQL Server zur Diva:
    // "SELECT ROUND(0.996, 2)"  => Arithmetischer Überlauf... (Fein 60998)
    Result := 'cast(' + Result + ' as float)';
  end;
end;

function FloatToStrForSQL(aValue: extended): string;
begin
  Result := FloatToStr(aValue);
  if pos(',', Result) > 0 then
    Result[pos(',', Result)] := '.';
  if SameText(Result, 'NAN') then
  begin
    Result := 'NULL' // Ticket 55743
  end
  else
  begin
    // Das ist wichtig, denn bei folgendem Ausdruck wird der SQL Server zur Diva:
    // "SELECT ROUND(0.996, 2)"  => Arithmetischer Überlauf... (Fein 60998)
    Result := 'cast(' + Result + ' as float)';
  end;
end;

function BoolToStrForSQL(aValue: boolean): string;
begin
  if (aValue) then
    Result := '1'
  else
    Result := '0';
end;

function FRound(Value: extended; iAnzahl: integer): extended;
var
  iCounter: Byte;
  sMultiplier: string[15];
begin
  sMultiplier := '1';
  for iCounter := 1 to iAnzahl do
    sMultiplier := sMultiplier + '0';
  Value := Value * StrToFloat(sMultiplier);
  if (copy(FloatToStr(Frac(Value)), 1, 3) = '0,5') or
    (copy(FloatToStr(Frac(Value)), 1, 4) = '0,49') then
    Value := Value + 0.1;
  Result := Trunc(Value) + Trunc(Frac(Value) * 2);
  Result := Result / StrToFloat(sMultiplier);
end;

function Runden05(aValue: double): double;
begin
  Result := Ceil(aValue * 20 - 0.000001) / 20; // Ticket 49669
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

function Runden10(aValue: double): double;
begin
  Result := Ceil(aValue * 10 - 0.000001) / 10; // Ticket 49669
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
  I: integer;
  r: string;
  c: char;

begin
  r := '';
  for I := 1 to Length(x) do
  begin
    c := x[I];
    if c in ['/', ' ', '-', '%'] then
      r := r + '_'
    else if c in ['ä', 'Ä'] then
      r := r + 'AE'
    else if c in ['ö', 'Ö'] then
      r := r + 'OE'
    else if c in ['ü', 'Ü'] then
      r := r + 'UE'
    else if c = 'ß' then
      r := r + 'SS'
    else
      r := r + upcase(c);
  end;
  Result := r;
end;

function hclBoolToStr(aValue: boolean): string;
begin
  if aValue = True then
    Result := StrJA
  else
    Result := StrNEIN;
end;

class function ThlUtils.GetFileVersion(filename: string): string;
var
  dwCoraVersionInfoSize: DWORD;
  dwBuffer: DWORD;
  iBuffer: UINT;
  pVersionInfoBuffer: PChar;
  pVersionBuffer: Pointer;

begin
  dwCoraVersionInfoSize := GetFileVersionInfoSize(PChar(ParamStr(0)),
    dwBuffer) + 1;
  pVersionInfoBuffer := AllocMem(dwCoraVersionInfoSize);
  try
    GetFileVersionInfo(PChar(filename), 0, dwCoraVersionInfoSize,
      pVersionInfoBuffer);
    iBuffer := 255;

    // 407 = Deutsch
    if VerQueryValue(pVersionInfoBuffer,
      '\\StringFileInfo\\040704E4\\FileVersion', pVersionBuffer, iBuffer) then
    begin
      Result := StrPas(PChar(pVersionBuffer));
      exit;
    end;
    if VerQueryValue(pVersionInfoBuffer,
      '\\StringFileInfo\\040704B0\\FileVersion', pVersionBuffer, iBuffer) then
    begin
      Result := StrPas(PChar(pVersionBuffer));
      exit;
    end;

    // 409 = Englisch
    // TODO: Bei Delphi 11 bleibt die Spracheinstellung in den Versioninfos nicht bei Deutsch, sondern geht immer wieder zurück auf Englisch
    if VerQueryValue(pVersionInfoBuffer,
      '\\StringFileInfo\\040904E4\\FileVersion', pVersionBuffer, iBuffer) then
    begin
      Result := StrPas(PChar(pVersionBuffer));
      exit;
    end;
    if VerQueryValue(pVersionInfoBuffer,
      '\\StringFileInfo\\040904B0\\FileVersion', pVersionBuffer, iBuffer) then
    begin
      Result := StrPas(PChar(pVersionBuffer));
      exit;
    end;

    Result := '???'
  finally
    FreeMem(pVersionInfoBuffer);
  end;
end;

class function ThlUtils.GetComputerName: string;
var
  ComputerName: String;
  nsize: DWORD;
begin
  nsize := 25;
  SetLength(ComputerName, nsize);
  if Windows.GetComputerName(PChar(ComputerName), nsize) then
  begin
    SetLength(ComputerName, nsize);
    Result := ComputerName;
  end
  else
    Result := '';
end;

procedure LoadStringListFromResource(const ResName: string; SL: TStrings;
  ResType: PChar = RT_RCDATA; otherHInstance: HMODULE = 0);
var
  RS: TResourceStream;
begin
  if otherHInstance = 0 then
    otherHInstance := HInstance;
  RS := TResourceStream.Create(otherHInstance, ResName, ResType);
  try
    SL.LoadFromStream(RS);
  finally
    FreeAndNil(RS);
  end;
end;

function GetTempDir: string;
var
  dir: string;
  Len: DWORD;
begin
  SetLength(dir, MAX_PATH);
  Len := GetTempPath(MAX_PATH, PChar(dir));
  if Len > 0 then
  begin
    SetLength(dir, Len);
    Result := dir;
  end
  else
    RaiseLastOSError;
  Result := IncludeTrailingPathDelimiter(Result);
end;

procedure CropFrame(frame: TFrame; onlyVisible: boolean;
  rightPadding, bottomPadding: integer);
var
  I: integer;
  c: TControl;
  maxRight: integer;
  maxBottom: integer;
begin
  maxRight := 0;
  maxBottom := 0;
  for I := 0 to frame.ControlCount - 1 do
  begin
    c := frame.Controls[I];
    if onlyVisible then
    begin
      if not c.Visible then
        Continue;
      if (c is TLabel) and (TLabel(c).Caption = '') then
        Continue;
    end;
    maxRight := Max(maxRight, c.Left + c.Width);
    maxBottom := Max(maxBottom, c.Top + c.Height);
  end;
  inc(maxRight, rightPadding);
  inc(maxBottom, bottomPadding);
  frame.ClientWidth := maxRight;
  frame.ClientHeight := maxBottom;
end;

Function CompressAnsiString(const Input: ansistring): ansistring;
{$IF CompilerVersion > 20.0} // Ich weiß nicht genau, ab welcher Version CompressBuf durch ZCompress ersetzt wurde... Ich habe daher irgendeine Version eingetragen
var
  InBytes: TBytes;
  OutBytes: TBytes;
begin
  SetLength(InBytes, Length(Input));
  Move(Input[1], InBytes[0], Length(Input));

  ZCompress(InBytes, OutBytes);

  SetLength(Result, Length(OutBytes));
  Move(OutBytes[0], Result[1], Length(OutBytes));
{$ELSE}
var
  Buffer: Pointer;
  BufSize: integer;
begin
  Buffer := nil;
  try
    CompressBuf(@Input[1], Length(Input), Buffer, BufSize);
    SetLength(Result, BufSize);
    Move(Buffer^, Result[1], BufSize);
  finally
    If Buffer <> nil Then
      FreeMem(Buffer);
  end;
{$IFEND}
  if DeCompressAnsiString(Result) <> Input then
    raise Exception.Create(StrKomprimierungStimmt);
end;

Function DeCompressAnsiString(const Input: ansistring): ansistring;
{$IF CompilerVersion > 20.0} // Ich weiß nicht genau, ab welcher Version DeCompressBuf durch ZDecompress ersetzt wurde... Ich habe daher irgendeine Version eingetragen
var
  InBytes: TBytes;
  OutBytes: TBytes;
begin
  SetLength(InBytes, Length(Input));
  Move(Input[1], InBytes[0], Length(Input));

  ZDecompress(InBytes, OutBytes);

  SetLength(Result, Length(OutBytes));
  Move(OutBytes[0], Result[1], Length(OutBytes));
{$ELSE}
var
  Buffer: Pointer;
  BufSize: integer;
begin
  Buffer := nil;
  try
    DeCompressBuf(@Input[1], Length(Input), 0, Buffer, BufSize);
    SetLength(Result, BufSize);
    Move(Buffer^, Result[1], BufSize);
  finally
    If Buffer <> nil then
      FreeMem(Buffer);
  end;
{$IFEND}
end;

function LoadFileToStr(const filename: TFileName): ansistring;
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(filename, fmOpenRead or fmShareDenyWrite);
  try
    if FileStream.Size > 0 then
    begin
      SetLength(Result, FileStream.Size);
      FileStream.Read(Pointer(Result)^, FileStream.Size);
    end;
  finally
    FreeAndNil(FileStream);
  end;
end;

procedure SaveStrToFile(const filename, SourceString: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(filename, fmCreate);
  try
    Stream.WriteBuffer(Pointer(SourceString)^, Length(SourceString));
  finally
    FreeAndNil(Stream);
  end;
end;

function StringXorCrypt(const text, key: ansistring): ansistring;
var
  I, j, keylen: integer;
begin
  SetLength(Result, Length(text));
  j := 1;
  keylen := Length(key);
  for I := 1 to Length(text) do
  begin
    Result[I] := AnsiChar(Ord(text[I]) xor Ord(key[j]));
    inc(j);
    if j > keylen then
      j := 1;
  end;
end;

procedure Split(Delimiter: char; Str: string; ListOfStrings: TStrings);
begin
  ListOfStrings.Clear;
  ListOfStrings.Delimiter := Delimiter;
  ListOfStrings.StrictDelimiter := True; // Requires D2006 or newer.
  ListOfStrings.DelimitedText := Str;
end;

function EnterToTabAllowed(f: TForm): boolean;
begin
  // Tastenfunktionen außerhalb des Grids zum Weiterschalten ( Tab-Ersatz )
  Result := (f.ActiveControl <> nil) and
    (f.ActiveControl.classname <> 'TwwDBGrid') and
    (f.ActiveControl.classname <> 'TMemo') and
    (f.ActiveControl.classname <> 'TwwDBRichEdit') and
    ((f.ActiveControl.parent = nil) or (f.ActiveControl.parent.classname <>
    'TwwDBGrid')) and not(f.ActiveControl is TDBMemo);
end;

function RandomString(strlength: integer): string;
var
  temp: integer;
begin
  // https://www.delphi-treff.de/tipps-tricks/object-pascal/strings/zufallstring-generieren/
  randomize;
  repeat
    temp := random(122); // ggf. erhöhen
    if temp in [48 .. 57 { 0-1 } , 65 .. 90 { A-Z } , 97 .. 122 { a-z } ] then
      // Kann um beliebige ASCII-Zeichen erweitert werden,
      // ggf. den Wert in Random hochsetzen
      Result := Result + Chr(temp);
  until Length(Result) = strlength;
end;

{$ENDREGION}

class function ThlUtils.GetInside(s, delimA, delimB: string): string;
var
  pa, pb: integer;
begin
  pa := pos(delimA, s);
  if pa = 0 then
    pa := 1;
  inc(pa, Length(delimA));
  s := copy(s, pa, Length(s) - pa + 1);

  pb := pos(delimB, s);
  if pb = 0 then
    pb := Length(s);
  s := copy(s, 1, pb - 1);

  Result := s;
end;

class function ThlUtils.FileGetContents(const filename: string): string;
{$IF CompilerVersion < 20.0} // Version geraten
var
  FileStream: TFileStream;
{$IFEND}
begin
{$IF CompilerVersion >= 20.0} // Version geraten
  Result := TFile.ReadAllText(filename);
{$ELSE}
  FileStream := TFileStream.Create(filename, fmOpenRead or fmShareDenyWrite);
  try
    if FileStream.Size > 0 then
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
    Result := True;
  except
    on E: EAbort do
    begin
      Result := False;
      Abort;
    end;
    on E: Exception do
    begin
      // Application.MessageBox(PChar(e.Message), '', 0);
      Result := False;
    end;
  end;
end;

class function ThlUtils.EinigeDateienNichtLesbar(filemask: string): boolean;
var
  SR: TSearchRec;
begin
  Result := False;
  if FindFirst(filemask, faAnyFile, SR) = 0 then
  begin
    repeat
      if not ThlUtils.FileIsReadable(ExtractFilePath(filemask) + '\' + SR.Name)
      then
      begin
        Result := True;
      end;
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;
end;

class function ThlUtils.GetWindowsUserName: String;
var
  nsize: DWORD;
begin
  nsize := 1024;
  SetLength(Result, nsize);
  if GetUserName(PChar(Result), nsize) then
    SetLength(Result, nsize - 1)
  else
    RaiseLastOSError;
end;

{$IFDEF UNICODE}
function PathRelativePathTo(pszPath: PChar; pszFrom: PChar; dwAttrFrom: DWORD;
  pszTo: PChar; dwAtrTo: DWORD): LongBool; stdcall;
  external 'shlwapi.dll' name 'PathRelativePathToW';
{$ELSE}
function PathRelativePathTo(pszPath: PChar; pszFrom: PChar; dwAttrFrom: DWORD;
  pszTo: PChar; dwAtrTo: DWORD): LongBool; stdcall;
  external 'shlwapi.dll' name 'PathRelativePathToA';
{$ENDIF}

class function ThlUtils.AbsToRel(const AbsPath, BasePath: string): string;
var
  path: array [0 .. MAX_PATH - 1] of char;
begin
  PathRelativePathTo(@path[0], PChar(BasePath), FILE_ATTRIBUTE_DIRECTORY,
    PChar(AbsPath), 0);
  Result := path;
end;

{$IFDEF UNICODE}
function PathCanonicalize(lpszDst: PChar; lpszSrc: PChar): LongBool; stdcall;
  external 'shlwapi.dll' name 'PathCanonicalizeW';
{$ELSE}
function PathCanonicalize(lpszDst: PChar; lpszSrc: PChar): LongBool; stdcall;
  external 'shlwapi.dll' name 'PathCanonicalizeA';
{$ENDIF}

class function ThlUtils.RelToAbs(const RelPath, BasePath: string): string;
var
  Dst: array [0 .. MAX_PATH - 1] of char;
begin
  if copy(RelPath, 1, 2) = '\\' then
    Result := RelPath
  else if copy(RelPath, 2, 2) = ':\' then
    Result := RelPath
  else
  begin
    PathCanonicalize(@Dst[0], PChar(IncludeTrailingPathDelimiter(BasePath) +
      RelPath));
    Result := Dst;
  end;
end;

class function ThlUtils.GetModificationTimeOfFile(const AFileName: String)
  : TDateTime;
var
  SR: TSearchRec;
  SystemTime: TSystemTime;
  NewModificationTime: TFileTime;
begin
  Result := 0;
  if FindFirst(AFileName, faAnyFile, SR) = 0 then
    try
      IF (Windows.FileTimeToLocalFiletime(SR.FindData.ftLastWriteTime,
        NewModificationTime) and Windows.FileTimeToSystemTime
        (NewModificationTime, SystemTime)) Then
        Result := Encodedate(SystemTime.wYear, SystemTime.wMonth,
          SystemTime.wDay) + Encodetime(SystemTime.wHour, SystemTime.wMinute,
          SystemTime.wSecond, SystemTime.wMilliseconds);
    finally
      SysUtils.FindClose(SR)
    end;
end;

procedure SplitChar(Delimiter: char; Str: string; ListOfStrings: TStrings);
// TODO: Redundant? (Split-Funktion)
begin
  ListOfStrings.Clear;
  ListOfStrings.Delimiter := Delimiter;
  ListOfStrings.StrictDelimiter := True; // Requires D2006 or newer.
  ListOfStrings.DelimitedText := Str;
end;

procedure SplitText(Delimiter: string; Str: string; ListOfStrings: TStrings);
// TODO: Redundant? (Split-Funktion)
begin
  SplitChar(#1, StringReplace(Str, Delimiter, #1, [rfIgnoreCase, rfReplaceAll]),
    ListOfStrings);
end;

function IsDirectoryWriteable(const AName: string): boolean;
var
  filename: String;
  H: HFILE;
begin
  // https://stackoverflow.com/questions/3599256/how-can-i-use-delphi-to-test-if-a-directory-is-writeable
  filename := IncludeTrailingPathDelimiter(AName) + 'chk.tmp';
  H := CreateFile(PChar(filename), GENERIC_READ or GENERIC_WRITE, 0, nil,
    CREATE_NEW, FILE_ATTRIBUTE_TEMPORARY or FILE_FLAG_DELETE_ON_CLOSE, 0);
  Result := H <> INVALID_HANDLE_VALUE;
  if Result then
    CloseHandle(H);
end;

class procedure ThlUtils.RequeryAndGotoSameSpot(ds: TAdoQuery);
var
  cdis: boolean;
  I: integer;
begin
  cdis := ds.ControlsDisabled;
  ds.DisableControls;
  try
    ds.Prior;

    I := 0;
    while not ds.Bof do
    begin
      ds.Prior;
      inc(I);
    end;

    ds.Requery;

    while I > 0 do
    begin
      Dec(I);
      ds.Next;
    end;
  finally
    if not cdis then
      ds.EnableControls
  end;
end;

const
  NERR_Success = 0;

function NetWkstaGetInfo(ServerName: LPWSTR; level: DWORD; BufPtr: Pointer)
  : Longint; stdcall; external 'netapi32.dll' Name 'NetWkstaGetInfo';

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

function GetNetParam(AParam: integer): string;
var
  PBuf: LPWKSTA_INFO_100;
  Res: Longint;
begin
  Result := '';
  Res := NetWkstaGetInfo(nil, 100, @PBuf);
  if Res = NERR_Success then
  begin
    case AParam of
      0:
        Result := string(PBuf^.wki100_computername);
      1:
        Result := string(PBuf^.wki100_langroup);
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
  processEntry: TProcessEntry32;
Begin
  Result := 0;
  appname := UpperCase(appname);
  snapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  If snapshot <> 0 Then
    try
      processEntry.dwSize := SizeOf(processEntry);
      if Process32First(snapshot, processEntry) Then
        repeat
          if pos(appname,
            UpperCase(ExtractFileName(StrPas(processEntry.szExeFile)))) > 0 then
          begin
            Result := processEntry.th32ProcessID;
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
    classname: String;
    WindowHandle: THandle;
  end;

function EnumWindowsProc(hWindow: HWND; lParam: Longint): boolean; stdcall;
var
  lpBuffer: PChar;
  WindowCaptionFound: boolean;
  ClassNameFound: boolean;
begin
  GetMem(lpBuffer, 255);
  Result := True;
  WindowCaptionFound := False;
  ClassNameFound := False;
  try
    if GetWindowText(hWindow, lpBuffer, 255) > 0 then
      if pos(PFindWindowStruct(lParam).Caption, StrPas(lpBuffer)) > 0 then
        WindowCaptionFound := True;
    if PFindWindowStruct(lParam).classname = '' then
      ClassNameFound := True
    else if GetClassName(hWindow, lpBuffer, 255) > 0 then
      if pos(PFindWindowStruct(lParam).classname, StrPas(lpBuffer)) > 0 then
        ClassNameFound := True;
    if (WindowCaptionFound and ClassNameFound) then
    begin
      PFindWindowStruct(lParam).WindowHandle := hWindow;
      Result := False;
    end;
  finally
    FreeMem(lpBuffer, SizeOf(lpBuffer^));
  end;
end;

function FindAWindow(WinCaption: string; WinClassName: string): THandle;
var
  WindowInfo: TFindWindowStruct;
begin
  with WindowInfo do
  begin
    Caption := WinCaption;
    classname := WinClassName;
    WindowHandle := 0;
    EnumWindows(@EnumWindowsProc, Longint(@WindowInfo));
    Result := WindowHandle;
  end;
end;

function ZahlEingetippt(key: char): boolean;
begin
  Result := (Ord(key) >= 48) and (Ord(key) <= 57);
end;

function NichtZahlEingetippt(key: char): boolean;
begin
  Result := ((Ord(key) >= 32) and (Ord(key) <= 47)) or (Ord(key) >= 58);
  // TODO <= 122?
end;

{$REGION 'Prozess-Kill-Funktionen'}

Function EnumThreadProc(Wnd: HWND; var appHwnd: HWND): LongBool; stdcall;
Var
  buf: array [0 .. 128] of char;
Begin
  if (GetClassName(Wnd, buf, SizeOf(buf)) > 0) and
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
  EnumThreadWindows(forThreadID, @EnumThreadProc, lParam(@Result));
End;

function KillProcess(PID: DWORD): boolean; overload;
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

function KillProcess(ExeName: string): boolean; overload;
var
  PID: DWORD;
begin
  PID := ProcessIDFromAppname32(ExeName);
  if PID <= 0 then
  begin
    Result := False;
    exit;
  end
  else
  begin
    Result := KillProcess(PID);
  end;
end;

{$ENDREGION}

function GetCharFromVirtualKey(key: Word): char;
var
  keyboardState: TKeyboardState;
  asciiResult: integer;
  s: string;
begin
  GetKeyboardState(keyboardState);

  SetLength(s, 2);
  asciiResult := ToAscii(key, MapVirtualKey(key, 0), keyboardState, @s[1], 0);
  case asciiResult of
    0:
      s := '';
    1:
      SetLength(s, 1);
    2:
      ;
  else
    s := '';
  end;

  if s = '' then
    Result := #0
  else
    Result := s[1];
end;

procedure GetUsbDrives(list: TStrings);
// http://ahmoremore.blogspot.com/2018/05/delphi-how-to-get-list-of-usb-removable.html

{$MINENUMSIZE 4}
const
  IOCTL_STORAGE_QUERY_PROPERTY = $002D1400;

type
  STORAGE_QUERY_TYPE = (PropertyStandardQuery = 0, PropertyExistsQuery,
    PropertyMaskQuery, PropertyQueryMaxDefined);
  TStorageQueryType = STORAGE_QUERY_TYPE;

  STORAGE_PROPERTY_ID = (StorageDeviceProperty = 0, StorageAdapterProperty);
  TStoragePropertyID = STORAGE_PROPERTY_ID;

  STORAGE_PROPERTY_QUERY = packed record
    PropertyId: STORAGE_PROPERTY_ID;
    QueryType: STORAGE_QUERY_TYPE;
    AdditionalParameters: array [0 .. 9] of AnsiChar;
  end;

  TStoragePropertyQuery = STORAGE_PROPERTY_QUERY;

  STORAGE_BUS_TYPE = (BusTypeUnknown = 0, BusTypeScsi, BusTypeAtapi, BusTypeAta,
    BusType1394, BusTypeSsa, BusTypeFibre, BusTypeUsb, BusTypeRAID,
    BusTypeiScsi, BusTypeSas, BusTypeSata, BusTypeMaxReserved = $7F);
  TStorageBusType = STORAGE_BUS_TYPE;

  STORAGE_DEVICE_DESCRIPTOR = packed record
    Version: DWORD;
    Size: DWORD;
    DeviceType: Byte;
    DeviceTypeModifier: Byte;
    RemovableMedia: boolean;
    CommandQueueing: boolean;
    VendorIdOffset: DWORD;
    ProductIdOffset: DWORD;
    ProductRevisionOffset: DWORD;
    SerialNumberOffset: DWORD;
    BusType: STORAGE_BUS_TYPE;
    RawPropertiesLength: DWORD;
    RawDeviceProperties: array [0 .. 0] of AnsiChar;
  end;

  TStorageDeviceDescriptor = STORAGE_DEVICE_DESCRIPTOR;

  function GetBusType(Drive: AnsiChar): TStorageBusType;
  var
    H: THandle;
    Query: TStoragePropertyQuery;
    dwBytesReturned: DWORD;
    Buffer: array [0 .. 1023] of Byte;
    sdd: TStorageDeviceDescriptor absolute Buffer;
    OldMode: UINT;
  begin
    Result := BusTypeUnknown;

    OldMode := SetErrorMode(SEM_FAILCRITICALERRORS);
    try
      H := CreateFile(PChar(Format('\\.\%s:', [AnsiLowerCase(Drive)])), 0,
        FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
      if H <> INVALID_HANDLE_VALUE then
      begin
        try
          dwBytesReturned := 0;
          FillChar(Query, SizeOf(Query), 0);
          FillChar(Buffer, SizeOf(Buffer), 0);
          sdd.Size := SizeOf(Buffer);
          Query.PropertyId := StorageDeviceProperty;
          Query.QueryType := PropertyStandardQuery;
          if DeviceIoControl(H, IOCTL_STORAGE_QUERY_PROPERTY, @Query,
            SizeOf(Query), @Buffer, SizeOf(Buffer), dwBytesReturned, nil) then
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
  DriveBits: set of 0 .. 25;
  I: integer;
  Drive: AnsiChar;
begin
  list.BeginUpdate;
  try
    Cardinal(DriveBits) := GetLogicalDrives;

    for I := 0 to 25 do
      if I in DriveBits then
      begin
        Drive := AnsiChar(Ord('a') + I);
        if GetBusType(Drive) = BusTypeUsb then
          list.Add(Drive);
      end;
  finally
    list.EndUpdate;
  end;
end;

procedure MoveFilesWildcards(Source, Target: string);
var
  FO: TSHFileOpStruct;
begin
  FillChar(FO, SizeOf(FO), #0);
  // FO.Wnd := Application.MainForm.Handle;
  FO.wFunc := FO_MOVE;
  FO.pFrom := PChar(Source + #0);
  FO.pTo := PChar(Target + #0);
  SHFileOperation(FO); // TODO: check result
end;

procedure DeleteFilesWildcards(Source: string; recyclebin: boolean);
var
  FO: TSHFileOpStruct;
begin
  FillChar(FO, SizeOf(FO), #0);
  // FO.Wnd := Application.MainForm.Handle;
  FO.wFunc := FO_DELETE;
  FO.pFrom := PChar(Source + #0);
  FO.fFlags := FOF_NOCONFIRMATION;
  if recyclebin then
    FO.fFlags := FO.fFlags or FOF_ALLOWUNDO;
  SHFileOperation(FO); // TODO: check result
end;

function IsWindows10: boolean;
var
  reg: TRegistry;
begin
  Result := False;

  if IsWow64 then
    reg := TRegistry.Create(KEY_READ OR KEY_WOW64_64KEY)
  else
    reg := TRegistry.Create(KEY_READ);
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    if reg.OpenKeyReadOnly('SOFTWARE\Microsoft\Windows NT\CurrentVersion') then
    begin
      try
        Result := reg.ReadInteger('CurrentMajorVersionNumber') = 10;
      except
        on E: EAbort do
        begin
          Abort;
        end;
        on E: Exception do
        begin
          // Windows Server 2012 Standard hat diesen Registry Key nicht. Es baut aber auf Win7 auf.
          Result := False;
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
  Result := False;

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
        // result := reg.ReadInteger('CurrentMajorVersionNumber') = 11;
        Result := (StrToInt(reg.ReadString('CurrentBuild')) >= 22000) and
          (StrToInt(reg.ReadString('CurrentBuildNumber')) >= 22000);
      except
        on E: EAbort do
        begin
          Abort;
        end;
        on E: Exception do
        begin
          Result := False;
        end;
      end;
      reg.CloseKey;
    end;
  finally
    FreeAndNil(reg);
  end;
end;

function Crw11_IstInstalliert: boolean;
var
  testFile, key: string;
  reg: TRegistry;
begin
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    if (WindowsBits = 64) and not IsWow64 then
    begin
      // CRW11 ist IMMER 64 Bit, deshalb brauchen wir in SOFTWARE\Business Objects\Suite 11.0\Crystal Reports gar nicht erst zu suchen
      key := 'SOFTWARE\WOW6432Node\Business Objects\Suite 11.0\Crystal Reports';
      // testFile := 'C:\Program Files (x86)\Common Files\Business Objects\3.0\bin\crpe32.dll';
    end
    else
    begin
      key := 'SOFTWARE\Business Objects\Suite 11.0\Crystal Reports';
      // testFile := 'C:\Program Files\Common Files\Business Objects\3.0\bin\crpe32.dll';
    end;
    if reg.OpenKeyReadOnly(key) then
    begin
      testFile := IncludeTrailingPathDelimiter(reg.ReadString('CommonFiles')) +
        'crpe32.dll';
      reg.CloseKey;
    end
    else
    begin
      Result := False;
      exit;
    end;
    if not FileExists(testFile) then
    begin
      Result := False;
      exit;
    end;
    if FileDateToDateTime(FileAge(testFile)) > Encodedate(2006, 1, 1) then
    begin
      Result := False;
      exit;
    end; // Runtime aus 2006 verursacht Probleme (Ticket 53942, 54766, 54770)
    Result := True;
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
    testFile :=
      'C:\Program Files (x86)\SAP BusinessObjects\Crystal Reports for .NET Framework 4.0\Common\SAP BusinessObjects Enterprise XI 4.0\win32_x86\crpe32.dll';
    if not FileExists(testFile) then
    begin
      Result := False;
      exit;
    end;
    if FileDateToDateTime(FileAge(testFile)) < Encodedate(2020, 1, 1) then
    begin
      // Runtime aus 2014 verursacht Probleme (Ticket 60253)
      Result := False;
      exit;
    end;
    // sic! Die 64 Bit crpe32.dll liegt wirklich in "Program Files (x86)" !
    testFile :=
      'C:\Program Files (x86)\SAP BusinessObjects\Crystal Reports for .NET Framework 4.0\Common\SAP BusinessObjects Enterprise XI 4.0\win64_x64\crpe32.dll';
    if not FileExists(testFile) then
    begin
      Result := False;
      exit;
    end;
    if FileDateToDateTime(FileAge(testFile)) < Encodedate(2020, 1, 1) then
    begin
      // Runtime aus 2014 verursacht Probleme (Ticket 60253)
      Result := False;
      exit;
    end;
    Result := True;
  end
  else
  begin
    testFile :=
      'C:\Program Files\SAP BusinessObjects\Crystal Reports for .NET Framework 4.0\Common\SAP BusinessObjects Enterprise XI 4.0\win32_x86\crpe32.dll';
    if not FileExists(testFile) then
    begin
      Result := False;
      exit;
    end;
    if FileDateToDateTime(FileAge(testFile)) < Encodedate(2020, 1, 1) then
    begin
      // Runtime aus 2014 verursacht Probleme (Ticket 60253)
      Result := False;
      exit;
    end;
    Result := True;
  end;
end;

function IsVCRuntime2022_32Bit_Installed: boolean;
var
  reg: TRegistry;
begin
  Result := False;
  reg := TRegistry.Create(KEY_READ);
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;

    // Prüfe den Standardpfad für 32-Bit Runtime
    if reg.OpenKeyReadOnly
      ('SOFTWARE\Microsoft\VisualStudio\14.0\VC\Runtimes\x86') then
    begin
      Result := reg.ValueExists('Installed') and
        (reg.ReadInteger('Installed') = 1);
      reg.CloseKey;
    end;

    // Prüfe den WOW6432Node-Pfad für 32-Bit Programme unter 64-Bit Windows
    if not Result and (WindowsBits = 64) and not IsWow64 and
      reg.OpenKeyReadOnly
      ('SOFTWARE\WOW6432Node\Microsoft\VisualStudio\14.0\VC\Runtimes\x86') then
    begin
      Result := reg.ValueExists('Installed') and
        (reg.ReadInteger('Installed') = 1);
      reg.CloseKey;
    end;
  finally
    FreeAndNil(reg);
  end;
end;

function IsVCRuntime2022_32Bit_Version: string;
var
  reg: TRegistry;
begin
  Result := '';
  reg := TRegistry.Create(KEY_READ);
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;

    // Prüfe den Standardpfad für 32-Bit Runtime
    if reg.OpenKeyReadOnly
      ('SOFTWARE\Microsoft\VisualStudio\14.0\VC\Runtimes\x86') then
    begin
      if reg.ValueExists('Version') then
        Result := reg.ReadString('Version');
      reg.CloseKey;
    end;

    // Prüfe den WOW6432Node-Pfad für 32-Bit Programme unter 64-Bit Windows
    if (Result = '') and (WindowsBits = 64) and not IsWow64 and
      reg.OpenKeyReadOnly
      ('SOFTWARE\WOW6432Node\Microsoft\VisualStudio\14.0\VC\Runtimes\x86') then
    begin
      if reg.ValueExists('Version') then
        Result := reg.ReadString('Version');
      reg.CloseKey;
    end;
  finally
    FreeAndNil(reg);
  end;
end;

function IsVCRuntime2022_64Bit_Installed: boolean;
var
  reg: TRegistry;
begin
  Result := False;
  reg := TRegistry.Create(KEY_READ);
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;

    // Prüfe den Standardpfad für 64-Bit Runtime
    if reg.OpenKeyReadOnly
      ('SOFTWARE\Microsoft\VisualStudio\14.0\VC\Runtimes\x64') then
    begin
      Result := reg.ValueExists('Installed') and
        (reg.ReadInteger('Installed') = 1);
      reg.CloseKey;
    end;

    // Prüfe den WOW6432Node-Pfad für 32-Bit Programme unter 64-Bit Windows
    // JA! Die Info, ob 64 Bit installiert ist, steht wirklich im Wow64 Node!
    // Computer\HKEY_LOCAL_MACHINE\SOFTWARE\WOW6432Node\Microsoft\VisualStudio\14.0\VC\Runtimes\X64
    if not Result and (WindowsBits = 64) and not IsWow64 and
      reg.OpenKeyReadOnly
      ('SOFTWARE\WOW6432Node\Microsoft\VisualStudio\14.0\VC\Runtimes\x64') then
    begin
      Result := reg.ValueExists('Installed') and
        (reg.ReadInteger('Installed') = 1);
      reg.CloseKey;
    end;
  finally
    FreeAndNil(reg);
  end;
end;

function IsVCRuntime2022_64Bit_Version: string;
var
  reg: TRegistry;
begin
  Result := '';
  reg := TRegistry.Create(KEY_READ);
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;

    // Prüfe den Standardpfad für 64-Bit Runtime
    if reg.OpenKeyReadOnly
      ('SOFTWARE\Microsoft\VisualStudio\14.0\VC\Runtimes\x64') then
    begin
      if reg.ValueExists('Version') then
        Result := reg.ReadString('Version');
      reg.CloseKey;
    end;

    // Prüfe den WOW6432Node-Pfad für 32-Bit Programme unter 64-Bit Windows
    // JA! Die Info, ob 64 Bit installiert ist, steht wirklich im Wow64 Node!
    // Computer\HKEY_LOCAL_MACHINE\SOFTWARE\WOW6432Node\Microsoft\VisualStudio\14.0\VC\Runtimes\X64
    if (Result = '') and (WindowsBits = 64) and not IsWow64 and
      reg.OpenKeyReadOnly
      ('SOFTWARE\WOW6432Node\Microsoft\VisualStudio\14.0\VC\Runtimes\X64') then
    begin
      if reg.ValueExists('Version') then
        Result := reg.ReadString('Version');
      reg.CloseKey;
    end;
  finally
    FreeAndNil(reg);
  end;
end;

function IsWow64: boolean;
{$IFDEF WIN64}
begin
  // Native 64 Bit App means OS and CPU is 64 Bit, too.
  Result := False;
{$ELSE}
type
  TIsWow64Process = function( // Type of IsWow64Process API fn
    Handle: Windows.THandle; var Res: Windows.BOOL): Windows.BOOL; stdcall;
var
  IsWow64Result: Windows.BOOL; // Result from IsWow64Process
  IsWow64Process: TIsWow64Process; // IsWow64Process fn reference
begin
  // Try to load required function from kernel32
  IsWow64Process := Windows.GetProcAddress(Windows.GetModuleHandle('kernel32'),
    'IsWow64Process');
  if Assigned(IsWow64Process) then
  begin
    // Function is implemented: call it
    if not IsWow64Process(Windows.GetCurrentProcess, IsWow64Result) then
      raise SysUtils.Exception.Create(StrIsWow64BadProcess);
    // Return result of function
    Result := IsWow64Result;
  end
  else
    // Function not implemented: can't be running on Wow64
    Result := False;
{$ENDIF}
end;

function ChangeFSRedirection(bDisable: boolean): boolean;
type
  TWow64DisableWow64FsRedirection = Function(Var Wow64FsEnableRedirection
    : LongBool): LongBool; StdCall;
  TWow64EnableWow64FsRedirection = Function(var Wow64FsEnableRedirection
    : LongBool): LongBool; StdCall;
var
  hHandle: THandle;
  Wow64DisableWow64FsRedirection: TWow64DisableWow64FsRedirection;
  Wow64EnableWow64FsRedirection: TWow64EnableWow64FsRedirection;
  Wow64FsEnableRedirection: LongBool;
begin

{$IFDEF Win64}
  Result := False;
  exit;
{$ENDIF}

  // https://www.delphipraxis.net/155861-windows-7-64bit-redirection.html

  Result := False;

  try
    hHandle := GetModuleHandle('kernel32.dll');
    @Wow64EnableWow64FsRedirection := GetProcAddress(hHandle,
      'Wow64EnableWow64FsRedirection');
    @Wow64DisableWow64FsRedirection := GetProcAddress(hHandle,
      'Wow64DisableWow64FsRedirection');

    if bDisable then
    begin
      if (hHandle <> 0) and (@Wow64DisableWow64FsRedirection <> nil) then
      begin
        Result := Wow64DisableWow64FsRedirection(Wow64FsEnableRedirection);
      end;
    end
    else
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
    on E: Exception do
    begin
      // ignore
    end;
  end;
end;

function FixedDrive(Drive: char): boolean;
begin
  Result := (Windows.GetDriveType(PChar(Drive + ':\')) = Windows.DRIVE_FIXED);
end;

function CopyFolder(const SrcFolder, DestFolder: String; iFileOp: integer;
  OverWrite: boolean; ShowDialog: boolean): boolean;
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
  Src, Dest: String;
  ResultVal: integer;
begin
  Result := False;

  Src := SrcFolder;
  Dest := DestFolder;

  if (Src = '') or ((iFileOp <> FO_DELETE) and (Dest = '')) or
    (CompareText(Src, Dest) = 0) then
    exit;

  if Src[Length(Src)] = '\' then
    SetLength(Src, Length(Src) - 1);
  Src := Src + #0#0;

  if (Dest <> '') and (Dest[Length(Dest)] = '\') then
    SetLength(Dest, Length(Dest) - 1);
  Dest := Dest + #0#0;

  // zero structure
  // ! Mandatory in XP
  FillChar(MyFOStruct, SizeOf(MyFOStruct), 0);

  // Fill in structure
  with MyFOStruct do
  begin
    Wnd := 0;

    // specify a copy operation
    wFunc := iFileOp;
    pFrom := @Src[1];
    pTo := @Dest[1];

    // set the flags
    fFlags := FOF_ALLOWUNDO or FOF_NOCONFIRMMKDIR;

    if not OverWrite then
      fFlags := fFlags or FOF_RENAMEONCOLLISION;
    if not ShowDialog then
      fFlags := fFlags or FOF_SILENT;
  end;

  Screen.Cursor := crHourGlass;
  try
    MyFOStruct.fAnyOperationsAborted := False;
    MyFOStruct.hNameMappings := nil;
    ResultVal := SHFileOperation(MyFOStruct);
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
  ShExecInfo.cbSize := SizeOf(ShExecInfo);
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
  BufSize: integer;
begin
  BufSize := ExpandEnvironmentStrings(PChar(AString), nil, 0);
  SetLength(Result, BufSize);
  ExpandEnvironmentStrings(PChar(AString), PChar(Result), BufSize);
  Result := TrimRight(Result);
end;

// include winapi methods
// https://www.delphipraxis.net/207745-utc-local-time.html
function TzSpecificLocalTimeToSystemTime(lpTimeZoneInformation
  : PTimeZoneInformation; var lpLocalTime, lpUniversalTime: TSystemTime): BOOL;
  stdcall; external kernel32 name 'TzSpecificLocalTimeToSystemTime';
function SystemTimeToTzSpecificLocalTime(lpTimeZoneInformation
  : PTimeZoneInformation; var lpUniversalTime, lpLocalTime: TSystemTime): BOOL;
  stdcall; external kernel32 name 'SystemTimeToTzSpecificLocalTime';

// convert local time to UTC
// https://www.delphipraxis.net/207745-utc-local-time.html
function DateTimeToUTC(const Local: TDateTime): TDateTime;
var
  TZI: TTimeZoneInformation;
  LocalTime, UniversalTime: TSystemTime;
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
  LocalTime, UniversalTime: TSystemTime;
begin
  GetTimeZoneInformation(TZI);
  DateTimeToSystemTime(UTC, UniversalTime);
  SystemTimeToTzSpecificLocalTime(@TZI, UniversalTime, LocalTime);
  Result := SystemTimeToDateTime(LocalTime);
end;

procedure SetFileCreationTime(const filename: string;
  const DateTime: TDateTime);
const
  FILE_WRITE_ATTRIBUTES = $0100;
var
  Handle: THandle;
  SystemTime: TSystemTime;
  FileTime: TFileTime;
begin
  // https://stackoverflow.com/questions/8446208/delphi-6-how-can-i-change-created-filedate-file-creation-date/8446338#8446338
  // Modified
  Handle := CreateFile(PChar(filename), FILE_WRITE_ATTRIBUTES,
    FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING,
    FILE_ATTRIBUTE_NORMAL, 0);
  if Handle = INVALID_HANDLE_VALUE then
    RaiseLastOSError;
  try
    DateTimeToSystemTime(DateTimeToUTC(DateTime), SystemTime);
    // DateTime zu DateTimeToUTC(DateTime) geändert, DM 30.03.2023
    if not SystemTimeToFileTime(SystemTime, FileTime) then
      RaiseLastOSError;
    if not SetFileTime(Handle, @FileTime, @FileTime, @FileTime) then
      // Modification + AccessTime hinzugefügt, nicht nur Create
      RaiseLastOSError;
  finally
    CloseHandle(Handle);
  end;
end;

function FileSize(const AFileName: String): int64;
var
  Info: TWin32FileAttributeData;
begin
  Result := -1;

  FillChar(Info, SizeOf(Info), 0);
  if GetFileAttributesEx(PChar(AFileName), GetFileExInfoStandard, @Info) then
  begin
    Int64Rec(Result).Hi := Info.nFileSizeHigh;
    Int64Rec(Result).Lo := Info.nFileSizeLow;
  end;
end;

function CleanFileName(const InputString: string): string;
var
  I: integer;
  ResultWithSpaces: string;
begin

  ResultWithSpaces := InputString;

  for I := 1 to Length(ResultWithSpaces) do
  begin
    // These chars are invalid in file names.
    case ResultWithSpaces[I] of
      '/', '\', ':', '*', '?', '"', '<', '>', '|', ' ', #$D, #$A, #9:
        // Use a * to indicate a duplicate space so we can remove
        // them at the end.
{$WARNINGS OFF} // W1047 Unsafe code 'String index to var param'
        if (I > 1) and ((ResultWithSpaces[I - 1] = ' ') or
          (ResultWithSpaces[I - 1] = '*')) then
          ResultWithSpaces[I] := '*'
        else
          ResultWithSpaces[I] := ' ';

{$WARNINGS ON}
    end;
  end;

  // A * indicates duplicate spaces.  Remove them.
  Result := ReplaceStr(ResultWithSpaces, '*', '');

  // Also trim any leading or trailing spaces
  Result := Trim(Result);

  (*
    if result = '' then
    begin
    raise(Exception.Create('Resulting FileName was empty Input string was: '
    + InputString));
    end;
  *)
end;

function FilesAreEqual(const File1, File2: TFileName): boolean;
const
  BlockSize = 65536;
var
  fs1, fs2: TFileStream;
  L1, L2: integer;
  B1, B2: array [1 .. BlockSize] of Byte;
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
            exit;
          end;
          if not CompareMem(@B1[1], @B2[1], L1) then
            exit;
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
  Buffer: array [0 .. MAX_PATH - 1] of char;
begin
  Result := '';
  SHGetSpecialFolderLocation(Application.Handle, CSIDL_DESKTOP, PIDList);
  if Assigned(PIDList) then
    if ShGetPathFromIDList(PIDList, Buffer) then
      Result := Buffer;
end;

function CreateDesktopShellLink(const TargetName, Args, ALinkName,
  AIconName: string; AIconIndex: integer): boolean;
var
  IObject: IUnknown;
  ISLink: IShellLink;
  IPFile: IPersistFile;
  PIDL: PItemIDList;
  LinkName: WideString;
  InFolder: array [0 .. MAX_PATH - 1] of char;
begin
  Result := False;

  IObject := CreateComObject(CLSID_ShellLink);
  ISLink := IObject as IShellLink;
  IPFile := IObject as IPersistFile;

  with ISLink do
  begin
    // SetDescription('Description ...');
    SetPath(PChar(TargetName));
    SetArguments(PChar(Args));
    SetIconLocation(PChar(AIconName), AIconIndex);
    SetWorkingDirectory(PChar(ExtractFilePath(TargetName)));
  end;

  SHGetSpecialFolderLocation(0, CSIDL_DESKTOPDIRECTORY, PIDL);
  ShGetPathFromIDList(PIDL, InFolder);

  LinkName := IncludeTrailingPathDelimiter(GetDesktopFolder) +
    ALinkName + '.lnk';

  DeleteFile(LinkName);

  if not FileExists(LinkName) then
    if IPFile.Save(PWideChar(LinkName), False) = S_OK then
      Result := True;
end;

procedure FilePutContentsA(filename, binary: ansistring);
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
  FO: TSHFileOpStruct;
begin
  FillChar(FO, SizeOf(FO), #0);
  FO.Wnd := 0;
  FO.wFunc := FO_COPY;
  FO.pFrom := PChar(Source + #0);
  FO.pTo := PChar(Target + #0);
  FO.fFlags := FOF_NOCONFIRMATION or FOF_SILENT;
  SHFileOperation(FO); // TODO: check result
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

function DirectoryExistsAndIsNotEmpty(DirName: string): boolean;
var
  SR: TSearchRec;
begin
  Result := False;
  if FindFirst(IncludeTrailingPathDelimiter(DirName) + '*', faAnyFile, SR) = 0
  then
  begin
    repeat
      if (copy(SR.Name, 1, 1) <> '.') and (UpperCase(SR.Name) <> 'THUMBS.DB')
      then
      begin
        Result := True;
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
  peOffset: integer;
begin
  try
    fs := TFileStream.Create(ExeFile, fmOpenRead or fmShareDenyNone);
    try
      fs.Seek($3C, soFromBeginning);
      fs.Read(peOffset, 4);

      fs.Seek(peOffset + 8, soFromBeginning);
      fs.Read(unixTime, 4);

{$IF CompilerVersion >= 20.0} // geraten
      Result := UnixToDateTime(unixTime, False);
{$ELSE}
      Result := UnixToDateTime(unixTime);
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
      if not FileAge(ExeFile, Result) then
        raise Exception.CreateFmt(StrGetBuildTimestamps, [ExeFile]);
    end;
  end;
end;

function GetOwnBuildTimestamp: TDateTime;
begin
  Result := GetBuildTimestamp(ParamStr(0));
end;

// TODO: CODE DUPLIKATE
// hl.Utils.pas (RichTextToPlainText)
// hl.Datenbank.CSVExporter.pas (RichTextToPlainText)
// hcl.Utils.Rtf.pas (ThclUtilsRtf.RtfToPlainText)
function RichTextToPlainText(richText: string): string;
var
  RichEdit1: TRichEdit;
  ss: TStringStream;
begin
  if copy(richText, 1, 5) <> '{\rtf' then
  begin
    Result := richText;
    exit;
  end;
  RichEdit1 := TRichEdit.Create(Application.MainForm);
  try
    // Wenn Visible=true oder Parent=nil, dann geht es nicht...
    RichEdit1.Width := 0;
    RichEdit1.Height := 0;
    RichEdit1.parent := Application.MainForm;

    // RichEdit1.Text := richText;
    ss := TStringStream.Create(richText);
    try
      RichEdit1.Lines.LoadFromStream(ss);
    finally
      FreeAndNil(ss);
    end;

    RichEdit1.PlainText := True;
    Result := Trim(RichEdit1.text);
  finally
    FreeAndNil(RichEdit1);
  end;
  if copy(Result, 1, 5) = '{\rtf' then
  begin
    ShowMessage(StrRTFToTextIstFehl);
  end;
end;

function ExpandEnvStr(const szInput: string): string;
// http://stackoverflow.com/a/2833147/3544341
const
  MAXSIZE = 32768;
begin
  SetLength(Result, MAXSIZE);
  SetLength(Result, ExpandEnvironmentStrings(PChar(szInput), @Result[1],
    Length(Result)));
end;

function ShellExecute64(HWND: HWND; Operation, filename, Parameters,
  Directory: PChar; ShowCmd: integer): HINST;
var
  testExe: string;
begin
  testExe := ThlUtils.Find3264ExeCandidate(filename);
  ChangeFSRedirection(True);
  try
    // Warum FS Umleitung deaktivieren? (Nur 32 Bit EXE auf 64 Bit OS)
    // Mit der WoW64-Weiterleitung würde ein 32-Bit Prozess bevorzugt
    // 32-Bit Anwendungen öffnen. Das heißt z.B. kein Snipping-Tool!
    Result := ShellExecute(HWND, Operation, PChar(testExe), Parameters,
      Directory, ShowCmd);
  finally
    ChangeFSRedirection(False);
  end;
end;

function RunCMD(cmdLine: string; WindowMode: integer): boolean;
var
  si: TStartupInfo;
  PI: TProcessInformation;
  sei: TShellExecuteInfo;
  err: integer;
begin
  ChangeFSRedirection(True);
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
    ZeroMemory(@si, SizeOf(si));
    si.cb := SizeOf(si);
    si.dwFlags := STARTF_USESHOWWINDOW;
    si.wShowWindow := WindowMode;

    if CreateProcess(nil, PChar(cmdLine), nil, nil, False, 0, nil, nil, si, PI)
    then
    begin
      CloseHandle(PI.hThread);
      CloseHandle(PI.hProcess);
      Result := True;
      exit;
    end;

    err := GetLastError;
    if (err = ERROR_BAD_EXE_FORMAT) or (err = ERROR_BAD_FORMAT) or
      (err = ERROR_FILE_NOT_FOUND { kommt bei URI z.B. 'ms-screensketch:' } )
    then
    begin
      ZeroMemory(@sei, SizeOf(sei));
      sei.cbSize := SizeOf(sei);
      sei.lpFile := PChar(cmdLine);
      sei.nShow := WindowMode;

      Result := ShellExecuteEx(@sei);
    end
    else
    begin
      Result := err = 0;
    end;
  finally
    ChangeFSRedirection(False);
  end;
end;

function GetFileModDate(filename: string): TDateTime;
var
  f: TSearchRec;
begin
  FindFirst(filename, faAnyFile, f);
  Result := FileDateToDateTime(f.Time);
  // if you really wanted an Int, change the return type and use this line:
  // Result := F.Time;
  FindClose(f);
end;

function Rot13char(c: char): char;
begin
  Result := c;
  if (c >= 'a') and (c <= 'm') or (c >= 'A') and (c <= 'M') then
    Result := Chr(Ord(c) + 13)
  else if (c >= 'n') and (c <= 'z') or (c >= 'N') and (c <= 'Z') then
    Result := Chr(Ord(c) - 13);
end;

{$REGION 'Firewall-Funktionen'}

function Firewall_IstPortFreigeschaltet(portno: integer): boolean;
var
  reg: TRegistry;
  SL: TStringList;
  s, s2: string;
begin
  Result := False;

  reg := TRegistry.Create;
  SL := TStringList.Create;
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    if reg.OpenKeyReadOnly
      ('SYSTEM\CurrentControlSet\Services\SharedAccess\Parameters\FirewallPolicy\FirewallRules')
    then
    begin
      reg.GetValueNames(SL);
      for s in SL do
      begin
        // Computer\HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Services\SharedAccess\Parameters\FirewallPolicy\FirewallRules
        // v2.30|Action=Allow|Active=TRUE|Dir=In|Protocol=6|LPort=11000|Name=CORAplus Socket-Dienst Port 11000|
        s2 := UpperCase(reg.ReadString(s));
        if (pos('|ACTION=ALLOW|', s2) > 0) and (pos('|ACTIVE=TRUE|', s2) > 0)
          and (pos('|DIR=IN|', s2) > 0) and
          (pos('|LPORT=' + intToStr(portno) + '|', s2) > 0) then
        begin
          Result := True;
          exit;
        end;
      end;
      reg.CloseKey;
    end;
  finally
    FreeAndNil(reg);
    FreeAndNil(SL);
  end;
end;

Procedure AddLANRule(Name: string; port: integer);
const
  NET_FW_IP_PROTOCOL_TCP = 6;
  NET_FW_ACTION_ALLOW = 1;
var
  CurrentProfiles: OleVariant;
  fwPolicy2: OleVariant;
  RulesObject: OleVariant;
  NewRule: OleVariant;
begin
  Name := StringReplace(Name, '"', '', [rfReplaceAll]);
  Name := StringReplace(Name, '''', '', [rfReplaceAll]);

  if Firewall_IstPortFreigeschaltet(port) then
    exit;

{$REGION 'PowerShell (bevorzugt)'}
  // Wir machen es so, weil wir dann automatisch Admin-Rechte einfordern, und weil Windows Server kein FwPolicy2 hat!
  // Hinweis: ShellExecuteWait funktioniert nicht. Nach dem UAC Dialog springt die Funktion zurück zum Programmcode
  if ShellExecute64(0, 'runas', 'powershell',
    PChar('-command "New-NetFirewallRule -DisplayName ''' + Name +
    ''' -Direction Inbound -Action Allow -Protocol TCP -LocalPort ' +
    intToStr(port) + '"'), '', SW_HIDE) > 32 then
  begin
    // ShellExecute64() wird erfolgreich beendet, wenn UAC angenommen wurde, NICHT wenn der Ziel-Prozess beendet wurde!
    while ThlUtils.processExists('powershell.exe') and
      not Firewall_IstPortFreigeschaltet(port) do
    begin
      Sleep(100);
    end;
  end;
  if Firewall_IstPortFreigeschaltet(port) then
    exit;
{$ENDREGION}
{$REGION 'netsh (deprecated)'}
  // netsh ist veraltet und wird in Kürze entfernt, daher haben wir zuerst PowerShell probiert
  // Hinweis: ShellExecuteWait funktioniert nicht. Nach dem UAC Dialog springt die Funktion zurück zum Programmcode
  if ShellExecute64(0, 'runas', 'netsh',
    PChar('advfirewall firewall add rule name="' + Name +
    '" dir=in action=allow protocol=TCP localport=' + intToStr(port)), '',
    SW_NORMAL) > 32 then
  begin
    // ShellExecute64() wird erfolgreich beendet, wenn UAC angenommen wurde, NICHT wenn der Ziel-Prozess beendet wurde!
    while ThlUtils.processExists('netsh.exe') and
      not Firewall_IstPortFreigeschaltet(port) do
    begin
      Sleep(100);
    end;
  end;
  if Firewall_IstPortFreigeschaltet(port) then
    exit;
{$ENDREGION}
{$REGION 'ActiveX/OLE'}

  // Sollte es jetzt immer noch nicht geklappt haben, probieren wir es noch einmal über OLE
  // Vorsicht: ActiveX muss initialisiert sein (CoInitialize) und man braucht Adminrechte!

  // https://theroadtodelphi.com/2013/11/21/using-the-windows-firewall-with-advanced-security-scripting-api-and-delphi/#Adding%20a%20Protocol%20Rule

  // Create the FwPolicy2 object.
  fwPolicy2 := CreateOleObject('HNetCfg.FwPolicy2');
  RulesObject := fwPolicy2.Rules;
  CurrentProfiles := fwPolicy2.CurrentProfileTypes;

  // Create a Rule Object.
  NewRule := CreateOleObject('HNetCfg.FWRule');

  NewRule.Name := Name;
  // NewRule.Description := 'Allow incoming network traffic coming from LAN interface type';
  NewRule.Protocol := NET_FW_IP_PROTOCOL_TCP;
  NewRule.LocalPorts := port;
  // NewRule.Interfacetypes := 'LAN';
  NewRule.Enabled := True;
  // NewRule.Grouping := 'My Group';
  // NewRule.Profiles := CurrentProfiles;
  NewRule.Action := NET_FW_ACTION_ALLOW;

  // Add a new rule
  RulesObject.Add(NewRule);

  if Firewall_IstPortFreigeschaltet(port) then
    exit;
{$ENDREGION}
  raise Exception.CreateFmt(StrPortDKonnteNicht, [port]);
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
  if Enable then
    sTmp := 'True'
  else
    sTmp := 'False';
  ShellExecute64(0, 'runas', 'powershell',
    PChar('-command "Set-NetFirewallRule -DisplayGroup ''' + GROUP_DE +
    ''' -Enabled ' + sTmp + ' -Profile Any"'), '', SW_HIDE);
  ShellExecute64(0, 'runas', 'powershell',
    PChar('-command "Set-NetFirewallRule -DisplayGroup ''' + GROUP_EN +
    ''' -Enabled ' + sTmp + ' -Profile Any"'), '', SW_HIDE);
{$ENDREGION}
{$REGION 'netsh (deprecated)'}
  if Enable then
    sTmp := 'Yes'
  else
    sTmp := 'No';
  ShellExecute64(0, 'runas', 'netsh',
    PChar('advfirewall firewall set rule group="' + GROUP_DE + '" new enable=' +
    sTmp), '', SW_HIDE);
  ShellExecute64(0, 'runas', 'netsh',
    PChar('advfirewall firewall set rule group="' + GROUP_EN + '" new enable=' +
    sTmp), '', SW_HIDE);
{$ENDREGION}
end;

{$ENDREGION}

function hl_GetMachineId: string;
var
  reg: TRegistry;
  ProductId: string;
  MachineGuid: string;
  I: integer;
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

    Result := ProductId + '.' + MachineGuid + '.' +
      UpperCase(MD5String(LowerCase(MD5String('ProductId' + ProductId)) +
      LowerCase(MD5String('MachineGuid' + MachineGuid))));
    for I := 1 to Length(Result) do
    begin
      Result[I] := Rot13char(Result[I]);
    end;
    Result := AnsiReverseString(Result);
  finally
    FreeAndNil(reg);
  end;
end;

procedure SecureDeleteFile(filename: string);
var
  SL: TStringList;
  muellName: string;
  siz: integer;
begin
  SL := TStringList.Create;
  try
    siz := FileSize(filename);
    if siz > 0 then
      SL.text := StringOfChar('@', siz);
    // TODO: Das ist recht langsam. Besser wäre ein FileStream
    SL.SaveToFile(filename);
    // Datei überschreiben, damit Undelete tools nichts wiederherstellen können
  finally
    FreeAndNil(SL);
  end;

  muellName := IncludeTrailingPathDelimiter(ExtractFilePath(filename)) +
    RandomString(10) + '.tmp';

  if RenameFile(filename, muellName) then
    DeleteFile(muellName)
  else
    DeleteFile(filename);
end;

function PadLeft(const Str: string; Ch: char; Count: integer): string;
begin
  // https://stackoverflow.com/questions/1679360/quick-padding-of-a-string-in-delphi
  if Length(Str) < Count then
  begin
    Result := StringOfChar(Ch, Count);
    Move(Str[1], Result[Count - Length(Str) + 1], Length(Str) * SizeOf(char));
  end
  else
    Result := Str;
end;

function PadRight(const Str: string; Ch: char; Count: integer): string;
begin
  // https://stackoverflow.com/questions/1679360/quick-padding-of-a-string-in-delphi
  if Length(Str) < Count then
  begin
    Result := StringOfChar(Ch, Count);
    Move(Str[1], Result[1], Length(Str) * SizeOf(char));
  end
  else
    Result := Str;
end;

function NewGUID: TGUID;
begin
  CreateGUID(Result);
end;

function NewGUIDString: string;
begin
  Result := GUIDToString(NewGUID);
end;

function IsEmptyGuidString(GuidStr: string): boolean;
begin
  Result := (GuidStr = '') or (GuidStr = '00000000-0000-0000-0000-000000000000')
    or (GuidStr = '{00000000-0000-0000-0000-000000000000}');
end;

{$IFDEF MSWINDOWS}
function IsEqualGUID; external 'ole32.dll' name 'IsEqualGUID';
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}

function IsEqualGUID(const guid1, guid2: TGUID): boolean;
var
  A, b: PIntegerArray;
begin
  A := PIntegerArray(@guid1);
  b := PIntegerArray(@guid2);
  Result := (A^[0] = b^[0]) and (A^[1] = b^[1]) and (A^[2] = b^[2]) and
    (A^[3] = b^[3]);
end;
{$ENDIF LINUX}

function StrgGedrueckt(key, Buchstabe: char): boolean;
begin
  // Bei OnKeyPress wird Strg+A als Key=#1 , Strg+B als Key=#2 übergeben usw.
  // Vorsicht: Key=#13 ist aber beispielsweise ENTER und nicht Strg+M !
  // if Ord(Key) < Ord('A') then Key := Chr(Ord(Key) - 1 + Ord('A'));
  if Ord(key) <= 26 then
    key := Chr(Ord(key) - 1 + Ord('A'));

  // Bei OnKeyDown / OnKeyPress:
  // Hier ist Strg+A wie folgt:  ssCtrl in ShiftState und Key='A'
  Result := (GetKeyState(VK_CONTROL) < { down } 0) and
    (GetKeyState(VK_SHIFT) >= { up } 0) and
    (GetKeyState(VK_MENU { Alt! } ) >= { up } 0) and
  // bei VK_MENU handelt es sich um die Alt-Taste, nicht um die Menü-Taste (= VK_APPS)
    (key = upcase(Buchstabe));
end;

function StrgShiftGedrueckt(key, Buchstabe: char): boolean;
begin
  // Bei OnKeyPress wird Strg+A als Key=#1 , Strg+B als Key=#2 übergeben usw.
  // Vorsicht: Key=#13 ist aber beispielsweise ENTER und nicht Strg+M !
  if Ord(key) < Ord('A') then
    key := Chr(Ord(key) - 1 + Ord('A'));

  // Bei OnKeyDown / OnKeyPress:
  // Hier ist Strg+A wie folgt:  ssShift in ShiftState and ssCtrl in ShiftState and Key='A'
  Result := (GetKeyState(VK_CONTROL) < { down } 0) and
    (GetKeyState(VK_SHIFT) < { down } 0) and
    (GetKeyState(VK_MENU { Alt! } ) >= { up } 0) and
  // bei VK_MENU handelt es sich um die Alt-Taste, nicht um die Menü-Taste (= VK_APPS)
    (key = upcase(Buchstabe));
end;

function GetDirSize(dir: string; subdir: boolean): int64;
var
  rec: TSearchRec;
  found: integer;
begin
  Result := 0;
  if not DirectoryExists(dir) then
    exit;
  dir := IncludeTrailingPathDelimiter(dir);
  found := FindFirst(dir + '*.*', faAnyFile, rec);
  while found = 0 do
  begin
    inc(Result, rec.Size);
    if subdir and (rec.Attr and faDirectory > 0) and (rec.Name[1] <> '.') then
      inc(Result, GetDirSize(dir + rec.Name, True));
    found := FindNext(rec);
  end;
  FindClose(rec);
end;

function SendArp(DestIP, SrcIP: ULONG; pMacAddr: Pointer; PhyAddrLen: Pointer)
  : DWORD; StdCall; external 'iphlpapi.dll' name 'SendARP';

function GetRouterMac(debug: boolean = False): string;

  function GetMacAddr(const IPAddress: string; var ErrCode: DWORD): string;
  var
    MacAddr: Array [0 .. 5] of Byte;
    DestIP: ULONG;
    PhyAddrLen: ULONG;
    WSAData: TWSAData;
  begin
    // https://stackoverflow.com/questions/4550672/delphi-get-mac-of-router
    Result := '';
    WSAStartup($0101, WSAData);
    try
      ZeroMemory(@MacAddr, SizeOf(MacAddr));
      DestIP := inet_addr(pansiChar(ansistring(IPAddress)));
      PhyAddrLen := SizeOf(MacAddr);
      ErrCode := SendArp(DestIP, 0, @MacAddr, @PhyAddrLen);
      if ErrCode = S_OK then
        Result := Format('%2.2x-%2.2x-%2.2x-%2.2x-%2.2x-%2.2x',
          [MacAddr[0], MacAddr[1], MacAddr[2], MacAddr[3], MacAddr[4],
          MacAddr[5]])
    finally
      WSACleanup;
    end;
  end;

var
  gateway: string;
  ec: DWORD;
  macrouter: string;
  SL: TStringList;
  serr: string;
const
  delim = ',';
begin
  Result := '';

  gateway := GetWMIarray('.', 'root\CIMV2', 'Win32_NetworkAdapterConfiguration',
    'DefaultIPGateway', delim);
  gateway := StringReplace(gateway, '{', '', [rfReplaceAll]);
  gateway := StringReplace(gateway, '}', '', [rfReplaceAll]);

  SL := TStringList.Create;
  try
    SL.Delimiter := delim;
    SL.DelimitedText := gateway;
    if SL.Count = 0 then
    begin
      if debug then
        macrouter := 'ERR_NO_ADAPTERS'
      else
        macrouter := '';
    end
    else
    begin
      try
        macrouter := GetMacAddr(SL[0], ec);
      except
        on E: EAbort do
        begin
          Abort;
        end;
        on E: Exception do
        begin
          if debug then
            macrouter := 'ERR_EXCEPT_' + E.Message
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
      else if ec = 1168 (* ERROR_NOT_FOUND *) then
        serr := 'ERROR_NOT_FOUND'
      else if ec = ERROR_NOT_SUPPORTED then
        serr := 'ERROR_NOT_SUPPORTED'
      else if ec = ERROR_NETWORK_UNREACHABLE then
        // not documented in MSDN WinApi
        serr := 'ERROR_NETWORK_UNREACHABLE'
      else if ec <> S_OK then
        serr := 'ERROR_' + intToStr(ec);

      if ec <> 0 then
      begin
        if debug then
          macrouter := serr
        else
          macrouter := '';
      end;
    end;
  finally
    FreeAndNil(SL);
  end;

  Result := macrouter;
end;

function DayOfWeekGerman(ADate: TDateTime): integer;
begin
  ADate := Trunc(ADate); // hinzugefügt DM 17.08.2022

  // https://www.delphipraxis.net/86137-kalenderwoche-ermitteln.html
  Result := DayOfWeek(ADate) - 1;
  if Result <= 0 then
    Result := 7;
end;

function WeekOfDate(A: TDateTime): integer;
var
  Day: integer;
  Y, M, d: Word;
begin
  A := Trunc(A); // hinzugefügt DM 17.08.2022

  // https://www.delphipraxis.net/86137-kalenderwoche-ermitteln.html
  Day := DayOfWeekGerman(A);
  DecodeDate(A + 4 - Day, Y, M, d);
  Result := Round(((A + 8 - Day) - Encodedate(Y, 1, 1)) / 7);
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
  SysLocale.FarEast := False;
  SysLocale.MiddleEast := True;

  FormatSettings.TwoDigitYearCenturyWindow := 50;
  FormatSettings.ListSeparator := ';';
end;

function MyDocumentsPath: string;
var
  Allocator: IMalloc;
  SpecialDir: PItemIDList;
  FBuf: array [0 .. MAX_PATH] of char;
begin
  Result := '';
  if ShGetMalloc(Allocator) = NOERROR then
  begin
    SHGetSpecialFolderLocation(0, CSIDL_PERSONAL, SpecialDir);
    ShGetPathFromIDList(SpecialDir, @FBuf[0]);
    Allocator.Free(SpecialDir);
    Result := string(FBuf);
  end;
end;

procedure HideFile(fname: string);
begin
  FileSetAttr(fname, FileGetAttr(fname) or faHidden);
end;

{$IFDEF UNICODE}
function GetUserNameEx(NameFormat: DWORD; lpNameBuffer: LPWSTR; nsize: PULONG)
  : DWORD; stdcall; external 'secur32.dll' Name 'GetUserNameExW';
{$ELSE}
function GetUserNameEx(NameFormat: DWORD; lpNameBuffer: LPSTR; nsize: PULONG)
  : DWORD; stdcall; external 'secur32.dll' Name 'GetUserNameExA';
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
  sUserName: string;
  dwUserNameLen: DWORD;
begin
  // https://www.viathinksoft.de/codelib/209
  dwUserNameLen := cnMaxUserNameLen - 1;
  SetLength(sUserName, cnMaxUserNameLen);
  if GetUserNameEx(EXTENDED_NAME_FORMAT_NameDisplay, PChar(sUserName),
    @dwUserNameLen) = 0 then
  begin
    Result := '';
    RaiseLastOSError;
  end;

  // There is probably a bug in Win10/Win11 in GetUserNameExW
  // When the attribute does not exist (e.g. the user has no display name),
  // then GetUserNameEx!=0 (success), but nSize stays untouched, not changed to 0!!!
  if dwUserNameLen = cnMaxUserNameLen - 1 then
    dwUserNameLen := 0;

  SetLength(sUserName, dwUserNameLen);
  Result := sUserName;
end;

// https://www.delphipraxis.net/189175-feststellen-ob-als-administrator-ausgefuehrt.html
Const
  SECURITY_NT_AUTHORITY: TSIDIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 5));
  SECURITY_BUILTIN_DOMAIN_RID = $00000020;
  DOMAIN_ALIAS_RID_ADMINS = $00000220;
  DOMAIN_ALIAS_RID_USERS = $00000221;
  DOMAIN_ALIAS_RID_GUESTS = $00000222;
  DOMAIN_ALIAS_RID_POWER_USERS = $00000223;
function CheckTokenMembership(TokenHandle: THandle; SidToCheck: PSID;
  var IsMember: BOOL): BOOL; stdcall; external advapi32;

function IsUserAdmin: boolean;
var
  IsMember: BOOL;
  AdministratorsGroup: PSID;
begin
  Win32Check(AllocateAndInitializeSid(
    { } SECURITY_NT_AUTHORITY,
    { } 2, // 2 sub-authorities
    { } SECURITY_BUILTIN_DOMAIN_RID, // sub-authority 0
    { } DOMAIN_ALIAS_RID_ADMINS, // sub-authority 1
    { } 0, 0, 0, 0, 0, 0, // sub-authorities 2-7 not passed
    { } AdministratorsGroup));
  try
    Win32Check(CheckTokenMembership(0, AdministratorsGroup, IsMember));
  finally
    FreeSid(AdministratorsGroup);
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
  if frm.WindowState = wsMinimized then
    ShowWindow(frm.Handle, SW_RESTORE);
  // Application.ProcessMessages;
end;

function DaysHumanReadable(days: integer): string;
begin
  if days >= 2 * 12 * 30 then
    Result := Format(StrDJahren, [days div (30 * 12)])
  else if days >= 12 * 30 then
    Result := Str1Jahr

  else if days >= 2 * 30 then
    Result := Format(StrDMonaten, [days div 30])
  else if days >= 2 * 30 then
    Result := Str1Monat

  else if days >= 2 then
    Result := Format(StrDTagen, [days])
  else
    Result := Str1Tag;
end;

function WindowsBits: integer;
begin
  // Gibt 32 oder 64 aus, je nachdem ob WINDOWS 32 oder 64 Bit ist.
  // NICHT die Anwendung, sondern WINDOWS!
{$IFDEF WIN64}
  Result := 64;
{$ELSE}
  if IsWow64 then
    Result := 64
  else
    Result := 32;
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

function Hash_djb2(Str: string): string;

  function _Hash_djb2(AData: Pointer; ADataLength: integer): Cardinal;
  var
    I: integer;
  begin
    // https://helloacm.com/simple-and-fast-hash-functions-in-delphi/
{$OVERFLOWCHECKS OFF}
    Result := 5381;
    for I := 1 to ADataLength do
    begin
      Result := ((Result shl 5) + Result) + PByte(AData)^;
      AData := Pointer(NativeUInt(AData) + 1);
    end;
{$OVERFLOWCHECKS ON}
  end;

begin
  Result := Format('%x', [_Hash_djb2(PChar(Str), Length(Str) * SizeOf(char))]);
end;

function GetDeepestDir(const AFileName: string): string;
begin
  Result := ExtractFileName(ExtractFileDir(AFileName));
end;

procedure EmptyKeyQueue;
var
  Msg: TMsg;
begin
  // https://www.swissdelphicenter.ch/en/showcode.php?id=1066
  while PeekMessage(Msg, 0, WM_KEYFIRST, WM_KEYLAST, PM_REMOVE or
    PM_NOYIELD) do;
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
  dir: array [0 .. MAX_PATH] of char;
begin
  // https://www.swissdelphicenter.ch/en/showcode.php?id=144
  GetWindowsDirectory(dir, MAX_PATH);
  Result := StrPas(dir);
end;

function DarkModeIsEnabled: boolean;
// https://github.com/checkdigits/delphidarkmode/blob/master/WindowsDarkMode.pas
{$IFDEF MSWINDOWS}
const
  TheKey = 'Software\Microsoft\Windows\CurrentVersion\Themes\Personalize\';
  TheValue = 'AppsUseLightTheme';
var
  reg: TRegistry;
{$ENDIF}
begin

  Result := False; // There is no dark side - the Jedi are victorious!

  // This relies on a registry setting only available on MS Windows
  // If the developer has somehow managed to get to this point then tell
  // them not to do this!
{$IFNDEF MSWINDOWS}
{$MESSAGE WARN '"DarkModeIsEnabled" will only work on MS Windows targets'}
{$ELSE}
  reg := TRegistry.Create(KEY_READ);
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if reg.KeyExists(TheKey) then
      if reg.OpenKey(TheKey, False) then
        try
          if reg.ValueExists(TheValue) then
            Result := reg.ReadInteger(TheValue) = 0;
        finally
          reg.CloseKey;
        end;
  finally
    FreeAndNil(reg);
  end;
{$ENDIF}
end;

function IsStrANumber(const s: string): boolean;
var
  p: PChar;
begin
  p := PChar(s);
  Result := False;
  while p^ <> #0 do
  begin
    if not(p^ in ['0' .. '9']) then
      exit;
    inc(p);
  end;
  Result := True;
end;

function GetSysDir: string;
var
  Buffer: array [0 .. MAX_PATH] of char;
begin
  GetSystemDirectory(Buffer, MAX_PATH - 1);
  SetLength(Result, StrLen(Buffer));
  Result := Buffer;
end;

function Rfc3339ToDatetime(rfc: string): TDateTime;
var
  jahr, monat, tag, stunde, minute, sekunde, off_h, off_m: integer;
  UTC: TDateTime;
begin
  // 2020-07-27T10:03:33+0000
  // 123456789012345678901234
  jahr := StrToInt(copy(rfc, 1, 4));
  monat := StrToInt(copy(rfc, 6, 2));
  tag := StrToInt(copy(rfc, 9, 2));
  stunde := StrToInt(copy(rfc, 12, 2));
  minute := StrToInt(copy(rfc, 15, 2));
  sekunde := StrToInt(copy(rfc, 18, 2));
  off_h := StrToInt(copy(rfc, 21, 2));
  off_m := StrToInt(copy(rfc, 23, 2));
  UTC := EncodeDateTime(jahr, monat, tag, stunde - off_h, minute - off_m,
    sekunde, 0);
  Result := UTCToLocalDateTime(UTC);
end;

// Prüft 32- und 64-Bit
function IsOdbcDriverInstalled(const DriverName: string): boolean;
var
  reg: TRegistry;
  found: integer;
  Expected: integer;
begin
  found := 0;
  Expected := 0;
  reg := TRegistry.Create(KEY_READ);
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;

    // 64-Bit-Treiber in 64-Bit Prozess
    // oder 32-Bit-Treiber in 32-Bit-Prozess
    inc(Expected);
    if reg.OpenKeyReadOnly('SOFTWARE\ODBC\ODBCINST.INI\ODBC Drivers') then
    begin
      if reg.ValueExists(DriverName) and
        (reg.ReadString(DriverName) = 'Installed') then
      begin
        inc(found);
      end;
      reg.CloseKey;
    end;

    // IsWow64=true, wenn es ein 32-Bit Prozess auf einem 64-Bit OS ist
    if (WindowsBits = 64) and not IsWow64 then
    begin
      // 32-Bit Treiber in 64-Bit-Prozess
      inc(Expected);
      if reg.OpenKeyReadOnly
        ('SOFTWARE\WOW6432Node\ODBC\ODBCINST.INI\ODBC Drivers') then
      begin
        if reg.ValueExists(DriverName) and
          (reg.ReadString(DriverName) = 'Installed') then
        begin
          inc(found);
        end;
        reg.CloseKey;
      end;
    end;
  finally
    FreeAndNil(reg);
  end;

  Result := found = Expected;
end;

function Make_EditDisplayFormat(nachkommastellen: integer;
  istEditFormat: boolean): string;
begin
  Result := '';
  Result := Result + StringOfChar('#', nachkommastellen);
  if not istEditFormat then
  begin
    Result := Result + ',';
  end;
  Result := Result + '##0.' + StringOfChar('0', nachkommastellen);
end;

function WindowsVersionString: string;
var
  reg: TRegistry;
  s1, s2: string;
begin
  try
    s1 := GetWMIstring('.', 'root\CIMV2', 'Win32_OperatingSystem', 'Caption');
    s2 := GetWMIstring('.', 'root\CIMV2', 'Win32_OperatingSystem',
      'OSArchitecture');
    Result := Format('%s (%s)', [s1, s2]);
  except
    on E: EAbort do
    begin
      Abort;
    end;
    on E: Exception do
    begin
      if IsWindows11 then
      begin
        Result := 'Windows 11';
        exit;
      end;

      if IsWow64 then
        reg := TRegistry.Create(KEY_READ OR KEY_WOW64_64KEY)
      else
        reg := TRegistry.Create(KEY_READ);
      try
        reg.RootKey := HKEY_LOCAL_MACHINE;
        if reg.OpenKeyReadOnly('Software\Microsoft\Windows NT\CurrentVersion')
        then
        begin
          Result := reg.ReadString('ProductName') + ', Build ' +
            reg.ReadString('CurrentBuild');
          reg.CloseKey;
        end
        else
          Result := 'Windows ???';
      finally
        FreeAndNil(reg);
      end;

      if IsWow64 then
        Result := Result + ' (64-Bit)'
      else
        Result := Result + ' (32-Bit)';
    end;
  end;
end;

function HasReadAccessToFile(const FileName: string): Boolean;
var
  hFile: THandle;
  SecurityAttr: TSecurityAttributes;
begin
  Result := False;

  // Datei muss existieren
  if not FileExists(FileName) then
    Exit;

  // Sicherheitsattribute für Zugriff ohne Vererbung
  SecurityAttr.nLength := SizeOf(TSecurityAttributes);
  SecurityAttr.bInheritHandle := False;
  SecurityAttr.lpSecurityDescriptor := nil;

  // Versuch, die Datei mit GENERIC_READ zu öffnen
  hFile := CreateFile(PChar(FileName),
                      GENERIC_READ,
                      FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,
                      @SecurityAttr,
                      OPEN_EXISTING,
                      FILE_ATTRIBUTE_NORMAL,
                      0);

  if hFile <> INVALID_HANDLE_VALUE then
  begin
    Result := True;
    CloseHandle(hFile);
  end;
end;

function HasWriteAccessToFile(const FileName: string): Boolean;
var
  hFile: THandle;
  SecurityAttr: TSecurityAttributes;
begin
  Result := False;

  // Sicherheitsattribute für Zugriff ohne Vererbung
  SecurityAttr.nLength := SizeOf(TSecurityAttributes);
  SecurityAttr.bInheritHandle := False;
  SecurityAttr.lpSecurityDescriptor := nil;

  if FileExists(FileName) then
  begin
    // Datei existiert: versuchen mit GENERIC_WRITE zu öffnen
    hFile := CreateFile(PChar(FileName),
                        GENERIC_WRITE,
                        FILE_SHARE_READ or FILE_SHARE_WRITE,
                        @SecurityAttr,
                        OPEN_EXISTING,
                        FILE_ATTRIBUTE_NORMAL,
                        0);
  end
  else
  begin
    // Datei existiert nicht: versuchen sie neu zu erstellen
    hFile := CreateFile(PChar(FileName),
                        GENERIC_WRITE,
                        FILE_SHARE_READ or FILE_SHARE_WRITE,
                        @SecurityAttr,
                        CREATE_NEW,
                        FILE_ATTRIBUTE_NORMAL or FILE_FLAG_DELETE_ON_CLOSE,
                        0);
  end;

  if hFile <> INVALID_HANDLE_VALUE then
  begin
    Result := True;
    CloseHandle(hFile);
  end;
end;

procedure SleepWithMessages(Milliseconds: Integer);
var
  TargetTime: DWORD;
begin
  TargetTime := GetTickCount + Milliseconds;
  while (GetTickCount < TargetTime) do
  begin
    Application.ProcessMessages;
    if Application.Terminated then Abort;
    Sleep(10); // kleine Pause, nicht komplett blockierend
  end;
end;

end.
