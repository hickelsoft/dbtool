unit hl.Utils.Gui;

interface

uses
  Windows, Messages, Forms, Classes, SysUtils, Math, Controls, ShellAPI,
  StdCtrls, ZLib, DBCtrls, ADODB, SHFolder, ComCtrls, Dialogs;

function AdvSelectDirectory(const Caption: string;
  const Root: WideString; var Directory: string; EditBox: boolean = False;
  ShowFiles: boolean = False; AllowCreateDirs: boolean = True): boolean;

procedure CropFrame(frame: TFrame; onlyVisible: boolean;
  rightPadding, bottomPadding: integer);

function EnterToTabAllowed(f: TForm): boolean;

function GetNextPossibleForm(ctrl: TComponent): TForm;

procedure AnDenAnfangScrollen(riched: TCustomRichEdit);
procedure AnsEndeScrollen(riched: TCustomRichEdit);

procedure MDI_Form_BringToFront(frm: TForm);

function RichTextToPlainText(richText: string): string;

{$IF CompilerVersion >= 20.0}
procedure BmpToPngFile(bmpFile, pngFile: string);
{$ENDIF}

implementation

uses
  FormatSettingsCompat, StrUtils, TlHelp32,
  Registry, ComObj, DateUtils,
  {$IF CompilerVersion >= 20.0} // geraten
  PngImage, Graphics,
  {$IFEND}
  ShlObj, ActiveX;

function AdvSelectDirectory(const Caption: string;
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

// TODO: CODE DUPLIKATE
// hl.Utils.Gui.pas (RichTextToPlainText)
// hl.Datenbank.CSVExporter.pas (RichTextToPlainText)
// hcl.Utils.Rtf.pas (ThclUtilsRtf.RtfToPlainText)
function RichTextToPlainText(richText: string): string;
resourcestring
  StrRTFToTextIstFehl = 'RTF-To-Text ist fehlgeschlagen!';
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

{$IF CompilerVersion >= 20.0}
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
{$ENDIF}

end.
