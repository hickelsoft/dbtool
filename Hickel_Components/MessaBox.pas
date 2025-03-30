unit MessaBox;

interface

uses
   Windows, SysUtils, Messages, Classes, Graphics, Controls, Forms, StdCtrls, ExtCtrls, Buttons, mmSystem, HsGauge, Menus, Clipbrd;

type
  // Wichtig: Muss nach Schwere aufsteigend sortiert sein
  TMBstyle = (mbsInformation, mbsQuestion, mbsExclamation, mbsStop);

type
  TMBbuttons = (mbbOk, mbbOkCancel, mbbYesNo, mbbYesNoCancel, mbbRetryCancel, mbbAbortRetryIgnore);

type
  TBeforeExecute = procedure(ParamString: string) of object;

type
  {$IF CompilerVersion > 20.0} // Version geraten
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$IFEND}
  TMessageBox = class(TComponent)
  private
    FCaption, FText: String;
    FStyle: TMBstyle;
    FButtons: TMBbuttons;
    FParamString: string;
    FBeforeExecute: TBeforeExecute;
    FScaleX, FScaleY: Integer;
    DLG_MessageBox: TForm;
    pmPopupMenu: TPopupMenu;
    FMoreInfoSL: TStrings;
    procedure DlgShowEvent(Sender: TObject);
    procedure MoreInfoClick(Sender: TObject);
    procedure MessageBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  protected
    procedure DoMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PopupMenuClick(Sender: TObject);
  public
    class var DisableOkFocus: boolean;
    procedure Close;
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: integer;
    procedure RaiseAsException;
    property ScaleX: Integer read FScaleX write FScaleX;
    property ScaleY: Integer read FScaleY write FScaleY;
    class procedure ZeigeException(E: Exception);
  published
    property Buttons: TMBbuttons read FButtons write FButtons default mbbOK;
    property Caption: string read FCaption write FCaption;
    property ParamString: string read FParamString write FParamString;
    property Style: TMBstyle read FStyle write FStyle default mbsInformation;
    property Text: string read FText write FText;
    property MoreInfoSL: TStrings read FMoreInfoSL;

    property BeforeExecute: TBeforeExecute read FBeforeExecute write FBeforeExecute;
  end;

// ShowMessage-Ersatz
// Typischer VCL Aufruf: MessageDlg('Nachricht', '', mtWarning, mbOKCancel, 0);
function HsShowMessage(sText: string; sCaption: string=''; mbStyle: TMBstyle=mbsInformation; mbButtons: TMBbuttons=mbbOk; AMoreInfoSL: TStrings=nil): Integer; overload;

// ShowMessageFmt-Ersatz
procedure HsShowMessageFmt(const sText: string; Params: array of const);

procedure HsShowMoreInfoDialog(const sText: string; AOwner: TComponent=nil);


procedure Register;

implementation

// Delphi 2007:
// Für die Unit JclDebug muss JEDI Installiert sein (VCL_JEDI\Install.bat ausführen und Anweisungen folgen).
// Manchmal, wenn er hier hängt, muss auch einfach nur Delphi neu gestartet werden.
// dann im Projekt-Suchpfad einfügen: ..\VCL_JEDI\source\common und ..\VCL_JEDI\source\windows
// Delphi 12:
// JEDI über GetIt PackageManager installieren. Im nachfolgenden Installer nicht vergessen "Only install selected" zu klicken, damit nur Delphi 12 bearbeitet wird.
uses HsTools, Dialogs, ShellAPI, Math, JclDebug;

{$R MsgBox.Res}

type
  TDLG_MoreInfoForm = class(TForm)
  public
    Memo1: TMemo;
    Panel1: TPanel;
    DruckenSpeichern: TButton;
    procedure DruckenSpeichernKlick(Sender: TObject);
    procedure ZwischenablageKopieKlick(Sender: TObject);
    procedure SchliessenKlick(Sender: TObject);
    constructor Create(AOwner: TComponent); reintroduce;
  end;

procedure Register;
begin
  RegisterComponents('HS', [TMessageBox]);
end;

function HsShowMessage(sText: string; sCaption: string=''; mbStyle: TMBstyle=mbsInformation; mbButtons: TMBbuttons=mbbOk; AMoreInfoSL: TStrings=nil): Integer; overload;
begin
// TODO: Prüfen, ob das irgendwelche Nebenwirkungen hätte
//  if Windows.GetCurrentThreadId <> System.MainThreadID then exit; // Webshop usw!

   sText := StringReplace(sText, '&', '&&', [rfReplaceAll]);
   with TMessageBox.Create(nil) do
   begin
      Caption := sCaption;
      Text := sText;
      Buttons := mbButtons;
      Style := mbStyle;
      if Assigned(AMoreInfoSL) then MoreInfoSL.Assign(AMoreInfoSL);
      result := Execute;
      Free;
   end;
end;

procedure HsShowMessageFmt(const sText: string; Params: array of const);
begin
  HsShowMessage(Format(sText, Params));
end;

{ TMessageBox }

constructor TMessageBox.Create(aOwner: TComponent);
var
   aMenuItem: TMenuItem;

begin
   inherited Create(aOwner);

   Style := mbsInformation;
   Buttons := mbbOK;
   FMoreInfoSL := TStringList.Create;

   ScaleY := 100;
   ScaleX := 100;

   pmPopupMenu := TPopupMenu.Create(self);
   aMenuItem := TMenuItem.Create(self);
   aMenuItem.Caption := 'Text in die Zwischenablage &kopieren';
   aMenuItem.OnClick := PopupMenuClick;
   pmPopupMenu.Items.Add(aMenuItem);
end;

procedure TMessageBox.PopupMenuClick(Sender: TObject);
begin
   ClipBoard.SetTextBuf(pChar(FText));
end;

procedure TMessageBox.RaiseAsException;
begin
  raise Exception.Create(Text);
end;

procedure TMessageBox.DoMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   ReleaseCapture;
   SendMessage(DLG_MessageBox.Handle, WM_NCLBUTTONDOWN, 2, 0);
end;

function Hi(Label1: TLabel): integer;
var
  lRect : TRect;
  lText : string;

begin
  lRect.Left := 0;
  lRect.Right := label1.Width;
  lRect.Top := 0;
  lRect.Bottom := 0;

  lText := Label1.Caption;

  Label1.Canvas.TextRect(
            {var} lRect, //will be modified to fit the text dimensions
            {var} lText, //not modified, unless you use the "tfModifyingString" flag
            [tfCalcRect, tfWordBreak] //flags to say "compute text dimensions with line breaks"
          );
  ASSERT( lRect.Top = 0 ); //this shouldn't have moved
  result := lRect.Bottom;
end;

function ForceForegroundWindow(hwnd: THandle): Boolean;
const
  SPI_GETFOREGROUNDLOCKTIMEOUT = $2000;
  SPI_SETFOREGROUNDLOCKTIMEOUT = $2001;
var
  ForegroundThreadID: DWORD;
  ThisThreadID: DWORD;
  timeout: DWORD;
begin
  if IsIconic(hwnd) then ShowWindow(hwnd, SW_RESTORE);

  if GetForegroundWindow = hwnd then Result := True
  else
  begin
    // Windows 98/2000 doesn't want to foreground a window when some other
    // window has keyboard focus

    if ((Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion > 4)) or
      ((Win32Platform = VER_PLATFORM_WIN32_WINDOWS) and
      ((Win32MajorVersion > 4) or ((Win32MajorVersion = 4) and
      (Win32MinorVersion > 0)))) then
    begin
      Result := False;
      ForegroundThreadID := GetWindowThreadProcessID(GetForegroundWindow, nil);
      ThisThreadID := GetWindowThreadPRocessId(hwnd, nil);
      if AttachThreadInput(ThisThreadID, ForegroundThreadID, True) then
      begin
        BringWindowToTop(hwnd); // IE 5.5 related hack
        SetForegroundWindow(hwnd);
        AttachThreadInput(ThisThreadID, ForegroundThreadID, False);
        Result := (GetForegroundWindow = hwnd);
      end;
      if not Result then
      begin
        // Code by Daniel P. Stasinski
        SystemParametersInfo(SPI_GETFOREGROUNDLOCKTIMEOUT, 0, @timeout, 0);
        SystemParametersInfo(SPI_SETFOREGROUNDLOCKTIMEOUT, 0, TObject(0),
          SPIF_SENDCHANGE);
        BringWindowToTop(hwnd); // IE 5.5 related hack
        SetForegroundWindow(hWnd);
        SystemParametersInfo(SPI_SETFOREGROUNDLOCKTIMEOUT, 0, TObject(timeout), SPIF_SENDCHANGE);
      end;
    end
    else
    begin
      BringWindowToTop(hwnd); // IE 5.5 related hack
      SetForegroundWindow(hwnd);
    end;

    Result := (GetForegroundWindow = hwnd);
  end;
end;

procedure TMessageBox.MessageBoxKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = Ord('C')) and (Shift = [ssCtrl]) then
  begin
    Key := 0;
    PopupMenuClick(Sender);
  end;
end;

function TMessageBox.Execute: Integer;
var
  aPanel: TPanel;
  Label1: TLabel;
  TopAdd: Integer;

begin
   if assigned(FBeforeExecute) then FBeforeExecute(FParamString);
   DLG_MessageBox := TForm.Create(self);
   try
     with DLG_MessageBox do
     begin
        ForceForegroundWindow(Handle);
        BorderStyle := bsNone;
        ClientHeight := 163;
        ClientWidth := 492;  // Ursprünglich 392
        OnKeyDown := MessageBoxKeyDown;
        KeyPreview := true;
        if Assigned(Application.MainForm) then
        begin
          if Application.MainForm.Position = poScreenCenter then
          begin
            // TODO: Das ist ein notbehelfmäßiger Fix und eigentlich falsch
            // Bei Touch-Screens ist eine Warnmeldung, die vor dem Öffnen des
            // Mainforms erscheint total verschoben und nicht bildschirmmittig.
            // Da das Programm (Wartung.exe oder Fernwartung.exe) gerade im OnShow ist,
            // kann ich nicht prüfen, ob das Fenster bereits sichtbar ist oder nicht.
            Application.MainForm.Left := 0;
            Application.MainForm.Top := 0;
          end;
        end;

        if Assigned(Application.MainForm) then
          Position := poMainFormCenter
        else
          Position := poScreenCenter;
     end;
     with TPanel.Create(DLG_MessageBox) do
     begin
        Parent := DLG_MessageBox;
        Align := alTop;
        Height := 20;
        Alignment := taLeftJustify;
        if FCaption = '' then
        begin
          case FStyle of
            mbsInformation:  Caption := Application.Title + ' - Information';
            mbsStop:         Caption := Application.Title + ' - Fehler';
            mbsQuestion:     Caption := Application.Title + ' - Frage';
            mbsExclamation:  Caption := Application.Title + ' - Achtung';
            else             Caption := Application.Title;
          end;
        end
        else
        begin
          Caption := FCaption;
        end;
        Caption := '  ' + Caption;
        ParentBackground := false;
        Color := clNavy;
        Font.Color := clWhite;
        Font.Style := [fsBold];
        BevelOuter := bvLowered;
        OnMouseDown := DoMouseDown;
     end;
     aPanel := TPanel.Create(DLG_MessageBox);
     with aPanel do
     begin
        Parent := DLG_MessageBox;
        Align := alClient;
        BevelOuter := bvLowered;
        Caption := '';
     end;
     with THsGauge.Create(aPanel) do
     begin
        Parent := aPanel;
        Align := alClient;
        BorderStyle := bsNone;
        if Style = mbsStop then
        begin
          ForeColor := clYellow;
          ForeColor2 := clRed;
        end
        else if Style = mbsExclamation then
        begin
          ForeColor := $0000DFFF;
          ForeColor2 := clInfoBk;
        end
        else
        begin
          ForeColor := clAqua;
          ForeColor2 := clWhite; // clBtnFace;
        end;
        Progress := 100;
        ShowText := false;
        Kind := gkVerticalBar;
     end;
   
     Label1 := TLabel.Create(aPanel);
     with Label1 do
     begin
        Parent := aPanel;
        AutoSize := false;
        WordWrap := true;
        Transparent := True;
        Left := 68;
        Top := 8;
        Width := 400; // Ursprünglich 300
        Height := 92;
        Caption := FText;
        PopupMenu := pmPopupMenu;
        Font.Color := clBlack;
     end;
     TopAdd := Max(0, Hi(Label1) - Label1.Height);
     Label1.Height := Label1.Height + TopAdd;

     with TImage.Create(aPanel) do
     begin
        Parent := aPanel;
        AutoSize := false;
        Stretch := true;
        Left := 8;
        Top := 8;
        Width := 48;
        Height := 48;
        Transparent := true;

        if Copy(DateToStr(Now),1,6) = '01.04.' then
        begin
          if RandomRange(0,2){return 0 or 1} = 0 then
          begin
            case FStyle of
               mbsInformation: begin Picture.Bitmap.LoadFromResourceName(hInstance, 'APRIL1'); MessageBeep(MB_ICONINFORMATION); end;
               mbsStop: begin Picture.Bitmap.LoadFromResourceName(hInstance, 'APRIL1'); MessageBeep(MB_ICONHAND); end;
               mbsQuestion: begin Picture.Bitmap.LoadFromResourceName(hInstance, 'APRIL1'); MessageBeep(MB_ICONQUESTION); end;
               mbsExclamation: begin Picture.Bitmap.LoadFromResourceName(hInstance, 'APRIL1'); MessageBeep(MB_ICONEXCLAMATION); end;
            end;
          end
          else
          begin
            case FStyle of
               mbsInformation: begin Picture.Bitmap.LoadFromResourceName(hInstance, 'APRIL2'); MessageBeep(MB_ICONINFORMATION); end;
               mbsStop: begin Picture.Bitmap.LoadFromResourceName(hInstance, 'APRIL2'); MessageBeep(MB_ICONHAND); end;
               mbsQuestion: begin Picture.Bitmap.LoadFromResourceName(hInstance, 'APRIL2'); MessageBeep(MB_ICONQUESTION); end;
               mbsExclamation: begin Picture.Bitmap.LoadFromResourceName(hInstance, 'APRIL2'); MessageBeep(MB_ICONEXCLAMATION); end;
            end;
          end;
        end
        else
        begin
          case FStyle of
             mbsInformation: begin Picture.Bitmap.LoadFromResourceName(hInstance, 'INFORMATION'); MessageBeep(MB_ICONINFORMATION); end;
             mbsStop: begin Picture.Bitmap.LoadFromResourceName(hInstance, 'STOP'); MessageBeep(MB_ICONHAND); end;
             mbsQuestion: begin Picture.Bitmap.LoadFromResourceName(hInstance, 'QUESTION'); MessageBeep(MB_ICONQUESTION); end;
             mbsExclamation: begin Picture.Bitmap.LoadFromResourceName(hInstance, 'EXCLAMATION'); MessageBeep(MB_ICONEXCLAMATION); end;
          end;
        end;
     end;
     with TBitBtn.Create(aPanel) do
     begin
        Parent := aPanel;
        Left := 96;
        Top := 109 + TopAdd;
        Width := 90;
        Height := 24;
        case FButtons of
           mbbOk: Visible := False;
           mbbOkCancel: Visible := False;
           mbbYesNo: Visible := False;
           mbbYesNoCancel: Kind := bkYes;
           mbbRetryCancel: Visible := False;
           mbbAbortRetryIgnore: Kind := bkAbort;
        end;
        Default := false;
     end;
     with TBitBtn.Create(aPanel) do
     begin
        Parent := aPanel;
        Left := 194;
        Top := 109 + TopAdd;
        Width := 90;
        Height := 24;
        case FButtons of
           mbbOk: Visible := False;
           mbbOkCancel: Kind := bkOK;
           mbbYesNo: Kind := bkYes;
           mbbYesNoCancel: Kind := bkNo;
           mbbRetryCancel: Kind := bkRetry;
           mbbAbortRetryIgnore: Kind := bkRetry;
        end;
        Default := false;
        case FButtons of mbbOkCancel: Default := not DisableOkFocus; end;
     end;
     with TBitBtn.Create(aPanel) do
     begin
      Parent := aPanel;
        Left := 292;
        Top := 109 + TopAdd;
        Width := 90;
        Height := 24;
        case FButtons of
           mbbOk: Kind := bkOK;
           mbbOkCancel: Kind := bkCancel;
           mbbYesNo: Kind := bkNo;
           mbbYesNoCancel: Kind := bkCancel;
           mbbRetryCancel: Kind := bkCancel;
           mbbAbortRetryIgnore: Kind := bkIgnore;
        end;
        Default := false;
        case FButtons of mbbOK: Default := not DisableOkFocus; end;
     end;
     DLG_MessageBox.Height := DLG_MessageBox.Height + TopAdd;
     if FMoreInfoSL.Text <> '' then
     begin
       with TLabel.Create(aPanel) do
       begin
         Parent := aPanel;
         AutoSize := true;
         Caption := 'Mehr Informationen anzeigen';
         Top := aPanel.ClientHeight - Height - 8;
         Left := 8;
         Transparent := true;
         if Style = mbsStop then
           Font.Color := clWhite
         else
           Font.Color := clHotLight;
         Font.Style := [fsUnderline];
         Cursor := crHandPoint;
         OnClick := MoreInfoClick;
       end;
     end;
     DLG_MessageBox.ActiveControl := aPanel;
     DLG_MessageBox.scaleby ( FScaleX, FScaleY );
     DLG_MessageBox.OnShow := DlgShowEvent;
     Result := DLG_MessageBox.ShowModal;
   finally
     FreeAndNil(DLG_MessageBox);
   end;
end;

procedure HsShowMoreInfoDialog(const sText: string; AOwner: TComponent=nil);
var
  DLG_MoreInfoForm: TDLG_MoreInfoForm;
begin
   DLG_MoreInfoForm := TDLG_MoreInfoForm.Create(AOwner);
   try
     // TODO: Form vergrößerbar machen?
     DLG_MoreInfoForm.BorderStyle := bsSizeable;
     DLG_MoreInfoForm.BorderIcons := [biSystemMenu, biMaximize];
     DLG_MoreInfoForm.ClientHeight := 450;
     DLG_MoreInfoForm.ClientWidth := 725;
     DLG_MoreInfoForm.Position := poMainFormCenter;
     DLG_MoreInfoForm.Caption := 'Weitere Informationen';

     DLG_MoreInfoForm.Memo1 := TMemo.Create(DLG_MoreInfoForm);
     DLG_MoreInfoForm.Memo1.Parent := DLG_MoreInfoForm;
     DLG_MoreInfoForm.Memo1.Align := alClient;
     DLG_MoreInfoForm.Memo1.ReadOnly := true;
     DLG_MoreInfoForm.Memo1.ScrollBars := ssBoth;
     DLG_MoreInfoForm.Memo1.Lines.BeginUpdate;
     DLG_MoreInfoForm.Memo1.Text := sText;
     DLG_MoreInfoForm.Memo1.Lines.EndUpdate;
     DLG_MoreInfoForm.Memo1.Font.Name := 'Courier New';

     DLG_MoreInfoForm.Panel1 := TPanel.Create(DLG_MoreInfoForm);
     DLG_MoreInfoForm.Panel1.Parent := DLG_MoreInfoForm;
     DLG_MoreInfoForm.Panel1.Align := alBottom;
     DLG_MoreInfoForm.Panel1.Height := 30;
     DLG_MoreInfoForm.Panel1.BorderStyle := bsNone;

     DLG_MoreInfoForm.DruckenSpeichern := TButton.Create(DLG_MoreInfoForm);
     DLG_MoreInfoForm.DruckenSpeichern.Parent := DLG_MoreInfoForm.Panel1;
     DLG_MoreInfoForm.DruckenSpeichern.Align := alRight;
     DLG_MoreInfoForm.DruckenSpeichern.Width := 160;
     DLG_MoreInfoForm.DruckenSpeichern.Caption := 'Kopieren in Zwischenablage';
     DLG_MoreInfoForm.DruckenSpeichern.OnClick := DLG_MoreInfoForm.ZwischenablageKopieKlick;

     DLG_MoreInfoForm.DruckenSpeichern := TButton.Create(DLG_MoreInfoForm);
     DLG_MoreInfoForm.DruckenSpeichern.Parent := DLG_MoreInfoForm.Panel1;
     DLG_MoreInfoForm.DruckenSpeichern.Align := alRight;
     DLG_MoreInfoForm.DruckenSpeichern.Width := 130;
     DLG_MoreInfoForm.DruckenSpeichern.Caption := 'Drucken / Speichern';
     DLG_MoreInfoForm.DruckenSpeichern.OnClick := DLG_MoreInfoForm.DruckenSpeichernKlick;

     DLG_MoreInfoForm.DruckenSpeichern := TButton.Create(DLG_MoreInfoForm);
     DLG_MoreInfoForm.DruckenSpeichern.Parent := DLG_MoreInfoForm.Panel1;
     DLG_MoreInfoForm.DruckenSpeichern.Align := alLeft;
     DLG_MoreInfoForm.DruckenSpeichern.Width := 130;
     DLG_MoreInfoForm.DruckenSpeichern.Caption := 'Schließen';
     DLG_MoreInfoForm.DruckenSpeichern.OnClick := DLG_MoreInfoForm.SchliessenKlick;

     DLG_MoreInfoForm.ShowModal;
   finally
     FreeAndNil(DLG_MoreInfoForm);
   end;
end;

procedure TMessageBox.MoreInfoClick(Sender: TObject);
begin
  HsShowMoreInfoDialog(FMoreInfoSL.Text, Self);
end;

constructor TDLG_MoreInfoForm.Create(AOwner: TComponent);
begin
  // TCustomForm.Create können wir nicht verwenden, da der eine Resource laden möchte, wenn es eine TForm-Ableitung ist (und kein unabgeleitetes TForm)
  GlobalNameSpace.BeginWrite;
  try
    CreateNew(AOwner);
  finally
    GlobalNameSpace.EndWrite;
  end;
end;

// Wir können nicht die Funktion von hl.Utils.pas nehmen, da es sonst ein Package-Konflikt gibt
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

// Wir können ChangeFSRedirection() nicht verwenden, da das in hl.Utils ist.
Function Wow64DisableWow64FsRedirection(Var Wow64FsEnableRedirection: LongBool): LongBool; StdCall;
  External 'Kernel32.dll' Name 'Wow64DisableWow64FsRedirection';
Function Wow64EnableWow64FsRedirection(Wow64FsEnableRedirection: LongBool): LongBool; StdCall;
  External 'Kernel32.dll' Name 'Wow64EnableWow64FsRedirection';

procedure TDLG_MoreInfoForm.DruckenSpeichernKlick(Sender: TObject);
var
  LogFile: string;
  Wow64FsEnableRedirection: LongBool;
begin
  LogFile := GetTempDir + 'CORA_WeitereInformationen.txt';
  Memo1.Lines.SaveToFile(LogFile);

  // Ich mache das hier so, weil wir Shellexecute64() in hl.Utils nicht von diesem Package aus verwenden dürfen
  Wow64DisableWow64FsRedirection(Wow64FsEnableRedirection);
  try
    ShellExecute(0, 'open', PChar(LogFile), '', '', SW_NORMAL);
  finally
    Wow64EnableWow64FsRedirection(Wow64FsEnableRedirection);
  end;
end;

procedure TDLG_MoreInfoForm.ZwischenablageKopieKlick(Sender: TObject);
begin
  Clipboard.AsText := Memo1.Lines.Text;
end;

procedure TDLG_MoreInfoForm.SchliessenKlick(Sender: TObject);
begin
  Close;
end;

destructor TMessageBox.Destroy;
begin
  FreeAndNil(FMoreInfoSL);
  
  inherited;
end;

procedure TMessageBox.DlgShowEvent(Sender: TObject);
begin
   Application.Restore;
   Application.BringToFront;
   TForm(Sender).BringToFront;
   // siehe auch MDI_Form_BringToFront in hl.Utils.pas
   if TForm(Sender).WindowState = wsMinimized then
     ShowWindow(TForm(Sender).Handle, SW_RESTORE);
end;

procedure TMessageBox.Close;
begin
  if assigned(DLG_MessageBox) then DLG_MessageBox.Close;
end;

class procedure TMessageBox.ZeigeException(E: Exception);
var
  slCallStack: TStrings;
  Caption: string;
begin
  slCallStack := TStringList.Create;
  try
    slCallStack.Add('');
    slCallStack.Add('Fehlerklasse: ' + e.ClassName);
    slCallStack.Add('Fehlermeldung: ' + e.Message);
    slCallStack.Add('');
    slCallStack.Add('Nachfolgend sehen Sie den Aufruf-Stapel, der einem HickelSOFT-Mitarbeiter');
    slCallStack.Add('beim Lokalisieren des Problems helfen kann:');
    slCallStack.Add('');
    JclLastExceptStackListToStrings(slCallStack, False, True, True, False); // Siehe Beschreibung im "Dokumentation Entwicklung" Verzeichnis
    Caption := 'Ausnahmebehandlung';
    if Assigned(Application) then Caption := Application.Title + ' - ' + Caption;
    HsShowMessage(e.message + #13#10#13#10 + 'Fehlerklasse: ' + e.classname, Caption, mbsStop, mbbOk, slCallStack);
  finally
    FreeAndNil(slCallStack);
  end;
end;

end.
