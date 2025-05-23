unit Drucker;

interface

uses
  Windows, Classes, StdCtrls, Spin, Printers, MessaBox,
  SysUtils, winSpool, IniFiles, Controls, Dialogs, Forms, DB, Registry,
  StrUtils;

type
  TOnAfterPrinterChange = procedure(Sender: TObject) of Object;
  TAfterPrinterDataInit = procedure(Sender: TObject) of Object;
  TEnableDisableEvent = procedure(Sender: TObject; Value: boolean) of Object;
  TKopienModus = (kmDefault, km112233, km123123, kmPfKopie);

{$IF CompilerVersion > 20.0} // Version geraten
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$IFEND}

  TPrinterData = class(TComponent)
  private
    FKopiendrucker: boolean;
    FPrinterIndex: Integer;
    FDrucker: string;
    FName: String;
    FPort: String;
    FDriver: String;
    FStandardIndex: Integer;
    FDruckerListe: TComboBox;
    FBildschirm: boolean;
    FOnAfterPrinterChange: TOnAfterPrinterChange;
    FPaperBin: TComboBox;
    FcbDuplex: TComboBox;
    FAnzahlKopien: TSpinEdit;
    FActive: boolean;
    FAfterPrinterDataInit: TAfterPrinterDataInit;
    LinkedTo: TList;
    FDruckenAdressueberschrift: boolean;
    FDruckenKopf: boolean;
    FKopfseite2Anders: boolean;
    FRandOben: Double;
    FRandUnten: Double;
    FDruckenFuss: boolean;
    FActiveCheckbox: TCheckBox;
    FOnEnableDisable: TEnableDisableEvent;
    procedure SetActiveCheckbox(const Value: TCheckBox);
    function GetAnzahl: Integer;
    procedure EnforceAnyDefaultPrinter;
  protected
    // FDevMode: PDeviceMode;
    // hMode: THandle;
    FBinNumber: TStrings;
    FPrinter: TPrinter;
    pcDriver: pChar;
    pcName: pChar;
    pcPort: pChar;

    mKopien: Integer;
    mPrinterIndex: Integer;
    mDuplexIndex: Integer;
    mSchachtNr: Integer;
    mSchachtIndex: Integer;

    FKopienModus: TKopienModus;

    function GetSchachtNr: Integer;
    function GetSchachtIndex: Integer;
    function GetDuplexIndex: Integer;
    procedure PrinterChange(Sender: TObject);
    procedure SetPrinterIndex(Index: Integer);
    procedure SetDruckerListe(PList: TComboBox);
    procedure SetDuplexListe(Value: TComboBox);
    procedure SetPaperBin(Value: TComboBox);
    procedure SetSpinKopien(PSpin: TSpinEdit);
    procedure SetActive(Value: boolean);
  public
    procedure Loaded; override;
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    property Drucker: String read FDrucker;
    property PrinterName: String read FName;
    property Port: String read FPort;
    property Driver: string read FDriver;
    property Kopiendrucker: boolean read FKopiendrucker write FKopiendrucker;

    property Schachtnr: Integer read GetSchachtNr write mSchachtNr;
    // TODO: Setter, der das VCL ItemIndex �ndert
    property SchachtIndex: Integer read GetSchachtIndex write mSchachtIndex;
    // TODO: Setter, der das VCL ItemIndex �ndert
    property DuplexIndex: Integer read GetDuplexIndex write mDuplexIndex;
    // TODO: Setter, der das VCL ItemIndex �ndert
    property Anzahl: Integer read GetAnzahl;

    function GetDruckerListe: TComboBox;
    function GetPaperBin: TComboBox;
    function GetDuplexListe: TComboBox;
    function GetSpinKopien: TSpinEdit;
    procedure AddLink(Sender: TObject);
    procedure RemoveLink(Sender: TObject);
    procedure SetDefaults;

    class function CachedPrinterList: TStrings;
  published
    property AfterPrinterDataInit: TAfterPrinterDataInit
      read FAfterPrinterDataInit write FAfterPrinterDataInit;
    property OnAfterPrinterChange: TOnAfterPrinterChange
      read FOnAfterPrinterChange write FOnAfterPrinterChange;
    property OnEnableDisable: TEnableDisableEvent read FOnEnableDisable
      write FOnEnableDisable;

    property PrinterIndex: Integer read FPrinterIndex write SetPrinterIndex;
    property StandardIndex: Integer read FStandardIndex;
    property DruckerListe: TComboBox read GetDruckerListe write SetDruckerListe;
    property ZufuhrListe: TComboBox read GetPaperBin write SetPaperBin;
    property DuplexListe: TComboBox read GetDuplexListe write SetDuplexListe;
    property SpinKopien: TSpinEdit read GetSpinKopien write SetSpinKopien;
    property Bildschirm: boolean read FBildschirm write FBildschirm;

    property Active: boolean read FActive write SetActive default True;
    property ActiveCheckbox: TCheckBox read FActiveCheckbox
      write SetActiveCheckbox;

    // TODO: wir sollten das auch mit TCheckBox verkn�pfen
    property DruckenKopf: boolean read FDruckenKopf write FDruckenKopf;
    property DruckenAdressueberschrift: boolean read FDruckenAdressueberschrift
      write FDruckenAdressueberschrift;
    property DruckenFuss: boolean read FDruckenFuss write FDruckenFuss;
    property RandOben: Double read FRandOben write FRandOben;
    property RandUnten: Double read FRandUnten write FRandUnten;
    property Kopfseite2Anders: boolean read FKopfseite2Anders
      write FKopfseite2Anders;

    property KopienModus: TKopienModus read FKopienModus write FKopienModus
      default kmDefault;
  end;

procedure Register;

procedure SetPrinterDataKopien(PDNew: TPrinterData);
procedure SetPrinterData(PDNew: TPrinterData);

resourcestring
  StrBildschirm = 'Bildschirm';

implementation

uses
  DateUtils;

resourcestring
  StrDuplexKein = 'Kein';
  StrDuplexHorizontal = 'Horizontal';
  StrDuplexVertikal = 'Vertikal';

procedure Register;
begin
  RegisterComponents('HS', [TPrinterData]);
end;

// TODO: Das kam auch TCrwReport. Diese Default-Setzerei m�ssen wir dann in den DFM-Formularen irgendwie noch manuell machen
procedure SetPrinterDataKopien(PDNew: TPrinterData);
begin
  with PDNew do
  begin
    Kopiendrucker := True;
    Bildschirm := False;
    if PrinterIndex <> StandardIndex then
      PrinterIndex := StandardIndex;
    if assigned(ActiveCheckbox) then
      ActiveCheckbox.Checked := False;
    if assigned(DruckerListe) then
      DruckerListe.enabled := True;
    if assigned(ZufuhrListe) then
      ZufuhrListe.enabled := True;
    if assigned(DuplexListe) then
      DuplexListe.enabled := True;
    if assigned(SpinKopien) then
      SpinKopien.MinValue := 0;
    if assigned(SpinKopien) then
      SpinKopien.Value := 0;
  end;
end;

// TODO: Das kam auch TCrwReport. Diese Default-Setzerei m�ssen wir dann in den DFM-Formularen irgendwie noch manuell machen
procedure SetPrinterData(PDNew: TPrinterData);
begin
  with PDNew do
  begin
    Kopiendrucker := False;
    Bildschirm := True;
    if PrinterIndex <> StandardIndex then
      PrinterIndex := StandardIndex;
    if assigned(ActiveCheckbox) then
      ActiveCheckbox.Checked := True;
  end;
end;

{$REGION 'TPrinterData'}

procedure TPrinterData.Loaded;
begin
  inherited Loaded;
  Active := True;
  if not(csDesigning in Componentstate) then
  begin
    // �nderung 17.05. Reinhard + Leif: Standardwerte setzen
    if FBildschirm = False then
    begin
      FStandardIndex := Printer.PrinterIndex;
      if PrinterIndex <> Printer.PrinterIndex then
        PrinterIndex := Printer.PrinterIndex;
      FBinNumber.Clear;
    end;
    // �nderung ende

    if assigned(AfterPrinterDataInit) then
      AfterPrinterDataInit(self as TObject);
  end;
end;

threadvar _CachedPrinterList: TStrings;
_PrinterListAlter:
Int64;

class function TPrinterData.CachedPrinterList: TStrings;
begin
  // Umgeleitete Drucker verlangsamen das gewaltig,
  // deshalb versuchen wir die Drucker-Enumeration nicht zu oft hintereinander
  // aufzurufen.

  if DateTimeToUnix(Now) - _PrinterListAlter > 10 then
  begin
    // �lter als 10 Sekunden = neu ziehen (Ticket 54093, Ticket 54270)
    _CachedPrinterList := nil;
    _PrinterListAlter := DateTimeToUnix(Now);
  end;

  if _CachedPrinterList = nil then
    _CachedPrinterList := Printer.Printers;
  result := _CachedPrinterList;
end;

constructor TPrinterData.Create(aOwner: TComponent);
begin
  EnforceAnyDefaultPrinter;

  inherited Create(aOwner);

  LinkedTo := TList.Create;
  pcDriver := AllocMem(512);
  pcPort := AllocMem(512);
  pcName := AllocMem(512);
  // hMode := 0;

  FPrinter := TPrinter.Create;

  if assigned(Application) then
    FPrinter.Title := Application.Title
  else
    FPrinter.Title := ChangeFileExt(ExtractFileName(ParamStr(0)), '');

  FBildschirm := True;

  if not(csDesigning in Componentstate) then
  begin
    try
      FStandardIndex := Printer.PrinterIndex;
    except
      on E: EAbort do
      begin
        Abort;
      end;
      on E: Exception do
      begin
        FStandardIndex := -1;
      end;
    end;
  end;

  FBinNumber := TStringlist.Create;

  if PrinterIndex <> Printer.PrinterIndex then
    PrinterIndex := Printer.PrinterIndex;
end;

destructor TPrinterData.Destroy;
var
  iCounter: LongInt;

begin
  FreeMem(pcDriver);
  FreeMem(pcPort);
  FreeMem(pcName);
  FreeAndNil(FBinNumber);
  FreeAndNil(FPrinter);

  try
    for iCounter := 0 to LinkedTo.Count - 1 do
    begin
      (*
        with TCrwReport(LinkedTo[iCounter]) do
        begin
        if PrinterData = self then
        begin
        bAllowDeleteLink := False;
        PrinterData := nil;
        bAllowDeleteLink := True;
        end;
        if PrinterDataKopien = self then
        begin
        bAllowDeleteLink := False;
        PrinterDataKopien := nil;
        bAllowDeleteLink := True;
        end;
        end;
      *)
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

  FreeAndNil(LinkedTo);
  (*
    if not (csDesigning in ComponentState) then
    if hMode <> 0 then
    begin
    GlobalUnLock(hMode);
    GlobalFree(hMode);
    hMode := 0;
    FDevMode := nil;
    end;
  *)

  inherited Destroy;
end;

procedure TPrinterData.AddLink(Sender: TObject);
begin
  if csDestroying in Componentstate then
    exit;
  // if not (Sender is TCrwReport) then exit;
  if LinkedTo.IndexOf(Sender) = -1 then
    LinkedTo.Add(Sender);
end;

procedure TPrinterData.RemoveLink(Sender: TObject);
begin
  if csDestroying in Componentstate then
    exit;
  // if not (Sender is TCrwReport) then exit;
  if LinkedTo.IndexOf(Sender) <> -1 then
    LinkedTo.Delete(LinkedTo.IndexOf(Sender));
end;

function TPrinterData.GetAnzahl: Integer;
begin
  if assigned(SpinKopien) and SpinKopien.enabled then
    result := SpinKopien.Value
  else
    result := 1;
end;

function TPrinterData.GetDruckerListe: TComboBox;
var
  iCounter: Integer;
  bFound: boolean;

begin
  result := nil;
  if FDruckerListe = nil then
    exit;

  bFound := False;
  for iCounter := 0 to Owner.ComponentCount - 1 do
  begin
    if Owner.Components[iCounter] = FDruckerListe then
    begin
      bFound := True;
      result := FDruckerListe;
      break;
    end;
  end;
  if bFound = False then
    result := nil;
end;

function TPrinterData.GetSpinKopien: TSpinEdit;
var
  iCounter: Integer;
  bFound: boolean;

begin
  result := nil;
  if FAnzahlKopien = nil then
  begin
    result := nil;
    exit;
  end;
  bFound := False;
  for iCounter := 0 to Owner.ComponentCount - 1 do
  begin
    if Owner.Components[iCounter] = FAnzahlKopien then
    begin
      bFound := True;
      result := FAnzahlKopien;
      break;
    end;
  end;
  if bFound = False then
    result := nil;
end;

function TPrinterData.GetDuplexListe: TComboBox;
var
  iCounter: Integer;
  bFound: boolean;

begin
  result := nil;
  if FcbDuplex = nil then
  begin
    result := nil;
    exit;
  end;
  bFound := False;
  for iCounter := 0 to Owner.ComponentCount - 1 do
  begin
    if Owner.Components[iCounter] = FcbDuplex then
    begin
      bFound := True;
      result := FcbDuplex;
      break;
    end;
  end;
  if bFound = False then
    result := nil;
end;

function TPrinterData.GetPaperBin: TComboBox;
var
  iCounter: Integer;
  bFound: boolean;

begin
  result := nil;
  if FPaperBin = nil then
  begin
    result := nil;
    exit;
  end;
  bFound := False;
  for iCounter := 0 to Owner.ComponentCount - 1 do
  begin
    if Owner.Components[iCounter] = FPaperBin then
    begin
      bFound := True;
      result := FPaperBin;
      break;
    end;
  end;
  if bFound = False then
    result := nil;
end;

procedure TPrinterData.SetActive(Value: boolean);
begin
  if not(assigned(FDruckerListe)) and // TODO: or?
    not(assigned(FPaperBin)) and // TODO: or?
    not(assigned(FcbDuplex)) and // TODO: or?
    not(assigned(FAnzahlKopien)) then
  begin
    FActive := False;
    exit;
  end
  else
    FActive := Value;

  if assigned(FDruckerListe) then
    FDruckerListe.Visible := True;
  if assigned(FPaperBin) then
    FPaperBin.Visible := True;
  if assigned(FcbDuplex) then
    FcbDuplex.Visible := True;
  if assigned(FAnzahlKopien) then
    FAnzahlKopien.Visible := True;

  if assigned(FActiveCheckbox) then
    FActiveCheckbox.Checked := FActive;
  if assigned(FDruckerListe) then
    FDruckerListe.enabled := FActive;
  if assigned(FPaperBin) then
    FPaperBin.enabled := FActive and (PrinterIndex >= 0);
  if assigned(FcbDuplex) then
    FcbDuplex.enabled := FActive and (PrinterIndex >= 0);
  if assigned(FAnzahlKopien) then
    FAnzahlKopien.enabled := FActive and (PrinterIndex >= 0);

  if assigned(FOnEnableDisable) then
    FOnEnableDisable(self, Value);
end;

procedure TPrinterData.SetActiveCheckbox(const Value: TCheckBox);
begin
  FActiveCheckbox := Value;
  if FActiveCheckbox <> nil then
    FActiveCheckbox.Checked := FActive;
end;

function TPrinterData.GetSchachtNr: Integer;
begin
  result := -1;

  if assigned(FPaperBin) and FPaperBin.enabled then
  begin
    if FPaperBin.ItemIndex >= 0 then
      result := StrToInt(FBinNumber[FPaperBin.ItemIndex])
  end
  else
  begin
    if (mSchachtIndex >= 0) and (mSchachtIndex < FBinNumber.Count) then
      result := StrToInt(FBinNumber[mSchachtIndex]);
  end;
end;

function TPrinterData.GetSchachtIndex: Integer;
begin
  result := -1;

  if assigned(FPaperBin) and FPaperBin.enabled then
  begin
    if FPaperBin.ItemIndex >= 0 then
      result := FPaperBin.ItemIndex
  end
  else
  begin
    if mSchachtIndex >= 0 then
      result := mSchachtIndex;
  end;
end;

function TPrinterData.GetDuplexIndex: Integer;
begin
  result := -1;

  if assigned(DuplexListe) and DuplexListe.enabled then
  begin
    if DuplexListe.ItemIndex >= 0 then
      result := DuplexListe.ItemIndex
  end
  else
  begin
    if mDuplexIndex >= 0 then
      result := mDuplexIndex;
  end;
end;

procedure TPrinterData.SetPaperBin(Value: TComboBox);
begin
  if FPaperBin = Value then
    exit;
  FPaperBin := Value;
  if Value = nil then
    exit;
  FPaperBin.ItemIndex := -1;
  FPaperBin.enabled := False;
end;

procedure TPrinterData.SetDuplexListe(Value: TComboBox);
begin
  if Value = FcbDuplex then
    exit;
  FcbDuplex := Value;
  if Value = nil then
    exit;
  with FcbDuplex.Items do
  begin
    Clear;
    Add(StrDuplexKein);
    Add(StrDuplexHorizontal);
    Add(StrDuplexVertikal);
  end;
  FcbDuplex.ItemIndex := -1;
  FcbDuplex.enabled := False;
end;

procedure TPrinterData.SetSpinKopien(PSpin: TSpinEdit);
begin
  if FAnzahlKopien = PSpin then
    exit;
  FAnzahlKopien := PSpin;
  if PSpin = nil then
    exit;
  FAnzahlKopien.MaxValue := 999;

  if not Kopiendrucker then
  begin
    FAnzahlKopien.MinValue := 0;
    FAnzahlKopien.Value := 1;
  end
  else
  begin
    FAnzahlKopien.MinValue := 0; // was soll das?!
    FAnzahlKopien.Value := 1;
  end;
  FAnzahlKopien.enabled := False;
end;

procedure TPrinterData.PrinterChange(Sender: TObject);
begin
  if FBildschirm = True then
  begin
    if PrinterIndex <> FDruckerListe.ItemIndex - 1 then
      PrinterIndex := FDruckerListe.ItemIndex - 1;
  end
  else
  begin
    if PrinterIndex <> FDruckerListe.ItemIndex then
      PrinterIndex := FDruckerListe.ItemIndex;
  end;
end;

procedure TPrinterData.SetDefaults;
var
  istBonFormat: boolean;
begin
  // siehe auch C# Quelltext CoraDruckAuftrag.cs:GetDruckEinstellungen()

  if Kopiendrucker then
  begin
    if Printer.Printers.Count = 0 then
    begin
      if PrinterIndex <> -1 then
        PrinterIndex := -1;
    end
    else
    begin
      if PrinterIndex <> 0 then
        PrinterIndex := 0;
    end;
  end
  else
  begin
    if PrinterIndex <> -1 then
      PrinterIndex := -1; // Bildschirm
  end;

  SchachtIndex := -1;
  if (ZufuhrListe <> nil) then
    ZufuhrListe.ItemIndex := -1;
  DuplexIndex := -1;
  if (DuplexListe <> nil) then
    DuplexListe.ItemIndex := -1;
  if (SpinKopien <> nil) then
  begin
    if Kopiendrucker then
      SpinKopien.Value := 0
    else
      SpinKopien.Value := 1;
  end;

  // istBonFormat := (formularArt = 'Kasse') or (formularArt = 'Kasse-Kett') or ((formularArt = 'LM-Belege') and (formularNr = 2));
  istBonFormat := Pos('kasse', LowerCase(ExtractFileName(ParamStr(0)))) > 0;

  DruckenKopf := True;
  DruckenAdressueberschrift := True;
  DruckenFuss := True;
  Kopfseite2Anders := False;
  if istBonFormat then
    RandOben := 0
  else
    RandOben := 2;
  if istBonFormat then
    RandUnten := 0
  else
    RandUnten := 2;

  Active := not Kopiendrucker;
end;

procedure TPrinterData.SetDruckerListe(PList: TComboBox);
begin
  FDruckerListe := PList;
  if FDruckerListe = nil then
    exit;

  if (csDesigning in Componentstate) then
    exit;

  with FDruckerListe do
  begin
    style := csDropDownList;

    if not(csDesigning in Componentstate) then
    begin
      Items.Assign(TPrinterData.CachedPrinterList);
      if FBildschirm = True then
      begin
        Items.Insert(0, StrBildschirm);
        if FPrinterIndex <> -1 then
          FPrinterIndex := -1;
      end
      else
      begin
        if FPrinterIndex <> FStandardIndex then
          FPrinterIndex := FStandardIndex;
      end;
    end;
    ItemIndex := 0;
    if assigned(Onchange) then
      FOnAfterPrinterChange := Onchange;
    Onchange := PrinterChange;
  end;
end;

procedure TPrinterData.SetPrinterIndex(Index: Integer);
var
  pcBuffer: pChar;
  wbuffer, wbuffer1: pWord;
  dwBuffer, iCounter: Integer;
{$IF CompilerVersion >= 20.0}
  // StubDevMode: PDeviceMode;
{$ELSE}
  // StubDevMode: TDeviceMode;
  // FPrinterHandle: THandle;
{$IFEND}
  hMode: THandle;
  FDevMode: PDeviceMode;
const
  Hs_Max_Bins = 1000; // durch uns festgelegte Obergrenze
begin
  if FPrinter = nil then
    exit;
  if Index < 0 then // Bildschirm
  begin
    FDrucker := TPrinterData.CachedPrinterList[FStandardIndex];
    if FPrinter.PrinterIndex <> FStandardIndex then
      FPrinter.PrinterIndex := FStandardIndex;
  end
  else // Drucker
  begin
    if index < TPrinterData.CachedPrinterList.Count then
    begin
      if FPrinter.PrinterIndex <> Index then
        FPrinter.PrinterIndex := Index;
      FDrucker := TPrinterData.CachedPrinterList[Index];
    end;
  end;
  if FPrinterIndex <> Index then
    FPrinterIndex := Index;


  // Dieser Code tut aus irgendeinem Grund ein Heap Alloc leaken!!! (App Verifier)
  (*

    // DEvmode
    hModeOrig := 0;
    FPrinter.GetPrinter( pcName, pcDriver, pcPort, hModeOrig );
    // Mit hModeOrig nix machen, denn es wird bei SetPrinter freigegeben!!!

    {$IFOPT R+}
    {$DEFINE RANGEON}
    {$R-}
    {$ELSE}
    {$UNDEF RANGEON}
    {$ENDIF}

    hModeNeu := 0;
    FDevMode := nil;
    if OpenPrinter(pcName, FPrinterHandle, nil) then
    begin
    // alloc new device mode block if one was not passed in
    {$IF CompilerVersion >= 20.0}
    // siehe https://stackoverflow.com/questions/37382720/documentproperties-fails-in-xe6-works-in-delphi-7
    StubDevMode := nil;
    hModeNeu := GlobalAlloc(GHND, DocumentProperties(0, FPrinterHandle, pcName, PDeviceMode(0), StubDevMode, 0));
    {$ELSE}
    hModeNeu := GlobalAlloc(GHND, DocumentProperties(0, FPrinterHandle, pcName, StubDevMode, StubDevMode, 0));
    {$IFEND}
    if hModeNeu <> 0 then
    begin
    FDevMode := GlobalLock(hModeNeu);
    if FDevMode = nil then RaiseLastOSError;
    FDevMode.dmSize := sizeof(DEVMODE);
    if DocumentProperties(0, FPrinterHandle, pcName, FDevMode^, FDevMode^, DM_OUT_BUFFER) < 0 then
    begin
    GlobalUnlock(hModeNeu);
    GlobalFree(hModeNeu);
    hModeNeu := 0;
    end
    else
    begin
    // In SetPrinter, GlobalLock wird aufgerufen
    GlobalUnlock(hModeNeu);
    end;
    end;
    ClosePrinter(FPrinterHandle);
    end;

    {$IFDEF RANGEON}
    {$R+}
    {$UNDEF RANGEON}
    {$ENDIF}

    FPrinter.SetPrinter( pcName, pcDriver, pcPort, hModeNeu ); // Muss bleiben, weil sonst das TPrinter-Objekt nicht den richtigen DevMode besitzt!
    hModeNeu := 0; // Ist ab sofort im Verantwortungsbereich von TPrinter und wird mit dessen Zerst�rung freigegeben!

  *)


  // Das ist der neue Code:

  (*
    // Ist das richtig? Denn hMode geh�rt doch zu FPrinter?!
    if hMode <> 0 then
    begin
    GlobalUnlock(hMode);
    GlobalFree(hMode);
    hMode := 0;
    FDevMode := nil;
    end;

    FPrinter.GetPrinter( pcName, pcDriver, pcPort, hMode );

    // Delphi macht das aus irgendeinem Grund in SetPrinterIndex, aber das ist doch quatsch,
    // weil SetPrinter macht doch bereits GlobalFree, also w�re es ein Double-Free!
    // (Bugreport bei Embarcadero er�ffnen?)
    GlobalUnlock(hMode);
    GlobalFree(hMode);
    hMode := 0;

    // SetPrinter wird unter anderem einen neuen DevMode erzeugen, wenn wir einen DevMode von 0 liefern.
    // (Wenn wir einen eigenen DevMode liefern w�rden, w�rde er seinen eigenen freigeben und unseren nehmen)
    FPrinter.SetPrinter( pcName, pcDriver, pcPort, hMode );

    // Nun �ber GetPrinter den dort erzeugten DevMode holen, damit wir ihn weiter unten verwenden k�nnen
    FPrinter.GetPrinter( pcName, pcDriver, pcPort, hMode );
    FDevMode := GlobalLock(hMode);
  *)


  // Hier nochmal ein ganz anderer Code...

  if FPrinter.PrinterIndex <> Index then
    FPrinter.PrinterIndex := Index;
  // FPrinterIndex := FPrinter.PrinterIndex;  // <-- nein, wir lassen kein "Feedback" zu. "-1" bleibt bei uns "-1"

  FPrinter.GetPrinter(pcName, pcDriver, pcPort, hMode);

  if (hMode = 0) then
  begin
    FPrinter.SetPrinter(pcName, pcDriver, pcPort, hMode);
    FPrinter.GetPrinter(pcName, pcDriver, pcPort, hMode);
  end;

  FDevMode := GlobalLock(hMode);
  try

    // Und jetzt geht's weiter.

    if (Index >= 0) then
    begin
      if not(csDesigning in Componentstate) then
      begin // DevMode
        if assigned(FBinNumber) then
        begin
          FBinNumber.Clear;
          wbuffer := AllocMem(Hs_Max_Bins * 2);
          try
            // DC_BINS: Retrieves a list of available paper bins. The pOutput buffer receives an array of WORD values that indicate the available paper sources for the printer. The return value indicates the number of entries in the array. For a list of the possible array values, see the description of the dmDefaultSource member of the DEVMODE structure. If pOutput is NULL, the return value indicates the required number of entries in the array.
            dwBuffer := DeviceCapabilities(pcName, pcPort, DC_BINS,
              pChar(wbuffer), nil);
            wbuffer1 := wbuffer;
            for iCounter := 0 to dwBuffer - 1 do
            begin
              FBinNumber.Add(IntToStr(wbuffer1^));
              Inc(pWord(wbuffer1), 1); // inkrement um 1 WORD (also 2 byte)
            end;
          finally
            FreeMem(wbuffer);
          end;
        end;

        if (FDevMode <> nil) and (FPaperBin <> NIL) then
        begin
          if (FDevMode.dmFields and DM_DEFAULTSOURCE = 0) then
          begin
            FPaperBin.enabled := False;
          end
          else
          begin
            FPaperBin.enabled := True;
            FPaperBin.Clear;
            pcBuffer := AllocMem(Hs_Max_Bins * 24);
            try
              // DC_BINNAMES: 	Retrieves the names of the printer's paper bins. The pOutput buffer receives an array of string buffers. Each string buffer is 24 characters long and contains the name of a paper bin. The return value indicates the number of entries in the array. The name strings are null-terminated unless the name is 24 characters long. If pOutput is NULL, the return value is the number of bin entries required.
              dwBuffer := DeviceCapabilities(pcName, pcPort, DC_BINNAMES,
                pcBuffer, FDevMode);
              for iCounter := 0 to dwBuffer - 1 do
              begin
                FPaperBin.Items.Add(strpas(pcBuffer + iCounter * 24));
              end;

              try
                FPaperBin.ItemIndex := -1;
                for iCounter := 0 to FBinNumber.Count - 1 do
                  if StrToInt(FBinNumber[iCounter]) = FDevMode.dmDefaultSource
                  then
                    FPaperBin.ItemIndex := iCounter;
                if FPaperBin.ItemIndex = -1 then
                  FPaperBin.ItemIndex := 0;
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
            finally
              FreeMem(pcBuffer);
            end;
          end;
        end;
      end;

      if FcbDuplex <> nil then
      begin
        FcbDuplex.enabled := True;
        if (FDevMode = nil) or (FDevMode.dmFields and DM_DUPLEX = 0) then
        begin
          FcbDuplex.enabled := False;
          FcbDuplex.ItemIndex := 0;
        end
        else
        begin
          FcbDuplex.enabled := True;
          if FDevMode.dmDuplex = DMDUP_SIMPLEX then
            FcbDuplex.ItemIndex := 0;
          if FDevMode.dmDuplex = DMDUP_HORIZONTAL then
            FcbDuplex.ItemIndex := 1;
          if FDevMode.dmDuplex = DMDUP_VERTICAL then
            FcbDuplex.ItemIndex := 2;
        end;
      end;

      if FAnzahlKopien <> nil then
      begin
        FAnzahlKopien.enabled := True;
        // fanzahlkopien.value := 0;
        // fanzahlkopien.value := FDevMode.dmcopies;
        try
          FAnzahlKopien.Value := FPrinter.Copies;
        except
          on E: EAbort do
          begin
            Abort;
          end;
          on E: Exception do
          begin
            // TODO: Ticket 33167 - wenn der Drucker im Netzwerk nicht verf�gbar (gesperrt oder nicht mehr freigegeben ist), dann verursacht das Weitermachen einen kaputten Stack
            // Unbedingt Meldung EPrinter-Exception "Operation auf ausgew�hltem Drucker nicht verf�gbar." beachten
            FAnzahlKopien.Value := 1;
          end;
        end;
      end;
    end
    else
    begin
      if FAnzahlKopien <> nil then
      begin
        FAnzahlKopien.Value := 1;
        FAnzahlKopien.enabled := False;
      end;
      if FcbDuplex <> nil then
      begin
        FcbDuplex.ItemIndex := 0;
        FcbDuplex.enabled := False;
      end;
      if FPaperBin <> nil then
      begin
        FPaperBin.ItemIndex := -1;
        FPaperBin.enabled := False;
      end;
    end;
    FName := strpas(pcName);
    FDriver := strpas(pcDriver);
    FPort := strpas(pcPort);
    with TiniFile.Create('Win.Ini') do
    begin
      FDriver := ReadString('devices', FName, '');
      Free;
    end;
    FDriver := copy(FDriver, 1, Pos(',', FDriver) - 1);
    StrPCopy(pcDriver, FDriver);
    if assigned(FDruckerListe) then
    begin
      // OnChange muss abgeschaltet werden, um Rekursionen zu vermeiden!
      if not(csDesigning in Componentstate) then
        FDruckerListe.Onchange := nil;
      if FBildschirm = True then
        FDruckerListe.ItemIndex := FPrinterIndex + 1
      else
        FDruckerListe.ItemIndex := FPrinterIndex;
      if not(csDesigning in Componentstate) then
        FDruckerListe.Onchange := PrinterChange;
    end;
    // Falls vorhanden, OnChange der ComboBox aufrufen
    if assigned(FOnAfterPrinterChange) then
      FOnAfterPrinterChange(self);
  finally
    if FDevMode <> nil then
      GlobalUnlock(hMode);
  end;
end;

procedure TPrinterData.EnforceAnyDefaultPrinter;
// Das ist erforderlich, um SNoDefaultPrinter zu verhindern
// Reproduzieren durch �ndern von Wert "Device" in einen ung�ltigen Wert bei
// HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Windows

  procedure GetPrinterList(List: TStrings);
  var
    Buffer, PrinterInfo: pChar;
    Flags, Count, NumInfo: DWORD;
    i: Integer;
    Level: Byte;
  begin
    List.Clear;
    if Win32Platform = VER_PLATFORM_WIN32_NT then
    begin
      Flags := PRINTER_ENUM_CONNECTIONS or PRINTER_ENUM_LOCAL;
      Level := 4;
    end
    else
    begin
      Flags := PRINTER_ENUM_LOCAL;
      Level := 5;
    end;
    Count := 0;
    EnumPrinters(Flags, nil, Level, nil, 0, Count, NumInfo);
    if Count > 0 then
    begin
      GetMem(Buffer, Count);
      try
        if not EnumPrinters(Flags, nil, Level, PByte(Buffer), Count, Count,
          NumInfo) then
          exit;
        PrinterInfo := Buffer;
        for i := 0 to NumInfo - 1 do
        begin
          if Level = 4 then
          begin
            List.Add(PPrinterInfo4(PrinterInfo)^.pPrinterName);
            Inc(PrinterInfo, SizeOf(TPrinterInfo4));
          end
          else
          begin
            List.Add(PPrinterInfo5(PrinterInfo)^.pPrinterName);
            Inc(PrinterInfo, SizeOf(TPrinterInfo5));
          end;
        end;
      finally
        FreeMem(Buffer, Count);
      end;
    end;
  end;

  function SetDefaultPrinter(aPrinterName: string): boolean;
  const
    WM_SETTINGCHANGE = $001A;
  type
    TSetDefaultPrinter = function(prnName: LPCTSTR): BOOL; stdcall;
  var
    WS: HINST;
    SetDefaultPrinter: TSetDefaultPrinter;
  begin
    WS := GetModuleHandle(pChar(winspl));
{$IFDEF Unicode}
    SetDefaultPrinter := GetProcAddress(WS, 'SetDefaultPrinterW');
{$ELSE}
    SetDefaultPrinter := GetProcAddress(WS, 'SetDefaultPrinterA');
{$ENDIF}
    if assigned(SetDefaultPrinter) then
    begin
      result := SetDefaultPrinter(pChar(aPrinterName));
      if result then
        SendNotifyMessage(HWND_BROADCAST, WM_SETTINGCHANGE, 0, 0);
    end
    else
    begin
      result := False;
    end;
  end;

var
  sl: TStringlist;
begin
  try
    if Printer.PrinterIndex <> -1 then
      Printer.PrinterIndex := -1;
  except
    on E: EAbort do
    begin
      Abort;
    end;
    on E: EPrinter do
    begin
      sl := TStringlist.Create;
      try
        GetPrinterList(sl);
        if sl.Count > 0 then
          SetDefaultPrinter(sl.Strings[0])
        else
          SetDefaultPrinter('');
      finally
        FreeAndNil(sl);
      end;
    end;
    on E: Exception do
    begin
      raise;
    end;
  end;
end;

{$ENDREGION}

end.
