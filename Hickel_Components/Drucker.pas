unit Drucker;

interface

uses
  HsTools, Windows, Classes, StdCtrls, Spin, Printers, MessaBox,
  SysUtils, winSpool, IniFiles, Controls, Dialogs, Forms, DB, Registry, StrUtils;

type
  TOnAfterPrinterChange = procedure( Sender: TObject ) of Object;
  TAfterPrinterDataInit = procedure ( Sender: TObject ) of Object;
  TEnableDisableEvent = procedure ( Sender: TObject; Value: boolean ) of Object;
  TKopienModus = (kmDefault, km112233, km123123, kmPfKopie);

  {$IF CompilerVersion > 20.0} // Version geraten
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$IFEND}
  TPrinterData = class( TComponent )
  private
      FKopiendrucker: Boolean;
      FPrinterIndex: Integer;
      FDrucker: string;
      FName: String;
      FPort: String;
      FDriver: String;
      FStandardIndex: Integer;
      FDruckerListe: TComboBox;
      FBildschirm: Boolean;
      FOnAfterPrinterChange: TOnAfterPrinterChange;
      FPaperBin: TComboBox;
      FcbDuplex: TComboBox;
      FAnzahlKopien: TSpinEdit;
      FActive: Boolean;
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
      function GetAnzahl: integer;
      procedure EnforceAnyDefaultPrinter;
  protected
      function GetSchachtNr : Integer;
      function GetSchachtIndex : Integer;
      function GetDuplexIndex : Integer;
      procedure PrinterChange     ( Sender: TObject );
      procedure SetPrinterIndex   ( Index: Integer );
      procedure SetDruckerListe   ( PList: TComboBox );
      procedure SetDuplexListe    ( Value: TComboBox );
      procedure SetPaperBin       ( Value: TComboBox );
      procedure SetSpinKopien     ( PSpin: TSpinEdit );
      procedure SetActive         ( Value: Boolean );
  public
      //FDevMode: PDeviceMode;
      //hMode: THandle;
      FBinNumber: TStrings;
      FPrinter : TPrinter;
      pcDriver: pChar;
      pcName: pChar;
      pcPort: pChar;

      mKopien: integer;
      mPrinterIndex: integer;
      mDuplexIndex: integer;
      mSchachtNr: integer;
      mSchachtIndex: integer;

      FKopienModus: TKopienModus;

      procedure Loaded; override;
      constructor Create( aOwner: TComponent ); override;
      destructor Destroy; override;
      property Drucker: String read FDrucker;
      property PrinterName: String read FName;
      property Port: String read FPort;
      property Driver: string read FDriver;
      property Kopiendrucker: Boolean read FKopiendrucker write FKopiendrucker;

      property Schachtnr: Integer read GetSchachtNr write mSchachtNr; // TODO: Setter, der das VCL ItemIndex ändert
      property SchachtIndex: Integer read GetSchachtIndex write mSchachtIndex; // TODO: Setter, der das VCL ItemIndex ändert
      property DuplexIndex: Integer read GetDuplexIndex write mDuplexIndex; // TODO: Setter, der das VCL ItemIndex ändert
      property Anzahl: integer read GetAnzahl;

      function GetDruckerListe: TComboBox;
      function GetPaperBin: TComboBox;
      function GetDuplexListe: tcombobox;
      function GetSpinKopien: tspinedit;
      procedure AddLink(Sender: TObject);
      procedure RemoveLink(Sender: TObject);
      procedure SetDefaults;

      class function CachedPrinterList: TStrings;
  published
      property AfterPrinterDataInit: TAfterPrinterDataInit     read FAfterPrinterDataInit write FAfterPrinterDataInit;
      property OnAfterPrinterChange: TOnAfterPrinterChange read FOnAfterPrinterChange write FOnAfterPrinterChange;
      property OnEnableDisable: TEnableDisableEvent read FOnEnableDisable write FOnEnableDisable;

      property PrinterIndex: Integer    read FPrinterIndex   write SetPrinterIndex;
      property StandardIndex: Integer   read FStandardIndex;
      property DruckerListe: TComboBox  read GetDruckerliste   write SetDruckerListe;
      property ZufuhrListe:  TComboBox  read GetPaperBin       write SetPaperBin;
      property DuplexListe:  TComboBox  read GetDuplexListe       write SetDuplexListe;
      property SpinKopien:   TSpinEdit  read GetSpinKopien   write SetSpinKopien;
      property Bildschirm:   Boolean    read FBildschirm     write FBildschirm;

      property Active : Boolean         read FActive         write SetActive default True;
      property ActiveCheckbox: TCheckBox read FActiveCheckbox write SetActiveCheckbox;

      // TODO: wir sollten das auch mit TCheckBox verknüpfen
      property DruckenKopf: boolean read FDruckenKopf write FDruckenKopf;
      property DruckenAdressueberschrift: boolean read FDruckenAdressueberschrift write FDruckenAdressueberschrift;
      property DruckenFuss: boolean read FDruckenFuss write FDruckenFuss;
      property RandOben: Double read FRandOben write FRandOben;
      property RandUnten: Double read FRandUnten write FRandUnten;
      property Kopfseite2Anders: boolean read FKopfseite2Anders write FKopfseite2Anders;

      property KopienModus: TKopienModus read FKopienModus write FKopienModus default kmDefault;
  end;

procedure Register;

procedure SetPrinterDataKopien ( PDNew: TPrinterdata );
procedure SetPrinterData ( PDNew: TPrinterdata );

implementation

uses
  DateUtils;

procedure Register;
begin
  RegisterComponents('HS', [TPrinterData]);
end;

// TODO: Das kam auch TCrwReport. Diese Default-Setzerei müssen wir dann in den DFM-Formularen irgendwie noch manuell machen
procedure SetPrinterDataKopien ( PDNew: TPrinterdata );
begin
   with PDNew do
   begin
      KopienDrucker := True;
      Bildschirm := False;
      Printerindex := StandardIndex;
      if assigned (ActiveCheckbox) then ActiveCheckbox.Checked := False;
      if assigned( Druckerliste ) then Druckerliste.enabled := True;
      if assigned( Zufuhrliste ) then Zufuhrliste.Enabled := True;
      if assigned( Duplexliste ) then DuplexListe.Enabled := True;
      if assigned( SpinKopien ) then SpinKopien.MinValue := 0;
      if assigned( SpinKopien ) then Spinkopien.Value := 0;
   end;
end;

// TODO: Das kam auch TCrwReport. Diese Default-Setzerei müssen wir dann in den DFM-Formularen irgendwie noch manuell machen
procedure SetPrinterData ( PDNew: TPrinterdata );
begin
   with PDNew do
   begin
      KopienDrucker := False;
      Bildschirm := True;
      Printerindex := StandardIndex;
      if assigned (ActiveCheckbox) then ActiveCheckbox.Checked := True;
   end;
end;

{$REGION 'TPrinterData'}
procedure TPrinterData.Loaded;
begin
   inherited Loaded;
   Active := True;
   if not (csDesigning in Componentstate) then
   begin
      // Änderung 17.05. Reinhard + Leif: Standardwerte setzen
      if FBildschirm = false then
      begin
         FStandardIndex := Printer.PrinterIndex;
         PrinterIndex := Printer.PrinterIndex;
         FBinNumber.Clear;
      end;
      // Änderung ende

      if assigned (AfterPrinterDataInit) then
         AfterPrinterDataInit (self as TObject);
   end;
end;

threadvar
  _CachedPrinterList: TStrings;
  _PrinterListAlter: Int64;
class function TPrinterData.CachedPrinterList: TStrings;
begin
  // Umgeleitete Drucker verlangsamen das gewaltig,
  // deshalb versuchen wir die Drucker-Enumeration nicht zu oft hintereinander
  // aufzurufen.

  if DateTimeToUnix(Now)-_PrinterListAlter > 10 then
  begin
    // Älter als 10 Sekunden = neu ziehen (Ticket 54093, Ticket 54270)
    _CachedPrinterList := nil;
    _PrinterListAlter := DateTimeToUnix(Now);
  end;

  if _CachedPrinterList = nil then
    _CachedPrinterList := Printer.Printers;
  result := _CachedPrinterList;
end;

constructor TPrinterData.Create( aOwner: TComponent );
begin
  EnforceAnyDefaultPrinter;

  inherited Create( aOwner );

  LinkedTo := TList.Create;
  pcDriver := AllocMem( 512 );
  pcPort := AllocMem( 512 );
  pcName := AllocMem( 512 );
  //hMode := 0;

  FPrinter := TPrinter.Create;

  if Assigned(Application) then
    FPrinter.Title := Application.Title
  else
    FPrinter.Title := ChangeFileExt(ExtractFileName(ParamStr(0)),'');

  FBildschirm := True;

  if not (csDesigning in Componentstate) then
  begin
    try
      FStandardIndex := Printer.PrinterIndex;
    except
      FStandardIndex := -1;
    end;
  end;

  FBinNumber := TStringlist.Create;

  PrinterIndex := Printer.PrinterIndex;
end;
destructor TPrinterData.Destroy;
var
   iCounter: LongInt;

begin
  FreeMem( pcDriver );
  FreeMem( pcPort );
  FreeMem( pcName );
  FreeAndNil(FBinNumber);
  FreeAndNil(FPrinter);

   try
    for iCounter := 0 to LinkedTo.Count-1 do
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
procedure TPrinterData.AddLink( Sender: TObject );
begin
   if csDestroying in ComponentState then exit;
   //if not (Sender is TCrwReport) then exit;
   if LinkedTo.IndexOf( Sender ) = -1 then LinkedTo.Add( Sender );
end;

procedure TPrinterData.RemoveLink( Sender: TObject );
begin
   if csDestroying in ComponentState then exit;
   //if not (Sender is TCrwReport) then exit;
   if LinkedTo.IndexOf( Sender ) <> -1 then LinkedTo.Delete( LinkedTo.IndexOf( Sender ) );
end;
function TPrinterData.GetAnzahl: integer;
begin
  if Assigned(SpinKopien) and SpinKopien.Enabled then
    result := SpinKopien.Value
  else
    result := 1;
end;

function TPrinterData.GetDruckerListe: TComboBox;
var
   iCounter: integer;
   bFound: boolean;

begin
   result := nil;
   if FDruckerListe = nil then exit;

   bFound := False;
   for iCounter := 0 to Owner.ComponentCount-1 do
   begin
      if Owner.Components[iCounter] = FDruckerListe then
      begin
         bFound := True;
         Result := FDruckerListe;
         break;
      end;
   end;
   if bFound = False then Result := nil;
end;
function TPrinterData.GetSpinKopien: TSpinEdit;
var
   iCounter: integer;
   bFound: boolean;

begin
   result := nil;
   if FAnzahlKopien = nil then
   begin
      result := nil;
      exit;
   end;
   bFound := False;
   for iCounter := 0 to Owner.ComponentCount-1 do
   begin
      if Owner.Components[iCounter] = FAnzahlKopien then
      begin
         bFound := True;
         Result := FAnzahlKopien;
         break;
      end;
   end;
   if bFound = False then Result := nil;
end;
function TPrinterData.GetDuplexListe: TComboBox;
var
   iCounter: integer;
   bFound: boolean;

begin
   result := nil;
   if FcbDuplex = nil then
   begin
      result := nil;
      exit;
   end;
   bFound := False;
   for iCounter := 0 to Owner.ComponentCount-1 do
   begin
      if Owner.Components[iCounter] = FcbDuplex then
      begin
         bFound := True;
         Result := FcbDuplex;
         break;
      end;
   end;
   if bFound = False then Result := nil;
end;
function TPrinterData.GetPaperBin: TComboBox;
var
   iCounter: integer;
   bFound: boolean;

begin
   result := nil;
   if FPaperBin = nil then
   begin
      result := nil;
      exit;
   end;
   bFound := False;
   for iCounter := 0 to Owner.ComponentCount-1 do
   begin
      if Owner.Components[iCounter] = FPaperBin then
      begin
         bFound := True;
         Result := FPaperBin;
         break;
      end;
   end;
   if bFound = False then Result := nil;
end;
procedure TPrinterData.SetActive(Value: Boolean);
begin
  if not ( assigned (FDruckerliste)) and  // TODO: or?
     not ( assigned (FPaperbin)) and      // TODO: or?
     not ( assigned (FcbDuplex)) and      // TODO: or?
     not ( assigned (FAnzahlKopien)) then
  begin
    FActive := False;
    exit;
  end
  else FActive := Value;

  if assigned (FDruckerliste) then FDruckerliste.Visible := True;
  if assigned (FPaperbin)     then FPaperbin.Visible := True;
  if assigned (FcbDuplex)     then FcbDuplex.Visible := True;
  if assigned (FAnzahlKopien) then FAnzahlKopien.Visible := True;

  if assigned (FActiveCheckbox) then FActiveCheckbox.Checked := FActive;
  if assigned (FDruckerliste) then FDruckerliste.Enabled := FActive;
  if assigned (FPaperbin)     then FPaperbin.Enabled := FActive and (PrinterIndex >= 0);
  if assigned (FcbDuplex)     then FcbDuplex.Enabled := FActive and (PrinterIndex >= 0);
  if assigned (FAnzahlKopien) then FAnzahlKopien.Enabled := FActive and (PrinterIndex >= 0);

  if Assigned(FOnEnableDisable) then FOnEnableDisable(self, Value);
end;
procedure TPrinterData.SetActiveCheckbox(const Value: TCheckBox);
begin
  FActiveCheckbox := Value;
  if FActiveCheckbox <> nil then FActiveCheckbox.Checked := FActive;
end;

function TPrinterData.GetSchachtNr : Integer;
begin
  result := -1;

  if Assigned(FPaperBin) and FPaperBin.Enabled then
  begin
    if FPaperBin.ItemIndex >= 0 then
      result := StrToInt (FBinNumber[FPaperBin.ItemIndex])
  end
  else
  begin
    if (mSchachtIndex >= 0) and (mSchachtIndex < FBinnumber.count) then
       result := StrToInt (FBinNumber[mSchachtIndex]);
  end;
end;
function TPrinterData.GetSchachtIndex : Integer;
begin
  result := -1;

  if Assigned(FPaperBin) and FPaperBin.Enabled then
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
function TPrinterData.GetDuplexIndex : Integer;
begin
  result := -1;

  if Assigned(DuplexListe) and DuplexListe.Enabled then
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
procedure TPrinterData.SetPaperBin ( Value: TComboBox );
begin
   if FPaperBin = Value then exit;
   FPaperBin := Value;
   if Value = nil then exit;
   FPaperBin.ItemIndex := -1;
   FPaperbin.Enabled := False;
end;
procedure TPrinterData.SetDuplexListe( Value: TComboBox );
begin
   if Value = FcbDuplex then exit;
   FcbDuplex := Value;
   if Value = nil then exit;
   with FCbDuplex.Items do
   begin
      Clear;
      Add ('Kein');
      Add ('Horizontal');
      Add ('Vertikal');
   end;
   FcbDuplex.ItemIndex := -1;
   FcbDuplex.Enabled := False;
end;
procedure TPrinterData.SetSpinKopien( PSpin: TSpinEdit );
begin
   if FAnzahlKopien = PSpin then exit;
   FAnzahlKopien := PSpin;
   if PSpin = nil then exit;
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
   FAnzahlKopien.Enabled := False;
end;
procedure TPrinterData.PrinterChange( Sender: TObject );
begin
   if FBildschirm = True then PrinterIndex := FDruckerListe.ItemIndex - 1
   else PrinterIndex := FDruckerListe.ItemIndex;
end;
procedure TPrinterData.SetDefaults;
var
  istBonFormat: boolean;
begin
  // siehe auch C# Quelltext CoraDruckAuftrag.cs:GetDruckEinstellungen()

  if KopienDrucker then
  begin
    if Printer.Printers.Count = 0 then
      PrinterIndex := -1
    else
      PrinterIndex := 0;
  end
  else
  begin
    PrinterIndex := -1; // Bildschirm
  end;

  SchachtIndex := -1;
  if (Zufuhrliste <> nil) then Zufuhrliste.Itemindex := -1;
  DuplexIndex := -1;
  if (DuplexListe <> nil) then DuplexListe.Itemindex := -1;
  if (SpinKopien  <> nil) then
  begin
    if KopienDrucker then
      SpinKopien.Value := 0
    else
      SpinKopien.Value := 1;
  end;

  // istBonFormat := (formularArt = 'Kasse') or (formularArt = 'Kasse-Kett') or ((formularArt = 'LM-Belege') and (formularNr = 2));
  istBonFormat := Pos('kasse', LowerCase(ExtractFileName(ParamStr(0)))) > 0;

  DruckenKopf := true;
  DruckenAdressueberschrift := true;
  DruckenFuss := true;
  Kopfseite2Anders := false;
  if istBonFormat then RandOben  := 0 else RandOben  := 2;
  if istBonFormat then RandUnten := 0 else RandUnten := 2;

  Active := not Kopiendrucker;
end;

procedure TPrinterdata.SetDruckerListe( PList: TComboBox );
begin
  FDruckerListe := PList;
   if FDruckerListe = nil then exit;

  if (csDesigning in Componentstate) then exit;

  with FDruckerListe do
  begin
    style := csDropDownList;

    if not (csDesigning in Componentstate) then
    begin
      Items.Assign(TPrinterData.CachedPrinterList);
      if FBildschirm = True then
      begin
         Items.Insert( 0, 'Bildschirm' );
       FPrinterIndex := -1;
      end
      else FPrinterIndex := FStandardIndex;
    end;
    ItemIndex := 0;
    if assigned( Onchange ) then FOnAfterPrinterChange := onChange;
    OnChange := PrinterChange;
  end;
end;
procedure TPrinterData.SetPrinterIndex( Index: Integer );
var
   pcBuffer: pCHAR;
   wbuffer, wbuffer1: pWord;
   dwBuffer, iCounter: Integer;
   {$IF CompilerVersion >= 20.0}
   //StubDevMode: PDeviceMode;
   {$ELSE}
   //StubDevMode: TDeviceMode;
   //FPrinterHandle: THandle;
   {$IFEND}
   hMode: THandle;
   FDevMode: PDeviceMode;
const
  Hs_Max_Bins = 1000; // durch uns festgelegte Obergrenze
begin
  if FPrinter = nil then exit;
  if Index < 0 then  // Bildschirm
  begin
    FDrucker := TPrinterData.CachedPrinterList[ FStandardIndex ];
    FPrinter.PrinterIndex := FStandardIndex;
  end
  else  // Drucker
  begin
    if index < TPrinterData.CachedPrinterList.Count then
    begin
      FPrinter.PrinterIndex := Index;
      FDrucker := TPrinterData.CachedPrinterList[ Index ];
    end;
  end;
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
  hModeNeu := 0; // Ist ab sofort im Verantwortungsbereich von TPrinter und wird mit dessen Zerstörung freigegeben!

  *)


  // Das ist der neue Code:

  (*
  // Ist das richtig? Denn hMode gehört doch zu FPrinter?!
  if hMode <> 0 then
  begin
    GlobalUnlock(hMode);
    GlobalFree(hMode);
    hMode := 0;
    FDevMode := nil;
  end;

  FPrinter.GetPrinter( pcName, pcDriver, pcPort, hMode );

  // Delphi macht das aus irgendeinem Grund ist SetPrinterIndex, aber das ist doch quatsch,
  // weil SetPrinter macht doch bereits GlobalFree, also wäre es ein Double-Free!
  // (Bugreport bei Embarcadero eröffnen?)
  GlobalUnlock(hMode);
  GlobalFree(hMode);
  hMode := 0;

  // SetPrinter wird unter anderem einen neuen DevMode erzeugen, wenn wir einen DevMode von 0 liefern.
  // (Wenn wir einen eigenen DevMode liefern würden, würde er seinen eigenen freigeben und unseren nehmen)
  FPrinter.SetPrinter( pcName, pcDriver, pcPort, hMode );

  // Nun über GetPrinter den dort erzeugten DevMode holen, damit wir ihn weiter unten verwenden können
  FPrinter.GetPrinter( pcName, pcDriver, pcPort, hMode );
  FDevMode := GlobalLock(hMode);
  *)


  // Hier nochmal ein ganz anderer Code...

  FPrinter.PrinterIndex := Index;
  //FPrinterIndex := FPrinter.PrinterIndex;  // <-- nein, wir lassen kein "Feedback" zu. "-1" bleibt bei uns "-1"

  FPrinter.GetPrinter( pcName, pcDriver, pcPort, hMode );

  if (hMode = 0) then
  begin
    FPrinter.SetPrinter(pcName,pcDriver,pcPort,hMode);
    FPrinter.GetPrinter(pcName,pcDriver,pcPort,hMode);
  end;

  FDevMode := GlobalLock(hMode);
  try

    // Und jetzt geht's weiter.

    if (Index >= 0) then
    begin
      if not (csDesigning in ComponentState) then
      begin                               // DevMode
        if Assigned(fBinNumber) then
        begin
          FBinNumber.Clear;
          wBuffer := AllocMem( Hs_Max_Bins * 2 );
          try
            // DC_BINS: Retrieves a list of available paper bins. The pOutput buffer receives an array of WORD values that indicate the available paper sources for the printer. The return value indicates the number of entries in the array. For a list of the possible array values, see the description of the dmDefaultSource member of the DEVMODE structure. If pOutput is NULL, the return value indicates the required number of entries in the array.
            dwBuffer := DeviceCapabilities( pcName, pcPort, DC_BINS, PChar(wBuffer), nil );
            wbuffer1 := wbuffer;
            for iCounter := 0 to dwBuffer-1 do
            begin
              FBinNumber.Add(IntToStr(wBuffer1^));
              Inc(pWord(wBuffer1), 1); // inkrement um 1 WORD (also 2 byte)
            end;
          finally
            FreeMem( wBuffer );
          end;
        end;

        if (FDevMode <> nil) and (FPaperbin <> NIL) then
        begin
          if (FDevMode.dmFields and DM_DEFAULTSOURCE = 0) then
          begin
            FPaperbin.Enabled := False;
          end
          else
          begin
            FPaperbin.Enabled := True;
            FPaperBin.Clear;
            pcBuffer := AllocMem( Hs_Max_Bins * 24 );
            try
              // DC_BINNAMES: 	Retrieves the names of the printer's paper bins. The pOutput buffer receives an array of string buffers. Each string buffer is 24 characters long and contains the name of a paper bin. The return value indicates the number of entries in the array. The name strings are null-terminated unless the name is 24 characters long. If pOutput is NULL, the return value is the number of bin entries required.
              dwBuffer := DeviceCapabilities (pcName, pcPort, DC_BINNAMES, pcBuffer, FDevMode);
              for iCounter := 0 to dwBuffer-1 do
              begin
                FPaperBin.Items.Add( strpas( pcBuffer + iCounter * 24 ));
              end;

              try
                 FPaperbin.ItemIndex := -1;
                 for iCounter := 0 to FBinNumber.Count -1 do
                    if strToInt (FBinNumber[iCounter]) = FDevMode.dmDefaultSource then FPaperBin.ItemIndex := iCounter;
                 if FPaperBin.ItemIndex = -1 then FPaperBin.ItemIndex := 0;
              except
              end;
            finally
              FreeMem( pcBuffer );
            end;
          end;
        end;
      end;

      if FcbDuplex <> nil then
      begin
        FcbDuplex.Enabled := True;
        if (FDevMode = nil) or (FdevMode.dmFields and DM_DUPLEX = 0)then
        begin
          FcbDuplex.Enabled := False;
          FcbDuplex.ItemIndex := 0;
        end
        else
        begin
          FcbDuplex.Enabled := True;
          if FDevMode.dmDuplex = DMDUP_SIMPLEX then FcbDuplex.ItemIndex := 0;
          if FDevMode.dmDuplex = DMDUP_HORIZONTAL then FcbDuplex.ItemIndex := 1;
          if FDevMode.dmDuplex = DMDUP_VERTICAL then FcbDuplex.ItemIndex := 2;
        end;
      end;

      if FAnzahlKopien <> nil then
      begin
         FAnzahlKopien.Enabled := True;
  //     fanzahlkopien.value := 0;
  //     fanzahlkopien.value := FDevMode.dmcopies;
         try
           fanzahlkopien.value := FPrinter.Copies;
         except
           // TODO: Ticket 33167 - wenn der Drucker im Netzwerk nicht verfügbar (gesperrt oder nicht mehr freigegeben ist), dann verursacht das Weitermachen einen kaputten Stack
           // Unbedingt Meldung EPrinter-Exception "Operation auf ausgewähltem Drucker nicht verfügbar." beachten
           fanzahlkopien.value := 1;
         end;
       end;
    end
    else
    begin
      if FAnzahlKopien <> nil then
      begin
        FAnzahlKopien.Value := 1;
        FAnzahlKopien.Enabled := False;
      end;
      if FcbDuplex <> nil then
      begin
        FcbDuplex.ItemIndex := 0;
        FcbDuplex.Enabled := False;
      end;
      if FPaperBin <> nil then
      begin
        FPaperBin.ItemIndex := -1;
        FPaperbin.Enabled := False;
      end;
    end;
    FName := StrPas( pcName );
    FDriver := StrPas( pcDriver );
    FPort := StrPas( pcPort );
    with TiniFile.Create( 'Win.Ini' ) do
    begin
      FDriver := ReadString( 'devices', FName, '' );
      Free;
    end;
    FDriver := copy( FDriver, 1, pos( ',', FDriver ) - 1 );
    StrPCopy( pcDriver, FDriver );
    if Assigned( FDruckerListe ) then
    begin
      // OnChange muss abgeschaltet werden, um Rekursionen zu vermeiden!
      if not (csDesigning in Componentstate) then FDruckerListe.OnChange := nil;
      if FBildschirm = True then
        FDruckerListe.ItemIndex := FPrinterIndex + 1
      else
        FDruckerListe.ItemIndex := FPrinterIndex;
      if not (csDesigning in Componentstate) then FDruckerListe.OnChange := PrinterChange;
    end;
    // Falls vorhanden, OnChange der ComboBox aufrufen
    if assigned( FOnAfterPrinterChange ) then FOnAfterPrinterChange( self );
  finally
    if FDevMode <> nil then
      GlobalUnlock(hMode);
  end;
end;


procedure TPrinterData.EnforceAnyDefaultPrinter;
  // Das ist erforderlich, um SNoDefaultPrinter zu verhindern
  // Reproduzieren durch ändern von Wert "Device" in einen ungültigen Wert bei
  // HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Windows

  procedure GetPrinterList(List: TStrings);
  var
    Buffer, PrinterInfo: PChar;
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
        if not EnumPrinters(Flags, nil, Level, PByte(Buffer), Count, Count, NumInfo) then
          Exit;
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

  function SetDefaultPrinter(aPrinterName: string): Boolean;
  const
    WM_SETTINGCHANGE = $001A;
  type
    TSetDefaultPrinter = function (prnName: LPCTSTR): BOOL; stdcall;
  var
    WS: HINST;
    SetDefaultPrinter: TSetDefaultPrinter;
  begin
    WS := GetModuleHandle(PChar(winspl));
    {$IFDEF Unicode}
    SetDefaultPrinter := GetProcAddress(WS, 'SetDefaultPrinterW');
    {$ELSE}
    SetDefaultPrinter := GetProcAddress(WS, 'SetDefaultPrinterA');
    {$ENDIF}

    if Assigned(SetDefaultPrinter) then
    begin
      Result := SetDefaultPrinter(PChar(aPrinterName));
      if Result then
        SendNotifyMessage(HWND_BROADCAST, WM_SETTINGCHANGE, 0, 0);
    end
    else
    begin
      Result := False;
    end;
  end;

var
  sl: TSTringList;
begin
  try
    Printer.Printerindex := -1;
  except
    on EPrinter do
    begin
      sl := TStringList.Create;
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
  end;
end;

{$ENDREGION}

end.
