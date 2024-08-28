unit hl_Printer_Neu;

// Hinweis: Diese Unit wird derzeit nur von CORA_DruckDLL.pas (C#) und
// Kasse Alt (über hl_Printer_Bon.pas) verwendet.
// Im Gegensatz zu HickelComponents\Drucker.pas ist diese Unit keine VCL-Unit,
// die anhand von ComboBoxen Druckerlisten anzeigt etc.

interface

uses
  Windows, Winspool, SysUtils, Classes, printers;

type
  ThlPrinterNeu = class(TPrinter)
  private
    function Win_GetDefaultSource: SmallInt;
    function Win_GetPapersize: SmallInt;
    function Win_GetDuplex: SmallInt;
  protected
    FPaperSourceNames: TStrings;
    FPaperSourceNumbers: TStrings;
    FPaperSizeNames: TStrings;
    FPaperSizeNumbers: TStrings;
    FPaperSourceIndex: integer;
    FPaperSizeIndex: integer;
    FDuplexTextListe: TStrings;

    ADevice: array[0..255] of char;
    ADriver: array[0..255] of char;
    APort: array[0..255] of char;
    function InitPrinterParams: THandle; // returns hDMode

    function GetPrinterName: string;
    procedure SetPrinterName(const Value: string);
    function GetDefaultPrinterName: string;

    function GetPaperSizeNames: TStrings;
    function GetPaperSizeNumbers: TStrings;
    function GetPaperSizeVerfuegbar: boolean;
    procedure SetPaperSizeIndex(Value: integer);
    function GetPaperSizeName: string;
    procedure SetPaperSizeName(const Value: string);
    function GetDefaultPapersize: string;
    function GetDefaultPapersizeNr: SmallInt;
    function GetPageHeight_mm: integer;
    function GetPageWidth_mm: integer;

    function GetPaperSourceNames: TStrings;
    function GetPaperSourceNumbers: TStrings;
    function GetSchachtVerfuegbar: boolean;
    procedure SetPaperSourceIndex(Value: integer);
    function GetSchachtName: string;
    procedure SetSchachtName(const Value: string);
    function GetDefaultSchachtName: string;
    function GetDefaultSchachtNr: SmallInt;

    function GetDuplexEnabled: boolean;
    function GetDuplexName: string;
    procedure SetDuplexName(const Value: string);
    function GetDuplexModus: integer;
    procedure SetDuplexModus(const Value: integer);
  public
    constructor Create;
    destructor Destroy; override;

    property PrinterName: string read GetPrinterName write SetPrinterName;
    property DefaultPrinterName: string read GetDefaultPrinterName;

    property PaperSizes: TStrings read GetPaperSizeNames;
    property PaperSizeIndex: integer read FPaperSizeIndex write SetPaperSizeIndex;
    property PaperSizeNumbers: TStrings read GetPaperSizeNumbers;
    property PaperSizeNr: SmallInt read Win_GetPapersize; // Achtung! Das ist die Windows-Interne SchachtNummer (FBinNumber) und nicht der Index in der Auswahlliste!
    property PaperSizeVerfuegbar: boolean read GetPaperSizeVerfuegbar;
    property PaperSizeName: string read GetPaperSizeName write SetPaperSizeName;
    property DefaultPapersize: string read GetDefaultPapersize;
    property PageHeight_mm: integer read GetPageHeight_mm;
    property PageWidth_mm: integer read GetPageWidth_mm;
    property DefaultPapersizeNr: SmallInt read GetDefaultPapersizeNr;

    property PaperSources: TStrings read GetPaperSourceNames;
    property PaperSourceIndex: integer read FPaperSourceIndex write SetPaperSourceIndex;
    property PaperSourceNumbers: TStrings read GetPaperSourceNumbers;
    property PaperSourceNr: SmallInt read Win_GetDefaultSource; // Achtung! Das ist die Windows-Interne SchachtNummer (FBinNumber) und nicht der Index in der Auswahlliste!
    property SchachtVerfuegbar: boolean read GetSchachtVerfuegbar;
    property SchachtName: string read GetSchachtName write SetSchachtName;
    property DefaultSchachtName: string read GetDefaultSchachtName;
    property DefaultSchachtNr: SmallInt read GetDefaultSchachtNr;

    property DuplexListe: TStrings read FDuplexTextListe;
    property DuplexEnabled: boolean read GetDuplexEnabled;
    property DuplexModus: integer read GetDuplexModus write SetDuplexModus;
    property DuplexName: string read GetDuplexName write SetDuplexName;
  end;

function hlPrinterNeu: ThlPrinterNeu;
function SethlPrinterNeu(NewPrinter: ThlPrinterNeu): ThlPrinterNeu;

implementation

uses
  Consts, hl_Exceptions, Drucker;

threadvar
  _hlPrinterNeu: ThlPrinterNeu;

function ThlPrinterNeu.Win_GetDefaultSource: SmallInt; // ACHTUNG! Result ist ein Windows-Index, kein normaler Index
var
  pDevMode: PDeviceMode;
  hDMode: THandle;
begin
  result := -1;
  hDMode := InitPrinterParams;
  if hDMode <> 0 then
  begin
    pDevMode := GlobalLock(hDMode);
    try
      if (pDevMode <> nil) and
         (pDevMode^.dmFields and DM_DEFAULTSOURCE = DM_DEFAULTSOURCE) then
      begin
        result := pDevMode^.dmDefaultSource;
      end;
    finally
      GlobalUnlock(hDMode);
    end;
  end;
end;

function ThlPrinterNeu.Win_GetDuplex: SmallInt;
var
  pDevMode: PDeviceMode;
  hDMode: THandle;
begin
  result := -1;
  hDMode := InitPrinterParams;
  if hDMode <> 0 then
  begin
    pDevMode := GlobalLock(hDMode);
    try
      if (pDevMode <> nil) and
         (pDevMode^.dmFields and DM_DUPLEX = DM_DUPLEX) then
      begin
        result := pDevMode^.dmDuplex;
      end;
    finally
      GlobalUnlock(hDMode);
    end;
  end;
end;

function ThlPrinterNeu.Win_GetPapersize: SmallInt;
var
  pDevMode: PDeviceMode;
  hDMode: THandle;
begin
  result := -1;
  hDMode := InitPrinterParams;
  if hDMode <> 0 then
  begin
    pDevMode := GlobalLock(hDMode);
    try
      if (pDevMode <> nil) and
         (pDevMode^.dmFields and DM_PAPERSIZE = DM_PAPERSIZE) then
      begin
        result := pDevMode^.dmPaperSize;
      end;
    finally
      GlobalUnlock(hDMode);
    end;
  end;
end;

constructor ThlPrinterNeu.Create;
begin
  inherited;

  Title := 'CORAplus';
  FPaperSizeIndex := -1;
  FPaperSourceIndex := -1;

  FDuplexTextListe := TStringList.Create;
  FDuplexTextListe.Add('Kein');
  FDuplexTextListe.Add('Horizontal');
  FDuplexTextListe.Add('Vertikal');
end;

function ThlPrinterNeu.GetDefaultPapersize: string;
var
  bak: integer;
begin
  bak := PaperSizeIndex;
  try
    PaperSizeIndex := -1;
    result := GetPaperSizeName;
  finally
    PaperSizeIndex := bak;
  end;
end;

function ThlPrinterNeu.GetDefaultPapersizeNr: SmallInt;
var
  bak: integer;
begin
  bak := PaperSizeIndex;
  try
    PaperSizeIndex := -1;
    result := PaperSizeNr;
  finally
    PaperSizeIndex := bak;
  end;
end;

function ThlPrinterNeu.GetDefaultSchachtNr: SmallInt;
var
  bak: integer;
begin
  bak := PaperSourceIndex;
  try
    PaperSourceIndex := -1;
    result := PaperSourceNr;
  finally
    PaperSourceIndex := bak;
  end;
end;

function ThlPrinterNeu.GetDefaultPrinterName: string;
var
  bak: integer;
begin
  bak := PrinterIndex;
  try
    PrinterIndex := -1;
    result := GetPrinterName;
  finally
    PrinterIndex := bak;
  end;
end;

function ThlPrinterNeu.GetDefaultSchachtName: string;
var
  bak: integer;
begin
  bak := PaperSourceIndex;
  try
    PaperSourceIndex := -1;
    result := GetSchachtName;
  finally
    PaperSourceIndex := bak;
  end;
end;

destructor ThlPrinterNeu.Destroy;
begin
  FreeAndNil(FPaperSizeNames);
  FreeAndNil(FPaperSourceNames);
  FreeAndNil(FPaperSourceNumbers);
  FreeAndNil(FDuplexTextListe);
  FreeAndNil(FPaperSizeNumbers);

  inherited;
end;

function ThlPrinterNeu.GetPaperSourceNames: TStrings;
var
  pDevMode: PDeviceMode;
  bin: array[0..255,0..23] of char;    // TODO: 255 nicht hartkodiert machen. lieber DeviceCapabilities mit pOutput=nil aufrufen um anzahl rauszufinden
  i: Integer;
  Res: Integer;
begin
  if (FPaperSourceNames = nil) then
  begin
    FPaperSourceNames := TStringList.Create;
  end;

  FPaperSourceNames.Clear;
  InitPrinterParams;

  pDevMode := nil;
  Res := DeviceCapabilities(ADevice,APort,DC_BINNAMES,PCHAR (@(bin[0][0])),pDevMode);
  for i := 0 to Res-1 do
  begin
    FPaperSourceNames.Add(bin[i]);
  end;

  Result := FPaperSourceNames;
end;

function ThlPrinterNeu.GetPrinterName: string;
begin
  result := Printers.Strings[PrinterIndex];
end;

function ThlPrinterNeu.GetSchachtVerfuegbar: boolean;
begin
  // Achtung: "-1" heißt wirklich "undefiniert" und nicht "Standard", da es sich um einen Windows-Index handelt
  result := Win_GetDefaultSource <> -1;
end;

function ThlPrinterNeu.InitPrinterParams: THandle;
begin
  GetPrinter(ADevice,ADriver,APort,result);
  {
  Ohne Handle auf eine DevMode-Struktur geht nichts. Deshalb wird durch den Aufruf
  von SetPrinter das Handle besorgt
  }
  if (result = 0) then
  begin
    SetPrinter(ADevice,ADriver,APort,result);
    GetPrinter(ADevice,ADriver,APort,result);
  end;
end;

function ThlPrinterNeu.GetPaperSizeName: string;
var
  i: integer;
  PapersizeNrStandard: integer;
begin
  if PaperSizeIndex = -1 then
  begin
    result := '';
    PapersizeNrStandard := Win_GetPapersize;
    for i := 0 to PaperSizeNumbers.Count -1 do
    begin
      if strToInt (PaperSizeNumbers.Strings[i]) = PapersizeNrStandard then
      begin
        result := PaperSizes.Strings[i];
        exit;
      end;
    end;
    if (result = '') and (PaperSizes.Count > 0) then result := PaperSizes.Strings[0];
  end
  else
  begin
    result := PaperSizes.Strings[FPaperSizeIndex];
  end;
end;

function ThlPrinterNeu.GetPaperSourceNumbers: TStrings;
var
  pDevMode: PDeviceMode;
  wBuffer, wbuffer1: pWord;
  i: Integer;
  Res: Integer;
const
  Hs_Max_Bins = 1000;
begin
  if (FPaperSourceNumbers = nil) then
  begin
    FPaperSourceNumbers := TStringList.Create;
  end;

  FPaperSourceNumbers.Clear;
  InitPrinterParams;

  wBuffer := AllocMem( Hs_Max_Bins * 2 );
  try
    pDevMode := nil;
    Res := DeviceCapabilities(ADevice,APort, DC_BINS, PChar(wBuffer), pDevMode );
    wbuffer1 := wbuffer;
    for i := 0 to Res-1 do
    begin
      FPaperSourceNumbers.Add(IntToStr(wBuffer1^));
      Inc(pWord(wBuffer1), 1); // inkrement um 1 WORD (also 2 byte)
    end;
  finally
    FreeMem(wBuffer);
  end;

  Result := FPaperSourceNumbers;
end;

function ThlPrinterNeu.GetPaperSizeNames: TStrings;
var
  pDevMode: PDeviceMode;
  pl: array[0..255,0..63] of char;    // TODO: 255 nicht hartkodiert machen. lieber DeviceCapabilities mit pOutput=nil aufrufen um anzahl rauszufinden
  i: Integer;
  Res: Integer;
begin
  if (FPaperSizeNames = nil) then
  begin
    FPaperSizeNames := TStringList.Create;
  end;

  FPaperSizeNames.Clear;
  InitPrinterParams;

  pDevMode := nil;
  Res := DeviceCapabilities(ADevice,APort,DC_PAPERNAMES,PCHAR(@(pl[0][0])),pDevMode);
  for i := 0 to Res-1 do
  begin
    FPaperSizeNames.Add(pl[i]);
  end;
  Result := FPaperSizeNames;
end;

function ThlPrinterNeu.GetPaperSizeNumbers: TStrings;
var
  pDevMode: PDeviceMode;
  pcBuffer: pchar;
  i: Integer;
  Res: Integer;
begin
  if (FPaperSizeNumbers = nil) then
  begin
    FPaperSizeNumbers := TStringList.Create;
  end;

  FPaperSizeNumbers.Clear;
  InitPrinterParams;

  pcBuffer := AllocMem( 2048 );
  try
    pDevMode := nil;
    Res := DeviceCapabilities(ADevice,APort, DC_PAPERS, pcBuffer, pDevMode );
    for i := 0 to Res-1 do
    begin
      FPaperSizeNumbers.Add( inttostr (ord(pcBuffer[i * 2])+ 256 * ord(pcBuffer[1+i*2])));
    end;
  finally
    FreeMem(pcBuffer);
  end;

  Result := FPaperSizeNumbers;
end;

function ThlPrinterNeu.GetPaperSizeVerfuegbar: boolean;
begin
  // Achtung: "-1" heißt wirklich "undefiniert" und nicht "Standard", da es sich um einen Windows-Index handelt
  result := Win_GetPapersize <> -1;
end;

function ThlPrinterNeu.GetDuplexEnabled: boolean;
begin
  // Achtung: "-1" heißt wirklich "undefiniert" und nicht "Standard", da es sich um einen Windows-Index handelt
  result := Win_GetDuplex <> -1;
end;

function ThlPrinterNeu.GetDuplexModus: integer;
begin
  result := Win_GetDuplex;
end;

function ThlPrinterNeu.GetDuplexName: string;
var
  duplex: integer;
begin
  duplex := Win_GetDuplex;
  if duplex = DMDUP_SIMPLEX    then result := DuplexListe.Strings[0];
  if duplex = DMDUP_HORIZONTAL then result := DuplexListe.Strings[1];
  if duplex = DMDUP_VERTICAL   then result := DuplexListe.Strings[2];
end;

function ThlPrinterNeu.GetPageHeight_mm: integer;
var
  pDevMode: PDeviceMode;
  p: array [0..99] of TPoint;    // TODO: 255 nicht hartkodiert machen. lieber DeviceCapabilities mit pOutput=nil aufrufen um anzahl rauszufinden
  Res: DWORD;
begin
  InitPrinterParams;
  pDevMode := nil;
  Res := DeviceCapabilities(ADevice,APort,DC_PAPERSIZE,PCHAR(@p[0]),pDevMode);
  if (FPaperSizeIndex <= integer(Res)) then
  begin
    Result := p[FPaperSizeIndex].y;
  end
  else
  begin
    Result := 0;
  end;
end;

function ThlPrinterNeu.GetPageWidth_mm: integer;
var
  pDevMode: PDeviceMode;
  p: array [0..99] of TPoint;     // TODO: 255 nicht hartkodiert machen. lieber DeviceCapabilities mit pOutput=nil aufrufen um anzahl rauszufinden
  Res: DWORD;

begin
  InitPrinterParams;
  pDevMode := nil;
  Res := DeviceCapabilities(ADevice,APort,DC_PAPERSIZE,PCHAR(@p[0]),pDevMode);
  if (FPaperSizeIndex <= integer(Res)) then
  begin
    Result := p[FPaperSizeIndex].x;
  end
  else
  begin
    Result := 0;
  end;
end;

function ThlPrinterNeu.GetSchachtName: string;
var
  i: integer;
  SchachtNrStandard: integer;
begin
  if PaperSourceIndex = -1 then
  begin
    result := '';
    SchachtNrStandard := Win_GetDefaultSource;
    for i := 0 to PaperSourceNumbers.Count -1 do
    begin
      if strToInt (PaperSourceNumbers.Strings[i]) = SchachtNrStandard then
      begin
        result := PaperSources.Strings[i];
        exit;
      end;
    end;
    if (result = '') and (PaperSources.Count > 0) then result := PaperSources.Strings[0];
  end
  else
  begin
    result := PaperSources.Strings[FPaperSourceIndex];
  end;
end;

procedure ThlPrinterNeu.SetDuplexModus(const Value: integer);
var
  pDevMode : PDeviceMode;
  p: array [0..99] of WORD;    // TODO: 255 nicht hartkodiert machen. lieber DeviceCapabilities mit pOutput=nil aufrufen um anzahl rauszufinden
  Res: DWORD;
  hDMode: THandle;

begin
  if (Printing) then raise EPrinter.Create(SPrinting);

  // FALSCH: if ((Value < 0) or (Value >= PaperSizes.Count)) then exit;
  if (Value < 0) then exit;

  hDMode := InitPrinterParams;
  if (hDMode <> 0) then
  begin
    pDevMode := nil;
    Res := DeviceCapabilities(ADevice,APort,DC_DUPLEX,PCHAR(@ p[0]),pDevMode);

    // FALSCH: if (Value <= integer(Res)) then
    if Res <> 0 then
    begin
      pDevMode := GlobalLock(hDMode);
      if (pDevMode <> nil) then
      begin
        pDevMode^.dmFields := pDevMode^.dmFields or DM_DUPLEX;
        pDevMode^.dmDuplex := Value;
      end;
      GlobalUnlock(hDMode);
    end;
  end;
end;

procedure ThlPrinterNeu.SetDuplexName(const Value: string);
var
  i: integer;
begin
  i := DuplexListe.IndexOf(Value);
  if i >= 0 then
    DuplexModus := i
  else
    raise ThlException.CreateFmt('Duplex-Modus %s nicht gefunden', [Value]);
end;

procedure ThlPrinterNeu.SetPaperSizeIndex(Value: integer);
var
  pDevMode : PDeviceMode;
  p: array [0..99] of WORD;     // TODO: 255 nicht hartkodiert machen. lieber DeviceCapabilities mit pOutput=nil aufrufen um anzahl rauszufinden
  Res: DWORD;
  found: Boolean;
  i: Integer;
  hDMode: THandle;

begin
  if (Printing) then raise EPrinter.Create(SPrinting);

  // FALSCH: if ((Value < 0) or (Value >= PaperSizes.Count)) then exit;
  if (Value < 0) then exit;

  hDMode := InitPrinterParams;
  if (hDMode <> 0) then
  begin
    pDevMode := nil;
    Res := DeviceCapabilities(ADevice,APort,DC_PAPERS,PCHAR(@ p[0]),pDevMode);

    // FALSCH: if (Value <= integer(Res)) then
    if Res <> 0 then
    begin
      pDevMode := GlobalLock(hDMode);
      if (pDevMode <> nil) then
      begin
        pDevMode^.dmFields := pDevMode^.dmFields or DM_PAPERSIZE;

        // FALSCH: pDevMode^.dmPaperSize := p[Value];
        found := false;
        for i := 0 to Res - 1 do
        begin
          if p[i] = StrToInt(PaperSizeNumbers.Strings[Value]) then
          begin
            found := true;
            break;
          end;
        end;
        if found then
        begin
          pDevMode^.dmPaperSize := StrToInt(PaperSizeNumbers.Strings[Value]);
          FPaperSizeIndex := Value;
        end
        else
        begin
          pDevMode^.dmPaperSize := DMBIN_AUTO;
          FPaperSizeIndex := -1;
        end;
      end;
      GlobalUnlock(hDMode);
    end;
  end;
end;

procedure ThlPrinterNeu.SetPaperSizeName(const Value: string);
var
  i: integer;
begin
  i := PaperSizes.IndexOf(Value);
  if i >= 0 then
    PaperSizeIndex := i
  else
    raise ThlException.CreateFmt('Papiergröße %s nicht gefunden', [Value]);
end;

procedure ThlPrinterNeu.SetPaperSourceIndex(Value: integer);
var
  pDevMode : PDeviceMode;
  wBuffer, wbuffer1: pWord;
  Res: DWORD;
  i: Integer;
  found: Boolean;
  hDMode: THandle;
const
  Hs_Max_Bins = 1000;
begin
  if (Printing) then raise EPrinter.Create(SPrinting);

  // DM 09.11.2017: Korrigiert. Hier wurden der Zufuhrlisten-Index und nicht die Schacht-Nr von Windows verwendet!
  //                Deswegen wurden nie die in den CORA-Druckeinstellungen gewählten Schächte gewählt, sondern nur die aus der Systemsteuerung.

  // FALSCH: if ((Value < 0) or (Value >= PaperSources.Count)) then exit;
  if (Value < 0) then exit;

  hDMode := InitPrinterParams;
  if (hDMode <> 0) then
  begin
    wBuffer := AllocMem( Hs_Max_Bins * 2 );
    try
      pDevMode := nil;
      Res := DeviceCapabilities(ADevice,APort,DC_BINS,PChar(wBuffer),pDevMode);

      // FALSCH: if (Value <= integer(Res)) then
      if Res <> 0 then
      begin
        pDevMode := GlobalLock(hDMode);
        if (pDevMode <> nil) then
        begin
          pDevMode^.dmFields := pDevMode^.dmFields or DM_DEFAULTSOURCE;

          // FALSCH: pDevMode^.dmDefaultSource := p[Value];
          found := false;

          wbuffer1 := wbuffer;
          for i := 0 to Res - 1 do
          begin
            if Integer(wBuffer1^) = StrToInt(PaperSourceNumbers.Strings[Value]) then
            begin
              found := true;
              break;
            end;
            Inc(pWord(wBuffer1), 1); // inkrement um 1 WORD (also 2 byte)
          end;
          if found then
          begin
            pDevMode^.dmDefaultSource := StrToInt(PaperSourceNumbers.Strings[Value]);
            FPaperSourceIndex := Value;
          end
          else
          begin
            pDevMode^.dmDefaultSource := DMBIN_AUTO;
            FPaperSourceIndex := -1;
          end;
        end;
        GlobalUnlock(hDMode);
      end;
    finally
      FreeMem(wBuffer);
    end;
  end;
end;

procedure ThlPrinterNeu.SetPrinterName(const Value: string);
var
  i: integer;
begin
  i := TPrinterData.CachedPrinterList.IndexOf(Value);
  if i >= 0 then
    PrinterIndex := i
  else
    raise ThlException.CreateFmt('Drucker "%s" nicht gefunden', [Value]);
end;

procedure ThlPrinterNeu.SetSchachtName(const Value: string);
var
  i: integer;
begin
  i := PaperSources.IndexOf(Value);
  if i >= 0 then
    PaperSourceIndex := i
  else
    raise ThlException.CreateFmt('Papierquelle %s nicht gefunden', [Value]);
end;

function hlPrinterNeu: ThlPrinterNeu;
begin
  if (_hlPrinterNeu = nil) then _hlPrinterNeu := ThlPrinterNeu.Create;
  Result := _hlPrinterNeu;
end;

function SethlPrinterNeu(NewPrinter: ThlPrinterNeu): ThlPrinterNeu;
begin
  Result := _hlPrinterNeu;
  _hlPrinterNeu := NewPrinter;
end;

initialization
finalization
  FreeAndNil(_hlPrinterNeu);
end.
