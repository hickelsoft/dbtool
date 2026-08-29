unit hl_Printer_Neu;

// Hinweis: Diese Unit wird derzeit nur von CORA_Druck.exe (C#) verwendet.
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

    FDevice: string;
    FDriver: string;
    FPort: string;
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

implementation

uses
  Consts, hl_Exceptions, Drucker;

resourcestring
  StrDuplexKein = 'Kein';
  StrDuplexHorizontal = 'Horizontal';
  StrDuplexVertikal = 'Vertikal';
  StrCORAplus = 'CORAplus';
  StrDuplexModusSNich = 'Duplex-Modus %s nicht gefunden';
  StrPapiergrößeSNicht = 'Papiergröße %s nicht gefunden';
  StrDruckerSNichtG = 'Drucker "%s" nicht gefunden';
  StrPapierquelleSNich = 'Papierquelle %s nicht gefunden';

threadvar _hlPrinterNeu: ThlPrinterNeu;

function ThlPrinterNeu.Win_GetDefaultSource: SmallInt;
// ACHTUNG! Result ist ein Windows-Index, kein normaler Index
var
  pDevMode: PDeviceMode;
  hDMode: THandle;
begin
  result := -1;
  hDMode := InitPrinterParams;
  if hDMode <> 0 then
  begin
    pDevMode := GlobalLock(hDMode);
    if pDevMode <> nil then
    begin
      if (pDevMode^.dmFields and DM_DEFAULTSOURCE = DM_DEFAULTSOURCE) then
      begin
        result := pDevMode^.dmDefaultSource;
      end;
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
    if pDevMode <> nil then
    begin
      if (pDevMode^.dmFields and DM_DUPLEX = DM_DUPLEX) then
      begin
        result := pDevMode^.dmDuplex;
      end;
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
    if pDevMode <> nil then
    begin
      if (pDevMode^.dmFields and DM_PAPERSIZE = DM_PAPERSIZE) then
      begin
        result := pDevMode^.dmPaperSize;
      end;
      GlobalUnlock(hDMode);
    end;
  end;
end;

constructor ThlPrinterNeu.Create;
begin
  inherited;

  Title := StrCORAplus;
  FPaperSizeIndex := -1;
  FPaperSourceIndex := -1;

  FDuplexTextListe := TStringList.Create;
  FDuplexTextListe.Add(StrDuplexKein);
  FDuplexTextListe.Add(StrDuplexHorizontal);
  FDuplexTextListe.Add(StrDuplexVertikal);
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
const
  CCH_BINNAME = 24;
var
  pDevMode: PDeviceMode;
  BinCount: Integer;
  Res: Integer;
  BinNames: array of array[0..CCH_BINNAME - 1] of Char;
  i: Integer;
begin
  if FPaperSourceNames = nil then
    FPaperSourceNames := TStringList.Create;

  FPaperSourceNames.Clear;
  InitPrinterParams;

  pDevMode := nil;

  // Erst herausfinden, wie viele Papierquellen vorhanden sind.
  BinCount := DeviceCapabilities(
    PChar(FDevice),
    PChar(FPort),
    DC_BINNAMES,
    nil,
    pDevMode
  );

  if BinCount <= 0 then
  begin
    Result := FPaperSourceNames;
    Exit;
  end;

  // Puffer passend zur tatsächlichen Anzahl anlegen.
  SetLength(BinNames, BinCount);

  // Namen der Papierquellen abfragen.
  Res := DeviceCapabilities(
    PChar(FDevice),
    PChar(FPort),
    DC_BINNAMES,
    PChar(@BinNames[0][0]),
    pDevMode
  );

  if Res <= 0 then
  begin
    Result := FPaperSourceNames;
    Exit;
  end;

  for i := 0 to Res - 1 do
    FPaperSourceNames.Add(BinNames[i]);

  Result := FPaperSourceNames;
end;

function ThlPrinterNeu.GetPrinterName: string;
begin
  result := printers.Strings[PrinterIndex];
end;

function ThlPrinterNeu.GetSchachtVerfuegbar: boolean;
begin
  // Achtung: "-1" heißt wirklich "undefiniert" und nicht "Standard", da es sich um einen Windows-Index handelt
  result := Win_GetDefaultSource <> -1;
end;

function ThlPrinterNeu.InitPrinterParams: THandle;
begin
  GetPrinter(FDevice, FDriver, FPort, result);
  {
    Ohne Handle auf eine DevMode-Struktur geht nichts. Deshalb wird durch den Aufruf
    von SetPrinter das Handle besorgt
  }
  if (result = 0) then
  begin
    SetPrinter(FDevice, FDriver, FPort, result);
    GetPrinter(FDevice, FDriver, FPort, result);
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
    for i := 0 to PaperSizeNumbers.Count - 1 do
    begin
      if strToInt(PaperSizeNumbers.Strings[i]) = PapersizeNrStandard then
      begin
        result := PaperSizes.Strings[i];
        exit;
      end;
    end;
    if (result = '') and (PaperSizes.Count > 0) then
      result := PaperSizes.Strings[0];
  end
  else
  begin
    result := PaperSizes.Strings[FPaperSizeIndex];
  end;
end;

function ThlPrinterNeu.GetPaperSourceNumbers: TStrings;
var
  pDevMode: PDeviceMode;
  Buffer: array of WORD;
  BinCount: Integer;
  Res: Integer;
  i: Integer;
begin
  if FPaperSourceNumbers = nil then
    FPaperSourceNumbers := TStringList.Create;

  FPaperSourceNumbers.Clear;
  InitPrinterParams;

  pDevMode := nil;

  // Erst Anzahl der vorhandenen Papierzuführungen ermitteln.
  BinCount := DeviceCapabilities(
    PChar(FDevice),
    PChar(FPort),
    DC_BINS,
    nil,
    pDevMode
  );

  if BinCount <= 0 then
  begin
    Result := FPaperSourceNumbers;
    Exit;
  end;

  // Puffer passend zur tatsächlichen Anzahl anlegen.
  SetLength(Buffer, BinCount);

  // Papierzuführungsnummern abfragen.
  Res := DeviceCapabilities(
    PChar(FDevice),
    PChar(FPort),
    DC_BINS,
    PChar(@Buffer[0]),
    pDevMode
  );

  for i := 0 to Res - 1 do
    FPaperSourceNumbers.Add(IntToStr(Buffer[i]));

  Result := FPaperSourceNumbers;
end;

function ThlPrinterNeu.GetPaperSizeNames: TStrings;
const
  CCH_PAPERNAME = 64;
var
  pDevMode: PDeviceMode;
  PaperCount: Integer;
  Res: Integer;
  PaperNames: array of array[0..CCH_PAPERNAME - 1] of Char;
  i: Integer;
begin
  if FPaperSizeNames = nil then
    FPaperSizeNames := TStringList.Create;

  FPaperSizeNames.Clear;
  InitPrinterParams;

  pDevMode := nil;

  // Erst herausfinden, wie viele Papiergrößen vorhanden sind.
  PaperCount := DeviceCapabilities(
    PChar(FDevice),
    PChar(FPort),
    DC_PAPERNAMES,
    nil,
    pDevMode
  );

  if PaperCount <= 0 then
  begin
    Result := FPaperSizeNames;
    Exit;
  end;

  // Puffer passend zur tatsächlichen Anzahl anlegen.
  SetLength(PaperNames, PaperCount);

  // Namen der Papiergrößen abfragen.
  Res := DeviceCapabilities(
    PChar(FDevice),
    PChar(FPort),
    DC_PAPERNAMES,
    PChar(@PaperNames[0][0]),
    pDevMode
  );

  if Res <= 0 then
  begin
    Result := FPaperSizeNames;
    Exit;
  end;

  for i := 0 to Res - 1 do
    FPaperSizeNames.Add(PaperNames[i]);

  Result := FPaperSizeNames;
end;

function ThlPrinterNeu.GetPaperSizeNumbers: TStrings;
var
  pDevMode: PDeviceMode;
  Buffer: array of WORD;
  PaperCount: Integer;
  Res: Integer;
  i: Integer;
begin
  if FPaperSizeNumbers = nil then
    FPaperSizeNumbers := TStringList.Create;

  FPaperSizeNumbers.Clear;
  InitPrinterParams;

  pDevMode := nil;

  // Erst Anzahl der unterstützten Papierformate ermitteln.
  PaperCount := DeviceCapabilities(
    PChar(FDevice),
    PChar(FPort),
    DC_PAPERS,
    nil,
    pDevMode
  );

  if PaperCount <= 0 then
  begin
    Result := FPaperSizeNumbers;
    Exit;
  end;

  // Puffer passend zur tatsächlichen Anzahl anlegen.
  SetLength(Buffer, PaperCount);

  // Papierformatnummern abfragen.
  Res := DeviceCapabilities(
    PChar(FDevice),
    PChar(FPort),
    DC_PAPERS,
    PChar(@Buffer[0]),
    pDevMode
  );

  for i := 0 to Res - 1 do
    FPaperSizeNumbers.Add(IntToStr(Buffer[i]));

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
  if duplex = DMDUP_SIMPLEX then
    result := DuplexListe.Strings[0];
  if duplex = DMDUP_HORIZONTAL then
    result := DuplexListe.Strings[1];
  if duplex = DMDUP_VERTICAL then
    result := DuplexListe.Strings[2];
end;

function ThlPrinterNeu.GetPageHeight_mm: Integer;
var
  pDevMode: PDeviceMode;
  PaperCount: Integer;
  Res: Integer;
  Papers: array of TPoint;
begin
  InitPrinterParams;
  pDevMode := nil;

  // Anzahl der unterstützten Papierformate ermitteln.
  PaperCount := DeviceCapabilities(
    PChar(FDevice),
    PChar(FPort),
    DC_PAPERSIZE,
    nil,
    pDevMode
  );

  if PaperCount <= 0 then
  begin
    Result := 0;
    Exit;
  end;

  // Puffer passend zur tatsächlichen Anzahl anlegen.
  SetLength(Papers, PaperCount);

  // Papiergrößen abfragen.
  Res := DeviceCapabilities(
    PChar(FDevice),
    PChar(FPort),
    DC_PAPERSIZE,
    PChar(@Papers[0]),
    pDevMode
  );

  if (FPaperSizeIndex >= 0) and (FPaperSizeIndex < Res) then
    Result := Papers[FPaperSizeIndex].Y
  else
    Result := 0;
end;

function ThlPrinterNeu.GetPageWidth_mm: Integer;
var
  pDevMode: PDeviceMode;
  PaperCount: Integer;
  Res: Integer;
  Papers: array of TPoint;
begin
  InitPrinterParams;
  pDevMode := nil;

  // Anzahl der unterstützten Papierformate ermitteln.
  PaperCount := DeviceCapabilities(
    PChar(FDevice),
    PChar(FPort),
    DC_PAPERSIZE,
    nil,
    pDevMode
  );

  if PaperCount <= 0 then
  begin
    Result := 0;
    Exit;
  end;

  // Puffer passend zur tatsächlichen Anzahl anlegen.
  SetLength(Papers, PaperCount);

  // Papiergrößen abfragen.
  Res := DeviceCapabilities(
    PChar(FDevice),
    PChar(FPort),
    DC_PAPERSIZE,
    PChar(@Papers[0]),
    pDevMode
  );

  if (FPaperSizeIndex >= 0) and (FPaperSizeIndex < Res) then
    Result := Papers[FPaperSizeIndex].X
  else
    Result := 0;
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
    for i := 0 to PaperSourceNumbers.Count - 1 do
    begin
      if strToInt(PaperSourceNumbers.Strings[i]) = SchachtNrStandard then
      begin
        result := PaperSources.Strings[i];
        exit;
      end;
    end;
    if (result = '') and (PaperSources.Count > 0) then
      result := PaperSources.Strings[0];
  end
  else
  begin
    result := PaperSources.Strings[FPaperSourceIndex];
  end;
end;

procedure ThlPrinterNeu.SetDuplexModus(const Value: integer);
var
  pDevMode: PDeviceMode;
  Res: DWORD;
  hDMode: THandle;

begin
  if (Printing) then
    raise EPrinter.Create(SPrinting);

  if (Value < 0) then
    exit;

  hDMode := InitPrinterParams;
  if (hDMode <> 0) then
  begin
    pDevMode := nil;
    Res := DeviceCapabilities(PChar(FDevice), PChar(FPort), DC_DUPLEX, nil, pDevMode);

    if Res <> 0 then
    begin
      pDevMode := GlobalLock(hDMode);
      if pDevMode <> nil then
      begin
        pDevMode^.dmFields := pDevMode^.dmFields or DM_DUPLEX;
        pDevMode^.dmDuplex := Value;
        GlobalUnlock(hDMode);
      end;
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
    raise ThlException.CreateFmt(StrDuplexModusSNich, [Value]);
end;

procedure ThlPrinterNeu.SetPaperSizeIndex(Value: Integer);
var
  pDevMode: PDeviceMode;
  p: array of WORD;
  PaperCount: Integer;
  Res: Integer;
  found: Boolean;
  i: Integer;
  hDMode: THandle;
  PaperSizeNumber: Integer;
begin
  if Printing then
    raise EPrinter.Create(SPrinting);

  if Value < 0 then
    Exit;

  // Auch den Zugriff auf PaperSizeNumbers absichern.
  if Value >= PaperSizeNumbers.Count then
    Exit;

  hDMode := InitPrinterParams;
  if hDMode <> 0 then
  begin
    pDevMode := nil;

    // Erst Anzahl der unterstützten Papierformate ermitteln.
    PaperCount := DeviceCapabilities(
      PChar(FDevice),
      PChar(FPort),
      DC_PAPERS,
      nil,
      pDevMode
    );

    if PaperCount > 0 then
    begin
      // Puffer passend zur tatsächlichen Anzahl anlegen.
      SetLength(p, PaperCount);

      // Papierformat-IDs abfragen.
      Res := DeviceCapabilities(
        PChar(FDevice),
        PChar(FPort),
        DC_PAPERS,
        PChar(@p[0]),
        pDevMode
      );

      if Res > 0 then
      begin
        pDevMode := GlobalLock(hDMode);
        if pDevMode <> nil then
        begin
          pDevMode^.dmFields := pDevMode^.dmFields or DM_PAPERSIZE;

          PaperSizeNumber := StrToInt(PaperSizeNumbers.Strings[Value]);

          found := False;
          for i := 0 to Res - 1 do
          begin
            if p[i] = PaperSizeNumber then
            begin
              found := True;
              Break;
            end;
          end;

          if found then
          begin
            pDevMode^.dmPaperSize := PaperSizeNumber;
            FPaperSizeIndex := Value;
          end
          else
          begin
            pDevMode^.dmPaperSize := 0;
            FPaperSizeIndex := -1;
          end;

          GlobalUnlock(hDMode);
        end;
      end;
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
    raise ThlException.CreateFmt(StrPapiergrößeSNicht, [Value]);
end;

procedure ThlPrinterNeu.SetPaperSourceIndex(Value: Integer);
var
  pDevMode: PDeviceMode;
  Buffer: array of WORD;
  BinCount: Integer;
  Res: Integer;
  i: Integer;
  found: Boolean;
  hDMode: THandle;
  PaperSourceNumber: Integer;
begin
  if Printing then
    raise EPrinter.Create(SPrinting);

  if Value < 0 then
    Exit;

  // Verhindert einen ungültigen Zugriff auf PaperSourceNumbers.
  if Value >= PaperSourceNumbers.Count then
    Exit;

  hDMode := InitPrinterParams;
  if hDMode <> 0 then
  begin
    pDevMode := nil;

    // Erst Anzahl der vorhandenen Papierzuführungen ermitteln.
    BinCount := DeviceCapabilities(
      PChar(FDevice),
      PChar(FPort),
      DC_BINS,
      nil,
      pDevMode
    );

    if BinCount > 0 then
    begin
      // Puffer passend zur tatsächlichen Anzahl anlegen.
      SetLength(Buffer, BinCount);

      // Papierzuführungsnummern abfragen.
      Res := DeviceCapabilities(
        PChar(FDevice),
        PChar(FPort),
        DC_BINS,
        PChar(@Buffer[0]),
        pDevMode
      );

      if Res > 0 then
      begin
        pDevMode := GlobalLock(hDMode);
        if pDevMode <> nil then
        begin
          pDevMode^.dmFields :=
            pDevMode^.dmFields or DM_DEFAULTSOURCE;

          PaperSourceNumber :=
            StrToInt(PaperSourceNumbers.Strings[Value]);

          found := False;

          for i := 0 to Res - 1 do
          begin
            if Buffer[i] = PaperSourceNumber then
            begin
              found := True;
              Break;
            end;
          end;

          if found then
          begin
            pDevMode^.dmDefaultSource := PaperSourceNumber;
            FPaperSourceIndex := Value;
          end
          else
          begin
            pDevMode^.dmDefaultSource := DMBIN_AUTO;
            FPaperSourceIndex := -1;
          end;

          GlobalUnlock(hDMode);
        end;
      end;
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
    raise ThlException.CreateFmt(StrDruckerSNichtG, [Value]);
end;

procedure ThlPrinterNeu.SetSchachtName(const Value: string);
var
  i: integer;
begin
  i := PaperSources.IndexOf(Value);
  if i >= 0 then
    PaperSourceIndex := i
  else
    raise ThlException.CreateFmt(StrPapierquelleSNich, [Value]);
end;

function hlPrinterNeu: ThlPrinterNeu;
begin
  if (_hlPrinterNeu = nil) then
    _hlPrinterNeu := ThlPrinterNeu.Create;
  result := _hlPrinterNeu;
end;

initialization

finalization

FreeAndNil(_hlPrinterNeu);

end.
