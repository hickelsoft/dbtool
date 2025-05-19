unit hl.Utils.ImageConvert;

interface

uses
  SysUtils, DB;

const
  GuidGIF: TGUID = '{557CF402-1A04-11D3-9A73-0000F81EF32E}';
  GuidPNG: TGUID = '{557CF406-1A04-11D3-9A73-0000F81EF32E}';
  GuidJPEG: TGUID = '{557CF401-1A04-11D3-9A73-0000F81EF32E}';
  GuidBMP: TGUID = '{557CF400-1A04-11D3-9A73-0000F81EF32E}';
  GuidTIFF: TGUID = '{557CF405-1A04-11D3-9A73-0000F81EF32E}';

function BlobStringImageFormatConvert(bBlobImage: ansistring;
  desiredFormat: TGUID): string;
procedure LoadToBlobField(filename: string; blobField: TBlobField);
procedure SaveFromBlobField(filename: string; blobField: TBlobField);
procedure ConvertImageToFormat(filenameIn, filenameOut: string;
  desiredFormat: TGUID);

implementation

uses
  ActiveX, Windows, Classes;

resourcestring
  StrErrorDAtS = 'Fehler %d bei %s';
  StrUnbekanntesDateifor = 'Unbekanntes Dateiformat';

type
  // https://learn.microsoft.com/en-us/windows/win32/api/gdiplusinit/ns-gdiplusinit-gdiplusstartupinput
  TGDIStartup = packed record
    Version: Integer; // =1
    DebugEventCallback: Pointer; // For debug
    SuppressBackgroundThread: Bool;
    SuppressExternalCodecs: Bool; // Tru to use internal codecs
  end;

  // https://learn.microsoft.com/de-de/previous-versions/ms534434(v=vs.85)
  TEncoderParameter = packed record
    Guid: TGUID;
    NumberOfValues: ULONG;
    Type_: ULONG;
    Value: Pointer;
  end;

  // https://learn.microsoft.com/de-de/previous-versions/ms534435(v=vs.85)
  TEncoderParameters = packed record
    Count: UINT;
    Parameter: array [0 .. 0] of TEncoderParameter;
  end;

  PEncoderParameters = ^TEncoderParameters;

function GdipSaveImageToFile(image: Pointer; filename: PWCHAR;
  clsidEncoder: PGUID; encoderParams: PEncoderParameters): Integer; stdcall;
  external 'GdiPlus.dll' name 'GdipSaveImageToFile';
function GdipLoadImageFromFile(const filename: PWideChar; out image: Pointer)
  : Integer; stdcall; external 'GdiPlus.dll' name 'GdipLoadImageFromFile';
function GdipSaveImageToStream(image: Pointer; stream: IStream;
  clsidEncoder: PGUID; encoderParams: PEncoderParameters): Integer; stdcall;
  external 'GdiPlus.dll' name 'GdipSaveImageToStream';
function GdipLoadImageFromStream(stream: IStream; out image: Pointer): Integer;
  stdcall; external 'GdiPlus.dll' name 'GdipLoadImageFromStream';
function GdiplusStartup(var Token: Longword; const Input, Output: Pointer)
  : Integer; stdcall; external 'GdiPlus.dll' name 'GdiplusStartup';
function GdipDisposeImage(image: Pointer): Integer; stdcall;
  external 'GdiPlus.dll' name 'GdipDisposeImage';

// -----------------------------------------------------------------------------

function HexToInt(HexNum: string): LongInt;
begin
  Result := StrToInt('$' + HexNum);
end;

procedure LoadToBlobField(filename: string; blobField: TBlobField);
var
  Err: Integer;
  InitToken: Longword;
  Startup: TGDIStartup;
  GdiImage: Pointer;
  ms: TMemoryStream;
  sa: IStream;
begin
  ms := TMemoryStream.Create;
  try
    FillChar(Startup, sizeof(Startup), 0);
    Startup.Version := 1;
    GdiplusStartup(InitToken, @Startup, nil);

    Err := GdipLoadImageFromFile(PWideChar(WideString(filename)), GdiImage);
    if Err = 7 then
      RaiseLastOSError;
    if Err <> 0 then
      raise Exception.CreateFmt(StrErrorDAtS, [Err, 'GdipLoadImageFromFile']);

    sa := TStreamAdapter.Create(ms);
    Err := GdipSaveImageToStream(GdiImage, sa, @GuidBMP, nil);
    if Err = 7 then
      RaiseLastOSError;
    if Err <> 0 then
      raise Exception.CreateFmt(StrErrorDAtS, [Err, 'GdipSaveImageToStream']);

    Err := GdipDisposeImage(GdiImage);
    if Err = 7 then
      RaiseLastOSError;
    if Err <> 0 then
      raise Exception.CreateFmt(StrErrorDAtS, [Err, 'GdipDisposeImage']);

    ms.Position := 0;
    blobField.LoadFromStream(ms);
  finally
    sa := nil;
    FreeAndNil(ms);
  end;
end;

procedure SaveFromBlobField(filename: string; blobField: TBlobField);
var
  Err: Integer;
  InitToken: Longword;
  Startup: TGDIStartup;
  GdiImage: Pointer;
  ms: TMemoryStream;
  sa: IStream;
  desiredFormat: TGUID;
  ext: string;
begin
  ext := Lowercase(ExtractFileExt(filename));
  if (ext = '.gif') then
    desiredFormat := GuidGIF
  else if (ext = '.png') then
    desiredFormat := GuidPNG
  else if (ext = '.jpg') or (ext = '.jpeg') then
    desiredFormat := GuidJPEG
  else if (ext = '.bmp') then
    desiredFormat := GuidBMP
  else if (ext = '.tif') or (ext = '.tiff') then
    desiredFormat := GuidTIFF
  else
    raise Exception.Create(StrUnbekanntesDateifor);

  ms := TMemoryStream.Create;
  try
    sa := TStreamAdapter.Create(ms, soReference);
    blobField.SaveToStream(ms);
    ms.Position := 0;

    FillChar(Startup, sizeof(Startup), 0);
    Startup.Version := 1;
    GdiplusStartup(InitToken, @Startup, nil);

    Err := GdipLoadImageFromStream(sa, GdiImage);
    if Err = 7 then
      RaiseLastOSError;
    if Err <> 0 then
      raise Exception.CreateFmt(StrErrorDAtS, [Err, 'GdipLoadImageFromStream']);

    Err := GdipSaveImageToFile(GdiImage, PWideChar(WideString(filename)),
      @desiredFormat, nil);
    if Err = 7 then
      RaiseLastOSError;
    if Err <> 0 then
      raise Exception.CreateFmt(StrErrorDAtS, [Err, 'GdipSaveImageToFile']);

    Err := GdipDisposeImage(GdiImage);
    if Err = 7 then
      RaiseLastOSError;
    if Err <> 0 then
      raise Exception.CreateFmt(StrErrorDAtS, [Err, 'GdipDisposeImage']);
  finally
    sa := nil;
    FreeAndNil(ms);
  end;
end;

function BlobStringImageFormatConvert(bBlobImage: ansistring;
  desiredFormat: TGUID): string;
var
  Err: Integer;
  InitToken: Longword;
  Startup: TGDIStartup;
  GdiImage: Pointer;
  ms: TMemoryStream;
  buf: AnsiChar;
  buf2: array of AnsiChar;
  i: Integer;
  sa: IStream;
begin
  if not(Lowercase(Copy(bBlobImage, 1, 2)) = '0x') and
    not(UpperCase(Copy(bBlobImage, 3, 14)) = '474946383761' { GIF87a } ) and
    not(UpperCase(Copy(bBlobImage, 3, 14)) = '474946383961' { GIF89a } ) and
    not(UpperCase(Copy(bBlobImage, 3, 18)) = '89504E470D0A1A0A' { PNG } ) and
    not(UpperCase(Copy(bBlobImage, 3, 18)) = '89504E470D0A1A0A' { PNG } ) and
    not(UpperCase(Copy(bBlobImage, 3, 10)) = 'FFD8FFDB' { JPEG raw } ) and
    not(UpperCase(Copy(bBlobImage, 3, 10))
    = 'FFD8FFE0' { nnnn4A4649460001) {JFIF } ) and
    not(UpperCase(Copy(bBlobImage, 3, 10))
    = 'FFD8FFE1' { nnnn457869660000 } { Exif } ) and
    not(UpperCase(Copy(bBlobImage, 3, 6)) = '424D' { BMP } ) and
    not(UpperCase(Copy(bBlobImage, 3, 10))
    = '49492A00' { TIFF little endian format } ) and
    not(UpperCase(Copy(bBlobImage, 3, 10))
    = '4D4D002A' { TIFF big endian format } ) then
  begin
    // Keine umzuwandelnden Daten
    Result := bBlobImage;
    exit;
  end;

  ms := TMemoryStream.Create;
  try
{$REGION 'Blob String -> Memory Stream'}
    for i := 1 to (Length(bBlobImage) div 2 - 1) do
    begin
      buf := AnsiChar(HexToInt(Copy(bBlobImage, i * 2 + 1, 2)));
      ms.Write(buf, 1);
    end;
    ms.Position := 0;
{$ENDREGION}
    sa := TStreamAdapter.Create(ms);

{$REGION 'Memory Stream umwandeln'}
    FillChar(Startup, sizeof(Startup), 0);
    Startup.Version := 1;
    GdiplusStartup(InitToken, @Startup, nil);

    Err := GdipLoadImageFromStream(sa, GdiImage);
    if Err <> 0 then
      raise Exception.CreateFmt(StrErrorDAtS, [Err, 'GdipLoadImageFromFile']);

    ms.Size := 0;

    Err := GdipSaveImageToStream(GdiImage, sa, @desiredFormat, nil);
    if Err <> 0 then
      raise Exception.CreateFmt(StrErrorDAtS, [Err, 'GdipSaveImageToFile']);

    Err := GdipDisposeImage(GdiImage);
    if Err <> 0 then
      raise Exception.CreateFmt(StrErrorDAtS, [Err, 'GdipDisposeImage']);
{$ENDREGION}
{$REGION 'Memory Stream -> Blob string'}
    Result := '0x';

    ms.Position := 0;
    SetLength(buf2, ms.Size);
    ms.ReadBuffer(buf2[0], ms.Size);
    for i := 0 to ms.Size - 1 do
      Result := Result + IntToHex(Ord(buf2[i]), 2);
{$ENDREGION}
  finally
    sa := nil;
    FreeAndNil(ms);
  end;
end;

procedure ConvertImageToFormat(filenameIn, filenameOut: string;
  desiredFormat: TGUID);
var
  Err: Integer;
  InitToken: Longword;
  Startup: TGDIStartup;
  GdiImage: Pointer;
begin
  FillChar(Startup, sizeof(Startup), 0);
  Startup.Version := 1;
  GdiplusStartup(InitToken, @Startup, nil);

  Err := GdipLoadImageFromFile(PWideChar(WideString(filenameIn)), GdiImage);
  if Err <> 0 then
    raise Exception.CreateFmt(StrErrorDAtS, [Err, 'GdipLoadImageFromFile']);

  Err := GdipSaveImageToFile(GdiImage, PWideChar(WideString(filenameOut)),
    @desiredFormat, nil);
  if Err <> 0 then
    raise Exception.CreateFmt(StrErrorDAtS, [Err, 'GdipSaveImageToFile']);

  Err := GdipDisposeImage(GdiImage);
  if Err <> 0 then
    raise Exception.CreateFmt(StrErrorDAtS, [Err, 'GdipDisposeImage']);
end;

end.
