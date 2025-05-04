unit hl.Utils.MD5;

interface

uses
  Classes, SysUtils;

function MD5Stream(const s: TStream): string;
function MD5Binary(binary: array of byte): string;
function MD5String(const str: string): string;
function MD5File(const fileName: string): string;

implementation

uses
  IdHashMessageDigest, idHash, idGlobal;

{$IF gsIdVersion <> '10.1.5'} // Delphi 2007 built-in Indy10; gsIdVersion requires idGlobal.pas
{$DEFINE NewIndy}
{$IFEND}

function MD5Stream(const s: TStream): string;
var
  idmd5: TIdHashMessageDigest5;
begin
  idmd5 := TIdHashMessageDigest5.Create;
  try
{$IFDEF NewIndy}
    result := idmd5.HashStreamAsHex(s);
{$ELSE}
    result := idmd5.AsHex(idmd5.HashValue(s));
{$ENDIF}
  finally
    FreeAndNil(idmd5);
  end;
end;

function MD5Binary(binary: array of byte): string;
{$IFDEF NewIndy}
var
  idmd5: TIdHashMessageDigest5;
  idbytes: TIdBytes;
  i: integer;
begin
  SetLEngth(idbytes, Length(binary));
  for i := 0 to Length(binary) - 1 do
    idbytes[i] := binary[i];
  idmd5 := TIdHashMessageDigest5.Create;
  try
    result := idmd5.HashBytesAsHex(idbytes);
  finally
    FreeAndNil(idmd5);
  end;
{$ELSE}

begin
  raise Exception.Create('MD5Binary nicht kompatibel mit altem Indy');
{$ENDIF}
end;

function MD5String(const str: string): string;
{$IFDEF NewIndy}
var
  idmd5: TIdHashMessageDigest5;
begin
  idmd5 := TIdHashMessageDigest5.Create;
  try
    result := idmd5.HashStringAsHex(str, IndyTextEncoding_OSDefault);
  finally
    FreeAndNil(idmd5);
  end;
{$ELSE}

var
  ss: TStringStream;
begin
  ss := TStringStream.Create(str);
  try
    result := MD5Stream(ss);
  finally
    FreeAndNil(ss);
  end;
{$ENDIF}
end;

function MD5File(const fileName: string): string;
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(fileName, fmOpenRead or fmShareDenyWrite);
  try
    result := MD5Stream(fs);
  finally
    FreeAndNil(fs);
  end;
end;

end.
