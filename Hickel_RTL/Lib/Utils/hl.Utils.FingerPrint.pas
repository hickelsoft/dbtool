unit hl.Utils.FingerPrint;

interface

uses
  SysUtils,
  Classes,
  IdFTP,
  IdSSLOpenSSL,
  IdFTPCommon,
  IdExplicitTLSClientServerBase,
  IdReplyRFC,
  IdFTPList,
  IdAllFTPListParsers,
  IdCharsets,
  hl.Utils,
  StrUtils,
  idGlobal;

{$IF gsIdVersion <> '10.1.5'} // Delphi 2007 built-in Indy10; gsIdVersion requires idGlobal.pas
{$DEFINE NewIndy}
{$IFEND}

type
  TCertFingerPrintVerifyBase = class(TObject)
  strict protected
    FExpected: string;
    FLogFile: string;
  public
    function Verify(Certificate: TIdX509; AOk: Boolean{$IFDEF NewIndy};
      ADepth, AError: Integer{$ENDIF}): Boolean; virtual; abstract;
    constructor Create(AExpected: string; ALogFile: string = '');
  end;

  TCertFingerPrintVerify = class(TCertFingerPrintVerifyBase)
  public
    function Verify(Certificate: TIdX509; AOk: Boolean{$IFDEF NewIndy};
      ADepth, AError: Integer{$ENDIF}): Boolean; override;
  end;

implementation

{ TCertFingerPrintVerifyBase }

constructor TCertFingerPrintVerifyBase.Create(AExpected: string;
  ALogFile: string = '');
begin
  inherited Create;

  FExpected := AExpected;
  FLogFile := ALogFile;
end;

{ TCertFingerPrintVerify }

function TCertFingerPrintVerify.Verify(Certificate: TIdX509;
  AOk: Boolean{$IFDEF NewIndy}; ADepth, AError: Integer{$ENDIF}): Boolean;
var
  sl: TStringList;
begin
{$IFDEF NewIndy}
  result := (FExpected = '') or
    SameText('md5/' + Certificate.Fingerprints.MD5AsString, Trim(FExpected)) or
    SameText('sha1/' + Certificate.Fingerprints.SHA1AsString, Trim(FExpected))
    or SameText('sha224/' + Certificate.Fingerprints.SHA224AsString,
    Trim(FExpected)) or
    SameText('sha256/' + Certificate.Fingerprints.SHA256AsString,
    Trim(FExpected)) or
    SameText('sha384/' + Certificate.Fingerprints.SHA384AsString,
    Trim(FExpected)) or
    SameText('sha512/' + Certificate.Fingerprints.SHA512AsString,
    Trim(FExpected));

  if FLogFile <> '' then
  begin
    sl := TStringList.Create;
    try
      sl.Add('md5/' + Certificate.Fingerprints.MD5AsString);
      sl.Add('sha1/' + Certificate.Fingerprints.SHA1AsString);
      sl.Add('sha224/' + Certificate.Fingerprints.SHA224AsString);
      sl.Add('sha256/' + Certificate.Fingerprints.SHA256AsString);
      sl.Add('sha384/' + Certificate.Fingerprints.SHA384AsString);
      sl.Add('sha512/' + Certificate.Fingerprints.SHA512AsString);
      sl.SaveToFile(FLogFile);
    finally
      FreeAndNil(sl);
    end;
  end;
{$ELSE}
  result := (FExpected = '') or
    SameText('md5/' + Certificate.FingerprintAsString, Trim(FExpected));

  if FLogFile <> '' then
  begin
    sl := TStringList.Create;
    try
      sl.Add('md5/' + Certificate.FingerprintAsString);
      sl.SaveToFile(FLogFile);
    finally
      FreeAndNil(sl);
    end;
  end;
{$ENDIF}
end;

end.
