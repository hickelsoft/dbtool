unit HS_Auth;

interface

var
  HS_SA_DB_OLD: boolean = false;

function IstHickelSoftTestPC: boolean;
function PruefeHickelSoftPassword(const pwd: WideString): boolean;
function HS_SA_DB_PASSWORD: WideString;
function HS_SA_DB_USER: WideString;
function HS_ALUV4_HMAC_SECRET: WideString;
function make_code(KundenNr: integer; SerienNr: integer; Dateianfrage: integer): WideString;

implementation

uses
  Windows, hl_HSK;

function IstHickelSoftTestPC: boolean;
begin
  if not Trusted_HSK then exit(false);
  result := HSK_0001;
end;

function PruefeHickelSoftPassword(const pwd: WideString): boolean;
begin
  if not Trusted_HSK then exit(false);
  result := HSK_0002(PWideChar(pwd));
end;

function HS_SA_DB_PASSWORD: WideString;
var
  Len: DWORD;
begin
  if not Trusted_HSK then exit('');
  Len := HSK_0003(HS_SA_DB_OLD, nil);
  SetLength(Result, Len);
  if Len > 0 then
  begin
    Assert(Len = HSK_0003(HS_SA_DB_OLD, PWideChar(Result)));
    SetLength(Result, Len-1); // Null Terminator entfernen
  end;
end;

function HS_SA_DB_USER: WideString;
var
  Len: DWORD;
begin
  if not Trusted_HSK then exit('');
  Len := HSK_0004(HS_SA_DB_OLD, nil);
  SetLength(Result, Len);
  if Len > 0 then
  begin
    Assert(Len = HSK_0004(HS_SA_DB_OLD, PWideChar(Result)));
    SetLength(Result, Len-1); // Null Terminator entfernen
  end;
end;

function HS_ALUV4_HMAC_SECRET: WideString;
var
  Len: DWORD;
begin
  if not Trusted_HSK then exit('');
  Len := HSK_0005(nil);
  SetLength(Result, Len);
  if Len > 0 then
  begin
    Assert(Len = HSK_0005(PWideChar(Result)));
    SetLength(Result, Len-1); // Null Terminator entfernen
  end;
end;

function make_code(KundenNr: integer; SerienNr: integer; Dateianfrage: integer): WideString;
var
  Len: DWORD;
begin
  if not Trusted_HSK then exit('');
  Len := HSK_0006(KundenNr, SerienNr, Dateianfrage, nil);
  SetLength(Result, Len);
  if Len > 0 then
  begin
    Assert(Len = HSK_0006(KundenNr, SerienNr, Dateianfrage, PWideChar(Result)));
    SetLength(Result, Len-1); // Null Terminator entfernen
  end;
end;

end.
