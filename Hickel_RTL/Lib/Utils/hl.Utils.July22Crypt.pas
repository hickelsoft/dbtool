unit hl.Utils.July22Crypt;

interface

function July22Crypt(passwort: WideString; do_encrypt: boolean): WideString;

implementation

uses
  Windows, hl_HSK_Client;

function July22Crypt(passwort: WideString; do_encrypt: boolean): WideString;
var
  Len: DWORD;
begin
  if not Trusted_HSK then exit('');
  Len := HSK_0007(PWideChar(passwort), do_encrypt, nil);
  SetLength(Result, Len);
  if Len > 0 then
  begin
    Assert(Len = HSK_0007(PWideChar(passwort), do_encrypt, PWideChar(Result)));
    SetLength(Result, Len-1); // Null Terminator entfernen
  end;
end;

end.
