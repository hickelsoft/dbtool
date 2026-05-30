unit HS_MidVer;

interface

uses
  Windows;

procedure MidVer_Start(AProduct: WideString; ASerienNr: integer; ADbName, ADbOwnerSid, ASqlServerMac: WideString);
procedure MidVer_Stop;

implementation

uses
  hl_HSK_Client;

procedure MidVer_Start(AProduct: WideString; ASerienNr: integer; ADbName, ADbOwnerSid, ASqlServerMac: WideString);
begin
  if not Trusted_HSK then exit;
  HSK_0009(PWideChar(AProduct), ASerienNr, PWideChar(ADbName), PWideChar(ADbOwnerSid), PWideChar(ASqlServerMac));
end;

procedure MidVer_Stop;
begin
  if not Trusted_HSK then exit;
  HSK_0010;
end;

end.
