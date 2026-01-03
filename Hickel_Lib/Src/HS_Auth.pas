unit HS_Auth;

interface

const HS_ALUV4_HMAC_SECRET = ''; // REDACTED IN OPEN SOURCE RELEASE
function HS_SA_DB_USER: string;
function HS_SA_DB_PASSWORD: string;
function IstHickelSoftTestPC: boolean;
function PruefeHickelSoftPassword(const pwd: string): boolean;

implementation

function HS_SA_DB_USER: string;
begin
  // REDACTED IN OPEN SOURCE RELEASE
  result := '';
end;

function HS_SA_DB_PASSWORD: string;
begin
  // REDACTED IN OPEN SOURCE RELEASE
  result := '';
end;

function IstHickelSoftTestPC: boolean;
begin
  // REDACTED IN OPEN SOURCE RELEASE
  result := false;
end;

function PruefeHickelSoftPassword(const pwd: string): boolean;
begin
  // REDACTED IN OPEN SOURCE RELEASE
  result := false;
end;

end.
