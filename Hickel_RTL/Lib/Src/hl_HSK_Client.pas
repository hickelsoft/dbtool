unit hl_HSK_Client;

// REDACTED IN OPEN SOURCE RELEASE
// (REPLACED BY DUMMY IMPLEMENTATION)

interface

uses
  Windows;

function Trusted_HSK: boolean;
function HSK_0000(A: UInt64): UInt64;
function HSK_0001: BOOL;
function HSK_0002(A: PWideChar): BOOL;
function HSK_0003(A: BOOL; B: PWideChar): DWORD;
function HSK_0004(A: BOOL; B: PWideChar): DWORD;
function HSK_0005(A: PWideChar): DWORD;
function HSK_0006(A: integer; B: integer; C: integer; D: PWideChar): DWORD;
function HSK_0007(A: PWideChar; B: BOOL; C: PWideChar): DWORD;
function HSK_0008: DWORD;
procedure HSK_0009(A: WideString; B: integer; C: Int64);
procedure HSK_0010;
procedure HSK_0011(A: PWideChar; B: PByte);
function HSK_0012(A: PWideChar): DWORD;
function HSK_0013: Double;

implementation

function Trusted_HSK: boolean;
begin
  exit(true);
end;

function HSK_0000(A: UInt64): UInt64;
begin
  exit(0);
end;

function HSK_0001: BOOL;
begin
  exit(false);
end;

function HSK_0002(A: PWideChar): BOOL;
begin
  exit(false);
end;

function HSK_0003(A: BOOL; B: PWideChar): DWORD;
begin
  exit(0);
end;

function HSK_0004(A: BOOL; B: PWideChar): DWORD;
begin
  exit(0);
end;

function HSK_0005(A: PWideChar): DWORD;
begin
  exit(0);
end;

function HSK_0006(A: integer; B: integer; C: integer; D: PWideChar): DWORD;
begin
  exit(0);
end;

function HSK_0007(A: PWideChar; B: BOOL; C: PWideChar): DWORD;
begin
  exit(0);
end;

function HSK_0008: DWORD;
begin
  exit(0);
end;

procedure HSK_0009(A: WideString; B: integer; C: Int64);
begin
  exit;
end;

procedure HSK_0010;
begin
  exit;
end;

procedure HSK_0011(A: PWideChar; B: PByte);
begin
  exit;
end;

function HSK_0012(A: PWideChar): DWORD;
begin
  exit(0);
end;

function HSK_0013: Double;
begin
  exit(0);
end;

end.