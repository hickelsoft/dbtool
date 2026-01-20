unit hl.Utils.Color;

interface

uses
  Graphics, GraphUtil, Math;

function GetWindowsAccentColor: TColor;
function DecreaseColorLightness(c: TColor; amount: byte): TColor;
function IncreaseColorLightness(c: TColor; amount: byte): TColor;
function GetColorLightness(c: TColor): byte;
function SetColorLightness(c: TColor; L: byte): TColor;
function GetContrastColorBlackOrWhite(AColor: TColor): TColor;

const
  COLOR_LIGHTNESS_MIN = 0;
  COLOR_LIGHTNESS_MAX = 240;

implementation

uses
  Windows, Registry, SysUtils;

function GetWindowsAccentColor: TColor;
var
  Reg: TRegistry;
begin
  result := TColor(ColorToRGB(clActiveCaption));

  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKeyReadOnly('Software\Microsoft\Windows\DWM') then
    begin
      result := TColor(Reg.ReadInteger('AccentColor') and $FFFFFF);
      Reg.CloseKey;
    end;
  finally
    FreeAndNil(Reg);
  end;
end;

function GetContrastColorBlackOrWhite(AColor: TColor): TColor;
var
  R, G, B: single;
begin
  AColor := ColorToRGB(AColor);

  R := GetRValue(AColor) * 0.25;
  G := GetGValue(AColor) * 0.625;
  B := GetBValue(AColor) * 0.125;

  if (R + G + B) > 128 then
  begin
    result := clBlack;
  end
  else
  begin
    result := clWhite;
  end;
end;

function IncreaseColorLightness(c: TColor; amount: byte): TColor;
var
  H, S, L: Word;
begin
  c := ColorToRGB(c);
  ColorRGBToHLS(c, H, L, S);
  result := ColorHLSToRGB(H, Min(L + amount, $FF), S);
end;

function DecreaseColorLightness(c: TColor; amount: byte): TColor;
var
  H, S, L: Word;
begin
  c := ColorToRGB(c);
  ColorRGBToHLS(c, H, L, S);
  result := ColorHLSToRGB(H, Max(L - amount, $00), S);
end;

function GetColorLightness(c: TColor): byte;
var
  H, S, L: Word;
begin
  c := ColorToRGB(c);
  ColorRGBToHLS(c, H, L, S);
  result := L;
end;

function SetColorLightness(c: TColor; L: byte): TColor;
var
  safeCounter: integer;
begin
  c := ColorToRGB(c);
  result := c;
  safeCounter := 0; // um Endlosschleifen zu verhindern
  if GetColorLightness(result) < L then
    repeat
      result := IncreaseColorLightness(result, 1);
      inc(safeCounter);
    until (GetColorLightness(result) >= L) or (safeCounter > 1000)
  else
    repeat
      result := DecreaseColorLightness(result, 1);
      inc(safeCounter);
    until (GetColorLightness(result) <= L) or (safeCounter > 1000);
end;

end.
