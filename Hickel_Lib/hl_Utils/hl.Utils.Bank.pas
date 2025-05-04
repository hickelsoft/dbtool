unit hl.Utils.Bank;

interface

uses
  SysUtils;

type
  ThlBankUtils = class(TObject)
  private
    class function ChangeAlpha(input: string): string;
    class function CalculateDigits(iban: string): Integer;
  public
    class function FormatIBAN(iban: string): string; static;

    class function FormatBLZ(blz: string): string; static;

    /// <see>http://www.swissdelphicenter.ch/torry/showcode.php?id=1470</see>
    class function CheckIBAN(iban: string): Boolean;

    class function CheckBIC(bic: string): Boolean;
    class function CheckKTO(kto: string): Boolean;
    class function CheckBLZ(blz: string): Boolean;

    /// <remarks>Bearbeitet</remarks>
    /// <see>Quelle: http://www.delphipraxis.net/176530-pruefziffer-fuer-iban-berechnen.html</see>
    class function CreateGermanIBAN(KontoNr, blz: String): String; static;
  end;

implementation

uses
  hl.Utils;

class function ThlBankUtils.ChangeAlpha(input: string): string;
// A -> 10, B -> 11, C -> 12 ...
var
  a: Char;
begin
  Result := input;
  for a := 'A' to 'Z' do
  begin
    Result := StringReplace(Result, a, IntToStr(Ord(a) - 55), [rfReplaceAll]);
  end;
end;

class function ThlBankUtils.CalculateDigits(iban: string): Integer;
var
  v, l: Integer;
  alpha: string;
  number: Longint;
  rest: Integer;
begin
  iban := UpperCase(iban);
  if Pos('IBAN', iban) > 0 then
    Delete(iban, Pos('IBAN', iban), 4);
  iban := iban + Copy(iban, 1, 4);
  Delete(iban, 1, 4);
  iban := ChangeAlpha(iban);
  v := 1;
  l := 9;
  rest := 0;
  alpha := '';
  try
    while v <= Length(iban) do
    begin
      if l > Length(iban) then
        l := Length(iban);
      alpha := alpha + Copy(iban, v, l);
      number := StrToInt(alpha);
      rest := number mod 97;
      v := v + l;
      alpha := IntToStr(rest);
      l := 9 - Length(alpha);
    end;
  except
    on E: EAbort do
    begin
      Abort;
    end;
    on E: Exception do
    begin
      rest := 0;
    end;
  end;
  Result := rest;
end;

class function ThlBankUtils.CheckBIC(bic: string): Boolean;
begin
  // TODO: bessere validierungs-routine, siehe http://bank-code.net/swift-code/DEMMUS31XXX.html
  Result := (Length(bic) = 8) or (Length(bic) = 11);
end;

class function ThlBankUtils.CheckBLZ(blz: string): Boolean;
begin
  Result := Length(blz) = 8;
end;

class function ThlBankUtils.CheckIBAN(iban: string): Boolean;
begin
  iban := StringReplace(iban, ' ', '', [rfReplaceAll]);

  // TODO: Länge in Zukunft auch prüfen... (siehe Wikipedia)

  if CalculateDigits(iban) = 1 then
    Result := True
  else
    Result := False;
end;

class function ThlBankUtils.CheckKTO(kto: string): Boolean;
var
  v: int64;
  ec: Integer;
begin
  Val(kto, v, ec);
  Result := (ec = 0) and (v > -1);
  // ec=0 heißt, dass es ein gültiger Integer ist
end;

class function ThlBankUtils.CreateGermanIBAN(KontoNr, blz: String): String;
var
  cs, i, cc: Integer;
  s: string;
begin
  if Length(KontoNr) < 10 then
  begin
    KontoNr := ThlUtils.AddLeadingZeroes(StrToInt(KontoNr), 10);
  end;
  if (Length(blz) <> 8) or (Length(KontoNr) > 10) then
  begin
    Result := '';
    Exit;
  end;
  s := blz + KontoNr + '131400'; // 131400 = 'DE00'
  cs := 0;
  for i := 1 to Length(s) do
  begin
    cs := (cs * 10 + Ord(s[i]) - Ord('0')) mod 97;
  end;
  cc := 98 - cs;
  if cc < 2 then
    inc(cc, 97); // 00-->97, 01--> 98
  Result := 'DE00';
  Result[3] := Chr(cc div 10 + Ord('0'));
  Result[4] := Chr(cc mod 10 + Ord('0'));
  Result := Result + blz + KontoNr;
end;

class function ThlBankUtils.FormatIBAN(iban: string): string;
begin
  Result := ThlUtils.InBlöckeAufspalten(iban, 4);
end;

class function ThlBankUtils.FormatBLZ(blz: string): string;
begin
  // 390 601 90
  Result := Copy(blz, 1, 3) + ' ' + Copy(blz, 4, 3) + ' ' + Copy(blz, 7, 2);
end;

end.
