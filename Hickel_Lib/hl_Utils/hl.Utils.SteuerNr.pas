unit hl.Utils.SteuerNr;

interface

type
  TSteuernummern = record
    SteuerNr: string;
    UstIdNr: string;
  end;

function DetectUstId_Deutschland(s: string): string;
function DetectSteuerNr_Deutschland(s: string): string;
function ErkenneSteuerNrUndUstId(SteuerNr, UStId: string): TSteuernummern;

implementation

uses
  SysUtils;

function DetectUstId_Deutschland(s: string): string;
var
  i: Integer;
  stmp: string;
  pUstId: Integer;
  pUstId2: Integer;
begin
  s := StringReplace(s, 'de', 'DE', [rfIgnoreCase, rfReplaceAll]);
  s := StringReplace(s, ' ', '', [rfIgnoreCase, rfReplaceAll]);

  {$REGION 'Sonderzeichen wegfiltern'}
  for i := 1 to Length(s) do
  begin
    if not (s[i] in ['0'..'9','A'..'Z','a'..'z','Ä','ä','Ö','ö','Ü','ü','ß']) then
      s[i] := '~';
  end;
  s := StringReplace(s, '~', '', [rfReplaceAll]);
  {$ENDREGION}

  {$REGION 'Zahlen erkennen'}
  stmp := StringReplace(s, '@', '|', [rfReplaceAll]);
  for i := 1 to Length(stmp) do
  begin
    if stmp[i] in ['0'..'9'] then stmp[i] := '@';
  end;
  {$ENDREGION}

  pUstId := Pos('DE@@@@@@@@@', stmp); // genau richtig (9 Zeichen)
  pUstId2 := Pos('DE@@@@@@@@@@', stmp); // zu lang (10 Zeichen)

  if (pUstId > 0) and (pUstId2 = 0) then
  begin
    result := Copy(s, pUstId);
  end
  else
  begin
    // Nicht erkannt
    result := '';
  end;
end;

function DetectSteuerNr_Deutschland(s: string): string;
var
  i: Integer;
  stmp: string;
  pStNr: Integer;
  pStNr2: Integer;
begin
  {$REGION 'Sonderzeichen wegfiltern'}
  for i := 1 to Length(s) do
  begin
    if not (s[i] in ['0'..'9','A'..'Z','a'..'z','Ä','ä','Ö','ö','Ü','ü','ß']) then
      s[i] := '~';
  end;
  s := StringReplace(s, '~', '', [rfReplaceAll]);
  {$ENDREGION}

  {$REGION 'Zahlen erkennen'}
  stmp := StringReplace(s, '@', '|', [rfReplaceAll]);
  for i := 1 to Length(stmp) do
  begin
    if stmp[i] in ['0'..'9'] then stmp[i] := '@';
  end;
  {$ENDREGION}

  (*
  gem. https://de.wikipedia.org/wiki/Steuernummer
  Standardschema der Länder immer 10-11 Zahlen lang
  Vereinheitlichtes Bundesschema (12-stellige Steuernummer)
  Vereinheitlichtes Bundesschema zur elektronischen Übermittlung (13-stellige Steuernummer)
  *)
  pStNr :=  Pos('@@@@@@@@@@', stmp); // richtig (mind. 10 Zeichen)
  pStNr2 := Pos('@@@@@@@@@@@@@@', stmp); // zu lang (14 Zeichen)

  if (pStNr > 0) and (pStNr2 = 0) then
  begin
    result := Copy(s, pStNr);
  end
  else
  begin
    // Nicht erkannt
    result := '';
  end;
end;

function ErkenneSteuerNrUndUstId(SteuerNr, UStId: string): TSteuernummern;
var
  sTmpUstIdInUstId: string;
  sTmpUstIdInStNr: string;
begin
  // Manche Kunden tragen in STEUERNR alles mögliche ein, z.B. "SteuerNr xxx" weil
  // die Formulare keinen Vortext hatten, und sich die Kunden selbst behelfen mussten!!!
  // Wir versuchen das Zeug so gut wie möglich zu filtern!

  sTmpUstIdInUstId := DetectUstId_Deutschland(UstId);
  sTmpUstIdInStNr  := DetectUstId_Deutschland(SteuerNr);

  {$REGION 'UStId Nummer erkennen'}
  if sTmpUstIdInUstId <> '' then
  begin
    result.UstIdNr := sTmpUstIdInUstId;
  end
  else if sTmpUstIdInStNr <> '' then
  begin
    result.UstIdNr := sTmpUstIdInStNr;
    SteuerNr := ''; // nicht mehr als SteuerNr verwenden, wenn bereits als UstId verwendet wurde
  end
  else
  begin
    result.UstIdNr := '';
  end;
  {$ENDREGION}

  result.SteuerNr := DetectSteuerNr_Deutschland(SteuerNr);
end;

end.
