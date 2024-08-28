unit HsTools;

interface

uses
  Windows, Forms, Types, Classes, SysUtils, Graphics, mmSystem, Math;

var
   HsZehnerpotenzen: array[0..18] of extended;  // Intern benutzt für HsRound

type
   eHsToolsError = class(exception);

const
   cGaugeSteps = 255;  // Anzahl der Farben im Farbverlauf der HsGauge (kleinere Zahl = schneller, sieht aber nicht so gut aus)

// Hilfsroutinen, die ich im Delphi vermisst habe:
//function GetFileVersion(FileName: string): string;
function HsRound(dValue: double; iAnzahl: integer): double;       // Rundet dValue auf iAnzahl Nachkommastellen
function GetCheckSum(sText: string; iLength: integer): char;      // Simple Prüfsummenberechnung
function Splice(var sString: string; cSeparator: char): string;   // Liefert den linken Teil von sString bis zum ersten cSeparator zurück, verkleinert sString
function Calc(sExpression: string): double;                       // Berechnet mathematische Ausdrücke mit "Punkt-vor-Strich". Vorsicht: Ohne Syntax-Überprüfung!

// Bitmap-Funktionen
procedure CreateColorFadeBitmap(aBitmap: TBitmap; Height, Width: integer; StartColor, StopColor: TColor; Vertical: boolean);
procedure TileBitmap(Dst, Src: TBitmap);

// CSV-Routinen
procedure CsvToSl(aCsv: string; var aList: TStringList);
function SlToCsv(aList: TStringList): string;
function ArrayToCsv(aList: array of string): string;

// Komprimierung
function RleCompress(sSrc: string): string;
function RleDeCompress(sSrc: string): string;

function Make_EditDisplayFormat(nachkommastellen: integer; istEditFormat: boolean): string;


implementation


function Make_EditDisplayFormat(nachkommastellen: integer; istEditFormat: boolean): string;
begin
  result := '';
  result := result + StringOfChar('#', nachkommastellen);
  if not istEditFormat then
  begin
    result := result + ',';
  end;
  result := result + '##0.' + StringOfChar('0', nachkommastellen);
end;

procedure InitHsTools;
var
  iCounter: integer;
begin
   for iCounter := 0 to 18 do HsZehnerpotenzen[iCounter] := intpower(10, iCounter);
end;

function Splice(var sString: string; cSeparator: char): string;
begin
   Result := copy(sString, 1, pos(cSeparator, sString)-1);
   sString := copy(sString, pos(cSeparator, sString)+1, Length(sString));
end;

function HsRound(dValue: double; iAnzahl: integer): double;
begin
   if dValue > 0 then Result := int((HsZehnerpotenzen[iAnzahl] * dValue + 0.5)) / HsZehnerpotenzen[iAnzahl]
   else Result := int((HsZehnerpotenzen[iAnzahl] * dValue - 0.5)) / HsZehnerpotenzen[iAnzahl];
end;

procedure CreateColorFadeBitmap(aBitmap: TBitmap; Height, Width: integer; StartColor, StopColor: TColor; Vertical: boolean);
var
   iCounter, iBuffer: integer;
   bR1, bG1, bB1: byte;
   bR2, bG2, bB2: byte;
   aColor1, aColor2: LongInt;
   dCurrentR, dCurrentG, dCurrentB, dRStep, dGStep, dBStep: double;  // für mehr Geschwindigkeit!
   iFillStep: integer;

begin
   aBitmap.Height := Height;
   aBitmap.Width := Width;
   aBitmap.Canvas.Pen.Style := psClear;
   aColor1 := ColorToRGB(StartColor);
   aColor2 := ColorToRGB(StopColor);
   bR1 := GetRValue(aColor1);
   bG1 := GetGValue(aColor1);
   bB1 := GetBValue(aColor1);
   bR2 := GetRValue(aColor2);
   bG2 := GetGValue(aColor2);
   bB2 := GetBValue(aColor2);

   dCurrentR := bR1;
   dCurrentG := bG1;
   dCurrentB := bB1;

   dRStep := (bR2-bR1) / cGaugeSteps;
   dGStep := (bG2-bG1) / cGaugeSteps;
   dBStep := (bB2-bB1) / cGaugeSteps;

   if Vertical then
   begin
      iFillStep := (Height div cGaugeSteps) + 1;
      for iCounter := 0 to cGaugeSteps do
      begin
         iBuffer := iCounter * Height div cGaugeSteps;
         aBitmap.Canvas.Brush.Color := rgb(trunc(dCurrentR), trunc(dCurrentG), trunc(dCurrentB));
         dCurrentR := dCurrentR + dRStep;
         dCurrentG := dCurrentG + dGStep;
         dCurrentB := dCurrentB + dBStep;
         aBitmap.Canvas.FillRect(Rect(0, iBuffer, Width, iBuffer + iFillStep));
      end;
   end
   else
   begin
      iFillStep := (Width div cGaugeSteps) + 1;
      for iCounter := 0 to cGaugeSteps do
      begin
         iBuffer := Width * iCounter div cGaugeSteps;
         aBitmap.Canvas.Brush.Color := rgb(trunc(dCurrentR), trunc(dCurrentG), trunc(dCurrentB));
         dCurrentR := dCurrentR + dRStep;
         dCurrentG := dCurrentG + dGStep;
         dCurrentB := dCurrentB + dBStep;
         aBitmap.Canvas.FillRect(Rect(iBuffer, 0, iBuffer + iFillStep, Height));
      end;
   end;
end;

procedure TileBitmap(Dst, Src: TBitmap);
var
   X, Y: integer;
   aRect, bRect: TRect;
   iWidth, iHeight: integer;

begin
   iWidth := Src.Width;
   iHeight := Src.Height;

   if (iWidth=0) or (iHeight=0) then exit;

   bRect.Left := 0;
   bRect.Top := 0;
   bRect.Right := iWidth;
   bRect.Bottom := iHeight;

   aRect.Left := 0;
   aRect.Top := 0;
   aRect.Right := iWidth;
   aRect.Bottom := iHeight;

   for X := 0 to (Dst.Width div iWidth) + 1 do
   begin
      for Y := 0 to (Dst.Height div iHeight) + 1 do
      begin
         Dst.Canvas.CopyRect(aRect, Src.Canvas, bRect);
         aRect.Top := aRect.Bottom;
         aRect.Bottom := aRect.Bottom + iHeight;
      end;
      aRect.Left := aRect.Right;
      aRect.Right := aRect.Right + iWidth;
      aRect.Top := 0;
      aRect.Bottom := iHeight;
   end;
end;

procedure CsvToSl(aCsv: string; var aList: TStringList);
var
   iCounter: integer;
   sBuffer: string;
   bQuoting: boolean;

begin
   aList.Clear;
   aCsv := aCsv + ';';
   iCounter := 1;
   sBuffer := '';
   bQuoting := false;

   while iCounter <= Length(aCsv) do
   begin
      if aCsv[iCounter] = '"' then
      begin
         if (aCsv[iCounter+1] = '"') and (bQuoting = true) then
         begin
            sBuffer := sBuffer + '"';
            inc(iCounter);
         end
         else
            bQuoting := not bQuoting;
      end
      else
      begin
         if (aCsv[iCounter] = ';') and (bQuoting = false) then
         begin
            aList.Add(sBuffer);
            sBuffer := '';
         end
         else
            if bQuoting = true then sBuffer := sBuffer + aCsv[iCounter];
      end;
      inc(iCounter);
   end;
end;

function SlToCsv(aList: TStringList): string;
var
   iCounter: integer;

begin
   Result := '"' + StringReplace(aList[0], '"', '""', [rfReplaceAll]) + '"';
   for iCounter := 1 to aList.Count-1 do
      Result := Result + ';"' + StringReplace(aList[iCounter], '"', '""', [rfReplaceAll]) + '"';
end;

function ArrayToCsv(aList: array of string): string;
var
   iCounter: integer;

begin
   Result := '"' + StringReplace(aList[0], '"', '""', [rfReplaceAll]) + '"';
   for iCounter := 1 to High(aList) do
      Result := Result + ';"' + StringReplace(aList[iCounter], '"', '""', [rfReplaceAll]) + '"';
end;

function RleCompress(sSrc: string): string;
var
   iCounter: integer;
   cLastChar: char;
   bAnzahl: byte;
   sResult: string;

begin
   if Length(sSrc) < 3 then
   begin
      Result := sSrc;
      exit;
   end;

   sResult := '';
   bAnzahl := 1;
   cLastChar := sSrc[1];

   for iCounter := 2 to Length(sSrc) do
   begin
      if (sSrc[iCounter] = cLastChar) and (bAnzahl < 250) then inc(bAnzahl)
      else
      begin
         if (bAnzahl > 2) or (cLastChar = #$ff) or (cLastChar = #$ff) then sResult := sResult + #$ff + chr(bAnzahl) + cLastChar
         else sResult := sResult + StringOfChar(cLastChar, bAnzahl);

         cLastChar := sSrc[iCounter];
         bAnzahl := 1;
      end;
   end;
   if (bAnzahl > 2) or (cLastChar = #$ff) then sResult := sResult + #$ff + chr(bAnzahl) + cLastChar
   else sResult := sResult + StringOfChar(cLastChar, bAnzahl);

   // Jetzt noch alle #$fe als #$ff#$ff#$ff verstecken
   Result := StringReplace(sResult, #$fe, #$ff#$ff#$ff, [rfReplaceAll]);
end;

function RleDeCompress(sSrc: string): string;
var
   iCounter: integer;

begin
   Result := '';
   // Versteckte Bytes "wiederbeleben"
   sSrc := StringReplace(sSrc, #$ff#$ff#$ff, #$fe, [rfReplaceAll]);

   iCounter := 1;
   while iCounter <= Length(sSrc) do
   begin
      if sSrc[iCounter] <> #$ff then Result := Result + sSrc[iCounter]
      else
      begin
         Result := Result + StringOfChar(sSrc[iCounter+2], ord(sSrc[iCounter+1]));
         inc(iCounter, 2);
      end;
      inc(iCounter);
   end;
end;

function GetCheckSum(sText: string; iLength: integer): char;
var
   iCounter: integer;
   bCheck: byte;

begin
   if Length(sText) < iLength then raise eHsToolsError.Create('GetCheckSum: Parameter fehlerhaft');
   bCheck := 170;
   for iCounter := 1 to iLength do bCheck := bCheck xor ord(sText[iCounter]);
   Result := chr(bCheck);
end;

function Calc(sExpression: string): double;
var
   iCounter: integer;      { Leseposition }
   cChar: char;            { aktuelles Zeichen }
   sCurrent: string;       { aktuelles Token }
   slPieces: tstringlist;  { Für den in Stücke zerlegten Ausdruck }
   slStack: tstringlist;   { Für den Stack bei Klammerungen }
   sOutput: string;        { In diesem String steht der Ergebnis-P-Code }

{
   Werte für sOutput (P-Code):
   ---------------------------
   A = Folgenden 8-Byte-Wert in A laden
   B = Folgenden 8-Byte-Wert in B laden
   C = A PUSHen
   D = B PUSHen
   E = A POPpen
   F = B POPpen
   G = Berechne A=A+B
   H = Berechne A=B-A
   I = Berechne A=A*B
   J = Berechne A=A/B
}

   function AddValue(sValue: string): string;
   begin
      Result := format('%3.3d%s', [Length(sValue), sValue]);
   end;

   procedure ClearStack;
   begin
      while slStack.Count > 0 do
      begin
         sOutput := sOutput + 'F';
         case slStack[0][1] of
            '+': sOutput := sOutput + 'G';
            '-': sOutput := sOutput + 'H';
            '*': sOutput := sOutput + 'I';
            '/': sOutput := sOutput + 'J';
         end;

         slStack.Delete(0);
      end;
   end;

   function InternalCalc(sExpression: string): double;  // P-Code ausrechnen
   var
      cBuffer: char;
      dA: double;
      dB: double;
      slStack: TStringList;

      function GetValue(var sValue: string): string;
      var
         iBuffer: integer;

      begin
         iBuffer := strtoint(copy(sValue, 1, 3));
         Result := copy(sValue, 4, iBuffer);
         sValue := copy(sValue, 4+iBuffer, 65535);
      end;

   begin
      slStack := TStringList.Create;
      try
        dA := 0;
        dB := 0;

        while sExpression <> '' do
        begin
           cBuffer := sExpression[1];
           sExpression := copy(sExpression, 2, 65535);

           case cBuffer of
              'A': dA := strtofloat(GetValue(sExpression));
              'B': dB := strtofloat(GetValue(sExpression));
              'C': slStack.Insert(0, floattostr(dA));
              'D': slStack.Insert(0, floattostr(dB));
              'E': begin dA := strtofloat(slStack[0]); slStack.Delete(0); end;
              'F': begin dB := strtofloat(slStack[0]); slStack.Delete(0); end;
              'G': dA := dA + dB;
              'H': dA := dB - dA;
              'I': dA := dA * dB;
              'J': begin
                     if dB <> 0 then
                       dA := dA / dB
                     else
                     begin
                       raise eHsToolsError.Create('Division durch Null');
                     end;
                   end;
           end;
        end;
        Result := dA;
      finally
        FreeAndNil(slStack);
      end;
   end;

begin
   slPieces := tstringlist.create;
   slStack := tstringlist.create;
   try
     sCurrent := '';

     { Den Ausdruck in seine Bestandteile zerlegen, Syntaxprüfung }
     for iCounter := 1 to Length(sExpression) do
     begin
       cChar := sExpression[iCounter];

        {$IFDEF Unicode}
        if CharInSet(cChar, ['0'..'9', ',']) then  { Ziffer }
        {$ELSE}
        if cChar in ['0'..'9', ','] then  { Ziffer }
        {$ENDIF}
        begin
           sCurrent := sCurrent + cChar;
        end
        else
        {$IFDEF Unicode}
        if CharInSet(cChar, ['+', '-', '*', '/']) then  { Operand }
        {$ELSE}
        if cChar in ['+', '-', '*', '/'] then  { Operand }
        {$ENDIF}
        begin
           if sCurrent <> '' then slPieces.Add(sCurrent);
           slPieces.Add(cChar);
           sCurrent := '';
        end;
     end;
     if sCurrent <> '' then slPieces.Add(sCurrent);

    { Den zerlegten Ausdruck in P-Code umwandeln }
     slStack.Clear;
     sOutput := 'A' + AddValue(slPieces[0]);
    iCounter := 1;
    while iCounter < slPieces.Count do
     begin
        case slPieces[iCounter][1] of
           '+': begin ClearStack; slStack.Insert(0, '+'); sOutput := sOutput + 'CA' + AddValue(slPieces[iCounter+1]); end;
           '-': begin ClearStack; slStack.Insert(0, '-'); sOutput := sOutput + 'CA' + AddValue(slPieces[iCounter+1]); end;
           '*': sOutput := sOutput + 'B' + AddValue(slPieces[iCounter+1]) + 'I';
           '/': sOutput := sOutput + 'B' + AddValue(slPieces[iCounter+1]) + 'J';
        end;
        inc(iCounter, 2);
     end;

     { Umwandlung fertig, also Stack räumen! }
     ClearStack;
   finally
     FreeAndNil(slStack);
     FreeAndNil(slPieces);
   end;

   { Und jetzt wird der PCode ausgerechnet und das Ergebnis zurückgegeben! }
   Result := InternalCalc(sOutput);
end;

initialization
   InitHsTools;

end.
