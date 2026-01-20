unit hl.System.Types;

interface

uses
  System.Types, System.Math, System.SysUtils;

{$REGION 'Operatoren Vorlage'}
// http://docwiki.embarcadero.com/RADStudio/Seattle/de/%C3%9Cberladene_Operatoren_%28Delphi%29
(*
  class operator Add(const Left, Right: Decimal): Decimal;
  class operator Dec(const D: Decimal): Decimal;
  class operator Divide(const Left, Right: Decimal): Decimal;
  class operator Equal(const Left, Right: Decimal): Boolean;
  class operator GreaterThan(const Left, Right: Decimal): Boolean;
  class operator GreaterThanOrEqual(const Left, Right: Decimal): Boolean;
  class operator Implicit(const C: Cardinal): Decimal;
  class operator Implicit(const c: Currency): Decimal;
  class operator Implicit(const D: Decimal): Currency;
  class operator Implicit(const D: Decimal): Extended;
  class operator Implicit(const D: Decimal): Int64;
  class operator Implicit(const D: Decimal): Longint;
  class operator Implicit(const D: Decimal): Longword;
  class operator Implicit(const D: Decimal): string;
  class operator Implicit(const D: Decimal): UInt64;
  class operator Implicit(const D: Double): Decimal;
  class operator Implicit(const E: Extended): Decimal;
  class operator Implicit(const I: Integer): Decimal;
  class operator Implicit(const I64: Int64): Decimal;
  class operator Implicit(const S: Single): Decimal;
  class operator Implicit(const S: string): Decimal;
  class operator Implicit(const UI64: UInt64): Decimal;
  class operator Inc(const D: Decimal): Decimal;
  class operator LessThan(const Left, Right: Decimal): Boolean;
  class operator LessThanOrEqual(const Left, Right: Decimal): Boolean;
  class operator Modulus(const Dividend, Divisor: Decimal): Decimal;
  class operator Multiply(const Left, Right: Decimal): Decimal;
  class operator Negative(const D: Decimal): Decimal;
  class operator NotEqual(const Left, Right: Decimal): Boolean;
  class operator Positive(const D: Decimal): Decimal;
  class operator Round(const D: Decimal): Decimal;
  class operator Subtract(const Left, Right: Decimal): Decimal;
  class operator Trunc(const D: Decimal): Decimal;
*)
{$ENDREGION}

type
  TIntegerArray = array of Integer;

{$REGION 'Interface hlString'}

type
  hlString = record
  strict private
    wert: string;

    // (DM intern, Ticket 29653, Punkt 2, Anrufliste und ggf. weitere Programmteile)
    // Dieser Dummy ist notwendig, um einen Compilerbug in Delphi 2007 zu umgehen.
    // Es muss ein zweites Feld, egal welche Größe oder an welcher Position,
    // in den Record eingebracht werden, da sonst das Reference-Counting des Strings
    // beim Kopieren des Records durch Zuweisung nicht erhöht wird.
    // Dies hat zur Folge, dass ein bereits freigegebener String nochmal freigegeben wird,
    // sodass dann hlStrings wie ein Weltmeister den Speicher korrumpiert.
    // Die Speicherkorruption ist sogar so stark, dass sich CORA beim Schließen
    // von einigen Formularen selbstständig beendet, ohne eine Meldung auszugeben!
    // -- DM 17.03.2016
    dummy: byte;
  public
    function Length: Integer;
    function toString: string;
    function toInteger(werfeException: boolean = false): Integer;
    function toSQLString: string;
    function toSQLObjectName(withoutDBO: boolean = false): string;
    function toFileName: string;
    function endsWith(ending: hlString; ignoreCase: boolean = false): boolean;
    function beginsWith(beginning: hlString;
      ignoreCase: boolean = false): boolean;
    function toUpperCase: hlString;
    function toLowerCase: hlString;
    function Trunc(len: Integer): hlString;
    function isEmpty: boolean;
    function trim: hlString;
    function contains(substr: hlString; caseSensitive: boolean = true): boolean;
    function Replace(search, Replace: hlString): hlString;
    function IncludeTrailingPathDelimiter: hlString;
    function LeftPad(howMany: Integer; padChar: char): hlString;
    function RightPad(howMany: Integer; padChar: char): hlString;

    /// <summary>Führt die Copy Funktion durch</summary>
    /// <param name="offset">1-basierender Index im String</param>
    /// <param name="length">Wie lange soll der Ausgabestring sein?</param>
    /// <returns>Geschnittener String</returns>
    function SubString(offset, Length: Integer): hlString;

    class operator Add(const Left: hlString; Right: hlString): hlString;
    class operator Implicit(const s: string): hlString;
    class operator Implicit(const s: hlString): string;
    class operator Implicit(const s: hlString): WideString;
    class operator Implicit(const i: Integer): hlString;
    class operator Equal(const Left, Right: hlString): boolean;
    class operator NotEqual(const Left, Right: hlString): boolean;
    constructor Create(const aWert: string);
  end;
{$ENDREGION}
{$REGION 'Interface hlInteger'}

type
  hlInteger = record
  strict private
    wert: int64;
    min: int64;
    max: int64;
    function EnsureInRange(i: int64): int64;
  public
    function toInteger: Integer;
    function toString: string;
    function toDouble: double;
    function toSQLString: string;
    function CompareTo(i: Integer): TValueRelationship;
    function TryParse(const s: string): boolean;
    function Between(lo, hi: hlInteger;
      einschliesslich: boolean = true): boolean;
    function LowerLimit(limit: hlInteger): hlInteger;
    function UpperLimit(limit: hlInteger): hlInteger;
    class operator Implicit(const i: int64): hlInteger;
    class operator Implicit(const i: Integer): hlInteger;
    class operator Implicit(const i: hlInteger): int64;
    class operator Implicit(const i: hlInteger): string;
    class operator Implicit(const s: string): hlInteger;
    class operator Implicit(const s: hlString): hlInteger;
    class operator Equal(const Left: hlInteger; const Right: int64): boolean;
    class operator Add(const Left: hlInteger; Right: int64): hlInteger;
    class operator Add(const Left: hlString; Right: hlInteger): hlString;
    class operator Subtract(const Left: hlInteger; Right: int64): hlInteger;
    class operator NotEqual(const Left, Right: hlInteger): boolean;
    class operator LessThan(const Left, Right: hlInteger): boolean;
    class operator LessThanOrEqual(const Left, Right: hlInteger): boolean;
    class operator GreaterThan(const Left, Right: hlInteger): boolean;
    class operator GreaterThanOrEqual(const Left, Right: hlInteger): boolean;
    class operator Negative(x: hlInteger): hlInteger;
    function IntMod(teiler: hlInteger): hlInteger;
    constructor Create(wert: int64); overload;
    constructor Create(wert, min, max: int64); overload;
    constructor Create(min, max: int64); overload;
    constructor CreateUInt32(dummy: Integer);
    constructor CreateInt64(dummy: Integer);
  end;
{$ENDREGION}
{$REGION 'Interface hlDecimal'}

type
  hlDecimal = record
  strict private
    wert: double;
    epsilon: Extended;
  public
    function toDouble: double;
    function toSQLString: string;
    function toString: string; overload;
    function toString(gesamtlänge, nachkommastellen: Integer): string; overload;
    function toString(gesamtlänge, nachkommastellen: Integer; fillchar: char)
      : string; overload;
    function CompareTo(d: double): TValueRelationship;
    function RundeToStelle(stelle: Integer): hlDecimal;
    function TryParse(const s: string): boolean;
    function Between(lo, hi: hlDecimal;
      einschliesslich: boolean = true): boolean;
    function LowerLimit(limit: hlDecimal): hlDecimal;
    function UpperLimit(limit: hlDecimal): hlDecimal;
    class operator Implicit(const d: double): hlDecimal;
    class operator Implicit(const d: hlDecimal): double;
    class operator Implicit(const i: hlInteger): hlDecimal;
    class operator Implicit(const s: string): hlDecimal;
    class operator Implicit(const s: hlString): hlDecimal;
    class operator Equal(const Left, Right: hlDecimal): boolean;
    class operator NotEqual(const Left, Right: hlDecimal): boolean;
    class operator LessThan(const Left, Right: hlDecimal): boolean;
    class operator LessThanOrEqual(const Left, Right: hlDecimal): boolean;
    class operator GreaterThan(const Left, Right: hlDecimal): boolean;
    class operator GreaterThanOrEqual(const Left, Right: hlDecimal): boolean;
    class operator Multiply(const Left, Right: hlDecimal): hlDecimal;
    class operator Modulus(const Dividend, Divisor: hlDecimal): hlDecimal;
    class operator Trunc(const d: hlDecimal): hlDecimal;
    class operator Divide(const Left, Right: hlDecimal): hlDecimal;
    class operator Add(const Left, Right: hlDecimal): hlDecimal;
    class operator Subtract(const Left, Right: hlDecimal): hlDecimal;
    class operator Negative(x: hlDecimal): hlDecimal;
    constructor Create(aWert: double; AEpsilon: Extended);
  end;
{$ENDREGION}
{$REGION 'Interface hlBoolean'}

type
  hlBoolean = record
  strict private
    wert: boolean;
    class function StrToBool(s: hlString): boolean; static;
  public
    function toString: string;
    function toBoolean: boolean;
    function toGermanString: string;
    function toSQLString: string;
    class operator Implicit(const s: string): hlBoolean;
    class operator Implicit(const s: hlString): hlBoolean;
    class operator Implicit(const b: boolean): hlBoolean;
    class operator Implicit(const b: hlBoolean): boolean;
    class operator Equal(const Left: hlBoolean; const Right: boolean): boolean;
    constructor Create(aWert: boolean);
  end;
{$ENDREGION}
{$REGION 'Interface hlDateTime'}

type
  hlDateTime = record
  strict private
    wert: TDateTime;
  public
    /// <remarks>Das Ausgabeformat ist ohne geschweifte Klammern und groß geschrieben</remarks>
    function toString: string;

    /// <remarks>Das Ausgabeformat ist ohne geschweifte Klammern und groß geschrieben</remarks>
    function toSQLString: string;

    function toDateTime: TDateTime;

    function Tomorrow: hlDateTime;
    function Yesterday: hlDateTime;
    function Date: hlDateTime;
    function Defined: boolean;

    class operator Implicit(const s: string): hlDateTime;
    class operator Implicit(const s: hlString): hlDateTime;
    class operator Implicit(const b: TDateTime): hlDateTime;
    class operator Implicit(const b: hlDateTime): TDateTime;
    class operator Implicit(const b: hlDateTime): string;
    // class operator Equal(const Left: TDateTime; const Right: TDateTime): boolean;
    class operator Equal(const Left: hlDateTime;
      const Right: hlDateTime): boolean;
    constructor Create(aWert: string); overload;
    constructor Create(aWert: TDateTime); overload;
  end;
{$ENDREGION}
{$REGION 'Interface hlGUID'}

type
  /// <summary>Eine GUID (auch manchmal UUID genannt) ist eine zufällig generierte 128-Bit Zahl, die als weltweit einzigartig angesehen wird, da eine Kollissionswahrscheinlichkeit gegen 0 läuft.</summary>
  hlGUID = record
  strict private
    wert: TGUID;
  public
    /// <remarks>Das Ausgabeformat ist ohne geschweifte Klammern und groß geschrieben</remarks>
    function toString: string;
    function isEmpty: boolean;

    /// <remarks>Das Ausgabeformat ist ohne geschweifte Klammern und groß geschrieben</remarks>
    function toSQLString: string;

    class operator Implicit(const s: string): hlGUID;
    class operator Implicit(const s: hlString): hlGUID;
    class operator Implicit(const b: TGUID): hlGUID;
    class operator Implicit(const b: hlGUID): TGUID;
    class operator Implicit(const b: hlGUID): string;
    // class operator Equal(const Left: TGUID; const Right: TGUID): boolean;
    class operator Equal(const Left: hlGUID; const Right: hlGUID): boolean;
    constructor Create(aWert: string); overload;
    constructor Create(aWert: TGUID); overload;

    /// <summary>Generiert eine neue zufällige GUID.</summary>
    constructor CreateNew(dummy: Integer);

    /// <summary>Erstellt eine GUID, die mit Nullen gefüllt ist. Es ist eine GUID_NULL bzw. Nil UUID IETF RFC 4122, Abschnitt 4.1.7. Siehe auch http://oid-info.com/get/2.25.0; Diese GUID ist für Ausnahmen gedacht und unterliegt keinem Schema der Recommendation ITU-T X.667 | ISO/IEC 9834-8.</summary>
    constructor CreateNull(dummy: Integer);
  end;
{$ENDREGION}

implementation

uses
  FormatSettingsCompat, System.StrUtils, hl.Utils;

resourcestring
  LNG_RANGE_ERROR =
    'Zuzuweisender Wert liegt außerhalb des gültigen Wertebereichs';
  LNG_CONV_ERROR = '"%s" ist kein gültiger Boolean-String';
  StrWahr = 'Wahr';
  StrFalsch = 'Falsch';

{$REGION 'hlDecimal'}

constructor hlDecimal.Create(aWert: double; AEpsilon: Extended);
begin
  self.wert := aWert;
  self.epsilon := AEpsilon;
end;

class operator hlDecimal.Implicit(const d: double): hlDecimal;
begin
  result := hlDecimal.Create(d, 0);
end;

class operator hlDecimal.Implicit(const d: hlDecimal): double;
begin
  result := d.wert;
end;

class operator hlDecimal.Implicit(const s: string): hlDecimal;
begin
  result := hlDecimal.Create(StrToFloat(s), 0);
end;

class operator hlDecimal.Implicit(const s: hlString): hlDecimal;
begin
  result := hlDecimal.Create(StrToFloat(s), 0);
end;

class operator hlDecimal.Implicit(const i: hlInteger): hlDecimal;
begin
  result := hlDecimal.Create(i.toDouble, 0);
end;

function hlDecimal.Between(lo, hi: hlDecimal;
  einschliesslich: boolean = true): boolean;
begin
  if einschliesslich then
  begin
    result := (self >= lo) and (self <= hi);
  end
  else
  begin
    result := (self > lo) and (self < hi);
  end;
end;

class operator hlDecimal.Add(const Left, Right: hlDecimal): hlDecimal;
begin
  result := hlDecimal.Create(Left.wert + Right.wert, 0);
end;

function hlDecimal.CompareTo(d: double): TValueRelationship;
begin
  // Bei epsilon = 0 wird Delphi automatisch ein gutes Epsilon suchen
  result := CompareValue(wert, d, epsilon);
end;

class operator hlDecimal.Equal(const Left, Right: hlDecimal): boolean;
begin
  result := Left.CompareTo(Right.wert) = 0;
end;

class operator hlDecimal.GreaterThan(const Left, Right: hlDecimal): boolean;
begin
  result := Left.CompareTo(Right.wert) > 0;
end;

class operator hlDecimal.GreaterThanOrEqual(const Left,
  Right: hlDecimal): boolean;
begin
  result := Left.CompareTo(Right.wert) >= 0;
end;

class operator hlDecimal.LessThan(const Left, Right: hlDecimal): boolean;
begin
  result := Left.CompareTo(Right.wert) < 0;
end;

class operator hlDecimal.LessThanOrEqual(const Left, Right: hlDecimal): boolean;
begin
  result := Left.CompareTo(Right.wert) <= 0;
end;

function hlDecimal.LowerLimit(limit: hlDecimal): hlDecimal;
begin
  result := System.Math.max(limit, wert);
end;

function hlDecimal.UpperLimit(limit: hlDecimal): hlDecimal;
begin
  result := System.Math.min(limit, wert);
end;

class operator hlDecimal.Modulus(const Dividend, Divisor: hlDecimal): hlDecimal;
var
  i: int64;
begin
  i := System.Trunc(Dividend.wert) mod System.Trunc(Divisor.wert);
  result := hlDecimal.Create(i, 0);
end;

class operator hlDecimal.Multiply(const Left, Right: hlDecimal): hlDecimal;
begin
  result := hlDecimal.Create(Left.wert * Right.wert, 0);
end;

class operator hlDecimal.Divide(const Left, Right: hlDecimal): hlDecimal;
begin
  result := hlDecimal.Create(Left.wert / Right.wert, 0);
end;

class operator hlDecimal.Negative(x: hlDecimal): hlDecimal;
begin
  result := -x.toDouble;
end;

class operator hlDecimal.NotEqual(const Left, Right: hlDecimal): boolean;
begin
  result := Left.CompareTo(Right.wert) <> 0;
end;

function hlDecimal.RundeToStelle(stelle: Integer): hlDecimal;
var
  multi: double;
begin
  result := hlDecimal.Create(wert, epsilon);
  multi := IntPower(10, stelle);
  result := round(result.wert * multi) / multi;
  // Round() = Kaufmännisches Runden
end;

class operator hlDecimal.Subtract(const Left, Right: hlDecimal): hlDecimal;
begin
  // TODO: (hier und bei anderen Vorkommen): Welches Epsilon soll gewählt werden?
  result := hlDecimal.Create(Left.wert - Right.wert, 0);
end;

function hlDecimal.toDouble: double;
begin
  result := wert;
end;

function hlDecimal.toString(gesamtlänge, nachkommastellen: Integer): string;
begin
  result := Format('%*.*f', [gesamtlänge, nachkommastellen, wert])
end;

function hlDecimal.toSQLString: string;
begin
  result := StringReplace(toString, FormatSettings.DecimalSeparator, '.',
    [rfReplaceAll]);
end;

function hlDecimal.toString(gesamtlänge, nachkommastellen: Integer;
  fillchar: char): string;
begin
  result := Format('%*.*f', [gesamtlänge, nachkommastellen, wert]);
  result := StringReplace(result, ' ', fillchar, [rfReplaceAll]);
end;

class operator hlDecimal.Trunc(const d: hlDecimal): hlDecimal;
begin
  result := hlDecimal.Create(System.Trunc(d.wert), d.epsilon);
end;

function hlDecimal.TryParse(const s: string): boolean;
begin
  result := TryStrToFloat(s, wert);
end;

function hlDecimal.toString: string;
begin
  result := FloatToStr(wert);
end;
{$ENDREGION}
{$REGION 'hlString'}

class operator hlString.Add(const Left: hlString; Right: hlString): hlString;
begin
  result := hlString.Create(Left.wert + Right.wert);
end;

function hlString.beginsWith(beginning: hlString;
  ignoreCase: boolean = false): boolean;
begin
  if ignoreCase then
    result := AnsiStartsText(beginning, wert)
  else
    result := AnsiStartsStr(beginning, wert)
end;

function hlString.contains(substr: hlString;
  caseSensitive: boolean = true): boolean;
begin
  if caseSensitive then
  begin
    result := Pos(substr.toString, wert) >= 1;
  end
  else // if not caseSensitive then
  begin
    result := Pos(substr.toUpperCase.toString, self.toUpperCase.toString) >= 1;
  end;
end;

constructor hlString.Create(const aWert: string);
begin
  dummy := dummy;
  // Die dummy-Variable nutzen, damit die Dumme Compilerwarnung weg geht.
  self.wert := aWert;
  UniqueString(self.wert);
end;

function hlString.endsWith(ending: hlString;
  ignoreCase: boolean = false): boolean;
begin
  if ignoreCase then
    result := AnsiEndsText(ending, wert)
  else
    result := AnsiEndsStr(ending, wert)
end;

class operator hlString.Equal(const Left, Right: hlString): boolean;
begin
  result := Left.wert = Right.wert;
end;

class operator hlString.Implicit(const s: hlString): WideString;
begin
  result := s.toString;
end;

class operator hlString.Implicit(const s: hlString): string;
begin
  result := s.toString;
end;

class operator hlString.Implicit(const i: Integer): hlString;
begin
  result := hlString.Create(IntToStr(i));
end;

function hlString.IncludeTrailingPathDelimiter: hlString;
begin
  result := hlString.Create(System.SysUtils.IncludeTrailingPathDelimiter(wert));
end;

function hlString.isEmpty: boolean;
begin
  result := self.trim = '';
end;

function hlString.LeftPad(howMany: Integer; padChar: char): hlString;
var
  Counter: Integer;
  x: Integer;
  NewString: string;
begin
  Counter := howMany - System.Length(wert);
  for x := 1 to Counter do
  begin
    NewString := NewString + padChar;
  end;
  result := hlString(NewString + wert);
end;

function hlString.RightPad(howMany: Integer; padChar: char): hlString;
var
  Counter: Integer;
  x: Integer;
  NewString: string;
begin
  Counter := howMany - System.Length(wert);
  for x := 1 to Counter do
  begin
    NewString := NewString + padChar;
  end;
  result := hlString(wert + NewString);
end;

function hlString.Length: Integer;
begin
  result := System.Length(wert);
end;

class operator hlString.NotEqual(const Left, Right: hlString): boolean;
begin
  result := Left.wert <> Right.wert;
end;

function hlString.Replace(search, Replace: hlString): hlString;
begin
  result := StringReplace(wert, search, Replace, [rfReplaceAll]);
end;

function hlString.SubString(offset, Length: Integer): hlString;
begin
  result := Copy(wert, offset, Length);
end;

class operator hlString.Implicit(const s: string): hlString;
begin
  result := hlString.Create(s);
end;

function hlString.toFileName: string;
begin
  result := wert;
  result := StringReplace(result, '/', '_', [rfReplaceAll]);
  result := StringReplace(result, '\', '_', [rfReplaceAll]);
  result := StringReplace(result, ':', '_', [rfReplaceAll]);
  result := StringReplace(result, '*', '_', [rfReplaceAll]);
  result := StringReplace(result, '?', '_', [rfReplaceAll]);
  result := StringReplace(result, '"', '_', [rfReplaceAll]);
  result := StringReplace(result, '<', '_', [rfReplaceAll]);
  result := StringReplace(result, '>', '_', [rfReplaceAll]);
  result := StringReplace(result, '|', '_', [rfReplaceAll]);
end;

function hlString.toInteger(werfeException: boolean = false): Integer;
var
  ec: Integer;
begin
  if werfeException then
    result := StrToInt(wert)
  else
    Val(wert, result, ec); // keine Exception werfen
end;

function hlString.toLowerCase: hlString;
begin
  result := AnsiLowerCase(wert);
end;

function hlString.toSQLObjectName(withoutDBO: boolean = false): string;
begin
  if withoutDBO then
    result := '[' + wert + ']'
  else
    result := '[dbo].[' + wert + ']';
end;

function hlString.toSQLString: string;
begin
  result := wert;

  // Die ehemalige Funktion hclStrToStr sah mal so aus:
  //if copy(s, Length(s), 1) = ':' then
  //  s := copy(s, 1, Length(s) - 1);
  //if copy(s, Length(s), 1) = '''' then
  //  s := copy(s, 1, Length(s) - 1);
  //s := StringReplace(s, ':', '::', [rfReplaceAll]);
  //s := StringReplace(s, '''', '''''', [rfReplaceAll]);

  // Escape SQL-Argument
  (*
    result := StringReplace(result, '\', '\\', [rfReplaceAll]);
    result := StringReplace(result, '_', '\_', [rfReplaceAll]);
    result := StringReplace(result, '%', '\%', [rfReplaceAll]);
    result := StringReplace(result, '[', '\[', [rfReplaceAll]);
    result := StringReplace(result, '''', '\''', [rfReplaceAll]);
  *)

  // DM 29.02.2016 Irgendwie versteh ich das nicht...
  // 'xxx\'xxx' ist erlaubt, aber 'xxx\'xxx\'xxx' nicht
  // aber 'xxx''xxx''xxx' geht.
  result := StringReplace(result, '''', '''''', [rfReplaceAll]);

  // Verhindern, dass SQL Server denkt, es sei ein Parameterobjekt
  // Brauchen wir nur, wenn die abfrage ParamCheck=true hat.
  // Wir haben aber in hl.Datenbank.pas das immer auf false.
  // result := StringReplace(result, ':', '::', [rfReplaceAll]);

  // New lines
  result := StringReplace(result, #13#10, '''+Char(13)+Char(10)+''',
    [rfReplaceAll]);
  result := StringReplace(result, #13, '''+Char(13)+''', [rfReplaceAll]);
  result := StringReplace(result, #10, '''+Char(10)+''', [rfReplaceAll]);
  result := StringReplace(result, 'Char(10)+''''+Char(', 'Char(10)+Char(',
    [rfReplaceAll]);
  result := StringReplace(result, 'Char(13)+''''+Char(', 'Char(13)+Char(',
    [rfReplaceAll]);

  result := '''' + result + '''';
end;

function hlString.toString: string;
begin
  result := wert;
  UniqueString(result);
end;

function hlString.toUpperCase: hlString;
begin
  result := AnsiUpperCase(wert)
end;

function hlString.trim: hlString;
begin
  result := hlString.Create(System.SysUtils.trim(wert));
end;

function hlString.Trunc(len: Integer): hlString;
begin
  result := hlString.Create(Copy(wert, 1, len));
end;

{$ENDREGION}
{$REGION 'hlInteger'}

constructor hlInteger.Create(wert, min, max: int64);
begin
  self.wert := wert;
  self.min := min;
  self.max := max;
end;

constructor hlInteger.Create(wert: int64);
begin
  Create(wert, Low(int64), High(int64));
end;

class operator hlInteger.Add(const Left: hlString; Right: hlInteger): hlString;
begin
  result := Left + Right.toString;
end;

function hlInteger.Between(lo, hi: hlInteger; einschliesslich: boolean)
  : boolean;
begin
  if einschliesslich then
  begin
    result := (self >= lo) and (self <= hi);
  end
  else
  begin
    result := (self > lo) and (self < hi);
  end;
end;

function hlInteger.CompareTo(i: Integer): TValueRelationship;
begin
  result := CompareValue(wert, i);
end;

constructor hlInteger.Create(min, max: int64);
begin
  Create(0, min, max);
end;

constructor hlInteger.CreateUInt32(dummy: Integer);
begin
  Create(0, 0, High(Cardinal));
end;

constructor hlInteger.CreateInt64(dummy: Integer);
begin
  Create(0, 0, High(int64));
end;

function hlInteger.EnsureInRange(i: int64): int64;
begin
  if (i < min) or (i > max) then
    raise ERangeError.Create(LNG_RANGE_ERROR)
  else
    result := i;
end;

class operator hlInteger.Add(const Left: hlInteger; Right: int64): hlInteger;
begin
  result := hlInteger.Create(Left.EnsureInRange(Left.wert + Right), Left.min,
    Left.max);
end;

class operator hlInteger.Subtract(const Left: hlInteger; Right: int64)
  : hlInteger;
begin
  result := hlInteger.Create(Left.EnsureInRange(Left.wert - Right), Left.min,
    Left.max);
end;

class operator hlInteger.Equal(const Left: hlInteger;
  const Right: int64): boolean;
begin
  result := Left.wert = Right;
end;

class operator hlInteger.GreaterThan(const Left, Right: hlInteger): boolean;
begin
  result := Left.CompareTo(Right.wert) > 0;
end;

class operator hlInteger.GreaterThanOrEqual(const Left,
  Right: hlInteger): boolean;
begin
  result := Left.CompareTo(Right.wert) >= 0;
end;

class operator hlInteger.Implicit(const i: hlInteger): string;
begin
  result := IntToStr(i.wert);
end;

class operator hlInteger.Implicit(const i: hlInteger): int64;
begin
  result := i.wert;
end;

class operator hlInteger.Implicit(const s: string): hlInteger;
begin
  result := hlInteger.Create(StrToInt64(s));
end;

class operator hlInteger.Implicit(const s: hlString): hlInteger;
begin
  result := hlInteger.Create(StrToInt64(s));
end;

function hlInteger.IntMod(teiler: hlInteger): hlInteger;
begin
  result := (wert mod teiler.wert + teiler.wert) mod teiler.wert;
end;

class operator hlInteger.Implicit(const i: Integer): hlInteger;
begin
  result := hlInteger.Create(i);
end;

class operator hlInteger.Implicit(const i: int64): hlInteger;
begin
  result := hlInteger.Create(i); // EnsureInRange(i);
end;

class operator hlInteger.LessThan(const Left, Right: hlInteger): boolean;
begin
  result := Left.CompareTo(Right.wert) < 0;
end;

class operator hlInteger.LessThanOrEqual(const Left, Right: hlInteger): boolean;
begin
  result := Left.CompareTo(Right.wert) <= 0;
end;

function hlInteger.LowerLimit(limit: hlInteger): hlInteger;
begin
  result := System.Math.max(limit, wert);
end;

function hlInteger.UpperLimit(limit: hlInteger): hlInteger;
begin
  result := System.Math.min(limit, wert);
end;

class operator hlInteger.Negative(x: hlInteger): hlInteger;
begin
  result := -x.toInteger;
end;

class operator hlInteger.NotEqual(const Left, Right: hlInteger): boolean;
begin
  result := Left.CompareTo(Right.wert) <> 0;
end;

function hlInteger.toDouble: double;
begin
  result := wert;
end;

function hlInteger.toInteger: Integer;
begin
  result := wert;
end;

function hlInteger.toSQLString: string;
begin
  result := IntToStr(wert);
end;

function hlInteger.toString: string;
begin
  result := IntToStr(wert);
end;

function hlInteger.TryParse(const s: string): boolean;
begin
  result := TryStrToInt64(s, wert);
end;

{$ENDREGION}
{$REGION 'hlBoolean'}

constructor hlBoolean.Create(aWert: boolean);
begin
  self.wert := aWert;
end;

class operator hlBoolean.Equal(const Left: hlBoolean;
  const Right: boolean): boolean;
begin
  result := Left.wert = Right;
end;

class operator hlBoolean.Implicit(const s: string): hlBoolean;
begin
  result := hlBoolean.Create(StrToBool(s));
end;

class operator hlBoolean.Implicit(const s: hlString): hlBoolean;
begin
  result := hlBoolean.Create(StrToBool(s));
end;

class operator hlBoolean.Implicit(const b: boolean): hlBoolean;
begin
  result := hlBoolean.Create(b);
end;

class operator hlBoolean.Implicit(const b: hlBoolean): boolean;
begin
  result := b.wert;
end;

class function hlBoolean.StrToBool(s: hlString): boolean;
begin
  if (s.toLowerCase = 'true') or (s.toLowerCase = 'wahr') or
    (s.toLowerCase = 'ja') or (s.toLowerCase = 'yes') or (s.toLowerCase = '1')
  then
    result := true
  else if (s.toLowerCase = 'false') or (s.toLowerCase = 'falsch') or
    (s.toLowerCase = 'nein') or (s.toLowerCase = 'no') or (s.toLowerCase = '0')
  then
    result := false
  else
    raise EConvertError.CreateFmt(LNG_CONV_ERROR, [s.toString]);
end;

function hlBoolean.toBoolean: boolean;
begin
  result := wert;
end;

function hlBoolean.toGermanString: string;
begin
  if wert then
    result := StrWahr
  else
    result := StrFalsch;
end;

function hlBoolean.toSQLString: string;
begin
  if wert then
    result := '1'
  else
    result := '0';
end;

function hlBoolean.toString: string;
begin
  if wert then
    result := 'true'
  else
    result := 'false';
end;
{$ENDREGION}
{$REGION 'hlDateTime'}

constructor hlDateTime.Create(aWert: TDateTime);
begin
  wert := aWert;
end;

function hlDateTime.Date: hlDateTime;
begin
  result := hlDateTime.Create(Trunc(wert));
end;

function hlDateTime.Defined: boolean;
begin
  result := wert <> 0;
end;

constructor hlDateTime.Create(aWert: string);
begin
  wert := StrToDateTime(aWert);
end;

class operator hlDateTime.Equal(const Left, Right: hlDateTime): boolean;
begin
  result := Left.wert = Right.wert;
end;

(*
  class operator hlDateTime.Equal(const Left, Right: TDateTime): boolean;
  begin
  result := Left = Right;
  end;
*)

class operator hlDateTime.Implicit(const s: string): hlDateTime;
begin
  result := hlDateTime.Create(StrToDateTime(s));
end;

class operator hlDateTime.Implicit(const s: hlString): hlDateTime;
begin
  result := StrToDateTime(s.toString);
end;

class operator hlDateTime.Implicit(const b: hlDateTime): string;
begin
  result := DateTimeToStr(b.wert);
end;

function hlDateTime.toDateTime: TDateTime;
begin
  result := wert;
end;

class operator hlDateTime.Implicit(const b: hlDateTime): TDateTime;
begin
  result := b.wert;
end;

class operator hlDateTime.Implicit(const b: TDateTime): hlDateTime;
begin
  result := hlDateTime.Create(b);
end;

function hlDateTime.toSQLString: string; // Das gibt einen DATETIME Typ zurück, das ist so gewollt!
var
  datum: string;
  zeit: string;
begin
  datum := FormatDateTime('dd.mm.yyyy', wert); // Jahreszahl muss 4-stellig sein (Ticket 53630)
  zeit := FormatDateTime('hh:nn:ss', wert);
  // Format 104 = "23.08.2019"
  // Format 108 = "11:22:33"
  if zeit <> '00:00:00' then
    result := 'CONVERT(datetime, ''' + datum + ''', 104) + CONVERT(datetime, ''' + zeit + ''', 108)'
  else
    result := 'CONVERT(datetime, ''' + datum + ''', 104)';
end;

function hlDateTime.toString: string;
var
  datum: string;
  zeit: string;
begin
  datum := FormatDateTime('dd.mm.yyyy', wert); // Jahreszahl muss 4-stellig sein (Ticket 53630)
  zeit := FormatDateTime('hh:nn:ss', wert);
  if zeit <> '00:00:00' then
    result := datum + ' ' + zeit
  else
    result := datum;
end;

function hlDateTime.Tomorrow: hlDateTime;
begin
  result := hlDateTime.Create(wert + 1);
end;

function hlDateTime.Yesterday: hlDateTime;
begin
  result := hlDateTime.Create(wert - 1);
end;

{$ENDREGION}
{$REGION 'hlGUID'}

constructor hlGUID.Create(aWert: TGUID);
begin
  wert := aWert;
end;

constructor hlGUID.Create(aWert: string);
begin
  wert := StringToGUID(aWert);
end;

constructor hlGUID.CreateNew(dummy: Integer);
begin
  CreateGUID(wert);
end;

constructor hlGUID.CreateNull(dummy: Integer);
begin
  Create('{00000000-0000-0000-0000-000000000000}');
end;

class operator hlGUID.Equal(const Left, Right: hlGUID): boolean;
begin
  result := IsEqualGUID(Left.wert, Right.wert);
end;

(*
  class operator hlGUID.Equal(const Left, Right: TGUID): boolean;
  begin
  result := IsEqualGUID(Left, Right);
  end;
*)

class operator hlGUID.Implicit(const s: string): hlGUID;
begin
  result := hlGUID.Create(StringToGUID(s));
end;

class operator hlGUID.Implicit(const s: hlString): hlGUID;
begin
  result := StringToGUID(s.toString);
end;

class operator hlGUID.Implicit(const b: hlGUID): string;
begin
  result := GUIDToString(b.wert);
end;

class operator hlGUID.Implicit(const b: hlGUID): TGUID;
begin
  result := b.wert;
end;

class operator hlGUID.Implicit(const b: TGUID): hlGUID;
begin
  result := hlGUID.Create(b);
end;

function hlGUID.toSQLString: string;
begin
  result := hlString.Create(toString).toSQLString;
end;

function hlGUID.toString: string;
begin
  result := GUIDToString(wert);
  result := Copy(result, 2, Length(result) - 2); // { und } entfernen
  result := AnsiUpperCase(result);
end;

function hlGUID.isEmpty: boolean;
begin
  result := IsEmptyGuidString(toString);
end;

{$ENDREGION}

end.
