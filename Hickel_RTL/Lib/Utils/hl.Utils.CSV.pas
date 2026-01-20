unit hl.Utils.CSV;

interface

uses
  SysUtils;

type
  ThsCSVUtils = record
    class function ExportCSVField_Str(s: string; quoteEmpty: boolean = false)
      : string; static;
    class function ExportCSVField_Bool(b: boolean): string; static;
    class function ExportCSVField_Int(i: integer): string; static;
    class function ExportCSVField_Float(f: double): string; static;
    class function ExportCSVField_DateTime(dt: TDateTime): string; static;
    class function ImportCSVField_Str(s: string): string; static;
    class function ImportCSVField_Bool(s: string): boolean; static;
    class function ImportCSVField_Int(s: string): integer; static;
    class function ImportCSVField_Float(s: string): double; static;
    class function ImportCSVField_DateTime(s: string): TDateTime; static;
    class function ExtractNextField(var s: string): string; static;
  end;

implementation

uses
  FormatSettingsCompat;

class function ThsCSVUtils.ExportCSVField_Str(s: string;
  quoteEmpty: boolean = false): string;
begin
  if (s = '') and not quoteEmpty then
    result := ''
  else
    result := '"' + StringReplace(s, '"', '""', [rfReplaceAll]) + '"';
end;

class function ThsCSVUtils.ImportCSVField_Bool(s: string): boolean;
begin
  result := s = 'true';
end;

class function ThsCSVUtils.ImportCSVField_DateTime(s: string): TDateTime;
var
  fs: TFormatSettings;
begin
  result := 0;
  if Length(s) < 10 then
    exit;

  if (s[3] = '.') and (s[6] = '.') then
  begin
    fs.ShortDateFormat := 'dd.mm.yyyy'; // 31.12.2017
    fs.DateSeparator := '.';
  end
  else if (s[5] = '-') and (s[8] = '-') then
  begin
    fs.ShortDateFormat := 'yyyy-mm-dd'; // 2017-12-31
    fs.DateSeparator := '-';
  end;

  fs.ShortTimeFormat := 'hh:nn:ss';
  fs.TimeSeparator := ':';
  result := StrToDateTimeDef(s, 0, fs);
end;

class function ThsCSVUtils.ImportCSVField_Float(s: string): double;
var
  bakDs, bakTs: Char;
begin
  bakTs := FormatSettings.ThousandSeparator;
  FormatSettings.ThousandSeparator := #0;
  bakDs := FormatSettings.DecimalSeparator;
  FormatSettings.DecimalSeparator := ',';
  try
    result := StrToFloat(s);
  finally
    FormatSettings.ThousandSeparator := bakTs;
    FormatSettings.DecimalSeparator := bakDs;
  end;
end;

class function ThsCSVUtils.ImportCSVField_Int(s: string): integer;
begin
  result := StrToInt(s);
end;

class function ThsCSVUtils.ImportCSVField_Str(s: string): string;

  function _Decode(s: string): string;
  begin
    result := Copy(s, 2, Length(s) - 2); // " vorne und hinten entfernen
    result := StringReplace(result, '""', '"', [rfReplaceAll]);
  end;

begin
  if s = '' then
    result := ''
  else if s[1] = '"' then
    result := _Decode(s)
  else
    result := s;
end;

class function ThsCSVUtils.ExportCSVField_Bool(b: boolean): string;
begin
  if b then
    result := 'true'
  else
    result := 'false';
end;

class function ThsCSVUtils.ExportCSVField_Int(i: integer): string;
begin
  result := IntToStr(i);
end;

class function ThsCSVUtils.ExportCSVField_DateTime(dt: TDateTime): string;
var
  bakDf, bakTf: string;
  bakDs, bakTs: Char;
begin
  bakDf := FormatSettings.ShortDateFormat;
  FormatSettings.ShortDateFormat := 'dd.mm.yyyy';
  bakTf := FormatSettings.ShortTimeFormat;
  FormatSettings.ShortTimeFormat := 'hh:nn:ss';
  bakDs := FormatSettings.DateSeparator;
  FormatSettings.DateSeparator := '.';
  bakTs := FormatSettings.TimeSeparator;
  FormatSettings.TimeSeparator := ':';
  try
    result := DateTimeToStr(dt)
  finally
    FormatSettings.ShortDateFormat := bakDf;
    FormatSettings.ShortTimeFormat := bakTf;
    FormatSettings.DateSeparator := bakDs;
    FormatSettings.TimeSeparator := bakTs;
  end;
end;

class function ThsCSVUtils.ExportCSVField_Float(f: double): string;
var
  fs: TFormatSettings;
begin
  fs.ThousandSeparator := #0;
  fs.DecimalSeparator := ',';
  result := FloatToStr(f, fs);
end;

class function ThsCSVUtils.ExtractNextField(var s: string): string;
var
  i: integer;
  bakS: string;
begin
  result := '';

  bakS := s + ';';
  for i := 1 to Length(bakS) do
  begin
    if bakS[i] = ';' then
    // TODO: Problem: Hier werden Strichpunkt in Anführungszeichen nicht ausgeschlossen?
    begin
      result := ImportCSVField_Str(Copy(bakS, 1, i - 1));
      s := Copy(bakS, i + 1, Length(bakS) - i);
      exit;
    end;
  end;
end;

end.
