unit FormatSettingsCompat;

// Polyfill for older Delphi versions to use FormatSettings
// Since Delphi XE:  FormatSettings is a global variable containing DecimalSeparator et. al.
// Before Delphi XE: DecimalSeparator et. al. were global Variables.
// Your code should use FormatSettings. This unit makes it compatible with older versions of Delphi
// by replacing FormatSettings.

{$IF CompilerVersion >= 22.0}

interface
implementation

{$ELSE}

interface

type
  TFormatSettingsPolyfill = class(TObject)
  private
    class function GetCurrencyDecimals: Byte;
    class function GetCurrencyFormat: Byte;
    class function GetCurrencyString: string;
    class function GetDateSeparator: Char;
    class function GetDecimalSeparator: Char;
    //class function GetEraInfo: array of TEraInfo;
    class function GetListSeparator: Char;
    class function GetLongDateFormat: string;
    class function GetLongDayNames(i: integer): string;
    class function GetLongMonthNames(i: integer): string;
    class function GetLongTimeFormat: string;
    class function GetNegCurrFormat: Byte;
    //class function GetNormalizedLocaleName: string;
    class function GetShortDateFormat: string;
    class function GetShortDayNames(i: integer): string;
    class function GetShortMonthNames(i: integer): string;
    class function GetShortTimeFormat: string;
    class function GetThousandSeparator: Char;
    class function GetTimeAMString: string;
    class function GetTimePMString: string;
    class function GetTimeSeparator: Char;
    class function GetTwoDigitYearCenturyWindow: Word;
    class procedure SetCurrencyDecimals(const Value: Byte);
    class procedure SetCurrencyFormat(const Value: Byte);
    class procedure SetCurrencyString(const Value: string);
    class procedure SetDateSeparator(const Value: Char);
    class procedure SetDecimalSeparator(const Value: Char);
    //class procedure SetEraInfo(const Value: array of TEraInfo);
    class procedure SetListSeparator(const Value: Char);
    class procedure SetLongDateFormat(const Value: string);
    class procedure SetLongDayNames(i: integer; const Value: string);
    class procedure SetLongMonthNames(i: integer; const Value: string);
    class procedure SetLongTimeFormat(const Value: string);
    class procedure SetNegCurrFormat(const Value: Byte);
    //class procedure SetNormalizedLocaleName(const Value: string);
    class procedure SetShortDateFormat(const Value: string);
    class procedure SetShortDayNames(i: integer; const Value: string);
    class procedure SetShortMonthNames(i: integer; const Value: string);
    class procedure SetShortTimeFormat(const Value: string);
    class procedure SetThousandSeparator(const Value: Char);
    class procedure SetTimeAMString(const Value: string);
    class procedure SetTimePMString(const Value: string);
    class procedure SetTimeSeparator(const Value: Char);
    class procedure SetTwoDigitYearCenturyWindow(const Value: Word);
  public
    property CurrencyString: string read GetCurrencyString write SetCurrencyString;
    property CurrencyFormat: Byte read GetCurrencyFormat write SetCurrencyFormat;
    property CurrencyDecimals: Byte read GetCurrencyDecimals write SetCurrencyDecimals;
    property DateSeparator: Char read GetDateSeparator write SetDateSeparator;
    property TimeSeparator: Char read GetTimeSeparator write SetTimeSeparator;
    property ListSeparator: Char read GetListSeparator write SetListSeparator;
    property ShortDateFormat: string read GetShortDateFormat write SetShortDateFormat;
    property LongDateFormat: string read GetLongDateFormat write SetLongDateFormat;
    property TimeAMString: string read GetTimeAMString write SetTimeAMString;
    property TimePMString: string read GetTimePMString write SetTimePMString;
    property ShortTimeFormat: string read GetShortTimeFormat write SetShortTimeFormat;
    property LongTimeFormat: string read GetLongTimeFormat write SetLongTimeFormat;
    property ShortMonthNames[i: integer]: string read GetShortMonthNames write SetShortMonthNames;
    property LongMonthNames[i: integer]: string read GetLongMonthNames write SetLongMonthNames;
    property ShortDayNames[i: integer]: string read GetShortDayNames write SetShortDayNames;
    property LongDayNames[i: integer]: string read GetLongDayNames write SetLongDayNames;
    //property EraInfo: array of TEraInfo read GetEraInfo write SetEraInfo;
    property ThousandSeparator: Char read GetThousandSeparator write SetThousandSeparator;
    property DecimalSeparator: Char read GetDecimalSeparator write SetDecimalSeparator;
    property TwoDigitYearCenturyWindow: Word read GetTwoDigitYearCenturyWindow write SetTwoDigitYearCenturyWindow;
    property NegCurrFormat: Byte read GetNegCurrFormat write SetNegCurrFormat;
    //property NormalizedLocaleName: string read GetNormalizedLocaleName write SetNormalizedLocaleName;
  end;

var
  FormatSettings: TFormatSettingsPolyfill;

implementation

uses
  SysUtils;

{ TFormatSettingsPolyfill }

class function TFormatSettingsPolyfill.GetCurrencyDecimals: Byte;
begin
  result := SysUtils.CurrencyDecimals;
end;

class function TFormatSettingsPolyfill.GetCurrencyFormat: Byte;
begin
  result := SysUtils.CurrencyFormat;
end;

class function TFormatSettingsPolyfill.GetCurrencyString: string;
begin
  result := SysUtils.CurrencyString;
end;

class function TFormatSettingsPolyfill.GetDateSeparator: Char;
begin
  result := SysUtils.DateSeparator;
end;

class function TFormatSettingsPolyfill.GetDecimalSeparator: Char;
begin
  result := SysUtils.DecimalSeparator;
end;

class function TFormatSettingsPolyfill.GetListSeparator: Char;
begin
  result := SysUtils.ListSeparator;
end;

class function TFormatSettingsPolyfill.GetLongDateFormat: string;
begin
  result := SysUtils.LongDateFormat;
end;

class function TFormatSettingsPolyfill.GetLongDayNames(i: integer): string;
begin
  result := SysUtils.LongDayNames[i];
end;

class function TFormatSettingsPolyfill.GetLongMonthNames(i: integer): string;
begin
  result := SysUtils.LongMonthNames[i];
end;

class function TFormatSettingsPolyfill.GetLongTimeFormat: string;
begin
  result := SysUtils.LongTimeFormat;
end;

class function TFormatSettingsPolyfill.GetNegCurrFormat: Byte;
begin
  result := SysUtils.NegCurrFormat;
end;

class function TFormatSettingsPolyfill.GetShortDateFormat: string;
begin
  result := SysUtils.ShortDateFormat;
end;

class function TFormatSettingsPolyfill.GetShortDayNames(i: integer): string;
begin
  result := SysUtils.ShortDayNames[i];
end;

class function TFormatSettingsPolyfill.GetShortMonthNames(i: integer): string;
begin
  result := SysUtils.ShortMonthNames[i];
end;

class function TFormatSettingsPolyfill.GetShortTimeFormat: string;
begin
  result := SysUtils.ShortTimeFormat;
end;

class function TFormatSettingsPolyfill.GetThousandSeparator: Char;
begin
  result := SysUtils.ThousandSeparator;
end;

class function TFormatSettingsPolyfill.GetTimeAMString: string;
begin
  result := SysUtils.TimeAMString;
end;

class function TFormatSettingsPolyfill.GetTimePMString: string;
begin
  result := SysUtils.TimePMString;
end;

class function TFormatSettingsPolyfill.GetTimeSeparator: Char;
begin
  result := SysUtils.TimeSeparator;
end;

class function TFormatSettingsPolyfill.GetTwoDigitYearCenturyWindow: Word;
begin
  result := SysUtils.TwoDigitYearCenturyWindow;
end;

class procedure TFormatSettingsPolyfill.SetCurrencyDecimals(const Value: Byte);
begin
  SysUtils.CurrencyDecimals := Value;
end;

class procedure TFormatSettingsPolyfill.SetCurrencyFormat(const Value: Byte);
begin
  SysUtils.CurrencyFormat := Value;
end;

class procedure TFormatSettingsPolyfill.SetCurrencyString(const Value: string);
begin
  SysUtils.CurrencyString := Value;
end;

class procedure TFormatSettingsPolyfill.SetDateSeparator(const Value: Char);
begin
  SysUtils.DateSeparator := Value;
end;

class procedure TFormatSettingsPolyfill.SetDecimalSeparator(const Value: Char);
begin
  SysUtils.DecimalSeparator := Value;
end;

class procedure TFormatSettingsPolyfill.SetListSeparator(const Value: Char);
begin
  SysUtils.ListSeparator := Value;
end;

class procedure TFormatSettingsPolyfill.SetLongDateFormat(const Value: string);
begin
  SysUtils.LongDateFormat := Value;
end;

class procedure TFormatSettingsPolyfill.SetLongDayNames(i: integer; const Value: string);
begin
  SysUtils.LongDayNames[i] := Value;
end;

class procedure TFormatSettingsPolyfill.SetLongMonthNames(i: integer; const Value: string);
begin
  SysUtils.LongMonthNames[i] := Value;
end;

class procedure TFormatSettingsPolyfill.SetLongTimeFormat(const Value: string);
begin
  SysUtils.LongTimeFormat := Value;
end;

class procedure TFormatSettingsPolyfill.SetNegCurrFormat(const Value: Byte);
begin
  SysUtils.NegCurrFormat := Value;
end;

class procedure TFormatSettingsPolyfill.SetShortDateFormat(const Value: string);
begin
  SysUtils.ShortDateFormat := Value;
end;

class procedure TFormatSettingsPolyfill.SetShortDayNames(i: integer; const Value: string);
begin
  SysUtils.ShortDayNames[i] := Value;
end;

class procedure TFormatSettingsPolyfill.SetShortMonthNames(i: integer; const Value: string);
begin
  SysUtils.ShortMonthNames[i] := Value;
end;

class procedure TFormatSettingsPolyfill.SetShortTimeFormat(const Value: string);
begin
  SysUtils.ShortTimeFormat := Value;
end;

class procedure TFormatSettingsPolyfill.SetThousandSeparator(const Value: Char);
begin
  SysUtils.ThousandSeparator := Value;
end;

class procedure TFormatSettingsPolyfill.SetTimeAMString(const Value: string);
begin
  SysUtils.TimeAMString := Value;
end;

class procedure TFormatSettingsPolyfill.SetTimePMString(const Value: string);
begin
  SysUtils.TimePMString := Value;
end;

class procedure TFormatSettingsPolyfill.SetTimeSeparator(const Value: Char);
begin
  SysUtils.TimeSeparator := Value;
end;

class procedure TFormatSettingsPolyfill.SetTwoDigitYearCenturyWindow(const Value: Word);
begin
  SysUtils.TwoDigitYearCenturyWindow := Value;
end;

initialization
  FormatSettings := TFormatSettingsPolyfill.Create;
finalization
  FreeAndNil(FormatSettings);

{$IFEND}

end.
