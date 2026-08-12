program TestSQLEscapeString;

{$mode delphi}

uses
  SysUtils;

var
  TestCount: Integer = 0;
  PassCount: Integer = 0;
  FailCount: Integer = 0;

{ --- Production code (extracted from C_Database.pas) --- }

function SQL_Escape_String_MySQL(const sString: String): String;
begin
  // Fixed: escape backslashes first, then quotes
  Result := StringReplace(sString, '\', '\\', [rfReplaceAll]);
  Result := StringReplace(Result, '''', '\''', [rfReplaceAll]);
end;

function SQL_Escape_String_Standard(const sString: String): String;
begin
  // SQL-standard double-quote escaping for InterBase, Firebird, Access
  Result := StringReplace(sString, '''', '''''', [rfReplaceAll]);
end;

{ --- Test helper --- }

procedure AssertEquals(const TestName, Expected, Actual: String);
begin
  Inc(TestCount);
  if Expected = Actual then
  begin
    Inc(PassCount);
    WriteLn('[PASS] ', TestName);
  end
  else
  begin
    Inc(FailCount);
    WriteLn('[FAIL] ', TestName);
    WriteLn('  Expected: ''', Expected, '''');
    WriteLn('  Actual:   ''', Actual, '''');
  end;
end;

{ --- MySQL tests --- }

procedure Test_MySQL_should_escape_quote_when_input_contains_single_quote;
var
  Input, Expected, Actual: String;
begin
  // given
  Input := 'O''Brien';
  Expected := 'O\''Brien';
  // when
  Actual := SQL_Escape_String_MySQL(Input);
  // then
  AssertEquals('MySQL: should escape quote when input contains single quote',
    Expected, Actual);
end;

procedure Test_MySQL_should_escape_backslash_when_input_contains_backslash;
var
  Input, Expected, Actual: String;
begin
  // given
  Input := 'C:\Users';
  Expected := 'C:\\Users';
  // when
  Actual := SQL_Escape_String_MySQL(Input);
  // then
  AssertEquals('MySQL: should escape backslash when input contains backslash',
    Expected, Actual);
end;

procedure Test_MySQL_should_escape_both_when_input_contains_quote_and_backslash;
var
  Input, Expected, Actual: String;
begin
  // given
  Input := 'it''s a \path';
  Expected := 'it\''s a \\path';
  // when
  Actual := SQL_Escape_String_MySQL(Input);
  // then
  AssertEquals('MySQL: should escape both when input contains quote and backslash',
    Expected, Actual);
end;

procedure Test_MySQL_should_not_double_escape_when_input_contains_quote;
var
  Input, Actual: String;
  ContainsDoubleEscape: Boolean;
begin
  // given: this was the original bug - quote was escaped to \' and then
  // the backslash in \' was escaped again to \\', producing invalid SQL
  Input := 'O''Brien';
  // when
  Actual := SQL_Escape_String_MySQL(Input);
  ContainsDoubleEscape := Pos('\\''', Actual) > 0;
  // then
  AssertEquals('MySQL: should not double-escape when input contains quote',
    'False', BoolToStr(ContainsDoubleEscape, True));
end;

procedure Test_MySQL_should_return_unchanged_when_no_special_chars;
var
  Input, Actual: String;
begin
  // given
  Input := 'hello world';
  // when
  Actual := SQL_Escape_String_MySQL(Input);
  // then
  AssertEquals('MySQL: should return unchanged when no special chars',
    Input, Actual);
end;

{ --- InterBase/Firebird/Access tests --- }

procedure Test_Standard_should_double_quote_when_input_contains_single_quote;
var
  Input, Expected, Actual: String;
begin
  // given
  Input := 'O''Brien';
  Expected := 'O''''Brien';
  // when
  Actual := SQL_Escape_String_Standard(Input);
  // then
  AssertEquals('Standard: should double-quote when input contains single quote',
    Expected, Actual);
end;

procedure Test_Standard_should_not_escape_backslash_when_input_contains_backslash;
var
  Input, Expected, Actual: String;
begin
  // given
  Input := 'C:\Users';
  Expected := 'C:\Users';
  // when
  Actual := SQL_Escape_String_Standard(Input);
  // then
  AssertEquals('Standard: should not escape backslash when input contains backslash',
    Expected, Actual);
end;

procedure Test_Standard_should_only_escape_quotes_when_input_contains_both;
var
  Input, Expected, Actual: String;
begin
  // given
  Input := 'it''s a \path';
  Expected := 'it''''s a \path';
  // when
  Actual := SQL_Escape_String_Standard(Input);
  // then
  AssertEquals('Standard: should only escape quotes when input contains both',
    Expected, Actual);
end;

procedure Test_Standard_should_return_unchanged_when_no_special_chars;
var
  Input, Actual: String;
begin
  // given
  Input := 'hello world';
  // when
  Actual := SQL_Escape_String_Standard(Input);
  // then
  AssertEquals('Standard: should return unchanged when no special chars',
    Input, Actual);
end;

{ --- Main --- }

begin
  WriteLn('=== SQL_Escape_String Tests ===');
  WriteLn;

  WriteLn('--- MySQL ---');
  Test_MySQL_should_escape_quote_when_input_contains_single_quote;
  Test_MySQL_should_escape_backslash_when_input_contains_backslash;
  Test_MySQL_should_escape_both_when_input_contains_quote_and_backslash;
  Test_MySQL_should_not_double_escape_when_input_contains_quote;
  Test_MySQL_should_return_unchanged_when_no_special_chars;

  WriteLn;
  WriteLn('--- InterBase / Firebird / Access ---');
  Test_Standard_should_double_quote_when_input_contains_single_quote;
  Test_Standard_should_not_escape_backslash_when_input_contains_backslash;
  Test_Standard_should_only_escape_quotes_when_input_contains_both;
  Test_Standard_should_return_unchanged_when_no_special_chars;

  WriteLn;
  WriteLn('=== Results: ', TestCount, ' tests, ', PassCount, ' passed, ', FailCount, ' failed ===');

  if FailCount > 0 then
    Halt(1);
end.
