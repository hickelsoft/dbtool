program TestSQLEscapeTableName;

{$mode delphi}

uses
  SysUtils;

var
  TestCount: Integer = 0;
  PassCount: Integer = 0;
  FailCount: Integer = 0;

{ --- Production code (extracted from C_Database.pas) --- }

function SQL_Escape_TableName_SqlServer(const sTableName: String): String;
var
  ary: TArray<String>;
  i: Integer;
begin
  Result := '';
  ary := sTableName.Split(['.']);
  for i := 0 to Length(ary) - 1 do
  begin
    if i <> 0 then
      Result := Result + '.';
    ary[i] := StringReplace(ary[i], ']', ']]', [rfReplaceAll]);
    Result := Result + '[' + ary[i] + ']';
  end;
end;

function SQL_Escape_TableName_MySQL(const sTableName: String): String;
begin
  Result := '`' + StringReplace(sTableName, '`', '``', [rfReplaceAll]) + '`';
end;

function SQL_Escape_TableName_Firebird(const sTableName: String): String;
begin
  Result := '"' + StringReplace(sTableName, '"', '""', [rfReplaceAll]) + '"';
end;

function SQL_Escape_TableName_Access(const sTableName: String): String;
begin
  Result := '[' + StringReplace(sTableName, ']', ']]', [rfReplaceAll]) + ']';
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

{ --- SQL Server tests --- }

procedure Test_SqlServer_should_bracket_when_simple_name;
var
  Input: String;
  Actual: String;
begin
  // given
  Input := 'Customers';
  // when
  Actual := SQL_Escape_TableName_SqlServer(Input);
  // then
  AssertEquals('SqlServer: should bracket simple name', '[Customers]', Actual);
end;

procedure Test_SqlServer_should_escape_bracket_when_name_contains_closing_bracket;
var
  Input: String;
  Actual: String;
begin
  // given
  Input := 'my]table';
  // when
  Actual := SQL_Escape_TableName_SqlServer(Input);
  // then
  AssertEquals('SqlServer: should escape ] when name contains closing bracket',
    '[my]]table]', Actual);
end;

procedure Test_SqlServer_should_split_on_dot_when_schema_qualified;
var
  Input: String;
  Actual: String;
begin
  // given
  Input := 'dbo.Customers';
  // when
  Actual := SQL_Escape_TableName_SqlServer(Input);
  // then
  AssertEquals('SqlServer: should split on dot when schema-qualified',
    '[dbo].[Customers]', Actual);
end;

procedure Test_SqlServer_should_handle_bracket_in_schema_when_complex_name;
var
  Input: String;
  Actual: String;
begin
  // given
  Input := 'my]schema.my]table';
  // when
  Actual := SQL_Escape_TableName_SqlServer(Input);
  // then
  AssertEquals('SqlServer: should escape brackets in schema-qualified name',
    '[my]]schema].[my]]table]', Actual);
end;

{ --- MySQL tests --- }

procedure Test_MySQL_should_backtick_when_simple_name;
var
  Input: String;
  Actual: String;
begin
  // given
  Input := 'Customers';
  // when
  Actual := SQL_Escape_TableName_MySQL(Input);
  // then
  AssertEquals('MySQL: should backtick simple name', '`Customers`', Actual);
end;

procedure Test_MySQL_should_escape_backtick_when_name_contains_backtick;
var
  Input: String;
  Actual: String;
begin
  // given
  Input := 'my`table';
  // when
  Actual := SQL_Escape_TableName_MySQL(Input);
  // then
  AssertEquals('MySQL: should escape backtick when name contains backtick',
    '`my``table`', Actual);
end;

{ --- Firebird/InterBase tests --- }

procedure Test_Firebird_should_doublequote_when_simple_name;
var
  Input: String;
  Actual: String;
begin
  // given
  Input := 'Customers';
  // when
  Actual := SQL_Escape_TableName_Firebird(Input);
  // then
  AssertEquals('Firebird: should double-quote simple name', '"Customers"', Actual);
end;

procedure Test_Firebird_should_escape_quote_when_name_contains_doublequote;
var
  Input: String;
  Actual: String;
begin
  // given
  Input := 'my"table';
  // when
  Actual := SQL_Escape_TableName_Firebird(Input);
  // then
  AssertEquals('Firebird: should escape " when name contains double-quote',
    '"my""table"', Actual);
end;

{ --- Access tests --- }

procedure Test_Access_should_bracket_when_simple_name;
var
  Input: String;
  Actual: String;
begin
  // given
  Input := 'Customers';
  // when
  Actual := SQL_Escape_TableName_Access(Input);
  // then
  AssertEquals('Access: should bracket simple name', '[Customers]', Actual);
end;

procedure Test_Access_should_escape_bracket_when_name_contains_closing_bracket;
var
  Input: String;
  Actual: String;
begin
  // given
  Input := 'my]table';
  // when
  Actual := SQL_Escape_TableName_Access(Input);
  // then
  AssertEquals('Access: should escape ] when name contains closing bracket',
    '[my]]table]', Actual);
end;

procedure Test_Access_should_handle_spaces_when_name_contains_space;
var
  Input: String;
  Actual: String;
begin
  // given
  Input := 'My Table';
  // when
  Actual := SQL_Escape_TableName_Access(Input);
  // then
  AssertEquals('Access: should handle spaces in name',
    '[My Table]', Actual);
end;

{ --- Main --- }

begin
  WriteLn('=== SQL_Escape_TableName Tests ===');
  WriteLn;

  WriteLn('--- SQL Server ---');
  Test_SqlServer_should_bracket_when_simple_name;
  Test_SqlServer_should_escape_bracket_when_name_contains_closing_bracket;
  Test_SqlServer_should_split_on_dot_when_schema_qualified;
  Test_SqlServer_should_handle_bracket_in_schema_when_complex_name;

  WriteLn;
  WriteLn('--- MySQL ---');
  Test_MySQL_should_backtick_when_simple_name;
  Test_MySQL_should_escape_backtick_when_name_contains_backtick;

  WriteLn;
  WriteLn('--- Firebird / InterBase ---');
  Test_Firebird_should_doublequote_when_simple_name;
  Test_Firebird_should_escape_quote_when_name_contains_doublequote;

  WriteLn;
  WriteLn('--- Access ---');
  Test_Access_should_bracket_when_simple_name;
  Test_Access_should_escape_bracket_when_name_contains_closing_bracket;
  Test_Access_should_handle_spaces_when_name_contains_space;

  WriteLn;
  WriteLn('=== Results: ', TestCount, ' tests, ', PassCount, ' passed, ', FailCount, ' failed ===');

  if FailCount > 0 then
    Halt(1);
end.
