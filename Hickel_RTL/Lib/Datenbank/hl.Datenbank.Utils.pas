unit hl.Datenbank.Utils;

interface

uses
  SysUtils, Classes, Variants, ADODB, DB;

type
  ThlDatenbankUtils = class(TObject)
  public
    class procedure GetPrimaryKeyNames(con: TADOConnection; TableName: string;
      outsl: TStrings);
  end;

  TFieldByNameCacher = class(TObject)
  private
    query: tadoquery;
    MyFieldByNameIndexes: TStringList;
    MyFieldByNameFields: array of TField;
  public
    function MyFieldByName(fieldname: string): TField;
    destructor Destroy; override;
    constructor Create(aquery: tadoquery);
  end;

implementation

{ ThlDatenbankUtils }

class procedure ThlDatenbankUtils.GetPrimaryKeyNames(con: TADOConnection;
  TableName: string; outsl: TStrings);
var
  ds: TADODataSet;
begin
  ds := TADODataSet.Create(nil);
  try
    con.OpenSchema(siPrimaryKeys, Unassigned, EmptyParam, ds);
    while not ds.Eof do
    begin
      if ds.FieldByName('TABLE_NAME').AsWideString = TableName then
      begin
        outsl.Add(ds.FieldByName('COLUMN_NAME').AsWideString);
      end;
      ds.Next;
    end;
    ds.Close;
  finally
    FreeAndNil(ds);
  end;
end;

{ TFieldByNameCacher }

constructor TFieldByNameCacher.Create(aquery: tadoquery);
begin
  inherited Create;
  MyFieldByNameIndexes := TStringList.Create;
  query := aquery;
end;

destructor TFieldByNameCacher.Destroy;
begin
  FreeAndNil(MyFieldByNameIndexes);
  inherited;
end;

function TFieldByNameCacher.MyFieldByName(fieldname: string): TField;
var
  io, len: integer;
begin
  io := MyFieldByNameIndexes.IndexOf(fieldname);
  if io = -1 then
  begin
    result := query.FieldByName(fieldname);

    len := Length(MyFieldByNameFields);
    SetLength(MyFieldByNameFields, len + 1);
    MyFieldByNameFields[len] := result;

    MyFieldByNameIndexes.Add(fieldname);
  end
  else
  begin
    result := MyFieldByNameFields[io];
  end;
end;

end.
