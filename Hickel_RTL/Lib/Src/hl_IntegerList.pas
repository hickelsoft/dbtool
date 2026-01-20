unit hl_IntegerList;

// HACK: Mit TStringList aufgebaut. Umbauen zu TList: Komplizierter, aber wesentlich schneller!

interface

uses Classes, SysUtils;

type
  ThlIntegerList = class(TObject)
  private
    mList: TStringList;
    function Get(index: integer): integer;
  public
    property items[index: integer]: integer read Get; default;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(value: integer);
    procedure Delete(index: integer);
    function Count: integer;
  end;

implementation

procedure ThlIntegerList.Add(value: integer);
begin
  mList.Add(IntToStr(value));
end;

function ThlIntegerList.Count: integer;
begin
  Result := mList.Count;
end;

constructor ThlIntegerList.Create;
begin
  inherited Create;
  mList := TStringList.Create;
end;

procedure ThlIntegerList.Delete(index: integer);
begin
  mList.Delete(index);
end;

destructor ThlIntegerList.Destroy;
begin
  FreeAndNil(mList);
  inherited;
end;

function ThlIntegerList.Get(index: integer): integer;
begin
  Result := StrToInt(mList[index]);
end;

end.
