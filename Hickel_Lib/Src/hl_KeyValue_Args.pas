unit hl_KeyValue_Args;

interface

uses
  Classes, SysUtils;

// Diese Funktionen lesen Parameter der Form "KEY=VALUE" aus den Programm-Argumenten
// (case insensitive)
procedure hl_GetKeyValueParams(sl: TStringList);
function hl_GetParamValueByKey(key: string): string;

implementation

procedure hl_GetKeyValueParams(sl: TStringList);
var
  i, p: integer;
  s: string;
  sName: string;
  sValue: string;
begin
  for i := 0 to ParamCount do
  begin
    s := ParamStr(i+1);
    p := Pos('=', s);
    if p > 0 then
    begin
      sName := UpperCase(Copy(s, 1, p-1));
      sValue := Copy(s, p+1, Length(s)-p);
      if Copy(sName,1,1) = '/' then Delete(sName,1,1);
      sl.Values[sName] := sValue;
    end;
  end;
end;

function hl_GetParamValueByKey(key: string): string;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    hl_GetKeyValueParams(sl);
    result := sl.Values[key];
  finally
    FreeAndNil(sl);
  end;
end;

end.
