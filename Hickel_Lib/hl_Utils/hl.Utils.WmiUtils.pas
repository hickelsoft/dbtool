unit hl.Utils.WmiUtils;

interface

uses
  SysUtils,
  Variants,
  ActiveX,
  ComObj;

function GetWMIstring(wmiHost, root, wmiClass, wmiProperty: string): string;
function GetWMIarray(wmiHost, root, wmiClass, wmiProperty,
  Separator: string): string;

implementation

function OleVariantToText(aVar: OleVariant): string;
// mostly quickdump for WMI researchpurposes
var
  i: integer;
begin
  Result := '';
  if not VarIsNull(aVar) then
    if VarIsArray(aVar) then
    begin
      Result := '{';
      for i := VarArrayLowBound(aVar, 1) to vararrayhighbound(aVar, 1) do
      begin
        if i <> 0 then
          Result := Result + ',';
        Result := Result + OleVariantToText(vararrayget(aVar, [i]));
      end;
      Result := Result + '}';
    end
    else
      Result := VarToStr(aVar);
end;

Function GetMultiString_FromArray(ArrayString: OleVariant;
  Separator: string): string;
begin
  If VarIsNull(ArrayString) Then
    Result := ''
  else
    Result := OleVariantToText(ArrayString);
  // arraystring.items[0]; // Join( ArrayString, Seprator )
end;

function GetWMIObject(const objectName: String): IDispatch;
var
  chEaten: integer;
  BindCtx: IBindCtx; // for access to a bind context
  Moniker: IMoniker; // Enables you to use a moniker object
begin
  OleCheck(CreateBindCtx(0, BindCtx));
  OleCheck(MkParseDisplayName(BindCtx, StringToOleStr(objectName), chEaten,
    Moniker));
  // Converts a string into a moniker that identifies the object named by the string
  OleCheck(Moniker.BindToObject(BindCtx, nil, IDispatch, Result));
  // Binds to the specified object
end;

function GetWMIarray(wmiHost, root, wmiClass, wmiProperty,
  Separator: string): string;
var
  objWMIService: OleVariant;
  colItems: OleVariant;
  colItem: OleVariant;
  oEnum: IEnumvariant;
  iValue: LongWord;
begin
  objWMIService := GetWMIObject(Format('winmgmts:\\%s\%s', [wmiHost, root]));
  colItems := objWMIService.ExecQuery(Format('SELECT * FROM %s', [wmiClass]
    ), 'WQL', 0);
  oEnum := IUnknown(colItems._NewEnum) as IEnumvariant;
  while oEnum.Next(1, colItem, iValue) = 0 do
  begin
    Result := GetMultiString_FromArray(colItem.Properties_.Item(wmiProperty, 0)
      .Value, Separator);
    // you can improve this code  ;) , storing the results in an TString.
    if Result <> '' then
      break;
  end;
end;

function GetWMIstring(wmiHost, root, wmiClass, wmiProperty: string): string;
var
  objWMIService: OleVariant;
  colItems: OleVariant;
  colItem: OleVariant;
  oEnum: IEnumvariant;
  iValue: LongWord;
begin
  objWMIService := GetWMIObject(Format('winmgmts:\\%s\%s', [wmiHost, root]));
  colItems := objWMIService.ExecQuery(Format('SELECT * FROM %s', [wmiClass]
    ), 'WQL', 0);
  oEnum := IUnknown(colItems._NewEnum) as IEnumvariant;
  while oEnum.Next(1, colItem, iValue) = 0 do
  begin
    Result := colItem.Properties_.Item(wmiProperty, 0);
    // you can improve this code  ;) , storing the results in an TString.
    if Result <> '' then
      break;
  end;
end;

initialization

CoInitialize(nil);

finalization

CoUnInitialize;

end.
