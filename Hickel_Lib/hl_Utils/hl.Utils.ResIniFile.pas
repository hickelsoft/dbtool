unit hl.Utils.ResIniFile;

interface

uses
  IniFiles, Classes, SysUtils;

type
  TResIniFile = class(TMemIniFile)
  public
    constructor Create(Instance: THandle; ResourceType: PChar;
      ResourceName: string);
  end;

implementation

constructor TResIniFile.Create(Instance: THandle; ResourceType: PChar;
  ResourceName: string);
var
  RS: TResourceStream;
  sl: TStrings;
begin
  inherited Create('');

  RS := TResourceStream.Create(Instance, ResourceName, ResourceType);
  sl := TStringList.Create;
  try
    sl.LoadFromStream(RS);
    SetStrings(sl);
  finally
    FreeAndNil(RS);
    FreeAndNil(sl);
  end;
end;

end.
