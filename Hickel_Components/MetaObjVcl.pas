unit MetaObjVcl;

interface

uses
  Classes;

type
  {$IF CompilerVersion > 20.0} // Version geraten
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$IFEND}
  TSerializationMetaObj = class(TComponent)
  private
    FData: string;
    FBeforeLoad: TNotifyEvent;
    FAfterLoad: TNotifyEvent;
    FBeforeSave: TNotifyEvent;
    FAfterSave: TNotifyEvent;
  published
    property Data: string read FData write FData;
    property OnBeforeLoad: TNotifyEvent read FBeforeLoad write FBeforeLoad;
    property OnAfterLoad: TNotifyEvent read FAfterLoad write FAfterLoad;
    property OnBeforeSave: TNotifyEvent read FBeforeSave write FBeforeSave;
    property OnAfterSave: TNotifyEvent read FAfterSave write FAfterSave;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Hs', [TSerializationMetaObj]);
end;


end.
