unit AdoQueryHelper;

interface

uses
  ADODB;

type
  TAdoQueryHelper = class helper for TADOQuery
  private
    function GetReadOnly: boolean;
    procedure SetReadOnly(const Value: boolean);
  published
  public
    property ReadOnly: boolean read GetReadOnly write SetReadOnly;
  end;

implementation

{ TAdoQueryHelper }

function TAdoQueryHelper.GetReadOnly: boolean;
begin
  result := LockType = ltReadOnly;
end;

procedure TAdoQueryHelper.SetReadOnly(const Value: boolean);
begin
  if Value then
    LockType := ltReadOnly
  else if LockType = ltReadOnly then
    LockType := ltOptimistic;
end;

end.
