(*
  * Diese Unit hackt TwwDBLookupComboDlg, um zu ermöglichen, dass OnCloseUp aufgerufen wird,
  * wenn kein Lookup-Dataset vorhanden ist. Dies ermöglicht uns, ein Eingabefeld mit den
  * schönen 3 Pünktchen zu haben, und trotzdem unseren gelben Suchdialog anstelle des
  * Schrott-Dialogs von InfoPower zu verwenden.
  * -- DM 16.12.2016
  *
  * Wichtig: Diese Unit MUSS im interface Uses-Abschnitt NACH wwdbdlg deklariert werden!
*)

unit hl.Utils.DBLookupComboDlgHack;

interface

uses
  wwdbdlg, DB, wwdblook;

type
  TwwDBLookupComboDlg = class(wwdbdlg.TwwDBLookupComboDlg)
  public
    procedure DropDown; override;
  end;

implementation

{ TwwDBLookupComboDlg }

procedure TwwDBLookupComboDlg.DropDown;
var
  modified: boolean;
  FillTable: TDataSet;
begin
  if Assigned(LookupTable) then
  begin
    inherited DropDown;
    exit;
  end;

  modified := false;
  if Assigned(DataSource) then
    FillTable := DataSource.DataSet
  else
    FillTable := nil;
  if Assigned(OnCloseUp) then
    OnCloseUp(self, LookupTable, FillTable, modified);

  // see wwdbdlg.TwwDBLookupComboDlg.DropDown
  if (Style <> csDropDownList) or AutoDropDown then
    SelectAll;
  FLastSearchKey := '';
  RefreshButton;
  SkipDataChange := false;
  if modified and LookupTable.active then
  begin { 5/12/98 check for active lookuptable }
    LookupTable.updateCursorPos;
    LookupTable.resync([]);
  end;
end;

end.
