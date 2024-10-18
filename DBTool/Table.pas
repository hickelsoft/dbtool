unit Table;

// TODO: "AutoInc" auch zeigen
// TODO: "Decimal" statt "BCD" und bei Größe 19,5 zeigen

interface

uses
  SysUtils, Windows, Classes, Controls, StdCtrls, Forms, ExtCtrls, Database,
  DB, DBGrids, Grids, Wwdbgrid, Wwdbigrd, ComCtrls, Menus, Graphics,
  ImgList, Buttons, Dialogs, C_Database, System.ImageList, AdoDb, AdoInt,
  HsGradientPanel;

type
  TMDI_Table = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Notebook1: TNotebook;
    Panel3: TPanel;
    dsData: TDataSource;
    LbSpeedButton1: TSpeedButton;
    LbSpeedButton2: TSpeedButton;
    LbSpeedButton4: TSpeedButton;
    dbgTable: TwwDBGrid;
    LbSpeedButton5: TSpeedButton;
    LbSpeedButton6: TSpeedButton;
    LbSpeedButton7: TSpeedButton;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    lvFields: TListView;
    lvIndexes: TListView;
    LbSpeedButton8: TSpeedButton;
    LbSpeedButton9: TSpeedButton;
    PopupMenu1: TPopupMenu;
    Spalteausblenden1: TMenuItem;
    ilTableMenu: TImageList;
    AuswahlbasierterFilter1: TMenuItem;
    AuswahlausschlieenderFilter1: TMenuItem;
    Filterentfernen1: TMenuItem;
    N1: TMenuItem;
    Panel8: TPanel;
    Panel9: TPanel;
    Label1: TLabel;
    mFilter: TMemo;
    LbSpeedButton10: TSpeedButton;
    FixierteSpalten1: TMenuItem;
    N2: TMenuItem;
    Spaltensumme1: TMenuItem;
    Minimalwert1: TMenuItem;
    Maximalwert1: TMenuItem;
    LngsterWert1: TMenuItem;
    LbSpeedButton11: TSpeedButton;
    btnAktualisieren: TSpeedButton;
    pmIndex: TPopupMenu;
    Datenstzezhlen1: TMenuItem;
    RTFbearbeiten1: TMenuItem;
    N3: TMenuItem;
    btnIndex: TSpeedButton;
    SpeedButton1: TSpeedButton;
    QueryStatusPanel: TPanel;
    FilternnachEingabe1: TMenuItem;
    pmFelder: TPopupMenu;
    Feldnamekopieren1: TMenuItem;
    abellennamekopieren1: TMenuItem;
    N4: TMenuItem;
    AlleFelderkopieren1: TMenuItem;
    AlleNotNullFelderkopieren1: TMenuItem;
    EinfacheInsertAnweisungkopieren1: TMenuItem;
    N5: TMenuItem;
    EinfacheInsertAnweisungkopieren2: TMenuItem;
    UnterschiedlicheWerte1: TMenuItem;
    AnzahlunterschiedlicherWerte1: TMenuItem;
    FindDialog1: TFindDialog;
    KrzesterWert11: TMenuItem;
    HsGradientPanel1: THsGradientPanel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);
    procedure dbgTableCalcCellColors(Sender: TObject; Field: TField; State: TGridDrawState; Highlight: boolean; AFont: TFont; ABrush: TBrush);
    procedure dbgTableKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure BtnClick(Sender: TObject);
    procedure Spalteausblenden1Click(Sender: TObject);
    procedure LbSpeedButton8Click(Sender: TObject);
    procedure LbSpeedButton9Click(Sender: TObject);
    procedure dbgTableMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure AuswahlbasierterFilter1Click(Sender: TObject);
    procedure AuswahlausschlieenderFilter1Click(Sender: TObject);
    procedure Filterentfernen1Click(Sender: TObject);
    procedure LbSpeedButton10Click(Sender: TObject);
    procedure FixierteSpalten1Click(Sender: TObject);
    procedure Spaltensumme1Click(Sender: TObject);
    procedure Minimalwert1Click(Sender: TObject);
    procedure Maximalwert1Click(Sender: TObject);
    procedure LngsterWert1Click(Sender: TObject);
    procedure LbSpeedButton11Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnAktualisierenClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure Datenstzezhlen1Click(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure RTFbearbeiten1Click(Sender: TObject);
    procedure mFilterKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure btnIndexClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure dsDataDataChange(Sender: TObject; Field: TField);
    procedure SpeedButton1Click(Sender: TObject);
    procedure FilternnachEingabe1Click(Sender: TObject);
    procedure Feldnamekopieren1Click(Sender: TObject);
    procedure abellennamekopieren1Click(Sender: TObject);
    procedure AlleFelderkopieren1Click(Sender: TObject);
    procedure AlleNotNullFelderkopieren1Click(Sender: TObject);
    procedure EinfacheInsertAnweisungkopieren1Click(Sender: TObject);
    procedure EinfacheInsertAnweisungkopieren2Click(Sender: TObject);
    procedure UnterschiedlicheWerte1Click(Sender: TObject);
    procedure AnzahlunterschiedlicherWerte1Click(Sender: TObject);
    procedure FindDialog1Find(Sender: TObject);
    procedure KrzesterWert11Click(Sender: TObject);
    procedure FindDialog1Close(Sender: TObject);
  private
    FDatabaseForm: TMDI_Database;
    FTableName: string;
    FFindStr: string;
    FFindCaseInsensitive: boolean;
    FNurStruktur: boolean;
    FInitialized: boolean;
    FVerwendeQueryAnstelleTable: boolean;
    FOwnsDataSet: boolean;

    function GetFilter(aField: TField; bInclude: boolean): string; overload;
    function GetFilter(aField: TField; wert: string; bInclude: boolean): string; overload;
    procedure ShowSqlFunction(sQuery, sText: string);
    function GetNextField(var iSearchField: integer): boolean;

    function GetSelectString: string;
    procedure CreateIndexInfo;
    function LoadTable(DB: TDbToolDatabase; TableName: string): TDataSet;
  public
    property NurStruktur: boolean read FNurStruktur;
    property Table: string read FTableName;
    property frmDatabase: TMDI_Database read FDatabaseForm;
    property SelectString: string read GetSelectString;

    constructor Create(Owner: TComponent; ADbFrm: TMDI_Database; ATableName: String; ANurStruktur: boolean=false); reintroduce;
    procedure IndexMenuClick(Sender: TObject);
    procedure Find;
    procedure FindNext;
  end;

implementation

{$R *.DFM}

uses
  Main, Globals, EditRTF, hl.Utils.DBGridUtils, hg_InputQuery, Clipbrd, Query,
  hl_PopupMenuHelper, StrUtils, System.Types, System.UITypes;

resourcestring
  SNothingAvailable = 'Nichts vorhanden';

procedure TMDI_Table.CreateIndexInfo;
var
  aIndexDefs: TIndexDefs;
  i: Integer;
  miNew: TMenuItem;
begin
  if lvIndexes.Tag = 1 then exit;
  if FVerwendeQueryAnstelleTable then
    aIndexDefs := FDatabaseForm.dbDatabase.GetIndexDefs(FTableName)
  else
    aIndexDefs := FDatabaseForm.dbDatabase.GetIndexDefs(dsData.Dataset);
  if aIndexDefs.Count > 0 then
  begin
    for i := 0 to aIndexDefs.Count-1 do
    begin
      lvIndexes.Items.Add.Caption := aIndexDefs.Items[i].Fields;
      lvIndexes.Tag := 1; // Don't call CreateIndexInfo again

      miNew := TMenuItem.Create(pmIndex);
      pmIndex.Items.Add(miNew);
      miNew.Caption := aIndexDefs.Items[i].Fields;
      miNew.OnClick := IndexMenuClick;
    end;
  end;
end;

function TMDI_Table.LoadTable(DB: TDbToolDatabase; TableName: string): TDataSet;
var
  tmp, sql: string;
  slPrimaryKeys: TStringList;
begin
  if FOwnsDataSet then
  begin
    dsData.DataSet.Free; dsData.DataSet := nil;
  end;

  if FVerwendeQueryAnstelleTable then
  begin

    sql := 'select * from ' + DB.SQL_Escape_TableName(TableName); // do not localize

    slPrimaryKeys := TStringList.Create;
    try
      DB.GetPrimaryKeys(slPrimaryKeys, TableName);
      if slPrimaryKeys.Count > 0 then
        sql := sql + ' order by '; // do not localize
      for tmp in slPrimaryKeys do
      begin
        sql := sql + tmp + ',';
      end;
      if slPrimaryKeys.Count > 0 then
        sql := Copy(sql, 1, Length(sql)-1);
    finally
      FreeAndNil(slPrimaryKeys);
    end;

    result := DB.Query(sql);
  end
  else
  begin
    result := DB.GetTable(TableName);
  end;

  FNurStruktur := false;

  FOwnsDataSet := true;
end;

constructor TMDI_Table.Create(Owner: TComponent; ADbFrm: TMDI_Database; ATableName: String; ANurStruktur: boolean=false);
var
  aTable: TDataSet;
  slPrimaryKeys: TStringList;
  slForeignKeys: TStringList;
  aFieldDefs: TFieldDefs;
  anItem: TListItem;
  i: integer;
  datenTyp: string;
resourcestring
  STabelleSS = 'Tabelle %s (%s)';
begin
  inherited Create(Owner);

  // DM 06.02.2024 : Wir verwenden nun TAdoQuery, weil das schneller ist und es bei ADO dann auch case-sensitive gefiltert werden kann
  //                 Dafür sind einige Workarounds und Hacks in C_Database.pas erforderlich, die durchgeführt wurden
  FVerwendeQueryAnstelleTable := true;

  FTableName := ATableName;
  FDatabaseForm := ADbFrm;
  Caption := Format(STabelleSS, [ATableName, ADbFrm.Caption]);
  Panel3.Caption := ' ' + ATableName; // ' ' + Caption;

  FNurStruktur := ANurStruktur;

  if ANurStruktur then
  begin
    if ADbFrm.dbDatabase.DatabaseType = dtSqlServer then
      aTable := ADbFrm.dbDatabase.Query('select top 0 * from ' + ADbFrm.dbDatabase.SQL_Escape_TableName(ATableName)) // do not localize
    else if ADbFrm.dbDatabase.DatabaseType = dtMySql then
      aTable := ADbFrm.dbDatabase.Query('select * from ' + ADbFrm.dbDatabase.SQL_Escape_TableName(ATableName) + ' limit 0') // do not localize
    else
      aTable := ADbFrm.dbDatabase.Query('select * from ' + ADbFrm.dbDatabase.SQL_Escape_TableName(ATableName) + ' where 1=0'); // do not localize
    FOwnsDataSet := true;
  end
  else
  begin
    aTable := LoadTable(ADbFrm.dbDatabase, ATableName);
  end;

  dsData.DataSet := aTable;

  slPrimaryKeys := TStringList.Create;
  slForeignKeys := TStringList.Create;
  try
    // Primary keys finden
    ADbFrm.dbDatabase.GetPrimaryKeys(slPrimaryKeys, ATableName);

    // Foreign keys finden
    ADbFrm.dbDatabase.GetForeignKeys(slForeignKeys, ATableName);

    // In einer View das Löschen verbieten, weil das sehr gefährlich ist (wenn aus mehreren Tabellen gelöscht wird)
    if ADbFrm.dbDatabase.IstHickelSoftProduktDb and ((Copy(ATableName,1,3)='vw_') or (Copy(ATableName,1,5)='X_vw_')) then // do not localize
    begin
      dbgTable.KeyOptions := dbgTable.KeyOptions - [dgAllowDelete];
      dbgTable.KeyOptions := dbgTable.KeyOptions - [dgAllowInsert];
    end;

    // Feldinfo erstellen
    aFieldDefs := ADbFrm.dbDatabase.GetFieldDefs(aTable);
    for i := 0 to aTable.FieldCount-1 do
    begin
      anItem := lvFields.Items.Add;
      anItem.Caption := aFieldDefs.Items[i].Name;
      anItem.SubItems.Add(IntToStr(i+1));
      datenTyp := FieldTypeNames[aFieldDefs.Items[i].DataType];
      if datenTyp = 'Guid' then datenTyp := 'GUID'; // do not localize
      anItem.SubItems.Add(datenTyp);
      if aFieldDefs.Items[i].Precision > 0 then
        anItem.SubItems.Add(IntToStr(aFieldDefs.Items[i].Precision)+','+IntToStr(aFieldDefs.Items[i].Size))
      else
        anItem.SubItems.Add(IntToStr(aFieldDefs.Items[i].Size));

      if (((adFldIsNullable+adFldMayBeNull) and TAdoTable(aTable).Recordset.Fields[i].Attributes) = 0) then
      //if aFieldDefs.Items[i].Required then  // .Required geht nicht mit Delphi ADO! Nur mit TBetterAdoDataSet!
        anItem.SubItems.Add('NOT NULL') // do not localize
      else
        anItem.SubItems.Add('NULL'); // do not localize

      if slPrimaryKeys.IndexOf(aFieldDefs.Items[i].Name) >= 0 then
        anItem.SubItems.Add('PK') // do not localize
      else
        anItem.SubItems.Add(''); // do not localize

      if slForeignKeys.IndexOf(aFieldDefs.Items[i].Name) >= 0 then
        anItem.SubItems.Add('FK') // do not localize
      else
        anItem.SubItems.Add(''); // do not localize

      anItem.Checked := true;
    end;
  finally
    FreeAndNil(slPrimaryKeys);
    FreeAndNil(slForeignKeys);
  end;

  if ANurStruktur then
  begin
    LbSpeedButton6.Down := true;
    BtnClick(LbSpeedButton6);
  end
  else
  begin
    LbSpeedButton2.Down := true;
    BtnClick(LbSpeedButton2);
  end;

  FInitialized := true;
end;

procedure TMDI_Table.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FormDeactivate(Sender);
  if FOwnsDataSet then
  begin
    dsData.DataSet.Free; dsData.DataSet := nil;
  end;
  Action := caFree;
end;

procedure TMDI_Table.FormCreate(Sender: TObject);
begin
  dbgTable.IniAttributes.SaveToRegistry := true;
  dbgTable.IniAttributes.FileName := IncludeTrailingPathDelimiter(ConfigRegKey) + 'Layouts'; // do not localize
  dbgTable.IniAttributes.Delimiter := ';;';

  FInitialized := true;
end;

procedure TMDI_Table.FormActivate(Sender: TObject);
begin
   dbgTable.Color := clTableBackground;
   dbgTable.PaintOptions.AlternatingRowColor := clTableZebra;
   dbgTable.PaintOptions.ActiveRecordColor := clActiveRecord;
   DLG_Main.BearbeitenSuchen1.Enabled := true;
   DLG_Main.BearbeitenWeitersuchen1.Enabled := true;
   DLG_Main.ExtrasExport1.Enabled := not NurStruktur;
   DLG_Main.ExtrasImport1.Enabled := not NurStruktur; // OK?
end;

procedure TMDI_Table.FormDeactivate(Sender: TObject);
begin
   DLG_Main.BearbeitenSuchen1.Enabled := false;
   DLG_Main.BearbeitenWeitersuchen1.Enabled := false;
   DLG_Main.ExtrasExport1.Enabled := false;
   DLG_Main.ExtrasImport1.Enabled := false;
end;

procedure TMDI_Table.dbgTableCalcCellColors(Sender: TObject; Field: TField; State: TGridDrawState; Highlight: boolean; AFont: TFont; ABrush: TBrush);
begin
   if not Assigned(Field) then exit;
   if Field.IsNull then
     ABrush.Color := clNullField;
   if not Highlight then exit;
   if Field.FieldNo = dbgTable.GetActiveField.FieldNo then
     ABrush.Color := clActiveField
   else
     ABrush.Color := clActiveRecord;
   AFont.Color := clTableText;
end;

procedure TMDI_Table.dbgTableKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if Assigned(dbgTable.InplaceEditor) and dbgTable.InplaceEditor.ClassNameIs('TwwInplaceEdit') then // do not localize
  begin
    TwwInplaceEdit(dbgTable.InplaceEditor).Color := clActiveField;
  end;
end;

procedure TMDI_Table.BtnClick(Sender: TObject);

  function _FilterValid(sFilter: string): boolean;
  var
    bQuote: boolean;
    iPos: integer;
    sInhaltOhneStrings: string;
  begin
    // Sicherstellen, dass der Anwender einen gültigen Filter eingegeben hat!
    // Denn wir verwenden ggf. eine TAdoQuery anstelle TAdoTable, da ADO keine FilterOptions erlaubt und wir aber Filter case-sensitive haben wollen!

    bQuote := false;
    sInhaltOhneStrings := '';
    iPos := 1;
    while iPos <= Length(sFilter) do
    begin
      if sFilter[iPos] = '''' then
        bQuote := not bQuote
      else if not bQuote then
      begin
        sInhaltOhneStrings := sInhaltOhneStrings + sFilter[iPos];
      end;
      Inc(iPos);
    end;

    sInhaltOhneStrings := StringReplace(sInhaltOhneStrings, #9, ' ', [rfReplaceAll]);
    sInhaltOhneStrings := StringReplace(sInhaltOhneStrings, #10, ' ', [rfReplaceAll]);
    sInhaltOhneStrings := StringReplace(sInhaltOhneStrings, #13, ' ', [rfReplaceAll]);
    sInhaltOhneStrings := ' ' + sInhaltOhneStrings + ' ';

    result :=
      not ContainsText(sInhaltOhneStrings, ' select ') and // do not localize
      not ContainsText(sInhaltOhneStrings, ' order ') and // do not localize
      not ContainsText(sInhaltOhneStrings, ' join ') and // do not localize
      not ContainsText(sInhaltOhneStrings, ' having ') and // do not localize
      not ContainsText(sInhaltOhneStrings, ' limit ') and // do not localize
      not ContainsText(sInhaltOhneStrings, ' where ') and // do not localize
      not ContainsText(sInhaltOhneStrings, ' group '); // do not localize
  end;

var
  i: integer;
resourcestring
  SFilterInvalid = 'Filter hat einen ungültigen Aufbau';
begin
  if FInitialized then
  begin
    // Felder-Ansicht verlassen: Sichtbare Felder im Grid anpassen
    if Notebook1.ActivePage = LbSpeedButton6.Caption{'Felder'} then
    begin
      // Das MUSS über FieldByName laufen! Vielleicht hat der Benutzer die Reihenfolge der Felder ja geändert...
      for i := 0 to lvFields.Items.Count-1 do
      begin
        dsData.DataSet.Fields.FieldByName(lvFields.Items.Item[i].Caption).Visible := lvFields.Items.Item[i].Checked;
      end;
    end;

    // Filter-Ansicht verlassen: Filter der Tabelle anpassen
    if Notebook1.ActivePage = LbSpeedButton4.Caption{'Filter'} then
    begin
      if FVerwendeQueryAnstelleTable and not _FilterValid(mFilter.Text) then
        raise Exception.Create(SFilterInvalid);
      FDatabaseForm.dbDatabase.SetTableFilter(dsData.DataSet, Trim(mFilter.Text));
    end;
  end;

  // Aktive Seite umstellen
  TSpeedButton(Sender).Down := True;
  Notebook1.ActivePage := TSpeedButton(Sender).Caption; // Attention: Translation must be correct, otherwise this results in failure

  // "Upgrade" notwendig? (Nur-Strukturansicht => Daten)
  if (Notebook1.ActivePage = LbSpeedButton2.Caption{'Tabelle'}) or
     (Notebook1.ActivePage = LbSpeedButton4.Caption{'Filter'}) or
     (Notebook1.ActivePage = LbSpeedButton7.Caption{'Indizes'}) then
  begin
    if NurStruktur then
    begin
      dsData.DataSet := LoadTable(FDatabaseForm.dbDatabase, FTableName);
      if (Notebook1.ActivePage = LbSpeedButton7.Caption{'Indizes'}) then
        CreateIndexInfo;
    end;
  end;

  // Jetzt auf Filter-Seite? Filter in Memo eintragen!
  if Notebook1.ActivePage = LbSpeedButton4.Caption{'Filter'} then
  begin
    mFilter.Text := FDatabaseForm.dbDatabase.GetTableFilter(dsData.DataSet);
  end;

  // Speedbutton Textfarben korrigieren
  FixSpeedButtonColors(Panel1, clWhite);
end;

procedure TMDI_Table.Spalteausblenden1Click(Sender: TObject);
var
  i: integer;
begin
   // Aktuelle Spalte im Feld-ListView suchen, "entkreuzen"
   for i := 0 to lvFields.Items.Count-1 do
   begin
      if lvFields.Items.Item[i].Caption = dbgTable.GetActiveField.FieldName then
      begin
         lvFields.Items.Item[i].Checked := false;
         break;
      end;
   end;
   
   // Das Feld selbst auch ausblenden
   dbgTable.GetActiveField.Visible := false;
end;

procedure TMDI_Table.LbSpeedButton8Click(Sender: TObject);
var
  i: integer;
begin
  // Alle Felder im ListView ankreuzen
  for i := 0 to lvFields.Items.Count-1 do
  begin
    lvFields.Items.Item[i].Checked := true;
  end;
end;

procedure TMDI_Table.LbSpeedButton9Click(Sender: TObject);
var
  i: integer;
begin
  // Alle Felder im ListView "entkreuzen"
  for i := 0 to lvFields.Items.Count-1 do
  begin
    lvFields.Items.Item[i].Checked := false;
  end;
end;

procedure TMDI_Table.dbgTableMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
resourcestring
  SHideColumnS = 'Spalte "%s" ausblenden';
begin
  if (Button = mbRight) and (Y > 17) then
  begin
    Spalteausblenden1.Caption := Format(SHideColumnS, [dbgTable.GetActiveField.FieldName]);
    RTFbearbeiten1.Visible := (dbgTable.GetActiveField.DataType = ftMemo) or (dbgTable.GetActiveField.DataType = ftFmtMemo);
    N3.Visible := RTFbearbeiten1.Visible;
    PopupMenu1.Popup(dbgTable.ClientToScreen(Point(X, 0)).x, dbgTable.ClientToScreen(Point(0, Y)).y);
  end;
end;

procedure TMDI_Table.dsDataDataChange(Sender: TObject; Field: TField);
resourcestring
  SRowCountD = 'Datensätze: %.0n';
begin
  // TStatusBar hat einen Bug: Wenn man ein neues MDI-Fenster aufmacht, dann
  // verschwindet der SimpleText für immer.
  QueryStatusPanel.Caption := '  ' + Format(SRowCountD, [TDataSource(Sender).DataSet.RecordCount/1.0]);
end;

procedure TMDI_Table.EinfacheInsertAnweisungkopieren1Click(Sender: TObject);
var
  i: integer;
  s, s2: string;
  typ: string;
  res: integer;
resourcestring
  SMultipleRowsWithComments = 'Mehrzeilig mit Kommentaren?';
  SCopiedInClipboard = 'In Zwischenablage kopiert';
begin
  // TODO: "AutoInc" nicht einfügen

  res := MessageDlg(SMultipleRowsWithComments, mtConfirmation, mbYesNoCancel, 0);
  if res = mrCancel then exit;

  if res = mrYes then
  begin
    s := #13#10;
    s2 := #13#10;
  end
  else
  begin
    s := '';
    s2 := '';
  end;
  for i := 0 to lvFields.Items.Count - 1 do
  begin
    if lvFields.Items[i].SubItems.Strings[3] = 'NOT NULL' then // do not localize
    begin
      s := s + lvFields.Items.Item[i].Caption;

      typ := lvFields.Items[i].SubItems.Strings[1];
      if (typ = 'DateTime') or (typ = 'String') then // do not localize
        s2 := s2 + ''''''
      else
        s2 := s2 + '0'; // TODO: weitere Typen unterstützen

      if res = mrYes then
      begin
        if i < lvFields.Items.Count - 1 then
        begin
          s := s + ',' + #13#10;
          s2 := s2 + ', -- ' + lvFields.Items.Item[i].Caption + #13#10; // do not localize
        end
        else
        begin
          s := s + #13#10;
          s2 := s2 + ' -- ' + lvFields.Items.Item[i].Caption + #13#10; // do not localize
        end;
      end
      else
      begin
        s := s + ', ';
        s2 := s2 + ', ';
      end;
    end;
  end;
  if s = '' then
    ShowMessage(SNothingAvailable)
  else
  begin
    s  := Copy(s,  1, Length(s)-2);  // remove ', '
    s2 := Copy(s2, 1, Length(s2)-2); // remove ', '
    Clipboard.AsText := 'insert into '+Table+' ('+s+') values ('+s2+')'; // do not localize
    // ShowMessage(SCopiedInClipboard);
  end;
end;

procedure TMDI_Table.EinfacheInsertAnweisungkopieren2Click(Sender: TObject);
var
  i: integer;
  s, s2: string;
  typ: string;
  res: integer;
resourcestring
  SMultipleRowsWithComments2 = 'Mehrzeilig mit Kommentaren?';
  SCopiedInClipboard = 'In Zwischenablage kopiert';
begin
  // TODO: "AutoInc" nicht einfügen

  res := MessageDlg(SMultipleRowsWithComments2, mtConfirmation, mbYesNoCancel, 0);
  if res = mrCancel then exit;

  if res = mrYes then
  begin
    s := #13#10;
    s2 := #13#10;
  end
  else
  begin
    s := '';
    s2 := '';
  end;
  for i := 0 to lvFields.Items.Count - 1 do
  begin
    s := s + lvFields.Items.Item[i].Caption;

    typ := lvFields.Items[i].SubItems.Strings[1];
    if lvFields.Items[i].SubItems.Strings[3] = 'NULL' then // do not localize
      s2 := s2 + 'NULL' // do not localize
    else if (typ = 'DateTime') or (typ = 'String') then // do not localize
      s2 := s2 + ''''''
    else
      s2 := s2 + '0'; // TODO: weitere Typen unterstützen

    if res = mrYes then
    begin
      if i < lvFields.Items.Count - 1 then
      begin
        s := s + ',' + #13#10;
        s2 := s2 + ', -- ' + lvFields.Items.Item[i].Caption + #13#10; // do not localize
      end
      else
      begin
        s := s + #13#10;
        s2 := s2 + ' -- ' + lvFields.Items.Item[i].Caption + #13#10; // do not localize
      end;
    end
    else
    begin
      if i < lvFields.Items.Count - 1 then
      begin
        s := s + ', ';
        s2 := s2 + ', ';
      end;
    end;
  end;
  if s = '' then
    ShowMessage(SNothingAvailable)
  else
  begin
    Clipboard.AsText := 'insert into '+Table+' ('+s+') values ('+s2+')'; // do not localize
    // ShowMessage(SCopiedInClipboard);
  end;
end;

function TMDI_Table.GetFilter(aField: TField; bInclude: boolean): string;
resourcestring
  SFieldTypeNotSupported = 'Dieser Feldtyp kann (noch) nicht automatisch gefiltert werden.';
begin
  if aField.IsNull then
  begin
    if FVerwendeQueryAnstelleTable then
    begin
      if bInclude then
        result := aField.FieldName + ' is null ' // do not localize
      else
        result := aField.FieldName + ' is not null '; // do not localize
    end
    else
    begin
      // Beim Filter muss man wirklich "= null" schreiben und nicht "is null"
      if bInclude then
        result := aField.FieldName + ' = null ' // do not localize
      else
        result := aField.FieldName + ' <> null '; // do not localize
    end;
  end
  else
  begin
    if bInclude then
      result := aField.FieldName + ' = '
    else
      result := aField.FieldName + ' <> ';

     case aField.DataType of
        ftBoolean:
           if aField.AsBoolean then
             result := result + '1'
           else
             result := result + '0';

        ftInteger,
        ftFloat,
        ftSmallint,
        ftAutoInc,
        ftCurrency,
        ftBCD,
        ftFMTBcd,
        ftLargeint,
        ftWord:
           result := result + StringReplace(aField.AsString, ',', '.', []);

        ftString,
        ftFixedChar,
        ftWideString,
        ftDateTime: // DM 05.12.2023 : OK mit SQL Server
           result := result + '''' + FDatabaseForm.dbDatabase.SQL_Escape_String(aField.AsString) + '''';

     else

           // Wir versuchen's einfach! Vielleicht geht es ja!
           result := result + '''' + FDatabaseForm.dbDatabase.SQL_Escape_String(aField.AsString) + '''';

           //result := '';
           //Application.MessageBox(SFieldTypeNotSupported, PChar(Application.Title), MB_ICONEXCLAMATION + MB_OK);
     end;

     if FVerwendeQueryAnstelleTable then
     begin
       if bInclude then
         result := Trim(result)
       else
         result := Trim(result) + ' or ' + aField.FieldName + ' is null'; // https://github.com/hickelsoft/dbtool/issues/2
     end
     else
     begin
       // This does not work!
       // (SUCHNAME <> '12345' or SUCHNAME is null) and (ADRESSART = 'LIE')
       // because you cannot mix or/and for some reason
     end;
  end;


// ftDate, ftTime, ftDateTime, ftTimeStamp
// ftGuid

(*
   ftBoolean:

   if TTable(dsData.DataSet).FieldByName(wwDBGrid1.GetActiveField.FieldName).AsString = '' then
      result := '(' + wwDBGrid1.GetActiveField.FieldName + ' <> 1) and (' + wwDBGrid1.GetActiveField.FieldName + ' <> 0)'
   else
      result := result + TTable(dsData.DataSet).FieldByName(wwDBGrid1.GetActiveField.FieldName).AsString;
*)
end;

function TMDI_Table.GetFilter(aField: TField; wert: string; bInclude: boolean): string;
resourcestring
  SFieldTypeNotSupported = 'Dieser Feldtyp kann (noch) nicht automatisch gefiltert werden.';
begin
  if bInclude then
    result := aField.FieldName + ' = '
  else
    result := aField.FieldName + ' <> ';

  case aField.DataType of
      ftBoolean:
         result := result + wert; // soll 1 oder 0 sein

      ftInteger,
      ftFloat,
      ftSmallint,
      ftAutoInc,
      ftCurrency,
      ftBCD,
      ftFMTBcd,
      ftLargeint,
      ftWord:
         result := result + StringReplace(wert, ',', '.', []);

      ftString,
      ftFixedChar,
      ftWideString,
      ftDateTime: // DM 05.12.2023 : OK mit SQL Server
         result := result + '''' + FDatabaseForm.dbDatabase.SQL_Escape_String(wert) + '''';

  else

         // Wir versuchen's einfach! Vielleicht geht es ja! Bei DateTime geht es.
         result := result + '''' + FDatabaseForm.dbDatabase.SQL_Escape_String(wert) + '''';

         // result := '';
         //Application.MessageBox(SFieldTypeNotSupported, PChar(Application.Title), MB_ICONEXCLAMATION + MB_OK);
  end;

  if FVerwendeQueryAnstelleTable then
    // https://github.com/hickelsoft/dbtool/issues/2
    result := aField.FieldName + ' is null or ' + Trim(result)


// ftDate, ftTime, ftDateTime, ftTimeStamp
// ftGuid

(*
   ftBoolean:

   if TTable(dsData.DataSet).FieldByName(wwDBGrid1.GetActiveField.FieldName).AsString = '' then
      result := '(' + wwDBGrid1.GetActiveField.FieldName + ' <> 1) and (' + wwDBGrid1.GetActiveField.FieldName + ' <> 0)'
   else
      result := result + TTable(dsData.DataSet).FieldByName(wwDBGrid1.GetActiveField.FieldName).AsString;
*)
end;

procedure TMDI_Table.AuswahlbasierterFilter1Click(Sender: TObject);
var
  sFilter, sNeu: string;
begin
  sNeu := GetFilter(dbgTable.GetActiveField, true);
  if(sNeu = '') then exit;
  sFilter := FDatabaseForm.dbDatabase.GetTableFilter(dsData.DataSet);
  if sFilter <> '' then sFilter := sFilter + ' and '; // do not localize
  FDatabaseForm.dbDatabase.SetTableFilter(dsData.DataSet, sFilter + '(' + sNeu + ')');
end;

procedure TMDI_Table.abellennamekopieren1Click(Sender: TObject);
resourcestring
  SCopiedInClipboard = 'In Zwischenablage kopiert';
begin
  Clipboard.AsText := Table;
  // ShowMessage(SCopiedInClipboard);
end;

procedure TMDI_Table.AlleFelderkopieren1Click(Sender: TObject);
var
  i: integer;
  s: string;
resourcestring
  SCopiedInClipboard = 'In Zwischenablage kopiert';
begin
  s := '';
  for i := 0 to lvFields.Items.Count - 1 do
  begin
    s := s + lvFields.Items.Item[i].Caption;
    s := s + ', ';
  end;
  s := Copy(s, 1, Length(s)-2); // ', ' am Ende entf.
  if s = '' then
    ShowMessage(SNothingAvailable)
  else
  begin
    Clipboard.AsText := s;
    // ShowMessage(SCopiedInClipboard);
  end;
end;

procedure TMDI_Table.AlleNotNullFelderkopieren1Click(Sender: TObject);
var
  i: integer;
  s: string;
resourcestring
  SCopiedInClipboard = 'In Zwischenablage kopiert';
begin
  s := '';
  for i := 0 to lvFields.Items.Count - 1 do
  begin
    if lvFields.Items[i].SubItems.Strings[3] = 'NOT NULL' then
    begin
      s := s + lvFields.Items.Item[i].Caption;
      s := s + ', ';
    end;
  end;
  s := Copy(s, 1, Length(s)-2); // ', ' am Ende entf.
  if s = '' then
    ShowMessage(SNothingAvailable)
  else
  begin
    Clipboard.AsText := s;
    // ShowMessage(SCopiedInClipboard);
  end;
end;

procedure TMDI_Table.AnzahlunterschiedlicherWerte1Click(Sender: TObject);
resourcestring
  SCountDifferentValues = 'Anzahl unterschiedliche Werte';
begin
   ShowSqlFunction('COUNT(DISTINCT ' + dbgTable.GetActiveField.FieldName + ')', SCountDifferentValues); // do not localize
end;

procedure TMDI_Table.AuswahlausschlieenderFilter1Click(Sender: TObject);
var
  sFilter, sNeu: string;
begin
   sNeu := GetFilter(dbgTable.GetActiveField, false);
   if(sNeu = '') then exit;
   sFilter := FDatabaseForm.dbDatabase.GetTableFilter(dsData.DataSet);
   if sFilter <> '' then sFilter := sFilter + ' and '; // do not localize
   FDatabaseForm.dbDatabase.SetTableFilter(dsData.DataSet, sFilter + '(' + sNeu + ')'); // do not localize
end;

procedure TMDI_Table.Feldnamekopieren1Click(Sender: TObject);
resourcestring
  SCopiedInClipboard = 'In Zwischenablage kopiert';
begin
  if lvFields.SelCount = 0 then exit;
  Clipboard.AsText := lvFields.Selected.Caption;
  // ShowMessage(SCopiedInClipboard);
end;

procedure TMDI_Table.Filterentfernen1Click(Sender: TObject);
begin
   FDatabaseForm.dbDatabase.SetTableFilter(dsData.DataSet, '');
end;

procedure TMDI_Table.FilternnachEingabe1Click(Sender: TObject);
var
  sFilter, sNeu, wert: string;
resourcestring
  SCopyByInputValue = 'Filtern nach Eingabewert';
begin
  if not ThgInputQry.InputQuery(SCopyByInputValue, wert) then exit;
  sNeu := GetFilter(dbgTable.GetActiveField, wert, true);
  if(sNeu = '') then exit;
  sFilter := FDatabaseForm.dbDatabase.GetTableFilter(dsData.DataSet);
  if sFilter <> '' then sFilter := sFilter + ' and '; // do not localize
  FDatabaseForm.dbDatabase.SetTableFilter(dsData.DataSet, sFilter + '(' + sNeu + ')'); // do not localize
end;

procedure TMDI_Table.LbSpeedButton10Click(Sender: TObject);
begin
  mFilter.Text := '';
  BtnClick(LbSpeedButton2);
end;

procedure TMDI_Table.FixierteSpalten1Click(Sender: TObject);
resourcestring
  SCountFixedColumns = 'Anzahl fixierter Spalten:';
begin
   dbgTable.FixedCols := StrToInt(InputBox(Application.Title, SCountFixedColumns, IntToStr(dbgTable.FixedCols)));
end;

procedure TMDI_Table.ShowSqlFunction(sQuery, sText: string);
var
  x: TDataSet;
  sSQL: string;
  sFilter: string;
  sResult: string;
resourcestring
  SCannotBeDeterminedMessageFromServer = '%s kann nicht ermittelt werden. Fehlermeldung des Servers: "%s"';
  SResultLine = '%s: %s';
begin
   sSQL := 'SELECT ' + sQuery + ' FROM ' + frmDatabase.dbDatabase.SQL_Escape_TableName(FTableName); // do not localize
   sFilter := FDatabaseForm.dbDatabase.GetTableFilter(dsData.DataSet);
   if sFilter <> '' then sSQL := sSQL + ' WHERE (' + sFilter + ')'; // do not localize
   sResult := sText;

   try
      x := FDatabaseForm.dbDatabase.Query(sSQL + ';'); // do not localize
      try
        sResult := Format(SResultLine, [sResult, x.Fields.Fields[0].AsString]);
      finally
        FreeAndNil(x);
      end;
   except
     on E: EAbort do
     begin
       Abort;
     end;
     on E: Exception do
     begin
       sResult := Format(SCannotBeDeterminedMessageFromServer, [sResult, e.Message]);
     end;
   end;
   Application.MessageBox(PChar(sResult), PChar(Application.Title), MB_ICONINFORMATION + MB_OK);
end;

procedure TMDI_Table.Spaltensumme1Click(Sender: TObject);
resourcestring
  SColumnSum = 'Spaltensumme';
begin
   ShowSqlFunction('SUM(' + dbgTable.GetActiveField.FieldName + ')', SColumnSum); // do not localize
end;

procedure TMDI_Table.SpeedButton1Click(Sender: TObject);
begin
  BtnClick(LbSpeedButton2);
end;

function CheckCase(s: string): integer; // 0=all lowercase, 1=mixed, 2=all uppercase
var
  i: integer;
  allLower, allUpper: boolean;
begin
  allLower := true;
  allUpper := true;
  for i := 1 to Length(s) do
  begin
    if CharInSet(s[i], ['a'..'z']) then allUpper := false;
    if CharInSet(s[i], ['A'..'Z']) then allLower := false;
  end;

  if allLower = allUpper then
    result := 1 // all mixed
  else if allLower then
    result := 0 // all lower case
  else
    result := 2; // all upper case
end;

procedure TMDI_Table.UnterschiedlicheWerte1Click(Sender: TObject);
var
  sSQL: string;
  sFilter: string;
  anzahlFieldName: string;
resourcestring
  SAmount = 'Anzahl';
begin
   case CheckCase(dbgTable.GetActiveField.FieldName) of
     0: anzahlFieldName := AnsiLowerCase(SAmount);
     1: anzahlFieldName := SAmount;
     2: anzahlFieldName := AnsiUpperCase(SAmount);
   end;

   sSQL := 'SELECT DISTINCT ' + frmDatabase.dbDatabase.SQL_Escape_FieldName(dbgTable.GetActiveField.FieldName) + ', COUNT(*) as ' + frmDatabase.dbDatabase.SQL_Escape_FieldName(anzahlFieldName) + ' FROM ' + frmDatabase.dbDatabase.SQL_Escape_TableName(FTableName); // do not localize
   sFilter := FDatabaseForm.dbDatabase.GetTableFilter(dsData.DataSet);
   if sFilter <> '' then sSQL := sSQL + ' WHERE (' + sFilter + ')'; // do not localize
   sSQL := sSQL + ' GROUP BY ' + frmDatabase.dbDatabase.SQL_Escape_FieldName(dbgTable.GetActiveField.FieldName); // do not localize
   sSQL := sSQL + ' ORDER BY ' + frmDatabase.dbDatabase.SQL_Escape_FieldName(dbgTable.GetActiveField.FieldName) + ';'; // do not localize

   with TMDI_Query.Create(FDatabaseForm, FDatabaseForm) do
   begin
     Memo1.Lines.Text := sSQL;
     SpeedButton1.Click;
   end;
end;

procedure TMDI_Table.Minimalwert1Click(Sender: TObject);
resourcestring
  SMinimumValue = 'Minimalwert';
begin
  ShowSqlFunction('MIN(' + dbgTable.GetActiveField.FieldName + ')', SMinimumValue); // do not localize
end;

procedure TMDI_Table.Maximalwert1Click(Sender: TObject);
resourcestring
  SMaximumValue = 'Maximalwert';
begin
  ShowSqlFunction('MAX(' + dbgTable.GetActiveField.FieldName + ')', SMaximumValue); // do not localize
end;

procedure TMDI_Table.Datenstzezhlen1Click(Sender: TObject);
resourcestring
  SRowCount = 'Anzahl Datensätze';
begin
  ShowSqlFunction('COUNT(*)', SRowCount); // do not localize
end;

procedure TMDI_Table.LngsterWert1Click(Sender: TObject);
var
  x: TDataSet;
  iMaxLen: integer;
  sMaxVal: string;
  sResult: string;
  sSQL: string;
  sFilter: string;
resourcestring
  SLongestValue = 'Längster Wert: "%s" (%d Zeichen)';
  SLongestValueCannotBeDetermined = 'Längster Wert kann nicht ermittelt werden. Fehlermeldung des Servers: "%s"';
begin
  sSQL := 'SELECT ' + frmDatabase.dbDatabase.SQL_Escape_FieldName(dbgTable.GetActiveField.FieldName) + ' FROM ' + frmDatabase.dbDatabase.SQL_Escape_TableName(FTableName); // do not localize
  sFilter := FDatabaseForm.dbDatabase.GetTableFilter(dsData.DataSet);
  if sFilter <> '' then sSQL := sSQL + ' WHERE (' + sFilter + ')'; // do not localize

   try
      x := FDatabaseForm.dbDatabase.Query(sSQL + ';');
      try
        iMaxLen := 0;
        while not x.Eof do
        begin
           if Length(x.Fields.Fields[0].AsString) > iMaxLen then
           begin
              sMaxVal := x.Fields.Fields[0].AsString;
              iMaxLen := Length(sMaxVal);
           end;
           x.Next;
        end;

        sResult := Format(SLongestValue, [sMaxVal, iMaxLen]);
      finally
        FreeAndNil(x);
      end;
   except
     on E: EAbort do
     begin
       Abort;
     end;
     on E: Exception do
     begin
       sResult := Format(SLongestValueCannotBeDetermined, [e.Message]);
     end;
   end;
   Application.MessageBox(PChar(sResult), PChar(Application.Title), MB_ICONINFORMATION + MB_OK);
end;

procedure TMDI_Table.LbSpeedButton11Click(Sender: TObject);
resourcestring
  SLayoutSaved = 'Layout wurde gespeichert.';
begin
   dbgTable.IniAttributes.SectionName := StringReplace(FDatabaseForm.Database + '.' + FTableName, '\', '_', [rfReplaceAll]); // do not localize
   dbgTable.SaveToIniFile;
   Application.MessageBox(PChar(SLayoutSaved), PChar(Application.Title), MB_ICONINFORMATION + MB_OK);
end;

procedure TMDI_Table.FormShow(Sender: TObject);
begin
   dbgTable.IniAttributes.SectionName := StringReplace(FDatabaseForm.Database + '.' + FTableName, '\', '_', [rfReplaceAll]); // do not localize
   dbgTable.SafeLoadFromIniFile;
end;

procedure TMDI_Table.btnAktualisierenClick(Sender: TObject);
begin
   FDatabaseForm.dbDatabase.RefreshTable(dsData.DataSet);
end;

procedure TMDI_Table.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if ord(Key) = VK_F5 then btnAktualisierenClick(Sender);
  if (Key = VK_ESCAPE) and (not Assigned(dbgTable.DataSource.DataSet) or not (dbgTable.DataSource.DataSet.State in [dsEdit,dsInsert]))
     // TODO: Wenn man im Eingabemodus von einem Darumsfeld ist, dann wird ESC trotzdem dazu führen, dass man rausfliegt. InplaceEditor ist bei DateTime-Edit nicht gesetzt
     and not (Assigned(dbgTable.InplaceEditor) and dbgTable.InplaceEditor.ClassNameIs('TwwInplaceEdit') and dbgTable.InplaceEditor.Visible) // do not localize
     then Close;
end;

procedure TMDI_Table.IndexMenuClick(Sender: TObject);
var
  i: integer;
begin
  if FVerwendeQueryAnstelleTable then exit; // TODO: !!! Nicht implementiert (TODO: Implementieren mittels ORDER BY, aber dann muss SetTableFilter/GetTableFilter muss angepasst werden)

  FDatabaseForm.dbDatabase.SetTableIndex(dsData.DataSet, StringReplace(TMenuItem(Sender).Caption, '&', '', [rfReplaceAll]));

  for i := 0 to pmIndex.Items.Count-1 do
  begin
    pmIndex.Items.Items[i].Checked := false;
  end;
  TMenuItem(Sender).Checked := true;
end;

procedure TMDI_Table.KrzesterWert11Click(Sender: TObject);
var
  x: TDataSet;
  iMinLen: integer;
  sMinVal: string;
  sResult: string;
  sSQL: string;
  sFilter: string;
resourcestring
  SShortestNonEmptyValue = 'Kürzester nicht leerer Wert: "%s" (%d Zeichen)';
  SShortestValueCannotBeDetermined = 'Kürzester Wert kann nicht ermittelt werden. Fehlermeldung des Servers: "%s"';
begin
  sSQL := 'SELECT ' + frmDatabase.dbDatabase.SQL_Escape_FieldName(dbgTable.GetActiveField.FieldName) + ' FROM ' + frmDatabase.dbDatabase.SQL_Escape_TableName(FTableName); // do not localize
  sFilter := FDatabaseForm.dbDatabase.GetTableFilter(dsData.DataSet);
  if sFilter <> '' then sSQL := sSQL + ' WHERE (' + sFilter + ')'; // do not localize

   try
      x := FDatabaseForm.dbDatabase.Query(sSQL + ';');
      try
        iMinLen := 999999;
        while not x.Eof do
        begin
           if Length(x.Fields.Fields[0].AsString) < iMinLen then
           begin
              sMinVal := x.Fields.Fields[0].AsString;
              iMinLen := Length(sMinVal);
           end;
           x.Next;
        end;

        sResult := Format(SShortestNonEmptyValue, [sMinVal, iMinLen]);
      finally
        FreeAndNil(x);
      end;
   except
     on E: EAbort do
     begin
       Abort;
     end;
     on E: Exception do
     begin
       sResult := Format(SShortestValueCannotBeDetermined, [e.Message]);
     end;
   end;
   Application.MessageBox(PChar(sResult), PChar(Application.Title), MB_ICONINFORMATION + MB_OK);
end;

procedure TMDI_Table.Find;
begin
  FindDialog1.Execute(Handle);
end;

function TMDI_Table.GetNextField(var iSearchField: integer): boolean;
var
  bResult: boolean;
resourcestring
  SEndOfTableReached = 'Tabellen-Ende erreicht. Es wurden keine weiteren Vorkommen des Suchtexts gefunden.';
begin
   bResult := true;
   Inc(iSearchField);

   while (iSearchField < dsData.DataSet.FieldCount) and not dsData.DataSet.Fields.Fields[iSearchField].Visible do Inc(iSearchField);

   if iSearchField = dsData.DataSet.FieldCount then
   begin
      iSearchField := 0;

      while (iSearchField < dsData.DataSet.FieldCount) and not dsData.DataSet.Fields.Fields[iSearchField].Visible do Inc(iSearchField);
      dsData.DataSet.Next;

      if dsData.DataSet.Eof then
      begin
         iSearchField := dsData.DataSet.FieldCount-1;
         while(iSearchField > 0) and not dsData.DataSet.Fields.Fields[iSearchField].Visible do Dec(iSearchField);
         dbgTable.SetActiveField(dsData.DataSet.Fields.Fields[iSearchField].FieldName);
         Application.MessageBox(PChar(SEndOfTableReached), PChar(Application.Title), MB_ICONINFORMATION + MB_OK);
         bResult := false;
      end;
   end;
   result := bResult;
end;

procedure TMDI_Table.FindNext;
var
  iPos, iSearchField: integer;
  sUpper: string;
begin
   iSearchField := dbgTable.GetActiveField.FieldNo-1;
   if not GetNextField(iSearchField) then exit;
   if FFindStr = '' then Find;
   if FFindStr = '' then exit;

   sUpper := UpperCase(FFindStr);

   while true do
   begin
      try
         if FFindCaseInsensitive then
           iPos := Pos(sUpper, UpperCase(dsData.DataSet.Fields.Fields[iSearchField].AsString))
         else
           iPos := Pos(FFindStr, dsData.DataSet.Fields.Fields[iSearchField].AsString);
      except
         iPos := 0;
      end;

      if iPos > 0 then break;
      if not GetNextField(iSearchField) then break;
   end;

   dbgTable.SetActiveField(dsData.DataSet.Fields.Fields[iSearchField].FieldName);
end;

function TMDI_Table.GetSelectString: string;
var
  bAddComma: boolean;
  sSQL: string;
  sFilter: string;
  i: integer;
begin
  sSQL := 'SELECT '; // do not localize
  bAddComma := false;
  for i := 0 to lvFields.Items.Count-1 do
  begin
    if(lvFields.Items.Item[i].Checked) then
    begin
      if bAddComma then sSQL := sSQL + ', ';
      sSQL := sSQL + frmDatabase.dbDatabase.SQL_Escape_FieldName(lvFields.Items.Item[i].Caption);
      bAddComma := true;
    end;
  end;

  sSQL := sSQL + ' FROM ' + frmDatabase.dbDatabase.SQL_Escape_TableName(FTableName); // do not localize
  sFilter := FDatabaseForm.dbDatabase.GetTableFilter(dsData.DataSet);
  if sFilter <> '' then sSQL := sSQL + ' WHERE (' + sFilter + ')'; // do not localize

  result := sSQL;
end;

procedure TMDI_Table.RTFbearbeiten1Click(Sender: TObject);
var
  aDlg: TDLG_EditRTF;
begin
  aDlg := TDLG_EditRTF.Create(Self);
  try
    aDlg.Caption := dbgTable.GetActiveField.FieldName;
    aDlg.DBRichEdit1.DataSource := dsData;
    aDlg.DBRichEdit1.DataField := dbgTable.GetActiveField.FieldName;
    aDlg.ShowModal;
  finally
    FreeAndNil(aDlg);
  end;
end;

procedure TMDI_Table.mFilterKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if (ssCtrl in Shift) and (ord(Key) = ord('A')) then // Ctrl+A
  begin
    TMemo(Sender).SelectAll;
  end
  else if Key = VK_F9 then
  begin
    BtnClick(LbSpeedButton2);
  end;
end;

procedure TMDI_Table.btnIndexClick(Sender: TObject);
begin
  CreateIndexInfo;
  pmIndex.OpenPopupOnControl(TControl(Sender));
end;

procedure TMDI_Table.FindDialog1Close(Sender: TObject);
begin
  // Workaround für seltsamen Fehler, nämlich wird beim schließen des Suchen-Dialogs
  // eine Anwendung aus dem Hintergrund vorgeholt, nämlich wenn MainFormOnTaskBar=true
  // Die ganzen Tipps im Internet mit Popupmode usw. klappen incht...
  SetForegroundWindow(Application.Handle);
end;

procedure TMDI_Table.FindDialog1Find(Sender: TObject);
begin
  FFindStr := FindDialog1.FindText;
  FFindCaseInsensitive := not (frMatchCase in FindDialog1.Options);
  FindNext;
end;

end.
