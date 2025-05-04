unit Query;

{$INCLUDE 'Globals.inc'}

interface

uses
  Windows, SysUtils, Classes, Controls, StdCtrls, ExtCtrls, Forms, Database,
  Graphics,
  DB, DBGrids, Grids, Wwdbgrid, Wwdbigrd, ComCtrls, Menus, ImgList, Buttons,
  C_Database, adodb, Dialogs, HsGradientPanel;

type
  TMDI_Query = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Notebook1: TNotebook;
    Panel3: TPanel;
    dbgTable: TwwDBGrid;
    Panel6: TPanel;
    Memo1: TMemo;
    Panel7: TPanel;
    SpeedButton1: TSpeedButton;
    DataSource1: TDataSource;
    Splitter1: TSplitter;
    btnNewQueryWindow: TSpeedButton;
    SpeedButton2: TSpeedButton;
    QueryStatusPanel: TPanel;
    SaveDialog1: TSaveDialog;
    OpenDialog1: TOpenDialog;
    FindDialog1: TFindDialog;
    btnSaveQuery: TSpeedButton;
    btnLoadQuery: TSpeedButton;
    btnCSVExport: TSpeedButton;
    HsGradientPanel1: THsGradientPanel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);
    procedure dbgTableCalcCellColors(Sender: TObject; Field: TField;
      State: TGridDrawState; Highlight: boolean; AFont: TFont; ABrush: TBrush);
    procedure dbgTableKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormDeactivate(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure btnNewQueryWindowClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Memo1KeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure DataSource1DataChange(Sender: TObject; Field: TField);
    procedure FindDialog1Find(Sender: TObject);
    procedure btnCSVExportClick(Sender: TObject);
    procedure btnLoadQueryClick(Sender: TObject);
    procedure btnSaveQueryClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FindDialog1Close(Sender: TObject);
  private
    FDatabaseForm: TMDI_Database;
    FFindStr: String;
    FDatabase: TDbToolDatabase;
    FFindCaseInsensitive: boolean;
    FShareDBObj: boolean;
    function GetNextField(var iSearchField: integer): boolean;
    function GetReadOnly: boolean;
    procedure SetReadOnly(const Value: boolean);
  public
    property ReadOnly: boolean read GetReadOnly write SetReadOnly;
    property frmDatabase: TMDI_Database read FDatabaseForm;
    constructor Create(Owner: TComponent; DbFrm: TMDI_Database); reintroduce;
    destructor Destroy; override;
    property Database: TDbToolDatabase read FDatabase;
    procedure Find;
    procedure FindNext;
  end;

implementation

{$R *.DFM}

uses
  Main, Globals, EditRTF, StrUtils, hl.Utils;

constructor TMDI_Query.Create(Owner: TComponent; DbFrm: TMDI_Database);
resourcestring
  SQueryFor = 'Abfrage für %s';
begin
  inherited Create(Owner);

{$IFNDEF WIN64}
  FShareDBObj := DbFrm.dbDatabase.DatabaseType = dtLocal;
{$ELSE}
  FShareDBObj := false;
{$ENDIF}
  if FShareDBObj then
    FDatabase := DbFrm.dbDatabase
  else
    FDatabase := DbFrm.dbDatabase.Clone;

  FDatabaseForm := DbFrm;
  Caption := Format(SQueryFor, [DbFrm.Caption]);
  // Panel3.Caption := ' ' + Caption;
end;

procedure TMDI_Query.DataSource1DataChange(Sender: TObject; Field: TField);
resourcestring
  SDatasetsD = 'Datensätze: %.0n';
begin
  // TStatusBar hat einen Bug: Wenn man ein neues MDI-Fenster aufmacht, dann
  // verschwindet der SimpleText für immer.
  QueryStatusPanel.Caption := '  ' + Format(SDatasetsD,
    [TDataSource(Sender).DataSet.RecordCount / 1.0]);
end;

procedure TMDI_Query.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(DataSource1.DataSet) then
  begin
    DataSource1.DataSet.Free;
    DataSource1.DataSet := nil;
  end;

  FormDeactivate(Sender);

  Action := caFree;
end;

procedure TMDI_Query.FormCloseQuery(Sender: TObject; var CanClose: boolean);
resourcestring
  SReallyClose = 'Fenster schließen und Abfrage verwerfen?';
begin
  if (Trim(Memo1.Text) <> '') and (Memo1.Modified) then
  begin
    MDI_Form_BringToFront(Self);
    // TODO: - funktioniert bei 2 fenstern und beim dritten nicht?! (Fehler nur in Delphi 11)
    // - Wenn man das Hauptfenster zu macht, dann geht das MDI-Child-Form NICHT zu, wenn man "Ja" klickt!! Wenn man also beim dritten Form "Nein" sagt, dann sind die anderen 2 immer noch offen (Fehler in Delphi 11 und 2007)
    CanClose := Application.MessageBox(PChar(SReallyClose),
      PChar(Application.Title), MB_ICONQUESTION + MB_YESNOCANCEL) = ID_YES;
  end
  else
    CanClose := true;
end;

procedure TMDI_Query.FormCreate(Sender: TObject);
begin
  (*
    dbgTable.IniAttributes.SaveToRegistry := true;
    dbgTable.IniAttributes.FileName := IncludeTrailingPathDelimiter(ConfigRegKey) + 'Layouts'; // do not localize
    dbgTable.IniAttributes.Delimiter := ';;';
  *)
end;

procedure TMDI_Query.FormActivate(Sender: TObject);
begin
  dbgTable.Color := clTableBackground;
  dbgTable.PaintOptions.AlternatingRowColor := clTableZebra;
  dbgTable.PaintOptions.ActiveRecordColor := clActiveRecord;
  DLG_Main.BearbeitenSuchen1.Enabled := true;
  DLG_Main.BearbeitenWeitersuchen1.Enabled := true;
  DLG_Main.ExtrasExport1.Enabled := true;
  DLG_Main.ExtrasImport1.Enabled := false;
end;

procedure TMDI_Query.FormDeactivate(Sender: TObject);
begin
  DLG_Main.BearbeitenSuchen1.Enabled := false;
  DLG_Main.BearbeitenWeitersuchen1.Enabled := false;
  DLG_Main.ExtrasExport1.Enabled := false;
  DLG_Main.ExtrasImport1.Enabled := false;
end;

procedure TMDI_Query.dbgTableCalcCellColors(Sender: TObject; Field: TField;
  State: TGridDrawState; Highlight: boolean; AFont: TFont; ABrush: TBrush);
begin
  if not Assigned(Field) then
    exit;
  if Field.IsNull then
    ABrush.Color := clNullField;
  if not Highlight then
    exit;
  if Field.FieldNo = dbgTable.GetActiveField.FieldNo then
    ABrush.Color := clActiveField
  else
    ABrush.Color := clActiveRecord;
  AFont.Color := clTableText;
end;

procedure TMDI_Query.dbgTableKeyUp(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  // TODO: sorgt dieser code für gelegentliche abstürze bei inplaceeditor "Listenindex überschreitet maximum (-1)"?
  if Assigned(dbgTable.InplaceEditor) and dbgTable.InplaceEditor.ClassNameIs
    ('TwwInplaceEdit') then // do not localize
  begin
    TwwInplaceEdit(dbgTable.InplaceEditor).Color := clActiveField;
  end;
end;

destructor TMDI_Query.Destroy;
begin
  if not FShareDBObj then
    FreeAndNil(FDatabase);
  inherited;
end;

procedure TMDI_Query.FormKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if Key = VK_F9 then
  begin
    Key := 0;
    SpeedButton1.Click;
  end;
  if (Key = VK_ESCAPE) and (not Assigned(DataSource1.DataSet) or
    not(DataSource1.DataSet.State in [dsEdit, dsInsert]))
  // TODO: Wenn man im Eingabemodus von einem Darumsfeld ist, dann wird ESC trotzdem dazu führen, dass man rausfliegt. InplaceEditor ist bei DateTime-Edit nicht gesetzt
    and not(Assigned(dbgTable.InplaceEditor) and
    dbgTable.InplaceEditor.ClassNameIs('TwwInplaceEdit') and
    dbgTable.InplaceEditor.Visible) // do not localize
  then
  begin
    Key := 0;
    Close;
  end;
end;

procedure TMDI_Query.Find;
begin
  if not dbgTable.Visible then
    exit;
  FindDialog1.Execute(Handle);
end;

procedure TMDI_Query.FindDialog1Close(Sender: TObject);
begin
  // Workaround für seltsamen Fehler, nämlich wird beim schließen des Suchen-Dialogs
  // eine Anwendung aus dem Hintergrund vorgeholt, nämlich wenn MainFormOnTaskBar=true
  // Die ganzen Tipps im Internet mit Popupmode usw. klappen incht...
  SetForegroundWindow(Application.Handle);
end;

procedure TMDI_Query.FindDialog1Find(Sender: TObject);
begin
  FFindStr := FindDialog1.FindText;
  FFindCaseInsensitive := not(frMatchCase in FindDialog1.Options);
  FindNext;
end;

function TMDI_Query.GetNextField(var iSearchField: integer): boolean;
resourcestring
  STableEndReached =
    'Tabellen-Ende erreicht. Es wurden keine weiteren Vorkommen des Suchtexts gefunden.';
begin
  result := true;
  Inc(iSearchField);

  while (iSearchField < DataSource1.DataSet.FieldCount) and
    (DataSource1.DataSet.Fields.Fields[iSearchField].Visible = false) do
  begin
    Inc(iSearchField);
  end;

  if (iSearchField = DataSource1.DataSet.FieldCount) then
  begin
    iSearchField := 0;

    while (iSearchField < DataSource1.DataSet.FieldCount) and
      (DataSource1.DataSet.Fields.Fields[iSearchField].Visible = false) do
    begin
      Inc(iSearchField);
    end;
    DataSource1.DataSet.Next;

    if DataSource1.DataSet.Eof then
    begin
      iSearchField := DataSource1.DataSet.FieldCount - 1;
      while (iSearchField > 0) and not DataSource1.DataSet.Fields.Fields
        [iSearchField].Visible do
      begin
        Dec(iSearchField);
      end;
      dbgTable.SetActiveField(DataSource1.DataSet.Fields.Fields[iSearchField]
        .FieldName);
      Application.MessageBox(PChar(STableEndReached), PChar(Application.Title),
        MB_ICONINFORMATION + MB_OK);
      result := false;
    end;
  end;
end;

function TMDI_Query.GetReadOnly: boolean;
begin
  result := dbgTable.ReadOnly;
end;

procedure TMDI_Query.Memo1KeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if (ssCtrl in Shift) and (Key = ord('A')) then
  begin
    Key := 0;
    TMemo(Sender).SelectAll;
  end
  (*
    else if (ssCtrl in Shift) and (Key = ord('O')) then
    begin
    Key := 0;
    SQLBefehlffnen1Click(pmSql);
    end
    else if (ssCtrl in Shift) and (Key = ord('S')) then
    begin
    Key := 0;
    Speichern1Click(pmSql);
    end;
  *)
end;

procedure TMDI_Query.FindNext;
var
  iPos, iSearchField: integer;
  sUpper: string;
  f: TField;
begin
  if not dbgTable.Visible then
    exit;
  f := dbgTable.GetActiveField;
  if f = nil then
    exit; // can happen if you had a syntax error before

  iSearchField := f.FieldNo - 1;
  if not GetNextField(iSearchField) then
    exit;
  if FFindStr = '' then
    Find;
  if FFindStr = '' then
    exit;

  sUpper := UpperCase(FFindStr);

  while true do
  begin
    try
      if (FFindCaseInsensitive) then
        iPos := Pos(sUpper, UpperCase(DataSource1.DataSet.Fields.Fields
          [iSearchField].AsString))
      else
        iPos := Pos(FFindStr, DataSource1.DataSet.Fields.Fields[iSearchField]
          .AsString);
    except
      on E: EAbort do
      begin
        Abort;
      end;
      on E: Exception do
      begin
        iPos := 0;
      end;
    end;

    if iPos > 0 then
      break;
    if not GetNextField(iSearchField) then
      break;
  end;

  dbgTable.SetActiveField(DataSource1.DataSet.Fields.Fields[iSearchField]
    .FieldName);
end;

type
  TADOAcc = class(TADODataSet)
  public
    property Recordset;
  end;

procedure TMDI_Query.SetReadOnly(const Value: boolean);
begin
  dbgTable.ReadOnly := Value;
  Memo1.ReadOnly := Value;
  if Value then
    Memo1.Color := clBtnFace
  else
    Memo1.Color := clWindow;
  btnLoadQuery.Visible := not Value;
  btnSaveQuery.Visible := not Value;
  btnNewQueryWindow.Visible := not Value;
end;

procedure TMDI_Query.SpeedButton1Click(Sender: TObject);
var
  iPos: integer;
  bLastWasQuery, bQuote: boolean;
  sBefehl, sGesamt, sGesamtMitZeilenumbruechen: string;
  slTmp: TStringList;
  iLine: integer;
  sLine: string;
resourcestring
  SSqlExecuted = 'SQL-Befehl ausgeführt.';
begin
  // Bitte Synchron halten:  DBtool\Query.pas  und hcg_Generic_SQL_Query.pas

  // 1. Schritt: Kommentarzeilen entfernen

  slTmp := TStringList.Create;
  try
    slTmp.Text := Memo1.Text;

    for iLine := 0 to slTmp.Count - 1 do
    begin
      sLine := slTmp.Strings[iLine];

      bQuote := false;

      iPos := 1;
      while iPos <= Length(sLine) - 1 do
      // Length()-1 ist korrekt, denn wir greifen auf iPos+1 zu!
      begin
        if sLine[iPos] = '''' then
          bQuote := not bQuote
          // Hier werden "--" Kommentare berücksichtigt
          // TODO: Auch /* */ Kommentare berücksichtigen!!!
        else if (sLine[iPos] = '-') and (sLine[iPos + 1] = '-') and not bQuote
        then
        begin
          sLine := Trim(Copy(sLine, 1, iPos - 1));
          slTmp.Strings[iLine] := sLine;
          break;
        end;
        Inc(iPos);
      end;

      if (sLine <> '') and (ContainsText(sLine, ' Procedure') or
        ContainsText(sLine, ' Trigger') or ContainsText(sLine, ' View')) then
        break; // do not localize
    end;

    sGesamt := slTmp.Text;
  finally
    FreeAndNil(slTmp);
  end;

  // 2. Schritt: Vielleicht haben wir mehrere Befehle auf einmal... Splitten und einzeln ausführen.

  sGesamtMitZeilenumbruechen := sGesamt;
  sGesamt := Trim(StringReplace(StringReplace(sGesamt, #13, ' ',
    [rfReplaceAll]), #10, ' ', [rfReplaceAll]));

  if sGesamt = '' then
    exit;

  bLastWasQuery := false;

  if (sGesamt <> '') and ((ContainsText(sGesamt, ' View')) or // do not localize
    (ContainsText(sGesamt, ' Procedure') and not ContainsText(sGesamt,
    '-- ' + SExecuteStoredProcedureWith_ + ' ')) or // do not localize
    (ContainsText(sGesamt, ' Trigger') and not ContainsText(sGesamt,
    '-- ' + STriggerActived) and not ContainsText(sGesamt,
    '-- ' + STriggerDeActived))) then // do not localize
  begin
    if Assigned(DataSource1.DataSet) then
    begin
      DataSource1.DataSet.Free;
      DataSource1.DataSet := nil;
    end;
    DataSource1.DataSet := FDatabase.Query(sGesamtMitZeilenumbruechen);
    bLastWasQuery := DataSource1.DataSet <> nil;
  end
  else
  begin
    while sGesamt <> '' do
    begin
      bQuote := false;

      iPos := 1;
      while iPos <= Length(sGesamt) do
      begin
        if sGesamt[iPos] = '''' then
          bQuote := not bQuote
        else if (sGesamt[iPos] = ';') and not bQuote then
          break;
        Inc(iPos);
      end;

      sBefehl := Trim(Copy(sGesamt, 1, iPos));

      if Assigned(DataSource1.DataSet) then
      begin
        DataSource1.DataSet.Free;
        DataSource1.DataSet := nil;
      end;
      (*
        if (UpperCase(Copy(sBefehl, 1, 6)) = 'SELECT') or // do not localize
        (UpperCase(Copy(sBefehl, 1, 7)) = 'EXPLAIN') or // do not localize
        (UpperCase(Copy(sBefehl, 1, 4)) = 'SHOW') then // do not localize
        begin
        bLastWasQuery := true;
        DataSource1.DataSet := FDatabase.Query(sBefehl);
        end
        else
        begin
        bLastWasQuery := false;
        FDatabase.ExecSql(sBefehl);
        end;
      *)
      DataSource1.DataSet := FDatabase.Query(sBefehl);
      bLastWasQuery := DataSource1.DataSet <> nil;

      sGesamt := Trim(Copy(sGesamt, Length(sBefehl) + 1));
    end;
  end;

  dbgTable.TabStop := true; // DataSource1.DataSet.RecordCount > 0;
  dbgTable.Visible := true;
  // war bug: wenn man im query fenster ist und auf den leeren dbgrid klickt und dann eine taste drückt, kommt listenindex fehler!

  if not bLastWasQuery then
    Application.MessageBox(PChar(SSqlExecuted), PChar(Application.Title),
      MB_ICONINFORMATION + MB_OK);
end;

procedure TMDI_Query.btnCSVExportClick(Sender: TObject);
begin
  DLG_Main.ExtrasExport1Execute(DLG_Main.ExtrasExport1);
end;

procedure TMDI_Query.btnLoadQueryClick(Sender: TObject);
begin
  if Modus_CORA_Verzeichnis then
  begin
    if DirectoryExists('C:\CORA2012\Scripts') then // do not localize
      OpenDialog1.InitialDir := 'C:\CORA2012\Scripts' // do not localize
    else if DirectoryExists('C:\CORAplus\Scripts') then // do not localize
      OpenDialog1.InitialDir := 'C:\CORAplus\Scripts'; // do not localize
  end;
  if OpenDialog1.Execute then
  begin
    Memo1.Lines.LoadFromFile(OpenDialog1.FileName);
    Memo1.Modified := false;
  end;
end;

procedure TMDI_Query.btnNewQueryWindowClick(Sender: TObject);
var
  frm: TMDI_Query;
begin
  frm := TMDI_Query.Create(FDatabaseForm, FDatabaseForm);
  frm.ReadOnly := ReadOnly;
end;

procedure TMDI_Query.btnSaveQueryClick(Sender: TObject);
begin
  if Modus_CORA_Verzeichnis then
  begin
    if DirectoryExists('C:\CORA2012\Scripts') then // do not localize
      SaveDialog1.InitialDir := 'C:\CORA2012\Scripts' // do not localize
    else if DirectoryExists('C:\CORAplus\Scripts') then // do not localize
      SaveDialog1.InitialDir := 'C:\CORAplus\Scripts'; // do not localize
  end;
  if SaveDialog1.Execute then
  begin
    Memo1.Lines.SaveToFile(SaveDialog1.FileName);
    Memo1.Modified := false;
  end;
end;

end.
