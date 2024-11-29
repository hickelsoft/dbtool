unit Database;

{$INCLUDE 'Globals.inc'}

interface

uses
  SysUtils, Windows, Classes, Controls, StdCtrls, Forms, ExtCtrls, Menus, ComCtrls, Grids,
  DB, Dialogs, Buttons, C_Database, Wwdbgrid, Wwdbigrd, Graphics, ClipBrd,
  HsGradientPanel;

type
  TMDI_Database = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    pmTables: TPopupMenu;
    ffnen1: TMenuItem;
    Loeschen1: TMenuItem;
    Leeren1: TMenuItem;
    uUmbenennen1: TMenuItem;
    Notebook1: TNotebook;
    Panel3: TPanel;
    btnAktualisieren: TSpeedButton;
    SpeedButton2: TSpeedButton;
    lvTables: TListView;
    Panel5: TPanel;
    NurStrukturzeigen1: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    Strukturdrucken1: TMenuItem;
    abellenNamenkopieren1: TMenuItem;
    ViewDefinitionanzeigen1: TMenuItem;
    N3: TMenuItem;
    Kopierennach1: TMenuItem;
    N4: TMenuItem;
    AnzahlZeilenzeigen1: TMenuItem;
    Erste100Zeilenzeigen1: TMenuItem;
    Triggeranzeigen1: TMenuItem;
    Panel4: TPanel;
    HsGradientPanel1: THsGradientPanel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure lvTablesDblClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure Leeren1Click(Sender: TObject);
    procedure lvTablesDragOver(Sender: TObject; Source: TObject; X, Y: integer; State: TDragState; var Accept: boolean);
    procedure lvTablesDragDrop(Sender: TObject; Source: TObject; X, Y: integer);
    procedure Loeschen1Click(Sender: TObject);
    procedure btnAktualisierenClick(Sender: TObject);
    procedure lvTablesEdited(Sender: TObject; Item: TListItem; var S: String);
    procedure Umbenennen1Click(Sender: TObject);
    procedure pmTablesPopup(Sender: TObject);
    procedure lvTablesKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure NurStrukturzeigen1Click(Sender: TObject);
    procedure lvTablesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure abellenNamenkopieren1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ViewDefinitionanzeigen1Click(Sender: TObject);
    procedure lvTablesCustomDrawItem(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure AnzahlZeilenzeigen1Click(Sender: TObject);
    procedure Erste100Zeilenzeigen1Click(Sender: TObject);
    procedure Strukturdrucken1Click(Sender: TObject);
    procedure Triggeranzeigen1Click(Sender: TObject);
  private
    FDatabaseStr: string;
    FDatabaseObj: TDbToolDatabase;
    procedure ReloadTabellenListe;
    procedure UpdateCounterPanel;
    procedure KopierenNachDBClick(Sender: TObject);
  public
    property Database: String read FDatabaseStr;
    property dbDatabase: TDbToolDatabase read FDatabaseObj;
    constructor Create(Owner: TComponent; DatabaseName: string); reintroduce;
    procedure OpenTable(TableName: string; NurStruktur: boolean=false);
  end;

implementation

uses
  Main, Table, Query, Export, Globals, Printers, hl.Utils, hl_Datenbank;

{$R *.DFM}

constructor TMDI_Database.Create(Owner: TComponent; DatabaseName: string);
var
  tmpDbName: string;
resourcestring
  SMySQL_S = 'MySQL: %s';
  SDatabase_S = 'Datenbank: %s';
  SSqlServer_S = 'SQL Server: %s';
  SOn = 'auf';
begin
  inherited Create(Owner); // Aus irgendeinem Grund ruft diese Funktion das OnShow aus !!!

  FDatabaseObj := TDbToolDatabase.Create(DatabaseName);
  FDatabaseStr := DatabaseName;

  if FDatabaseObj.DatabaseType = dtSqlServer then
  begin
    DatabaseName := DatabaseName + ';';
    FDatabaseStr := ConnStrReadAttr('Initial Catalog', Copy(DatabaseName, Length('_SQLSRV:')+1)); // do not localize
    tmpDbName := FDatabaseStr;
    FDatabaseStr := FDatabaseStr+';Data Source='+ConnStrReadAttr('Data Source', Copy(DatabaseName, Length('_SQLSRV:')+1)); // do not localize
    FDatabaseStr := Format(SSqlServer_S, [StringReplace(FDatabaseStr, ';Data Source=', ' '+SOn+' ', [])]); // do not localize

    // Ist es eine CORA- oder HSInfo2-Datenbank? Dann darf nur ein HickelSOFT-Mitarbeiter dran!
    if not FDatabaseObj.CheckDatabaseSecurityPassword then
    begin
      Close;
      Exit;
    end;
  end;
  if FDatabaseObj.DatabaseType = dtMySql then
  begin
    FDatabaseStr := Copy(DatabaseName, 17);
    tmpDbName := FDatabaseStr;
    FDatabaseStr := Format(SMySQL_S, [Copy(FDatabaseStr, 1, Length(FDatabaseStr)-1)]);
  end;

  Caption := Format(SDatabase_S, [FDatabaseStr]);
  if (tmpDbName <> '') and (Pos(';', tmpDbName) >= 1) then
  begin
    tmpDbName := Copy(tmpDbName, 1, Pos(';', tmpDbName)-1);
    Panel3.Caption := ' ' + tmpDbName; // ' ' + Caption;
  end;

  ReloadTabellenListe; // Müssen wir machen, weil das OnShow bereits oben ausgelöst wurde, aus welchem Grund auch immer!!!
end;

procedure TMDI_Database.Erste100Zeilenzeigen1Click(Sender: TObject);
var
  QueryForm: TMDI_Query;
  tableName: string;
  i: Integer;
begin
  for i := 0 to lvTables.Items.Count-1 do
  begin
    if lvTables.Items.Item[i].Selected then
    begin
      if (Integer(lvTables.Items.Item[i].Data) and 3) in [0{Table}, 1{View}] then
      begin
        tableName := lvTables.Items.Item[i].Caption;
        QueryForm := TMDI_Query.Create(Self, Self);
        if FDatabaseObj.DatabaseType = dtSqlServer then
          QueryForm.Memo1.Text := 'select top 1000 * from ' + FDatabaseObj.SQL_Escape_TableName(tableName) // do not localize
        else
          QueryForm.Memo1.Text := 'select * from ' + FDatabaseObj.SQL_Escape_TableName(tableName) + ' limit 1000'; // do not localize
        QueryForm.Memo1.Modified := false;
        QueryForm.SpeedButton1.Click;
        QueryForm.BringToFront;
      end;
    end;
  end;
end;

procedure TMDI_Database.ReloadTabellenListe;
var
  i: integer;
  slTables: TStringList;
  slAllViews: TStringList;
  slAllStoredProcedures: TStringList;
  slAllTriggers: TStringList;
begin
  if FDatabaseObj = nil then exit;

  lvTables.Items.BeginUpdate;
  try
    {$IF CompilerVersion < 20.0} // Version geraten
    // Dieser "Hack" bewirkt, dass die Spalten nicht "zusammenschrumpfen", wenn man die Liste neu lädt.
    lvTables.ViewStyle := vsReport;
    {$IFEND}

    lvTables.Items.Clear;

    slTables := TStringList.Create;
    slAllViews := TSTringList.Create;
    slAllStoredProcedures := TStringList.Create;
    slAllTriggers := TStringList.Create;
    try
      FDatabaseObj.GetTableNames(slTables); // includes Views!!
      FDatabaseObj.GetAllViews(slAllViews);
      FDatabaseObj.GetAllStoredProcedures(slAllStoredProcedures);
      FDatabaseObj.GetAllTablesWithTriggers(slAllTriggers);
      slTables.AddStrings(slAllStoredProcedures);
      slTables.Sort;
      for i := 0 to slTables.Count - 1 do
      begin
        with lvTables.Items.Add do
        begin
          if slAllViews.IndexOf(slTables.Strings[i]) <> -1 then
            Data := Pointer(1{View})
          else if slAllStoredProcedures.IndexOf(slTables.Strings[i]) <> -1 then
            Data := Pointer(2{SP})
          else
            Data := Pointer(0{Table});

          if slAllTriggers.IndexOf(slTables.Strings[i]) <> -1 then
            Data := Pointer(Integer(Data) or 4{Trigger});

          Caption := slTables.Strings[i];
        end;
      end;
    finally
      FreeAndNil(slTables);
      FreeAndNil(slAllViews);
      FreeAndNil(slAllStoredProcedures);
      FreeAndNil(slAllTriggers);
    end;
  finally
    lvTables.ViewStyle := vsList;
    lvTables.Items.EndUpdate;
  end;

  {$IF CompilerVersion >= 20.0} // Version geraten
  // Dieser "Hack" bewirkt, dass die Spalten nicht "zusammenschrumpfen", wenn man die Liste neu lädt.
  lvTables.ViewStyle := vsReport;
  lvTables.ViewStyle := vsList;
  {$IFEND}

  UpdateCounterPanel;
end;

procedure TMDI_Database.Triggeranzeigen1Click(Sender: TObject);
var
  sl: TStringList;
  i, j: Integer;
  qw: TMDI_Query;
begin
  for i := 0 to lvTables.Items.Count-1 do
  begin
    if lvTables.Items.Item[i].Selected then
    begin
      if (Integer(lvTables.Items.Item[i].Data) and 4{Trigger}) <> 0 then
      begin
        sl := TStringList.Create;
        try
          FDatabaseObj.GetTriggers(lvTables.Items.Item[i].Caption, sl);
          for j := 0 to sl.Count-1 do
          begin
            qw := TMDI_Query.Create(Self, Self);
            qw.Memo1.Text := sl.ValueFromIndex[j];
            qw.Memo1.Modified := false;
            qw.Panel6.Height := Round(qw.ClientHeight * 0.7);
            qw.BringToFront;
          end;
        finally
          FreeAndNil(sl);
        end;
      end;
    end;
  end;
end;

procedure TMDI_Database.UpdateCounterPanel;
var
  i, cnt: integer;
resourcestring
  SObjectsD = 'Objekte: %.0n';
  SSSelected = '%s (%d selektiert)';
begin
  cnt := 0;
  for i := 0 to lvTables.Items.Count - 1 do
  begin
    if lvTables.Items[i].Selected then Inc(cnt);
  end;

  // TStatusBar hat einen Bug: Wenn man ein neues MDI-Fenster aufmacht, dann
  // verschwindet der SimpleText für immer.
  Panel5.Caption := '  '+Format(SObjectsD, [lvTables.Items.Count/1.0]);
  if cnt > 0 then
  begin
    Panel5.Caption := Format(SSSelected, [Panel5.Caption, cnt]);
  end;
end;

procedure TMDI_Database.ViewDefinitionanzeigen1Click(Sender: TObject);
var
  def: string;
  qw: TMDI_Query;
  i: Integer;
begin
  for i := 0 to lvTables.Items.Count-1 do
  begin
    if lvTables.Items.Item[i].Selected then
    begin
      if (Integer(lvTables.Items.Item[i].Data) and 3) = 1{View} then
      begin
        def := FDatabaseObj.GetViewDefinition(lvTables.Items.Item[i].Caption);
        if def <> '' then
        begin
          qw := TMDI_Query.Create(Self, Self);
          qw.Memo1.Text := def;
          qw.Memo1.Modified := false;
          qw.Panel6.Height := Round(qw.ClientHeight * 0.7);
          qw.SpeedButton1.Click;
          qw.BringToFront;
        end;
      end;
    end;
  end;
end;

procedure TMDI_Database.OpenTable(TableName: string; NurStruktur: boolean=false);
var
  i: integer;
begin
  // Prüfen: Ist die Tabelle schon offen?
  // Wenn ja: Fenster in den Vordergrund bringen und raus hier!
  for i := 0 to DLG_Main.MDIChildCount-1 do
  begin
    if DLG_Main.MDIChildren[i].ClassNameIs('TMDI_Table') and  // do not localize
       (TMDI_Table(DLG_Main.MDIChildren[i]).Table = TableName) and
       (TMDI_Table(DLG_Main.MDIChildren[i]).frmDatabase = self) then
    begin
      if TMDI_Table(DLG_Main.MDIChildren[i]).NurStruktur and not NurStruktur then
      begin
        // "Upgrade"
        DLG_Main.MDIChildren[i].Close;
        // KEIN EXIT. Wir öffnen das Fenster nun richtig
      end
      else if not TMDI_Table(DLG_Main.MDIChildren[i]).NurStruktur and NurStruktur then
      begin
        TMDI_Table(DLG_Main.MDIChildren[i]).LbSpeedButton6.Down := true;
        TMDI_Table(DLG_Main.MDIChildren[i]).BtnClick(TMDI_Table(DLG_Main.MDIChildren[i]).LbSpeedButton6);
        MDI_Form_BringToFront(DLG_Main.MDIChildren[i]);
        exit;
      end
      else
      begin
        MDI_Form_BringToFront(DLG_Main.MDIChildren[i]);
        exit;
      end;
    end;
  end;

  TMDI_Table.Create(Self, Self, TableName, NurStruktur);
end;

procedure TMDI_Database.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(OnDeactivate) then OnDeactivate(Sender);
  Action := caFree;
end;

procedure TMDI_Database.FormCreate(Sender: TObject);
begin
  (*
  dbgTable.IniAttributes.SaveToRegistry := true;
  dbgTable.IniAttributes.FileName := IncludeTrailingPathDelimiter(ConfigRegKey) + 'Layouts';  // do not localize
  dbgTable.IniAttributes.Delimiter := ';;';
  *)
end;

procedure TMDI_Database.lvTablesCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  // Bit 0..1 Type = 0b00 (0) Table
  //                 0b01 (1) View
  //                 0b10 (2) Stored Procedure
  //                 0b11 (3) Undefined
  if (Integer(Item.Data) and 3) = 0{Table} then
    Sender.Canvas.Font.Color := clWindowText
  else if (Integer(Item.Data) and 3) = 1{View} then
    Sender.Canvas.Font.Color := clGreen
  else if (Integer(Item.Data) and 3) = 2{SP} then
    Sender.Canvas.Font.Color := clBlue
  else if (Integer(Item.Data) and 3) = 3{Undefined} then
    Assert(false);

  // Bit 2 (4) = Trigger
  if (Integer(Item.Data) and 4{Trigger}) <> 0 then
    Sender.Canvas.Font.Style := [TFontStyle.fsBold];
end;

procedure TMDI_Database.lvTablesDblClick(Sender: TObject);
var
  i: integer;
  qw: TMDI_Query;
  def: string;
begin
  for i := 0 to lvTables.Items.Count-1 do
  begin
    if lvTables.Items.Item[i].Selected then
    begin
      if (Integer(lvTables.Items.Item[i].Data) and 3) = 2{SP} then
      begin
        def := FDatabaseObj.GetStoredProcedureDefinition(lvTables.Items.Item[i].Caption);
        if def <> '' then
        begin
          qw := TMDI_Query.Create(Self, Self);
          qw.Memo1.Text := def;
          qw.Memo1.Modified := false;
          qw.Panel6.Height := Round(qw.ClientHeight * 0.7);
          qw.BringToFront;
        end;
      end
      else
      begin
        OpenTable(lvTables.Items.Item[i].Caption, false);
      end;
    end;
  end;
end;

procedure TMDI_Database.FormDestroy(Sender: TObject);
begin
  if Assigned(FDatabaseObj) then FreeAndNil(FDatabaseObj);
end;

procedure TMDI_Database.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if (Key = VK_F5) then
  begin
    ReloadTabellenListe;
  end;
  if Key = VK_ESCAPE then Close;
end;

procedure TMDI_Database.FormShow(Sender: TObject);
begin
  ReloadTabellenListe;
end;

procedure TMDI_Database.Leeren1Click(Sender: TObject);
var
  i: integer;
  viewsIgnoriert: integer;
  tabellenGeleert: integer;
resourcestring
  SDiscardAreYouSure = 'Sind Sie sicher, dass Sie die ausgewählten Tabellen leeren möchten?';
  SNoDeleteOnViews1 = 'Aus Sicherheitsgründen ist diese Funktion bei Views nicht möglich!';
  SNoDeleteOnViewsN = 'Aus Sicherheitsgründen ist diese Funktion bei Views nicht möglich! (%d Views)';
  SSelectedDObjectsTruncated = 'Die ausgewählten %d Tabelle(n) wurden geleert.';
begin
  if lvTables.SelCount = 0 then exit;
  if Application.MessageBox(PChar(SDiscardAreYouSure), PChar(Application.Title), MB_ICONQUESTION + MB_YESNOCANCEL) = idYes then
  begin
    viewsIgnoriert := 0;
    tabellenGeleert := 0;
    for i := 0 to lvTables.Items.Count-1 do
    begin
      if not lvTables.Items.Item[i].Selected then continue;
      if FDatabaseObj.ViewDetectionImplemented then
      begin
        if (Integer(lvTables.Items.Item[i].Data) and 3) = 2{SP} then
        begin
          // Nix machen mit Stored Procedures
        end
        else if (Integer(lvTables.Items.Item[i].Data) and 3) = 1{View} then
        begin
          // Bei einem DELETE würden aus allen verknüpften Tabellen gelöscht werden
          Inc(viewsIgnoriert);
        end
        else
        begin
          if FDatabaseObj.DatabaseType = dtSqlServer then
          begin
            // DELETE anstelle TRUNCATE, da man bei einer Tabelle mit Foreign Keys kein TRUNCATE machen kann (keine Ahnung warum)
            FDatabaseObj.ExecSql('DELETE FROM ' + FDatabaseObj.SQL_Escape_TableName(lvTables.Items.Item[i].Caption) + ';'); // do not localize
          end
          else
          begin
            FDatabaseObj.ExecSql('TRUNCATE TABLE ' + FDatabaseObj.SQL_Escape_TableName(lvTables.Items.Item[i].Caption) + ';'); // do not localize
          end;
          Inc(tabellenGeleert);
        end;
      end
      else
      begin
        // DM 03.05.2023 : DELETE FROM durch TRUNCATE TABLE ersetzt, da sonst bei Views ein Datenverlust entsteht!
        //if(lvTables.Items.Item[i].Selected) then FDatabaseObj.ExecSql('DELETE FROM ' + FDatabaseObj.SQL_Escape_TableName(lvTables.Items.Item[i].Caption) + ';');
        FDatabaseObj.ExecSql('TRUNCATE TABLE ' + FDatabaseObj.SQL_Escape_TableName(lvTables.Items.Item[i].Caption) + ';'); // do not localize
        Inc(tabellenGeleert);
      end;
    end;
    if viewsIgnoriert = 1 then
      Application.MessageBox(PChar(SNoDeleteOnViews1), PChar(Application.Title), MB_ICONSTOP + MB_OK)
    else if viewsIgnoriert > 1 then
      Application.MessageBox(PChar(Format(SNoDeleteOnViewsN, [viewsIgnoriert])), PChar(Application.Title), MB_ICONSTOP + MB_OK);

    //if tabellenGeleert >= 1 then
    //  Application.MessageBox(PChar(Format(SSelectedDObjectsTruncated, [tabellenGeleert])), PChar(Application.Title), MB_ICONINFORMATION + MB_OK);
  end;
end;

procedure TMDI_Database.SpeedButton2Click(Sender: TObject);
begin
  TMDI_Query.Create(Self, Self);
end;

procedure TMDI_Database.Strukturdrucken1Click(Sender: TObject);
begin
  // TODO: Implement
end;

procedure TMDI_Database.lvTablesDragOver(Sender: TObject; Source: TObject; X, Y: integer; State: TDragState; var Accept: boolean);
begin
  Accept := Source.ClassNameIs('TListView') and (TListView(Source) <> lvTables);  // do not localize
end;

procedure TMDI_Database.lvTablesDragDrop(Sender: TObject; Source: TObject; X, Y: integer);
var
  lvSource: TListView;
  dbSource: TDbToolDatabase;
  i: integer;
begin
  Self.BringToFront;

  lvSource := TListView(Source);
  dbSource := TMDI_Database(TListView(Source).Owner).dbDatabase;

  try
    // Den Rest der Arbeit soll gefälligst die Zieldatenbank übernehmen!
    for i := 0 to lvSource.Items.Count-1 do
    begin
      if(lvSource.Items.Item[i].Selected) then
      begin
        FDatabaseObj.ImportFromDatabase(dbSource, lvSource.Items.Item[i].Caption);
      end;
    end;
  finally
    // Jetzt noch schnell unsere ListView anpassen, wir haben vielleicht eine neue Tabelle bekommen...
    FDatabaseObj.CommitRetaining;
    ReloadTabellenListe;
    BringToFront;
  end;
end;

procedure TMDI_Database.Loeschen1Click(Sender: TObject);
var
  i: integer;
  tabellenGeloescht: integer;
resourcestring
  SDeleteAreYouSure = 'Sind Sie sicher, dass Sie die ausgewählten Objekte löschen möchten?';
  SSelectedDObjectsDeleted = 'Die ausgewählten %d Objekte wurden gelöscht.';
begin
  if lvTables.SelCount = 0 then exit;
  if Application.MessageBox(PChar(SDeleteAreYouSure), PChar(Application.Title), MB_ICONQUESTION + MB_YESNOCANCEL) = IDYES then
  begin
    try
      tabellenGeloescht := 0;

      for i := 0 to lvTables.Items.Count-1 do
      begin
        if(lvTables.Items.Item[i].Selected) then
        begin
          if (Integer(lvTables.Items.Item[i].Data) and 3) = 2{SP} then
            FDatabaseObj.DropStoredProcedure(lvTables.Items.Item[i].Caption)
          else
            FDatabaseObj.DropTable(lvTables.Items.Item[i].Caption);
          Inc(tabellenGeloescht);
        end;
      end;

      //if tabellenGeloescht >= 1 then
      //  Application.MessageBox(PChar(Format(SSelectedDObjectsDeleted, [tabellenGeloescht])), PChar(Application.Title), MB_ICONINFORMATION + MB_OK);
    finally
      ReloadTabellenListe;
    end;
  end;
end;

procedure TMDI_Database.abellenNamenkopieren1Click(Sender: TObject);
var
  i, cnt: integer;
  outputStr: string;
resourcestring
  SNoTableSelected = 'Keine Tabellen selektiert.';
begin
  outputStr := '';
  cnt := 0;
  for i := 0 to lvTables.Items.Count - 1 do
  begin
    if lvTables.Items[i].Selected then
    begin
      outputStr := outputStr + #13#10 + lvTables.Items[i].Caption;
      inc(cnt);
    end;
  end;
  outputStr := Trim(outputStr);

  if cnt > 0 then // if outputStr <> '' then
  begin
    ClipBrd.Clipboard.AsText := outputStr;
    // ShowMessageFmt('Namen von %d Tabelle(n) in die Zwischenablage kopiert.', [cnt]);
  end
  else
  begin
    ShowMessage(SNoTableSelected);
  end;
end;

procedure TMDI_Database.AnzahlZeilenzeigen1Click(Sender: TObject);
var
  QueryForm: TMDI_Query;
  tableName: string;
  i: Integer;
begin
  for i := 0 to lvTables.Items.Count-1 do
  begin
    if lvTables.Items.Item[i].Selected then
    begin
      if (Integer(lvTables.Items.Item[i].Data) and 3) in [0{Table}, 1{View}] then
      begin
        tableName := lvTables.Items.Item[i].Caption;
        QueryForm := TMDI_Query.Create(Self, Self);
        QueryForm.Memo1.Text := 'select count(*) from ' + FDatabaseObj.SQL_Escape_TableName(tableName);  // do not localize
        QueryForm.Memo1.Modified := false;
        QueryForm.SpeedButton1.Click;
        QueryForm.BringToFront;
      end;
    end;
  end;
end;

procedure TMDI_Database.btnAktualisierenClick(Sender: TObject);
begin
  ReloadTabellenListe;
end;

procedure TMDI_Database.lvTablesEdited(Sender: TObject; Item: TListItem; var S: string);
begin
  // Tabelle umbenennen
  FDatabaseObj.RenameTable(Item.Caption, S);
end;

procedure TMDI_Database.Umbenennen1Click(Sender: TObject);
var
  i: integer;
  tableName, tableNameOriginal: string;
resourcestring
  SRenameTable = 'Tabelle umbenennen';
  SNewNameForTableS = 'Neuer Name für Tabelle %s:';
begin
  for i := 0 to lvTables.Items.Count-1 do
  begin
    if lvTables.Items.Item[i].Selected then
    begin
      tableName := lvTables.Items.Item[i].Caption;
      tableNameOriginal := tableName;
      InputQuery(SRenameTable, Format(SNewNameForTableS, [tableNameOriginal]), tableName);

      if tableName <> tableNameOriginal then
      begin
        if (Integer(lvTables.Items.Item[i].Data) and 3) = 2{SP} then
        begin
          FDatabaseObj.RenameStoredProcedure(tableNameOriginal, tableName);
        end
        else
        begin
          FDatabaseObj.RenameTable(tableNameOriginal, tableName);
        end;
        lvTables.Items.Item[i].Caption := tableName;
      end;
    end;
  end;
end;

procedure TMDI_Database.pmTablesPopup(Sender: TObject);

  function TriggerVorhanden: boolean;
  var
    i: integer;
  begin
    result := false;
    for i := 0 to lvTables.Items.Count-1 do
    begin
      if lvTables.Items.Item[i].Selected then
      begin
        if FDatabaseObj.HasTriggers(lvTables.Items.Item[i].Caption) then
        begin
          result := true;
          exit;
        end;
      end;
    end;
  end;

  function ViewsVorhanden: boolean;
  var
    i: integer;
  begin
    result := false;
    for i := 0 to lvTables.Items.Count-1 do
    begin
      if lvTables.Items.Item[i].Selected then
      begin
        if (Integer(lvTables.Items.Item[i].Data) and 3) = 1{View} then
        begin
          result := true;
          exit;
        end;
      end;
    end;
  end;

  function AusschliesslichStoredProceduresVorhanden: boolean;
  var
    i: integer;
  begin
    result := lvTables.Items.Count > 0;
    for i := 0 to lvTables.Items.Count-1 do
    begin
      if lvTables.Items.Item[i].Selected then
      begin
        if (Integer(lvTables.Items.Item[i].Data) and 3) in [0{Table}, 1{View}] then
        begin
          result := false;
          exit;
        end;
      end;
    end;
  end;

var
  anItem: TMenuItem;
  i: Integer;
begin
  if lvTables.SelCount = 0 then Abort;

  NurStrukturzeigen1.Enabled := not AusschliesslichStoredProceduresVorhanden;

  ViewDefinitionanzeigen1.Visible := FDatabaseObj.GetViewDefinition_Implemented;
  ViewDefinitionanzeigen1.Enabled := ViewsVorhanden;

  Triggeranzeigen1.Visible := FDatabaseObj.GetTriggers_Implemented;
  Triggeranzeigen1.Enabled := TriggerVorhanden;

  Leeren1.Enabled := not AusschliesslichStoredProceduresVorhanden;
  AnzahlZeilenzeigen1.Enabled := not AusschliesslichStoredProceduresVorhanden;
  Erste100Zeilenzeigen1.Enabled := not AusschliesslichStoredProceduresVorhanden;

  // Kopieren nach...
  while Kopierennach1.Count > 0 do
  begin
    anItem := Kopierennach1.Items[0];
    try
      Kopierennach1.Delete(0);
    finally
      FreeAndNil(anItem);
    end;
  end;
  for i := 0 to DLG_Main.MDIChildCount-1 do
  begin
    anItem := TMenuItem.Create(Self);
    if DLG_Main.MDIChildren[i].Tag = 0 then
      DLG_Main.MDIChildren[i].Tag := Random(999999999);
    anItem.Tag := DLG_Main.MDIChildren[i].Tag;
    anItem.OnClick := KopierenNachDBClick;
    if DLG_Main.MDIChildren[i].ClassNameIs('TMDI_Database') and  // do not localize
      (TMDI_Database(DLG_Main.MDIChildren[i]).Tag <> Self.Tag) then
    begin
      anItem.Caption := TMDI_Database(DLG_Main.MDIChildren[i]).Database;
      Kopierennach1.Add(anItem);
    end;
  end;
  Kopierennach1.Enabled := Kopierennach1.Count > 0;
end;

procedure TMDI_Database.KopierenNachDBClick(Sender: TObject);
var
  aForm: TForm;
  i: integer;
begin
  for i := 0 to DLG_Main.MDIChildCount-1 do
  begin
    if (DLG_Main.MDIChildren[i].Tag <> 0) and (DLG_Main.MDIChildren[i].Tag = TMenuItem(Sender).Tag) then
    begin
      aForm := TForm(DLG_Main.MDIChildren[i]);
      MDI_Form_BringToFront(aForm);
      TMdi_DataBase(aForm).lvTablesDragDrop(aForm, lvTables, 0, 0);
    end;
  end;
end;

procedure TMDI_Database.lvTablesKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if (Key = 13) then
    lvTablesDblClick(Sender)
  else if (ssCtrl in Shift) and (Key = ord('A')) then
  begin
    lvTables.SelectAll;
    Key := 0;
  end
  else if (Key = VK_DELETE) then
    Loeschen1Click(Loeschen1)
  else if (Key = VK_F2) then
    Umbenennen1Click(uUmbenennen1);
end;

procedure TMDI_Database.lvTablesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  UpdateCounterPanel;
end;

procedure TMDI_Database.NurStrukturzeigen1Click(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to lvTables.Items.Count-1 do
  begin
    if lvTables.Items.Item[i].Selected then
    begin
      if (Integer(lvTables.Items.Item[i].Data) and 3) in [0{Table}, 1{View}] then
      begin
        OpenTable(lvTables.Items.Item[i].Caption, true);
      end;
    end;
  end;
end;

end.
