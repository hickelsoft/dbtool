unit C_Database;

{$INCLUDE 'Globals.inc'}

interface

uses
  Windows, Classes, Forms, DB, {$IFNDEF WIN64}DBTables, bdeconst, {$ENDIF} IBDatabase, IBTable, IBQuery, ADODB, Registry,
  OleCtrls, Variants, ComObj, dialogs, ib, adoconst;

type
  TDatabaseType = (
{$IFNDEF WIN64}
    dtLocal,
{$ENDIF}
    dtInterbase, dtAccess, dtSqlServer, dtMySql);

type
  TProductDbType = (ptNotChecked, ptCORAplus, ptHsInfo2, ptCmDb2, ptOther);

type
  TDbToolDatabase = class(TObject)
  private
    FDatabaseType: TDatabaseType;
{$IFNDEF WIN64}
    DB_BDE: TDatabase;
{$ENDIF}
    DB_IB: TIBDatabase;
    DB_IB_Trans: TIBTransaction;
    DB_ADO: TADOConnection;
    FDatabaseName: string;
    FConnWasOk: boolean;
    function GetSupportsCommit: boolean;
    function GetSqlFieldType(FieldType: TFieldType;
      Precision, FieldSize: integer): string;
    // function UmlauteRaus(Sql: String): string;
    procedure BeforeDelete(DataSet: TDataSet);
  protected
    function SQL_CreateTable_Head(sMyTable: String): String;
  public
    constructor Create(DatabaseName: string);
    destructor Destroy; override;

    class procedure CreateDataBase(dt: TDatabaseType;
      const sName, sServer: string);

    function GetTable(Tablename: string): TDataSet;
    function Query(Sql: string): TDataSet;

    function GetViewDefinition_Implemented: boolean;
    function GetViewDefinition(viewName: string): string;
    // requires GetViewDefinition_Implemented=true

    function ViewDetectionImplemented: boolean;
    function IsView(viewName: string): boolean;
    // requires ViewDetectionImplemented=true
    procedure GetAllViews(sl: TStringList);
    // requires ViewDetectionImplemented=true

    function GetAllStoredProcedures_Implemented: boolean;
    procedure GetAllStoredProcedures(sl: TStringList);
    // requires GetAllStoredProcedures_Implemented=true
    function IsStoredProcedure(procedureName: string): boolean;
    // requires GetAllStoredProcedures_Implemented=true

    function GetStoredProcedureDefinition_Implemented: boolean;
    function GetStoredProcedureDefinition(procedureName: string): string;
    // requires GetStoredProcedureDefinition_Implemented=true

    procedure GetAllTablesWithTriggers(sl: TStringList);
    function GetTriggers_Implemented: boolean;
    function HasTriggers(Tablename: string): boolean;
    // requires GetTriggers_Implemented=true
    procedure GetTriggers(Tablename: string; sl: TStringList);
    // requires GetTriggers_Implemented=true

    procedure ExecSql(Sql: String);
    procedure CommitRetaining;
    procedure RefreshTable(aTable: TDataSet);
    procedure ImportFromDatabase(dbSource: TDbToolDatabase; sTable: String);
    procedure RenameTable(oldName, newName: String);
    procedure RenameStoredProcedure(oldName, newName: string);
    procedure DropTable(Tablename: String);
    procedure DropStoredProcedure(storedProcedure: String);
    function GetFieldDefs(aTable: TDataSet): TFieldDefs;
    function GetIndexDefs(aTableName: string): TIndexDefs; overload;
    function GetIndexDefs(aTable: TDataSet): TIndexDefs; overload;
    procedure GetTableNames(Dest: TStringList); // Table names AND Views
    procedure GetPrimaryKeys(slPrimaryKeys: TStringList; Tablename: String);
    procedure GetForeignKeys(slForeignKeys: TStringList; Tablename: String);
    procedure SetTableFilter(aTable: TDataSet; sFilter: String);
    function GetTableFilter(aTable: TDataSet): String;
    procedure SetTableIndex(aTable: TDataSet; sIndex: String);
    property SupportsCommit: boolean read GetSupportsCommit;
    property DatabaseType: TDatabaseType read FDatabaseType;
    property DatabaseName: string read FDatabaseName;
    function SQL_Escape_DatabaseName(sDatabaseName: string): string;
    function SQL_Escape_FieldName(sFieldName: String): String;
    function SQL_Escape_TableName(sTableName: String): String;
    function SQL_Escape_String(sString: String): String;
    function Clone: TDbToolDatabase;
    function CheckDatabaseSecurityPassword: boolean;
    function IstHickelSoftProduktDb: boolean;
  end;

resourcestring
  SExecuteStoredProcedureWith_ = 'Ausführen der Stored Procedure mittels';
  STriggerActived = 'Trigger ist aktiviert';
  STriggerDeactived = 'Trigger ist DEAKTIVIERT!';

implementation

uses
  ComCtrls, Globals, IbDatabaseName, SysUtils, Controls, ProgrDlg,
  hl_SqlServerProvider,
  hl_Datenbank, StrUtils, HS_Auth, System.Hash;

resourcestring
  SInternalError = 'Interner Fehler';

constructor TDbToolDatabase.Create(DatabaseName: string);

  procedure TryAccessDb(ProvName: string);
  begin
    Screen.Cursor := crHourGlass;
    try
      FDatabaseType := dtAccess;
      DB_ADO := TADOConnection.Create(nil);
      DB_ADO.ConnectionString := 'Provider=' + ProvName +
        ';User ID=Admin;Data Source=' + DatabaseName + ';'; // do not localize
      DB_ADO.LoginPrompt := false;
      DB_ADO.Connected := true;
    finally
      Screen.Cursor := crDefault;
    end;
  end;

var
  sExt: string;
  accDbSuccessful: boolean;
  accDbLastError: string;

  aDlg: TDLG_IbDatabaseName;
  bFehler: boolean;
  aReg: TRegIniFile;
  reg: TRegistry;
  ProvName: string;
  i: integer;
  connStrPrefix: string;
  GivenProvider: string;
  sTmp: string;
resourcestring
  SAccessProviderLoadError =
    'Access DB Provider konnte nicht geladen werden: %s';
  SUnknownDatabaseType = 'Unbekannter Datenbanktyp: %s';
  SDatabaseCouldNotBeOpened = 'Die Datenbank konnte nicht geöffnet werden.';
  SNoAceOleDbProviderRegistered = 'Kein ACE OLEDB Provider registriert!';
begin
  inherited Create;

  sExt := AnsiUpperCase(ExtractFileExt(DatabaseName));

  FDatabaseName := DatabaseName;

{$IFNDEF WIN64}
  DB_BDE := nil;
{$ENDIF}
  DB_IB := nil;
  DB_IB_Trans := nil;
  DB_ADO := nil;

  if Copy(DatabaseName, 1, 7) = '_MYSQL:' then // do not localize
  begin
    Screen.Cursor := crHourGlass;
    try
      FDatabaseType := dtMySql;
      DB_ADO := TADOConnection.Create(nil);
      DB_ADO.ConnectionTimeout := 6;
      DB_ADO.ConnectionString :=
        'Provider=MSDASQL.1;Persist Security Info=False;Extended Properties="driver={mysql};'
        + Copy(DatabaseName, 8) + '"'; // do not localize
      DB_ADO.LoginPrompt := false;
      DB_ADO.Connected := true;
    finally
      Screen.Cursor := crDefault;
    end;
  end
  else if Copy(DatabaseName, 1, 8) = '_SQLSRV:' then // do not localize
  begin
    Screen.Cursor := crHourGlass;
    try
      FDatabaseType := dtSqlServer;
      DB_ADO := TADOConnection.Create(nil);
      DB_ADO.LoginPrompt := false;
      DB_ADO.ConnectionTimeout := 6;

      GivenProvider := ConnStrReadAttr('Provider',
        Copy(DatabaseName, Length('_SQLSRV:') + 1)); // do not localize
      if GivenProvider = '' then
        GivenProvider := SqlServerProvider;

      connStrPrefix := 'Provider=' + GivenProvider + ';'; // do not localize

      if (ConnStrReadAttr('Integrated Security', Copy(DatabaseName,
        Length('_SQLSRV:') + 1)) = '') and // do not localize
        (ConnStrReadAttr('User ID', Copy(DatabaseName, Length('_SQLSRV:') + 1))
        = '') then // do not localize
        connStrPrefix := connStrPrefix + 'Integrated Security=SSPI;';
      // do not localize

      if (GivenProvider = 'MSOLEDBSQL19') and
        (ConnStrReadAttr('Use Encryption for Data', Copy(DatabaseName,
        Length('_SQLSRV:') + 1)) = '') then // do not localize
        connStrPrefix := connStrPrefix + 'Use Encryption for Data=False;';
      // wichtig für MSOLEDBSQL19 (Generation 3, Version 19+) Treiber // do not localize

      if (SqlServerProvider <> 'SQLOLEDB') and
        (ConnStrReadAttr('DataTypeCompatibility', Copy(DatabaseName,
        Length('_SQLSRV:') + 1)) = '') then // do not localize
        connStrPrefix := connStrPrefix + 'DataTypeCompatibility=80;';
      // ansonsten funktionieren "time" Datentypen nicht! (sind im Fields[] und FieldDefs[] nicht da und dbGrid kackt ab)
      // leider wird der Typ dann als "WideString" ausgegeben, aber ist halt so...

      if Modus_CORA_Verzeichnis then
      begin
        try
          // SA Auth probieren
          sTmp := connStrPrefix +
            Copy(DatabaseName, Length('_SQLSRV:') + 1) + ';Application Name=' +
            ExtractFileName(ParamStr(0)); // do not localize
          try
            HS_SA_DB_OLD := false; // User "cora-admin"
            DB_ADO.ConnectionString := StringReplace(sTmp,
              'Integrated Security=SSPI;', 'User ID=' + HS_SA_DB_USER +
              ';Password=' + HS_SA_DB_PASSWORD + ';', []); // do not localize
            DB_ADO.Connected := true;
          except
            on E: EAbort do
            begin
              Abort;
            end;
            on E: Exception do
            begin
              HS_SA_DB_OLD := true; // User "sa"
              DB_ADO.ConnectionString := StringReplace(sTmp,
                'Integrated Security=SSPI;', 'User ID=' + HS_SA_DB_USER +
                ';Password=' + HS_SA_DB_PASSWORD + ';', []); // do not localize
              DB_ADO.Connected := true;
            end;
          end;
        except
          on E: EAbort do
          begin
            Abort;
          end;
          on E: Exception do
          begin
            // NT Auth probieren
            DB_ADO.ConnectionString := connStrPrefix +
              Copy(DatabaseName, Length('_SQLSRV:') + 1) + ';Application Name='
              + ExtractFileName(ParamStr(0)); // do not localize
            DB_ADO.Connected := true;
          end;
        end;
      end
      else
      begin
        DB_ADO.ConnectionString := connStrPrefix +
          Copy(DatabaseName, Length('_SQLSRV:') + 1) + ';Application Name=' +
          ExtractFileName(ParamStr(0)); // do not localize
        DB_ADO.Connected := true;
      end;

    finally
      Screen.Cursor := crDefault;
    end;
  end
{$IFNDEF WIN64}
  else if sExt = '' then // Lokale Datenbank (BDE)
  begin
    Screen.Cursor := crHourGlass;
    try
      FDatabaseType := dtLocal;
      DB_BDE := TDatabase.Create(nil);
      DB_BDE.DatabaseName := DatabaseName;
      DB_BDE.LoginPrompt := false;
      DB_BDE.Connected := true;
    finally
      Screen.Cursor := crDefault;
    end
  end
{$ENDIF}
  else if sExt = '.GDB' then // Interbase-Datenbank // do not localize
  begin
    Screen.Cursor := crHourGlass;
    try
      FDatabaseType := dtInterbase;
      DB_IB := TIBDatabase.Create(nil);
      DB_IB.DatabaseName := DatabaseName;
      DB_IB.LoginPrompt := false;
      DB_IB.SQLDialect := 3;
      DB_IB.Params.Add('user_name=SYSDBA'); // do not localize
      DB_IB.Params.Add('password=masterkey'); // do not localize

      DB_IB_Trans := TIBTransaction.Create(nil);
      DB_IB_Trans.DefaultDatabase := DB_IB;
      DB_IB.DefaultTransaction := DB_IB_Trans;

      try
        DB_IB.Connected := true;
        DB_IB_Trans.Active := true;
      except
        on E: EAbort do
        begin
          Abort;
        end;
        on E: Exception do
        begin
          bFehler := true;

          while bFehler do
          begin
            aDlg := TDLG_IbDatabaseName.Create(nil);
            try
              // Combobox füllen...
              aReg := TRegIniFile.Create(ConfigRegKey);
              try
                aDlg.Edit1.Items.CommaText :=
                  aReg.ReadString('MRU', 'InterBase', ''); // do not localize
              finally
                FreeAndNil(aReg);
              end;

              if aDlg.ShowModal = mrOk then
              begin
                // ggf. Datenbank-MRU speichern...
                if aDlg.Edit1.Items.IndexOf(aDlg.Edit1.Text) = -1 then
                begin
                  aDlg.Edit1.Items.Add(aDlg.Edit1.Text);
                  aReg := TRegIniFile.Create(ConfigRegKey);
                  try
                    aReg.WriteString('MRU', 'InterBase',
                      aDlg.Edit1.Items.CommaText); // do not localize
                  finally
                    FreeAndNil(aReg);
                  end;
                end;

                DatabaseName := aDlg.Edit1.Text;
                DB_IB.DatabaseName := DatabaseName;

                try
                  DB_IB.Connected := true;
                  DB_IB_Trans.Active := true;
                  bFehler := false;
                except
                  on E: EAbort do
                  begin
                    Abort;
                  end;
                  on E: Exception do
                  begin
                    // ignore
                  end;
                end;
              end
              else
              begin
                raise Exception.Create(SDatabaseCouldNotBeOpened);
              end;
            finally
              FreeAndNil(aDlg);
            end;
          end;
        end;
      end;
    finally
      Screen.Cursor := crDefault;
    end;
  end
  else if sExt = '.MDB' then // Access 97 Datenbank // do not localize
  begin
    Screen.Cursor := crHourGlass;
    try
      FDatabaseType := dtAccess;
      DB_ADO := TADOConnection.Create(nil);
      DB_ADO.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0;Data Source='
        + DatabaseName; // do not localize
      DB_ADO.LoginPrompt := false;
      DB_ADO.Connected := true;
    finally
      Screen.Cursor := crDefault;
    end;
  end
  else if sExt = '.ACCDB' then // Moderne Access Datenbank // do not localize
  begin
    // Versuche einen geeigneten Provider zu finden. Bevorzuge den mit der höchsten Version
    accDbSuccessful := false;
    accDbLastError := SNoAceOleDbProviderRegistered;
    reg := TRegistry.Create;
    try
      reg.RootKey := HKEY_CLASSES_ROOT;
      for i := 99 downto 12 do
      begin
        if reg.KeyExists('Microsoft.ACE.OLEDB.' + IntToStr(i) + '.0') then
        // do not localize
        begin
          ProvName := 'Microsoft.ACE.OLEDB.' + IntToStr(i) + '.0';
          // do not localize
          try
            TryAccessDb(ProvName);
            accDbSuccessful := true;
            break;
          except
            on E: EAbort do
            begin
              Abort;
            end;
            on E: Exception do
            begin
              // Es kann sein dass z.B. Provider 16 nicht funktioniert, aber Provider 12 funktioniert
              accDbLastError := E.Message + ' (' + ProvName + ')';
            end;
          end;
        end;
      end;
    finally
      FreeAndNil(reg);
    end;
    if not accDbSuccessful then
    begin
      raise Exception.CreateFmt(SAccessProviderLoadError, [accDbLastError]);
    end;
  end
  // TODO: Repro. Die Meldung kam plötzlich, nachdem man erfolgreich eine CSV exportiert hat... Können wir nicht mehr nachstellen.
  else
    raise Exception.CreateFmt('(TDbToolDatabase.Create) ' +
      SUnknownDatabaseType, [DatabaseName]);

  FConnWasOk := true;
end;

class procedure TDbToolDatabase.CreateDataBase(dt: TDatabaseType;
  const sName, sServer: string);
var
  DB: TDbToolDatabase;
  catalog: Variant;
resourcestring
  SLocalCreateDatabaseNotPossible =
    'Local File Create Database nicht unterstützt';
  SInterbaseNotPossible =
    'InterBase-Datenbanken können aufgrund von Einschränkungen der InterBase-API (noch) nicht erstellt werden.';
  SJetServerNameNotAllowed = 'Servername in JET CreateDatabase nicht erlaubt';
  SInterbaseServerNameNotAllowed =
    'Servername in Interbase CreateDatabase nicht erlaubt';
begin
{$IFNDEF WIN64}
  if dt = dtLocal then
  begin
    raise Exception.Create(SLocalCreateDatabaseNotPossible);
  end;
{$ENDIF}
  if dt = dtInterbase then
  begin
    raise Exception.Create(SInterbaseNotPossible);
    {
      if sServer <> '' then raise Exception.Create(SInterbaseServerNameNotAllowed); // should not happen
      //VORSICHT: Dann läuft's nicht mehr ohne die GDS-Datei, oder?
      long __stdcall isc_dsql_execute_immediate (long*, void**, void**, unsigned short, char*, unsigned short, void*);
      long __stdcall isc_commit_transaction(long*, void**);
      long __stdcall isc_detach_database(long*, void**);

      void* newdb = NULL;
      void* trans = NULL;
      long status[20];
      sCreate = "CREATE DATABASE \'" + sName + "\'";
      isc_dsql_execute_immediate(status, &newdb, &trans, 0, sCreate, 1, NULL);
      isc_commit_transaction(status, &trans);
      isc_detach_database(status, &newdb);
    }
  end;
  if dt = dtAccess then
  begin
    if sServer <> '' then
      raise Exception.Create(SJetServerNameNotAllowed); // should not happen
    catalog := CreateOleObject('ADOX.Catalog'); // do not localize
    catalog.Create('Provider=Microsoft.Jet.OLEDB.4.0;Data Source=' + sName +
      ';Jet OLEDB:Engine Type=5'); // do not localize
    catalog := Unassigned;
  end;
  if dt = dtSqlServer then
  begin
    DB := TDbToolDatabase.Create('_SQLSRV:Initial Catalog=master;Data Source=' +
      sServer + ';'); // do not localize
    DB.ExecSql('CREATE DATABASE ' + sName + ';'); // do not localize
    FreeAndNil(DB);
  end;
  if dt = dtMySql then
  begin
    DB := TDbToolDatabase.Create('_MYSQL:database=mysql;server=' + sServer +
      ';'); // do not localize
    DB.ExecSql('CREATE DATABASE ' + sName + ';'); // do not localize
    FreeAndNil(DB);
  end;
end;

destructor TDbToolDatabase.Destroy;
begin
  // Beim Freigeben der Datenbanken werden die erstellten Tabellen automatisch freigegeben
{$IFNDEF WIN64}
  if Assigned(DB_BDE) then
    FreeAndNil(DB_BDE);
{$ENDIF}
  if Assigned(DB_IB) then
    FreeAndNil(DB_IB);
  if Assigned(DB_IB_Trans) then
    FreeAndNil(DB_IB_Trans);
  if Assigned(DB_ADO) then
  begin
    try
      // Workaround for Problem:  Open DB, Close DB, sp_who2 entry stays and DB cannot be deleted. KeepConnection=False does not help.
      // "select 1" ist dafür da, damit man den Fehler "Erhält keine Ereignismenge" bekommt und das den Debugger stört
      if FConnWasOk then
        Query('use master; select 1').Free; // do not localize
    except
      on E: EAbort do
      begin
        Abort;
      end;
      on E: Exception do
      begin
        // ignore
      end;
    end;
    DB_ADO.Close;
    FreeAndNil(DB_ADO);
  end;

  inherited Destroy;
end;

function TDbToolDatabase.GetTable(Tablename: String): TDataSet;
var
{$IFNDEF WIN64}
  bdeReturn: TTable;
{$ENDIF}
  adoReturn: TADOTable;
  ibReturn: TIBTable;
begin
  case FDatabaseType of
{$IFNDEF WIN64}
    dtLocal:
      begin
        Screen.Cursor := crHourGlass;
        try
          bdeReturn := TTable.Create(DB_BDE);
          bdeReturn.DatabaseName := DB_BDE.DatabaseName;
          bdeReturn.Tablename := SQL_Escape_TableName(Tablename);
          bdeReturn.Open;
          result := bdeReturn;
          exit;
        finally
          Screen.Cursor := crDefault;
        end;
      end;
{$ENDIF}
    dtInterbase:
      begin
        Screen.Cursor := crHourGlass;
        try
          ibReturn := TIBTable.Create(DB_IB);
          ibReturn.Database := DB_IB;
          ibReturn.Tablename := SQL_Escape_TableName(Tablename);
          ibReturn.Open;
          result := ibReturn;
          exit;
        finally
          Screen.Cursor := crDefault;
        end;
      end;

    dtAccess, dtSqlServer, dtMySql:
      begin
        Screen.Cursor := crHourGlass;
        try
          adoReturn := TADOTable.Create(DB_ADO);
          adoReturn.Connection := DB_ADO;
          adoReturn.Tablename := SQL_Escape_TableName(Tablename);
          adoReturn.Open;
          result := adoReturn;
          exit;
        finally
          Screen.Cursor := crDefault;
        end;
      end;

  else
    raise Exception.Create('(TDbToolDatabase.GetTable) ' + SInternalError);
  end;
end;

procedure TDbToolDatabase.ExecSql(Sql: string);
var
  IBQuery: TIBQuery;
{$IFNDEF WIN64}
  bdeQuery: TQuery;
{$ENDIF}
  adoQuery: TADOQuery;
begin
  if Copy(Sql, Length(Sql), 1) = ';' then
    Sql := Copy(Sql, 1, Length(Sql) - 1);
  Screen.Cursor := crHourGlass;

  try
    case FDatabaseType of

{$IFNDEF WIN64}
      dtLocal:
        begin
          bdeQuery := TQuery.Create(DB_BDE);
          try
            bdeQuery.DatabaseName := DB_BDE.DatabaseName;
            bdeQuery.Sql.Clear;
            bdeQuery.Sql.Add(Sql);
            bdeQuery.ExecSql;
          finally
            FreeAndNil(bdeQuery);
          end;
        end;
{$ENDIF}
      dtInterbase:
        begin
          IBQuery := TIBQuery.Create(DB_IB);
          try
            IBQuery.Database := DB_IB;
            IBQuery.Transaction := DB_IB_Trans;
            IBQuery.Sql.Clear;
            IBQuery.Sql.Add(Sql);
            IBQuery.ExecSql;
          finally
            FreeAndNil(IBQuery);
          end;
        end;

      dtAccess, dtSqlServer, dtMySql:
        begin
          adoQuery := TADOQuery.Create(DB_ADO);
          try
            adoQuery.Connection := DB_ADO;
            adoQuery.CommandTimeout := 86400; // 24 Stunden
            adoQuery.ParamCheck := false;
            adoQuery.Sql.Clear;
            adoQuery.Sql.Add(Sql);
            adoQuery.ExecSql;
          finally
            FreeAndNil(adoQuery);
          end;
        end;

    else
      raise Exception.Create('(TDbToolDatabase.ExecSql) ' + SInternalError);
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TDbToolDatabase.GetAllViews(sl: TStringList);
var
  q: TDataSet;
begin
  if not ViewDetectionImplemented then
    exit;

  q := Query('SELECT v.name ' + // do not localize
    'FROM sys.views v'); // do not localize
  try
    while not q.Eof do
    begin
      sl.Add(q.Fields[0].AsWideString);
      q.Next;
    end;
  finally
    FreeAndNil(q);
  end;
end;

procedure TDbToolDatabase.GetAllStoredProcedures(sl: TStringList);
var
  q: TDataSet;
begin
  if not GetAllStoredProcedures_Implemented then
    exit;

  q := Query('select * ' + // do not localize
    'from dbo.sysobjects ' + // do not localize
    'where xtype = ''P'' and name not like ''dt_%'';'); // do not localize
  try
    while not q.Eof do
    begin
      sl.Add(q.Fields[0].AsWideString);
      q.Next;
    end;
  finally
    FreeAndNil(q);
  end;
end;

function TDbToolDatabase.IsStoredProcedure(procedureName: string): boolean;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    GetAllStoredProcedures(sl);
    result := sl.IndexOf(procedureName) >= 0;
  finally
    FreeAndNil(sl);
  end;
end;

function TDbToolDatabase.GetStoredProcedureDefinition(procedureName
  : string): string;
var
  q: TDataSet;
begin
  result := '';
  if not GetStoredProcedureDefinition_Implemented then
    exit;

  q := Query('SELECT OBJECT_DEFINITION (OBJECT_ID(N''' + procedureName +
    '''))'); // do not localize
  try
    if q.RecordCount = 0 then
      exit;
    if Trim(q.Fields[0].AsWideString) = '' then
      exit;
    if IstHickelSoftProduktDb then
      result := '-- ' + SExecuteStoredProcedureWith_ + ' ''exec ' +
        SQL_Escape_String(procedureName) + '''' + #13#10 +
        Trim(q.Fields[0].AsWideString) // do not localize
    else
      result := '-- ' + SExecuteStoredProcedureWith_ + ' ''EXEC ' +
        SQL_Escape_String(procedureName) + '''' + #13#10 +
        Trim(q.Fields[0].AsWideString); // do not localize
  finally
    FreeAndNil(q);
  end;
end;

function TDbToolDatabase.IsView(viewName: string): boolean;
var
  q: TDataSet;
begin
  result := false;
  if not ViewDetectionImplemented then
    exit;

  q := Query('SELECT top 1 v.name ' + // do not localize
    'FROM sys.views v ' + // do not localize
    'WHERE v.name = ''' + viewName + ''''); // do not localize
  try
    result := q.RecordCount > 0;
  finally
    FreeAndNil(q);
  end;
end;

function TDbToolDatabase.GetViewDefinition_Implemented: boolean;
begin
  result := DatabaseType = dtSqlServer;
  // TODO: Also implement other DBMS in the future
end;

function TDbToolDatabase.GetViewDefinition(viewName: string): string;
var
  q: TDataSet;
  p: integer;
begin
  result := '';
  if not GetViewDefinition_Implemented then
    exit;

  q := Query('SELECT ' + // do not localize
    '    m.definition ' + // do not localize
    'FROM sys.views v ' + // do not localize
    'INNER JOIN sys.sql_modules m ON m.object_id = v.object_id ' +
    // do not localize
    'WHERE v.name = ''' + viewName + ''''); // do not localize
  try
    if q.RecordCount = 0 then
      exit;
    result := q.Fields[0].AsWideString;
    if Trim(result) = '' then
      exit;
    p := Pos(' as', LowerCase(result));
    if p > 0 then
      result := Copy(result, p + 4, Length(result) - (p + 4) + 1);
    result := Trim(result);
    if result = '' then
      exit;
    result := '-- ALTER VIEW ' + SQL_Escape_TableName(viewName) + ' AS' + #13#10
      + result; // do not localize
  finally
    FreeAndNil(q);
  end;
end;

function TDbToolDatabase.Query(Sql: String): TDataSet;
// Achtung: Ergebnis muss mit Free() freigegeben werden
var
  IBQuery: TIBQuery;
{$IFNDEF WIN64}
  bdeQuery: TQuery;
{$ENDIF}
  adoQuery: TADOQuery;
begin
  if Copy(Sql, Length(Sql), 1) = ';' then
    Sql := Copy(Sql, 1, Length(Sql) - 1);
  Screen.Cursor := crHourGlass;
  result := nil;

  try
    case FDatabaseType of
{$IFNDEF WIN64}
      dtLocal:
        begin
          bdeQuery := TQuery.Create(DB_BDE);
          bdeQuery.DatabaseName := DB_BDE.DatabaseName;
          bdeQuery.BeforeDelete := BeforeDelete;
          bdeQuery.Sql.Clear;
          bdeQuery.Sql.Add(Sql);
          try
            bdeQuery.Active := true;
            result := bdeQuery;
          except
            on E: EAbort do
            begin
              Abort;
            end;
            on E: Exception do
            begin
              if E.Message = bdeconst.SHandleError then
                result := nil
              else
                raise;
            end
            else
              raise;
          end;
        end;
{$ENDIF}
      dtInterbase:
        begin
          IBQuery := TIBQuery.Create(DB_IB);
          IBQuery.Database := DB_IB;
          IBQuery.Transaction := DB_IB_Trans;
          IBQuery.BeforeDelete := BeforeDelete;
          IBQuery.Sql.Clear;
          IBQuery.Sql.Add(Sql);
          try
            IBQuery.Active := true;
            result := IBQuery;
          except
            on E: EAbort do
            begin
              Abort;
            end;
            on E: EIBError do
            begin
              if E.SQLCode = Ord(ibxeEmptySQLStatement) then
                result := nil
              else
                raise;
            end;
            on E: Exception do
            begin
              raise;
            end;
          end;
        end;

      dtAccess, dtSqlServer, dtMySql:
        begin
          adoQuery := TADOQuery.Create(DB_ADO);
          adoQuery.Connection := DB_ADO;
          adoQuery.CommandTimeout := 86400; // 24 Stunden
          adoQuery.ParamCheck := false;
          adoQuery.BeforeDelete := BeforeDelete;
          adoQuery.Sql.Clear;
          adoQuery.Sql.Add(Sql);
          try
            adoQuery.Active := true;
            result := adoQuery;
          except
            on E: EAbort do
            begin
              Abort;
            end;
            on E: Exception do
            begin
              if E.Message = adoconst.SNoResultSet then
                result := nil
              else
                raise;
            end;
          end;
        end;

    else
      raise Exception.Create('(TDbToolDatabase.ExecSql) ' + SInternalError);
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TDbToolDatabase.GetPrimaryKeys(slPrimaryKeys: TStringList;
  Tablename: String);
var
  tmpTbl: TDataSet;
begin
  if FDatabaseType = dtSqlServer then
  begin
    tmpTbl := TADOQuery.Create(nil);
    try
      TADOQuery(tmpTbl).Connection := DB_ADO;
      TADOQuery(tmpTbl).Sql.Text :=
        'SELECT COLUMN_NAME FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE WHERE OBJECTPROPERTY(OBJECT_ID(CONSTRAINT_SCHEMA+''.''+CONSTRAINT_NAME), ''IsPrimaryKey'') = 1 AND TABLE_NAME = '''
        + Tablename + ''''; // do not localize
      tmpTbl.Active := true;
      while not tmpTbl.Eof do
      begin
        slPrimaryKeys.Add(tmpTbl.FieldByName('COLUMN_NAME').AsWideString);
        // do not localize
        tmpTbl.Next;
      end;
    finally
      FreeAndNil(tmpTbl);
    end;
  end
  else if FDatabaseType = dtAccess then
  // Geht auch mit dtSqlServer, aber dort ist OpenSchema so lahm
  begin
    tmpTbl := TADODataSet.Create(nil);
    try
      // TODO: Warum ist das so langsam???
      DB_ADO.OpenSchema(siPrimaryKeys, EmptyParam, EmptyParam,
        TADODataSet(tmpTbl));
      while not tmpTbl.Eof do
      begin
        if SQL_Escape_TableName(tmpTbl.FieldByName('TABLE_NAME').AsWideString) = Tablename
        then // do not localize
        begin
          slPrimaryKeys.Add(tmpTbl.FieldByName('COLUMN_NAME').AsWideString);
          // do not localize
        end;
        tmpTbl.Next;
      end;
    finally
      FreeAndNil(tmpTbl);
    end;
  end
  else
  begin
    // TODO: implement
  end;
end;

procedure TDbToolDatabase.GetForeignKeys(slForeignKeys: TStringList;
  Tablename: string);
var
  tmpTbl: TDataSet;
begin
  if FDatabaseType = dtSqlServer then
  begin
    tmpTbl := TADOQuery.Create(nil);
    try
      TADOQuery(tmpTbl).Connection := DB_ADO;
      TADOQuery(tmpTbl).Sql.Text :=
        'SELECT COLUMN_NAME FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE WHERE OBJECTPROPERTY(OBJECT_ID(CONSTRAINT_SCHEMA+''.''+CONSTRAINT_NAME), ''IsForeignKey'') = 1 AND TABLE_NAME = '''
        + Tablename + ''''; // do not localize
      tmpTbl.Active := true;
      while not tmpTbl.Eof do
      begin
        slForeignKeys.Add(tmpTbl.FieldByName('COLUMN_NAME').AsWideString);
        // do not localize
        tmpTbl.Next;
      end;
    finally
      FreeAndNil(tmpTbl);
    end;
  end
  else if FDatabaseType = dtAccess then
  // Geht auch mit dtSqlServer, aber dort ist OpenSchema so lahm
  begin
    tmpTbl := TADODataSet.Create(nil);
    try
      // TODO: Warum ist das so langsam???
      DB_ADO.OpenSchema(siForeignKeys, EmptyParam, EmptyParam,
        TADODataSet(tmpTbl));
      while not tmpTbl.Eof do
      begin
        if SQL_Escape_TableName(tmpTbl.FieldByName('FK_TABLE_NAME').AsWideString) = Tablename
        then // do not localize
        begin
          slForeignKeys.Add(tmpTbl.FieldByName('FK_COLUMN_NAME').AsWideString);
          // do not localize
        end;
        tmpTbl.Next;
      end;
    finally
      FreeAndNil(tmpTbl);
    end;
  end
  else
  begin
    // TODO: implement
  end;
end;

type
  TADOConnectionEx = class helper for TADOConnection
  public
    procedure GetTableNames_WithSchemaName(List: TStrings;
      SystemTables: boolean = false);
  end;

procedure TADOConnectionEx.GetTableNames_WithSchemaName(List: TStrings;
  SystemTables: boolean);
var
  TypeField, NameField, SchemaField: TField;
  TableType: string;
  DataSet: TADODataSet;
begin
  CheckActive;
  DataSet := TADODataSet.Create(nil);
  try
    OpenSchema(siTables, EmptyParam, EmptyParam, DataSet);
    TypeField := DataSet.FieldByName('TABLE_TYPE'); { do not localize }
    NameField := DataSet.FieldByName('TABLE_NAME'); { do not localize }
    SchemaField := DataSet.FieldByName('TABLE_SCHEMA'); { do not localize }
    List.BeginUpdate;
    try
      List.Clear;
      while not DataSet.Eof do
      begin
        TableType := TypeField.AsWideString;
        if (TableType = 'TABLE') or (TableType = 'VIEW') or { do not localize }
          (SystemTables and (TableType = 'SYSTEM TABLE'))
        then { do not localize }
        begin
          if Assigned(SchemaField) and (SchemaField.AsWideString <> '') and
            (SchemaField.AsWideString <> 'dbo') then { do not localize }
            List.Add(SchemaField.AsWideString + '.' + NameField.AsWideString)
          else
            List.Add(NameField.AsWideString);
        end;
        DataSet.Next;
      end;
    finally
      List.EndUpdate;
    end;
  finally
    DataSet.Free;
  end;
end;

procedure TDbToolDatabase.GetTableNames(Dest: TStringList);
{$IFNDEF WIN64}
var
  FindRec: TSearchRec;
{$ENDIF}
begin

  if not Assigned(Dest) then
    raise Exception.Create
      ('(TDbToolDatabase.GetTableNames) NULL-Zeiger als Parameter');

  case FDatabaseType of
{$IFNDEF WIN64}
    dtLocal:
      begin
        Dest.Clear;

        if findfirst(IncludeTrailingPathDelimiter(DB_BDE.DatabaseName) + '*.db',
          faAnyFile, FindRec) = 0 then // do not localize
        begin
          repeat
            Dest.Add(FindRec.Name);
          until findnext(FindRec) <> 0;
        end;
        findclose(FindRec);

        if findfirst(IncludeTrailingPathDelimiter(DB_BDE.DatabaseName) +
          '*.dbf', faAnyFile, FindRec) = 0 then // do not localize
        begin
          repeat
            Dest.Add(FindRec.Name);
          until findnext(FindRec) <> 0;
        end;
        findclose(FindRec);
      end;
{$ENDIF}
    dtInterbase:
      begin
        DB_IB.GetTableNames(Dest);
      end;

    dtAccess, dtSqlServer, dtMySql:
      begin
        DB_ADO.GetTableNames_WithSchemaName(Dest);
      end;

  else
    raise Exception.Create('(TDbToolDatabase.GetTableNames) ' + SInternalError);
  end;
  Dest.Sort;
end;

function TDbToolDatabase.GetFieldDefs(aTable: TDataSet): TFieldDefs;
begin
  result := aTable.FieldDefs;
end;

function TDbToolDatabase.GetIndexDefs(aTableName: string): TIndexDefs;
var
{$IFNDEF WIN64}
  bdeReturn: TTable;
{$ENDIF}
  adoReturn: TADOTable;
  ibReturn: TIBTable;
begin
  case FDatabaseType of
{$IFNDEF WIN64}
    dtLocal:
      begin
        Screen.Cursor := crHourGlass;
        try
          bdeReturn := TTable.Create(DB_BDE);
          bdeReturn.DatabaseName := DB_BDE.DatabaseName;
          bdeReturn.Tablename := SQL_Escape_TableName(aTableName);
          bdeReturn.DisableControls; // Performance?
          bdeReturn.Open;
          result := TTable(bdeReturn).IndexDefs;
          exit;
        finally
          Screen.Cursor := crDefault;
        end;
      end;
{$ENDIF}
    dtInterbase:
      begin
        Screen.Cursor := crHourGlass;
        try
          ibReturn := TIBTable.Create(DB_IB);
          ibReturn.Database := DB_IB;
          ibReturn.Tablename := SQL_Escape_TableName(aTableName);
          ibReturn.DisableControls; // Performance?
          ibReturn.Open;
          result := TIBTable(ibReturn).IndexDefs;
          exit;
        finally
          Screen.Cursor := crDefault;
        end;
      end;

    dtAccess, dtSqlServer, dtMySql:
      begin
        Screen.Cursor := crHourGlass;
        try
          adoReturn := TADOTable.Create(DB_ADO);
          adoReturn.Connection := DB_ADO;
          adoReturn.Tablename := SQL_Escape_TableName(aTableName);
          adoReturn.DisableControls; // Performance?
          adoReturn.Open;
          result := TADOTable(adoReturn).IndexDefs;
          exit;
        finally
          Screen.Cursor := crDefault;
        end;
      end;

  else
    raise Exception.Create('(TDbToolDatabase.GetIndexDefs) ' + SInternalError);
  end;
end;

function TDbToolDatabase.GetIndexDefs(aTable: TDataSet): TIndexDefs;
begin
{$IFNDEF WIN64}
  if aTable.ClassNameIs('TTable') then // do not localize
    result := TTable(aTable).IndexDefs
  else
{$ENDIF}
    if aTable.ClassNameIs('TIBTable') then // do not localize
      result := TIBTable(aTable).IndexDefs
    else if aTable.ClassNameIs('TADOTable') then // do not localize
      result := TADOTable(aTable).IndexDefs
    else
      raise Exception.Create('(TDbToolDatabase.GetIndexDefs) ' +
        SInternalError);
end;

procedure TDbToolDatabase.RefreshTable(aTable: TDataSet);
begin
{$IFNDEF WIN64}
  if aTable.ClassNameIs('TTable') then // do not localize
  begin
    TTable(aTable).Refresh;
  end
  else if aTable.ClassNameIs('TQuery') then // do not localize
  begin
    TQuery(aTable).Active := false;
    TQuery(aTable).Active := true;
  end
  else
{$ENDIF}
    if aTable.ClassNameIs('TIBTable') then // do not localize
    begin
      TIBTable(aTable).Refresh;
    end
    else if aTable.ClassNameIs('TIBQuery') then // do not localize
    begin
      TIBQuery(aTable).Active := false;
      TIBQuery(aTable).Active := true;
    end
    else if aTable.ClassNameIs('TADOTable') then // do not localize
    begin
      TADOTable(aTable).Requery;
    end
    else if aTable.ClassNameIs('TAdoQuery') then // do not localize
    begin
      TADOQuery(aTable).Active := false;
      TADOQuery(aTable).Active := true;
    end
    else
      raise Exception.Create('(TDbToolDatabase.RefreshTable) ' +
        SInternalError);
end;

procedure TDbToolDatabase.SetTableFilter(aTable: TDataSet; sFilter: string);
var
  sTmp, sOrder: string;
  p: integer;
begin
  if not Assigned(aTable) then
    exit;
{$IFNDEF WIN64}
  if (aTable.ClassNameIs('TTable')) then // do not localize
  begin
    TTable(aTable).Filter := sFilter;
    TTable(aTable).Filtered := sFilter <> '';
  end
  else if (aTable.ClassNameIs('TQuery')) then // do not localize
  begin
    sTmp := TQuery(aTable).Sql.Text;
    p := Pos(' order by ', LowerCase(sTmp)); // do not localize
    if p > 0 then
    begin
      sOrder := ' order by ' + Copy(sTmp, p + Length(' order by '));
      // do not localize
      sTmp := Copy(sTmp, 1, p - 1);
    end
    else
    begin
      sOrder := '';
    end;
    p := Pos(' where ', LowerCase(sTmp)); // do not localize
    if p > 0 then
    begin
      sTmp := Copy(sTmp, 1, p - 1);
    end;
    TQuery(aTable).Active := false;
    if Trim(sFilter) <> '' then
      sFilter := ' where ' + sFilter + sOrder; // do not localize
    TQuery(aTable).Sql.Text := sTmp + sFilter;
    TQuery(aTable).Active := true;
  end
  else
{$ENDIF}
    if (aTable.ClassNameIs('TIBTable')) then // do not localize
    begin
      TIBTable(aTable).Filter := sFilter;
      TIBTable(aTable).Filtered := sFilter <> '';
    end
    else if (aTable.ClassNameIs('TIBQuery')) then // do not localize
    begin
      sTmp := TIBQuery(aTable).Sql.Text;
      p := Pos(' order by ', LowerCase(sTmp)); // do not localize
      if p > 0 then
      begin
        sOrder := ' order by ' + Copy(sTmp, p + Length(' order by '));
        // do not localize
        sTmp := Copy(sTmp, 1, p - 1);
      end
      else
      begin
        sOrder := '';
      end;
      p := Pos(' where ', LowerCase(sTmp)); // do not localize
      if p > 0 then
      begin
        sTmp := Copy(sTmp, 1, p - 1);
      end;
      TIBQuery(aTable).Active := false;
      if Trim(sFilter) <> '' then
        sFilter := ' where ' + sFilter + sOrder; // do not localize
      TIBQuery(aTable).Sql.Text := sTmp + sFilter;
      TIBQuery(aTable).Active := true;
    end
    else if (aTable.ClassNameIs('TADOTable')) then // do not localize
    begin
      TADOTable(aTable).Filter := sFilter;
      TADOTable(aTable).Filtered := sFilter <> '';
    end
    else if aTable.ClassNameIs('TADOQuery') then // do not localize
    begin
      sTmp := TADOQuery(aTable).Sql.Text;
      p := Pos(' order by ', LowerCase(sTmp)); // do not localize
      if p > 0 then
      begin
        sOrder := ' order by ' + Copy(sTmp, p + Length(' order by '));
        // do not localize
        sTmp := Copy(sTmp, 1, p - 1);
      end
      else
      begin
        sOrder := '';
      end;
      p := Pos(' where ', LowerCase(sTmp)); // do not localize
      if p > 0 then
      begin
        sTmp := Copy(sTmp, 1, p - 1);
      end;
      TADOQuery(aTable).Active := false;
      if Trim(sFilter) <> '' then
        sFilter := ' where ' + sFilter; // do not localize
      TADOQuery(aTable).Sql.Text := sTmp + sFilter + sOrder;
      TADOQuery(aTable).Active := true;
    end
    else
      raise Exception.Create('(TDbToolDatabase.SetTableFilter) ' +
        SInternalError);
end;

function TDbToolDatabase.GetTableFilter(aTable: TDataSet): string;
var
  sTmp: string;
  p: integer;
begin
{$IFNDEF WIN64}
  if aTable.ClassNameIs('TTable') then // do not localize
  begin
    result := TTable(aTable).Filter;
  end
  else if aTable.ClassNameIs('TQuery') then // do not localize
  begin
    sTmp := TQuery(aTable).Sql.Text;
    p := Pos(' order by ', LowerCase(sTmp)); // do not localize
    if p > 0 then
    begin
      sTmp := Copy(sTmp, 1, p - 1);
    end;
    p := Pos(' where ', LowerCase(sTmp)); // do not localize
    if p = 0 then
    begin
      result := '';
    end
    else
    begin
      p := p + Length(' where '); // do not localize
      result := Copy(sTmp, p);
    end;
  end
  else
{$ENDIF}
    if aTable.ClassNameIs('TIBTable') then // do not localize
    begin
      result := TIBTable(aTable).Filter;
    end
    else if aTable.ClassNameIs('TIBQuery') then // do not localize
    begin
      sTmp := TIBQuery(aTable).Sql.Text;
      p := Pos(' order by ', LowerCase(sTmp)); // do not localize
      if p > 0 then
      begin
        sTmp := Copy(sTmp, 1, p - 1);
      end;
      p := Pos(' where ', LowerCase(sTmp)); // do not localize
      if p = 0 then
      begin
        result := '';
      end
      else
      begin
        p := p + Length(' where '); // do not localize
        result := Copy(sTmp, p);
      end;
    end
    else if aTable.ClassNameIs('TADOTable') then // do not localize
    begin
      result := TADOTable(aTable).Filter;
    end
    else if aTable.ClassNameIs('TADOQuery') then // do not localize
    begin
      sTmp := TADOQuery(aTable).Sql.Text;
      p := Pos(' order by ', LowerCase(sTmp)); // do not localize
      if p > 0 then
      begin
        sTmp := Copy(sTmp, 1, p - 1);
      end;
      p := Pos(' where ', LowerCase(sTmp)); // do not localize
      if p = 0 then
      begin
        result := '';
      end
      else
      begin
        p := p + Length(' where '); // do not localize
        result := Copy(sTmp, p);
      end;
    end
    else
      raise Exception.Create('(TDbToolDatabase.GetTableFilter) ' +
        SInternalError);
end;

procedure TDbToolDatabase.SetTableIndex(aTable: TDataSet; sIndex: string);
begin
{$IFNDEF WIN64}
  if aTable.ClassNameIs('TTable') then
    TTable(aTable).IndexFieldNames := sIndex
  else
{$ENDIF}
    if aTable.ClassNameIs('TIBTable') then // do not localize
      TIBTable(aTable).IndexFieldNames := sIndex
    else if aTable.ClassNameIs('TADOTable') then // do not localize
      TADOTable(aTable).IndexFieldNames := sIndex
    else
      raise Exception.Create('(TDbToolDatabase.SetTableIndex) ' +
        SInternalError);
end;

procedure TDbToolDatabase.BeforeDelete(DataSet: TDataSet);
var
  Sql: string;
resourcestring
  SViewDeleteWarningCora =
    'STOPP! Diese View könnte eine JOIN-Abfrage beinhalten. Ein Löschen ist deswegen zu gefährlich und wird daher verboten.';
  SViewDeleteWarningGeneral =
    'STOPP! Ein Löschen aus einer JOIN-Abfrage löscht die Vorkommen auf ALLEN verbundenen Tabellen. Dieser Vorgang ist zu gefährlich und wird daher verboten.';
begin
  Sql := TADOQuery(DataSet).Sql.Text;
  Sql := StringReplace(Sql, #13, ' ', [rfReplaceAll]);
  Sql := StringReplace(Sql, #10, ' ', [rfReplaceAll]);
  Sql := StringReplace(Sql, #9, ' ', [rfReplaceAll]);
  if IstHickelSoftProduktDb then
  begin
    if ContainsStr(Sql, ' vw_') or // do not localize
      ContainsStr(Sql, ' X_vw_') then // do not localize
    begin
      raise Exception.Create(SViewDeleteWarningCora);
    end;
  end;
  Sql := StringReplace(Sql, ' ', '', [rfReplaceAll]);
  if (ContainsText(Sql, 'INNERJOIN') or // do not localize
    ContainsText(Sql, 'LEFTJOIN') or // do not localize
    ContainsText(Sql, 'LEFTOUTERJOIN') or // do not localize
    ContainsText(Sql, 'RIGHTJOIN') or // do not localize
    ContainsText(Sql, 'RIGHTOUTERJOIN') or // do not localize
    ContainsText(Sql, 'FULLJOIN') or // do not localize
    ContainsText(Sql, 'FULLOUTERJOIN') or // do not localize
    ContainsText(Sql, 'CROSSJOIN')) then // do not localize
  begin
    raise Exception.Create(SViewDeleteWarningGeneral);
  end;
end;

function TDbToolDatabase.Clone: TDbToolDatabase;
begin
  result := TDbToolDatabase.Create(DatabaseName);
end;

procedure TDbToolDatabase.CommitRetaining;
begin
  case FDatabaseType of
{$IFNDEF WIN64}
    dtLocal:
      begin
        // Nichts tun
      end;
{$ENDIF}
    dtInterbase:
      if DB_IB_Trans.InTransaction then
        DB_IB_Trans.CommitRetaining;

    dtAccess, dtSqlServer, dtMySql:
      if DB_ADO.InTransaction then
        DB_ADO.CommitTrans;

  else
    raise Exception.Create('(TDbToolDatabase.CommitRetaining) ' +
      SInternalError);
  end;
end;

function TDbToolDatabase.GetSupportsCommit: boolean;
begin
{$IFNDEF WIN64}
  result := FDatabaseType <> dtLocal;
{$ELSE}
  result := true;
{$ENDIF}
end;

function TDbToolDatabase.GetSqlFieldType(FieldType: TFieldType;
  Precision, FieldSize: integer): string;
var
  sFieldType: string;
resourcestring
  SFieldTypeNotSupportedCopy =
    'Feldtyp "%s" ist nicht implementiert und wird beim Kopieren von Tabellen nicht unterstützt!';
begin
  // TODO: Diese Liste ist ggf. unvollständig und für Nicht-SQL-Server ggf. auch falsch!!

  case FieldType of

    ftAutoInc, ftInteger:
      sFieldType := 'int'; // do not localize

    ftLargeint:
      if FDatabaseType in [dtMySql, dtSqlServer] then
        sFieldType := 'bigint' // do not localize
      else
        sFieldType := 'largeint'; // do not localize

    ftBCD, ftFMTBcd:
      if FDatabaseType = dtSqlServer then
        sFieldType := 'decimal(' + IntToStr(Precision) + ',' +
          IntToStr(FieldSize) + ')' // do not localize
      else
        sFieldType := 'numeric'; // do not localize

    ftBoolean:
      begin
        if FDatabaseType = dtInterbase then
          sFieldType := 'smallint' // do not localize
        else if FDatabaseType = dtMySql then
          sFieldType := 'tinyint(1)' // do not localize
        else
          sFieldType := 'bit'; // do not localize
      end;

    ftCurrency, ftFloat:
      sFieldType := 'float'; // do not localize

    ftDate, ftTime, ftDateTime:
      begin
        if FDatabaseType = dtInterbase then
          sFieldType := 'timestamp' // do not localize
        else
          sFieldType := 'datetime'; // do not localize
      end;

    ftFixedChar:
      sFieldType := 'char(' + IntToStr(FieldSize) + ')'; // do not localize

    ftMemo:
      begin
        if FDatabaseType = dtInterbase then
          sFieldType := 'blob sub_type text' // do not localize
        else
          sFieldType := 'text'; // do not localize
      end;

    ftWideMemo:
      begin
        if FDatabaseType = dtInterbase then
          sFieldType := 'blob sub_type text' // do not localize
        else if FDatabaseType = dtSqlServer then
          sFieldType := 'ntext' // do not localize
        else
          sFieldType := 'text'; // do not localize
      end;

    ftSmallint, ftWord:
      sFieldType := 'smallint'; // do not localize

    ftString:
      begin
        if (FDatabaseType <> dtInterbase) and (FieldSize > 255) then
          sFieldType := 'text' // do not localize
        else
          sFieldType := 'varchar(' + IntToStr(FieldSize) + ')';
        // do not localize
      end;

    ftWideString:
      begin
        if (FDatabaseType <> dtInterbase) and (FieldSize > 255) then
          sFieldType := 'text' // do not localize
        else if FDatabaseType = dtSqlServer then
          sFieldType := 'nvarchar(' + IntToStr(FieldSize) + ')'
          // do not localize
        else
          sFieldType := 'varchar(' + IntToStr(FieldSize) + ')';
        // do not localize
      end;

    ftGuid:
      begin
        if FDatabaseType = dtSqlServer then
          sFieldType := 'uniqueidentifier' // do not localize
        else
          sFieldType := 'varchar(38)'; // do not localize
      end;

    ftBlob, ftVarBytes:
      begin
        if FDatabaseType = dtSqlServer then
          sFieldType := 'varbinary(max)' // do not localize
        else
          sFieldType := 'blob'; // do not localize
      end;
  else
    raise Exception.CreateFmt(SFieldTypeNotSupportedCopy,
      [FieldTypeNames[FieldType]]);
  end;

  result := sFieldType;
end;

function TDbToolDatabase.GetAllStoredProcedures_Implemented: boolean;
begin
  result := DatabaseType = dtSqlServer;
  // TODO: Also implement other DBMS in the future
end;

function TDbToolDatabase.GetStoredProcedureDefinition_Implemented: boolean;
begin
  result := DatabaseType = dtSqlServer;
  // TODO: Also implement other DBMS in the future
end;

(*
  function TDbToolDatabase.UmlauteRaus(Sql: String): String;
  begin
  Sql := StringReplace(Sql, 'ä', 'ae', [rfReplaceAll]);
  Sql := StringReplace(Sql, 'Ä', 'AE', [rfReplaceAll]);
  Sql := StringReplace(Sql, 'ö', 'oe', [rfReplaceAll]);
  Sql := StringReplace(Sql, 'Ö', 'OE', [rfReplaceAll]);
  Sql := StringReplace(Sql, 'ü', 'ue', [rfReplaceAll]);
  Sql := StringReplace(Sql, 'Ü', 'UE', [rfReplaceAll]);
  Sql := StringReplace(Sql, 'ß', 'ss', [rfReplaceAll]);
  result := Sql;
  end;
*)

procedure TDbToolDatabase.RenameTable(oldName, newName: string);
resourcestring
  SNoRenamingInMsAccess =
    'Tabellen können in Microsoft Access per SQL-Befehl nicht umbenannt werden.';
begin
  // TODO: Für alle unterstützten DBMS implementieren
  // Beispiele gibt es in C:\Program Files (x86)\Common Files\CodeGear Shared\Data
  case FDatabaseType of
    dtAccess:
      raise Exception.Create(SNoRenamingInMsAccess);

    dtSqlServer:
      begin
        ExecSql('sp_rename ' + SQL_Escape_TableName(oldName) + ', ' +
          SQL_Escape_TableName(newName) + ';'); // do not localize
      end;

{$IFNDEF WIN64}
    dtLocal, // Nicht getestet
{$ENDIF}
    dtInterbase, // Nicht getestet
    dtMySql:
      ExecSql('RENAME TABLE ' + SQL_Escape_TableName(oldName) + ' TO ' +
        SQL_Escape_TableName(newName) + ';'); // do not localize
  else
    raise Exception.Create('(TDbToolDatabase.RenameTable) ' + SInternalError);
  end;
end;

procedure TDbToolDatabase.RenameStoredProcedure(oldName, newName: string);
resourcestring
  SNotImplementedForThisDBMS = 'Nicht für dieses DBMS implementiert';
begin
  case FDatabaseType of
    dtSqlServer:
      begin
        ExecSql('sp_rename ' + SQL_Escape_String(oldName) + ', ' +
          SQL_Escape_String(newName) + ';'); // do not localize
      end;
  else
    raise Exception.Create(SNotImplementedForThisDBMS);
  end;
end;

procedure TDbToolDatabase.DropStoredProcedure(storedProcedure: String);
begin
  ExecSql('DROP PROCEDURE ' + SQL_Escape_TableName(storedProcedure) + ';');
  // do not localize
end;

procedure TDbToolDatabase.DropTable(Tablename: String);
var
  errorCount: integer;

  procedure _TryDeleteView;
  begin
    try
      ExecSql('DROP VIEW ' + SQL_Escape_TableName(Tablename) + ';');
      // do not localize
      errorCount := 0;
    except
      on E: EAbort do
      begin
        Abort;
      end;
      on E: EOleException do
      begin
        // ShowMessage(E.Message);
        Inc(errorCount);
      end;
      on E: Exception do
      begin
        raise;
      end;
    end;
  end;

  procedure _TryDeleteTable;
  begin
    try
      ExecSql('DROP TABLE ' + SQL_Escape_TableName(Tablename) + ';');
      // do not localize
      errorCount := 0;
    except
      on E: EAbort do
      begin
        Abort;
      end;
      on E: EOleException do
      begin
        // ShowMessage(E.Message);
        Inc(errorCount);
      end;
      on E: Exception do
      begin
        raise;
      end;
    end;
  end;

resourcestring
  SCloudNotDeleteS = 'Konnte %s nicht löschen';
begin
  // TODO: Für alle unterstützten DBMS implementieren
  // Beispiele gibt es in C:\Program Files (x86)\Common Files\CodeGear Shared\Data
  errorCount := 0;
  if ViewDetectionImplemented then
  begin
    if IsView(Tablename) then
      _TryDeleteView
    else
      _TryDeleteTable;
  end
  else
  begin
    _TryDeleteView;
    if (errorCount > 0) then
      _TryDeleteTable;
  end;
  if (errorCount > 0) then
  begin
    // TODO: It would be good to show the actual error message!!!
    raise Exception.CreateFmt(SCloudNotDeleteS, [Tablename]);
  end;
end;

function TDbToolDatabase.SQL_CreateTable_Head(sMyTable: string): String;
begin
  result := 'CREATE TABLE ' + SQL_Escape_TableName(sMyTable); // do not localize
end;

function TDbToolDatabase.SQL_Escape_DatabaseName(sDatabaseName: string): string;
begin
  case FDatabaseType of
    dtSqlServer:
      result := '[' + sDatabaseName + ']';

{$IFNDEF WIN64}
    dtLocal, // Nicht getestet Unbekannt, ob es Escaping gibt.
{$ENDIF}
    dtInterbase, // Nicht getestet Unbekannt, ob es Escaping gibt.
    dtAccess, // Nicht getestet. Unbekannt, ob es Escaping gibt.
    dtMySql: // Nicht getestet. Unbekannt, ob es Escaping gibt.
      result := sDatabaseName;

  else
    raise Exception.Create('(TDbToolDatabase.SQL_Escape_DatabaseName) ' +
      SInternalError);
  end;
end;

function TDbToolDatabase.SQL_Escape_FieldName(sFieldName: string): string;
begin
  case FDatabaseType of
    dtSqlServer:
      result := '[' + sFieldName + ']';

{$IFNDEF WIN64}
    dtLocal, // Nicht getestet Unbekannt, ob es Escaping gibt.
{$ENDIF}
    dtInterbase, // Nicht getestet Unbekannt, ob es Escaping gibt.
    dtAccess, // Nicht getestet. Unbekannt, ob es Escaping gibt.
    dtMySql: // Nicht getestet. Unbekannt, ob es Escaping gibt.
      result := sFieldName;

  else
    raise Exception.Create('(TDbToolDatabase.SQL_Escape_FieldName) ' +
      SInternalError);
  end;
end;

function TDbToolDatabase.SQL_Escape_TableName(sTableName: String): string;
var
  ary: TArray<string>;
  i: integer;
begin
  case FDatabaseType of
    dtSqlServer:
      begin
        result := '';
        ary := SplitString(sTableName, '.');
        for i := 0 to Length(ary) - 1 do
        begin
          if i <> 0 then
            result := result + '.';
          ary[i] := StringReplace(ary[i], '[', '[[]', [rfReplaceAll]);
          // TODO: Geht nicht... deshalb dürfen Tabellennamen vorerst keine Klammern haben
          result := result + '[' + ary[i] + ']';
        end;
      end;

    dtMySql:
      begin
        result := '`' + sTableName + '`';
      end;

{$IFNDEF WIN64}
    dtLocal, // Nicht getestet Unbekannt, ob es Escaping gibt.
{$ENDIF}
    dtInterbase, // Nicht getestet Unbekannt, ob es Escaping gibt.
    dtAccess: // Nicht getestet. Unbekannt, ob es Escaping gibt.
      result := sTableName;
  else
    raise Exception.Create('(TDbToolDatabase.SQL_Escape_TableName) ' +
      SInternalError);
  end;
end;

function TDbToolDatabase.ViewDetectionImplemented: boolean;
begin
  result := DatabaseType = dtSqlServer;
  // TODO: Also implement other DBMS in the future
end;

function TDbToolDatabase.SQL_Escape_String(sString: String): String;
begin
  result := sString;

  case FDatabaseType of
    dtSqlServer:
      begin
        // Escape SQL-Argument
        (*
          result := StringReplace(result, '\', '\\', [rfReplaceAll]);
          result := StringReplace(result, '_', '\_', [rfReplaceAll]);
          result := StringReplace(result, '%', '\%', [rfReplaceAll]);
          result := StringReplace(result, '[', '\[', [rfReplaceAll]);
          result := StringReplace(result, '''', '\''', [rfReplaceAll]);
        *)

        // DM 29.02.2016 Irgendwie versteh ich das nicht...
        // 'xxx\'xxx' ist erlaubt, aber 'xxx\'xxx\'xxx' nicht
        // aber 'xxx''xxx''xxx' geht.
        result := StringReplace(result, '''', '''''', [rfReplaceAll]);
      end;

    dtMySql,
{$IFNDEF WIN64}
    dtLocal, // Nicht getestet Unbekannt, ob es Escaping gibt, und wie dieses aussieht.
{$ENDIF}
    dtInterbase,
    // Nicht getestet Unbekannt, ob es Escaping gibt, und wie dieses aussieht.
    dtAccess: // Nicht getestet. Unbekannt, ob es Escaping gibt, und wie dieses aussieht.
      begin
        result := StringReplace(result, '''', '\''', [rfReplaceAll]);
        result := StringReplace(result, '\', '\\', [rfReplaceAll]);
      end;
  else
    raise Exception.Create('(TDbToolDatabase.SQL_Escape_String) ' +
      SInternalError);
  end;
end;

function TDbToolDatabase.GetTriggers_Implemented: boolean;
begin
  result := FDatabaseType = dtSqlServer;
end;

function TDbToolDatabase.HasTriggers(Tablename: string): boolean;
var
  slDummy: TStringList;
begin
  slDummy := TStringList.Create;
  try
    GetTriggers(Tablename, slDummy);
    result := slDummy.Count > 0;
  finally
    FreeAndNil(slDummy);
  end;
end;

procedure TDbToolDatabase.GetAllTablesWithTriggers(sl: TStringList);
var
  ds: TDataSet;
begin
  case FDatabaseType of
    dtSqlServer:
      begin
        ds := Query('select distinct obj.name ' + // do not localize
          'from sys.triggers trg ' + // do not localize
          'inner join sys.objects obj on obj.object_id = trg.parent_id');
        // do not localize
        while not ds.Eof do
        begin
          sl.Add(ds.FieldByName('name').AsWideString); // do not localize
          ds.Next;
        end;
      end;

    // TODO: Andere DBMS auch implementieren
  end;
end;

procedure TDbToolDatabase.GetTriggers(Tablename: string; sl: TStringList);

  function CountOccurences(const SubText: string; const Text: string): integer;
  begin
    // https://stackoverflow.com/questions/5265317/delphi-count-number-of-times-a-string-occurs-in-another-string
    if (SubText = '') OR (Text = '') OR (Pos(SubText, Text) = 0) then
      result := 0
    else
      result := (Length(Text) - Length(StringReplace(Text, SubText, '',
        [rfReplaceAll]))) div Length(SubText);
  end;

var
  ds: TDataSet;
  i, j: integer;
begin
  case FDatabaseType of
    dtSqlServer:
      begin
        ds := Query
          ('select trg.name, cmt.text as definition, trg.is_disabled from sys.triggers trg ' +
          // do not localize
          'inner join sys.objects obj on obj.object_id = trg.parent_id ' +
          // do not localize
          'inner join syscomments cmt on cmt.id = trg.object_id ' +
          // do not localize
          'where obj.name = ''' + SQL_Escape_String(Tablename) + ''' ' +
          // do not localize
          'order by colid;'); // do not localize
        while not ds.Eof do
        begin
          if sl.Values[ds.FieldByName('name').AsWideString] = '' then
          // do not localize
          begin
            if ds.FieldByName('is_disabled').AsBoolean then // do not localize
              sl.Values[ds.FieldByName('name').AsWideString] :=
                '-- ' + STriggerDeactived + #13#10 // do not localize
            else
              sl.Values[ds.FieldByName('name').AsWideString] :=
                '-- ' + STriggerActived + #13#10; // do not localize
          end;
          sl.Values[ds.FieldByName('name').AsWideString] := // do not localize
            sl.Values[ds.FieldByName('name').AsWideString] +
            ds.FieldByName('definition').AsWideString; // do not localize
          ds.Next;
        end;

        if IstHickelSoftProduktDb then
        begin
          // Hack für alte CORAplus Trigger, bei denen kein CRLF vorhanden ist
          for i := 0 to sl.Count - 1 do
          begin
            if CountOccurences(#13#10, sl.Values[sl.KeyNames[i]]) <= 1 then
            begin
              sl.Values[sl.KeyNames[i]] :=
                StringReplace(sl.Values[sl.KeyNames[i]], #9,
                StringOfChar(' ', 4), [rfReplaceAll]);
              for j := 100 downto 3 do
              begin
                sl.Values[sl.KeyNames[i]] :=
                  StringReplace(sl.Values[sl.KeyNames[i]], StringOfChar(' ', j),
                  #27 + '[' + IntToStr(j) + ']', [rfReplaceAll]);
                // do not localize
              end;
              for j := 100 downto 3 do
              begin
                sl.Values[sl.KeyNames[i]] :=
                  StringReplace(sl.Values[sl.KeyNames[i]],
                  #27 + '[' + IntToStr(j) + ']', #13#10 + StringOfChar(' ', j),
                  [rfReplaceAll]); // do not localize
              end;
            end;
          end;
        end;
      end;

    // TODO: Andere DBMS auch implementieren
  end;
end;

procedure TDbToolDatabase.ImportFromDatabase(dbSource: TDbToolDatabase;
  sTable: String);
var
  dsSource: TDataSet;
  dsDest: TDataSet;
  bSchonVorhanden: boolean;
  bOK: boolean;
  sMyTable: String;
  slMyTables: TStringList;
  aIndexDefs: TIndexDefs;
  sCreate: string;
  sFieldName: string;
  sFields: string;
  aiFeldIndizes: array of integer;
  iFieldCount: integer;
  i, j: integer;
  sUnique, sTmp: string;
  pd1: TProgressDlg;
  viewDef: string;
  slTrigger: TStringList;
resourcestring
  SCopyTableS = 'Kopiere Tabelle %s...';
  SStoredProcedureCreateError =
    'Stored Procedure %s konnte nicht erzeugt werden: %s';
  SViewCouldNotBeCreated =
    'View %s konnte nicht erzeugt werden. Stattdessen die Inhalte als Tabelle kopieren?';
  STriggerCouldNotBeCreated = 'Trigger %s konnte nicht erzeugt werden: %s';
  SCouldNotCopyTable = 'Tabelle %s wird nicht kopiert.';
begin
  bOK := true;

  // TODO: Soltle man UmlauteRaus verwenden, wenn die Zieldatenbank keine Umlaute kann? z.B. SQL Server nach Paradox o.ä.?
  sMyTable := { UmlauteRaus } (StringReplace(StringReplace(sTable, '.dbf', '',
    [rfReplaceAll, rfIgnoreCase]), '.db', '', [rfReplaceAll, rfIgnoreCase]));
  // do not localize

  if dbSource.GetStoredProcedureDefinition_Implemented then
  begin
    viewDef := dbSource.GetStoredProcedureDefinition(sTable);
    if viewDef <> '' then
    begin
      try
        if IsStoredProcedure(sTable) then
          ExecSql(StringReplace(viewDef, 'CREATE PROCEDURE', 'ALTER PROCEDURE',
            [rfIgnoreCase])) // do not localize
        else
          ExecSql(viewDef);
        exit;
      except
        on E: EAbort do
        begin
          Abort;
        end;
        on E: Exception do
        begin
          Application.MessageBox(PChar(Format(SStoredProcedureCreateError,
            [sTable, E.Message])), PChar(Application.Title),
            MB_ICONEXCLAMATION + MB_OK);
        end;
      end;
    end;
  end;

  if dbSource.ViewDetectionImplemented then
  begin
    viewDef := dbSource.GetViewDefinition(sTable);
    if viewDef <> '' then
    begin
      try
        if IsView(sTable) then
          ExecSql('Alter View ' + SQL_Escape_TableName(sTable) + ' as ' +
            viewDef) // do not localize
        else
          ExecSql('Create View ' + SQL_Escape_TableName(sTable) + ' as ' +
            viewDef); // do not localize
        exit;
      except
        on E: EAbort do
        begin
          Abort;
        end;
        on E: Exception do
        begin
          case Application.MessageBox(PChar(Format(SViewCouldNotBeCreated,
            [sTable]) + #13#10#13#10 + E.Message), PChar(Application.Title),
            MB_ICONEXCLAMATION + MB_YESNOCANCEL) of
            ID_YES:
              begin
              end;

            ID_NO:
              begin
                exit;
              end;

            ID_CANCEL:
              begin
                Abort;
              end;
          end;
        end;
      end;
    end;
  end;

  // "Bitte Warten"-Meldung anzeigen
  pd1 := TProgressDlg.Create(nil);
  try
    pd1.Text := Format(SCopyTableS, [sTable]);
    pd1.Open;

    try
      // Prüfen: Ist die Tabelle in der Zieldatenbank schon vorhanden?
      slMyTables := TStringList.Create;
      try
        GetTableNames(slMyTables);
        slMyTables.CaseSensitive := false;
        bSchonVorhanden := (slMyTables.IndexOf(sMyTable) <> -1);
      finally
        FreeAndNil(slMyTables);
      end;

      if not bSchonVorhanden then
      begin
        // Tabelle ist noch nicht vorhanden: Neu erstellen!
        dsSource := dbSource.GetTable(sTable);
        try
          sCreate := SQL_CreateTable_Head(sMyTable) + ' (';

          for i := 0 to dsSource.FieldCount - 1 do
          begin
            sFieldName := SQL_Escape_FieldName
              (dsSource.FieldDefs.Items[i].Name);
            try
              sCreate := sCreate + sFieldName + ' ' +
                GetSqlFieldType(dsSource.FieldDefs.Items[i].DataType,
                dsSource.FieldDefs.Items[i].Precision,
                dsSource.FieldDefs.Items[i].Size);

              if dsSource.FieldDefs.Items[i].Required then
              begin
                sCreate := sCreate + ' NOT NULL'; // do not localize
              end;

              if i < dsSource.FieldCount - 1 then
              begin
                sCreate := sCreate + ', ';
              end;
            except
              on E: EAbort do
              begin
                Abort;
              end;
              on E: Exception do
              begin
                Application.MessageBox(PChar(E.Message),
                  PChar(Application.Title), MB_ICONEXCLAMATION + MB_OK);
                bOK := false;
                break;
              end;
            end;
          end;

          if bOK then
          begin
            aIndexDefs := dbSource.GetIndexDefs(dsSource);

            // Primärindex...
            if Assigned(aIndexDefs) and (aIndexDefs.Count > 0) then
            begin
              for i := 0 to aIndexDefs.Count - 1 do
              begin
                if ixPrimary in aIndexDefs.Items[i].Options then
                begin
                  sCreate := sCreate + ', primary key(' +
                    StringReplace(aIndexDefs.Items[i].Fields, ';', ',',
                    [rfReplaceAll]) + ')'; // do not localize
                  break;
                end;
              end;
            end;

            sCreate := sCreate + ');';
            ExecSql( { UmlauteRaus } (sCreate));

            // Sekundärindizes...
            if Assigned(aIndexDefs) then
            begin
              for i := 1 to aIndexDefs.Count - 1 do
              begin
                if not(ixPrimary in aIndexDefs.Items[i].Options) then
                begin
                  sTmp := StringReplace(aIndexDefs.Items[i].Fields, ';', ',',
                    [rfReplaceAll]);
                  for j := 1 to Length(sTmp) do
                  begin
                    if CharInset(sTmp[j], ['a' .. 'z', 'A' .. 'Z', '_']) then
                      sFields := sFields + sTmp[j];
                  end;

                  if (ixUnique in aIndexDefs.Items[i].Options) then
                    sUnique := 'UNIQUE ' // do not localize
                  else
                    sUnique := '';

                  // SQL Server: Indexname max. Länge 128 (wird geknackt bei BELEGEWABASIS...).
                  // TODO: Warum nicht den gleichen Index-Namen verwenden wie in der Quelldatenbank?
                  sCreate := 'CREATE ' + sUnique + 'INDEX I' +
                    UpperCase(Copy(sFields, 1, 127)) + ' ON ' +
                    SQL_Escape_TableName(sMyTable) + '(' +
                    StringReplace(aIndexDefs.Items[i].Fields, ';', ',',
                    [rfReplaceAll]) + ');'; // do not localize
                  ExecSql( { UmlauteRaus } (sCreate));
                end;
              end;
            end;

          end;
        finally
          FreeAndNil(dsSource);
        end;
      end;

      if (bOK) then
      begin
        // So, die Zieltabelle ist angelegt. Jetzt rein mit den Datensätzen!
        dsSource := dbSource.Query('SELECT * FROM ' +
          SQL_Escape_TableName(sTable) + ';');
        // nicht freigeben! // do not localize
        pd1.MaxValue := dsSource.RecordCount + 1;
        pd1.ShowExactPosition := true;
        pd1.ShowStopButton := true;
        dsDest := GetTable(sMyTable);
        try
          // Feldindizes in Array schreiben für mehr Geschwindigkeit
          iFieldCount := 0;

          SetLength(aiFeldIndizes, dsSource.Fields.Count);
          for i := 0 to dsSource.Fields.Count - 1 do
          begin
            try
              aiFeldIndizes[iFieldCount] :=
                dsDest.FieldByName
                ( { UmlauteRaus } (dsSource.Fields.Fields[i].FieldName)).Index;
              Inc(iFieldCount);
            except
              on E: EAbort do
              begin
                Abort;
              end;
              on E: Exception do
              begin
                // ignore
              end;
            end;
          end;

          while not dsSource.Eof do
          begin
            dsDest.Append;

            try
              for i := 0 to iFieldCount - 1 do
              begin
                if dsSource.Fields.Fields[aiFeldIndizes[i]].IsNull then
                  dsDest.Fields.Fields[i].Clear
                else if dsDest.Fields.Fields[aiFeldIndizes[i]].DataType = ftBoolean
                then
                  dsDest.Fields.Fields[aiFeldIndizes[i]].AsBoolean :=
                    dsSource.Fields.Fields[i].AsBoolean
                else if dsDest.Fields.Fields[aiFeldIndizes[i]].DataType
                  in [ftDate, ftTime, ftDateTime] then
                  dsDest.Fields.Fields[aiFeldIndizes[i]].AsDateTime :=
                    dsSource.Fields.Fields[i].AsDateTime
                else if dsDest.Fields.Fields[aiFeldIndizes[i]].DataType
                  in [ftFloat, ftBCD, ftFMTBcd, ftSingle, ftExtended] then
                  dsDest.Fields.Fields[aiFeldIndizes[i]].AsFloat :=
                    dsSource.Fields.Fields[i].AsFloat
                else if dsDest.Fields.Fields[aiFeldIndizes[i]].DataType
                  in [ftInteger, ftSmallint] then
                  dsDest.Fields.Fields[aiFeldIndizes[i]].AsInteger :=
                    dsSource.Fields.Fields[i].AsInteger
                else if dsDest.Fields.Fields[aiFeldIndizes[i]].DataType <> ftAutoInc
                then
                  dsDest.Fields.Fields[aiFeldIndizes[i]].AsWideString :=
                    dsSource.Fields.Fields[i].AsWideString;
                // this is also good for most of other types (even integer types)
              end;
              dsDest.Post;
            except
              on E: EAbort do
              begin
                dsDest.Cancel;
                Abort;
              end;
              on E: Exception do
              begin
                Application.MessageBox
                  (PChar('(TDbToolDatabase.ImportFromDatabase) ' + E.Message),
                  PChar(Application.Title), MB_ICONEXCLAMATION + MB_OK);
                // do not localize
                dsDest.Cancel;
                Abort;
              end;
            end;

            pd1.IncPos;
            if pd1.StopButtonSignal then
              Abort;
            Application.ProcessMessages;
            dsSource.Next;
          end;
        finally
          FreeAndNil(dsDest);
          FreeAndNil(dsSource);
        end;
      end;

      if bOK then
      begin
        if (dbSource.DatabaseType = DatabaseType) and dbSource.GetTriggers_Implemented
        then
        begin
          slTrigger := TStringList.Create;
          try
            dbSource.GetTriggers(sTable, slTrigger);
            for i := 0 to slTrigger.Count - 1 do
            begin
              try
                ExecSql(slTrigger.ValueFromIndex[i]);
              except
                on E: EAbort do
                begin
                  Abort;
                end;
                on E: Exception do
                begin
                  Application.MessageBox(PChar(Format(STriggerCouldNotBeCreated,
                    [slTrigger.Names[i], E.Message])), PChar(Application.Title),
                    MB_ICONEXCLAMATION + MB_OK);
                end;
              end;
            end;
          finally
            FreeAndNil(slTrigger);
          end;
        end;
      end;

      if not bOK then
      begin
        // TODO: Eventuell fragen, ob man mergen möchte
        Application.MessageBox(PChar(Format(SCouldNotCopyTable, [sTable])),
          PChar(Application.Title), MB_ICONEXCLAMATION + MB_OK);
      end;
    except
      on E: EAbort do
      begin
        Abort;
      end;
      on E: Exception do
      begin
        Application.MessageBox(PChar('(TDbToolDatabase.ImportFromDatabase) ' +
          E.Message), PChar(Application.Title), MB_ICONEXCLAMATION + MB_OK);
        // do not localize
      end;
    end;
    pd1.Close;
  finally
    FreeAndNil(pd1);
  end;
end;

function Ist_HsInfo2_Datenbank(slTables: TStrings): boolean;
begin
  result := (slTables.IndexOf('SERVICE') >= 0) and // do not localize
    (slTables.IndexOf('ZUGANGSDATEN') >= 0) and // do not localize
    (slTables.IndexOf('SIGNAL') >= 0) and // do not localize
    (slTables.IndexOf('ANRUF_HISTORIE') >= 0); // do not localize
end;

function Ist_CmDb2_Datenbank(slTables: TStrings): boolean;
begin
  result := (slTables.IndexOf('COMMISSION') >= 0) and // do not localize
    (slTables.IndexOf('ARTIST') >= 0) and // do not localize
    (slTables.IndexOf('CONFIG') >= 0) and // do not localize
    (slTables.IndexOf('MANDATOR') >= 0); // do not localize
end;

function Ist_CORA_Datenbank(slTables: TStrings): boolean;
var
  istSystemDb: boolean;
  istMandantenDb: boolean;
begin
  istSystemDb := (slTables.IndexOf('CORASYS') >= 0) and // do not localize
    (slTables.IndexOf('MANDANTEN') >= 0) and // do not localize
    (slTables.IndexOf('BEDIENER') >= 0); // do not localize
  istMandantenDb := (slTables.IndexOf('BELEGEWABASIS') >= 0) and
  // do not localize
    (slTables.IndexOf('ARTIKEL') >= 0) and // do not localize
    (slTables.IndexOf('GEBINDE') >= 0); // do not localize
  result := istSystemDb or istMandantenDb;
end;

var
  HickelSOFTEinmaligBestaetigt: boolean = false;
  FIstHickelSoftProduktDb_Cache: TProductDbType;

function TDbToolDatabase.CheckDatabaseSecurityPassword: boolean;

resourcestring
  SDbPasswordQueryCaption = 'Passwortabfrage';
  SDbPasswordQueryText = 'Für diese Datenbank ist ein Passwort erforderlich';

type
  TAcceptDbPasswords = set of (apHickelEmployee, apHickelPC, apCmDbAdmin);

  function VerifyCmDb2Password(s: string): boolean;
  var
    hashedPassword, salt: string;
    q: TDataSet;
  begin
    q := Query('select VALUE from CONFIG where NAME = ''PASSWORD_HASHED'';');
    // do not localize
    try
      hashedPassword := q.Fields[0].AsWideString;
    finally
      FreeAndNil(q);
    end;
    if (hashedPassword = '') and (s = '') then
    begin
      result := true;
    end
    else
    begin
      q := Query('select VALUE from CONFIG where NAME = ''INSTALL_ID'';');
      // do not localize
      try
        salt := q.Fields[0].AsWideString;
      finally
        FreeAndNil(q);
      end;
      result := SameText(THashSHA2.GetHashString(salt + s), hashedPassword);
    end;
  end;

  function VerifyHickelSOFTPassword(s: string): boolean;
  begin
    // Defined in HS_Auth.pas (confidential / redacted in OpenSource release)
    result := PruefeHickelSoftPassword(s);
  end;

  function RequireDatabasePassword(accepted: TAcceptDbPasswords): boolean;
  var
    s: string;
  begin
    if ((apHickelEmployee in accepted) and HickelSOFTEinmaligBestaetigt) or
       ((apHickelPC in accepted) and IstHickelSoftTestPC) or
       ((apCmDbAdmin in accepted) and VerifyCmDb2Password(''{Not protected})) then
    begin
      result := true;
      exit;
    end;

    while true do
    begin
      // Das "#0" sorgt dafür, dass es ein Passwort-Eingabefeld ist!
      s := '';
      if not InputQuery(SDbPasswordQueryCaption, #0 + SDbPasswordQueryText, s) then
      begin
        result := false;
        exit;
      end;
      if apHickelEmployee in accepted then
      begin
        result := VerifyHickelSOFTPassword(s);
        if result then
        begin
          HickelSOFTEinmaligBestaetigt := true;
          exit;
        end;
      end;
      if apCmDbAdmin in accepted then
      begin
        result := VerifyCmDb2Password(s);
        if result then
        begin
          exit;
        end;
      end;
    end;
  end;

begin
  result := true;
  if IstHickelSoftProduktDb then
  begin
    if (FIstHickelSoftProduktDb_Cache = ptCORAplus) or
      (FIstHickelSoftProduktDb_Cache = ptHsInfo2) then
    begin
      RequireDatabasePassword([apHickelEmployee, apHickelPC]);
    end
    else if FIstHickelSoftProduktDb_Cache = ptCmDb2 then
    begin
      RequireDatabasePassword([apHickelEmployee, apHickelPC, apCmDbAdmin]);
    end;
  end;
end;

function TDbToolDatabase.IstHickelSoftProduktDb: boolean;
var
  slTables: TStringList;
begin
  if FIstHickelSoftProduktDb_Cache = ptNotChecked then
  begin
    slTables := TStringList.Create;
    try
      GetTableNames(slTables);
      if Ist_CORA_Datenbank(slTables) then
        FIstHickelSoftProduktDb_Cache := ptCORAplus
      else if Ist_HsInfo2_Datenbank(slTables) then
        FIstHickelSoftProduktDb_Cache := ptHsInfo2
      else if Ist_CmDb2_Datenbank(slTables) then
        FIstHickelSoftProduktDb_Cache := ptCmDb2
      else
        FIstHickelSoftProduktDb_Cache := ptOther;
    finally
      FreeAndNil(slTables);
    end;
  end;
  result := FIstHickelSoftProduktDb_Cache <> ptOther;
end;

end.
