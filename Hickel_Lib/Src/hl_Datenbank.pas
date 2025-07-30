unit hl_Datenbank;

// DM: Dies ermöglicht, dass eine verschachtelte Transaktion nicht zum Fehler
// führt. Allerdings ist diese Funktionalität gefährlich, denn ein Rollback
// in einer verschachtelten Transaktion würde "verschluckt" werden.
// Daher deaktiviert.
{ .$IFDEF TransaktionsTiefeAutomatik }

interface

{$IF CompilerVersion <= 20.0}
{$DEFINE UseBetterADO}
{$IFEND}

uses
  ActiveX, hl_Exceptions, AdoDB, {$IFDEF UseBetterADO}BetterAdoDataset, {$ENDIF} DB, SysUtils, hl_Log, Forms,
  hl.System.Types, Classes (*, hl.Datenbank.RowLock*) , Windows, wwdblook;

const
  MaxChars = 50000;

type
  /// <summary>
  /// <b>lmFehlerAnzeigenUndAbbrechen</b>: Exception wird angezeigt, Programmfluss wird abgebrochen.<br />
  /// <b>lmExceptionWerfen</b>: Bei einem Fehler wird eine echte Exception geworfen.<br />
  /// <b>lmBeiFehlerAbbrechen</b>: Bei einem Fehler wird der Programmfluss abgebrochen, allerdings ohne Fehlermeldung.<br />
  /// <b>lmFehlerIgnorieren</b>: Es wird trotz Fehler bei der nächsten Zeile weitergemacht. ACHTUNG: Nicht zusammen mit "WithTransAction" nutzen.<br />
  /// <b>lmFehlerAnzeigenUndWeitermachen</b>: Es wird trotz Fehler bei der nächsten Zeile weitergemacht, allerdings wird der Fehler angezeigt. ACHTUNG: Nicht zusammen mit "WithTransAction" nutzen.
  /// </summary>
  TExecSQLListMode = (lmFehlerAnzeigenUndAbbrechen, lmExceptionWerfen,
    lmBeiFehlerAbbrechen, lmFehlerIgnorieren, lmFehlerAnzeigenUndWeitermachen);

type
  // Das wird benötigt, da TField an ein Datenbankfeld gebunden ist
  ThlDatenbankFeld = record
  private
    mValue: hlString;
    mIsNull: boolean;
  public
    constructor Create(value: TField); overload;
    constructor Create(value: hlString); overload;
    function AsHlBoolean: hlBoolean;
    function AsHlFloat: hlDecimal;
    function AsHlInteger: hlInteger;
    function AsHlString: hlString;
    function AsHlDecimal: hlDecimal;
    function AsHlDateTime: hlDateTime;

    function AsBoolean: boolean;
    function AsFloat: Double;
    function AsInteger: integer;
    function AsInt64: int64;
    function AsString: string;
    function AsWideString: string;
    function AsDateTime: TDateTime;

    property IsNull: boolean read mIsNull;
  end;

type
  EHSCannotGetAutoInc = class(Exception);

type
  ThlDataSet = type TADODataSet;

type
  ThlSQLDebugEvent = procedure(const query: string; milliseconds: integer)
    of object;

type
  ThlDatenbank = class(TObject)
  private
    mConnection: TAdoConnection;
    mCommand: TAdoCommand;
    mScalarTable: ThlDataSet;

    mTransaktionsTiefe: integer;

    FLastKnownConnectionID: TGuid;

    procedure CreateIndiv(ConnStr: string; AConnectionTimeout: integer = 0);
    procedure CreateStandard(Datenbank, Server: string;
      AnmeldungAlsBenutzer: boolean; AConnectionTimeout: integer = 0);

    /// <summary>Die holt jeweils den nächsten (und nur den einen) Befehl aus der Stringliste und speichert diesen ohne Zeilenumbrüche im result.</summary>
    /// <param name="ListFile">In dieser Stringliste werden die Befehle, die das File enthält gespeichert. Sie ist als var angelegt, d.h. diue aufrufende Funktion oder Procedure kann hier Werte zurückerhalten.</param>
    class function GetBefehlszeile(ListFile: Tstringlist): string;
    function GetCommandTimeout: integer;
    procedure SetCommandTimeout(const value: integer);
    function GetDatenbankName: hlString;

    function GetConnectionID: TGuid;

    class procedure MakeActiveTryReconnect(q: TCustomADODataSet;
      active: boolean = true);
  protected
    ownsConnection: boolean;

    // Diese werden NICHT aufgerufen, wenn ownsConnection=true!
    procedure ConnAfterConnect(Sender: TObject); virtual;
    procedure ConnBeforeDisconnect(Sender: TObject); virtual;

    /// <summary>Ersetzt /**/ durch einen Zeilenumbruch und /*.*/ durch ein Semikolon mit einem Zeilenumbruch.</summary>
    /// <remarks>Wird von ExecSqlList und SaveSqlListToFile verwendet, damit Trigger trotz ReorgSqlList trotzdem noch gescheit aussehen und auch komplizierte Programmabläufe (in Triggern), die Semikolons benötigen, realisiert werden können</remarks>
    class procedure ErweitereSQLZeile(var zeile: string);

    /// <remarks>Achtung! Nicht benutzen, um Tabellen zu initialisieren! Die Funktionen SetConnection CreateNewADOQuery und CreateNewADOTable sollen verwendet werden sollen. (Diese kümmern sich darum, dass EnableBCD auch wirklich überall verwendet wird.) Ebenfalls nicht verwenden, um eine SQL-Query durchzuführen, da sich die Funktion ExecSQL darum kümmert, dass bestehende Transaktionen im Fehlerfall abgebrochen werden.</remarks>
    property Connection: TAdoConnection read mConnection;
  public
    class var Debug: ThlSQLDebugEvent;

    constructor Create(Datenbank, Server: string; AnmeldungAlsBenutzer: boolean;
      Isolated: boolean = false; AConnectionTimeout: integer = 0);
      reintroduce; overload;
    constructor Create(ConnStr: string; Isolated: boolean = false;
      AConnectionTimeout: integer = 0); overload;
    constructor Create(ADOConnection: TAdoConnection); reintroduce; overload;
    destructor Destroy; reintroduce; override;

    procedure BeginTransaction;
    procedure Commit;
    procedure Rollback;

    function IstExpressEdition: boolean;

    function DbOwnerSid: string;
    function SqlServerMac: string;

    procedure ShrinkDatabase(Datenbankname: string; typ: string = 'LOG');

    class function GetScalar(sql: hlString; adoCon: TAdoConnection;
      timeout: integer = 300; controlsEnabled: boolean = false)
      : ThlDatenbankFeld; overload;
    function GetScalar(sql: string): ThlDatenbankFeld; overload;
    function GetScalar(timeout: integer; sql: string)
      : ThlDatenbankFeld; overload;

    function Any(const sql: string): boolean;

    property ConnectionID: TGuid read GetConnectionID;
    // function ConnectionString: string;
    // function Clone: ThlDatenbank;

    procedure RecheckConnectionStatus(Sender: TObject);

    class function GetTableWithCon(sql: hlString; adoCon: TAdoConnection;
      timeout: integer = 300; controlsEnabled: boolean = false)
      : ThlDataSet; overload;
    function GetTable(sql: string = ''; controlsEnabled: boolean = false)
      : ThlDataSet; overload;
    function GetTable(timeout: integer; sql: string = '';
      controlsEnabled: boolean = false): ThlDataSet; overload;

    /// <summary>
    /// Führt eine Query aus und liefert den eingefügten AutoIncrement-Wert zurück
    /// </summary>
    /// <param name="query">Die SQL-Abfrage die ausgeführt werden soll.
    /// Es muss sich um einen INSERT-Befehl handeln oder einen vergleichbaren Befehl,
    /// der die SQL-Session-Variable @@IDENTITY beeinflusst.</param>
    /// <returns>Der angelegte AutoIncrement-Wert</returns>
    function InsertAndReturnID(query: string): integer;

    class procedure ExecSql(sQuery: string; aCon: TAdoConnection); overload;
    procedure ExecSql(sql: string = ''); overload;
    procedure ExecSql(timeout: integer; sql: string = ''); overload;

    property Datenbankname: hlString read GetDatenbankName;

    class procedure DropTable(aTableName: hlString;
      adoCon: TAdoConnection); overload;
    procedure DropTable(aTableName: string); overload;

    function FieldCount(aTableName: string): integer;
    function IndexCount(aTableName: string): integer;

    function TableExists(aTableName: hlString): boolean; overload;
    class function TableExists(aTableName: hlString; adoCon: TAdoConnection)
      : boolean; overload;

    function ViewExists(aViewName: hlString): boolean; overload;
    class function ViewExists(aViewName: hlString; adoCon: TAdoConnection)
      : boolean; overload;

    function ColumnExists(aTableName, aColumnName: hlString): boolean;

    procedure DropColumn(aTableName, aColumnName: hlString);
    function IndexExists(aTableName, aIndexName: hlString): boolean;

    procedure SetConnection(x: TADOQuery;
      adoCon: TAdoConnection = nil); overload;
    procedure SetConnection(x: TADOTable;
      adoCon: TAdoConnection = nil); overload;
    procedure SetConnection(x: TADODataSet;
      adoCon: TAdoConnection = nil); overload;

    function CreateNewADOQuery(adoCon: TAdoConnection = nil): TADOQuery;
    function CreateNewADOTable(adoCon: TAdoConnection = nil): TADOTable;
    function CreateNewADODataSet(adoCon: TAdoConnection = nil): TADODataSet;

    class function StaticCreateNewADOQuery(adoCon: TAdoConnection = nil)
      : TADOQuery;
    class function StaticCreateNewADOTable(adoCon: TAdoConnection = nil)
      : TADOTable;
    class function StaticCreateNewADODataSet(adoCon: TAdoConnection = nil)
      : TADODataSet;

    class procedure StaticSetConnection(x: TADOQuery;
      adoCon: TAdoConnection = nil); overload;
    class procedure StaticSetConnection(x: TADOTable;
      adoCon: TAdoConnection = nil); overload;
    class procedure StaticSetConnection(x: TADODataSet;
      adoCon: TAdoConnection = nil); overload;

    /// <summary>Setzt für alle TADOTable, TADOQuery etc. die "Connection"-Eigenschaft auf die Datenbankverbindung in hclMandanten. Außerdem wird EnableBCD auf False gesetzt.</summary>
    procedure ConnectionsFuerFormKomponentenSetzen(aForm: TForm); overload;
    procedure ConnectionsFuerFormKomponentenSetzen(aForm: TFrame); overload;

    /// <summary>Diese Funktion soll anstelle von x.Active=true verwendet werden. Sie sorgt dafür, dass alle per MasterSource verbundenen Tabellen ebenfalls reaktiviert werden.</summary>
    class procedure ReaktiviereVerbindung(x: TDataset; aForm: TForm);

    class function DefaultCommandTimeout: integer;

    /// <summary>Funktion lädt ein File, übersetzt den Inhalt in getrennte SQL-Befehle (;-separated) und speichert diese in je einem Eintrag in der Stringliste.</summary>
    /// <param name="aFileName">Hier kommt das File, das gelden werden soll, mitsamt der Pfadangaben</param>
    /// <param name="aList">In dieser Stringliste werden die Befehle, die das File enthält gespeichert. Sie ist als var angelegt, d.h. diue aufrufende Funktion oder Procedure kann hier Werte zurückerhalten.</param>
    class function FileToStringlist(aFileName: string;
      aList: Tstringlist): integer;

    /// <summary>Hier wird eine übergeben Stringlist einfgach als Sql ausgeführt, diese Stringlist kann mehrere SQL-Befehle enthalten, Die Connection muss ebenfalls angegeben werden.</summary>
    /// <param name="aList">Enthält die Liste der SQL-Befehle</param>
    /// <param name="mode">siehe <see>TExecSQLListMode</see></param>
    /// <param name="WithTransaction">Wenn true, dann werden die SQL-Befehle in eine Transaktion gepackt.</param>
    /// <param name="timeout">Timeout in Sekunden</param>
    /// <param name="slFehler">Sofern der Modus lmFehlerAnzeigenUndWeitermachen oder lmFehlerIgnorieren ist, werden die SQL-Fehler in diese StringList geschrieben.</param>
    function ExecSqlList(aList: TStrings;
      mode: TExecSQLListMode = lmFehlerAnzeigenUndAbbrechen;
      WithTransaction: boolean = false; timeout: integer = 600;
      slFehler: TStrings = nil): boolean;

    class procedure SaveSqlListToFile(aList: TStrings; fileName: string);

    /// <summary>Funktion lädt ein File, übersetzt den Inhalt in getrennte SQL-Befehle (;-separated) und speichert diese in je einem Eintrag in der Stringliste.</summary>
    /// <param name="aList">In dieser Stringliste werden die Befehle, die das File enthält gespeichert. Sie ist als var angelegt, d.h. diue aufrufende Funktion oder Procedure kann hier Werte zurückerhalten.</param>
    /// <remarks>Vorsicht: Funktioniert nicht immer, z.B. werden "begin try" oder "begin catch" nicht als eigenständige Zeile angesehen und man darf kein ; verwenden.</remarks>
    class function ReorgExecList(aList: TStrings): integer;

    (*
      class procedure ExecSql(sQuery: string; aCon: TAdoConnection); overload;
      class procedure ExecSql(timeOut: integer; sQuery: string; aCon: TAdoConnection); overload;
      class procedure DropTable (aTableName: string; aCon: TAdoConnection); overload;
    *)

    class procedure ttRefresh(aTable: TDataset);

    procedure GetTableNames(List: TStrings; SystemTables: boolean = false);

    property CommandTimeout: integer read GetCommandTimeout
      write SetCommandTimeout;

    property Transaktionstiefe: integer read mTransaktionsTiefe;
    function InTransaction: boolean;

    class function SQKKommentareUmwandeln(sql: TStrings;
      subquery: boolean = false): string;

    procedure DefragIndexes;

{$REGION 'RowLock-Funktionen'}
    (*
      function NewLock(AUsername, ATableName, APK1, APK2, APK3, APK4, APK5, APK6: hlString): ThlRowLock;
      function NewLockSpecial(AUsername, AModule, AIdentifier: hlString): ThlRowLock;
      procedure RemoveAllUserLocks(AUsername: hlString);
      procedure RemoveAllLocks;
      procedure RemoveInvalidLocks;
    *)
{$ENDREGION}
  end;

function ConnStrReadAttr(attr, ConnStr: string): string;
function ConnStrWriteAttr(attr, val, ConnStr: string): string;

implementation

uses
  MessaBox, hl.System.ExceptionHandler, HsDruckerConfig, hl_SqlServerProvider,
  HS_Auth;

resourcestring
  StrGetScalarMitLeerem = 'GetScalar mit leerem SQL String aufgerufen';
  StrFehlerSBeimAusfü = 'Fehler %s beim Ausführen der Query %s';
  StrFehlerSBeimSQLV = 'Fehler %s beim SQL-Verbindungsaufbau in "%s"';
  StrHighResolutionTime = 'High resolution timer not availalbe!';
  StrFolgendeSQLQueryK =
    'Folgende SQL-Query konnte nicht an den Datenbank-Server gesendet werden, da die Verbindung bereits abgebaut wurde:';
  StrQueryFEHLGESCHLAGEN = 'Query FEHLGESCHLAGEN: %s bei Query %s';
  StrExceptionSAtQuer = 'Ausnahmefehler %s bei Datenbankabfrage %s';
  StrFehlerSBeimAusfüTransaktion =
    'Fehler %s beim Ausführen der Transaktion in ExecSqlList';
  StrAutoIncFehler = 'Kann keinen AutoIncrement-Wert feststellen!';
  StrInternerFehlerInS =
    'Interner Fehler in Sql_Scripts.ExecSqlList(): lmFehlerIgnorieren/lmFehlerAnzeigenUndWeitermachen und WithTransAction sind nicht vereinbar. Bitte melden Sie den Fehler an HickelSOFT.';

var
  { Timer für Debugging }
  iFrequency, iTimerStart, iTimerEnd: int64;

procedure _TimerStart;
begin
  if NOT QueryPerformanceFrequency(iFrequency) then
    raise Exception.Create(StrHighResolutionTime);
  QueryPerformanceCounter(iTimerStart);
end;

function _TimerEnd: integer; { In miliseconds }
begin
  QueryPerformanceCounter(iTimerEnd);
  Result := Round(1000 * ((iTimerEnd - iTimerStart) / iFrequency));
end;

{ ThlDatenbankFeld }

constructor ThlDatenbankFeld.Create(value: TField);
begin
  if Assigned(value) then
{$IFDEF UNICODE}
    mValue := hlString.Create(value.AsWideString)
{$ELSE}
    mValue := hlString.Create(value.AsString)
{$ENDIF}
  else
    mValue := '';

  mIsNull := value.IsNull;
end;

constructor ThlDatenbankFeld.Create(value: hlString);
begin
  mValue := value;
  mIsNull := false; // kann nicht bestimmt werden!!!
end;

function ThlDatenbankFeld.AsHlBoolean: hlBoolean;
begin
  if mValue <> '' then
    Result := mValue.toString
  else
    Result := false;
end;

function ThlDatenbankFeld.AsHlDecimal: hlDecimal;
begin
  if mValue <> '' then
    Result := mValue.toString // mValue.toString
  else
    Result := 0.00000;
end;

function ThlDatenbankFeld.AsHlDateTime: hlDateTime;
begin
  if mValue <> '' then
    Result := mValue.toString // mValue.toString
  else
    Result := 0.00000;
end;

function ThlDatenbankFeld.AsHlFloat: hlDecimal;
begin
  if mValue <> '' then
    Result := mValue
  else
    Result := 0.0;
end;

function ThlDatenbankFeld.AsHlInteger: hlInteger;
begin
  if mValue <> '' then
    Result := mValue
  else
    Result := 0;
end;

function ThlDatenbankFeld.AsHlString: hlString;
begin
  Result := mValue;
end;

function ThlDatenbankFeld.AsBoolean: boolean;
begin
  Result := AsHlBoolean;
end;

function ThlDatenbankFeld.AsDateTime: TDateTime;
begin
  Result := AsHlDateTime;
end;

function ThlDatenbankFeld.AsFloat: Double;
begin
  Result := AsHlFloat;
end;

function ThlDatenbankFeld.AsInt64: int64;
begin
  Result := AsHlInteger;
end;

function ThlDatenbankFeld.AsInteger: integer;
begin
  Result := AsHlInteger;
end;

function ThlDatenbankFeld.AsString: string;
{$IFDEF UNICODE}
var
  sws: string;
  sas: AnsiString;
begin
  sws := AsHlString;
  sas := AnsiString(sws);
  Result := string(sas);
{$ELSE}
begin
  Result := AsHlString;
{$ENDIF}
end;

function ThlDatenbankFeld.AsWideString: string;
begin
  Result := AsHlString;
end;

{ ThlDatenbank }

function ThlDatenbank.TableExists(aTableName: hlString): boolean;
begin
  Result := TableExists(aTableName, mConnection);
end;

class function ThlDatenbank.GetTableWithCon(sql: hlString;
  adoCon: TAdoConnection; timeout: integer = 300;
  controlsEnabled: boolean = false): ThlDataSet;
begin
  // TODO: Diese Methode ist leider nicht statisch...
  // result := CreateNewADODataset(adoCon);

{$IFDEF UseBetterADO}
  Result := ThlDataSet(TBetterADODataSet.Create(nil));
{$ELSE}
  Result := ThlDataSet(TADODataSet.Create(nil));
{$ENDIF}
  Result.EnableBCD := false;
  Result.Connection := adoCon;
  Result.ParamCheck := false;
  Result.CommandText := sql;
  Result.CommandTimeout := timeout;
  if Assigned(Debug) then
    _TimerStart;
  MakeActiveTryReconnect(Result, Trim(sql) <> '');
  if Assigned(Debug) then
    Debug(sql, _TimerEnd);
  if not controlsEnabled then
  begin
    Result.DisableControls;
    // Mach iterationen viel schneller, auch wenn keine GUI dran hängt!
  end;
end;

class function ThlDatenbank.GetScalar(sql: hlString; adoCon: TAdoConnection;
  timeout: integer = 300; controlsEnabled: boolean = false): ThlDatenbankFeld;
var
  q: ThlDataSet;
begin
  if sql = '' then
    raise Exception.Create(StrGetScalarMitLeerem);
  q := GetTableWithCon(sql, adoCon, timeout, controlsEnabled);
  try
    if q.RecordCount = 0 then
    begin
      Result := ThlDatenbankFeld.Create('');
      Result.mIsNull := true;
    end
    else
      Result := ThlDatenbankFeld.Create(q.Fields[0]);
  finally
    FreeAndNil(q);
  end;
end;

class function ThlDatenbank.TableExists(aTableName: hlString;
  adoCon: TAdoConnection): boolean;
begin
  if Copy(aTableName, 1, 1) = '#' then
  begin
    // TempTable
    Result := GetScalar('select case when OBJECT_ID(''tempdb..' + aTableName +
      ''') is not null then ''1'' else ''0'' end', adoCon).AsInteger > 0;
  end
  else
  begin
    // Physikalische Tabelle (in Schema dbo)
    // result := GetScalar('select count (*) from sysobjects where name = ' + aTableName.toSQLString).AsInteger > 0;
    // result := GetScalar('SELECT 1 FROM ['+adoCon.DefaultDatabase+'].sys.tables WHERE name = N'''+aTableName+''' AND schema_id = SCHEMA_ID(''dbo'')').AsInteger > 0;
    Result := GetScalar
      ('SELECT count(*) FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_CATALOG = N'''
      + adoCon.DefaultDatabase +
      ''' AND TABLE_SCHEMA = N''dbo'' AND TABLE_NAME = N''' + aTableName + '''',
      adoCon).AsInteger > 0;
  end;
end;

procedure ThlDatenbank.CreateStandard(Datenbank, Server: string;
  AnmeldungAlsBenutzer: boolean; AConnectionTimeout: integer = 0);

  procedure _Versuchen;
  var
    sqlConnStr: string;
  begin
    sqlConnStr := 'Provider=' + SqlServerProvider + ';';
    sqlConnStr := sqlConnStr + 'Application Name=' + ExtractFileName(ParamStr(0))
      + ' hl_Datenbank;';

    if not(AnmeldungAlsBenutzer) then
      sqlConnStr := sqlConnStr +
        'Integrated Security=SSPI;Persist Security Info=False;'
    else
      sqlConnStr := sqlConnStr + 'User ID=' + HS_SA_DB_USER + ';Password=' +
        HS_SA_DB_PASSWORD + ';Persist Security Info=True;';

    if SqlServerProvider = 'MSOLEDBSQL19' then
      sqlConnStr := sqlConnStr + 'Use Encryption for Data=False;';

    // DataTypeCompatibility=80, ansonsten funktionieren "time" Datentypen nicht! (sind im Fields[] und FieldDefs[] nicht da und dbGrid verursacht AccessViolation)
    if SqlServerProvider <> 'SQLOLEDB' then
      sqlConnStr := sqlConnStr + 'DataTypeCompatibility=80;';

    sqlConnStr := sqlConnStr + 'Initial Catalog=' + Datenbank + ';';
    sqlConnStr := sqlConnStr + 'Data Source=' + Server;

    CreateIndiv(sqlConnStr, AConnectionTimeout);
  end;

begin
  try
    HS_SA_DB_OLD := false; // "cora-client" Auth für die Zeit nach dem Anlegen
    _Versuchen;
  except
    on E: EAbort do
    begin
      Abort;
    end;
    on E: Exception do
    begin
      HS_SA_DB_OLD := true;
      _Versuchen;
    end;
  end;
end;

constructor ThlDatenbank.Create(Datenbank, Server: string;
  AnmeldungAlsBenutzer: boolean; Isolated: boolean = false;
  AConnectionTimeout: integer = 0);
begin
  inherited Create;
  CreateStandard(Datenbank, Server, AnmeldungAlsBenutzer, AConnectionTimeout);
  if (Isolated) then
    mConnection.IsolationLevel := ilIsolated;
end;

{$REGION 'Funktionen, die ausgeschaltetes BCD sicherstellen'}

function ThlDatenbank.CreateNewADODataSet(adoCon: TAdoConnection = nil)
  : TADODataSet; // TODO: "class function" ???
begin
  // Abgeschaltet weil CORA SCHON WIEDER zu langsam wird!!! (Ticket 59369, 59374, 59381)
  // RecheckConnectionStatus(self);

{$IFDEF UseBetterADO}
  Result := TBetterADODataSet.Create(nil);
{$ELSE}
  Result := TADODataSet.Create(nil);
{$ENDIF}
  SetConnection(Result, adoCon);
  Result.EnableBCD := false;
end;

function ThlDatenbank.CreateNewADOQuery(adoCon: TAdoConnection = nil)
  : TADOQuery;
begin
  // Abgeschaltet weil CORA SCHON WIEDER zu langsam wird!!! (Ticket 59369, 59374, 59381)
  // RecheckConnectionStatus(self);

  Result := TADOQuery.Create(nil);
  SetConnection(Result, adoCon);
  Result.EnableBCD := false;
end;

function ThlDatenbank.CreateNewADOTable(adoCon: TAdoConnection = nil)
  : TADOTable;
begin
  // Abgeschaltet weil CORA SCHON WIEDER zu langsam wird!!! (Ticket 59369, 59374, 59381)
  // RecheckConnectionStatus(self);

  Result := TADOTable.Create(nil);
  SetConnection(Result, adoCon);
  Result.EnableBCD := false;
end;

procedure ThlDatenbank.SetConnection(x: TADOQuery;
  adoCon: TAdoConnection = nil);
begin
  x.ConnectionString := '';
  if Assigned(adoCon) then
    x.Connection := adoCon
  else
    x.Connection := mConnection;
  x.EnableBCD := false;
end;

procedure ThlDatenbank.SetConnection(x: TADOTable;
  adoCon: TAdoConnection = nil);
begin
  x.ConnectionString := '';
  if Assigned(adoCon) then
    x.Connection := adoCon
  else
    x.Connection := mConnection;
  x.EnableBCD := false;
end;

class procedure ThlDatenbank.SaveSqlListToFile(aList: TStrings;
  fileName: string);
var
  i: integer;
  line: string;
  sl: Tstringlist;
begin
  sl := Tstringlist.Create;
  try
    for i := 0 to aList.Count - 1 do
    begin
      line := aList[i];
      ErweitereSQLZeile(line);
      sl.Add(line);
    end;
    sl.SaveToFile(fileName);
  finally
    FreeAndNil(sl);
  end;
end;

procedure ThlDatenbank.SetCommandTimeout(const value: integer);
begin
  mConnection.CommandTimeout := value;
end;

procedure ThlDatenbank.SetConnection(x: TADODataSet; adoCon: TAdoConnection);
begin
  x.ConnectionString := '';
  if Assigned(adoCon) then
    x.Connection := adoCon
  else
    x.Connection := mConnection;
  x.EnableBCD := false;
end;

procedure ThlDatenbank.ShrinkDatabase(Datenbankname: string;
  typ: string = 'LOG'); // typ ist 'LOG' oder 'ROWS'
var
  q: ThlDataSet;
begin
  q := GetTable('SELECT f.name as LogicalName, ' +
    'f.physical_name AS PhysicalName, ' + 'f.type_desc TypeofFile ' +
    'FROM sys.master_files f ' +
    'INNER JOIN sys.databases d ON d.database_id = f.database_id ' +
    'where d.name = ''' + Datenbankname + ''' ' + 'and f.type_desc = ''' +
    typ + ''';');
  try
    while not q.Eof do
    begin
      ExecSql(3600, 'DBCC SHRINKFILE (N''' + q.Fields[0].AsWideString + ''' , 0)');
      q.Next;
    end;
  finally
    FreeAndNil(q);
  end;
end;

class function ThlDatenbank.StaticCreateNewADODataSet(adoCon: TAdoConnection)
  : TADODataSet;
begin
{$IFDEF UseBetterADO}
  Result := TBetterADODataSet.Create(nil);
{$ELSE}
  Result := TADODataSet.Create(nil);
{$ENDIF}
  StaticSetConnection(Result, adoCon);
end;

class function ThlDatenbank.StaticCreateNewADOQuery(adoCon: TAdoConnection)
  : TADOQuery;
begin
  Result := TADOQuery.Create(nil);
  StaticSetConnection(Result, adoCon);
end;

class function ThlDatenbank.StaticCreateNewADOTable(adoCon: TAdoConnection)
  : TADOTable;
begin
  Result := TADOTable.Create(nil);
  StaticSetConnection(Result, adoCon);
end;

class procedure ThlDatenbank.StaticSetConnection(x: TADOQuery;
  adoCon: TAdoConnection);
begin
  x.Connection := adoCon;
  x.EnableBCD := false;
end;

class procedure ThlDatenbank.StaticSetConnection(x: TADODataSet;
  adoCon: TAdoConnection);
begin
  x.Connection := adoCon;
  x.EnableBCD := false;
end;

class procedure ThlDatenbank.StaticSetConnection(x: TADOTable;
  adoCon: TAdoConnection);
begin
  x.Connection := adoCon;
  x.EnableBCD := false;
end;

class procedure ThlDatenbank.ErweitereSQLZeile(var zeile: string);
begin
  zeile := StringReplace(zeile, '/**/', #13#10, [rfReplaceAll]);
  zeile := StringReplace(zeile, '/*.*/', ';' + #13#10, [rfReplaceAll]);
end;

var
  stackoverflowProtection: boolean = false;

procedure ThlDatenbank.RecheckConnectionStatus(Sender: TObject);
var
  CurConnId: TGuid;
begin
  if stackoverflowProtection then
    exit;
  stackoverflowProtection := true;
  try
    CurConnId := ConnectionID;
    if not IsEqualGuid(CurConnId, FLastKnownConnectionID) then
    begin
      if not IsEqualGuid(FLastKnownConnectionID, GUID_NULL) then
      begin
        // ConnBeforeDisconnect(self);
        ConnAfterConnect(self);
      end;
      FLastKnownConnectionID := CurConnId;
    end;
  finally
    stackoverflowProtection := false;
  end;
end;

procedure ThlDatenbank.ConnAfterConnect(Sender: TObject);
begin
  // Hier steht nichts.
end;

procedure ThlDatenbank.ConnBeforeDisconnect(Sender: TObject);
begin
  // Hier steht nichts.
end;

procedure ThlDatenbank.ConnectionsFuerFormKomponentenSetzen(aForm: TForm);
var
  iCounter: integer;
  bakActive: boolean;
begin
  for iCounter := aForm.ComponentCount - 1 downto 0 do
  begin

{$IF CompilerVersion >= 20.0}
    // Das gehört eigentlich nicht in diese Funktion, aber es ist wichtig, dass das überall gemacht wird
    // da sonst alles am Arsch ist. (TAdoTable im Lookup zeigt Fehler "Der aktuelle Provider unterstützt nicht die erforderliche Schnittstelle für die Indexfunktion")
    // (Ticket 54113/11)
    // => Eigentlich OrderByDisplay nur einen Effekt haben, wenn das LookupDataset ein TwwTable ist. Fehler berichtet am 8.6.2023.
    // => Fixed in Woll2Woll InfoPower 4K Alexandria (22.1.0.7, 2023-06-13)
    // DM 21.06.2023: Doch wieder eingebaut, denn es gibt auch mit wwSetIndexName Probleme, nämlich wenn man im Auftrag die Tour ändern will
    // (Ticket 54113/49)
    // DM 08.08.2023: Und es geht grad so weiter...! Logbuch Name ändern, auch Absturz (Ticket 54683/1)
    if (aForm.Components[iCounter] is TwwDBCustomLookupCombo) then
    begin
      TwwDBCustomLookupCombo(aForm.Components[iCounter]).OrderByDisplay
        := false;
    end;
{$IFEND}
    // Hier ist das, um was es eigentlich geht
    if (aForm.Components[iCounter] is TADOTable) then
    begin
      bakActive := TADOTable(aForm.Components[iCounter]).active;
      MakeActiveTryReconnect(TADOTable(aForm.Components[iCounter]), false);
      SetConnection(aForm.Components[iCounter] as TADOTable);
      MakeActiveTryReconnect(TADOTable(aForm.Components[iCounter]), bakActive);
    end;
    if (aForm.Components[iCounter] is TADOQuery) then
    begin
      bakActive := TADOQuery(aForm.Components[iCounter]).active;
      MakeActiveTryReconnect(TADOQuery(aForm.Components[iCounter]), false);
      SetConnection(aForm.Components[iCounter] as TADOQuery);
      MakeActiveTryReconnect(TADOQuery(aForm.Components[iCounter]), bakActive);
    end;
    if (aForm.Components[iCounter] is THsDruckerConfig) then
    begin
      THsDruckerConfig(aForm.Components[iCounter]).Connection :=
        self.Connection;
    end;
  end;
end;

procedure ThlDatenbank.ConnectionsFuerFormKomponentenSetzen(aForm: TFrame);
var
  iCounter: integer;
  bakActive: boolean;
begin
  for iCounter := aForm.ComponentCount - 1 downto 0 do
  begin
    if (aForm.Components[iCounter] is TADOTable) then
    begin
      bakActive := TADOTable(aForm.Components[iCounter]).active;
      MakeActiveTryReconnect(TADOTable(aForm.Components[iCounter]), false);
      SetConnection(aForm.Components[iCounter] as TADOTable);
      MakeActiveTryReconnect(TADOTable(aForm.Components[iCounter]), bakActive);
    end;
    if (aForm.Components[iCounter] is TADOQuery) then
    begin
      bakActive := TADOQuery(aForm.Components[iCounter]).active;
      MakeActiveTryReconnect(TADOQuery(aForm.Components[iCounter]), false);
      SetConnection(aForm.Components[iCounter] as TADOQuery);
      MakeActiveTryReconnect(TADOQuery(aForm.Components[iCounter]), bakActive);
    end;
  end;
end;

// Die Funktion ist nicht gut, da das SA-Passwort nicht preserved wird!
(*
  function ThlDatenbank.ConnectionString: string;
  begin
  result := mConnection.ConnectionString;
  end;
*)

{$ENDREGION}

class procedure ThlDatenbank.ReaktiviereVerbindung(x: TDataset; aForm: TForm);
var
  iCounter, iCounter2: integer;
  Y: TDataSource;
  Z: TADOTable;
begin
  x.active := false; // MakeActiveTryReconnect(x, false);
  x.active := true; // MakeActiveTryReconnect(x, true);
  for iCounter := aForm.ComponentCount - 1 downto 0 do
  begin
    // Schritt 1: Finde alle DataSources, die an das DataSet "X" gebunden sind
    if (aForm.Components[iCounter] is TDataSource) then
    begin
      Y := aForm.Components[iCounter] as TDataSource;
      if Y.DataSet = x then
      begin
        // Schritt 2: Finde alle TADOTables, deren MasterSource unsere zuvor gefundene DataSource ist.
        for iCounter2 := aForm.ComponentCount - 1 downto 0 do
        begin
          if (aForm.Components[iCounter2] is TADOTable) then
          begin
            Z := aForm.Components[iCounter2] as TADOTable;
            // Schritt 3: Verbinde diese neu (am besten Rekursiv).
            if Z.MasterSource = Y then
              ReaktiviereVerbindung(Z, aForm);
          end;
        end;
      end;
    end;
  end;
end;

function ThlDatenbank.DbOwnerSid: string;
begin
  // Achtung! "sa" Benutzer hat SID 0x01
  Result := GetScalar
    ('select CONVERT([varchar](100), owner_sid, 1) from sys.databases where name = '
    + Datenbankname.toSQLString).AsWideString;
end;

function ThlDatenbank.SqlServerMac: string;
begin
  ExecSql('IF object_id(''SeqId'', ''P'') IS NOT NULL DROP PROCEDURE [dbo].[SeqId];');

  ExecSql('CREATE PROCEDURE SeqId ' + 'AS ' + 'begin ' + '  declare @t table ' +
    '    ( ' + '    i uniqueidentifier default newsequentialid(), ' +
    '    m as cast(i as char(36)) ' + '    ) ' + ' ' +
    '    insert into @t default values; ' + ' ' + '    select m FROM @t '
    + 'end;');

  Result := Copy(GetScalar('exec SeqId').AsWideString, 25, 12);

  ExecSql('DROP PROCEDURE [dbo].[SeqId];');
end;

class function ThlDatenbank.DefaultCommandTimeout: integer;
begin
  Result := 300;
end;

procedure ThlDatenbank.DefragIndexes;
var
  q: ThlDataSet;
  SchemaName, TableName, IndexName: string;
begin
  q := GetTable('SELECT ' + '  s.name AS SchemaName, ' +
    '  t.name AS TableName, ' + '  i.name AS IndexName, ' +
    '  ips.index_type_desc AS IndexType, ' +
    '  ips.avg_fragmentation_in_percent ' +
    'FROM sys.dm_db_index_physical_stats (DB_ID(), NULL, NULL, NULL, ''DETAILED'') ips '
    + 'JOIN sys.tables t ON ips.object_id = t.object_id ' +
    'JOIN sys.schemas s ON t.schema_id = s.schema_id ' +
    'JOIN sys.indexes i ON ips.object_id = i.object_id AND ips.index_id = i.index_id '
    + 'WHERE ips.index_level = 0 ' + // 0=Leaf, 1=Intermediate, 2=Root
    'ORDER BY ips.avg_fragmentation_in_percent DESC');
  try
    while not q.Eof do
    begin
      SchemaName := q.FieldByName('SchemaName').AsWideString;
      TableName := q.FieldByName('TableName').AsWideString;
      IndexName := q.FieldByName('IndexName').AsWideString;

      if q.FieldByName('IndexType').AsWideString <> 'HASH' then
      begin
        if q.FieldByName('IndexType').AsWideString = 'HEAP' then
        begin
          ExecSql(Format('ALTER TABLE [%s].[%s] REBUILD;',
            [SchemaName, TableName]));
        end
        else
        begin
          if q.FieldByName('avg_fragmentation_in_percent').AsInteger > 30 then
            ExecSql(Format('ALTER INDEX [%s] ON [%s].[%s] REBUILD;',
              [IndexName, SchemaName, TableName]))
          else if q.FieldByName('avg_fragmentation_in_percent').AsInteger > 10
          then
            ExecSql(Format('ALTER INDEX [%s] ON [%s].[%s] REORGANIZE;',
              [IndexName, SchemaName, TableName]));
        end;
      end;

      ExecSql(Format('UPDATE STATISTICS [%s].[%s] WITH FULLSCAN;',
        [SchemaName, TableName]));

      q.Next;
    end;
  finally
    FreeAndNil(q);
  end;

  q := GetTable('SELECT s.name as SchemaName, v.name as TableName ' +
    'FROM sys.views v ' + 'JOIN sys.schemas s ON v.schema_id = s.schema_id;');
  try
    while not q.Eof do
    begin
      SchemaName := q.FieldByName('SchemaName').AsWideString;
      TableName := q.FieldByName('TableName').AsWideString;
      ExecSql(Format('sp_recompile ''[%s].[%s]'';', [SchemaName, TableName]));
      q.Next;
    end;
  finally
    FreeAndNil(q);
  end;

  ExecSql('DBCC FREEPROCCACHE;');
  // geht nicht ohne sysadmin Rechte, obwohl die Dokumentation sagt, dass man nur db_owner sein muss...
  //ExecSql('exec sp_updatestats;');
end;

destructor ThlDatenbank.Destroy;
begin
  if ownsConnection then
    mConnection.Connected := false;
  FreeAndNil(mCommand);
  if ownsConnection then
    FreeAndNil(mConnection);
  FreeAndNil(mScalarTable);
  inherited;
end;

class procedure ThlDatenbank.DropTable(aTableName: hlString;
  adoCon: TAdoConnection);
begin
  if ViewExists(aTableName, adoCon) then
  begin
    ExecSql('drop view ' + aTableName.toSQLObjectName, adoCon);
  end;
  if TableExists(aTableName, adoCon) then
  begin
    ExecSql('drop table ' + aTableName.toSQLObjectName, adoCon);
  end;
end;

procedure ThlDatenbank.BeginTransaction;
begin
  Inc(mTransaktionsTiefe);

{$IFDEF TransaktionsTiefeAutomatik}
  if mTransaktionsTiefe > 1 then
    exit;
{$ENDIF}
  mConnection.Connected := true;

  // Wir prüfen nicht, ob nicht schon eine Transaktion besteht, aus folgendem Grund:
  // 1. Sollte eine vorherige Transaktion nicht mittels Rollback im Fehlerfall beendet worden sein, dann wäre das erneute Öffnen einer Transaktion fatal, da vorherige inkonsistente Änderungen beim nächsten Commit gespeichert werden würden.
  // 2. Im Falle von verschachtelten Transaktionen durch unbedachte Verwendungen von Delphi Prozeduren, die selbst Transaktionen verwenden, soll die Delphi Prozedur beim Commit nicht die Daten der aufrufenden (unabgeschlossenen) Prozedur speichern.
  // -- DM 04.04.2016

  // if not mConnection.InTransaction then
  mConnection.BeginTrans;
end;

// Die Funktion ist nicht gut, da das SA-Passwort nicht preserved wird!
(*
  function ThlDatenbank.Clone: ThlDatenbank;
  begin
  result := ThlDatenbank.Create(mConnection.ConnectionString, mConnection.IsolationLevel=ilIsolated, mConnection.ConnectionTimeout);
  end;
*)

function ThlDatenbank.ColumnExists(aTableName, aColumnName: hlString): boolean;
begin
  Result := GetScalar('select count (*) from sys.columns where Name = N' +
    aColumnName.toSQLString + ' and Object_ID = Object_ID(N' +
    aTableName.toSQLString + ')').AsInteger > 0;
end;

procedure ThlDatenbank.DropColumn(aTableName, aColumnName: hlString);
begin
  if ColumnExists(aTableName, aColumnName) then
  begin
    ExecSql('alter table ' + aTableName.toSQLObjectName + ' drop column ' +
      aColumnName.toString);
  end;
end;

function ThlDatenbank.IndexExists(aTableName, aIndexName: hlString): boolean;
begin
  Result := GetScalar('select count (*) from sys.indexes where name = N' +
    aIndexName.toSQLString + ' and Object_ID = Object_ID(N' +
    aTableName.toSQLString + ')').AsInteger > 0;
end;

procedure ThlDatenbank.Commit;
begin
  Dec(mTransaktionsTiefe);

{$IFDEF TransaktionsTiefeAutomatik}
  if mTransaktionsTiefe > 0 then
    exit;
{$ENDIF}
  if mConnection.InTransaction then
    mConnection.CommitTrans;
end;

procedure ThlDatenbank.Rollback;
begin
  Dec(mTransaktionsTiefe);

{$IFDEF TransaktionsTiefeAutomatik}
  if mTransaktionsTiefe > 0 then
    exit;
{$ENDIF}
  if mConnection.InTransaction then
    mConnection.RollbackTrans;
end;

class procedure ThlDatenbank.ExecSql(sQuery: string; aCon: TAdoConnection);
var
  command: TAdoCommand;
  OK: boolean;
begin
  try
    // aCon.Execute(sQuery);

    // Wir machen das über einen TADOCommand, weil wir da den ParamCheck haben
    command := TAdoCommand.Create(nil);
    try
      command.Connection := aCon;
      command.ParamCheck := false;
      command.CommandText := sQuery;
      if Assigned(Debug) then
        _TimerStart;

      OK := false;
      repeat
        try
          command.Execute;
          OK := true;
        except
          on E: EAbort do
          begin
            Abort;
          end;
          on E: Exception do
          begin
            if (Pos('Warten Sie, bis die Wiederherstellung beendet ist',
              E.Message) >= 1) or
              (Pos('Waiting until recovery is finished', E.Message) >= 1) then
            begin
              // "Die LUTZ_1-Datenbank wird wiederhergestellt. Warten Sie, bis die Wiederherstellung beendet ist"
              // oder "Database 'LUTZ_1' is being recovered. Waiting until recovery is finished"
              Sleep(5000);
            end
            else
            begin
              raise Exception.CreateFmt(StrFehlerSBeimAusfü,
                [E.Message, sQuery]);
            end;
          end;
        end;
      until OK;

      if Assigned(Debug) then
        Debug(sQuery, _TimerEnd);
    finally
      FreeAndNil(command);
    end;
  except
    on E: EAbort do
    begin
      if aCon.InTransaction then
        aCon.RollbackTrans;
      Abort;
    end;
    on E: Exception do
    begin
      // Ticket 29825, Punkt 2a, Szenario 1
      // Ticket 29729, Punkt 1
      if aCon.InTransaction then
        aCon.RollbackTrans;
      raise Exception.CreateFmt(StrFehlerSBeimAusfü, [E.Message, sQuery]);
    end;
  end;
end;

procedure ThlDatenbank.ExecSql(sql: string = '');
begin
  // Connection.Execute(sql);
  ExecSql(DefaultCommandTimeout, sql);

  if Assigned(Application) and
    ((UpperCase(ExtractFileName(ParamStr(0))) = 'CORA_AUTOARCHIVIERUNG.EXE') or
    (UpperCase(ExtractFileName(ParamStr(0))) = 'CORA_AUTOARCHIVIERUNG64.EXE'))
  then
  begin
    Application.ProcessMessages;
  end;
end;

procedure ThlDatenbank.ExecSql(timeout: integer; sql: string = '');
// var
// oldCommandTimeout: integer;
begin
  if not Assigned(mConnection) or not mConnection.Connected then
  begin
    raise Exception.CreateFmt(StrFolgendeSQLQueryK + #13#10#13#10 +
      '%s', [sql]);
  end;

  // oldCommandTimeout := mCommand.CommandTimeout;
  mCommand.CommandTimeout := timeout;

  // aCon.Execute(sql);
  mCommand.ParamCheck := false;
  mCommand.CommandText := sql;
  try
    mCommand.ParamCheck := false;
    if Assigned(Debug) then
      _TimerStart;
    mCommand.Execute;
    if Assigned(Debug) then
      Debug(sql, _TimerEnd);
  except
    on E: EAbort do
    begin
      Abort;
    end;
    on E: Exception do
    begin
      // Ticket 29825, Punkt 2a, Szenario 1
      // Ticket 29729, Punkt 1
      with ThlLog.Create do
      begin
        Write(Format(StrQueryFEHLGESCHLAGEN, [E.Message, sql]));
        Free;
      end;
      if mConnection.InTransaction then
        mConnection.RollbackTrans;
      raise Exception.CreateFmt(StrFehlerSBeimAusfü, [E.Message, sql]);
    end;
  end;

  // Hier passiert ein EXTREM seltsames Phänomen!!!
  // Bedingungen:
  // - Es wird SQL-Provider MSOLEDBSQL (Generation 3) oder SQLNCLI11 (Generation 2) ausgeführt
  // - FMTOnly ist "off" (default bei Gen2 und Gen3).
  // Bei FMTOnly=On funktionieren andere Dinge nicht, z.B. sind die Result-Sets alle leer?!
  // - Es wird ein Alter Table Statement verwendet
  // hclDBMandant.Datenbank.BeginTransaction;
  // hclDBMandant.Datenbank.ExecSQL('alter table [dbo].[DAUERAUFTRAG] add VORLAUFZEIT int;');
  // hclDBMandant.Datenbank.Rollback;
  // - Es wird eine Transaktion verwendet (nicht ganz sicher, ob das Pflicht ist)
  // - CommandTimeout wird nach Execute nochmal geändert
  // => Dann ruft er aus irgendeinem Grund Metadaten ab, und führt innerlich AlterTable erneut aus?
  // - Diese Änderung an CommandTimeout MUSS INNERHALB DES SELBEN FUNKTIONSAUFRUFS SEIN
  // Wenn die Funktion unmittelbar danach erneut aufgerufen wird (und selbst wenn Commandtimeout
  // der erste Befehl ist, dann macht dieses CommandTimeout keine Probleme?!?!?!
  // UND Execute muss ebenfalls in dieser Funktion drin sein!
  // Wenn das alles erfüllt ist, dann kommt die Meldung, dass die Spalte bereits vorhanden ist,
  // obwohl sie das nicht ist!
  // siehe https://stackoverflow.com/questions/63597112/setting-commandtimeout-after-execution-causes-error-column-appears-multiple-tim?noredirect=1#comment112462368_63597112
  // mCommand.CommandTimeout := tmp;
end;

function ThlDatenbank.Any(const sql: string): boolean;
var
  q: ThlDataSet;
begin
  q := GetTable(sql);
  try
    Result := q.RecordCount > 0;
  finally
    FreeAndNil(q);
  end;
end;

function ThlDatenbank.GetScalar(timeout: integer; sql: string)
  : ThlDatenbankFeld;
var
  ds: ThlDataSet;
begin
  if sql = '' then
    raise Exception.Create(StrGetScalarMitLeerem);
  ds := GetTable(timeout, sql);
  try
    if ds.RecordCount = 0 then
    begin
      Result := ThlDatenbankFeld.Create('');
      Result.mIsNull := true;
    end
    else
      Result := ThlDatenbankFeld.Create(ds.Fields[0]);
  finally
    FreeAndNil(ds);
  end;
end;

function ThlDatenbank.GetScalar(sql: string): ThlDatenbankFeld;
begin
  if sql = '' then
    raise Exception.Create(StrGetScalarMitLeerem);
  Result := GetScalar(DefaultCommandTimeout, sql);
end;

function ThlDatenbank.GetTable(sql: string = '';
  controlsEnabled: boolean = false): ThlDataSet;
begin
  Result := GetTable(DefaultCommandTimeout, sql, controlsEnabled);
end;

class procedure ThlDatenbank.MakeActiveTryReconnect(q: TCustomADODataSet;
  active: boolean = true);
begin
  try
    q.active := active;
  except
    on E: EAbort do
    begin
      Abort;
    end;
    on E: Exception do
    begin
      if Pos('[DBNETLIB][Connection', E.Message) > 0 then
      begin
        q.Connection.Connected := false;
        q.Connection.Connected := true;
        q.active := active;
      end
      else
      begin
        raise Exception.CreateFmt(StrFehlerSBeimSQLV,
          [E.Message, 'MakeActiveTryReconnect']);
      end;
    end;
  end;
end;

function ThlDatenbank.GetTable(timeout: integer; sql: string = '';
  controlsEnabled: boolean = false): ThlDataSet;
begin
  Result := ThlDataSet(CreateNewADODataSet);
  Result.ParamCheck := false;
  Result.CommandText := sql;
  Result.CommandTimeout := timeout;
  if Assigned(Debug) then
    _TimerStart;
  MakeActiveTryReconnect(Result, Trim(sql) <> '');
  if Assigned(Debug) then
    Debug(sql, _TimerEnd);
  if not controlsEnabled then
  begin
    Result.DisableControls;
    // Macht Iterationen viel schneller, *auch wenn keine GUI dran hängt*!
  end;
  // Kommentar von RH (ursprünglich in hcl_LohnmostKumulierung.pas)
  // "SCh.... Delphi: Das "q.first" muss bleiben, da sonst SPORADISCH der Cursor auf BOF steht und nicht auf dem 1. Datensatz!
  // Falls dies der Fall ist, knallts es unten, da die Instanz "q" keinen sauberen Cursor zurückliefert!"
  // Zur Sicherheit bauen wir daher .First für alle GetTable() Aufrufe ein.
  if Result.active then
    // DM 07.06.2021 : If-Condition hinzugefügt, weil man beim GoBD Export beispielsweise "GetTable" mit sql='' aufruft, um ein leeres Dataset zu bekommen
    Result.First;
end;

function ThlDatenbank.InsertAndReturnID(query: string): integer;
var
  q1: ThlDataSet;
begin
  mConnection.BeginTrans;
  try
    ExecSql(query); // mConnection.Execute(query);

    // Hinweis: @@IDENTITY geht nur lokal, nicht über linked Servers
    // q1 := GetTable('select @@IDENTITY as INSERT_ID;');
    q1 := GetTable('select SCOPE_IDENTITY() as INSERT_ID;');
    try
      if (q1.RecordCount = 0) or
        (q1.{ FieldByName('INSERT_ID') } Fields[0].AsInteger = 0) then
      begin
        Result := -1;
        raise EHSCannotGetAutoInc.Create(StrAutoIncFehler);
      end;

      Result := q1.{ FieldByName('INSERT_ID') } Fields[0].AsInteger;
    finally
      FreeAndNil(q1);
    end;
  finally
    mConnection.CommitTrans;
  end;
end;

function ThlDatenbank.InTransaction: boolean;
begin
  Result := Transaktionstiefe > 0;
end;

function ThlDatenbank.IstExpressEdition: boolean;
begin
  Result := (Copy(GetScalar('select serverproperty(''Edition'')').AsWideString, 1,
    Length('Express')) = 'Express') or
    (AnsiPos('Express Edition', GetScalar('select @@version').AsWideString) > 0);
end;

procedure ThlDatenbank.DropTable(aTableName: string);
begin
  DropTable(aTableName, mConnection);
end;

function ThlDatenbank.FieldCount(aTableName: string): integer;
begin
  Result := GetScalar
    ('select count (*) from syscolumns where id = (select id from sysobjects where name = '''
    + aTableName + ''') ').AsInteger;
end;

function ThlDatenbank.IndexCount(aTableName: string): integer;
begin
  Result := GetScalar
    ('select max (ik.indid) from sysindexkeys ik left join sysindexes ind on ind.id = ik.id and ind.indid = ik.indid where ik.id = (select id from sysobjects '
    + 'where name = ''' + aTableName + ''') and ind.status < 10000000')
    .AsInteger;
end;

class function ThlDatenbank.FileToStringlist(aFileName: string;
  aList: Tstringlist): integer;
var
  ListFile: Tstringlist;
  BefehlsCount: integer;
  Buffer: string;

begin
  aList.Clear;
  ListFile := Tstringlist.Create;
  try
    ListFile.LoadFromFile(aFileName);
    BefehlsCount := 0;
    while ListFile.Count > 0 do
    begin
      Buffer := GetBefehlszeile(ListFile);
      aList.Add(Buffer);
      BefehlsCount := BefehlsCount + 1;
    end;
  finally
    FreeAndNil(ListFile);
  end;
  Result := BefehlsCount;
end;

class function ThlDatenbank.ReorgExecList(aList: TStrings): integer;
var
  ListFile: Tstringlist;
  BefehlsCount: integer;
  Buffer: string;

begin
  ListFile := Tstringlist.Create;
  try
    ListFile.Assign(aList);
    aList.Clear;

    BefehlsCount := 0;
    while ListFile.Count > 0 do
    begin
      Buffer := GetBefehlszeile(ListFile);
      aList.Add(Buffer);
      Inc(BefehlsCount);
    end;
  finally
    FreeAndNil(ListFile);
  end;

  Result := BefehlsCount;
end;

class function ThlDatenbank.GetBefehlszeile(ListFile: Tstringlist): string;
var
  Buffer: string;
  posi: integer;

begin
  Result := '';
  while ListFile.Count > 0 do
  begin
    posi := Pos(';', ListFile[0]);
    if posi <= 0 then
      posi := MaxChars;
    Buffer := Copy(ListFile[0], 1, posi);
    Buffer := StringReplace(Buffer, #9, '', [rfReplaceAll]);
    if Result <> '' then
      Result := Result + ' ' + Buffer
    else
      Result := Buffer;
    Buffer := Copy(ListFile[0], posi, MaxChars);
    ListFile.Delete(0);
    if posi < MaxChars then
      break;
  end;
end;

function ThlDatenbank.GetCommandTimeout: integer;
begin
  Result := mConnection.CommandTimeout;
end;

function ThlDatenbank.GetConnectionID: TGuid;
var
  s: string;

begin
  s := GetScalar('select connection_id from sys.dm_exec_connections ec ' +
    'left join sys.dm_exec_sessions se on ec.session_id = se.session_id ' +
    'where se.session_id = @@SPID').AsWideString;

  Result := StringToGUID(s);
end;

function ThlDatenbank.GetDatenbankName: hlString;
begin
  // Hinweis: Das funktioniert auch, wenn man mit "use" eine andere Datenbank selektiert hat.
  Result := mConnection.DefaultDatabase;
end;

function ThlDatenbank.ExecSqlList(aList: TStrings;
  mode: TExecSQLListMode = lmFehlerAnzeigenUndAbbrechen;
  WithTransaction: boolean = false; timeout: integer = 600;
  slFehler: TStrings = nil): boolean;
var
  i: integer;
  line: string;
begin
  if (mode in [lmFehlerIgnorieren, lmFehlerAnzeigenUndWeitermachen]) and WithTransaction
  then
  begin
    raise Exception.Create(StrInternerFehlerInS);
  end;

  if (WithTransaction = true) then
    BeginTransaction;

  if mode in [lmFehlerIgnorieren, lmFehlerAnzeigenUndWeitermachen] then
  begin
    Result := true;
    for i := 0 to aList.Count - 1 do
    begin
      try
        line := aList[i];
        ErweitereSQLZeile(line);
        if Trim(line) <> '' then
        begin
          try
            ExecSql(timeout, line);
          except
            on E: EAbort do
            begin
              Abort;
            end;
            on E: Exception do
            begin
              raise Exception.CreateFmt(StrExceptionSAtQuer, [E.Message, line]);
            end;
          end;
        end;
      except
        on E: EAbort do
        begin
          Abort;
        end;
        on E: Exception do
        begin
          Result := false;
          if WithTransaction then
          begin
            Commit; // Wir führen den Commit früher aus, damit die Transaktion nicht unnötig lange läuft während der User die Fehlermeldung liest
            WithTransaction := false; // nicht nochmal comitten
          end;
          if mode = lmFehlerAnzeigenUndWeitermachen then
          begin
            ThlExceptionHandler.ErstelleStackTrace(E);
            TMessageBox.ZeigeException(E);
          end;
          if Assigned(slFehler) then
            slFehler.Add(E.ClassName + ': ' + E.Message);
        end;
      end;
    end;
  end
  else
  begin
    try
      for i := 0 to aList.Count - 1 do
      begin
        line := aList[i];
        ErweitereSQLZeile(line);
        if Trim(line) <> '' then
        begin
          try
            ExecSql(timeout, line);
          except
            on E: EAbort do
            begin
              Abort;
            end;
            on E: Exception do
            begin
              raise Exception.CreateFmt(StrExceptionSAtQuer, [E.Message, line]);
            end;
          end;
        end;
      end;
      Result := true;
    except
      on E: EAbort do
      begin
        Result := false;
        if WithTransaction then
        begin
          Rollback;
          WithTransaction := false; // nicht comitten
        end;
        Abort;
      end;
      on E: Exception do
      begin
        Result := false;
        if WithTransaction then
        begin
          Rollback;
          WithTransaction := false; // nicht comitten
        end;
        if mode = lmFehlerAnzeigenUndAbbrechen then
        begin
          ThlExceptionHandler.ErstelleStackTrace(E);
          TMessageBox.ZeigeException(E);
        end;
        if Assigned(slFehler) then
          slFehler.Add(E.ClassName + ': ' + E.Message);
        if mode = lmExceptionWerfen then
        begin
          raise Exception.CreateFmt(StrFehlerSBeimAusfüTransaktion,
            [E.Message]);
        end;
      end;
    end;
  end;

  if WithTransaction then
    Commit;
end;

procedure ThlDatenbank.GetTableNames(List: TStrings;
  SystemTables: boolean = false);
begin
  mConnection.GetTableNames(List, SystemTables);
end;

class procedure ThlDatenbank.ttRefresh(aTable: TDataset);
var
  wasActive: boolean;
begin
  wasActive := aTable.active;
  if wasActive = true then
  begin
    aTable.active := false; // MakeActiveTryReconnect(aTable, False);
    aTable.active := true; // MakeActiveTryReconnect(aTable, True);
  end;
end;

class function ThlDatenbank.ViewExists(aViewName: hlString;
  adoCon: TAdoConnection): boolean;
begin
  Result := GetScalar('select count (*) from sys.views where name = N' +
    aViewName.toSQLString + ' and type = ''V''', adoCon).AsInteger > 0;
end;

function ThlDatenbank.ViewExists(aViewName: hlString): boolean;
begin
  Result := ViewExists(aViewName, mConnection);
end;

constructor ThlDatenbank.Create(ADOConnection: TAdoConnection);
begin
  inherited Create;

  mConnection := ADOConnection;

  mCommand := TAdoCommand.Create(nil);

{$IFDEF UseBetterADO}
  mScalarTable := ThlDataSet(TBetterADODataSet.Create(nil));
{$ELSE}
  mScalarTable := ThlDataSet(TADODataSet.Create(nil));
{$ENDIF}
  mScalarTable.Connection := mConnection;

  (*
    mConnection.AfterConnect := ConnAfterConnect;
    mConnection.BeforeDisconnect := ConnBeforeDisconnect;
  *)

  mCommand.Connection := mConnection;

  ownsConnection := false;
end;

constructor ThlDatenbank.Create(ConnStr: string; Isolated: boolean = false;
  AConnectionTimeout: integer = 0);
begin
  inherited Create;
  CreateIndiv(ConnStr, AConnectionTimeout);
  if (Isolated) then
    mConnection.IsolationLevel := ilIsolated;
end;

procedure ThlDatenbank.CreateIndiv(ConnStr: string;
  AConnectionTimeout: integer = 0);
begin
  (*
    So funktionierts 100%ig:

    AdoCon.ConnectionString := 'Provider='+SqlServerProvider+';Password=achtung;Persist Security Info=True;User ID=hickelsoft;' +
    'Initial Catalog=FiVe;Data Source=SERVER;Use Procedure for Prepare=1;Auto Translate=True;Packet Size=4096;'+
    'Workstation ID=LAPTOP-2;Use Encryption for Data=False;Tag with column collation when possible=False';
  *)

  mConnection := TAdoConnection.Create(nil);
  try
    mConnection.ConnectionString := ConnStr;

    mConnection.KeepConnection := true;
    mConnection.LoginPrompt := false;

    mCommand := TAdoCommand.Create(nil);
{$IFDEF UseBetterADO}
    mScalarTable := ThlDataSet(TBetterADODataSet.Create(nil));
{$ELSE}
    mScalarTable := ThlDataSet(TADODataSet.Create(nil));
{$ENDIF}
    mScalarTable.Connection := mConnection;

    mConnection.CommandTimeout := ThlDatenbank.DefaultCommandTimeout;
    mConnection.ConnectionTimeout := AConnectionTimeout;

    mConnection.AfterConnect := ConnAfterConnect;
    mConnection.BeforeDisconnect := ConnBeforeDisconnect;

    mCommand.Connection := mConnection;

    mConnection.Connected := true;

    ownsConnection := true;
  except
    on E: EAbort do
    begin
      Abort;
    end;
    on E: Exception do
    begin
      FreeAndNil(mConnection);
      FreeAndNil(mCommand);
      FreeAndNil(mScalarTable);
      raise Exception.CreateFmt(StrFehlerSBeimSQLV, [E.Message, 'CreateIndiv']);
    end;
  end;
end;

class function ThlDatenbank.SQKKommentareUmwandeln(sql: TStrings;
  subquery: boolean = false): string;
var
  i, p: integer;
  s: string;
begin
  if subquery then
  begin
    // ';' am Ende entfernen (da sonst Subqueries nicht richtig arbeiten)
    s := Trim(sql.Text);
    if Copy(s, Length(s), 1) = ';' then
    begin
      s := Copy(s, 1, Length(s) - 1);
    end;
    sql.Text := s;
  end;

  for i := 0 to sql.Count - 1 do
  begin
    // TODO: Folgende Konstellation funktioniert nicht:    "-- /*   [Neue Zeile]   */"
    // Denn es wird dann zu "/* /* */  [Neue Zeile]  */"
    p := Pos('--', sql.Strings[i]);

    if p > 0 then
    begin
      s := sql.Strings[i];
      s[p] := '/';
      s[p + 1] := '*';
      s := s + ' */';

      sql.Strings[i] := s;
    end;
  end;
end;

{$REGION 'RowLock-Funktionen'}
(*
  function THlDatenbank.NewLock(AUsername, ATableName, APK1, APK2, APK3, APK4, APK5, APK6: hlString): ThclRowLock;
  begin
  result := ThclRowLock.Create(Self, AUsername, ATableName, APK1, APK2, APK3, APK4, APK5, APK6);
  end;

  function THlDatenbank.NewLockSpecial(AUsername, AModule, AIdentifier: hlString): ThclRowLock;
  begin
  result := ThclRowLock.CreateSpecial(Self, AUsername, AModule, AIdentifier);
  end;

  procedure THlDatenbank.RemoveAllLocks;
  begin
  ThclRowLock.RemoveAllLocks(Self);
  end;

  procedure THlDatenbank.RemoveAllUserLocks(AUsername: hlString);
  begin
  ThclRowLock.RemoveAllUserLocks(Self, AUsername);
  end;

  procedure THlDatenbank.RemoveInvalidLocks;
  begin
  ThclRowLock.RemoveInvalidLocks(Self);
  end;
*)

{$ENDREGION}

function ConnStrReadAttr(attr, ConnStr: string): string;
var
  p, p2: integer;
begin
  // connStr:      123456789
  // ..;abc=..
  // ';'+connstr:  123456789
  // ;..;abc=..
  p := Pos(';' + attr + '=', ';' + ConnStr);
  if p = 0 then
  begin
    Result := '';
    exit;
  end;
  p2 := Pos(';', Copy(ConnStr, p) + ';');
  Result := Copy(ConnStr, p + Length(attr) + 1, p2 - Length(attr) - 2);
end;

function ConnStrWriteAttr(attr, val, ConnStr: string): string;
var
  tmp: string;
  len_tmp: integer;
begin
  // 1. Alle Vorkommen von attr entf.
  Result := ConnStr;
  repeat
    len_tmp := Length(Result);
    tmp := ConnStrReadAttr(attr, Result);
    Result := ';' + StringReplace(Result, ';' + attr + '=' + tmp, '',
      [rfIgnoreCase]);
    Result := Copy(Result, 2, Length(Result) - 1);
    // ';' wieder entf. (war nur für das Ersetzen wichtig)
  until len_tmp = Length(Result);
  // abbrechen wenn nix mehr ersetzt (oder nix gefunden)
  // 2. Nun attr. mit neuem Value wieder hinzufügen (Provider muss an den Anfang)
  if SameText(attr, 'Provider') then
    Result := attr + '=' + val + ';' + Result
  else
    Result := Result + ';' + attr + '=' + val;
  // 3. ';;' sorgen eventuell dafür dass irgendwann ';;;' rauskommt. Normalisieren auf ';'
  repeat
    len_tmp := Length(Result);
    Result := StringReplace(Result, ';;', ';', [rfReplaceAll]);
  until len_tmp = Length(Result);
end;

initialization

// Wird verwendet für ADO DB
CoInitialize(nil); // steht in uses:ActiveX

finalization

CoUninitialize;

end.
