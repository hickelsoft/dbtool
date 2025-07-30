unit hl.Datenbank.RowLock;

interface

uses
  SysUtils, hl.System.Types, hl_Datenbank, hl_ExceptionLogger;

const
  // in Millisekunden. -1 = deaktiviert
  HSROWLOCK_DEFAULT_RECHECK_TIMER = 200;

type
  EHSDatasetLockedByOtherUser = class(EHlBedienfehler);

type
  // Achtung: Das ist der HL-RowLock . Er unterscheidet sich zum HCL-RowLock insofern,
  // dass er allgemeiner gefasst ist, und nicht an CORA gebunden ist. Dafür entfallen
  // allerdings CORA-spezifische Besonderheiten!
  ThlRowLock = record
  strict private
    _RecheckTimer: integer;
    _LockLFD: integer;

    _hlDB: ThlDatenbank;

    _Username: hlString;
    _Computername: hlString;
    _TableName: hlString;
    _PK1: hlString;
    _PK2: hlString;
    _PK3: hlString;
    _PK4: hlString;
    _PK5: hlString;
    _PK6: hlString;

    _LockedByOtherUser: boolean;
    _LockedBy: hlString;
    _LockedOnPC: hlString;
    _LockedSince: TDateTime;

    procedure DeleteLock;
    function LockEintragen: integer;
    procedure ResetResults;

    /// <summary>
    /// Prüft, ob jemand die Sperre belegt hat und trägt das Ergebnis in die Felder LockedByOtherUser, LockedBy und LockedSince eingetragen.
    /// </summary>
    /// <remarks>ACHTUNG! LockedByOtherUser wird gesetzt, auch wenn der Datensatz uns gehört. Muss später bearbeitet werden.</remarks>
    procedure CheckLock;
  public

{$REGION 'Constructor'}
    constructor Create(aHlDB: ThlDatenbank; AUserName, ATableName, APK1, APK2,
      APK3, APK4, APK5, APK6: hlString); overload;

    /// <summary>Dieser Konstruktor wird bei Sperrobjekten verwendet, die keine Referenz auf eine Tabelle haben.</summary>
    constructor CreateSpecial(aHlDB: ThlDatenbank;
      AUserName, AModule, AIdentifier: hlString); overload;
{$ENDREGION}
{$REGION 'Ergebnis'}
    /// <summary>
    /// Ist true, wenn der Datensatz durch einen anderen Benutzer gesperrt ist.
    /// Ist gesetzt nach dem Aufruf von ApplyLock bzw. des Konstruktors mit dem Argument AAutomaticApply = true.
    /// </summary>
    property LockedByOtherUser: boolean read _LockedByOtherUser;

    /// <summary>
    /// Gibt den Namen des Benutzers an, der den Datensatz gerade bearbeitet.
    /// Ist gesetzt nach dem Aufruf von ApplyLock bzw. des Konstruktors mit dem Argument AAutomaticApply = true.
    /// Der Wert sollte nur verwendet werden, wenn LockedByOtherUser = true ist.
    /// </summary>
    property LockedBy: hlString read _LockedBy;

    /// <summary>
    /// Gibt den Namen des Computers an, der den Datensatz gerade bearbeitet.
    /// Ist gesetzt nach dem Aufruf von ApplyLock bzw. des Konstruktors mit dem Argument AAutomaticApply = true.
    /// Der Wert sollte nur verwendet werden, wenn LockedByOtherUser = true ist.
    /// </summary>
    property LockedOnPC: hlString read _LockedOnPC;

    /// <summary>
    /// Gibt die Uhrzeit der Sperrung des Datensatzes aus.
    /// Ist gesetzt nach dem Aufruf von ApplyLock bzw. des Konstruktors mit dem Argument AAutomaticApply = true.
    /// Der Wert sollte nur verwendet werden, wenn LockedByOtherUser = true ist.
    /// </summary>
    property LockedSince: TDateTime read _LockedSince;
{$ENDREGION}
{$REGION 'Eingabeparameter'}
    // Eingabeparameter
    property Username: hlString read _Username;
    property Computername: hlString read _Computername;
    property TableName: hlString read _TableName;
    property PK1: hlString read _PK1;
    property PK2: hlString read _PK2;
    property PK3: hlString read _PK3;
    property PK4: hlString read _PK4;
    property PK5: hlString read _PK5;
    property PK6: hlString read _PK6;
    // property LockLFD: integer read FLockLFD;
    property RecheckTimer: integer read _RecheckTimer write _RecheckTimer;
{$ENDREGION}
    /// <summary>
    /// Sperrt den Datensatz. Dies kann durchgeführt werden, wenn zuvor ein
    /// ReleaseLock stattgefunden hat oder wenn ein der Konstruktor ohne
    /// AAutomaticApply aufgerufen wurde.
    /// Achtung! Wirft keine Exception! Um eine Exception anzuzeigen,
    /// muss RaiseExceptionIfLocked aufgerufen werden.
    /// </summary>
    procedure ApplyLock(AAutomaticException: boolean = true;
      AAllowOwnUser: boolean = false);

    /// <summary>
    /// Gibt die Datensatzsperre frei. Das Objekt kann weiterverwendet werden.
    /// Mit ApplyLock kann die Sperre wieder angelegt werden,
    /// ohne Neuinitialisierung des Objekts.
    /// </summary>
    procedure ReleaseLock;

    /// <summary>
    /// Prüft ob die Eigenschaft LockedByOtherUser wahr ist und gibt in diesem
    /// Falle eine Exception des Typs EHSDatasetLockedByOtherUser aus, die den
    /// Benutzernamen LockedBy und das Sperrdatum LockedSince enthält.
    /// </summary>
    procedure RaiseExceptionIfLocked;

    /// <summary>
    /// Diese Funktion löscht alle Sperren eines anzugebenen Benutzers.
    /// </summary>
    /// <param name="AUsername">Der Benutzername, von dem alle Locks entfernt werden sollen.</param>
    class procedure RemoveAllUserLocks(aHlDB: ThlDatenbank;
      AUserName: hlString); overload; static;
    procedure RemoveAllUserLocks(AUserName: hlString); overload;

    class procedure RemoveAllLocks(aHlDB: ThlDatenbank); overload; static;
    procedure RemoveAllLocks; overload;

    class procedure RemoveInvalidLocks(aHlDB: ThlDatenbank); overload; static;
    procedure RemoveInvalidLocks; overload;

    // Zu Debugzwecken in HS-Info 2.0
    property LockLfd: integer read _LockLFD;
  end;

implementation

uses
  hl.Utils;

resourcestring
  StrInternerFehlerEs = 'Interner Fehler! Es besteht bereits eine Datensatz-' +
    'Sperre (ID=%d)';
  StrInternerFehlerIN = 'Interner Fehler: "INSERT" in RowLock Tabelle hat ni' +
    'cht funktioniert!';
  StrDatasetLockedByOtherUser =
    'Der Datensatz ist seit %s durch den Benutzer %s an Arbeitsplatz %s gesperrt.'
    + #13#10#13#10 + 'Technische Information: %s';

  { ThlRowLock }

{$REGION 'Create and Destroy'}

constructor ThlRowLock.Create(aHlDB: ThlDatenbank;
  AUserName, ATableName, APK1, APK2, APK3, APK4, APK5, APK6: hlString);
begin
  _LockLFD := 0;

  _hlDB := aHlDB;

  _Username := AUserName;
  _Computername := ThlUtils.GetComputerName;
  _TableName := ATableName;
  _PK1 := APK1;
  _PK2 := APK2;
  _PK3 := APK3;
  _PK4 := APK4;
  _PK5 := APK5;
  _PK6 := APK6;
  _RecheckTimer := HSROWLOCK_DEFAULT_RECHECK_TIMER;

  _LockedByOtherUser := false;
  _LockedBy := '';
  _LockedSince := 0;
end;

constructor ThlRowLock.CreateSpecial(aHlDB: ThlDatenbank;
  AUserName, AModule, AIdentifier: hlString);
begin
  Create(aHlDB, AUserName, '---', AModule, AIdentifier, '', '', '', '');
end;
{$ENDREGION}

function ThlRowLock.LockEintragen: integer;
var
  query: string;
begin
  query := 'insert into DATENSATZSPERRE ' +
    '(TABLENAME, PK1, PK2, PK3, PK4, PK5, PK6, CONNECTION_ID, LOCKED_BY, LOCKED_ON_PC, LOCKED_SINCE) values '
    + '(' + TableName.toSQLString + ', ' + PK1.toSQLString + ', ' +
    PK2.toSQLString + ', ' + PK3.toSQLString + ', ' + PK4.toSQLString + ', ' +
    PK5.toSQLString + ', ' + PK6.toSQLString + ', ' + '''' +
    GUIDToString(_hlDB.ConnectionID) + ''', ' + Username.toSQLString + ', ' +
    Computername.toSQLString + ', GETDATE());';
  result := _hlDB.InsertAndReturnID(query);
end;

procedure ThlRowLock.ApplyLock(AAutomaticException: boolean = true;
  AAllowOwnUser: boolean = false);
var
  q1: ThlDataSet;
  LowestInsertID: integer;
  query: string;
begin
  // Wenn das übertrieben oft aufgerufen wird, wird CORA langsam (Ticket 59369, 59374, 59381)
  // Da der Rowlock aber unbedingt eine gültige Verbindung braucht, ist es OK, zumal Rowlocks nicht
  // übermäßig verwendet werden sollen, aufgrund der Performance
  _hlDB.RecheckConnectionStatus(nil);

  try
    if _LockLFD > 0 then
    begin
      // Zwei mal hintereinander ApplyLock aufrufen ist nix gut
      raise Exception.CreateFmt(StrInternerFehlerEs, [_LockLFD]);
    end;

    CheckLock;

    if LockedByOtherUser then
    begin
      // Prüfen: Sind WIR der "Other user"?
      if (LockedBy = Username) and (LockedOnPC = Computername) and AAllowOwnUser
      then
      begin
        // Der Datensatz ist von uns gesperrt. Wir lassen den Zugriff zu.
        _LockedByOtherUser := false;
        // Wir fügen einen weiteren Eintrag in die Hilfstabelle ein, damit wir eine eigene _LockLFD bekommen
        _LockLFD := LockEintragen;
      end
      else
      begin
        // Datensatz ist von jemand anderes gesperrt.
        // _LockLFD = 0, damit wir nicht bei einem versehentlichen Release deren Lock löschen
        _LockLFD := 0;
      end;
      Exit;
    end;

    // Datensatz ist nicht gesperrt. Wir propagieren nun den Lock in der Hilfstabelle.
    _LockLFD := LockEintragen;

    // Double Check: Wir prüfen nun, ob wir auch der Erste waren
    query := 'select top 1 dsp.LFD, dsp.LOCKED_BY, dsp.LOCKED_ON_PC, dsp.LOCKED_SINCE from DATENSATZSPERRE dsp '
      + 'left join sys.dm_exec_connections sp ' +
      '  on sp.connection_id = dsp.CONNECTION_ID ' + 'where dsp.TABLENAME = ' +
      TableName.toSQLString + '  and dsp.CONNECTION_ID is not null ' +
      '  and sp.connection_id is not null ' + '  and dsp.LOCKED_BY = ' +
      Username.toSQLString + '  and dsp.LOCKED_ON_PC = ' +
      Computername.toSQLString;
    if PK1 <> '' then
      query := query + ' and dsp.PK1 = ' + PK1.toSQLString;
    if PK2 <> '' then
      query := query + ' and dsp.PK2 = ' + PK2.toSQLString;
    if PK3 <> '' then
      query := query + ' and dsp.PK3 = ' + PK3.toSQLString;
    if PK4 <> '' then
      query := query + ' and dsp.PK4 = ' + PK4.toSQLString;
    if PK5 <> '' then
      query := query + ' and dsp.PK5 = ' + PK5.toSQLString;
    if PK6 <> '' then
      query := query + ' and dsp.PK6 = ' + PK6.toSQLString;
    query := query + ' order by dsp.LFD asc;';
    q1 := _hlDB.GetTable(query);
    try
      LowestInsertID := q1.{ FieldByName('LFD') } Fields[0].AsInteger;
      if LowestInsertID = 0 then
      begin
        raise Exception.Create(StrInternerFehlerIN);
      end;
      if _LockLFD <> LowestInsertID then
      begin
        // Jemand anderes hat zur selben Zeit gelocked und hatte sich schneller
        // in die Hilfstabelle eingetragen. Wir haben den Lock doch nicht erhalten.

        DeleteLock; // unsere Eintragung rückgängig machen

        _LockedByOtherUser := true;
        _LockedBy := q1.{ FieldByName('LOCKED_BY') } Fields[1].AsWideString;
        _LockedOnPC := q1.{ FieldByName('LOCKED_ON_PC') } Fields[2].AsWideString;
        _LockedSince := q1.{ FieldByName('LOCKED_SINCE') } Fields[3].AsDateTime;
        Exit;
      end;
    finally
      FreeAndNil(q1);
    end;

    // Wir lassen den Zugriff auf den Datensatz zu
    _LockedByOtherUser := false;
  finally
    if AAutomaticException then
      RaiseExceptionIfLocked;
  end;
end;

procedure ThlRowLock.ResetResults;
begin
  _LockedByOtherUser := false;
  _LockedBy := '';
  _LockedSince := 0;
  _LockLFD := 0;
end;

procedure ThlRowLock.CheckLock;
var
  q1: ThlDataSet;
  query: string;
begin
  query := 'select top 1 dsp.LFD, dsp.LOCKED_BY, dsp.LOCKED_ON_PC, dsp.LOCKED_SINCE from DATENSATZSPERRE dsp '
    + 'left join sys.dm_exec_connections sp ' +
    '  on sp.connection_id = dsp.CONNECTION_ID ' + 'where dsp.TABLENAME = ' +
    TableName.toSQLString + '  and dsp.CONNECTION_ID is not null ' +
    '  and sp.connection_id is not null ';
  if PK1 <> '' then
    query := query + ' and dsp.PK1 = ' + PK1.toSQLString;
  if PK2 <> '' then
    query := query + ' and dsp.PK2 = ' + PK2.toSQLString;
  if PK3 <> '' then
    query := query + ' and dsp.PK3 = ' + PK3.toSQLString;
  if PK4 <> '' then
    query := query + ' and dsp.PK4 = ' + PK4.toSQLString;
  if PK5 <> '' then
    query := query + ' and dsp.PK5 = ' + PK5.toSQLString;
  if PK6 <> '' then
    query := query + ' and dsp.PK6 = ' + PK6.toSQLString;
  query := query + ' order by dsp.LFD asc;';
  q1 := _hlDB.GetTable(query);
  try
    if q1.RecordCount = 0 then
    begin
      // Es besteht keine Sperre
      ResetResults;
      Exit;
    end;

    // Ggf. warten und dann nochmal prüfen
    if _RecheckTimer <> -1 then
    begin
      Sleep(_RecheckTimer);
      FreeAndNil(q1);
      q1 := _hlDB.GetTable(query);
      if q1.RecordCount = 0 then
      begin
        // Die Sperre ist in der Zwischenzeit aufgehoben worden
        ResetResults;
        Exit;
      end;
    end;

    // Der Datensatz ist gesperrt
    _LockedByOtherUser := true;
    _LockLFD := q1.{ FieldByName('LFD') } Fields[0].AsInteger;
    _LockedBy := q1.{ FieldByName('LOCKED_BY') } Fields[1].AsWideString;
    _LockedOnPC := q1.{ FieldByName('LOCKED_ON_PC') } Fields[2].AsWideString;
    _LockedSince := q1.{ FieldByName('LOCKED_SINCE') } Fields[3].AsDateTime;
  finally
    FreeAndNil(q1);
  end;
end;

procedure ThlRowLock.DeleteLock;
var
  query: string;
begin
  if _LockLFD > 0 then
  begin
    query := 'delete from DATENSATZSPERRE where LFD = ' +
      IntToStr(_LockLFD) + ';';
    _hlDB.ExecSql(query);
    _LockLFD := 0;
  end;
end;

class procedure ThlRowLock.RemoveInvalidLocks(aHlDB: ThlDatenbank);
begin
  aHlDB.ExecSql('delete DATENSATZSPERRE from DATENSATZSPERRE dsp ' +
    'left join sys.dm_exec_connections sp ' +
    '  on sp.connection_id = dsp.CONNECTION_ID ' +
    'where dsp.CONNECTION_ID is null ' + '   or sp.connection_id is null;');
end;

procedure ThlRowLock.RemoveInvalidLocks;
begin
  RemoveInvalidLocks(_hlDB);
end;

procedure ThlRowLock.RaiseExceptionIfLocked;
begin
  if LockedByOtherUser then
  begin
    raise EHSDatasetLockedByOtherUser.CreateFmt(StrDatasetLockedByOtherUser,
      [DateTimeToStr(LockedSince), LockedBy.toString, LockedOnPC.toString,
      string(Self.TableName + ' : ' + Self.PK1 + '/' + Self.PK2 + '/' + Self.PK3
      + '/' + Self.PK4 + '/' + Self.PK5 + '/' + Self.PK6)]);
  end;
end;

procedure ThlRowLock.ReleaseLock;
begin
  DeleteLock;
end;

class procedure ThlRowLock.RemoveAllLocks(aHlDB: ThlDatenbank);
var
  query: string;
begin
  query := 'delete from DATENSATZSPERRE;';
  aHlDB.ExecSql(query);
end;

procedure ThlRowLock.RemoveAllLocks;
begin
  RemoveAllLocks(_hlDB);
end;

class procedure ThlRowLock.RemoveAllUserLocks(aHlDB: ThlDatenbank;
  AUserName: hlString);
var
  query: string;
begin
  query := 'delete from DATENSATZSPERRE where LOCKED_BY = ' +
    AUserName.toSQLString + ';';
  aHlDB.ExecSql(query);
end;

procedure ThlRowLock.RemoveAllUserLocks(AUserName: hlString);
begin
  RemoveAllUserLocks(_hlDB, AUserName);
end;

end.
