unit Main;

{$INCLUDE 'Globals.inc'}

interface

uses
  Windows, SysUtils, Classes, Controls, StdCtrls, Forms, ComCtrls, ToolWin, Menus, ActnList,
  ActnMan, ImgList, StdActns, ActnCtrls, ActnMenus, ExtCtrls, DdeMan{, XPMan}, Graphics,
  ShellAPI, Messages, System.ImageList, System.Actions;

type
  TDLG_Main = class(TForm)
    alStandard: TActionList;
    ilMenu: TImageList;
    FileOpen1: TFileOpen;
    Beenden1: TAction;
    FilePrintSetup1: TFilePrintSetup;
    DateiSeitenansicht1: TAction;
    EditCut1: TEditCut;
    EditCopy1: TEditCopy;
    EditPaste1: TEditPaste;
    WindowCascade1: TWindowCascade;
    WindowTileHorizontal1: TWindowTileHorizontal;
    WindowTileVertical1: TWindowTileVertical;
    WindowMinimizeAll1: TWindowMinimizeAll;
    WindowArrange1: TWindowArrange;
    MainMenu1: TMainMenu;
    Datei1: TMenuItem;
    Bearbeiten1: TMenuItem;
    Fenster1: TMenuItem;
    Hilfe1: TMenuItem;
    Extras1: TMenuItem;
    Info1: TMenuItem;
    berlappend1: TMenuItem;
    Untereinander1: TMenuItem;
    Nebeneinander1: TMenuItem;
    N1: TMenuItem;
    Anordnen1: TMenuItem;
    Alleverkleinern1: TMenuItem;
    Kopieren1: TMenuItem;
    Ausschneiden1: TMenuItem;
    Einfgen1: TMenuItem;
    HilfeInfo1: TAction;
    ExtrasOptionen1: TAction;
    Optionen1: TMenuItem;
    ffnen1: TMenuItem;
    N2: TMenuItem;
    Druckereinstellungen1: TMenuItem;
    Drucken1: TMenuItem;
    Seitenansicht1: TMenuItem;
    N3: TMenuItem;
    Beenden2: TMenuItem;
    DateiDrucken1: TAction;
    Panel1: TPanel;
    tbStandard: TToolBar;
    ToolButton1: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    FileOpenSqlServerDb: TAction;
    SQLServerDatenbankffnen1: TMenuItem;
    System: TDdeServerConv;
    DdeServerItem1: TDdeServerItem;
    BearbeitenSuchen1: TAction;
    BearbeitenWeitersuchen1: TAction;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    N4: TMenuItem;
    Suchen1: TMenuItem;
    Weitersuchen1: TMenuItem;
    ExtrasExport1: TAction;
    N5: TMenuItem;
    Export1: TMenuItem;
    ToolButton12: TToolButton;
    N6: TMenuItem;
    Speichern1: TMenuItem;
    ExtrasImport1: TAction;
    Import1: TMenuItem;
    DateiNeu1: TAction;
    ToolButton15: TToolButton;
    Neu1: TMenuItem;
    FileOpenMySqlDb: TAction;
    MySQLDatenbankffnen1: TMenuItem;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    N7: TMenuItem;
    Tabellenfenster1: TMenuItem;
    Datenbankfenster1: TMenuItem;
    AlleTabellenschlieen1: TMenuItem;
    StartUpTimer: TTimer;
    AlleAbfragefensterschlieen1: TMenuItem;
    Abfragefenster1: TMenuItem;
    N8: TMenuItem;
    N9: TMenuItem;
    N10: TMenuItem;
    Updates1: TMenuItem;
    procedure Beenden1Execute(Sender: TObject);
    procedure FileOpen1Accept(Sender: TObject);
    procedure HilfeInfo1Execute(Sender: TObject);
    procedure FileOpenSqlServerDbExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SystemExecuteMacro(Sender: TObject; Msg: TStrings);
    procedure BearbeitenSuchen1Execute(Sender: TObject);
    procedure BearbeitenWeitersuchen1Execute(Sender: TObject);
    procedure ExtrasOptionen1Execute(Sender: TObject);
    procedure ExtrasExport1Execute(Sender: TObject);
    procedure ExtrasImport1Execute(Sender: TObject);
    procedure DateiNeu1Execute(Sender: TObject);
    procedure FileOpenMySqlDbExecute(Sender: TObject);
    procedure Fenster1Click(Sender: TObject);
    procedure AlleTabellenschlieen1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure StartUpTimerTimer(Sender: TObject);
    procedure AlleAbfragefensterschlieen1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer;
      var Resize: Boolean);
    procedure Updates1Click(Sender: TObject);
  private
    procedure DynMenuClick(Sender: TObject);
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
  public
    procedure OpenFile(FileName: string);
  end;

var
  DLG_Main: TDLG_Main;

implementation

{$R *.DFM}

uses
  Globals, Database, Table, Info, Optionen, OpenSqlDb, OpenMySqlDb, Export, Import, DateiNeu,
  Registry, Query, hl.Utils.Color, ProgrDlg, hl.Utils, IniFiles,
  hl_Datenbank, HickelSOFT_Design, hl.System.ExceptionHandler, hl.Utils.Web,
  HS_Auth, hl_SqlServerProvider;

function GetHsInfo2ConnectionString: string;
var
  iniClient, iniServer: TMemIniFile;
  sServerPath: string;
  sSQLServerPath: string;
  sServerIni: string;
  sMode: string;
const
  CONFIG_INI = 'Config.ini';
  CONFIG_HEADER = 'HSINFO2';
resourcestring
  SSNotFound = '%s nicht gefunden';
  SModeIsNotS = '%s: Modus ist nicht "%s"';
  SSqlConnStrNotFoundInS = 'SQLConnStr innerhalb %s nicht gefunden';
  SInvalidModeInS = 'Ungültiger Client/Server Modus in %s';
begin
  // Bitte Synchron halten mit THsInfo2Kern.ConnectionString

  result := '';
  iniClient := TMemIniFile.Create(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))+CONFIG_INI);
  try
    sMode := iniClient.ReadString(CONFIG_HEADER, 'Mode', ''); // do not localize
    if sMode = 'Client' then // do not localize
    begin
      sSQLServerPath := iniClient.ReadString(CONFIG_HEADER, 'SQLConnStr', ''); // do not localize
      if sSQLServerPath <> '' then
      begin
        // Ein Client darf einen abweichenden SQL-Conn-Str haben, aber das ist eher eine Ausnahme!
        result := sSQLServerPath;
      end
      else
      begin
        sServerPath := iniClient.ReadString(CONFIG_HEADER, 'ServerPath', ''); // do not localize
        sServerIni := IncludeTrailingPathDelimiter(sServerPath)+CONFIG_INI;
        if not FileExists(sServerIni) then
        begin
          raise Exception.CreateFmt(SSNotFound, [sServerIni]);
        end;
        iniServer := TMemIniFile.Create(sServerIni);
        try
          if iniServer.ReadString(CONFIG_HEADER, 'Mode', '') <> 'Server' then // do not localize
          begin
            raise Exception.CreateFmt(SModeIsNotS, [sServerIni, 'Server']); // do not localize
          end;
          sSQLServerPath := iniServer.ReadString(CONFIG_HEADER, 'SQLConnStr', ''); // do not localize
          if sSQLServerPath = '' then
          begin
            raise Exception.CreateFmt(SSqlConnStrNotFoundInS, [sServerIni]);
          end;
          result := sSQLServerPath;
        finally
          FreeAndNil(iniServer);
        end;
      end;
    end
    else if sMode = 'Server' then // do not localize
    begin
      iniServer := TMemIniFile.Create(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))+CONFIG_INI);
      try
        sSQLServerPath := iniServer.ReadString(CONFIG_HEADER, 'SQLConnStr', ''); // do not localize
        result := sSQLServerPath;
      finally
        FreeAndNil(iniServer);
      end;
    end
    else
    begin
      raise Exception.CreateFmt(SInvalidModeInS, [CONFIG_INI]);
    end;
  finally
    FreeAndNil(iniClient);
  end;

  // Der Anwendungsname muss einheitlich sein, sonst kann die Anzahl gleichzeitiger Benutzer (für die Lizenz) nicht ermittelt werden
  result := StringReplace(result, 'Application Name', 'emaN noitacilppA', [rfReplaceAll, rfIgnoreCase]); // do not localize
  //result := result + ';Application Name=' + DB_APPLICATION_NAME;
  // Entfernt, da sonst DBTool zur Lizenzanzahl hinzuzählt

  // Geheimes HickelSOFT "sa" Passwort sollte nicht in der Config.ini stehen
  result := StringReplace(result, '???', HS_SA_DB_PASSWORD, []); // do not localize

  // Automatisch den besten SQL Client Treiber wählen
  result := StringReplace(result, 'Provider=(Auto)', 'Provider='+SqlServerProvider, []);

  if result = '' then
  begin
    raise Exception.Create('Konnte SQL-Connection-String nicht ermitteln');
  end;
end;

procedure TDLG_Main.Beenden1Execute(Sender: TObject);
begin
  Close;
end;

procedure TDLG_Main.FileOpen1Accept(Sender: TObject);
begin
  if Modus_CORA_Verzeichnis or Modus_HsInfo2_Verzeichnis then exit;
  while FileOpen1.Dialog.Files.Count > 0 do
  begin 
    OpenFile(FileOpen1.Dialog.Files.Strings[0]);
    FileOpen1.Dialog.Files.Delete(0);  
  end;
end;

procedure TDLG_Main.OpenFile(FileName: string);
var
  sExt, sPath: string;
  aDbFrm: TMDI_Database;
  i: integer;
begin
   // Datenbank oder Tabelle?
   sExt := AnsiUpperCase(ExtractFileExt(FileName));
   sPath := ExtractFilePath(FileName);

   // Lokale Tabelle: Ggf. Datenbank öffnen, Tabelle auf jeden Fall
   if (sExt = '.DBF') or (sExt = '.DB') then // do not localize
   begin
      // Prüfen: Ist die Datenbank schon offen?
      aDbFrm := nil;
      for i := 0 to MDIChildCount-1 do
      begin
        if MDIChildren[i].ClassNameIs('TMDI_Database') and // do not localize
           (TMDI_Database(MDIChildren[i]).dbDatabase.DatabaseName = sPath) then
        begin
          aDbFrm := TMDI_Database(MDIChildren[i]);
          MDI_Form_BringToFront(aDbFrm);
          break;
        end;
      end;

      // Wenn nicht, Datenbank neu öffnen!
      if not Assigned(aDbFrm) then
      begin
        aDbFrm := TMDI_Database.Create(Self, sPath);
      end;

      // Tabelle öffnen
      aDbFrm.OpenTable(ExtractFileName(FileName));
   end
   else
   begin
      // Prüfen: Ist die Datenbank schon offen?
      aDbFrm := nil;
      for i := 0 to MDIChildCount-1 do
      begin
        if MDIChildren[i].ClassNameIs('TMDI_Database') and // do not localize
          (TMDI_Database(MDIChildren[i]).dbDatabase.DatabaseName = FileName) then
        begin
          aDbFrm := TMDI_Database(MDIChildren[i]);
          MDI_Form_BringToFront(aDbFrm);
          break;
        end;
      end;

      // Wenn nicht, Datenbank neu öffnen!
      if not Assigned(aDbFrm) then {aDbFrm :=} TMDI_Database.Create(Self, FileName);
   end;
//   if(MDIChildCount == 2) ActiveMDIChild.WindowState = wsMaximized;
end;

procedure TDLG_Main.HilfeInfo1Execute(Sender: TObject);
var
  aDlg: TDLG_Info;
begin
  aDlg := TDLG_Info.Create(Self);
  try
    aDlg.ShowModal;
  finally
    FreeAndNil(aDlg);
  end;
end;

procedure TDLG_Main.FileOpenSqlServerDbExecute(Sender: TObject);
var
  aDlg: TDLG_OpenSqlDb;
begin
  aDlg := TDLG_OpenSqlDb.Create(Self);
  try
    if aDlg.ShowModal = mrOk then
    begin
      // SQL-Server-Datenbank öffnen
      OpenFile('_SQLSRV:Initial Catalog=' + aDlg.lbDatabases.Items.Strings[aDlg.lbDatabases.ItemIndex] + ';Data Source=' + aDlg.eServer.Text + ';'); // do not localize
    end;
  finally
    FreeAndNil(aDlg);
  end;
end;

procedure TDLG_Main.FileOpenMySqlDbExecute(Sender: TObject);
var
  aDlg: TDLG_OpenMySqlDb;
resourcestring
  SMySqlUnstable = 'Die MySQL-Unterstützung befindet sich noch im Experimentalstadium. Der Datenzugriff funktioniert, ist aber noch nicht so stabil wie bei den anderen Datenbankservern.';
begin
  Application.MessageBox(PChar(SMySqlUnstable), PChar(Application.Title), MB_ICONINFORMATION + MB_OK);
  aDlg := TDLG_OpenMySqlDb.Create(Self);
  try
    if aDlg.ShowModal = mrOk then
    begin
      // MySQL-Datenbank öffnen
      OpenFile('_MYSQL:database=' + aDlg.lbDatabases.Items.Strings[aDlg.lbDatabases.ItemIndex] + ';server=' + aDlg.eServer.Text + ';'); // do not localize
    end;
  finally
    FreeAndNil(aDlg);
  end;
end;

procedure TDLG_Main.FormShow(Sender: TObject);
var
  aReg: TRegIniFile;
begin
  THickelSOFTDesign.HS_Design_Aktivieren(False);

  // Farbeinstellungen einlesen
  aReg := TRegIniFile.Create(ConfigRegKey);
  try
    clActiveField     := TColor(GetNearestColor(Canvas.Handle, ColorToRGB(StringToColor(aReg.ReadString('Colors', 'ActiveField',    '$0080FFFF'))))); // do not localize
    clActiveRecord    := TColor(GetNearestColor(Canvas.Handle, ColorToRGB(StringToColor(aReg.ReadString('Colors', 'ActiveRecord',   '$00DED3D6'))))); // do not localize
    clTableBackground := TColor(GetNearestColor(Canvas.Handle, ColorToRGB(StringToColor(aReg.ReadString('Colors', 'Background',     'clWhite'))))); // do not localize
    clTableText       := TColor(GetNearestColor(Canvas.Handle, ColorToRGB(StringToColor(aReg.ReadString('Colors', 'Text',           'clBlack'))))); // do not localize
    clTableZebra      := TColor(GetNearestColor(Canvas.Handle, ColorToRGB(StringToColor(aReg.ReadString('Colors', 'Zebra',          '$00EBFFFF'))))); // do not localize
  finally
    FreeAndNil(aReg);
  end;

  StartUpTimer.Enabled := true;
end;

procedure TDLG_Main.SystemExecuteMacro(Sender: TObject; Msg: TStrings);
var
  sBuffer: String;
begin                                              
  // Per DDE übergebene Dateien öffnen
  if (Copy(Msg.Strings[0], 1, 7) = '[open("') and not Modus_CORA_Verzeichnis and not Modus_HsInfo2_Verzeichnis then // do not localize
  begin
    sBuffer := Copy(Msg.Strings[0], 8);
    sBuffer := Copy(sBuffer, 1, Pos('"', sBuffer)-1); // do not localize
    OpenFile(sBuffer);
  end;
end;

procedure TDLG_Main.Updates1Click(Sender: TObject);
var
  iniFileName: string;
  ini: TMemIniFile;
  localSetupMd5: string;
  remoteSetupMd5: string;
  pgd: TProgressDlg;
  downloadedFile: string;
resourcestring
  SLocalMd5Error = 'DBTool kann nicht nach Updates suchen, da es nicht fest auf diesen Computer installiert ist. Bei portablen Versionen schauen Sie bitte regelmäßig auf github.com/hickelsoft/dbtool nach Updates.';
  SRemoteMd5Error = 'Fehler beim Abfragen der Prüfsumme vom Server. Bitte prüfen Sie Ihre Internetverbindung oder schauen Sie auf github.com/hickelsoft/dbtool nach Updates';
  SWaitUpdateDownload = 'Update herunterladen...';
  SNewVersionAvailable = 'Eine neue Version ist verfügbar! Jetzt herunterladen?';
  SNoNewVersion = 'Keine neuen Updates verfügbar.';
begin
  if Modus_CORA_Verzeichnis or Modus_HsInfo2_Verzeichnis then exit;
  iniFileName := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))+'Setup.ini'; // do not localize
  if not FileExists(iniFileName) then
  begin
    raise Exception.Create(SLocalMd5Error);
  end;
  ini := TMemIniFile.Create(iniFileName);
  try
    localSetupMd5 := ini.ReadString('Setup', 'InstalledMD5', ''); // do not localize
    if Length(Trim(localSetupMd5)) <> 32 then
      raise Exception.Create(SLocalMd5Error);

    try
      remoteSetupMd5 := DoGet('https://www.hickelsoft.de/dbtool/checksum.php'); // do not localize
    except
      on E: EAbort do
      begin
        Abort;
      end;
      on E: Exception do
      begin
        remoteSetupMd5 := '';
      end;
    end;
    if Length(Trim(remoteSetupMd5)) <> 32 then
      raise Exception.Create(SRemoteMd5Error);

    if not SameText(localSetupMd5, remoteSetupMd5) then
    begin
      if Application.MessageBox(PChar(SNewVersionAvailable), PChar(Application.Title), MB_ICONINFORMATION + MB_YESNOCANCEL) = ID_YES then
      begin
        pgd := TProgressDlg.Create(Self);
        try
          pgd.Text := SWaitUpdateDownload;
          pgd.ShowStopButton := true;
          pgd.Open;
          downloadedFile := IncludeTrailingPathDelimiter(GetDesktopFolder) + 'DBTool_Setup.exe'; // do not localize
          DownloadFile('https://www.hickelsoft.de/dbtool/download.php', downloadedFile, pgd); // do not localize
          ShellExecute64(Handle, nil, 'explorer.exe', PChar('/select,'+downloadedFile), nil, SW_SHOWNORMAL); // do not localize
        finally
          FreeAndNil(pgd);
        end
      end;
    end
    else
    begin
      Application.MessageBox(PChar(SNoNewVersion), PChar(Application.Title), MB_ICONINFORMATION + MB_OK);
    end;
  finally
    FreeAndNil(ini);
  end;
end;

procedure TDLG_Main.BearbeitenSuchen1Execute(Sender: TObject);
begin
  if ActiveMDIChild is TMDI_Table then
    TMDI_Table(ActiveMDIChild).Find
  else if ActiveMDIChild is TMDI_Query then
    TMDI_Query(ActiveMDIChild).Find;
end;

procedure TDLG_Main.BearbeitenWeitersuchen1Execute(Sender: TObject);
begin
  if ActiveMDIChild is TMDI_Table then
    TMDI_Table(ActiveMDIChild).FindNext
  else if ActiveMDIChild is TMDI_Query then
    TMDI_Query(ActiveMDIChild).FindNext;
end;

procedure TDLG_Main.ExtrasOptionen1Execute(Sender: TObject);
var
  aDlg: TDLG_Optionen;
begin
  aDlg := TDLG_Optionen.Create(Self);
  try
    aDlg.ShowModal;
  finally
    FreeAndNil(aDlg);
  end;
end;

procedure TDLG_Main.ExtrasExport1Execute(Sender: TObject);
var
  aDlg: TDLG_Export;
resourcestring
  SFirstExecuteQuery = 'Bitte zuerst Abfrage ausführen';
  SQueryS = 'Abfrage_%s';
  SSelectFittingForm = 'Bitte passendes Fenster auswählen.';
begin
  aDlg := TDLG_Export.Create(Self);
  try
    if ActiveMDIChild is TMDI_Table then
    begin
      // TODO: Sollte hier nicht eine TTable erstellt werden?
      aDlg.Table1 := TMDI_Table(ActiveMDIChild).frmDatabase.dbDatabase.Query(TMDI_Table(ActiveMDIChild).SelectString);
      aDlg.DateinameBasis := TMDI_Table(ActiveMDIChild).Table;
    end
    else if ActiveMDIChild is TMDI_Query then
    begin
      aDlg.Table1 := TMDI_Query(ActiveMDIChild).DataSource1.DataSet;
      if not Assigned(aDlg.Table1) then raise Exception.Create(SFirstExecuteQuery);
      aDlg.DateinameBasis := Format(SQueryS, [formatdatetime('yyyymmdd_hhnnss', Now)]);
    end
    else
      raise Exception.Create(SSelectFittingForm);
    aDlg.ShowModal;
    if ActiveMDIChild is TMDI_Table then
    begin
      FreeAndNil(aDlg.Table1);
    end;
  finally
    FreeAndNil(aDlg);
  end;
end;

procedure TDLG_Main.ExtrasImport1Execute(Sender: TObject);
var
  aDlg: TDLG_Import;
begin
  if not (ActiveMDIChild is TMDI_Table) then exit;
  aDlg := TDLG_Import.Create(Self, TMDI_Table(ActiveMDIChild).frmDatabase.dbDatabase, TMDI_Table(ActiveMDIChild).dsData.DataSet, TMDI_Table(ActiveMDIChild).Table);
  try
    aDlg.ShowModal;
    if aDlg.DatenGeaendert then
    begin
      TMDI_Table(ActiveMDIChild).btnAktualisierenClick(Sender);
    end;
  finally
    FreeAndNil(aDlg);
  end;
end;

procedure TDLG_Main.DateiNeu1Execute(Sender: TObject);
var
  aDlg: TDLG_DateiNeu;
begin
  aDlg := TDLG_DateiNeu.Create(Self);
  try
    aDlg.ShowModal;
  finally
    FreeAndNil(aDlg);
  end;
end;

procedure TDLG_Main.Fenster1Click(Sender: TObject);
var
  anItem: TMenuItem;
  i: integer;
begin
  // Untermenüs "Datenbankfenster" und "Tabellenfenster" leeren
  while Datenbankfenster1.Count > 0 do
  begin
    anItem := Datenbankfenster1.Items[0];
    try
      Datenbankfenster1.Delete(0);
    finally
      FreeAndNil(anItem);
    end;
  end;
  while Tabellenfenster1.Count > 0 do
  begin
    anItem := Tabellenfenster1.Items[0];
    try
      Tabellenfenster1.Delete(0);
    finally
      FreeAndNil(anItem);
    end;
  end;
  while Abfragefenster1.Count > 0 do
  begin
    anItem := Abfragefenster1.Items[0];
    try
      Abfragefenster1.Delete(0);
    finally
      FreeAndNil(anItem);
    end;
  end;

  // Untermenüs "Datenbankfenster" und "Tabellenfenster" neu füllen
  for i := 0 to MDIChildCount-1 do
  begin
    anItem := TMenuItem.Create(Self);
    if MDIChildren[i].Tag = 0 then
      MDIChildren[i].Tag := Random(999999999);
    anItem.Tag := MDIChildren[i].Tag;
    anItem.OnClick := DynMenuClick;

    if MDIChildren[i].ClassNameIs('TMDI_Database') then // do not localize
    begin
      anItem.Caption := TMDI_Database(MDIChildren[i]).Database;
      Datenbankfenster1.Add(anItem);
    end
    else if(MDIChildren[i].ClassNameIs('TMDI_Table')) then // do not localize
    begin
      anItem.Caption := TMDI_Table(MDIChildren[i]).Table;
      Tabellenfenster1.Add(anItem);
    end
    else if(MDIChildren[i].ClassNameIs('TMDI_Query')) then // do not localize
    begin
      anItem.Caption := TMDI_Query(MDIChildren[i]).Database.DatabaseName; // TODO: Da haben alle den gleichen Titel
      Abfragefenster1.Add(anItem);
    end;
  end;

  Datenbankfenster1.Enabled := Datenbankfenster1.Count > 0;
  Tabellenfenster1.Enabled := Tabellenfenster1.Count > 0;
  Abfragefenster1.Enabled := Abfragefenster1.Count > 0;

  AlleTabellenschlieen1.Enabled := Tabellenfenster1.Enabled;
  AlleAbfragefensterschlieen1.Enabled := Abfragefenster1.Enabled;
end;

procedure TDLG_Main.DynMenuClick(Sender: TObject);
var
  aForm: TForm;
  i: integer;
begin
  for i := 0 to MDIChildCount-1 do
  begin
    if (MDIChildren[i].Tag <> 0) and (MDIChildren[i].Tag = TMenuItem(Sender).Tag) then
    begin
      aForm := TForm(MDIChildren[i]);
      MDI_Form_BringToFront(aForm);
    end;
  end;
end;

procedure TDLG_Main.AlleAbfragefensterschlieen1Click(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to MDIChildCount-1 do
  begin
    if MDIChildren[i].ClassNameIs('TMDI_Query') then MDIChildren[i].Close; // do not localize
  end;
end;

procedure TDLG_Main.AlleTabellenschlieen1Click(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to MDIChildCount-1 do
  begin
    if MDIChildren[i].ClassNameIs('TMDI_Table') then MDIChildren[i].Close; // do not localize
  end;
end;

procedure TDLG_Main.FormCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
  THickelSOFTDesign.RefreshMainMenuColor(MainMenu1);
end;

procedure TDLG_Main.FormCloseQuery(Sender: TObject; var CanClose: Boolean);

  function GetFirstNonDatabaseWindow: integer;
  var
    i: integer;
  begin
    for i := 0 to MDIChildCount - 1 do
    begin
      if not (MDIChildren[i] is TMDI_Database) then
      begin
        result := i;
        exit;
      end;
    end;
    result := -1;
  end;

//var
//  i: integer;
begin
  // TODO: Das scheint nicht ganz zu funktionieren?
  //       1. Kindsfenster gehen zu, noch bevor hier FormCloseQuery() aufgerufen wird?!
  //       2. Jedes Fenster bekommt 2x CloseQuery, also bekomme ich doppelt so viele Fragen "jetzt speichern ja/nein"
  (*
  CanClose := false;

  // Erst alle Nicht-Datenbankfenster (da diese ein eigenes OnCloseQuery haben könnten)
  while true do
  begin
    i := GetFirstNonDatabaseWindow;
    if i = -1 then break;
    MDIChildren[i].Close;
    Application.ProcessMessages;
  end;

  // Nun alle Datenbankfenster (Das Schließen macht alle abhängigen Fenster ohne Abfrage kaputt)
  while MDIChildCount > 0 do
  begin
    MDIChildren[0].Close;
    Application.ProcessMessages;
  end;
  *)

  CanClose := true;
end;

procedure TDLG_Main.FormCreate(Sender: TObject);
begin
  //Application.OnException := AppException;
  ThlExceptionHandler.Init();

  DoubleBuffered := true;

  if Modus_CORA_Verzeichnis then
  begin
    // Sonstige Anpassungen
    Caption := 'CORAplus ' + Caption;

    FileOpenMySqlDb.Enabled := false;
    FileOpenMySqlDb.Visible := false;

    DateiNeu1.Enabled := false;
    DateiNeu1.Visible := false;

    FileOpen1.Enabled := false;
    FileOpen1.Visible := false;

    ExtrasOptionen1.Enabled := false;
    ExtrasOptionen1.Visible := false;

    N5.Visible := false;

    Updates1.Visible := false;
  end
  else if Modus_HsInfo2_Verzeichnis then
  begin
    // Sonstige Anpassungen
    Caption := 'HS-Info 2.0 ' + Caption;

    FileOpenMySqlDb.Enabled := false;
    FileOpenMySqlDb.Visible := false;

    DateiNeu1.Enabled := false;
    DateiNeu1.Visible := false;

    FileOpen1.Enabled := false;
    FileOpen1.Visible := false;

    ExtrasOptionen1.Enabled := false;
    ExtrasOptionen1.Visible := false;

    N5.Visible := false;

    Updates1.Visible := false;
  end
  else
  begin
    Caption := Application.Title;

    // https://stackoverflow.com/questions/13482304/why-doesnt-my-form-receive-wm-dropfiles-when-files-are-dropped-on-it
    ChangeWindowMessageFilter (WM_DROPFILES, MSGFLT_ADD);
    ChangeWindowMessageFilter (WM_COPYGLOBALDATA, MSGFLT_ADD);

    DragAcceptFiles(Self.Handle, True);
  end;
end;

procedure TDLG_Main.FormDestroy(Sender: TObject);
begin
  DragAcceptFiles(Self.Handle, False);
end;

procedure TDLG_Main.FormPaint(Sender: TObject);
resourcestring
  SBackgroundText1 = 'Datenbank';
  SBackgroundText2 = '';
begin
  THickelSOFTDesign.ZeichneHintergrundschrift(SBackgroundText1, SBackgroundText2, 0.6);
end;

procedure TDLG_Main.StartUpTimerTimer(Sender: TObject);
var
  connStr: string;
  i: integer;
begin
  StartUpTimer.Enabled := false;
  if Modus_HsInfo2_Verzeichnis then
  begin
    connStr := GetHsInfo2ConnectionString;
    OpenFile('_SQLSRV:'+connStr); // do not localize
  end
  else if Modus_CORA_Verzeichnis then
  begin
    // Bei CORAplus und HS-Info 2.0 direkt die Datenbank öffnen
    FileOpenSqlServerDb.Execute;
  end
  else if (ParamCount > 0) and (ParamStr(1) <> '-Embedding') then  // do not localize
  begin
    // Per Kommandozeile übergebene Dateien öffnen
    for i := 1 to ParamCount do
    begin
      OpenFile(ParamStr(i));
    end;
  end;
end;

procedure TDLG_Main.WMDropFiles(var Msg: TWMDropFiles);
var
  DropH: HDROP;               // drop handle
  DroppedFileCount: Integer;  // number of files dropped
  FileNameLength: Integer;    // length of a dropped file name
  FileName: string;           // a dropped file name
  I: Integer;                 // loops thru all dropped files
  DropPoint: TPoint;          // point where files dropped
begin
  inherited;
  // Store drop handle from the message
  DropH := Msg.Drop;
  try
    // Get count of files dropped
    DroppedFileCount := DragQueryFile(DropH, $FFFFFFFF, nil, 0);
    // Get name of each file dropped and process it
    for I := 0 to Pred(DroppedFileCount) do
    begin
      // get length of file name
      FileNameLength := DragQueryFile(DropH, I, nil, 0);
      // create string large enough to store file
      SetLength(FileName, FileNameLength);
      // get the file name
      DragQueryFile(DropH, I, PChar(FileName), FileNameLength + 1);
      // process file name (application specific)
      // ... processing code here
      if not Modus_CORA_Verzeichnis and not Modus_HsInfo2_Verzeichnis then
      begin
        OpenFile(FileName);
      end;
    end;
    // Optional: Get point at which files were dropped
    DragQueryPoint(DropH, DropPoint);
    // ... do something with drop point here
  finally
    // Tidy up - release the drop handle
    // don't use DropH again after this
    DragFinish(DropH);
  end;
  // Note we handled message
  Msg.Result := 0;
end;

end.
