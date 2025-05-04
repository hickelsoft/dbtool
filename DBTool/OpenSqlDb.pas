unit OpenSqlDb;

{$INCLUDE 'Globals.inc'}

interface

uses
  Windows, SysUtils, Classes, Controls, StdCtrls, Forms, ADODB, Registry,
  Buttons,
  msxmldom, XMLDoc, xmldom, XMLIntf, DB, Vcl.Graphics, Vcl.ExtCtrls;

type
  TDLG_OpenSqlDb = class(TForm)
    LbButton1: TButton;
    LbButton2: TButton;
    Label1: TLabel;
    Label2: TLabel;
    lbDatabases: TListBox;
    LbSpeedButton1: TSpeedButton;
    XMLDocument1: TXMLDocument;
    eServer: TComboBox;
    Image1: TImage;
    procedure FormShow(Sender: TObject);
    procedure eServerExit(Sender: TObject);
    procedure LbButton1Click(Sender: TObject);
    procedure eServerChange(Sender: TObject);
    procedure Image1Click(Sender: TObject);
  public
    class var FirstRun: boolean;
  private
    FServerChanged: boolean;
  end;

var
  DLG_OpenSqlDb: TDLG_OpenSqlDb;

implementation

{$R *.DFM}

uses
  Globals, C_Database, hl_SqlServerProvider, ShellApi;

procedure TDLG_OpenSqlDb.FormShow(Sender: TObject);
var
  aReg: TRegIniFile;
  serverFound: boolean;
  CoraClientConfXML: string;
  dbServer: string;
  serverConfigFile: string;
  serverPath: string;
  document: TXMLDocument;
  selectDbName: string;
  idx: integer;
  dbListFile: string;
begin
  serverFound := false;

  // TODO: In zukunft konfigurierbar machen (in den Optionen)
  dbListFile := GetSQLServerDBListFilename;
  if FileExists(dbListFile) then
  begin
    eServer.Items.LoadFromFile(dbListFile);
    eServer.Items.Text := Trim(eServer.Items.Text);
  end;

  if FirstRun then
  begin

    if Modus_CORA_Verzeichnis then
    begin
      CoraClientConfXML := IncludeTrailingPathDelimiter
        (ExtractFilePath(ParamStr(0))) + 'config\Hickel.Config.xml';
      // do not localize
      if not FileExists(CoraClientConfXML) then
        CoraClientConfXML := IncludeTrailingPathDelimiter
          (ExtractFilePath(ParamStr(0))) + '..\config\Hickel.Config.xml';
      // do not localize
      if FileExists(CoraClientConfXML) then
      begin
        // document := TXMLDocument.Create(DLG_OpenSqlDb); // Geht irgendwie nicht
        document := XMLDocument1;
        try
          document.LoadFromFile(CoraClientConfXML);
          serverPath := document.ChildNodes.FindNode('Hickel')
            .ChildNodes.FindNode('Config').ChildNodes.FindNode('Server').Text;
          // do not localize

          serverConfigFile := IncludeTrailingPathDelimiter(serverPath) +
            'Config\Cora.Config.xml'; // do not localize
          if FileExists(serverConfigFile) then
          begin
            document.LoadFromFile(serverConfigFile);
            dbServer := document.ChildNodes.FindNode('Cora')
              .ChildNodes.FindNode('Config').ChildNodes.FindNode
              ('DatenbankServer').Text; // do not localize
            selectDbName := document.ChildNodes.FindNode('Cora')
              .ChildNodes.FindNode('Config').ChildNodes.FindNode
              ('DatenbankName').Text;; // do not localize

            eServer.Text := dbServer;
            FServerChanged := false;
            serverFound := true;
          end;
        finally
          // FreeAndNil(document);
        end;
      end;
    end;

    FirstRun := false;
  end;

  if not serverFound then
  begin
    // Letzten ausgewählten SQL-Server auslesen
    aReg := TRegIniFile.Create(ConfigRegKey);
    try
      eServer.Text := aReg.ReadString('MRU', 'SqlServer', '');
      // do not localize
      FServerChanged := false;
    finally
      FreeAndNil(aReg);
    end;
  end;

  eServerExit(Sender);

  if Modus_CORA_Verzeichnis then
  begin
    if selectDbName <> '' then
    begin
      idx := lbDatabases.Items.IndexOf(selectDbName + '_1'); // do not localize
      if idx = -1 then
        lbDatabases.Items.IndexOf(selectDbName + '_0'); // do not localize
      if idx = -1 then
        lbDatabases.Items.IndexOf(selectDbName);
      if idx <> -1 then
        lbDatabases.ItemIndex := idx;
    end;
  end;

  // DM 22.11.2016: Bin mir nicht sicher, ob das gut ist.
  // lbDatabases.SetFocus;
end;

procedure TDLG_OpenSqlDb.Image1Click(Sender: TObject);
var
  fileToOpen: string;
  sl: TStringList;
begin
  fileToOpen := GetSQLServerDBListFilename;
{$REGION 'Create empty file if required'}
  if not DirectoryExists(ExtractFilePath(fileToOpen)) then
  begin
    ForceDirectories(ExtractFilePath(fileToOpen));
  end;
  if not FileExists(fileToOpen) then
  begin
    sl := TStringList.Create;
    try
      sl.SaveToFile(fileToOpen);
    finally
      FreeAndNil(sl);
    end;
  end;
{$ENDREGION}
  ShellExecute(Handle, 'open', PChar(fileToOpen), '', '', SW_NORMAL);
end;

procedure TDLG_OpenSqlDb.eServerChange(Sender: TObject);
begin
  FServerChanged := true;
end;

procedure TDLG_OpenSqlDb.eServerExit(Sender: TObject);
var
  X: TDbToolDatabase;
  aDS: TDataSet;
  // DB_ADO: TADOConnection;
  // aQuery: TADOQuery;
begin
  lbDatabases.Items.Clear;

  if eServer.Text = '' then
    exit;

  X := TDbToolDatabase.Create('_SQLSRV:Initial Catalog=master;Data Source=' +
    eServer.Text + ';'); // do not localize
  Screen.Cursor := crHourGlass;
  try
    aDS := X.Query
      ('SELECT name FROM sysdatabases where not (name in (''master'', ''tempdb'', ''model'', ''msdb''));');
    // do not localize
    try
      while not aDS.Eof do
      begin
        lbDatabases.Items.Add(aDS.Fields.Fields[0].AsString);
        aDS.Next;
      end;
    finally
      FreeAndNil(aDS);
    end;
  finally
    Screen.Cursor := crDefault;
    FreeAndNil(X);
  end;
end;

(*
  DB_ADO := TADOConnection.Creaate(nil);
  try
  DB_ADO.ConnectionString := "Provider='+SqlServerProvider+';UID=sa;Pwd=;Initial Catalog=master;Data Source=" + eServer.Text + ";"; // do not localize
  DB_ADO.LoginPrompt := false;
  DB_ADO.Connected := true;

  aQuery := TADOQuery.Create(nil);
  aQuery.Connection := DB_ADO;
  aQuery.SQL.Add("SELECT name FROM sysdatabases where not (name in ('master', 'tempdb', 'model', 'msdb'));"); // do not localize
  aQuery.Active := true;

  while not aQuery.Eof do
  begin
  lbDatabases.Items.Add(aQuery.Fields.Fields[0].AsString);
  aQuery.Next;
  end;
  finally
  Screen.Cursor := crDefault;
  FreeAndNil(DB_ADO);
  end;
*)

procedure TDLG_OpenSqlDb.LbButton1Click(Sender: TObject);
var
  aReg: TRegIniFile;
resourcestring
  SNoDatabaseSelected = 'Sie haben keine Datenbank ausgewählt!';
begin
  if FServerChanged and eServer.Focused then
  begin
    LbSpeedButton1.Click;
    lbDatabases.SetFocus;
    FServerChanged := false;
    exit;
  end;

  if lbDatabases.ItemIndex <> -1 then
  begin
    // Letzten ausgewählten SQL-Server merken
    aReg := TRegIniFile.Create(ConfigRegKey);
    try
      aReg.WriteString('MRU', 'SqlServer', eServer.Text); // do not localize
    finally
      FreeAndNil(aReg);
    end;
    if fsModal in FormState then
      ModalResult := mrOk
    else
      Close;
  end
  else
  begin
    Application.MessageBox(PChar(SNoDatabaseSelected), PChar(Application.Title),
      MB_ICONEXCLAMATION + MB_OK);
    lbDatabases.SetFocus;
  end;
end;

initialization

TDLG_OpenSqlDb.FirstRun := true;

end.
