unit OpenMySqlDb;

interface

uses
  Windows, Classes, Controls, StdCtrls, Forms, ADODB, Registry, Buttons, SysUtils,
  Vcl.Graphics, Vcl.ExtCtrls;

type
  TDLG_OpenMySqlDb = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    LbButton1: TButton;
    LbButton2: TButton;
    lbDatabases: TListBox;
    LbSpeedButton1: TSpeedButton;
    eServer: TComboBox;
    Image1: TImage;
    procedure FormShow(Sender: TObject);
    procedure eServerExit(Sender: TObject);
    procedure LbButton1Click(Sender: TObject);
    procedure eServerChange(Sender: TObject);
    procedure Image1Click(Sender: TObject);
  private
    FServerChanged: boolean;
  end;

var
  DLG_OpenMySqlDb: TDLG_OpenMySqlDb;

implementation

uses
  Globals, ShellAPI;

{$R *.DFM}

procedure TDLG_OpenMySqlDb.FormShow(Sender: TObject);
var
  aReg: TRegIniFile;
  dbListFile: string;
begin
  // TODO: In zukunft konfigurierbar machen (in den Optionen)
  dbListFile := GetMySQLDBListFilename;
  if FileExists(dbListFile) then
  begin
    eServer.Items.LoadFromFile(dbListFile);
    eServer.Items.Text := Trim(eServer.Items.Text);
  end;

  // Letzten ausgewählten SQL-Server auslesen
  aReg := TRegIniFile.Create(ConfigRegKey);
  try
    eServer.Text := aReg.ReadString('MRU', 'MySql', 'localhost'); // do not localize
    FServerChanged := false;
  finally
    FreeAndNil(aReg);
  end;

  eServerExit(Sender);
end;

procedure TDLG_OpenMySqlDb.Image1Click(Sender: TObject);
var
  fileToOpen: string;
  sl: TStringList;
begin
  fileToOpen := GetMySQLDBListFilename;
  {$REGION 'Create empty file if required'}
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

procedure TDLG_OpenMySqlDb.eServerChange(Sender: TObject);
begin
  FServerChanged := true;
end;

procedure TDLG_OpenMySqlDb.eServerExit(Sender: TObject);
var
  sBuf: string;
  DB_ADO: TADOConnection;
  aQuery: TADOQuery;
begin
  Screen.Cursor := crHourGlass;
  lbDatabases.Items.Clear;
  DB_ADO := TADOConnection.Create(nil);
  try
    DB_ADO.ConnectionString := 'Provider=MSDASQL.1;Persist Security Info=False;Extended Properties=\"driver={mysql};database=mysql;server=' + eServer.Text + ';"'; // do not localize
    DB_ADO.LoginPrompt := false;
    DB_ADO.Connected := true;

    aQuery := TADOQuery.Create(nil);
    aQuery.Connection := DB_ADO;
    aQuery.SQL.Add('show databases'); // do not localize
    aQuery.Active := true;

    while not aQuery.Eof do
    begin
      sBuf := aQuery.Fields.Fields[0].AsString;
      if sBuf <> 'mysql' then lbDatabases.Items.Add(sBuf); // do not localize
      aQuery.Next;
    end;
  finally
    Screen.Cursor := crDefault;
    FreeAndNil(DB_ADO);
  end;
end;

procedure TDLG_OpenMySqlDb.LbButton1Click(Sender: TObject);
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
    Exit;
  end;

  if lbDatabases.ItemIndex <> -1 then
  begin
    // Letzten ausgewählten MySQL-Server merken
    aReg := TRegIniFile.Create(ConfigRegKey);
    try
      aReg.WriteString('MRU', 'MySql', eServer.Text); // do not localize
    finally
      FreeAndNil(aReg);
    end;
    if fsModal in FormState then ModalResult := mrOk else Close;
  end
  else
  begin
    Application.MessageBox(PChar(SNoDatabaseSelected), PChar(Application.Title), MB_ICONEXCLAMATION + MB_OK);
    lbDatabases.SetFocus;
  end;
end;

end.
