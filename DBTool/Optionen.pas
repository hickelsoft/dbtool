unit Optionen;

interface

uses
  SysUtils, Windows, Classes, Controls, StdCtrls, Forms, Dialogs, Buttons,
  Graphics;

type
  TDLG_Optionen = class(TForm)
    GroupBox1: TGroupBox;
    btnStandardDB: TSpeedButton;
    GroupBox2: TGroupBox;
    btnHintergrund: TSpeedButton;
    Label1: TLabel;
    btnText: TSpeedButton;
    Label2: TLabel;
    btnZeile: TSpeedButton;
    Label3: TLabel;
    btnFeld: TSpeedButton;
    Label4: TLabel;
    Label5: TLabel;
    btnZebra: TSpeedButton;
    LbButton1: TButton;
    LbButton2: TButton;
    ColorDialog1: TColorDialog;
    btnStandardDBF: TSpeedButton;
    btnStandardMDB: TSpeedButton;
    btnStandardGDB: TSpeedButton;
    btnStandardAccDb: TSpeedButton;
    procedure FormShow(Sender: TObject);
    procedure LbButton1Click(Sender: TObject);
    procedure btnStandardDBClick(Sender: TObject);
    procedure btnFarbeClick(Sender: TObject);
    procedure btnStandardDBFClick(Sender: TObject);
    procedure btnStandardMDBClick(Sender: TObject);
    procedure btnStandardGDBClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure btnStandardAccDbClick(Sender: TObject);
  private
    procedure MakeStandard(Ext, Type_, DisplayName, MessageName: string);
  end;

var
  DLG_Optionen: TDLG_Optionen;

implementation

{$R *.DFM}

uses
  Registry, Globals;

procedure TDLG_Optionen.FormShow(Sender: TObject);
var
  aReg: TRegIniFile;
  sWorkDir: string;
begin
// TODO: Implementieren
//   btnText.Color          := clTableText;
//   btnHintergrund.Color   := clTableBackground;
//   btnZebra.Color         := clTableZebra;
//   btnZeile.Color         := clActiveRecord;
//   btnFeld.Color          := clActiveField;

   aReg := TRegIniFile.Create(ConfigRegKey);
   try
     sWorkDir := aReg.ReadString('Settings', 'WorkDir', '.'); // do not localize
   finally
     FreeAndNil(aReg);
   end;
end;

procedure TDLG_Optionen.LbButton1Click(Sender: TObject);
var
  aReg: TRegIniFile;
begin
   // Farbeinstellungen speichern
// TODO: Implementieren
//   clTableText       := btnText.Color;
//   clTableBackground := btnHintergrund.Color;
//   clTableZebra      := btnZebra.Color;
//   clActiveRecord    := btnZeile.Color;
//   clActiveField     := btnFeld.Color;

   aReg := TRegIniFile.Create(ConfigRegKey);
   try
     aReg.WriteString('Colors',   'ActiveField',    ColorToString(clActiveField)); // do not localize
     aReg.WriteString('Colors',   'ActiveRecord',   ColorToString(clActiveRecord)); // do not localize
     aReg.WriteString('Colors',   'Background',     ColorToString(clTableBackground)); // do not localize
     aReg.WriteString('Colors',   'Text',           ColorToString(clTableText)); // do not localize
     aReg.WriteString('Colors',   'Zebra',          ColorToString(clTableZebra)); // do not localize
   finally
     FreeAndNil(aReg);
   end;
end;

procedure TDLG_Optionen.btnFarbeClick(Sender: TObject);
begin
// TODO: Implementieren
//   ColorDialog1.Color := TSpeedButton(Sender).Color;
//   if(ColorDialog1.Execute) TSpeedButton(Sender).Color := ColorDialog1.Color;
end;

procedure TDLG_Optionen.MakeStandard(Ext, Type_, DisplayName, MessageName: string);
var
  aReg: TRegistry;
resourcestring
  SDBToolDefaultInstalled = 'DBTool wurde als Standard-Anwendung für %s eingerichtet.';
  SDefaultErrorNoAdmin = 'Fehler beim Setzen der Standard-Anwendung. Eventuell muss DBTool mit Administrator-Rechten gestartet werden.';
begin
  // TODO: elevate to admin privileges
  try
     aReg := TRegistry.Create;
     try
       aReg.RootKey := HKEY_CLASSES_ROOT;
       if aReg.OpenKey(Ext, true) then begin                                      aReg.WriteString('', Type_);           aReg.CloseKey; end; // do not localize
       if aReg.OpenKey(Type_, true) then begin                                    aReg.WriteString('', DisplayName);     aReg.CloseKey; end; // do not localize
       if aReg.OpenKey(Type_+'\shell\open\command', true) then begin              aReg.WriteString('', ParamStr(0));     aReg.CloseKey; end; // do not localize
       if aReg.OpenKey(Type_+'\shell\open\ddeexec', true) then begin              aReg.WriteString('', '[open("%1")]');  aReg.CloseKey; end; // do not localize
       if aReg.OpenKey(Type_+'\shell\open\ddeexec\Application', true) then begin  aReg.WriteString('', 'dbtool');        aReg.CloseKey; end; // do not localize
       if aReg.OpenKey(Type_+'\shell\open\ddeexec\Topic', true) then begin        aReg.WriteString('', 'System');        aReg.CloseKey; end; // do not localize
     finally
       FreeAndNil(aReg);
     end;
     Application.MessageBox(PChar(Format(SDBToolDefaultInstalled, [MessageName])), PChar(Application.Title), MB_ICONINFORMATION + MB_OK);
  except
    on E: EAbort do
    begin
      Abort;
    end;
    on E: Exception do
    begin
      Application.MessageBox(PChar(SDefaultErrorNoAdmin), PChar(Application.Title), MB_ICONERROR + MB_OK);
    end;
  end;
end;

procedure TDLG_Optionen.btnStandardAccDbClick(Sender: TObject);
resourcestring
  SAccessDatabase = 'Access-Datenbank';
  SAccessDatabases = 'Access-Datenbanken';
begin
  MakeStandard('.accdb', 'ACCDBFILE', SAccessDatabase, SAccessDatabases); // do not localize
end;

procedure TDLG_Optionen.btnStandardDBClick(Sender: TObject);
resourcestring
  SParadoxTable = 'Paradox-Tabelle';
  SParadoxTables = 'Paradox-Tabellen';
begin
  MakeStandard('.db', 'DBFILE', SParadoxTable, SParadoxTables); // do not localize
end;

procedure TDLG_Optionen.btnStandardDBFClick(Sender: TObject);
resourcestring
  SDBaseTable = 'dBase-Tabelle';
  SDBaseTables = 'dBase-Tabellen';
begin
  MakeStandard('.dbf', 'DBFFILE', SDBaseTable, SDBaseTables); // do not localize
end;

procedure TDLG_Optionen.btnStandardMDBClick(Sender: TObject);
resourcestring
  SAccess97Database = 'Access97-Datenbank';
  SAccess97Databases = 'Access97-Datenbanken';
begin
  MakeStandard('.mdb', 'MDBFILE', SAccess97Database, SAccess97Databases); // do not localize
end;

procedure TDLG_Optionen.btnStandardGDBClick(Sender: TObject);
resourcestring
  SInterbaseDatabase = 'Interbase-Datenbank';
  SInterbaseDatabases = 'Interbase-Datenbanken';
begin
  MakeStandard('.gdb', 'GDBFILE', SInterbaseDatabase, SInterbaseDatabases); // do not localize
end;

procedure TDLG_Optionen.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    Key := 0;
    Close;
  end;
end;

end.
