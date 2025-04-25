unit DateiNeu;

{$INCLUDE 'Globals.inc'}

interface

uses
  Windows, SysUtils, Classes, Controls, StdCtrls, Forms, ExtCtrls, Dialogs, Buttons;

type
  TDLG_DateiNeu = class(TForm)
    rgTyp: TRadioGroup;
    lDateiname: TLabel;
    eDateiname: TEdit;
    btnDatei: TSpeedButton;
    LbButton1: TButton;
    LbButton2: TButton;
    SaveDialog1: TSaveDialog;
    lServer: TLabel;
    eServer: TEdit;
    procedure btnDateiClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rgTypClick(Sender: TObject);
    procedure LbButton1Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
  end;

var
  DLG_DateiNeu: TDLG_DateiNeu;

implementation

uses
  C_Database, Globals, Main, hl.Utils;

{$R *.DFM}

procedure TDLG_DateiNeu.btnDateiClick(Sender: TObject);
resourcestring
  SInterbaseDatabases = 'InterBase-Datenbanken';
  SAccessDatabases = 'Access/Jet-Datenbanken';
  SAllFiles = 'Alle Dateien';
begin
  case rgTyp.ItemIndex of
      0:
      begin
         SaveDialog1.Filter := SInterbaseDatabases + ' (*.gdb)|*.gdb|' + SAllFiles + ' (*.*)|*.*'; // do not localize
         SaveDialog1.DefaultExt := '.gdb'; // do not localize
      end;
      1:
      begin
         SaveDialog1.Filter := SAccessDatabases + ' (*.mdb;*.accdb)|*.mdb;*.accdb|' + SAllFiles + ' (*.*)|*.*'; // do not localize
         SaveDialog1.DefaultExt := '.mdb'; // do not localize
      end;
  end;
  SaveDialog1.FileName := eDateiname.Text;
  if(SaveDialog1.Execute) then eDateiname.Text := SaveDialog1.FileName;
end;

procedure TDLG_DateiNeu.FormShow(Sender: TObject);
begin
  rgTyp.ItemIndex := 0;
end;

procedure TDLG_DateiNeu.rgTypClick(Sender: TObject);
resourcestring
  SFileName = 'Dateiname';
  SDatabaseName = 'Datenbankname';
  SNew = 'Neu';
  SMySqlExperimental = 'Die MySQL-Unterstützung befindet sich noch im Experimentalstadium. Der Datenzugriff funktioniert, ist aber noch nicht so stabil wie bei den anderen Datenbankservern.';
begin
  case rgTyp.ItemIndex of
      0:
      begin
         lDateiname.Caption := SFileName + ':';
         eDateiname.Text := IncludeTrailingPathDelimiter(GetDesktopFolder) + SNew + '.gdb'; // do not localize
         btnDatei.Visible := true;
         lServer.Visible := false;
      end;
      1:
      begin
         lDateiname.Caption := SFileName + ':';
         eDateiname.Text := IncludeTrailingPathDelimiter(GetDesktopFolder) + SNew + '.accdb'; // do not localize
         btnDatei.Visible := true;
         lServer.Visible := false;
      end;
      2:
      begin
         lDateiname.Caption := SDatabaseName + ':';
         eDateiname.Text := sNew;
         btnDatei.Visible := false;
         lServer.Visible := true;
         Application.MessageBox(PChar(SMySqlExperimental), PChar(Application.Title), MB_ICONINFORMATION + MB_OK);
      end;
      3:
      begin
         lDateiname.Caption := SDatabaseName + ':';
         eDateiname.Text := sNew;
         btnDatei.Visible := false;
         lServer.Visible := true;
      end;
  end;
  if btnDatei.Visible then
    eDateiname.Width := 256
  else
    eDateiname.Width := 277;
  eServer.Visible := lServer.Visible;
end;

procedure TDLG_DateiNeu.LbButton1Click(Sender: TObject);
begin
  case rgTyp.ItemIndex of
      0:
      begin
         try
           TDbToolDatabase.CreateDatabase(dtInterbase, eDateiname.Text, '');
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
         DLG_Main.OpenFile(eDateiname.Text);
         Close;
      end;
      1:
      begin
         try
           TDbToolDatabase.CreateDatabase(dtAccess, eDateiname.Text, '');
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
         DLG_Main.OpenFile(eDateiname.Text);
         Close;
      end;
      2:
      begin
         TDbToolDatabase.CreateDatabase(dtMySql, eDateiname.Text, eServer.Text);
         DLG_Main.OpenFile('_MYSQL:database=' + eDateiname.Text + ';server=' + eServer.Text + ';'); // do not localize
         Close;
      end;
      3:
      begin
         TDbToolDatabase.CreateDatabase(dtSqlServer, eDateiname.Text, eServer.Text);
         DLG_Main.OpenFile('_SQLSRV:Initial Catalog=' + eDateiname.Text + ';Data Source=' + eServer.Text + ';'); // do not localize
         Close;
      end;
  end;
end;

procedure TDLG_DateiNeu.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    Key := 0;
    Close;
  end;
end;

end.
