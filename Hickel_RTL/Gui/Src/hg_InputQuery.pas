unit hg_InputQuery;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  ThgInputQry = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Button2: TButton;
    ComboBox1: TComboBox;
    Label1: TLabel;
    procedure FormShow(Sender: TObject);
  public
    class function InputQuery(title: string; var text: string;
      liste: TStrings = nil): boolean; overload;
    class function InputQuery(title, description: string; var text: string;
      liste: TStrings = nil): boolean; overload;
  end;

var
  hgInputQry: ThgInputQry;

implementation

{$R *.dfm}
{ ThgInputQry }

// Die Funktion Dialogs.InputBox ist der größte Mist. Wenn man CANCEL drückt, dann wird der Default-Wert zurückgegeben. Also kann man bei einer Wahlwiederholung gar nicht wissen, welcher Button gedrückt wurde.

procedure ThgInputQry.FormShow(Sender: TObject);
begin
  if Edit1.CanFocus then
    Edit1.SetFocus;
  if ComboBox1.CanFocus then
    ComboBox1.SetFocus;
end;

class function ThgInputQry.InputQuery(title: string; var text: string;
  liste: TStrings = nil): boolean;
begin
  result := InputQuery(title, title, text, liste);
end;

class function ThgInputQry.InputQuery(title, description: string;
  var text: string; liste: TStrings): boolean;
var
  frm: ThgInputQry;
begin
  frm := ThgInputQry.Create(nil);
  try
    frm.Caption := title;

    frm.Label1.Caption := description;

    if Assigned(liste) then
    begin
      frm.Edit1.Visible := false;
      frm.ComboBox1.Visible := true;
      frm.ComboBox1.Items.AddStrings(liste);
      frm.ComboBox1.text := text;
      frm.ComboBox1.Top := frm.ComboBox1.Top + frm.Label1.Height;
    end
    else
    begin
      frm.Edit1.Visible := true;
      frm.ComboBox1.Visible := false;
      frm.Edit1.text := text;
      frm.Edit1.Top := frm.Edit1.Top + frm.Label1.Height;
    end;
    frm.Button1.Top := frm.Button1.Top + frm.Label1.Height;
    frm.Button2.Top := frm.Button2.Top + frm.Label1.Height;
    frm.ClientHeight := frm.ClientHeight + frm.Label1.Height;
    result := frm.ShowModal = mrOk;
    if result then
    begin
      if Assigned(liste) then
        text := frm.ComboBox1.text
      else
        text := frm.Edit1.text;
    end;
  finally
    FreeAndNil(frm);
  end;
end;

end.
