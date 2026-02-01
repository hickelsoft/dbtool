unit hg_InputQuery;

// Die Funktion Dialogs.InputBox ist der größte Mist. Wenn man CANCEL drückt,
// dann wird der Default-Wert zurückgegeben. Also kann man bei einer
// Wahlwiederholung gar nicht wissen, welcher Button gedrückt wurde.

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
    Memo1: TMemo;
    procedure FormShow(Sender: TObject);
    procedure Memo1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  public
    class function InputQuery(title: string; var text: string; liste: TStrings = nil): boolean; overload;
    class function InputQuery(title, description: string; var text: string; liste: TStrings = nil): boolean; overload;
    class function InputMemo(title: string; var text: string): boolean; overload;
    class function InputMemo(title, description: string; var text: string): boolean; overload;
  end;

var
  hgInputQry: ThgInputQry;

implementation

{$R *.dfm}

{ ThgInputQry }

procedure ThgInputQry.FormShow(Sender: TObject);
begin
  if Edit1.CanFocus then
    Edit1.SetFocus
  else if ComboBox1.CanFocus then
    ComboBox1.SetFocus
  else if Memo1.CanFocus then
    Memo1.SetFocus;
end;

class function ThgInputQry.InputMemo(title: string; var text: string): boolean;
begin
  result := InputMemo(title, title, text);
end;

class function ThgInputQry.InputMemo(title, description: string; var text: string): boolean;
var
  frm: ThgInputQry;
begin
  frm := ThgInputQry.Create(nil);
  try
    frm.Caption := title;

    frm.Label1.Caption := StringReplace(description, '&', '&&', [rfReplaceAll]);

    frm.Memo1.Visible := true;
    frm.Edit1.Visible := false;
    frm.ComboBox1.Visible := false;
    frm.Memo1.text := text;
    frm.Memo1.Top := frm.Edit1.Top + frm.Label1.Height;

    frm.Width := Screen.Width div 2;
    frm.Height := Screen.Height div 2;

    frm.Memo1.Height := frm.Memo1.Height - frm.Label1.Height;
    //frm.Button1.Top := frm.Button1.Top + frm.Label1.Height;
    //frm.Button2.Top := frm.Button2.Top + frm.Label1.Height;
    frm.ClientHeight := frm.ClientHeight + frm.Label1.Height;
    result := frm.ShowModal = mrOk;
    if result then
    begin
      text := frm.Memo1.text;
    end;
  finally
    FreeAndNil(frm);
  end;
end;

class function ThgInputQry.InputQuery(title: string; var text: string;
  liste: TStrings = nil): boolean;
begin
  result := InputQuery(title, title, text, liste);
end;

class function ThgInputQry.InputQuery(title, description: string; var text: string; liste: TStrings): boolean;
var
  frm: ThgInputQry;
begin
  frm := ThgInputQry.Create(nil);
  try
    frm.Caption := title;

    frm.Label1.Caption := StringReplace(description, '&', '&&', [rfReplaceAll]);

    if Assigned(liste) then
    begin
      frm.Memo1.Visible := false;
      frm.Edit1.Visible := false;
      frm.ComboBox1.Visible := true;
      frm.ComboBox1.Items.AddStrings(liste);
      frm.ComboBox1.text := text;
      frm.ComboBox1.Top := frm.ComboBox1.Top + frm.Label1.Height;
    end
    else
    begin
      frm.Memo1.Visible := false;
      frm.Edit1.Visible := true;
      frm.ComboBox1.Visible := false;
      frm.Edit1.text := text;
      frm.Edit1.Top := frm.Edit1.Top + frm.Label1.Height;
    end;
    //frm.Button1.Top := frm.Button1.Top + frm.Label1.Height;
    //frm.Button2.Top := frm.Button2.Top + frm.Label1.Height;
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

procedure ThgInputQry.Memo1KeyDown(Sender: TObject; var Key: Word; Shift:
    TShiftState);
begin
  if (Shift = [ssCtrl]) and (Key = VK_RETURN) then
    ModalResult := mrOk;
end;

end.
