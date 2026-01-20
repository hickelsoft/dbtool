unit TextEing;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, ComCtrls;

type
  TDLG_Texteingabe = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Edit: TEdit;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    CheckBox1: TCheckBox;
    DateTimePicker1: TDateTimePicker;
    procedure EditEnter(Sender: TObject);
    procedure EditExit(Sender: TObject);
  public
    FVordergrundfarbe: TColor;
    FHintergrundfarbe: TColor;
  end;

var
  DLG_Texteingabe: TDLG_Texteingabe;

implementation

{$R *.DFM}

procedure TDLG_Texteingabe.EditEnter(Sender: TObject);
begin
  if Sender is TEdit then
  begin
    TEdit(Sender).Color := FHintergrundfarbe;
    TEdit(Sender).Font.Color := FVordergrundfarbe;
  end;
end;

procedure TDLG_Texteingabe.EditExit(Sender: TObject);
begin
  if Sender is TEdit then
  begin
    TEdit(Sender).Color := clWindow;
    TEdit(Sender).Font.Color := clBlack;
  end;
end;

end.
