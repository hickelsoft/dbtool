unit TeEinDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  textEing;

type
  TEditDlgFormat = (fText, fDate);

  {$IF CompilerVersion > 20.0} // Version geraten
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$IFEND}
  TEditDlg = class(TComponent)
  private
    FVorgabe: string;
    FCaption: string;
    FVordergrundfarbe: TColor;
    FHintergrundfarbe: TColor;
    FCheckBoxCaption: string;
    FFormat: TEditDlgFormat;
  public
    Text: string;
    Checked: boolean;
    DLG_Texteingabe: TDLG_Texteingabe;
    function Execute( ParentForm: TForm; MaxLen: integer=0; Caption: string='' ): boolean;
  published
    property Caption: string read FCaption write FCaption;
    property CheckBoxCaption: string read FCheckBoxCaption write FCheckBoxCaption;
    property Hintergrundfarbe: TColor read FHintergrundfarbe write FHintergrundfarbe;
    property Vordergrundfarbe: TColor read FVordergrundfarbe write FVordergrundfarbe;
    property Format: TEditDlgFormat read FFormat write FFormat;
    property Vorgabe: string read FVorgabe write FVorgabe;
  end;

procedure Register;

implementation

procedure Register;
begin
   RegisterComponents( 'HS', [TEditDlg] );
end;

function TEditDlg.Execute( ParentForm: TForm; MaxLen: integer=0; Caption: string='' ): boolean;
var
  bOK: boolean;
begin
  bOK := False;
  Text := '';
  DLG_Texteingabe := TDlg_TextEingabe.Create( ParentForm );
  try
    DLG_Texteingabe.Edit.Text := FVorgabe;
    DLG_Texteingabe.Edit.MaxLength := MaxLen;
    if Caption <> '' then
      DLG_Texteingabe.Panel1.Caption := Caption
    else
      DLG_Texteingabe.Panel1.Caption := FCaption;
    DLG_Texteingabe.FVordergrundfarbe := FVordergrundfarbe;
    DLG_Texteingabe.FHintergrundfarbe := FHintergrundfarbe;
    if FCheckBoxCaption <> '' then
    begin
      DLG_TextEingabe.CheckBox1.Visible := True;
      DLG_TextEingabe.CheckBox1.Caption := FCheckBoxCaption;
    end;
    DLG_TextEingabe.Edit.Visible := FFormat = fText;
    DLG_TextEingabe.DateTimePicker1.Visible := FFormat = fDate;
    DLG_TextEingabe.DateTimePicker1.Date := Date;
    if DLG_TextEingabe.ShowModal = mrOK then
    begin
      bOK := True;
      if Format = fDate then
        Text := DateToStr(DLG_TextEingabe.DateTimePicker1.Date)
      else if Format = fText then
        Text := DLG_TextEingabe.Edit.Text;
    end;
    Checked := DLG_TextEingabe.Checkbox1.Checked;
  finally
    FreeAndNil(DLG_Texteingabe);
  end;
  result := bOK;
end;

end.
