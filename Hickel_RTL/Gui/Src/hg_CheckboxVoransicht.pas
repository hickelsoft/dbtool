unit hg_CheckboxVoransicht;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ImgList, ExtCtrls, HsGradientPanel,
  System.ImageList;

type
  TDLG_CheckboxVoransicht = class(TForm)
    ListView1: TListView;
    Button1: TButton;
    Button2: TButton;
    ImageList1: TImageList;
    Label1: TLabel;
    Image1: TImage;
    Image2: TImage;
    HsGradientPanel1: THsGradientPanel;
    procedure ListView1DblClick(Sender: TObject);
    procedure Image1DblClick(Sender: TObject);
    procedure Image2Click(Sender: TObject);
    procedure ListView1KeyPress(Sender: TObject; var Key: Char);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure CheckOkButtonEnabled;
  end;

var
  DLG_CheckboxVoransicht: TDLG_CheckboxVoransicht;

implementation

{$R *.dfm}

procedure TDLG_CheckboxVoransicht.CheckOkButtonEnabled;
var
  i: Integer;
begin
  for i := 0 to ListView1.Items.Count - 1 do
  begin
    if ListView1.Items.Item[i].ImageIndex = 1 { Checked } then
    begin
      Button2.Enabled := true;
      exit;
    end;
  end;
  Button2.Enabled := false;
end;

procedure TDLG_CheckboxVoransicht.FormResize(Sender: TObject);
begin
  ListView1.Columns[0].MaxWidth := ListView1.ClientWidth;
  ListView1.Columns[0].Width := ListView1.ClientWidth;
end;

procedure TDLG_CheckboxVoransicht.FormShow(Sender: TObject);
begin
  CheckOkButtonEnabled;
end;

procedure TDLG_CheckboxVoransicht.Image1DblClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to ListView1.Items.Count - 1 do
  begin
    if ListView1.Items.Item[i].ImageIndex = 1 { Checked } then
      ListView1.Items.Item[i].ImageIndex := 0 { Not checked };
  end;
  CheckOkButtonEnabled;
end;

procedure TDLG_CheckboxVoransicht.Image2Click(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to ListView1.Items.Count - 1 do
  begin
    if ListView1.Items.Item[i].ImageIndex = 0 { Not checked } then
      ListView1.Items.Item[i].ImageIndex := 1 { Checked };
  end;
  CheckOkButtonEnabled;
end;

procedure TDLG_CheckboxVoransicht.ListView1DblClick(Sender: TObject);
begin
  if ListView1.Selected.ImageIndex = 1 { Checked } then
    ListView1.Selected.ImageIndex := 0 { Not checked }
  else if ListView1.Selected.ImageIndex = 0 { Not checked } then
    ListView1.Selected.ImageIndex := 1 { Checked };
  CheckOkButtonEnabled;
end;

procedure TDLG_CheckboxVoransicht.ListView1KeyPress(Sender: TObject;
  var Key: Char);
var
  i: Integer;
  tmp: boolean;
begin
  if Key = ' ' then
  begin
    Key := #0;

    tmp := false;
    for i := 0 to ListView1.Items.Count - 1 do
    begin
      if ListView1.Items.Item[i].Selected then
      begin
        if ListView1.Items.Item[i].ImageIndex <> 2 { Disabled } then
        begin
          tmp := tmp or (ListView1.Items.Item[i].ImageIndex = 1 { Checked } );
        end;
      end;
    end;
    tmp := not tmp;

    for i := 0 to ListView1.Items.Count - 1 do
    begin
      if ListView1.Items.Item[i].Selected then
      begin
        if ListView1.Items.Item[i].ImageIndex <> 2 { Disabled } then
        begin
          if tmp then
            ListView1.Items.Item[i].ImageIndex := 1 { Checked }
          else
            ListView1.Items.Item[i].ImageIndex := 0 { Not checked };
        end;
      end;
    end;
  end;
  CheckOkButtonEnabled;
end;

end.
