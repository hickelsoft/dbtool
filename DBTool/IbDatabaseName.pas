unit IbDatabaseName;

interface

uses
  Classes, Controls, StdCtrls, Forms;

type
  TDLG_IbDatabaseName = class(TForm)
    Label1: TLabel;
    LbButton1: TButton;
    LbButton2: TButton;
    Edit1: TComboBox;
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
  end;

var
  DLG_IbDatabaseName: TDLG_IbDatabaseName;

implementation

{$R *.DFM}

procedure TDLG_IbDatabaseName.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  case Key of
    13:
    begin
      Key := 0;
      LbButton1.Click;
    end;
      
    27:
    begin
      Key := 0;
      LbButton2.Click;
    end;
  end;
end;

end.
