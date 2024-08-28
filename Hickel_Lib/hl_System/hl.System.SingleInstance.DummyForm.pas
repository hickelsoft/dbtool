unit hl.System.SingleInstance.DummyForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs;

type
  TDummyForm = class(TForm)
    procedure FormShow(Sender: TObject);
  end;

var
  DummyForm: TDummyForm;

implementation

{$R *.dfm}

procedure TDummyForm.FormShow(Sender: TObject);
begin
  Close;
end;

end.
