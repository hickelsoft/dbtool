unit DBMaskEdit;
//****************************
// This component makes the MaskEdit Component Data aware
//
// By Richard A Gilbert
// Written in Delphi 5
// EMail  : ragilbert@mindspring.com
//    or  : richard.gilbert@mcmail.vanderbilt.edu
//****************************

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Mask, db, dbctrls, Variants;

type
  {$IF CompilerVersion > 20.0} // Version geraten
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$IFEND}
  TDBMaskEdit = class(TMaskEdit)
  private
    fDataLink : TFieldDataLink;
    function GetDataField : string;
    function GetDataSource : TDataSource;
    procedure SetDataField(const Value : string);
    procedure SetDataSource(Value : TDataSource);
    procedure DataChange(Sender : TObject);
    procedure UpdateData(Sender : TObject);
    procedure CMExit(var Message : TWMNoParams); message CM_Exit;
  protected
    procedure MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y : Integer); override;
    procedure KeyDown(var Key : Word; Shift : TShiftState); override;
    procedure Change; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
    property DataField : string read GetDataField write SetDataField;
    property DataSource : TDataSource read GetDataSource write SetDataSource;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Data Controls', [TDBMaskEdit]);
end;

//*************************
// TDBMaskEdit Private
//*************************

function TDBMaskEdit.GetDataField : string;
begin
  result := fDataLink.FieldName;
end;//GetDataField

function TDBMaskEdit.GetDataSource : TDataSource;
begin
  result := fDataLink.DataSource;
end;//GetDataSource

procedure TDBMaskEdit.SetDataField(const Value : string);
begin
  fDataLink.FieldName := Value;
end;//SetDataField

procedure TDBMaskEdit.SetDataSource(Value : TDataSource);
begin
  fDataLink.DataSource := Value;
end;//SetDataSource

procedure TDBMaskEdit.DataChange(Sender : TObject);
begin

//  if (fDataLink.Field = nil) or (FDataLink.Field.asVariant = NULL) then
  if (fDataLink.Field = nil) or VarIsNull(FDataLink.Field.asVariant) then
    Text := ''
  else
    Text := FDataLink.Field.asVariant;
end;//DataChange

procedure TDBMaskEdit.UpdateData(Sender : TObject);
begin
  FDataLink.Field.asVariant := Text;
end;//UpdateData

procedure TDBMaskEdit.CMExit(var Message : TWMNoParams);
begin
  try
    FDataLink.UpdateRecord;
  except
    on E: EAbort do Abort;
    on E: Exception do SetFocus;
  end;//try
  inherited;
end;//CMExit

//*************************
// TDBMaskEdit Protected
//*************************

procedure TDBMaskEdit.MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
var
  MyMouseDown : TMouseEvent;
begin
  if not fDataLink.ReadOnly and fDataLink.Edit then
    inherited MouseDown(Button, Shift, X, Y)
  else begin
    MyMouseDown := OnMouseDown;
    if Assigned(MyMouseDown) then MyMouseDown(Self, Button, Shift, X, Y);
  end;//if
end;

procedure TDBMaskEdit.KeyDown(var Key : Word; Shift : TShiftState);
var
  MyKeyDown : TKeyEvent;
begin
  if not fDataLink.ReadOnly and fDataLink.Edit then
    inherited KeyDown(Key, Shift)
  else begin
    MyKeyDown := OnKeyDown;
    if Assigned(MyKeyDown) then MyKeyDown(Self, Key, Shift);
  end;//if
end;//KeyDown

procedure TDBMaskEdit.Change;
begin
  FDataLink.Modified;
  inherited Change;
end;//Change

//*************************
// TDBMaskEdit Public
//*************************

constructor TDBMaskEdit.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  fDataLink := TFieldDataLink.Create;
  fDataLink.OnDataChange := DataChange;
  fDataLink.OnUpdateData := UpdateData;
end;//Create

destructor TDBMaskEdit.Destroy;
begin
  fDataLink.OnDataChange := nil;
  FreeAndNil(fDataLink);
  inherited Destroy;
end;//Destroy


end.
