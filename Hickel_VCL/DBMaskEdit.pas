// ****************************
// This component makes the MaskEdit Component Data aware
//
// By Richard A Gilbert
// Written in Delphi 5
// EMail  : ragilbert@mindspring.com
// or  : richard.gilbert@mcmail.vanderbilt.edu
// ****************************

// NOTE: Component was not created by HickelSOFT.
// It was moved into Hickel_VCL.bpl to make development easier
// See a few small improvements marked with "by HickelSOFT"

unit DBMaskEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Mask, db, dbctrls, Variants;

type
{$IF CompilerVersion > 20.0} // Version guessed
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)] // (by HickelSOFT)
{$IFEND}

  TDBMaskEdit = class(TMaskEdit)
  private
    fDataLink: TFieldDataLink;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure DataChange(Sender: TObject);
    procedure UpdateData(Sender: TObject);
    procedure CMExit(var Message: TWMNoParams); message CM_Exit;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Change; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Data Controls', [TDBMaskEdit]);
end;

{ TDBMaskEdit }

function TDBMaskEdit.GetDataField: string;
begin
  result := fDataLink.FieldName;
end;

function TDBMaskEdit.GetDataSource: TDataSource;
begin
  result := fDataLink.DataSource;
end;

procedure TDBMaskEdit.SetDataField(const Value: string);
begin
  fDataLink.FieldName := Value;
end;

procedure TDBMaskEdit.SetDataSource(Value: TDataSource);
begin
  fDataLink.DataSource := Value;
end;

procedure TDBMaskEdit.DataChange(Sender: TObject);
begin
  // if (fDataLink.Field = nil) or (FDataLink.Field.asVariant = NULL) then
  if (fDataLink.Field = nil) or VarIsNull(fDataLink.Field.asVariant) then
    Text := ''
  else
    Text := fDataLink.Field.asVariant;
end;

procedure TDBMaskEdit.UpdateData(Sender: TObject);
begin
  fDataLink.Field.asVariant := Text;
end;

procedure TDBMaskEdit.CMExit(var Message: TWMNoParams);
begin
  try
    fDataLink.UpdateRecord;
  except
    on E: EAbort do
      Abort;
    on E: Exception do
      SetFocus;
  end;
  inherited;
end;

procedure TDBMaskEdit.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  MyMouseDown: TMouseEvent;
begin
  if not fDataLink.ReadOnly and fDataLink.Edit then
    inherited MouseDown(Button, Shift, X, Y)
  else
  begin
    MyMouseDown := OnMouseDown;
    if Assigned(MyMouseDown) then
      MyMouseDown(Self, Button, Shift, X, Y);
  end;
end;

procedure TDBMaskEdit.KeyDown(var Key: Word; Shift: TShiftState);
var
  MyKeyDown: TKeyEvent;
begin
  if not fDataLink.ReadOnly and fDataLink.Edit then
    inherited KeyDown(Key, Shift)
  else
  begin
    MyKeyDown := OnKeyDown;
    if Assigned(MyKeyDown) then
      MyKeyDown(Self, Key, Shift);
  end;
end;

procedure TDBMaskEdit.Change;
begin
  fDataLink.Modified;
  inherited Change;
end;

constructor TDBMaskEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fDataLink := TFieldDataLink.Create;
  fDataLink.OnDataChange := DataChange;
  fDataLink.OnUpdateData := UpdateData;
end;

destructor TDBMaskEdit.Destroy;
begin
  fDataLink.OnDataChange := nil;
  FreeAndNil(fDataLink);
  inherited Destroy;
end;

end.
