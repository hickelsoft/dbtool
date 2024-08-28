unit HsProgressChecklist;

// Daniel Marschall
// 20.09.2016

interface

uses
  Windows, SysUtils, Classes, Controls, ComCtrls, Graphics;

// TODO: Kann man irgendwie eine TStringList geben, und diese irgendwie überwachen?
//       Dann könnte man die Jobs nämlich auch im IDE Editor bearbeiten können

type
  {$IF CompilerVersion > 20.0} // Version geraten
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$IFEND}
  THsProgressChecklist = class(TCustomListView)
  private
    { Private-Deklarationen }
    FImgList: TImageList;
    FImgIdxInProgress: integer;
    FImgIdxFinished: integer;
    FProgress: integer;
    procedure SetProgress(const Value: integer);
  protected
    { Protected-Deklarationen }
    procedure RefreshIcons;
  public
    { Public-Deklarationen }
    property Progress: integer read FProgress write SetProgress;
    procedure Clear; override;
    procedure IncProgress;
    procedure Addjob(name: string);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Action;
    property Align;
    property AllocBy;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind default bkNone;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property BorderWidth;
    property Checkboxes;
    property Color;
    // property Columns;
    // property ColumnClick;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FlatScrollBars;
    property FullDrag;
    property GridLines;
    property HideSelection;
    property HotTrack;
    property HotTrackStyles;
    property HoverTime;
    property IconOptions;
    // property Items;
    // property LargeImages;
    property MultiSelect;
    property OwnerData;
    property OwnerDraw;
    // property ReadOnly default False;
    property RowSelect;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowColumnHeaders;
    property ShowWorkAreas;
    property ShowHint;
    //property SmallImages;
    //property SortType;
    //property StateImages;
    property TabOrder;
    property TabStop default False;//True;
    //property ViewStyle;
    property Visible;
    property OnAdvancedCustomDraw;
    property OnAdvancedCustomDrawItem;
    property OnAdvancedCustomDrawSubItem;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnColumnClick;
    property OnColumnDragged;
    property OnColumnRightClick;
    property OnCompare;
    property OnContextPopup;
    property OnCustomDraw;
    property OnCustomDrawItem;
    property OnCustomDrawSubItem;
    property OnData;
    property OnDataFind;
    property OnDataHint;
    property OnDataStateChange;
    property OnDblClick;
    property OnDeletion;
    property OnDrawItem;
    property OnEdited;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetImageIndex;
    property OnGetSubItemImage;
    property OnDragDrop;
    property OnDragOver;
    property OnInfoTip;
    property OnInsert;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnSelectItem;
    property OnStartDock;
    property OnStartDrag;
  end;

procedure Register;

implementation

{$R *.res}

procedure Register;
begin
  RegisterComponents('HS', [THsProgressChecklist]);
end;

// http://stackoverflow.com/questions/3056889/delphi-populate-an-imagelist-with-icons-at-runtime-destroys-transparency
function AddBitmapFromResource(ImageList: TImageList; ResName: string): Integer;
var
  Bitmap: TBitmap;
begin
  Bitmap := TBitmap.Create;
  try
    Bitmap.LoadFromResourceName(HInstance, ResName);
    Result := ImageList.Add(Bitmap, nil);
  finally
    FreeAndNil(Bitmap);
  end;
end;

{ THsProgressChecklist }

procedure THsProgressChecklist.Addjob(name: string);
begin
  with Items.Add do
  begin
    ImageIndex := -1;
    Caption := name;
  end;
end;

procedure THsProgressChecklist.Clear;
begin
  FProgress := -1;
  Items.Clear;
end;

constructor THsProgressChecklist.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ViewStyle := vsReport;
  ReadOnly := true;
  ShowColumnHeaders := false;

  with Columns.Add do
  begin
    // Use -1 setting to set the column header to the size of the largest subitem text in the column,
    // and a -2 setting to set the column header to the size of the text in the column header.
    Width := -2;
  end;

  FProgress := -1;

  FImgList := TImageList.Create(self);
  FImgIdxInProgress := AddBitmapFromResource(FImgList, 'HsProgressChecklist_InProgress');
  FImgIdxFinished   := AddBitmapFromResource(FImgList, 'HsProgressChecklist_Finished');
  SmallImages := FImgList;
end;

destructor THsProgressChecklist.Destroy;
begin
  FreeAndNil(FImgList);
  
  inherited;
end;

procedure THsProgressChecklist.IncProgress;
begin
  Progress := Progress + 1;
end;

procedure THsProgressChecklist.RefreshIcons;
var
  i: integer;
begin
  for i := 0 to Items.Count - 1 do
  begin
    if i < FProgress then
      Items.Item[i].ImageIndex := FImgIdxFinished
    else if i = FProgress then
      Items.Item[i].ImageIndex := FImgIdxInProgress
    else
      Items.Item[i].ImageIndex := -1;
  end;
  if (FProgress >= 0) and (FProgress < Items.Count) then
  begin
    Items.Item[FProgress].MakeVisible(true);
  end;
end;

procedure THsProgressChecklist.SetProgress(const Value: integer);
begin
  FProgress := Value;
  RefreshIcons;
end;

end.
