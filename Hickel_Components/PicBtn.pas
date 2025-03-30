unit PicBtn;

interface

uses
  Windows, Forms, SysUtils, Messages, Classes, Graphics, Controls, ExtCtrls;

type
  THsButtonState = (bsDown, bsUp, bsFlat);

  {$IF CompilerVersion > 20.0} // Version geraten
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$IFEND}
  THsPictBtn = class(TGraphicControl)
  private
    FOwner: TComponent;
    FCaption: string;
    FColor: TColor;
    FPictureEnabled: TPicture;
    FPictureDisabled: TPicture;
    FEnabled: boolean;
    FAlignment: TAlignment;
    FOffice97Look: boolean;
    bCapturing: boolean;
      bCursorOnButton: boolean;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseDown: TMouseEvent;
    FOnMouseUp: TMouseEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseExit: TNotifyEvent;
    FOnClick: TNotifyEvent;
      FState: THsButtonState;
    bLoaded: boolean;
      FTransparent: boolean;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
  protected
    FLabelLeft: integer;
    FLabelHeight: integer;
    FLabelTop: integer;
    FLabelWidth: integer;
    FImageLeft: integer;
    FImageHeight: integer;
    FImageTop: integer;
    FImageWidth: integer;

    procedure SetPictureEnabled( pNew: TPicture );
    procedure SetPictureDisabled( pNew: TPicture );
    procedure SetCaption( sNew: string );
    procedure SetColor( cNew: TColor );
    procedure SetEnabled( bNew: boolean ); override;
    procedure SetLabelLeft( iNew: integer );
    procedure SetLabelHeight( iNew: integer );
    procedure SetLabelTop( iNew: integer );
    procedure SetLabelWidth( iNew: integer );
    procedure SetImageLeft( iNew: integer );
    procedure SetImageHeight( iNew: integer );
    procedure SetImageTop( iNew: integer );
    procedure SetImageWidth( iNew: integer );
    procedure SetAlignment( aNew: TAlignment );
    procedure SetOffice97Look( bNew: boolean );
      procedure SetTransparent( bNew: boolean );
    procedure MouseMove( Shift: TShiftState; X, Y: Integer ); override;
    procedure MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); override;
    procedure MouseUp( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); override;
    procedure MouseHandler( Shift: TShiftState; X, Y: Integer );
    procedure Loaded; override;
    procedure ChangeScale( M, D: Integer ); override;
    procedure ModifyPanelBorder( bCursorOnButton: boolean; bButtonDown: boolean );
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
      procedure Paint; override;
    procedure DrawTransparentBitmap (ahdc: HDC; Image: TBitmap; xStart, yStart: Word);
  published
    property Anchors;
    property Alignment: TAlignment read FAlignment write SetAlignment;
    property Caption: string read FCaption write SetCaption;
    property Color: TColor read FColor write SetColor;
      property DragCursor;
    property Enabled: boolean read FEnabled write SetEnabled;
    property Font;
    property LabelPos_Left: integer read FLabelLeft write SetLabelLeft;
    property LabelPos_Height: integer read FLabelHeight write SetLabelHeight;
    property LabelPos_Top: integer read FLabelTop write SetLabelTop;
    property LabelPos_Width: integer read FLabelWidth write SetLabelWidth;
    property ImagePos_Left: integer read FImageLeft write SetImageLeft;
    property ImagePos_Height: integer read FImageHeight write SetImageHeight;
    property ImagePos_Top: integer read FImageTop write SetImageTop;
    property ImagePos_Width: integer read FImageWidth write SetImageWidth;
    property Office97Look: boolean read FOffice97Look write SetOffice97Look;
      property IsTransparent: boolean read FTransparent write SetTransparent;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property PictureEnabled: TPicture read FPictureEnabled write SetPictureEnabled;
    property PictureDisabled: TPicture read FPictureDisabled write SetPictureDisabled;
    property ShowHint;
    property Visible;
    property PopupMenu;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseExit: TNotifyEvent read FOnMouseExit write FOnMouseExit;
    property Align;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Hs', [THsPictBtn]);
end;

constructor THsPictBtn.Create( AOwner: TComponent );
begin
   inherited Create( AOwner );

  bLoaded := False;
   FOwner := AOwner;
   ControlStyle := ControlStyle + [csSetCaption, csCaptureMouse];
   ControlStyle := ControlStyle - [csOpaque];

   FPictureEnabled := TPicture.Create;
   FPictureDisabled := TPicture.Create;

   SetAlignment( taRightJustify );

   FImageLeft := 4;
   FImageTop := 4;
   FImageWidth := 32;
   FImageHeight := 32;

   FLabelLeft := 5;
   FLabelWidth := 90;
   FLabelTop := 34;
   FLabelHeight := 18;

   Height := 50;
   Width := 100;

   FEnabled := True;
   Color := clBtnFace;
   FState := bsUp;

   bCapturing := False;
end;

destructor THsPictBtn.Destroy;
begin
   FreeAndNil(FPictureEnabled);
   FreeAndNil(FPictureDisabled);
   inherited Destroy;
end;

procedure THsPictBtn.Paint;
var
   aRect1, aRect2: TRect;
   iOffset: integer;
   iBuffer: LongInt;
   aBitmap: TBitmap;
   bPaint: boolean;
   Stretch: Extended;

begin
  if csDestroying in ComponentState then exit;

  Canvas.Pen.Style := psClear;
  Canvas.Brush.Color := FColor;
  if FTransparent = False then Canvas.Rectangle( 0, 0, Width+1, Height+1 );
  Canvas.Pen.Style := psSolid;

  iOffset := 0;
  if FState = bsDown then iOffset := 1;

   { Bild zeichnen, falls eins vorhanden ist }
   if Enabled = True then bPaint := not (FPictureEnabled.Bitmap.Empty)
  else bPaint := not (FPictureDisabled.Bitmap.Empty);

   if bPaint = True then
   begin
      Stretch := Screen.PixelsPerInch / 96;

      aRect1.Left               := FImageLeft + iOffset;
      aRect1.Top                := FImageTop + iOffset;
      aRect1.Right      := round(FImageWidth * Stretch) + FImageLeft + iOffset;
      aRect1.Bottom     := round(FImageHeight * Stretch) + FImageTop + iOffset;
      aRect2.Left               := 0;
      aRect2.Top                := 0;
      aRect2.Right      := round(FImageWidth * Stretch);
      aRect2.Bottom     := round(FImageHeight * Stretch);

      aBitmap := TBitmap.Create;
      try
        aBitmap.Height := round(FImageHeight * Stretch);
        aBitmap.Width := round(FImageWidth * Stretch);

        try
           if Enabled = True then aBitmap.Canvas.StretchDraw(aRect2, FPictureEnabled.Bitmap)
           else aBitmap.Canvas.StretchDraw(aRect2, FPictureDisabled.Bitmap);

           if FTransparent = True then DrawTransparentBitmap(Canvas.Handle, aBitmap, aRect1.Left, aRect1.Top)
           else Canvas.CopyRect(aRect1, aBitmap.Canvas, aRect2);
        except
          on E: EAbort do
          begin
            Abort;
          end;
        end;
      finally
        FreeAndNil(aBitmap);
      end;
  end;

   { Caption zeichnen }
  Canvas.Font := Font;
  SetBkMode( Canvas.Handle, TRANSPARENT );
  if Enabled = False then Canvas.Font.Color := clGray;
   aRect1.Left          := FLabelLeft + iOffset;
   aRect1.Top           := FLabelTop + iOffset;
   aRect1.Right := FLabelWidth + FLabelLeft + iOffset;
   aRect1.Bottom        := FLabelHeight + FLabelTop + iOffset;
  iBuffer := DT_WORDBREAK;
  if FAlignment = taRightJustify then iBuffer := iBuffer or DT_RIGHT;
  if FAlignment = taCenter then iBuffer := iBuffer or DT_CENTER;
  DrawText( Canvas.Handle, pCHAR( FCaption ), Length( FCaption ), aRect1, iBuffer );


   { Rahmen zeichnen }
  aRect1 := ClientRect;
  if FState = bsUp then Frame3D(Canvas, aRect1, clWhite, clBlack, 1);
  if FState = bsDown then Frame3D(Canvas, aRect1, clBlack, clWhite, 1);
end;

procedure THsPictBtn.SetCaption( sNew: string );
begin
   FCaption := sNew;
   Invalidate;
end;

procedure THsPictBtn.SetTransparent( bNew: boolean );
begin
  FTransparent := bNew;
  Invalidate;
end;

procedure THsPictBtn.SetImageLeft( iNew: integer );
begin
   FImageLeft := iNew;
   Invalidate;
end;

procedure THsPictBtn.SetImageHeight( iNew: integer );
begin
   FImageHeight := iNew;
   Invalidate;
end;

procedure THsPictBtn.SetImageTop( iNew: integer );
begin
   FImageTop := iNew;
   Invalidate;
end;

procedure THsPictBtn.SetImageWidth( iNew: integer );
begin
   FImageWidth := iNew;
   Invalidate;
end;

procedure THsPictBtn.SetLabelLeft( iNew: integer );
begin
   FLabelLeft := iNew;
   Invalidate;
end;

procedure THsPictBtn.SetLabelHeight( iNew: integer );
begin
   FLabelHeight := iNew;
   Invalidate;
end;

procedure THsPictBtn.SetLabelTop( iNew: integer );
begin
   FLabelTop := iNew;
   Invalidate;
end;

procedure THsPictBtn.SetLabelWidth( iNew: integer );
begin
   FLabelWidth := iNew;
   Invalidate;
end;

procedure THsPictBtn.SetPictureEnabled( pNew: TPicture );
begin
  FPictureEnabled.Assign( pNew );
   Invalidate;
end;

procedure THsPictBtn.SetPictureDisabled( pNew: TPicture );
begin
   FPictureDisabled.Assign( pNew );
   Invalidate;
end;

procedure THsPictBtn.SetColor( cNew: TColor );
begin
   FColor := cNew;
   Invalidate;
end;

procedure THsPictBtn.SetEnabled( bNew: boolean );
begin
   FEnabled := bNew;
   Invalidate;
end;

procedure THsPictBtn.SetAlignment( aNew: TAlignment );
begin
   FAlignment := aNew;
   Invalidate;
end;

procedure THsPictBtn.CMMouseEnter(var Message: TMessage);
begin
   if csDesigning in ComponentState then exit;
   bCursorOnButton := True;
   if assigned( FOnMouseEnter ) then FOnMouseEnter( self );
end;

procedure THsPictBtn.CMMouseLeave(var Message: TMessage);
begin
   if csDesigning in ComponentState then exit;
  ModifyPanelBorder( not FOffice97Look, False );
   bCursorOnButton := False;
   if assigned( FOnMouseExit ) then FOnMouseExit( self );
end;

procedure THsPictBtn.MouseHandler( Shift: TShiftState; X, Y: Integer );
begin
   if csDesigning in ComponentState then exit;

   ModifyPanelBorder( bCursorOnButton, ssLeft in Shift );

   if ssLeft in Shift then bCapturing := True;

   if not (ssLeft in Shift) and (bCapturing = True) then
   begin
      bCapturing := False;
      if (bCursorOnButton = True) and (Assigned( FOnClick )) then
      begin
   ModifyPanelBorder( False, False );
   FOnClick( self );
      end;
   end;
end;

procedure THsPictBtn.ModifyPanelBorder( bCursorOnButton: boolean; bButtonDown: boolean );
begin
  if csDesigning in ComponentState then exit;

   if bCursorOnButton = True then
   begin
      if (bButtonDown = True) and (FState <> bsDown) then
      begin
      FState := bsDown;
       Invalidate;
      end;
      if (bButtonDown = False) and (FState <> bsUp) then
      begin
      FState := bsUp;
       Invalidate;
      end;
   end;

   if bCursorOnButton = False then
   begin
      if FOffice97Look = True then
      begin
   if FState <> bsFlat then
   begin
        FState := bsFlat;
         Invalidate;
   end;
      end
      else
      begin
   if FState <> bsUp then
   begin
        FState := bsUp;
         Invalidate;
   end;
      end;
   end;
end;

procedure THsPictBtn.MouseMove( Shift: TShiftState; X, Y: Integer );
begin
   if csDesigning in ComponentState then exit;
   if FEnabled = False then exit;
   bCursorOnButton := False;
   if (X>=0) and (X<=Width) and (Y>=0) and (Y<=Height) then bCursorOnButton := True;
   MouseHandler( Shift, X, Y );
   if assigned( FOnMouseMove ) then FOnMouseMove( self, Shift, X, Y );
end;

procedure THsPictBtn.MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
begin
   if csDesigning in ComponentState then exit;
   if FEnabled = False then exit;
   MouseHandler( Shift, X, Y );
   if assigned( FOnMouseDown ) then FOnMouseDown( self, Button, Shift, X, Y );
end;

procedure THsPictBtn.MouseUp( Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
begin
   if csDesigning in ComponentState then exit;
   if FEnabled = False then exit;
   MouseHandler( Shift, X, Y );
   if assigned( FOnMouseUp ) then FOnMouseUp( self, Button, Shift, X, Y );
end;

procedure THsPictBtn.SetOffice97Look( bNew: boolean );
begin
   FOffice97Look := bNew;
   ModifyPanelBorder( False, False );
   Invalidate;
end;

procedure THsPictBtn.Loaded;
begin
  bLoaded := True;
   ModifyPanelBorder( False, False );
end;

procedure THsPictBtn.ChangeScale( M, D: Integer );
begin
   Width    := Width * M div D;
   Height   := Height * M div D;
   Top      := Top * M div D;
   Left     := Left * M div D;

   FImageWidth    := FImageWidth * M div D;
   FImageHeight   := FImageHeight * M div D;
   FImageTop      := FImageTop * M div D;
   FImageLeft     := FImageLeft * M div D;

   FLabelWidth    := FLabelWidth * M div D;
   FLabelHeight   := FLabelHeight * M div D;
   FLabelTop      := FLabelTop * M div D;
   FLabelLeft     := FLabelLeft * M div D;

   Font.Size := Font.Size * M div D;
   Invalidate;
end;

procedure THsPictBtn.DrawTransparentBitmap (ahdc: HDC; Image: TBitmap; xStart, yStart: Word);
var
  cColor: TColorRef;
  bmAndBack, bmAndObject, bmAndMem, bmSave, bmBackOld, bmObjectOld, bmMemOld, bmSaveOld: HBitmap;
  hdcMem, hdcBack, hdcObject, hdcTemp, hdcSave: HDC;
  ptSize: TPoint;

begin
  hdcTemp := CreateCompatibleDC( aHdc );
  SelectObject( HdcTemp, Image.Handle );
  ptSize.x := Image.Width;
  ptSize.y := Image.Height;
  DPtoLP( HdcTemp, ptSize, 1 );
  hdcBack := CreateCompatibleDC( aHdc );
  hdcObject := CreateCompatibleDC( aHdc );
  hdcMem := CreateCompatibleDC( aHdc );
  hdcSave := CreateCompatibleDC( aHdc );

  bmAndBack := CreateBitmap( ptSize.x, ptSize.y, 1, 1, nil );
  bmAndObject := CreateBitmap( ptSize.x, ptSize.y, 1, 1, nil );

  bmAndMem := CreateCompatibleBitmap( aHdc, ptSize.x, ptSize.y );
  bmSave := CreateCompatibleBitmap( aHdc, ptSize.x, ptSize.y );

  bmBackOld   := SelectObject( hdcBack, bmAndBack);
  bmObjectOld := SelectObject( hdcObject, bmAndObject);
  bmMemOld    := SelectObject( hdcMem, bmAndMem);
  bmSaveOld   := SelectObject( hdcSave, bmSave);

  SetMapMode( hdcTemp, GetMapMode (ahdc) );
  BitBlt( hdcSave, 0, 0, ptSize.x, ptSize.y, hdcTemp, 0, 0, SRCCOPY );
  cColor := SetBkColor( hdcTemp, FColor or $02000000 );
  BitBlt( hdcObject, 0, 0, ptSize.x, ptSize.y, hdcTemp, 0, 0, SRCCOPY );

  SetBkColor( hdcTemp, cColor );
  BitBlt( hdcBack, 0, 0, ptSize.x, ptSize.y, hdcObject, 0, 0, NOTSRCCOPY );

  BitBlt( HdcMem, 0, 0, ptSize.x, ptSize.y, aHdc, xStart, yStart, SRCCOPY );
  BitBlt( HdcMem, 0, 0, ptSize.x, ptSize.y, HdcObject, 0, 0, SRCAND );
  BitBlt( HdcTemp, 0, 0, ptSize.x, ptSize.y, HdcBack, 0, 0, SRCAND );
  BitBlt( HdcMem, 0, 0, ptSize.x, ptSize.y, HdcTemp, 0, 0, SRCPAINT );
  BitBlt( aHdc, xStart, yStart, ptSize.x, ptSize.y, HdcMem, 0, 0, SRCCOPY );
  BitBlt( HdcTemp, 0, 0, ptSize.x, ptSize.y, HdcSave, 0, 0, SRCCOPY );
  DeleteObject( SelectObject ( hdcBack, bmBackOld ) );
  DeleteObject( SelectObject ( hdcObject, bmObjectOld ) );
  DeleteObject( SelectObject ( hdcMem, bmMemOld ) );
  DeleteObject( SelectObject ( hdcSave, bmSaveOld ) );
  DeleteDC( hdcMem );
  DeleteDC( hdcBack );
  DeleteDC( hdcObject );
  DeleteDC( hdcSave );
  DeleteDC( hdcTemp );
end;

end.
