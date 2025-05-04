(* **********************************************************************
  *                                                                     *
  *  Copyright (C) 2001  Renato Amaral (BRASIL) alertmax@hotmail.com    *
  *  #UIN 543210
  *                                                                     *
  *                                                                     *
  * to move the Form  select in ObjectInspector MoveWho= mwParent       *
  *                                                                     *
  *                                                                     *
  ********************************************************************** *)

// NOTE: Component was not created by HickelSOFT.
// It was moved into Hickel_Components.bpl to make development easier
// See a few small improvements marked with "by HickelSOFT"

unit HsGradientPanel;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons;

type
  TWhoToMove = (mwNone, mwSelf, mwParent);
  TGradientStyle = (gsHorizontal, gsVertical, gsVerticalCenter,
    gsHorizontalCenter, gsElliptic, gsRectangle);

{$IF CompilerVersion > 20.0} // Version geraten
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$IFEND}

  THsGradientPanel = class(TCustomPanel)
  private
    OldX, OldY, OldLeft, OldTop: Integer;
    ScreenDC: HDC;
    MoveRect: TRect;
    Moving: Boolean;
    FMoveWho: TWhoToMove;
    { Private declarations }
    FBeginColor: TColor;
    FEndColor: TColor;
    FRealBeginColor: TColor; // added by HickelSOFT
    FRealEndColor: TColor; // added by HickelSOFT
    FGradientStyle: TGradientStyle;
    bmpGradient: TBitmap;
    FInUpdate: Boolean; // added by HickelSOFT
    procedure SetBeginColor(Value: TColor);
    procedure SetEndColor(Value: TColor);
    procedure SetGradientStyle(Value: TGradientStyle);
    procedure PaintSolid(r, g, b: Integer); // added by HickelSOFT
    procedure PaintHorizontal(br, bg, bb, dr, dg, db: Integer);
    procedure PaintVertical(br, bg, bb, dr, dg, db: Integer);
    procedure PaintElliptic(br, bg, bb, dr, dg, db: Integer);
    procedure PaintRectangle(br, bg, bb, dr, dg, db: Integer);
    procedure PaintVerticalCenter(br, bg, bb, dr, dg, db: Integer);
    procedure PaintHorizontalCenter(br, bg, bb, dr, dg, db: Integer);
    procedure PaintCaption;
    procedure PaintBevel;
  protected
    { Protected declarations }
    procedure Paint; override;
  public
    { Public declarations }
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;

    procedure BeginUpdate; // added by HickelSOFT
    procedure EndUpdate; // added by HickelSOFT

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property MoveWho: TWhoToMove read FMoveWho write FMoveWho;
    property BeginColor: TColor read FBeginColor write SetBeginColor;
    property EndColor: TColor read FEndColor write SetEndColor;
    property GradientStyle: TGradientStyle read FGradientStyle
      write SetGradientStyle;
    property Align;
    property Alignment;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BorderWidth;
    property BorderStyle;
    property DragCursor;
    property DragMode;
    property Enabled;
    property FullRepaint;
    property Caption;
    property Ctl3D;
    property Font;
    property Locked;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
  end;

procedure Register;

implementation

procedure THsGradientPanel.BeginUpdate; // added by HickelSOFT
begin
  FInUpdate := true;
end;

constructor THsGradientPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BeginColor := clBtnFace;
  // Changed by HickelSOFT from clTeal to clBtnFace NOT FBeginColor!
  EndColor := clBtnFace;
  // Changed by HickelSOFT from clTeal to clBtnFace. NOT FEndColor!
  FGradientStyle := gsVertical;
  bmpGradient := TBitmap.Create;
  bmpGradient.Canvas.Pen.Style := psSolid;
  bmpGradient.Canvas.Pen.Mode := pmCopy;
end;

procedure THsGradientPanel.Paint;
var
  BeginR, BeginG, BeginB: Integer;
  DiffR, DiffG, DiffB: Integer;
  // LineCount : Integer;
  PanelRect: TRect;
begin
  if FInUpdate then
    exit; // added by HickelSOFT

  inherited;
  bmpGradient.Width := Width;
  bmpGradient.Height := Height;
  PanelRect := Rect(0, 0, Width, Height);
  BeginR := FRealBeginColor and $000000FF;
  // Changed from FBeginColor to FRealBeginColor by HickelSOFT
  BeginG := (FRealBeginColor shr 8) and $000000FF;
  // Changed from FBeginColor to FRealBeginColor by HickelSOFT
  BeginB := (FRealBeginColor shr 16) and $000000FF;
  // Changed from FBeginColor to FRealBeginColor by HickelSOFT
  DiffR := (FRealEndColor and $000000FF) - BeginR;
  // Changed from FEndColor to FRealEndColor by HickelSOFT
  DiffG := ((FRealEndColor shr 8) and $000000FF) - BeginG;
  // Changed from FEndColor to FRealEndColor by HickelSOFT
  DiffB := ((FRealEndColor shr 16) and $000000FF) - BeginB;
  // Changed from FEndColor to FRealEndColor by HickelSOFT
  if (DiffR = 0) and (DiffG = 0) and (DiffB = 0) then // added by HickelSOFT
    PaintSolid(BeginR, BeginG, BeginB) // added by HickelSOFT
  else if FGradientStyle = gsHorizontal then
    PaintHorizontal(BeginR, BeginG, BeginB, DiffR, DiffG, DiffB)
  else if FGradientStyle = gsVertical then
    PaintVertical(BeginR, BeginG, BeginB, DiffR, DiffG, DiffB)
  else if FGradientStyle = gsElliptic then
    PaintElliptic(BeginR, BeginG, BeginB, DiffR, DiffG, DiffB)
  else if FGradientStyle = gsRectangle then
    PaintRectangle(BeginR, BeginG, BeginB, DiffR, DiffG, DiffB)
  else if FGradientStyle = gsVerticalCenter then
    PaintVerticalCenter(BeginR, BeginG, BeginB, DiffR, DiffG, DiffB)
  else if FGradientStyle = gsHorizontalCenter then
    PaintHorizontalCenter(BeginR, BeginG, BeginB, DiffR, DiffG, DiffB);
  PaintCaption;
  PaintBevel;
  BitBlt(Canvas.Handle, 0, 0, Width, Height, bmpGradient.Canvas.Handle, 0,
    0, SRCCOPY);
end;

procedure THsGradientPanel.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);

begin
  if (Button = mbleft) and not Moving then
    Moving := true;
  inherited MouseDown(Button, Shift, X, Y);
  if (MoveWho <> mwNone) and (Button = mbleft) then
  begin
    SetCapture(Self.Handle);
    if MoveWho = mwParent then
      if Parent = Screen.ActiveForm then
        ScreenDC := GetDC(0)
      else
        ScreenDC := GetDC(Parent.Handle)
    else
      ScreenDC := GetDC(Parent.Handle);

    OldX := X;
    OldY := Y;
    OldLeft := X;
    OldTop := Y;
    if MoveWho = mwParent then
      MoveRect := Rect(Parent.Left, Parent.Top, Parent.Left + Parent.Width,
        Parent.Top + Parent.Height)
    else
      MoveRect := Rect(Self.Left, Self.Top, Self.Left + Self.Width,
        Self.Top + Self.Height);
    DrawFocusRect(ScreenDC, MoveRect);
    Moving := true;
  end;
end;

procedure THsGradientPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if (MoveWho <> mwNone) and Moving then
  begin
    DrawFocusRect(ScreenDC, MoveRect);
    OldX := X;
    OldY := Y;
    if MoveWho = mwParent then
      MoveRect := Rect(Parent.Left + OldX - OldLeft, Parent.Top + OldY - OldTop,
        Parent.Left + Parent.Width + OldX - OldLeft, Parent.Top + Parent.Height
        + OldY - OldTop)
    else
      MoveRect := Rect(Self.Left + OldX - OldLeft, Self.Top + OldY - OldTop,
        Self.Left + Self.Width + OldX - OldLeft, Self.Top + Self.Height + OldY
        - OldTop);
    DrawFocusRect(ScreenDC, MoveRect);
  end;

end;

procedure THsGradientPanel.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Moving = true then
  begin
    if (MoveWho <> mwNone) and (Button = mbleft) then
    begin
      ReleaseCapture;
      DrawFocusRect(ScreenDC, MoveRect);
      if MoveWho = mwParent then
      begin
        if (Parent.Left <> Parent.Left + X + OldLeft) or
          (Parent.Top <> Parent.Top + Y - OldTop) then
        begin
          Parent.Visible := False;
          Parent.Left := Parent.Left + X - OldLeft;
          Parent.Top := Parent.Top + Y - OldTop;
          Parent.Visible := true;
        end
      end
      else
      begin
        if (Self.Left <> Self.Left + X + OldLeft) or
          (Self.Top <> Self.Top + Y - OldTop) then
        begin
          Self.Visible := False;
          Self.Left := Self.Left + X - OldLeft;
          Self.Top := Self.Top + Y - OldTop;
          Self.Visible := true;
        end
      end;
      ReleaseDC(0, ScreenDC);
    end;
    Moving := False;
  end;
end;

procedure THsGradientPanel.PaintCaption;
var
  OffSet: Integer;
begin
  bmpGradient.Canvas.Font := Font;
  bmpGradient.Canvas.Font.Color := Font.Color;
  bmpGradient.Canvas.Brush.Style := bsClear;
  OffSet := 0;
  if BevelInner <> bvNone then
    inc(OffSet, BevelWidth);
  if BevelOuter <> bvNone then
    inc(OffSet, BevelWidth);
  if Alignment = taCenter then
    bmpGradient.Canvas.TextOut
      (((bmpGradient.Width - bmpGradient.Canvas.TextWidth(Caption)) div 2),
      ((bmpGradient.Height - bmpGradient.Canvas.TextHeight(Caption))
      div 2), Caption);
  if Alignment = taLeftJustify then
    bmpGradient.Canvas.TextOut(OffSet,
      ((bmpGradient.Height - bmpGradient.Canvas.TextHeight(Caption))
      div 2), Caption);
  if Alignment = taRightJustify then
    bmpGradient.Canvas.TextOut
      ((bmpGradient.Width - bmpGradient.Canvas.TextWidth(Caption) - OffSet),
      ((bmpGradient.Height - bmpGradient.Canvas.TextHeight(Caption))
      div 2), Caption);
end;

procedure THsGradientPanel.PaintBevel;
var
  LineCount, AdjustedLineCount, MaxY, MaxX: Integer;
  TopColor, BottomColor: TColor;
begin
  MaxY := Height - 1;
  MaxX := Width - 1;
  TopColor := clBtnHighlight;
  BottomColor := clBtnShadow;
  if (BevelOuter <> bvNone) then
  begin
    if (BevelOuter = bvLowered) then
    begin
      TopColor := clBtnShadow;
      BottomColor := clBtnHighlight;
    end;
    for LineCount := 0 to (BevelWidth - 1) do
    begin
      bmpGradient.Canvas.Pen.Color := TopColor;
      bmpGradient.Canvas.MoveTo(LineCount, MaxY - LineCount);
      bmpGradient.Canvas.LineTo(LineCount, LineCount);
      bmpGradient.Canvas.LineTo(MaxX - LineCount, LineCount);
      bmpGradient.Canvas.Pen.Color := BottomColor;
      bmpGradient.Canvas.LineTo(MaxX - LineCount, MaxY - LineCount);
      bmpGradient.Canvas.LineTo(LineCount, MaxY - LineCount);
    end;
  end;

  if (BevelInner <> bvNone) then
  begin
    TopColor := clBtnHighlight;
    BottomColor := clBtnShadow;
    if (BevelInner = bvLowered) then
    begin
      TopColor := clBtnShadow;
      BottomColor := clBtnHighlight;
    end;
    if (BevelOuter <> bvNone) then
    begin
      MaxY := MaxY - BevelWidth;
      MaxX := MaxX - BevelWidth;
    end;
    for LineCount := 0 to (BevelWidth - 1) do
    begin
      AdjustedLineCount := LineCount;
      if (BevelOuter <> bvNone) then
        AdjustedLineCount := LineCount + BevelWidth;
      bmpGradient.Canvas.Pen.Color := TopColor;
      bmpGradient.Canvas.MoveTo(AdjustedLineCount, MaxY - LineCount);
      bmpGradient.Canvas.LineTo(AdjustedLineCount, AdjustedLineCount);
      bmpGradient.Canvas.LineTo(MaxX - LineCount, AdjustedLineCount);
      bmpGradient.Canvas.Pen.Color := BottomColor;
      bmpGradient.Canvas.LineTo(MaxX - LineCount, MaxY - LineCount);
      bmpGradient.Canvas.LineTo(AdjustedLineCount, MaxY - LineCount);
    end;
  end;
end;

procedure THsGradientPanel.SetBeginColor(Value: TColor);
begin
  if FBeginColor <> Value then
  begin
    FBeginColor := Value;
    if FBeginColor < 0 then
      FRealBeginColor := GetSysColor(FBeginColor and $000000FF)
      // added by HickelSOFT
    else
      FRealBeginColor := FBeginColor;
    Invalidate;
  end;
end;

procedure THsGradientPanel.SetEndColor(Value: TColor);
begin
  if FEndColor <> Value then
  begin
    FEndColor := Value;
    if FEndColor < 0 then
      FRealEndColor := GetSysColor(FEndColor and $000000FF)
      // added by HickelSOFT
    else
      FRealEndColor := FEndColor;
    Invalidate;
  end;
end;

procedure THsGradientPanel.SetGradientStyle(Value: TGradientStyle);
begin
  if FGradientStyle <> Value then
  begin
    FGradientStyle := Value;
    Invalidate;
  end;
end;

procedure THsGradientPanel.PaintHorizontal(br, bg, bb, dr, dg, db: Integer);
var
  RectGradient: TRect;
  I: Integer;
  r, g, b: Byte;
begin
  RectGradient.Top := 0;
  RectGradient.Bottom := Height;
  for I := 0 to 255 do
  begin
    RectGradient.Left := MulDiv(I, Width, 256);
    RectGradient.Right := MulDiv(I + 1, Width, 256);
    r := br + MulDiv(I, dr, 255);
    g := bg + MulDiv(I, dg, 255);
    b := bb + MulDiv(I, db, 255);
    bmpGradient.Canvas.Brush.Color := RGB(r, g, b);
    bmpGradient.Canvas.FillRect(RectGradient);
  end;
end;

procedure THsGradientPanel.PaintVertical(br, bg, bb, dr, dg, db: Integer);
var
  RectGradient: TRect;
  I: Integer;
  r, g, b: Byte;
begin
  RectGradient.Left := 0;
  RectGradient.Right := Width;
  for I := 0 to 255 do
  begin
    RectGradient.Top := MulDiv(I, Height, 256);
    RectGradient.Bottom := MulDiv(I + 1, Height, 256);
    r := br + MulDiv(I, dr, 255);
    g := bg + MulDiv(I, dg, 255);
    b := bb + MulDiv(I, db, 255);
    bmpGradient.Canvas.Brush.Color := RGB(r, g, b);
    bmpGradient.Canvas.FillRect(RectGradient);
  end;
end;

procedure THsGradientPanel.PaintElliptic(br, bg, bb, dr, dg, db: Integer);
var
  I: Integer;
  r, g, b: Byte;
  Pw, Ph: Real;
  x1, y1, x2, y2: Real;
begin
  bmpGradient.Canvas.Pen.Style := psClear;
  bmpGradient.Canvas.Pen.Mode := pmCopy;
  x1 := 0 - (bmpGradient.Width / 4);
  x2 := bmpGradient.Width + (bmpGradient.Width / 4);
  y1 := 0 - (bmpGradient.Height / 4);
  y2 := bmpGradient.Height + (bmpGradient.Height / 4);
  Pw := ((bmpGradient.Width / 4) + (bmpGradient.Width / 2)) / 155;
  Ph := ((bmpGradient.Height / 4) + (bmpGradient.Height / 2)) / 155;
  for I := 0 to 155 do
  begin
    x1 := x1 + Pw;
    x2 := x2 - Pw;
    y1 := y1 + Ph;
    y2 := y2 - Ph;
    r := br + MulDiv(I, dr, 155);
    g := bg + MulDiv(I, dg, 155);
    b := bb + MulDiv(I, db, 155);
    bmpGradient.Canvas.Brush.Color := r or (g shl 8) or (b shl 16);
    bmpGradient.Canvas.Ellipse(Trunc(x1), Trunc(y1), Trunc(x2), Trunc(y2));
  end;
  bmpGradient.Canvas.Pen.Style := psSolid;
end;

procedure THsGradientPanel.PaintRectangle(br, bg, bb, dr, dg, db: Integer);
var
  I: Integer;
  r, g, b: Byte;
  Pw, Ph: Real;
  x1, y1, x2, y2: Real;
begin
  bmpGradient.Canvas.Pen.Style := psClear;
  bmpGradient.Canvas.Pen.Mode := pmCopy;
  x1 := 0;
  x2 := bmpGradient.Width;
  y1 := 0;
  y2 := bmpGradient.Height;
  Pw := (bmpGradient.Width / 2) / 255;
  Ph := (bmpGradient.Height / 2) / 255;
  for I := 0 to 255 do
  begin
    x1 := x1 + Pw;
    x2 := x2 - Pw;
    y1 := y1 + Ph;
    y2 := y2 - Ph;
    r := br + MulDiv(I, dr, 255);
    g := bg + MulDiv(I, dg, 255);
    b := bb + MulDiv(I, db, 255);
    bmpGradient.Canvas.Brush.Color := RGB(r, g, b);
    bmpGradient.Canvas.FillRect(Rect(Trunc(x1), Trunc(y1), Trunc(x2),
      Trunc(y2)));
  end;
  bmpGradient.Canvas.Pen.Style := psSolid;
end;

procedure THsGradientPanel.PaintSolid(r, g, b: Integer); // added by HickelSOFT
var
  RectGradient: TRect;
begin
  RectGradient.Top := 0;
  RectGradient.Bottom := Height;
  RectGradient.Left := 0;
  RectGradient.Right := Width;
  bmpGradient.Canvas.Brush.Color := RGB(r, g, b);
  bmpGradient.Canvas.FillRect(RectGradient);
end;

procedure THsGradientPanel.PaintVerticalCenter(br, bg, bb, dr, dg, db: Integer);
var
  RectGradient: TRect;
  I: Integer;
  r, g, b: Byte;
  Half: Integer;
begin
  Half := bmpGradient.Height Div 2;
  RectGradient.Left := 0;
  RectGradient.Right := bmpGradient.Width;
  for I := 0 to Half do
  begin
    RectGradient.Top := MulDiv(I, Half, Half);
    RectGradient.Bottom := MulDiv(I + 1, Half, Half);
    r := br + MulDiv(I, dr, Half);
    g := bg + MulDiv(I, dg, Half);
    b := bb + MulDiv(I, db, Half);
    bmpGradient.Canvas.Brush.Color := RGB(r, g, b);
    bmpGradient.Canvas.FillRect(RectGradient);
    RectGradient.Top := bmpGradient.Height - (MulDiv(I, Half, Half));
    RectGradient.Bottom := bmpGradient.Height - (MulDiv(I + 1, Half, Half));
    bmpGradient.Canvas.FillRect(RectGradient);
  end;
end;

procedure THsGradientPanel.PaintHorizontalCenter(br, bg, bb, dr, dg,
  db: Integer);
var
  RectGradient: TRect;
  I: Integer;
  r, g, b: Byte;
  Half: Integer;
begin
  Half := bmpGradient.Width Div 2;
  RectGradient.Top := 0;
  RectGradient.Bottom := bmpGradient.Height;
  for I := 0 to Half do
  begin
    RectGradient.Left := MulDiv(I, Half, Half);
    RectGradient.Right := MulDiv(I + 1, Half, Half);
    r := br + MulDiv(I, dr, Half);
    g := bg + MulDiv(I, dg, Half);
    b := bb + MulDiv(I, db, Half);
    bmpGradient.Canvas.Brush.Color := RGB(r, g, b);
    bmpGradient.Canvas.FillRect(RectGradient);
    RectGradient.Left := bmpGradient.Width - (MulDiv(I, Half, Half));
    RectGradient.Right := bmpGradient.Width - (MulDiv(I + 1, Half, Half));
    bmpGradient.Canvas.FillRect(RectGradient);
  end;
end;

destructor THsGradientPanel.Destroy;
begin
  FreeAndNil(bmpGradient);
  inherited Destroy;
end;

procedure THsGradientPanel.EndUpdate; // added by HickelSOFT
begin
  FInUpdate := False;
  Invalidate;
end;

procedure Register;
begin
  RegisterComponents('Hs', [THsGradientPanel]);
end;

end.
