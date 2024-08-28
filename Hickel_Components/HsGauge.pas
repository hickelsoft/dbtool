unit HsGauge;

interface

uses
  Windows, Gauges, Classes, Graphics, SysUtils, Forms;

type
   THsGaugeKind = (gkHorizontalBar, gkVerticalBar);

   {$IF CompilerVersion > 20.0} // Version geraten
   [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
   {$IFEND}
   THsGauge = class(TGauge)
   private
      FForeColor: TColor;
      FForeColor2: TColor;
      FKind: THsGaugeKind;
      PaintBitmap: TBitmap;
   public
      aBitmap: TBitmap;
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure Paint; override;
   protected
      procedure SetForeColor(cNew: TColor);
      procedure SetForeColor2(cNew: TColor);
      procedure SetKind(kNew: THsGaugeKind);
   published
      property DragMode;
      property DragCursor;
      property ForeColor: TColor read FForeColor write SetForeColor;
      property ForeColor2: TColor read FForeColor2 write SetForeColor2;
      property Kind: THsGaugeKind read FKind write SetKind;
   end;

procedure Register;

implementation

uses HsTools;

{------------------------------------------------------------------------------}

procedure Register;
begin
  RegisterComponents('Hs', [THsGauge]);
end;

{##############################################################################}

constructor THsGauge.Create(AOwner: TComponent);
begin
   inherited;
   aBitmap := TBitmap.Create;
   PaintBitmap := TBitmap.Create;
   Height := 20;
   Width := 128;
   FForeColor := clAqua;
   FForeColor2 := clNavy;
   FKind := gkHorizontalBar;
end;

{------------------------------------------------------------------------------}

destructor THsGauge.Destroy;
begin
   FreeAndNil(PaintBitmap);
   FreeAndNil(aBitmap);
   inherited;
end;

{------------------------------------------------------------------------------}

procedure THsGauge.Paint;
var
   aRect: TRect;
   pcBuffer: array[0..7] of char;

begin
   { disabled or invisible? Get out of here! }
   if (enabled and visible) = false then exit;
   if csLoading in Componentstate then exit;

   { has the size changed? If so, rebuild bitmap! }
   if (height <> aBitmap.Height) or (width <> aBitmap.Width) then
   begin
      CreateColorFadeBitmap(aBitmap, Height, Width, FForeColor, FForeColor2, (Kind = gkVerticalBar));
      PaintBitmap.Height := Height;
      PaintBitmap.Width := Width;
   end;

   { init }
   PaintBitmap.Canvas.Brush.Color := BackColor;

   { erase background }
   PaintBitmap.Canvas.Rectangle(-1, -1, Width+1, Height+1);

   if MaxValue <> MinValue then
   begin
      { print gauge }
      aRect.right := Width * (Progress-MinValue) div (MaxValue-MinValue);
      aRect.top := 0;
      aRect.left := 0;
      aRect.bottom := Height;
      PaintBitmap.Canvas.CopyRect(aRect, aBitmap.Canvas, aRect);

      { print text }
      if ShowText then
      begin
         aRect := ClientRect;
         PaintBitmap.Canvas.Font := Font;
         SetBkMode(PaintBitmap.Canvas.Handle, Transparent);
         StrPCopy(pcBuffer, format('%d%%', [100 * (Progress-MinValue) div (MaxValue-MinValue)]));
         DrawText(PaintBitmap.Canvas.Handle, pcBuffer, strlen(pcBuffer), aRect, DT_CENTER or DT_SINGLELINE or DT_VCENTER);
      end;
   end;

   { have a border? Paint it! }
   if BorderStyle = bsSingle then
   begin
      PaintBitmap.Canvas.Brush.Color := clBlack;
      PaintBitmap.Canvas.FrameRect(Rect(0, 0, Width, Height));
   end;

   { now copy the PaintBitmap to the real canvas! }
   Canvas.CopyRect(ClientRect, PaintBitmap.Canvas, ClientRect);
end;

{------------------------------------------------------------------------------}

procedure THsGauge.SetForeColor(cNew: TColor);
begin
   FForeColor := cNew;
   aBitmap.Width := 0;
   Paint;
end;

{------------------------------------------------------------------------------}

procedure THsGauge.SetForeColor2(cNew: TColor);
begin
   FForeColor2 := cNew;
   aBitmap.Width := 0;
   Paint;
end;

{------------------------------------------------------------------------------}

procedure THsGauge.SetKind(kNew: THsGaugeKind);
begin
   FKind := kNew;
   aBitmap.Width := 0;
   Paint;
end;

end.
