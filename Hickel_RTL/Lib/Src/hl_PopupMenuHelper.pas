unit hl_PopupMenuHelper;

interface

uses
  Windows, Menus, Controls, Classes, System.Types;

type
  TPopupMenuHelper = class helper for TPopupMenu
    procedure OpenPopupOnControl(ctl: TControl;
      OnlyIfAutopopup: boolean = True);
  end;

implementation

procedure TPopupMenuHelper.OpenPopupOnControl(ctl: TControl;
  OnlyIfAutopopup: boolean = True);
var
  lPoint: TPoint;
  oPoint: TPoint;
begin
  if OnlyIfAutopopup and not Self.AutoPopup then
    exit;

  (*

    //Woll2Woll macht das so:

    p:= FormatBar.ClientToScreen(Point(ColorButton.left, ColorButton.Top + ColorButton.Height));

    TrackPopupMenu(PopupMenu1.Handle, TPM_LEFTALIGN,
    p.x-1, p.y, 0, RichEditBar.Parent.handle, nil);
    //     p.x-1, p.y, 0, handle, nil);

  *)

  oPoint := ctl.ClientToScreen(Point(0, 0));
  if (Mouse.CursorPos.X < oPoint.X) or
    (Mouse.CursorPos.X > oPoint.X + ctl.Width) or (Mouse.CursorPos.Y < oPoint.Y)
    or (Mouse.CursorPos.Y > oPoint.Y + ctl.Height) then
  begin
    // Button möglicherweise per Tastatur bedient (Enter)
    lPoint := ctl.ClientToScreen(Point(ctl.Width div 2, ctl.Height div 2));
  end
  else
  begin
    // Button möglicherweise mit Maus bedient
    lPoint := Mouse.CursorPos;
  end;
  Self.Popup(lPoint.X, lPoint.Y);
end;

end.
