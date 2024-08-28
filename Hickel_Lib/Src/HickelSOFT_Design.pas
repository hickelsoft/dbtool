unit HickelSOFT_Design;

interface

uses
  Windows, Forms, Controls, ExtCtrls, Graphics, Menus, HsGradientPanel;

type
  THickelSOFTDesign = class(TObject)
  class var
    DESIGN62_PANELCOLOR: TColor;
    DESIGN62_PANELCOLORHIGHLIGHT: TColor;
    DESIGN62_PANELCOLORTEXT: TColor;
    DESIGN62_GRAYNESS: integer; // Um wie viel "heller" sollen deaktivierte Schriften werden?
    Design62_MenuBrushHandle: THandle;
  strict private
    class procedure MainMenuAdvancedDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState);
    class procedure MainMenuMeasureItem(Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);
    class procedure TimerTick(Sender: TObject);
  strict protected
    class function FindMainMenu(frm: TForm): TMainMenu;
  public
    class procedure HS_Design_Aktivieren(AllowDarkMode: boolean=True); // call at FormShow
    class procedure PanelObenBearbeiten(PanelOben: THsGradientPanel); // call at FormShow, after HS_Design_Aktivieren
    class procedure RefreshMainMenuColor(MainMenu1: TMainMenu); // call at FormCanResize
    class procedure ZeichneHintergrundschrift(text1_normal, text2_kursiv: string; minScale: double); // call at FormPaint
    class procedure WindowMenuItemClick(Sender: TObject); // use at OnClick of the "Window" menu item
    class procedure MainMenuSetAdvancedDrawItem(AMenu: TMenuItem); // needs to be applied if submenu items have been added
  end;

implementation

uses
  hl.Utils, hl.Utils.Color, ProgrDlg, Classes, SysUtils;

procedure ReplaceLineUnderMainMenu(frm: TForm; color: TColor);
var
  Ahwnd: HWND;
  mbifo: TMenuBarInfo;
  RW, r2: TRect;
  dc: HDC;
  br: HBRUSH;
  windowtop: Integer;
  inactiveborder: COLORREF;
begin
  Ahwnd := frm.Handle;
  inactiveborder := ColorToRGB(color);
  fillchar(mbifo,sizeof(mbifo),#0);
  mbifo.cbSize:=sizeof(mbifo);
  {$RANGECHECKS OFF}
  if GetMenuBarInfo(Ahwnd,OBJID_MENU,0,mbifo) then
  begin
    Ahwnd := 0;
    windows.GetWindowRect(Ahwnd,RW);
    windowtop:=0;//rw.top;
    MapWindowPoints(0,Ahwnd,RW,2);
    OffsetRect(RW,-RW.Left,-RW.Top);
    dc := GetWindowDC(Ahwnd);
    br:=CreateSolidBrush(inactiveborder);
    r2:=RW;
    r2.top:=mbifo.rcBar.bottom-windowtop;
    r2.bottom:=r2.top+1;
    FillRect(dc,r2,br);
    DeleteObject(br);
    ReleaseDC(Ahwnd, dc);
  end;
  {$RANGECHECKS ON}
end;

class procedure THickelSOFTDesign.ZeichneHintergrundschrift(text1_normal, text2_kursiv: string; minScale: double);
var
  coraFontColor: TColor;
  frm: TForm;

  procedure _ChangeToText1(size: integer);
  begin
    frm.Canvas.Font.Color := coraFontColor;
    frm.Canvas.Font.Name := 'Arial';
    frm.Canvas.Font.Height := -size;
    frm.Canvas.Font.Style := [fsBold];
  end;

  procedure _ChangeToText2(size: integer);
  begin
    frm.Canvas.Font.Color := coraFontColor;
    frm.Canvas.Font.Name := 'Arial';
    frm.Canvas.Font.Height := -size;
    frm.Canvas.Font.Style := [fsBold, fsItalic];
  end;

var
  x1, x2, y1, y2, w1, w2, h1, h2, size: Int64;
begin
  if OpenedProgressDlgs > 0 then exit; // wegen Performance sehr wichtig

  frm := Application.MainForm;

  coraFontColor := IncreaseColorLightness(frm.Color, 20);

  frm.Canvas.Lock;
  try
    size := 10;
    repeat
      _ChangeToText1(size);
      w1 := frm.Canvas.TextWidth(text1_normal);
      _ChangeToText2(size);
      w2 := frm.Canvas.TextWidth(text2_kursiv);
      Inc(size);
      if size > 1000 then exit; // irgendwas läuft schief. Notbremse ziehen.
    until (w1+w2)/frm.ClientWidth > minScale;

    _ChangeToText1(size);
    w1 := frm.Canvas.TextWidth(text1_normal);
    h1 := frm.Canvas.TextHeight(text1_normal);

    _ChangeToText2(size);
    w2 := frm.Canvas.TextWidth(text2_kursiv);
    h2 := frm.Canvas.TextHeight(text2_kursiv);

    x1 := frm.ClientWidth div 2 - (w1+w2) div 2;
    y1 := frm.ClientHeight div 2 - h1 div 2;
    x2 := x1 + w1;
    y2 := frm.ClientHeight div 2 - h2 div 2;

    _ChangeToText1(size);
    frm.Canvas.TextOut(x1, y1, text1_normal);
    _ChangeToText2(size);
    frm.Canvas.TextOut(x2, y2, text2_kursiv);
  finally
    frm.Canvas.Unlock;
  end;
end;

class procedure THickelSOFTDesign.RefreshMainMenuColor(MainMenu1: TMainMenu);
var
  lMenuInfo: TMenuInfo;
  lMenuColor: TColor;
begin
  lMenuColor := DESIGN62_PANELCOLOR;

  DeleteObject(Design62_MenuBrushHandle);
  Design62_MenuBrushHandle := CreateSolidBrush(ColorToRGB(lMenuColor));

  FillChar(lMenuInfo, SizeOf(lMenuInfo), 0);

  lMenuInfo.cbSize := SizeOf(lMenuInfo);
  lMenuInfo.hbrBack := Design62_MenuBrushHandle;
  lMenuInfo.fMask := MIM_BACKGROUND;
  SetMenuInfo(MainMenu1.Handle, lMenuInfo);
end;

class procedure THickelSOFTDesign.TimerTick(Sender: TObject);
begin
  // Funktioniert im OnPaint vom Form nicht! (TODO: geht es nicht doch irgendwie anders?)
  ReplaceLineUnderMainMenu(Application.MainForm, DESIGN62_PANELCOLOR);
end;

class procedure THickelSOFTDesign.MainMenuAdvancedDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState);
var
  text: string;
  attrMain: integer;
  attrShortcut: integer;
  isRTL: boolean;
  attrAdd: integer;
  IstUntermenue: boolean;
  tmp: TObject;
begin
  // TODO: bei einem enabled=false eintrag ist trotzdem für der pfeil-rechts für das submenü
  // TODO: Support für das Zeichnen von Symbolen
  // QUE: bei RTL sind die Pfeile im Weg sodass man kein Icon zeichnen kann?

  tmp := Sender;
  while TMenuItem(tmp).Parent is TMenuItem do
    tmp := TMenuItem(tmp).Parent;
  if not (tmp is TMainMenu) then
    tmp := FindMainMenu(Application.MainForm); // should not happen
  isRTL := not (TMainMenu(tmp).BidiMode = bdLeftToRight); // TODO: Do we need to do something about ParentBiDiMode?

  if TMenuItem(Sender).Enabled and ((odHotLight in State) or (odSelected in State)) then
    ACanvas.Brush.Color := DESIGN62_PANELCOLORHIGHLIGHT
  else
    ACanvas.Brush.Color := DESIGN62_PANELCOLOR;
  ACanvas.FillRect(ARect);

  IstUntermenue := Assigned(TMenuItem(Sender).Parent) and (TMenuItem(Sender).Parent.Caption <> '');
  if IstUntermenue then
  begin
    if isRTL then
    begin
      Dec(ARect.Right,24);
      Inc(ARect.Left,4);
    end
    else
    begin
      Inc(ARect.Left,24);
      Dec(ARect.Right,4);
    end;
    attrAdd := 0;
  end
  else
  begin
    attrAdd := DT_CENTER;
  end;
  if TMenuItem(Sender).Enabled then
    ACanvas.Font.Color := DESIGN62_PANELCOLORTEXT
  else
    ACanvas.Font.Color := IncreaseColorLightness(GetSysColor(DESIGN62_PANELCOLORTEXT and $00FFFFFF), DESIGN62_GRAYNESS);

  if TMenuItem(Sender).IsLine then
  begin
    ACanvas.Pen.Color := IncreaseColorLightness(GetSysColor(DESIGN62_PANELCOLORTEXT and $00FFFFFF), DESIGN62_GRAYNESS);
    ACanvas.MoveTo(ARect.Left,    ARect.Top + (ARect.Bottom-ARect.Top) div 2);
    ACanvas.LineTo(ARect.Right-4, ARect.Top + (ARect.Bottom-ARect.Top) div 2);
  end
  else
  begin
    if isRTL then
    begin
      ACanvas.Font.Charset := ARABIC_CHARSET;
      attrMain := DT_RTLREADING or DT_RIGHT;
      attrShortcut := DT_RTLREADING or DT_LEFT;
    end
    else
    begin
      attrMain := DT_LEFT;
      attrShortcut := DT_RIGHT;
    end;

    text := TMenuItem(Sender).Caption;
    if text <> '' then
    begin
      DrawText(ACanvas.Handle, PChar(text),Length(text),ARect,attrMain or attrAdd or DT_SINGLELINE or DT_VCENTER);
    end;

    text := ShortCutToText(TMenuItem(Sender).ShortCut);
    if (text <> '') and (IstUntermenue) then
    begin
      ACanvas.Font.Color := IncreaseColorLightness(GetSysColor(DESIGN62_PANELCOLORTEXT and $00FFFFFF), DESIGN62_GRAYNESS);
      ARect.Right := ARect.Right - 5; // damit es nicht so gequetscht ist
      DrawText(ACanvas.Handle, PChar(text),Length(text),ARect,attrShortcut or attrAdd or DT_SINGLELINE or DT_VCENTER);
    end;

    if TMenuItem(Sender).Checked and IstUntermenue then
    begin
      ACanvas.Font.Color := DESIGN62_PANELCOLORTEXT;
      ACanvas.Font.Name := 'Wingdings';
      ACanvas.Font.Size := ACanvas.Font.Size + 2;
      text := 'w'; // Haken   (TODO: wenn WingDings nicht existiert, kommt hier keine Raute, sondern ein 'w')

      if isRTL then
      begin
        Inc(ARect.Right, 18);
        ARect.Left := 24;
      end
      else
      begin
        Dec(ARect.Left, 18);
        ARect.Right := 24;
      end;
      ACanvas.Font.Charset := DEFAULT_CHARSET; // important, otherwise "Wingdings" won'T work
      if isRTL then
        DrawText(ACanvas.Handle, PChar(text),Length(text),ARect,DT_RIGHT or DT_SINGLELINE or DT_VCENTER)
      else
        DrawText(ACanvas.Handle, PChar(text),Length(text),ARect,DT_LEFT or DT_SINGLELINE or DT_VCENTER);
    end;
  end;
end;

class procedure THickelSOFTDesign.MainMenuMeasureItem(Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);
var
  IstUntermenue: Boolean;
begin
  IstUntermenue := Assigned(TMenuItem(Sender).Parent) and (TMenuItem(Sender).Parent.Caption <> '');
  if not IstUntermenue then
  begin
    // Workaround für einen Bug in Delphi: Wenn OwnerDraw eingeschaltet ist, dann sind die
    // Haupteinträge im MainMenu, die einen Shortcut haben, extrem breit (weil wohl der Shortcut mitgedruckt werden soll)
    // Deshalb muss Width explizit gesetzt werden.
    // Außerdem wird noch um 5% verschmälert, weil es sonst aus irgendeinem Grund viel zu breit ist. (TextWidth ist falsch?)
    width := round(ACanvas.TextWidth(TMenuItem(Sender).Caption) * 0.95);
  end
  else
  begin
    // damit es nicht so gequetscht ist
    width := round(width * 1.1);
    height := round(height * 1.1);
  end;
end;

class procedure THickelSOFTDesign.MainMenuSetAdvancedDrawItem(AMenu: TMenuItem);
var
  i: integer;
begin
  AMenu.OnAdvancedDrawItem := Self.MainMenuAdvancedDrawItem;
  AMenu.OnMeasureItem := Self.MainMenuMeasureItem;
  for i := AMenu.Count - 1 downto 0 do
  begin
    AMenu[i].OnAdvancedDrawItem := Self.MainMenuAdvancedDrawItem;
    AMenu[i].OnMeasureItem := Self.MainMenuMeasureItem;
    MainMenuSetAdvancedDrawItem(AMenu[i]);
  end;
end;

class procedure THickelSOFTDesign.PanelObenBearbeiten(PanelOben: THsGradientPanel);
begin
  PanelOben.BevelOuter := bvNone;
  PanelOben.BeginColor := DESIGN62_PANELCOLOR;
  PanelOben.EndColor := DESIGN62_PANELCOLORHIGHLIGHT;
end;

class function THickelSOFTDesign.FindMainMenu(frm: TForm): TMainMenu;
var
  i: Integer;
begin
  result := nil;
  for i := 0 to frm.ComponentCount-1 do
  begin
    if frm.Components[i] is TMainMenu then
    begin
      result := frm.Components[i] as TMainMenu;
      break;
    end;
  end;
end;

class procedure THickelSOFTDesign.HS_Design_Aktivieren(AllowDarkMode: boolean=True);
var
  MainMenu1: TMainMenu;
  frm: TForm;
  //tm: TTimer;
begin
  frm := Application.MainForm;

  // Das sieht einfach nicht gut aus! (TODO)
  (*
  tm := TTimer.Create(frm);
  tm.Interval := 10;
  tm.OnTimer := TimerTick;
  tm.Enabled := true;
  *)

  MainMenu1 := FindMainMenu(frm);

  if AllowDarkMode and DarkModeIsEnabled then
  begin
    DESIGN62_PANELCOLOR := $00333333;
    DESIGN62_PANELCOLORHIGHLIGHT := $00888888;
    DESIGN62_PANELCOLORTEXT := $00EEEEEE;
    if frm.FormStyle = fsMDIForm then frm.Color := clBlack;
  end
  else
  begin
    DESIGN62_PANELCOLOR := $00FFFFFF;
    DESIGN62_PANELCOLORHIGHLIGHT := $00FFE7C6;
    DESIGN62_PANELCOLORTEXT := $00222222;
    if frm.FormStyle = fsMDIForm then frm.Color := clBlack{ $00C0AC97 };
  end;

  DESIGN62_GRAYNESS := 120; // Um wie viel "heller" sollen deaktivierte Schriften werden?

  if Assigned(MainMenu1) then
  begin
    MainMenuSetAdvancedDrawItem(MainMenu1.Items);
    RefreshMainMenuColor(MainMenu1);
    MainMenu1.OwnerDraw := true;

    if MainMenu1.Items.Count > 0 then
    begin
      // Dieser seltsame Workaround ist notwendig, damit es funktioniert, wenn man OwnerDraw zur Laufzeit setzt.
      // (Getestet mit Delphi 2007)
      MainMenu1.Items[0].Visible := false;
      MainMenu1.Items[0].Visible := true;
    end;
  end;
end;

class procedure THickelSOFTDesign.WindowMenuItemClick(Sender: TObject);
var
  mi: TMenuItem;
  i: integer;
  sl: TStringList;
  frm: TForm;
begin
  frm := Application.MainForm;
  if TMenuItem(Sender).Tag >= 100 then
  begin
    if Assigned(frm.MDIChildren[TMenuItem(Sender).Tag-100]) then
      MDI_Form_BringToFront(frm.MDIChildren[TMenuItem(Sender).Tag-100]);
  end
  else
  begin
    sl := TStringList.Create;
    try
      // Wir simulieren TForm.WindowMenu = Fenster1,
      // denn leider sorgt WindowMenu dafür, dass die Items direkt über die WinAPI
      // gezeichnet werden, und nicht als TMenuItem vorliegen. Folglich kann
      // Design62 nicht angewendet werden.
      for i := TMenuItem(Sender).Count - 1 downto 0 do
      begin
        if TMenuItem(Sender).Items[i].Tag = 99 then
        begin
          // Tag 99 ist ein optionales Menü-Item mit dem Titel "Keine Fenster offen" o.ä.
          TMenuItem(Sender).Items[i].Visible := frm.MDIChildCount = 0;
        end
        else if TMenuItem(Sender).Items[i].Tag >= 100 then
        begin
          {$IF CompilerVersion > 20.0} // Version geraten
          FreeAndNil(TMenuItem(Sender).Items[i]);
          {$ELSE}
          TMenuItem(Sender).Items[i].Free;
          {$IFEND}
        end;
      end;
      for i := 0 to frm.MDIChildCount - 1 do
      begin
        if Assigned(frm.MDIChildren[i]) then
        begin
          mi := TMenuItem.Create(TMenuItem(Sender));
          mi.Caption := frm.MDIChildren[i].Caption;
          mi.OnAdvancedDrawItem := Self.MainMenuAdvancedDrawItem;
          mi.OnMeasureItem := Self.MainMenuMeasureItem;
          mi.Tag := 100+i;
          mi.Checked := frm.MDIChildren[i].Active;
          mi.OnClick := WindowMenuItemClick;
          sl.AddObject(frm.MDIChildren[i].Caption, mi);
        end;
      end;
      sl.Sort;
      for i := 0 to sl.Count - 1 do
      begin
        TMenuItem(Sender).Add(TMenuItem(sl.Objects[i]));
      end;
    finally
      FreeAndNil(sl);
    end;
  end;
end;

end.
