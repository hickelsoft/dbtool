unit HsHalfModalForm;

interface

uses
  Windows, Messages, Classes, Controls, Forms, Consts, Vcl.Themes;

type
  TModalContext = record
    WindowList: TTaskWindowList;
    LSaveFocusState: TFocusState;
    SaveCursor: TCursor;
    SaveCount: Integer;
    ActiveWindow: HWnd;
    SaveHandle: HWND;
    SaveStyle: TCustomStyleServices;
  end;

  TForm_ = class helper for TForm
    /// <summary>Öffnet das Form als Modal, aber lässt den weiteren Programmflus zu.</summary>
    function ShowModalStart: TModalContext;

    /// <summary>Schließt ein geöffnetes modales Fenster</summary>
    function ShowModalEnd(mc: TModalContext): TModalResult;
  end;

implementation

{ TForm_ }

var
  // Quelltext wurde von Delphi 12 Forms.pas entnommen
  TaskActiveWindow: HWnd = 0;
  TaskFirstWindow: HWnd = 0;
  TaskFirstTopMost: HWnd = 0;

function DoFindWindow(Window: HWnd; Param: LPARAM): Bool; {$IFNDEF CLR}stdcall;{$ENDIF}
begin
  // Quelltext wurde von Delphi 12 Forms.pas entnommen

  if (Window <> TaskActiveWindow) and (Window <> Application.Handle) and
    IsWindowVisible(Window) and IsWindowEnabled(Window) then
    if GetWindowLong(Window, GWL_EXSTYLE) and WS_EX_TOPMOST = 0 then
    begin
      if TaskFirstWindow = 0 then TaskFirstWindow := Window;
    end else
    begin
      if TaskFirstTopMost = 0 then TaskFirstTopMost := Window;
    end;
  Result := True;
end;

function FindTopMostWindow(ActiveWindow: HWnd): HWnd;
var
  EnumProc: TFNWndEnumProc; // keep a reference to the delegate!
begin
  // Quelltext wurde von Delphi 12 Forms.pas entnommen

  TaskActiveWindow := ActiveWindow;
  TaskFirstWindow := 0;
  TaskFirstTopMost := 0;
  EnumProc := @DoFindWindow;
  EnumThreadWindows(GetCurrentThreadID, EnumProc, 0);
  if TaskFirstWindow <> 0 then
    Result := TaskFirstWindow else
    Result := TaskFirstTopMost;
end;


function TForm_.ShowModalStart: TModalContext;
begin
  // Quelltext wurde von Delphi 12 Forms.pas ShowModal() entnommen und modifiziert (aufgesplittet, und TModalContext hinzugefügt)

  CancelDrag;
  if Visible or not Enabled or (fsModal in FFormState) or
    (FormStyle = fsMDIChild) then
    raise EInvalidOperation.Create(SCannotShowModal);
  if GetCapture <> 0 then SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
  ReleaseCapture;
  Application.ModalStarted;

  { RecreateWnd could change the active window }
  result.ActiveWindow := GetActiveWindow;
  Include(FFormState, fsModal);
  if (PopupMode = pmNone) and (Application.ModalPopupMode <> pmNone) then
  begin
    RecreateWnd;
    HandleNeeded;
    { The active window might have become invalid, refresh it }
    if (result.ActiveWindow = 0) or not IsWindow(result.ActiveWindow) then
      result.ActiveWindow := GetActiveWindow;
  end;
  result.LSaveFocusState := SaveFocusState;
  result.LSaveFocusState := Forms.SaveFocusState;
  Screen.SaveFocusedList.Insert(0, Screen.FocusedForm);
  Screen.FocusedForm := Self;
  result.SaveCursor := Screen.Cursor;
  Screen.Cursor := crDefault;
  result.SaveCount := Screen.CursorCount;
  result.WindowList := DisableTaskWindows(0);
  result.SaveHandle := Handle;
  result.SaveStyle := TStyleManager.ActiveStyle;

  Show;

  SendMessage(Handle, CM_ACTIVATE, 0, 0);
  ModalResult := 0;

  Application.ProcessMessages;
end;

function TForm_.ShowModalEnd(mc: TModalContext): TModalResult;
begin
  // Quelltext wurde von Delphi 12 Forms.pas ShowModal() entnommen und modifiziert (aufgesplittet, und TModalContext hinzugefügt)

//        repeat
          if (mc.SaveHandle <> Handle) and (Screen.ActiveCustomForm = Self) and (mc.SaveStyle <> TStyleManager.ActiveStyle) then
          begin
            mc.SaveHandle := Handle;
            mc.SaveStyle := TStyleManager.ActiveStyle;
            Visible := False;
            try
              EnableTaskWindows(mc.WindowList);
              try
                Application.ProcessMessages;
              finally
                mc.WindowList := DisableTaskWindows(0);
              end;
            finally
              Visible := True;
            end;
          end;
          Application.HandleMessage;
          if Application.Terminated then ModalResult := mrCancel else
            if ModalResult <> 0 then CloseModal;
//        until ModalResult <> 0;

  result := ModalResult;

  SendMessage(Handle, CM_DEACTIVATE, 0, 0);
  if GetActiveWindow <> Handle then mc.ActiveWindow := 0;

  Hide;

  if Screen.CursorCount = mc.SaveCount then
  Screen.Cursor := mc.SaveCursor
  else Screen.Cursor := crDefault;
  EnableTaskWindows(mc.WindowList);
  if Screen.SaveFocusedList.Count > 0 then
  begin
    Screen.FocusedForm := TCustomForm(Screen.SaveFocusedList.First);
    Screen.SaveFocusedList.Remove(Screen.FocusedForm);
  end else Screen.FocusedForm := nil;
  { ActiveWindow might have been destroyed and using it as active window will
    force Windows to activate another application }
  if (mc.ActiveWindow <> 0) and not IsWindow(mc.ActiveWindow) then
    mc.ActiveWindow := FindTopMostWindow(0);
  if mc.ActiveWindow <> 0 then
    SetActiveWindow(mc.ActiveWindow);
  RestoreFocusState(mc.LSaveFocusState);
  Exclude(FFormState, fsModal);

  Application.ModalFinished;
end;

end.
