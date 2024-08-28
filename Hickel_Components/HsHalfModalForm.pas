unit HsHalfModalForm;

interface

uses
  Windows, Messages, Classes, Controls, Forms, Consts;

type
  TModalContext = record
    WindowList: Pointer;
    SaveFocusState: TFocusState;
    SaveCursor: TCursor;
    SaveCount: Integer;
    ActiveWindow: HWnd;
  end;

  TForm_ = class helper for TForm
    /// <summary>Öffnet das Form als Modal, aber lässt den weiteren Programmflus zu.</summary>
    function ShowModalStart: TModalContext;

    /// <summary>Schließt ein geöffnetes modales Fenster</summary>
    function ShowModalEnd(mc: TModalContext): TModalResult;
  end;

implementation

{ TForm_ }

function TForm_.ShowModalEnd(mc: TModalContext): TModalResult;
begin
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
    Screen.FocusedForm := Screen.SaveFocusedList.First;
    Screen.SaveFocusedList.Remove(Screen.FocusedForm);
  end else Screen.FocusedForm := nil;
  if mc.ActiveWindow <> 0 then SetActiveWindow(mc.ActiveWindow);
  RestoreFocusState(mc.SaveFocusState);
  Exclude(FFormState, fsModal);

  Application.ModalFinished;
end;

function TForm_.ShowModalStart: TModalContext;
begin
  // Quelltext wurde von Delphi 2007 Forms.pas entnommen und modifiziert (aufgesplittet um TModalContext hinzugefügt)
  
  CancelDrag;
  if Visible or not Enabled or (fsModal in FFormState) or
    (FormStyle = fsMDIChild) then
    raise EInvalidOperation.Create(SCannotShowModal);
  if GetCapture <> 0 then SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
  ReleaseCapture;
  Application.ModalStarted;

  Include(FFormState, fsModal);
  if (PopupMode = pmNone) and (Application.ModalPopupMode <> pmNone) then
  begin
    RecreateWnd;
    HandleNeeded;
  end;
  result.ActiveWindow := GetActiveWindow;
  result.SaveFocusState := Forms.SaveFocusState;
  Screen.SaveFocusedList.Insert(0, Screen.FocusedForm);
  Screen.FocusedForm := Self;
  result.SaveCursor := Screen.Cursor;
  Screen.Cursor := crDefault;
  result.SaveCount := Screen.CursorCount;
  result.WindowList := DisableTaskWindows(0);

  Show;

  SendMessage(Handle, CM_ACTIVATE, 0, 0);
  ModalResult := 0;

  Application.ProcessMessages;
end;

end.
