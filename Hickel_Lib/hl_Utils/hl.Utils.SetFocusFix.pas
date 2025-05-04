unit hl.Utils.SetFocusFix;

(*
  * Diese Unit hackt die Funktion "SetFocus" und ersetzt sie durch eine Variante,
  * die keine Exception auslöst. Wir haben nämlich an vielen Stellen im Programm
  * Bugs, bei denen eine solche Exception auslöst. Somit wird aus einem optischen
  * Problem gleich ein kompletter Ausfall der Funktion. Insbesondere bei der
  * CORAplus Handelsversion, bei der wir sehr viele Controls ausblenden, ist die
  * Gefahr groß, dass hier ein SetFocus vergessen wird und es damit zum Absturz kommt.
  * -- DM 07.12.2016
  *
  * Verwendung: Diese Unit wird einmal irgendwo im Projekt eingebunden,
  * am besten gleich im Hauptform.
*)

interface

implementation

uses
  Controls,
  Forms,
  SysUtils,
  Windows;

type
  TWinControlHack = class(TWinControl)
  public
    procedure SetFocus; override;
  end;

procedure TWinControlHack.SetFocus;
var
  Parent: TCustomForm;
begin
  if not CanFocus then
    Exit;

  Parent := GetParentForm(Self);
  if Parent <> nil then
    Parent.FocusControl(Self)
  else if ParentWindow <> 0 then
    Windows.SetFocus(Handle)
  else
    ValidParentForm(Self);
end;

procedure RedirectFunction(OrgProc, NewProc: Pointer);
type
  TJmpBuffer = packed record
    Jmp: Byte;
    Offset: Integer;
  end;
var
  n: UINT_PTR;
  JmpBuffer: TJmpBuffer;
begin
  JmpBuffer.Jmp := $E9;
  // JmpBuffer.Offset := PByte(NewProc) - (PByte(OrgProc) + 5);
  JmpBuffer.Offset := uint64(PByte(NewProc)) - (uint64(PByte(OrgProc)) + 5);
  if not WriteProcessMemory(GetCurrentProcess, OrgProc, @JmpBuffer,
    SizeOf(JmpBuffer), n) then
    RaiseLastOSError;
end;

initialization

RedirectFunction(@TWinControl.SetFocus, @TWinControlHack.SetFocus);

end.
