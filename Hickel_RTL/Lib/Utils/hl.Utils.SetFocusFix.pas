unit hl.Utils.SetFocusFix;

(*
  Diese Unit ersetzt TWinControl.SetFocus durch eine Variante,
  die keine Exception auslöst, wenn ein Control nicht fokussierbar ist.
  Die Hook-Installation erfolgt beim Laden der Unit.
*)

interface

implementation

uses
  Windows,
  SysUtils,
  Controls,
  Forms;

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
{$IFDEF WIN64}
type
  TAbsoluteJump = packed record
    MovRax: Word;   // $B848 = mov rax, imm64
    Address: UInt64;
    JmpRax: Word;   // $E0FF = jmp rax
  end;
const
  PatchSize = SizeOf(TAbsoluteJump); // 12 Bytes
var
  Patch: TAbsoluteJump;
  OldProtect: DWORD;
begin
  if (OrgProc = nil) or (NewProc = nil) then
    raise Exception.Create('RedirectFunction: nil pointer');

  Patch.MovRax := $B848;
  Patch.Address := UInt64(NewProc);
  Patch.JmpRax := $E0FF;

  if not VirtualProtect(OrgProc, PatchSize, PAGE_EXECUTE_READWRITE, OldProtect) then
    RaiseLastOSError;
  try
    Move(Patch, Pointer(OrgProc)^, PatchSize);
    FlushInstructionCache(GetCurrentProcess, OrgProc, PatchSize);
  finally
    VirtualProtect(OrgProc, PatchSize, OldProtect, OldProtect);
  end;
end;
{$ELSE}
type
  TJmpRel32 = packed record
    Jmp: Byte;       // $E9
    Offset: Integer; // rel32
  end;
const
  PatchSize = SizeOf(TJmpRel32); // 5 Bytes
var
  Patch: TJmpRel32;
  OldProtect: DWORD;
  Delta: Int64;
begin
  if (OrgProc = nil) or (NewProc = nil) then
    raise Exception.Create('RedirectFunction: nil pointer');

  Delta := NativeInt(NewProc) - (NativeInt(OrgProc) + PatchSize);
  if (Delta < Low(Integer)) or (Delta > High(Integer)) then
    raise Exception.Create('RedirectFunction: target out of range for rel32 jump');

  Patch.Jmp := $E9;
  Patch.Offset := Integer(Delta);

  if not VirtualProtect(OrgProc, PatchSize, PAGE_EXECUTE_READWRITE, OldProtect) then
    RaiseLastOSError;
  try
    Move(Patch, Pointer(OrgProc)^, PatchSize);
    FlushInstructionCache(GetCurrentProcess, OrgProc, PatchSize);
  finally
    VirtualProtect(OrgProc, PatchSize, OldProtect, OldProtect);
  end;
end;
{$ENDIF}

initialization
  try
    RedirectFunction(@TWinControl.SetFocus, @TWinControlHack.SetFocus);
  except
  end;

end.
