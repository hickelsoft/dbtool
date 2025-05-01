unit VCL_Hotfix_AltGr;

// IsAltGRPressed stürzt bei Delphi 2007 ab,
// wenn ein Tastaturtreiber von Parallels Desktop (Apple VM)
// installiert ist und man die Shift-Taste drückt. (HS Ticket 52189)
// Dieser Hotfix patched die Funktion, in dem sie mit der Funktion
// aus Delphi 12.3 überschrieben wird

interface

implementation

uses
  Menus, Windows, Registry, SysUtils;

// Der folgende Code kommt von Delphi 12.3, mit folgenden Änderungen
// - Umbenannt zu IsAltGRPressed_New
// - IsWow64 geändert
// - $ENDIF -> $IFEND (alte Delphi Kompatiblität)
// - .Free -> FreeAndNil()

{$IF NOT DEFINED(CLR)}
var
  CurrentKeyboardLayout: HKL = 0;
  IsAltGRKeyLayout: Boolean = False;

function IsAltGRPressed_New: Boolean;

  // Original
(*
  function IsWOW64: BOOL;
  begin
    Result := False;
    if GetProcAddress(GetModuleHandle(kernel32), 'IsWow64Process') <> nil then
      IsWow64Process(GetCurrentProcess, Result);
  end;
*)
  // Ersetzt https://www.delphipraxis.net/192951-laeuft-programm-unter-32-bit-oder-64-bit-ab-2.html
  function IsWOW64: BOOL;
  type
    TIsWow64Process = function( // Type of IsWow64Process API fn
      Handle: Windows.THandle; var Res: Windows.BOOL ): Windows.BOOL; stdcall;
  var
    IsWow64Result: Windows.BOOL; // Result from IsWow64Process
    IsWow64Process: TIsWow64Process; // IsWow64Process fn reference
  begin
    // Try to load required function from kernel32
    IsWow64Process := Windows.GetProcAddress(Windows.GetModuleHandle('kernel32'), 'IsWow64Process');

    if Assigned(IsWow64Process) then begin
      // Function is implemented: call it
      if not IsWow64Process(Windows.GetCurrentProcess, IsWow64Result) then
        raise Exception.Create('IsWow64: bad process handle');

      Result := IsWow64Result; // Return result of function
    end else
      // Function not implemented: can't be running on Wow64
      Result := False;
  end;

  procedure GetAltGRStatus;
  type
    PKBDTABLES = ^TKBDTABLES;
    TKBDTABLES = record
      pCharModifiers: pointer;
      pVkToWcharTable: pointer;
      pDeadKey: pointer;
      pKeyNames: Pointer;
      pKeyNamesExt: Pointer;
      pKeyNamesDead: PWideChar;
      pusVSCtoVK: Pointer;
      bMaxVSCtoVK: BYTE;
      pVSCtoVK_E0: Pointer;
      pVSCtoVK_E1: Pointer;
      fLocaleFlags: DWORD;
      nLgMax: Byte;
      cbLgEntry: Byte;
      pLigature: Pointer;
    end;
    PKBDTABLESWOW64 = ^TKBDTABLESWOW64;
    TKBDTABLESWOW64 = record
      pCharModifiers: UInt64;
      pVkToWcharTable: UInt64;
      pDeadKey: UInt64;
      pKeyNames: UInt64;
      pKeyNamesExt: UInt64;
      pKeyNamesDead: UInt64;
      pusVSCtoVK: UInt64;
      bMaxVSCtoVK: BYTE;
      pVSCtoVK_E0: UInt64;
      pVSCtoVK_E1: UInt64;
      fLocaleFlags: DWORD;
      nLgMax: Byte;
      cbLgEntry: Byte;
      pLigature: UInt64;
    end;
    TFNKbdLayerDescriptor = function: Pointer;
  const
    KLLF_ALTGR = $0001;
    LayoutsListRegKey = '\SYSTEM\CurrentControlSet\Control\Keyboard Layouts\';
  var
    DLLName: string;
    DLLHandle: HModule;
    R: TRegistry;
    LayoutName: array[0..KL_NAMELENGTH] of char;
    LocaleFlags: DWORD;
    KbdTablePtr: Pointer;
    KbdLayerDescriptor: TFNKbdLayerDescriptor;
  begin
    IsAltGRKeyLayout := False;
    LayoutName[0] := #0;
    GetKeyboardLayoutName(LayoutName);
    R := TRegistry.Create;
    try
      R.RootKey := HKEY_LOCAL_MACHINE;
      if R.OpenKeyReadOnly(LayoutsListRegKey + LayoutName) then
      begin
        DLLName := R.ReadString('Layout File');
        DLLHandle := SafeLoadLibrary(DLLName);
        if DLLHandle <> 0 then
          try
            KbdLayerDescriptor := GetProcAddress(DLLHandle, 'KbdLayerDescriptor');
            if Assigned(KbdLayerDescriptor) then
            begin
              KbdTablePtr := KbdLayerDescriptor;
              if KbdTablePtr <> nil then
              begin
                if IsWOW64 then
                  LocaleFlags := PKBDTABLESWOW64(KbdTablePtr).fLocaleFlags
                else
                  LocaleFlags := PKBDTABLES(KbdTablePtr).fLocaleFlags;
                IsAltGRKeyLayout := (LocaleFlags and KLLF_ALTGR) = KLLF_ALTGR;
              end;
            end;
          finally
            FreeLibrary(DLLHandle);
          end;
      end;
    finally
      FreeAndNil(R);
    end;
  end;

var
  KeyboardLayout: HKL;
begin
  Result := False;
  if GetKeyState(VK_RMENU) < 0 then
  begin
    KeyboardLayout := GetKeyboardLayout(0);
    if KeyboardLayout <> CurrentKeyboardLayout then
    begin
      GetAltGRStatus;
      CurrentKeyboardLayout := KeyboardLayout;
    end;
    Result := IsAltGRKeyLayout;
  end;
end;
{$IFEND}

// Ende vom Delphi 12.3 Code

// Der folgende Code kommt von
// https://stackoverflow.com/questions/8978177/patch-routine-call-in-delphi

procedure PatchCode(Address: Pointer; const NewCode; Size: Integer);
var
  OldProtect: DWORD;
begin
  if VirtualProtect(Address, Size, PAGE_EXECUTE_READWRITE, OldProtect) then
  begin
    Move(NewCode, Address^, Size);
    FlushInstructionCache(GetCurrentProcess, Address, Size);
    VirtualProtect(Address, Size, OldProtect, @OldProtect);
  end;
end;

type
  PInstruction = ^TInstruction;
  TInstruction = packed record
    Opcode: Byte;
    Offset: Integer;
  end;

procedure RedirectProcedure(OldAddress, NewAddress: Pointer);
var
  NewCode: TInstruction;
begin
  NewCode.Opcode := $E9;//jump relative
  NewCode.Offset := NativeInt(NewAddress)-NativeInt(OldAddress)-SizeOf(NewCode);
  PatchCode(OldAddress, NewCode, SizeOf(NewCode));
end;

var
  AlreadyFixed: boolean = false;

procedure PatchAltGrBug;
begin
  {$IF CompilerVersion < 20.0} // Version geraten
  if AlreadyFixed then exit;
  RedirectProcedure(@IsAltGRPressed, @IsAltGRPressed_New);
  AlreadyFixed := true;
  {$IFEND}
end;

initialization
  PatchAltGrBug;
end.
