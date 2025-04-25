unit hl.Utils.IndyMemoryHack;

{.$DEFINE UseFastMM4}

interface

procedure IndyRegisterExpectedMemoryLeak;

implementation

{$IFDEF UseFastMM4}

uses
  {$IF CompilerVersion < 20.0}FastMM4, {$IFEND}IdStack;

procedure IndyRegisterExpectedMemoryLeak; // for Indy 10
{$IF CompilerVersion < 20.0}
type
  PObject = ^TObject;
var
  bak: byte;
  m: pointer;
{$IFEND}
begin
  {$IF CompilerVersion < 20.0}
  if DebugHook = 0 then exit;
  bak := DebugHook;
  DebugHook := 0; // AccessViolation nicht zeigen
  try
    // Wir müssen das Symbol GStackCriticalSection in IdStack.pas finden.
    // Leider ist es im implementation-Abschnitt versteckt.
    // Deshalb suchen wir die Adresse mit diesem ekelhaften Hack.

    (*
    TIdStack.DecUsage:
    00634BEC 55               push ebp
    00634BED 8BEC             mov ebp,esp
    00634BEF 51               push ecx
    00634BF0 8945FC           mov [ebp-$04],eax
    IdStack.pas.641: class procedure TIdStack.DecUsage;
    00634BF3 833D7C7B980000   cmp dword ptr [$00987b7c],$00  <---  $00987b7c = GStackCriticalSection  (DecUsage+$9)
    00634BFA 7514             jnz $00634c10
    00634BFC B981020000       mov ecx,$00000281
    00634C01 BA784C6300       mov edx,$00634c78
    00634C06 B8B04C6300       mov eax,$00634cb0
    00634C0B E8940DDDFF       call @Assert
    IdStack.pas.643: Assert(GStackCriticalSection<>nil);
    00634C10 A17C7B9800       mov eax,[$00987b7c]  <---  $00987b7c = GStackCriticalSection (DecUsage+$25)
    00634C15 8B10             mov edx,[eax]
    00634C17 FF12             call dword ptr [edx]
    00634C19 33C0             xor eax,eax
    00634C1B 55               push ebp
    00634C1C 68654C6300       push $00634c65
    00634C21 64FF30           push dword ptr fs:[eax]
    00634C24 648920           mov fs:[eax],esp
    *)

    m := @TidStack.DecUsage;
    m := Pointer(integer(m)+9);
    repeat
      try
        if PObject(PPointer(m)^)^.ClassName = 'TIdCriticalSection' then
        begin
          FastMM4.RegisterExpectedMemoryLeak(PPointer(PPointer(m)^)^);
          break;
        end;
      except
        on E: EAbort do
        begin
          Abort;
        end;
        on E: Exception do
        begin
          // ignore
        end;
      end;
      m := Pointer(integer(m)+1);
    until false;
  finally
    DebugHook := bak;
  end;
  {$IFEND}
end;

{$ELSE}

procedure IndyRegisterExpectedMemoryLeak;
begin
end;

{$ENDIF}

end.
