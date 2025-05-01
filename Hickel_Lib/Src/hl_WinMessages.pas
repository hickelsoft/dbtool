unit hl_WinMessages;

interface

uses
  Windows, Messages;

type
  THsWindowsMessage = class(TObject)
  strict protected
    class function GetMessageID: UINT; virtual; abstract;
    class procedure Send(awParam: WPARAM; alParam: LPARAM); virtual;
  end;

  ThswmOpenAppEvent = procedure (AppCrc32: Cardinal) of object;
  ThswmOpenApp = class(THsWindowsMessage)
  strict private
    class var FMsg: integer;
  strict protected
    class function GetMessageID: UINT; override;
  public
    class procedure Send(AppCrc32: Cardinal); reintroduce;
    class function Check(m: TMessage; Event: ThswmOpenAppEvent): boolean;
  end;

implementation

{ THsWindowsMessage }

class procedure THsWindowsMessage.Send(awParam: WPARAM; alParam: LPARAM);
var
  Msg: UINT;
begin
  Msg := GetMessageID;
  SendNotifyMessage(wnd_Broadcast, Msg, awParam, alParam);
end;

{ ThswmOpenApp }

class function ThswmOpenApp.GetMessageID: UINT;
var
  oid: string;
begin
  if FMsg = 0 then
  begin
    // { iso(1) identified-organization(3) dod(6) internet(1) private(4) enterprise(1) 56776 windows-messages(150) open-app(3) }
    oid := '1.3.6.1.4.1.56776.150.3';
    FMsg := RegisterWindowMessage(PChar(oid));
  end;
  result := FMsg;
end;

type
  pWPARAM = ^WPARAM;

class function ThswmOpenApp.Check(m: TMessage; Event: ThswmOpenAppEvent): boolean;
var
  AppCrc32: Cardinal;
  tmp1: pWPARAM;
  tmp2: PCardinal;
begin
  if m.Msg = GetMessageID then
  begin
    if Assigned(Event) then
    begin
      tmp1 := @m.WParam;
      tmp2 := pCardinal(tmp1);

      AppCrc32 := tmp2^;
      Event(AppCrc32);
    end;
    result := true;
  end
  else result := false;
end;

class procedure ThswmOpenApp.Send(AppCrc32: Cardinal);
var
  lwParam: WPARAM;
  llParam: LPARAM;
  tmp1: pWPARAM;
  tmp2: PCardinal;
begin
  tmp2 := @AppCrc32;
  tmp1 := pWPARAM(tmp2);

  lwParam := tmp1^;
  llParam := 0;
  inherited Send(lwParam, llParam);
end;

end.
