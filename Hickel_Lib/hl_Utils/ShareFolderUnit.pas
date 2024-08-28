unit ShareFolderUnit;
// This is for the NT platform.
// https://groups.google.com/g/borland.public.delphi.nativeapi.win32/c/YJK3xpPA_XM

interface

uses
  Windows, SysUtils;

function FolderShareAdd(Path, NetName, Remark: WideString): Boolean;
function FolderShareDel(NetName: WideString): Boolean;

implementation

const
  STYPE_DISKTREE = 0;
  netapi32 = 'NETAPI32.DLL';

function NetShareAdd(serverName: PWideChar; level: DWord; buf: PChar; var parm_err : DWord): DWord; stdcall; external netapi32 name 'NetShareAdd';
function NetShareDel(serverName, netName: PWideChar; reserved: Integer): DWord; stdcall; external netapi32 name 'NetShareDel';

type
  SHARE_INFO_2 = record
    shi2_netname: PWideChar;
    shi2_type: Integer;
    shi2_remark: PWideChar;
    shi2_permissions: Integer;
    shi2_max_uses: Integer;
    shi2_current_uses: Integer;
    shi2_path: PWideChar;
    shi2_passwd: PWideChar;
  end;

function FolderShareAdd(Path, NetName, Remark: WideString): Boolean;
var
  ParamErr: DWord;
  ShareInfo: SHARE_INFO_2;
  //NetName, remark, path: string;
  Res: DWord;
  Buf: PChar;
  Str: string;
begin
  Path := ExcludeTrailingPathDelimiter(Path); // Added by Daniel Marschall / HickelSOFT Huth GmbH

  FillChar(ShareInfo, SizeOf(ShareInfo), 0);
  with ShareInfo do
  begin
    //NetName := 'Testing';
    shi2_NetName := PWideChar(NetName);
    shi2_Type := STYPE_DISKTREE;
    //Remark := 'No Remarks';
    shi2_Remark := PWideChar(Remark);
    shi2_Permissions := 0;
    shi2_Max_Uses := -1;
    shi2_Current_Uses := 0;
    //Path := 'C:\Temp';
    shi2_Path := PWideChar(Path);
    shi2_Passwd := nil;
  end;
  ParamErr := 0;
  //Res := NetShareAdd(PWideChar(''), 2, @ShareInfo, ParamErr);
  Res := NetShareAdd(nil, 2, @ShareInfo, ParamErr);
  Result := Res = 0;
  if Res <> 0 then
  begin
    Buf := StrAlloc(255);
    FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil, Res, 0, Buf, 255, nil);
    Str := Buf;
    StrDispose(Buf);
    raise Exception.Create(Str);
  end;
end;

function FolderShareDel(NetName: WideString): Boolean;
var
  Res: DWord;
  Buf: PChar;
  Str: string;
begin
  //Res := NetShareDel(PWideChar(''), PWideChar(NetName), 0);
  Res := NetShareDel(nil, PWideChar(NetName), 0);
  Result := Res = 0;
  if Res <> 0 then
  begin
    Buf := StrAlloc(255);
    FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil, Res, 0, Buf, 255, nil);
    Str := Buf;
    StrDispose(Buf);
    //ShowMessage(Str);
  end;
end;

end.
