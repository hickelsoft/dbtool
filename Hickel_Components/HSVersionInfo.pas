unit HSVersionInfo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  VS_FIXEDFILEINFO = record
    dwSignature : Integer;
    dwStrucVersion : Integer;
    dwFileVersionMS : Integer;
    dwFileVersionLS : Integer;
    dwProductVersionMS : Integer;
    dwProductVersionLS : Integer;
    dwFileFlagsMask : Integer;
    dwFileFlags : Integer;
    dwFileOS : Integer;
    dwFileType : Integer;
    dwFileSubtype : Integer;
    dwFileDateMS : Integer;
    dwFileDateLS : Integer
  end;

  {$IF CompilerVersion > 20.0} // Version geraten
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$IFEND}
  THSVersionInfo = class(TComponent)
  private
    { Private declarations }
    FAutoGetInfo : Boolean;
    FHaveVersionInfo : Boolean;
    FhZero : DWORD;
    FVersionInfoSize : Integer;
    FVersionInfoBuffer : PChar;
    FFileName : PChar;
    FParam : Pointer;
    FParameterLength : UINT;
    FLanguage : Integer;
    FCharSet : Integer;
    FLangChar : String{[8]};
    FLanguageStr : String{[4]};
    FCharSetStr : String{[4]};
    FFixedFileInfo : VS_FIXEDFILEINFO;
  protected
    { Protected declarations }
    function GetFileName : String;
    procedure SetFileName(Name : String);
  public
    { Public declarations }
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    procedure GetFileInfo(FileName : String);
    procedure SetAutoGetInfo(Value : Boolean);

    property FileName : String read GetFileName write SetFileName;
    property AutoGetInfo : Boolean read FAutoGetInfo write SetAutoGetInfo default True;

    property HaveVersionInfo : Boolean read FHaveVersionInfo;
    property Language : Integer read FLanguage;
    property CharSet : Integer read FCharSet;
    property Signature : Integer read FFixedFileInfo.dwSignature;
    property StrucVersion : Integer read FFixedFileInfo.dwStrucVersion;
    property FileVersionMS : Integer read FFixedFileInfo.dwFileVersionMS;
    property FileVersionLS : Integer read FFixedFileInfo.dwFileVersionLS;
    property ProductVersionMS : Integer read FFixedFileInfo.dwProductVersionMS;
    property ProductVersionLS : Integer read FFixedFileInfo.dwProductVersionLS;
    property FileFlagsMask : Integer read FFixedFileInfo.dwFileFlagsMask;
    property FileFlags : Integer read FFixedFileInfo.dwFileFlags;
    property FileOS : Integer read FFixedFileInfo.dwFileOS;
    property FileType : Integer read FFixedFileInfo.dwFileType;
    property FileSubtype : Integer read FFixedFileInfo.dwFileSubtype;
    property FileDateMS : Integer read FFixedFileInfo.dwFileDateMS;
    property FileDateLS : Integer read FFixedFileInfo.dwFileDateLS;
    function GetValue(ValueName : String) : String;
  end;

procedure Register;

implementation

procedure Register;
begin
   RegisterComponents('HS', [THSVersionInfo]);
end;

constructor THSVersionInfo.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FFileName := nil;
  FAutoGetInfo := True;
  FLanguage := 0;
  FCharSet := 0;
  SetFileName(ParamStr(0));
  GetFileInfo(FileName);
end;

destructor THSVersionInfo.Destroy;
begin
  inherited Destroy;
  if FFileName <> nil then
    StrDispose(FFileName);
  if FVersionInfoBuffer <> nil then
    StrDispose(FVersionInfoBuffer);
end;

function THSVersionInfo.GetFileName : String;
begin
  Result := FFileName;
end;

procedure THSVersionInfo.SetFileName(Name : String);
begin
  if FFileName <> nil then begin
    StrDispose(FFileName);
    FFileName := nil;
  end;
  FFileName := StrAlloc(Length(Name) + 1);
  StrPCopy(FFileName, Name);
end;

procedure THSVersionInfo.SetAutoGetInfo(Value : Boolean);
begin
  if FAutoGetInfo <> Value then begin
    FAutoGetInfo := Value;
    if FAutoGetInfo then
      SetFileName(ParamStr(0));
    GetFileInfo(FFileName);
  end;
end;

procedure THSVersionInfo.GetFileInfo(FileName : String);
var
  Temp : Integer;
begin
  FVersionInfoSize := GetFileVersionInfoSize(FFileName, FhZero);
  FHaveVersionInfo := (FVersionInfoSize <> 0);
  try
    FVersionInfoBuffer := StrAlloc(FVersionInfoSize);
    FHaveVersionInfo := GetFileVersionInfo(FFileName, 0, FVersionInfoSize, FVersionInfoBuffer);
    if FHaveVersionInfo then begin
      VerQueryValue(FVersionInfoBuffer,
                    '\',
                    FParam, FParameterLength);
      CopyMemory(@FFixedFileInfo, FParam, FParameterLength);
      VerQueryValue(FVersionInfoBuffer,
                    '\VarFileInfo\Translation',
                    FParam, FParameterLength);
      Temp := Integer(FParam^);
      FLanguage := Temp and $FFFF;
      FCharSet := ((Temp and $FFFF0000) shr 16) and $FFFF;
      FLanguageStr := IntToHex(FLanguage, 4);
      FCharSetStr := IntToHex(FCharSet, 4);
      FLangChar := FLanguageStr + FCharSetStr;
    end;
  except
    on E: EAbort do
    begin
      Abort;
    end;
    on E: Exception do
    begin
      FVersionInfoBuffer := nil;
    end;
  end;
end;

function THSVersionInfo.GetValue(ValueName : String) : String;
begin
  if VerQueryValue(FVersionInfoBuffer,
                   PChar('\StringFileInfo\' + string(FLangChar) + '\' + ValueName),
                   FParam, FParameterLength) then
    Result := StrPas(PChar(FParam))
  else
    Result := 'Keine solche VersionsInfo';
end;

end.
