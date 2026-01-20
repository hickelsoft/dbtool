unit hl.Utils.StringStreamEx;

interface

uses
  SysUtils, Classes;

type
  TStreamEx = class helper for TStream
  public
    procedure Add(const data: string);
  end;

  TStringStreamEx = class helper for TStringStream
  public
    constructor Create; overload;
    constructor Create(const s: string); overload;
    procedure LoadFromFile(const Filename: string);
    procedure SaveToFile(const Filename: string; FileMode: Word = fmCreate);
    procedure Clear;
  end;

implementation

{ TStreamEx }

procedure TStreamEx.Add(const data: string);
var
  len: cardinal;
  oString: UTF8String;
begin
  oString := UTF8String(data + #13#10);
  len := length(oString);
  if len > 0 then
    self.WriteBuffer(oString[1], len);
end;

{ TStringStreamEx }

procedure TStringStreamEx.Clear;
begin
  Position := 0;
  Size := 0;
end;

constructor TStringStreamEx.Create;
begin
  inherited Create('');
end;

constructor TStringStreamEx.Create(const s: string);
begin
  inherited Create(s);
end;

procedure TStringStreamEx.LoadFromFile(const Filename: string);
var
  fs: TFileStream;
  bakPos: Int64;
begin
  fs := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
  try
    bakPos := fs.Position;
    try
      fs.Position := 0;
      CopyFrom(fs, fs.Size);
    finally
      fs.Position := bakPos;
    end;
  finally
    FreeAndNil(fs);
  end;
end;

procedure TStringStreamEx.SaveToFile(const Filename: string;
  FileMode: Word = fmCreate);
var
  fs: TFileStream;
  bakPos: Int64;
begin
  fs := TFileStream.Create(Filename, FileMode);
  try
    bakPos := Position;
    try
      Position := 0;
      fs.CopyFrom(self, self.Size);
    finally
      Position := bakPos;
    end;
  finally
    FreeAndNil(fs);
  end;
end;

end.
