unit hl.Utils.StreamEx;

interface

uses
  SysUtils, Classes;

type
  TStreamEx = class helper for TStream
  public
    procedure WriteString(const data: string);
    procedure SaveToFile(outfile: string);
    procedure Clear;
  end;

implementation

procedure TStreamEx.WriteString(const data: string);
var
  len: cardinal;
  oString: UTF8String;
begin
  // https://stackoverflow.com/questions/1434413/writing-a-string-to-a-tfilestream-in-delphi-2010
  oString := UTF8String(data);
  len := length(oString);
  if len > 0 then
    self.WriteBuffer(oString[1], len);
end;

procedure TStreamEx.Clear;
begin
  self.Size := 0;
end;

procedure TStreamEx.SaveToFile(outfile: string);
var
  bakpos: Int64;
  fs: TFileStream;
begin
  fs := TFileStream.Create(outfile, fmCreate or fmOpenReadWrite);
  try
    fs.Size := 0;
    bakpos := self.Position;
    self.Position := 0;
    fs.CopyFrom(self, self.Size);
    self.Position := bakpos;
  finally
    FreeAndNil(fs);
  end;
end;

end.
