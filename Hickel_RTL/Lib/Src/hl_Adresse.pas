unit hl_Adresse;

interface

uses
  SysUtils;

type
  ThlAdresse = class(TObject)
  private
    mMatchCode: string;
    mName1: string;
    mName2: string;
    mName3: string;
    mStrasse: string;
    mLand: string;
    mPlz: string;
    mOrt: string;
    mTelefon: string;
    mFax: string;
    mHandy: string;
    mInternet: string;
    mEMail: string;

  protected
    function GetLand: string;
    procedure SetPlz(value: string);

  public
    function ToString: string; {$IF CompilerVersion > 20.0}override; {$IFEND}
    property MatchCode: string read mMatchCode write mMatchCode;
    property Name1: string read mName1 write mName1;
    property Name2: string read mName2 write mName2;
    property Name3: string read mName3 write mName3;
    property Strasse: string read mStrasse write mStrasse;
    property Land: string read GetLand write mLand;
    property Plz: string read mPlz write SetPlz;
    property Ort: string read mOrt write mOrt;
    property Telefon: string read mTelefon write mTelefon;
    property Fax: string read mFax write mFax;
    property Handy: string read mHandy write mHandy;
    property Internet: string read mInternet write mInternet;
    property EMail: string read mEMail write mEMail;

  end;

implementation

function ThlAdresse.ToString: string;
begin
  result := mName1;
  if (mName2 <> '') and (mName2 <> '.') then
    result := result + #13#10 + mName2;
  if (mStrasse <> '') and (mStrasse <> '.') then
    result := result + #13#10 + mStrasse;
  if (mOrt <> '') and (mOrt <> '.') then
    result := result + #13#10 + mOrt;
end;

function ThlAdresse.GetLand: string;
begin
  if (mLand = '') or (mLand = ' ') then
    result := 'D'
  else
    result := mLand;

end;

procedure ThlAdresse.SetPlz(value: string);
begin
  mPlz := copy(value, 1, 5);
end;

end.
