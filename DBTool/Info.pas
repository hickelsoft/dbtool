unit Info;

{$INCLUDE 'Globals.inc'}

interface

uses
  Windows, SysUtils, Classes, Controls, StdCtrls, Forms, ExtCtrls, Graphics;

type
  TDLG_Info = class(TForm)
    Label1: TLabel;
    lVersion: TLabel;
    Bevel1: TBevel;
    LbButton1: TButton;
    Label2: TLabel;
    Label6: TLabel;
    Label5: TLabel;
    lCopyright: TLabel;
    Image1: TImage;
    Label4: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  end;

var
  DLG_Info: TDLG_Info;

implementation

{$R *.DFM}

uses
  hl.Utils, Globals;

procedure TDLG_Info.FormCreate(Sender: TObject);
var
  dateidatum: TDateTime;
resourcestring
  S32Bits = '32 Bit';
  S64Bits = '64 Bit';
  SCORABundled = 'Bestandteil von CORAplus';
  SHsInfoBundled = 'Bestandteil von HS-Info 2.0';
  SHsLicense1 = 'Alle Rechte vorbehalten.';
  SHsLicense2 =
    'ACHTUNG: Dieses Programm ist durch Urhebergesetze und vertragliche Bindungen geschützt. Unbefugte Vervielfältigung oder unbefugter Vertrieb dieses Programms oder eines Teils davon sind strafbar.';
  SHsLicense3 =
    'Dies wird sowohl straf- als auch zivilrechtlich verfolgt und kann schwere Strafen und Schadensersatzforderungen zur Folge haben.';
  SPublicLicense1 = 'Lizenziert unter der Apache 2.0 Lizenz.' + #13#10 +
    'Weitergabe ausdrücklich erwünscht!';
  SPublicLicense2 =
    'Der Hersteller dieser Software haftet unter keinen Umständen für Schäden, die durch die Verwendung dieser Software entstehen!';
  SPublicLicense3 =
    'Der Quelltext sowie eine kostenlose Download-Möglichkeit finden Sie unter www.github.com/hickelsoft/dbtool';
begin
  dateidatum := GetOwnBuildTimestamp;

  lVersion.Caption := StringReplace(lVersion.Caption, '{VERSION}',
    FormatDateTime('YYYY-mm-dd', dateidatum), []); // do not localize
  lCopyright.Caption := StringReplace(lCopyright.Caption, '{YEAR}',
    FormatDateTime('YYYY', dateidatum), []); // do not localize

  if Modus_CORA_Verzeichnis then
  begin
{$IFNDEF WIN64}
    Label5.Caption := S32Bits + ' / ' + SCORABundled;
{$ELSE}
    Label5.Caption := S64Bits + ' / ' + SCORABundled;
{$ENDIF}
    // Zwar ist DBTool kostenlos und OpenSource, jedoch sollten beim Bundle mit CORAplus
    // die Weitergabe unterbunden werden, da man das dann so interpretieren könnte, dass man
    // das ganze CORAplus weitergeben darf. Daher folgender Text um Missverständnisse zu vermeiden:
    Label4.Caption := SHsLicense1;
    Label2.Caption := SHsLicense2;
    Label6.Caption := SHsLicense3;
  end
  else if Modus_HsInfo2_Verzeichnis then
  begin
{$IFNDEF WIN64}
    Label5.Caption := S32Bits + ' / ' + SHsInfoBundled;
{$ELSE}
    Label5.Caption := S64Bits + ' / ' + SHsInfoBundled;
{$ENDIF}
    // Zwar ist DBTool kostenlos und OpenSource, jedoch sollten beim Bundle mit HS-Info 2.0
    // die Weitergabe unterbunden werden, da man das dann so interpretieren könnte, dass man
    // das ganze HS-Info 2.0 weitergeben darf. Daher folgender Text um Missverständnisse zu vermeiden:
    Label4.Caption := SHsLicense1;
    Label2.Caption := SHsLicense2;
    Label6.Caption := SHsLicense3;
  end
  else
  begin
{$IFNDEF WIN64}
    Label5.Caption := S32Bits +
      '  [Access, dBase, InterBase, Firebird, Paradox, MySQL, SqlServer]';
    // do not localize
{$ELSE}
    Label5.Caption := S64Bits + '  [Access, InterBase, Firebird, MySQL, SqlServer]';
    // do not localize
{$ENDIF}
    // Die "Vollversion" von DBTool ist kostenlos und OpenSource!
    Label4.Caption := SPublicLicense1;
    Label2.Caption := SPublicLicense2;
    Label6.Caption := SPublicLicense3;
  end;
end;

procedure TDLG_Info.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Ord(Key) = VK_ESCAPE then
  begin
    Key := #0;
    Close;
  end;
end;

end.
