unit StatusAn;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Gauges, ExtCtrls, StdCtrls, HsGauge, Buttons;

type
  /// <summary>Dieses Form wird nur von der VCL TProgressDlg aus ProgrDlg.pas verwendet.</summary>
  TDLG_Statusanzeige = class(TForm)
    Panel2: TPanel;
    Panel1: TPanel;
    Panel3: TPanel;
    LabelRestzeit: TLabel;
    TimerRestzeit: TTimer;
    ProgressBar1: THsGauge;
    StatusText: TLabel;
    HsGauge1: THsGauge;
    btnStop: TButton;
    LblExactPosition: TLabel;
    procedure TimerRestzeitTimer(Sender: TObject);
    procedure DoMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure btnStopKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    bShowTime: boolean;
    lastTimerUpd: TDateTime;
    lastExactPosUpd: TDateTime;
    function GetShowStopButton: boolean;
    procedure SetShowStopButton(const Value: boolean);
    function GetMaxValue: integer;
    procedure SetMaxValue(const Value: integer);
    function GetPosition: integer;
    procedure SetPosition(const Value: integer);
    procedure UpdateExactPosition;
    procedure SetShowTime(bNew: boolean);
    function GetShowExactPosition: boolean;
    procedure SetShowExactPosition(const Value: boolean);
    function GetText: string;
    procedure SetText(const Value: string);
  public
    dStartzeit: TDateTime;
    property ShowTime: boolean read bShowTime write SetShowTime;
    property ShowStopButton: boolean read GetShowStopButton write SetShowStopButton;
    property MaxValue: integer read GetMaxValue write SetMaxValue;
    property Position: integer read GetPosition write SetPosition;
    property ShowExactPosition: boolean read GetShowExactPosition write SetShowExactPosition;
    property Text: string read GetText write SetText;
  end;

var
  DLG_Statusanzeige: TDLG_Statusanzeige;

implementation

{$R *.DFM}

uses
  DateUtils, Math;

procedure TDLG_Statusanzeige.btnStopKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    Key := 0;
    btnStop.Click;
  end;
end;

procedure TDLG_Statusanzeige.DoMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   ReleaseCapture;
   SendMessage(Handle, WM_NCLBUTTONDOWN, 2, 0);
end;

procedure TDLG_Statusanzeige.FormCreate(Sender: TObject);
begin
  if Assigned(Application) and Assigned(Application.MainForm) and IsWindowVisible(Application.MainForm.Handle) then
    inherited Position := poMainFormCenter
  else
    inherited Position := poScreenCenter;
  SetShowTime(false);
end;

function TDLG_Statusanzeige.GetMaxValue: integer;
begin
  result := ProgressBar1.MaxValue;
end;

function TDLG_Statusanzeige.GetPosition: integer;
begin
  result := ProgressBar1.Progress;
end;

function TDLG_Statusanzeige.GetShowExactPosition: boolean;
begin
  result := LblExactPosition.Visible;
end;

function TDLG_Statusanzeige.GetShowStopButton: boolean;
begin
  result := btnStop.Visible;
end;

function TDLG_Statusanzeige.GetText: string;
begin
  result := StatusText.Caption;
end;

procedure TDLG_Statusanzeige.SetMaxValue(const Value: integer);
begin
  if Value = 0 then
  begin
    Height := 94;
    LblExactPosition.Visible := false;
  end
  else
  begin
    Height := 117;
    ProgressBar1.MaxValue := Value;
  end;

  UpdateExactPosition;
  Update;
end;

procedure TDLG_Statusanzeige.UpdateExactPosition;
begin
  // Das Setzen von Caption ist MEGA EXTREM langsam!!! Deshalb machen wir es nur alle 100ms und
  // nur wenn das Label Visible ist
  if LblExactPosition.Visible then
  begin
    // TODO: Hier gibt es jedoch ein Problem:
    // Wenn sehr schnell eine neue Zahl kommt, wird sie ignoriert. Aber wenn es danach lange
    // Dauert, bis die nächste Zahl kommt, wäre es eigentlich angebracht, die vorherige Zahl,
    // die "Verschluckt wurde" zu zeigen... Deshalb prüfen wir auf "MaxValue > 1000", da
    // erst bei so großen Werten eine signifikante verschlechterung durch Caption="" erfolgt.
    // Bei 1000 Objekten ist die exakte Anzahl eines Objekts nicht sonderlich wichtig
    // Wichtiger wären z.B. 14 Datenbanktabellen, da sollte der Status immer sehr exakt sein,
    // und es darf nichts verschluckt werden.
    if MaxValue > 1000 then
    begin
      if MilliSecondsBetween(Now, lastExactPosUpd) < 250{ms} then Exit;
      lastExactPosUpd := Now;
    end;

    LblExactPosition.Caption := IntToStr(Position) + ' / ' + IntToStr(MaxValue);
  end;
end;

procedure TDLG_Statusanzeige.SetPosition(const Value: integer);
begin
  ProgressBar1.Progress := Value;

  if Progressbar1.Progress > (Progressbar1.MaxValue shr 1) then
  begin
    if Progressbar1.Font.Color <> clWhite then
      Progressbar1.Font.Color := clWhite;
  end
  else
  begin
    if Progressbar1.Font.Color <> clBlack then
      Progressbar1.Font.Color := clBlack;
  end;

  UpdateExactPosition;
  Update;
end;

procedure TDLG_Statusanzeige.SetShowExactPosition(const Value: boolean);
begin
  LblExactPosition.Visible := Value;

  if Value then
  begin
    LblExactPosition.Transparent := true;
    StatusText.Height := 59;
  end
  else
  begin
    StatusText.Height := 69;
  end;

  UpdateExactPosition;
end;

procedure TDLG_Statusanzeige.SetShowStopButton(const Value: boolean);
begin
  btnStop.Visible := Value;
  if Value then
    StatusText.Width := 301
  else
    StatusText.Width := 342;
end;

procedure TDLG_Statusanzeige.SetShowTime(bNew: boolean);
begin
   bShowTime := bNew;
   if bShowTime then
   begin
     Panel3.Width := 248;
     LabelRestzeit.Caption := 'Restzeit (ca.): xx:xx';
     dStartzeit := time;
     TimerRestzeit.Enabled := true;
     LabelRestzeit.Visible := true;
   end
   else
   begin
     TimerRestzeit.Enabled := false;
     LabelRestzeit.Visible := false;
     Panel3.Width := 348;
   end;
end;

procedure TDLG_Statusanzeige.SetText(const Value: string);
begin
  StatusText.Caption := Value;
  StatusText.Refresh;
end;

procedure TDLG_Statusanzeige.TimerRestzeitTimer(Sender: TObject);
var
  iMinuten: Integer;
  iSekunden: Integer;
  secZeitverstrichen: Extended;
  prozentFortschritt: Extended;
  secGesamtzeit: Extended;
  secFehlen: Integer;

begin
   if ProgressBar1.Progress = 0 then exit;

   // Das ist notwendig, da wir in ProgrDlg kein ProcessMessages verwenden dürfen.
   // Daher müssen wir in ProgrDlg.pas diese Funktion bei jedem Tick manuell anschupsen.
   // TODO: Problematisch: Wenn jeder Tick länger als 1 Sekunde dauert, dann ist es auch nicht optimal...
   if MilliSecondsBetween(Now, lastTimerUpd) < TimerRestzeit.Interval then Exit;

   {$REGION 'Zeitbestimmung'}
   if (ProgressBar1.Progress = 0) or (ProgressBar1.MaxValue = 0) then
   begin
     // DIV0 verhindern
     LabelRestzeit.Caption := 'Restzeit ca. xx:xx';
   end
   else
   begin
     secZeitverstrichen := (Time - dStartzeit) * (60*60*24);
     prozentFortschritt := ProgressBar1.Progress/ProgressBar1.MaxValue*100;
     secGesamtzeit := secZeitverstrichen/prozentFortschritt*100;
     secFehlen := Ceil(secGesamtzeit - secZeitverstrichen);

     iMinuten := secFehlen div 60;
     iSekunden := secFehlen mod 60;
     LabelRestzeit.Caption := format('Restzeit ca. %2.2d:%2.2d', [iMinuten, iSekunden]);
   end;
   {$ENDREGION}

   lastTimerUpd := Now;
end;

end.
