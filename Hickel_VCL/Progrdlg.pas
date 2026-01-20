unit ProgrDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Registry, StatusAn, HsHalfModalForm;

var
  OpenedProgressDlgs: integer = 0;

type
{$IF CompilerVersion > 20.0} // Version geraten
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$IFEND}

  TProgressDlg = class(TComponent)
  private
    mOwner: TComponent;
    FText: string;
    FMax: Int64;
    FShowTime: boolean;

{$REGION 'Werte für das aktuell geöffnete Fenster, sonfern vorhanden'}
    FCurWindowOpenedHalfModal: boolean;
    mc: TModalContext; // nur wenn FHalfModal=true
{$ENDREGION}
    FCurWindowOpen: boolean;
    FShowStopButton: boolean;
    FOnStopButtonClick: TNotifyEvent;
    FExactPosition: boolean;
    FPosition: Int64;
    FEnabled: boolean;
    FHalfModal: boolean;

    FStopButtonSignal: boolean;

    procedure SetMax(const Value: Int64);
    procedure SetStopButton(const Value: boolean);

    procedure FormBtnStopClick(Sender: TObject);
    procedure SetPosition(const Value: Int64);
    procedure SetExactPosition(const Value: boolean);
    function GetText: String;
    procedure SetShowTime(const Value: boolean);

  protected
    procedure SetText(aValue: string);

  public
    DLG_Statusanzeige: TDLG_Statusanzeige;

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    procedure Open;
    procedure Close;
    procedure IncPos;
    procedure TestStop;
    procedure Reset;

    property StopButtonSignal: boolean read FStopButtonSignal;
    procedure ResetStopButtonSignal;

  published
    property Text: String read GetText write SetText;
    property MaxValue: Int64 read FMax write SetMax default 0;
    property Position: Int64 read FPosition write SetPosition default 0;
    property ShowTime: boolean read FShowTime write SetShowTime default false;
    property ShowStopButton: boolean read FShowStopButton write SetStopButton
      default false;
    property OnStopButtonClick: TNotifyEvent read FOnStopButtonClick
      write FOnStopButtonClick;
    property ShowExactPosition: boolean read FExactPosition
      write SetExactPosition default false;
    property Enabled: boolean read FEnabled write FEnabled default true;
    property HalfModal: boolean read FHalfModal write FHalfModal default true;
  end;

procedure Register;

implementation

resourcestring
  StrBitteWarten = 'Bitte warten...';

procedure Register;
begin
  RegisterComponents('HS', [TProgressDlg]);
end;

constructor TProgressDlg.Create(aOwner: TComponent);
begin
  mOwner := aOwner;
  FEnabled := true;
  FHalfModal := true;
  inherited Create(aOwner);
end;

destructor TProgressDlg.Destroy;
begin
  Close;

  inherited;
end;

procedure TProgressDlg.FormBtnStopClick(Sender: TObject);
begin
  // Event weiterleiten
  FStopButtonSignal := true;
  if Assigned(FOnStopButtonClick) then
  begin
    FOnStopButtonClick(Self);
  end;
end;

function TProgressDlg.GetText: String;
begin
  if Trim(FText) = '' then
    result := StrBitteWarten
  else
    result := FText;
end;

procedure TProgressDlg.Open;
begin
  if not Enabled then
    exit;
  if FCurWindowOpen then
    exit;

  if not Assigned(DLG_Statusanzeige) then
  begin
    DLG_Statusanzeige := TDLG_Statusanzeige.Create(mOwner);
  end;
  Reset;
  DLG_Statusanzeige.MaxValue := FMax;
  DLG_Statusanzeige.ShowStopButton := ShowStopButton;
  DLG_Statusanzeige.btnStop.OnClick := FormBtnStopClick;
  DLG_Statusanzeige.ShowExactPosition := FExactPosition;
  DLG_Statusanzeige.Position := FPosition;

  FCurWindowOpenedHalfModal := FHalfModal;
  FCurWindowOpen := true;
  if FHalfModal then
    mc := DLG_Statusanzeige.ShowModalStart
  else
    DLG_Statusanzeige.Show;

  Inc(OpenedProgressDlgs);

  DLG_Statusanzeige.Update;
end;

procedure TProgressDlg.Close;
begin
  if not Assigned(DLG_Statusanzeige) then
    exit;
  if not FCurWindowOpen then
    exit;

  FCurWindowOpen := false;
  if FCurWindowOpenedHalfModal then
    DLG_Statusanzeige.ShowModalEnd(mc)
  else
    DLG_Statusanzeige.Close;

  FreeAndNil(DLG_Statusanzeige);

  Dec(OpenedProgressDlgs);

  // TODO: Design62_ZeichneHintergrundschrift wird nicht ausgeführt... warum?!
  (*
    if Assigned(Application) and Assigned(Application.MainForm) then
    begin
    Application.MainForm.Invalidate;
    Application.MainForm.Repaint;
    end;
  *)
end;

procedure TProgressDlg.IncPos;
begin
  Position := Position + 1;
  // TestStop;
end;

procedure TProgressDlg.Reset;
begin
  FPosition := 0;

  if Assigned(DLG_Statusanzeige) then
  begin
    DLG_Statusanzeige.TimerRestzeit.Enabled := false;
    DLG_Statusanzeige.Text := FText;
    DLG_Statusanzeige.ShowTime := ShowTime;
    DLG_Statusanzeige.MaxValue := FMax;
    DLG_Statusanzeige.Position := 0;
    DLG_Statusanzeige.ShowExactPosition := FExactPosition;
    DLG_Statusanzeige.ShowStopButton := ShowStopButton;
    DLG_Statusanzeige.btnStop.OnClick := FormBtnStopClick;
  end;

  ResetStopButtonSignal;

  if Assigned(DLG_Statusanzeige) then
  begin
    DLG_Statusanzeige.ShowTime := false;
    DLG_Statusanzeige.ShowTime := ShowTime; // This resets the timer
    DLG_Statusanzeige.Update;
  end;

  Application.ProcessMessages;
end;

procedure TProgressDlg.ResetStopButtonSignal;
begin
  FStopButtonSignal := false;
end;

procedure TProgressDlg.SetExactPosition(const Value: boolean);
begin
  FExactPosition := Value;
  if Assigned(DLG_Statusanzeige) then
  begin
    DLG_Statusanzeige.ShowExactPosition := Value;
  end;
end;

procedure TProgressDlg.SetMax(const Value: Int64);
begin
  FMax := Value;
  if Assigned(DLG_Statusanzeige) then
  begin
    DLG_Statusanzeige.MaxValue := Value;
  end;
  Application.ProcessMessages;
end;

procedure TProgressDlg.SetPosition(const Value: Int64);
begin
  // FPosition := Value mod (FMax + 1);
  if Value > FMax then
    FPosition := FMax
  else if Value < 0 { FMin } then
    FPosition := 0 { FMin }
  else
    FPosition := Value;

  if Assigned(DLG_Statusanzeige) then
  begin
    DLG_Statusanzeige.Position := FPosition;

    (*
      Dieser Code sorgt dafür, dass die Restzeit-Anzeige aktualisiert wird.
      Der Timer funktioniert leider NICHT eigenständig, da der Mainthread (in dem auch der
      Warten-Dialog untergebracht ist) normalerweise ausgelastet ist (es sei denn, man
      macht Thread-Programmierung).
      Das Aktivieren von ProcessMessages würde das Problem lösen, und man könnte auch
      das Warten-Fenter dann per Drag'n'Drop verschieben, ABER der Nachteil ist, dass
      man mit CORA arbeiten könnte und Dinge verstellen kann, weswegen z.B. eine
      Auswertung fehlschlagen würde.
      Daher verwenden wir kein ProcessMessages, sondern stupsen den Timer automatisch an.
      -- DM 30.03.2016

      TODO: Hier gibt es aber trotzdem noch Probleme:
      1. Wenn ein Tick länger dauert als der TimerRestzeit, wird die Zeit nicht im korrekten Interval aktualisiert
      2. Da kein ProcessMessages gemacht wird, wird das Fenster milchig, da Windows davon ausgeht, dass die Anwendung nicht reagiert
      ==> Besser wäre also ProcessMessages, aber dann den Dialog in einen ModalDialog ändern!
      ==> Wurde mit dem HalfModalMode zum Teil erreicht...
    *)

    if FCurWindowOpenedHalfModal then
      Application.ProcessMessages
    else
      DLG_Statusanzeige.TimerRestzeitTimer(DLG_Statusanzeige.TimerRestzeit);
  end;
end;

procedure TProgressDlg.SetShowTime(const Value: boolean);
begin
  FShowTime := Value;

  if Assigned(DLG_Statusanzeige) then
  begin
    DLG_Statusanzeige.ShowTime := Value;
  end;

  Application.ProcessMessages;
end;

procedure TProgressDlg.SetStopButton(const Value: boolean);
begin
  FShowStopButton := Value;

  if Assigned(DLG_Statusanzeige) then
  begin
    DLG_Statusanzeige.btnStop.Visible := Value;
  end;

  Application.ProcessMessages;
end;

procedure TProgressDlg.SetText(aValue: string);
begin
  if aValue = FText then
    exit;
  FText := aValue;

  if Assigned(DLG_Statusanzeige) then
  begin
    DLG_Statusanzeige.Text := FText;
  end;

  Application.ProcessMessages;
end;

procedure TProgressDlg.TestStop;
begin
  if Application.Terminated or FStopButtonSignal then
    Abort;
end;

end.
