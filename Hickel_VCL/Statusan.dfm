object DLG_Statusanzeige: TDLG_Statusanzeige
  Left = 546
  Top = 142
  ActiveControl = Panel3
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'Bitte warten...'
  ClientHeight = 117
  ClientWidth = 352
  Color = clAqua
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poDefault
  OnCreate = FormCreate
  TextHeight = 13
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 352
    Height = 20
    Align = alTop
    Alignment = taLeftJustify
    BevelOuter = bvLowered
    Caption = '  Bitte warten...'
    Color = clNavy
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentBackground = False
    ParentFont = False
    TabOrder = 0
    OnMouseDown = DoMouseDown
  end
  object Panel1: TPanel
    Left = 0
    Top = 20
    Width = 352
    Height = 97
    Align = alClient
    BevelOuter = bvLowered
    Color = clAqua
    TabOrder = 1
    object HsGauge1: THsGauge
      Left = 1
      Top = 1
      Width = 350
      Height = 95
      Align = alClient
      BorderStyle = bsNone
      ForeColor = clAqua
      Kind = gkVerticalBar
      Progress = 100
      ShowText = False
      ForeColor2 = clWhite
    end
    object LabelRestzeit: TLabel
      Left = 252
      Top = 80
      Width = 95
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Restzeit ca. xx:xx'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object StatusText: TLabel
      Left = 5
      Top = 5
      Width = 342
      Height = 69
      AutoSize = False
      Caption = 'StatusText'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Transparent = True
      WordWrap = True
    end
    object LblExactPosition: TLabel
      Left = 5
      Top = 60
      Width = 9
      Height = 13
      Caption = '...'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Transparent = True
      Visible = False
    end
    object Panel3: TPanel
      Left = 2
      Top = 79
      Width = 248
      Height = 15
      BevelOuter = bvLowered
      Caption = 'Panel1'
      TabOrder = 0
      object ProgressBar1: THsGauge
        Left = 1
        Top = 1
        Width = 246
        Height = 13
        Align = alClient
        BorderStyle = bsNone
        ForeColor = clAqua
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Kind = gkHorizontalBar
        ParentFont = False
        Progress = 50
        ForeColor2 = clNavy
      end
    end
    object btnStop: TButton
      Left = 280
      Top = 48
      Width = 67
      Height = 26
      Cancel = True
      Caption = 'Abbrechen'
      TabOrder = 1
      TabStop = False
      Visible = False
      OnKeyDown = btnStopKeyDown
    end
  end
  object TimerRestzeit: TTimer
    Enabled = False
    OnTimer = TimerRestzeitTimer
    Left = 12
    Top = 40
  end
end
