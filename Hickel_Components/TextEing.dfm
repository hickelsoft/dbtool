object DLG_Texteingabe: TDLG_Texteingabe
  Left = 704
  Top = 120
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'Text-Eingabe'
  ClientHeight = 79
  ClientWidth = 407
  Color = 12648447
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 407
    Height = 20
    Align = alTop
    Alignment = taLeftJustify
    BevelOuter = bvLowered
    Caption = ' Text-Eingabe'
    Color = clNavy
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentBackground = False
    ParentFont = False
    TabOrder = 0
  end
  object Panel2: TPanel
    Left = 0
    Top = 20
    Width = 407
    Height = 59
    Align = alClient
    BevelOuter = bvLowered
    Color = 12648447
    TabOrder = 1
    object Edit: TEdit
      Left = 4
      Top = 4
      Width = 400
      Height = 21
      MaxLength = 80
      TabOrder = 0
      Text = 'Edit'
      OnEnter = EditEnter
      OnExit = EditExit
    end
    object BitBtn1: TBitBtn
      Left = 200
      Top = 30
      Width = 100
      Height = 25
      Caption = '&OK'
      TabOrder = 1
      Kind = bkOK
    end
    object BitBtn2: TBitBtn
      Left = 303
      Top = 30
      Width = 100
      Height = 25
      Caption = '&Abbrechen'
      TabOrder = 2
      Kind = bkCancel
    end
    object CheckBox1: TCheckBox
      Left = 4
      Top = 34
      Width = 190
      Height = 17
      TabOrder = 3
      Visible = False
    end
    object DateTimePicker1: TDateTimePicker
      Left = 4
      Top = 4
      Width = 157
      Height = 21
      Date = 43342.000000000000000000
      Time = 43342.000000000000000000
      TabOrder = 4
    end
  end
end
