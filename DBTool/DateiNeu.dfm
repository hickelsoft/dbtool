object DLG_DateiNeu: TDLG_DateiNeu
  Left = 279
  Top = 107
  BorderStyle = bsDialog
  Caption = 'Neue Datenbank'
  ClientHeight = 151
  ClientWidth = 426
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Position = poScreenCenter
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  TextHeight = 13
  object lDateiname: TLabel
    Left = 136
    Top = 12
    Width = 55
    Height = 13
    Caption = 'Dateiname:'
  end
  object btnDatei: TSpeedButton
    Left = 392
    Top = 28
    Width = 21
    Height = 21
    Caption = '...'
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    OnClick = btnDateiClick
  end
  object lServer: TLabel
    Left = 136
    Top = 60
    Width = 36
    Height = 13
    Caption = 'Server:'
  end
  object rgTyp: TRadioGroup
    Left = 8
    Top = 8
    Width = 113
    Height = 133
    Caption = 'Datenbanktyp'
    Items.Strings = (
      'InterBase'
      'Jet / Access'
      'MySQL (ODBC)'
      'SQL-Server'
      'Firebird')
    TabOrder = 0
    OnClick = rgTypClick
  end
  object eDateiname: TEdit
    Left = 136
    Top = 28
    Width = 257
    Height = 21
    TabOrder = 1
  end
  object LbButton1: TButton
    Left = 206
    Top = 116
    Width = 100
    Height = 23
    Caption = 'OK'
    Default = True
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    OnClick = LbButton1Click
  end
  object LbButton2: TButton
    Left = 313
    Top = 116
    Width = 100
    Height = 23
    Cancel = True
    Caption = 'Abbrechen'
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ModalResult = 2
    ParentFont = False
    TabOrder = 3
  end
  object eServer: TEdit
    Left = 136
    Top = 76
    Width = 277
    Height = 21
    TabOrder = 4
    Text = 'localhost'
  end
  object SaveDialog1: TSaveDialog
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Title = 'Neue Datenbank'
    Left = 384
    Top = 56
  end
end
