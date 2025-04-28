object DLG_OpenMySqlDb: TDLG_OpenMySqlDb
  Left = 733
  Top = 124
  BorderStyle = bsDialog
  Caption = 'MySQL-Datenbank '#246'ffnen'
  ClientHeight = 430
  ClientWidth = 584
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Icon.Data = {
    0000010001001010100000000000280100001600000028000000100000002000
    0000010004000000000080000000000000000000000010000000100000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    0000008000000880008870088880000780700078070870807807807708080887
    0070070700800700707008070000000700700807000080777708807700000000
    8800008700000000878880700000000077177818000000008711711700000000
    081171170000000000888117000000000000077800000000000000000000FFDF
    00009C0100008400000000000000818500008187000089870000000700008007
    0000F05F0000F00F0000F00F0000F80F0000FC0F0000FF8F0000FFFF0000}
  Position = poMainFormCenter
  OnShow = FormShow
  DesignSize = (
    584
    430)
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 142
    Height = 13
    Caption = 'Name des Datenbankservers:'
  end
  object Label2: TLabel
    Left = 16
    Top = 68
    Width = 118
    Height = 13
    Caption = 'Zu '#246'ffnende Datenbank:'
  end
  object LbSpeedButton1: TSpeedButton
    Left = 405
    Top = 64
    Width = 80
    Height = 23
    Anchors = [akTop, akRight]
    Caption = 'Aktualisieren'
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    OnClick = eServerExit
    ExplicitLeft = 394
  end
  object Image1: TImage
    Left = 469
    Top = 16
    Width = 16
    Height = 13
    Cursor = crHandPoint
    Hint = 'Favoriten-Liste bearbeiten'
    Anchors = [akTop, akRight]
    AutoSize = True
    ParentShowHint = False
    Picture.Data = {
      07544269746D6170DE000000424DDE0000000000000076000000280000001000
      00000D000000010004000000000068000000120B0000120B0000100000000000
      0000000000000000800000800000008080008000000080008000808000008080
      8000C0C0C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
      FF0000000000000000000FFFFFFFFFFFFFF00F00F00F00F000F00FFFFFFFFFFF
      FFF00F00F00F00F000F00FFFFFFFFFFFFFF00F00F00F00F000F00FFFFFFFFFFF
      FFF00F00F00F00F000F00FFFFFFFFFFFFFF00CCCCCCCCCCCCCC0088CCCCCCCCC
      C8800000000000000000}
    ShowHint = True
    OnClick = Image1Click
  end
  object LbButton1: TButton
    Left = 499
    Top = 16
    Width = 77
    Height = 23
    Anchors = [akTop, akRight]
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
    Left = 499
    Top = 45
    Width = 77
    Height = 23
    Anchors = [akTop, akRight]
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
  object lbDatabases: TListBox
    Left = 16
    Top = 88
    Width = 469
    Height = 321
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    Sorted = True
    TabOrder = 1
    OnDblClick = LbButton1Click
  end
  object eServer: TComboBox
    Left = 16
    Top = 35
    Width = 469
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = 'localhost'
    OnChange = eServerChange
  end
end
