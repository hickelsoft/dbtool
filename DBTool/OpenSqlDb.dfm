object DLG_OpenSqlDb: TDLG_OpenSqlDb
  Left = 733
  Top = 124
  BorderStyle = bsDialog
  Caption = 'SQL-Server-Datenbank '#246'ffnen'
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
    00000000000000000077FFFFF0007888887FF2FFFF00788897FFF22FFFF07888
    87FFF222FFF0788887FFF2222FF0788887FFF222FFF07FFFF7FFF22FFFF07777
    777FF2FFFF0078888877FFFFF0007FFFFFF77777700077777770700000007888
    8887000000007FFFFFF77000000078888888700000000777777770000000FF07
    0000800100000001000000000000000000000000000000000000000000000001
    00000001000000070000003F0000003F0000003F0000003F0000807F0000}
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
    Left = 401
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
    Left = 465
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
    Left = 495
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
    Left = 495
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
    Width = 465
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
    Width = 465
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = 'localhost'
    OnChange = eServerChange
  end
  object XMLDocument1: TXMLDocument
    Left = 496
    Top = 312
    DOMVendorDesc = 'MSXML'
  end
end
