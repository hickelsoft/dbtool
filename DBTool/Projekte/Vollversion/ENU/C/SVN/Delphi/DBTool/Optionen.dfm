object DLG_Optionen: TDLG_Optionen
  Left = 287
  Top = 118
  BorderStyle = bsDialog
  Caption = 'Optionen'
  ClientHeight = 378
  ClientWidth = 356
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
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 237
    Height = 178
    Caption = 'Als Standard-Anwendung einrichten'
    TabOrder = 0
    object btnStandardDB: TSpeedButton
      Left = 16
      Top = 142
      Width = 205
      Height = 25
      Hint = 'DBTool als Standard-Anwendung f'#252'r Datenbanken einrichten'
      Caption = ' Paradox-Tabellen (*.db)'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      OnClick = btnStandardDBClick
    end
    object btnStandardDBF: TSpeedButton
      Left = 16
      Top = 86
      Width = 205
      Height = 25
      Hint = 'DBTool als Standard-Anwendung f'#252'r Datenbanken einrichten'
      Caption = ' dBase-Tabellen (*.dbf)'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      OnClick = btnStandardDBFClick
    end
    object btnStandardMDB: TSpeedButton
      Left = 16
      Top = 24
      Width = 205
      Height = 25
      Hint = 'DBTool als Standard-Anwendung f'#252'r Datenbanken einrichten'
      Caption = ' Access 97-Datenbanken (*.mdb)'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      OnClick = btnStandardMDBClick
    end
    object btnStandardGDB: TSpeedButton
      Left = 16
      Top = 114
      Width = 205
      Height = 25
      Hint = 'DBTool als Standard-Anwendung f'#252'r Datenbanken einrichten'
      Caption = ' InterBase-Datenbanken (*.gdb)'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      OnClick = btnStandardGDBClick
    end
    object btnStandardAccDb: TSpeedButton
      Left = 16
      Top = 55
      Width = 205
      Height = 25
      Hint = 'DBTool als Standard-Anwendung f'#252'r Datenbanken einrichten'
      Caption = ' Access-Datenbanken (*.accdb)'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      OnClick = btnStandardAccDbClick
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 192
    Width = 237
    Height = 169
    Caption = 'Farben'
    TabOrder = 1
    Visible = False
    object btnHintergrund: TSpeedButton
      Left = 132
      Top = 20
      Width = 90
      Height = 24
      OnClick = btnFarbeClick
    end
    object Label1: TLabel
      Left = 16
      Top = 24
      Width = 100
      Height = 13
      Caption = 'Tabellenhintergrund:'
    end
    object btnText: TSpeedButton
      Left = 132
      Top = 76
      Width = 90
      Height = 24
      OnClick = btnFarbeClick
    end
    object Label2: TLabel
      Left = 16
      Top = 80
      Width = 26
      Height = 13
      Caption = 'Text:'
    end
    object btnZeile: TSpeedButton
      Left = 132
      Top = 104
      Width = 90
      Height = 24
      OnClick = btnFarbeClick
    end
    object Label3: TLabel
      Left = 16
      Top = 108
      Width = 67
      Height = 13
      Caption = 'Aktuelle Zeile:'
    end
    object btnFeld: TSpeedButton
      Left = 132
      Top = 132
      Width = 90
      Height = 24
      OnClick = btnFarbeClick
    end
    object Label4: TLabel
      Left = 16
      Top = 136
      Width = 70
      Height = 13
      Caption = 'Aktuelles Feld:'
    end
    object Label5: TLabel
      Left = 16
      Top = 52
      Width = 109
      Height = 13
      Caption = 'Tabellenhintergrund 2:'
    end
    object btnZebra: TSpeedButton
      Left = 132
      Top = 48
      Width = 90
      Height = 24
      OnClick = btnFarbeClick
    end
  end
  object LbButton1: TButton
    Left = 256
    Top = 12
    Width = 90
    Height = 25
    Caption = 'OK'
    Default = True
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ModalResult = 1
    ParentFont = False
    TabOrder = 2
    OnClick = LbButton1Click
  end
  object LbButton2: TButton
    Left = 256
    Top = 44
    Width = 90
    Height = 25
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
  object ColorDialog1: TColorDialog
    Options = [cdSolidColor]
    Left = 264
    Top = 244
  end
end
