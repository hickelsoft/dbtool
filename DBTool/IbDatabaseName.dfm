object DLG_IbDatabaseName: TDLG_IbDatabaseName
  Left = 284
  Top = 107
  BorderStyle = bsDialog
  Caption = 'InterBase-Fehler'
  ClientHeight = 109
  ClientWidth = 464
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Position = poScreenCenter
  OnKeyDown = FormKeyDown
  TextHeight = 13
  object Label1: TLabel
    Left = 12
    Top = 16
    Width = 315
    Height = 41
    AutoSize = False
    Caption = 
      'Die Datenbank konnte nicht ge'#246'ffnet werden. Vermutlich befindet ' +
      'sie sich auf einem anderen Computer. Bitte geben Sie den Namen a' +
      'n, mit dem die Datenbank ge'#246'ffnet werden soll:'
    WordWrap = True
  end
  object LbButton1: TButton
    Left = 352
    Top = 12
    Width = 100
    Height = 23
    Caption = 'OK'
    Default = True
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ModalResult = 1
    ParentFont = False
    TabOrder = 0
  end
  object LbButton2: TButton
    Left = 352
    Top = 40
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
    TabOrder = 1
  end
  object Edit1: TComboBox
    Left = 12
    Top = 72
    Width = 440
    Height = 21
    Sorted = True
    TabOrder = 2
  end
end
