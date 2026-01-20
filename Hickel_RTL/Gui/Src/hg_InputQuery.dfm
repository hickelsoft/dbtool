object hgInputQry: ThgInputQry
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'hgInputQry'
  ClientHeight = 103
  ClientWidth = 391
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poMainFormCenter
  OnShow = FormShow
  TextHeight = 13
  object Label1: TLabel
    Left = 56
    Top = 8
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object Button1: TButton
    Left = 254
    Top = 62
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object Edit1: TEdit
    Left = 56
    Top = 27
    Width = 273
    Height = 21
    TabOrder = 0
    Text = 'Edit1'
  end
  object Button2: TButton
    Left = 56
    Top = 62
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Abbrechen'
    ModalResult = 2
    TabOrder = 2
  end
  object ComboBox1: TComboBox
    Left = 56
    Top = 27
    Width = 273
    Height = 21
    TabOrder = 3
    Text = 'ComboBox1'
  end
end
