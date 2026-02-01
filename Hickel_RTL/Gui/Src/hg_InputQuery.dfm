object hgInputQry: ThgInputQry
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'hgInputQry'
  ClientHeight = 109
  ClientWidth = 451
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poMainFormCenter
  OnShow = FormShow
  DesignSize = (
    451
    109)
  TextHeight = 13
  object Label1: TLabel
    Left = 56
    Top = 8
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object Button1: TButton
    Left = 291
    Top = 68
    Width = 100
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object Edit1: TEdit
    Left = 56
    Top = 27
    Width = 335
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = 'Edit1'
    ExplicitWidth = 345
  end
  object Button2: TButton
    Left = 56
    Top = 68
    Width = 100
    Height = 25
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Abbrechen'
    ModalResult = 2
    TabOrder = 2
  end
  object ComboBox1: TComboBox
    Left = 56
    Top = 27
    Width = 335
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    Text = 'ComboBox1'
  end
  object Memo1: TMemo
    Left = 56
    Top = 27
    Width = 335
    Height = 23
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'Memo1')
    TabOrder = 4
    Visible = False
    OnKeyDown = Memo1KeyDown
  end
end
