object MDI_Database: TMDI_Database
  Left = 554
  Top = 123
  Caption = 'Datenbank'
  ClientHeight = 492
  ClientWidth = 837
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsMDIChild
  Icon.Data = {
    0000010001001010100000000000280100001600000028000000100000002000
    00000100040000000000C0000000000000000000000000000000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    0000000000000000BBFBFB000000003B3BFFF3B3000003B3BBFBFB3B30000B3B
    000000B3B00003003BFFF300300000B3BBFBFB3B00000B3B000000B3B0000300
    3BFFF300300000B3BBFBFB3B00000B3B000000B3B0000300BFBFBF00300000FB
    FBFBFBFB000000FFBFBFBFFF00000000FFFFFF0000000000000000000000FFFF
    0000F03F0000C00F0000800700008007000080070000C00F0000800700008007
    0000C00F00008007000080070000C00F0000C00F0000F03F0000FFFF0000}
  KeyPreview = True
  Position = poDefault
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 100
    Height = 492
    Align = alLeft
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Color = 5263440
    ParentBackground = False
    TabOrder = 0
    Visible = False
    object HsGradientPanel1: THsGradientPanel
      Left = 0
      Top = 0
      Width = 96
      Height = 488
      MoveWho = mwNone
      BeginColor = 5263440
      EndColor = 11711154
      GradientStyle = gsVertical
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
    end
  end
  object Panel2: TPanel
    Left = 100
    Top = 0
    Width = 737
    Height = 492
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object Notebook1: TNotebook
      Left = 0
      Top = 0
      Width = 737
      Height = 492
      Align = alClient
      TabOrder = 0
      object TPage
        Left = 0
        Top = 0
        Caption = 'Tabellen'
        object Panel3: TPanel
          Left = 0
          Top = 0
          Width = 737
          Height = 24
          Align = alTop
          Alignment = taLeftJustify
          BevelOuter = bvNone
          Caption = ' Datenbank'
          Color = 5263440
          Font.Charset = ANSI_CHARSET
          Font.Color = clWhite
          Font.Height = -16
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentBackground = False
          ParentFont = False
          TabOrder = 0
          object btnAktualisieren: TSpeedButton
            Left = 662
            Top = 0
            Width = 75
            Height = 24
            Align = alRight
            Caption = 'Aktualisieren'
            Flat = True
            Font.Charset = ANSI_CHARSET
            Font.Color = clWhite
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            OnClick = btnAktualisierenClick
            ExplicitLeft = 315
          end
          object SpeedButton2: TSpeedButton
            Left = 576
            Top = 0
            Width = 86
            Height = 24
            Align = alRight
            Caption = 'Neue Abfrage'
            Flat = True
            Font.Charset = ANSI_CHARSET
            Font.Color = clWhite
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            OnClick = SpeedButton2Click
            ExplicitLeft = 360
          end
        end
        object lvTables: TListView
          Left = 0
          Top = 24
          Width = 737
          Height = 443
          Align = alClient
          Columns = <>
          DragMode = dmAutomatic
          MultiSelect = True
          PopupMenu = pmTables
          SortType = stText
          TabOrder = 1
          ViewStyle = vsList
          OnCustomDrawItem = lvTablesCustomDrawItem
          OnDblClick = lvTablesDblClick
          OnEdited = lvTablesEdited
          OnDragDrop = lvTablesDragDrop
          OnDragOver = lvTablesDragOver
          OnKeyDown = lvTablesKeyDown
          OnSelectItem = lvTablesSelectItem
        end
        object Panel5: TPanel
          Left = 0
          Top = 467
          Width = 737
          Height = 25
          Align = alBottom
          Alignment = taLeftJustify
          BevelOuter = bvLowered
          Color = 5263440
          Font.Charset = ANSI_CHARSET
          Font.Color = clWhite
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentBackground = False
          ParentFont = False
          TabOrder = 2
          object Panel4: TPanel
            Left = 248
            Top = 1
            Width = 488
            Height = 23
            Align = alRight
            Alignment = taRightJustify
            BevelOuter = bvNone
            Caption = 
              'schwarz = Tabellen,  gr'#252'n = View,  blau = Stored Procedures,  fe' +
              'tt = mit Trigger   '
            Color = 5263440
            Font.Charset = ANSI_CHARSET
            Font.Color = clWhite
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentBackground = False
            ParentFont = False
            TabOrder = 0
          end
        end
      end
    end
  end
  object pmTables: TPopupMenu
    OnPopup = pmTablesPopup
    Left = 136
    Top = 56
    object ffnen1: TMenuItem
      Caption = #214'&ffnen'
      Default = True
      OnClick = lvTablesDblClick
    end
    object NurStrukturzeigen1: TMenuItem
      Caption = 'Nur &Struktur zeigen'
      OnClick = NurStrukturzeigen1Click
    end
    object ViewDefinitionanzeigen1: TMenuItem
      Caption = 'View Definition anzeigen'
      OnClick = ViewDefinitionanzeigen1Click
    end
    object Triggeranzeigen1: TMenuItem
      Caption = 'Trigger anzeigen'
      OnClick = Triggeranzeigen1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object uUmbenennen1: TMenuItem
      Caption = '&Umbenennen'
      OnClick = Umbenennen1Click
    end
    object Leeren1: TMenuItem
      Caption = '&Leeren'
      OnClick = Leeren1Click
    end
    object Loeschen1: TMenuItem
      Caption = 'L'#246'sche&n'
      OnClick = Loeschen1Click
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object Kopierennach1: TMenuItem
      Caption = 'Kopieren nach'
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object AnzahlZeilenzeigen1: TMenuItem
      Caption = 'Anzahl Zeilen zeigen'
      OnClick = AnzahlZeilenzeigen1Click
    end
    object Erste100Zeilenzeigen1: TMenuItem
      Caption = 'Erste 1000 Zeilen zeigen'
      OnClick = Erste100Zeilenzeigen1Click
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object Strukturdrucken1: TMenuItem
      Caption = 'Struktur &drucken'
      Visible = False
      OnClick = Strukturdrucken1Click
    end
    object abellenNamenkopieren1: TMenuItem
      Caption = 'Tabellen-Namen kopieren'
      OnClick = abellenNamenkopieren1Click
    end
  end
end
