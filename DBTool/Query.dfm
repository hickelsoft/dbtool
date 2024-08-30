object MDI_Query: TMDI_Query
  Left = 437
  Top = 117
  Caption = 'Abfrage'
  ClientHeight = 340
  ClientWidth = 752
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsMDIChild
  Icon.Data = {
    0000010001001010000001002000680400001600000028000000100000002000
    0000010020000000000000040000890B0000890B00000000000000000000FFFF
    FF00FFFFFF00FFFFFF00FFFFFF00808080FF808080FF808080FF808080FF8080
    80FF808080FF808080FF808080FF808080FF808080FF000000FFFFFFFF00FFFF
    FF00FFFFFF00FFFFFF00808080FFC0C0C0FF808080FFC0C0C0FF808080FFC0C0
    C0FF808080FFC0C0C0FF808080FFC0C0C0FF808080FF808080FF000000FFFFFF
    FF00FFFFFF00808080FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0
    C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FF808080FF000000FFFFFF
    FF00FFFFFF00808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0C0FF000000FF8000
    8081000080FF80008081000080FF800080FF000080FF800080FF000080FF8000
    80FF000080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0C0FF000000FF0000
    80FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
    FFFF800080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0C0FF000000FF8000
    80FFFFFFFFFF800080FF000080FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
    FFFF000080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0C0FF000000FF0000
    80FFFFFFFFFF000080FF800080FF000080FF808080FFFFFFFFFFFFFFFFFFFFFF
    FFFF800080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0C0FF000000FF8000
    80FFFFFFFFFFFFFFFFFF000080FF800080FF000080FF800080FF808080FFFFFF
    FFFF000080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0C0FF000000FF0000
    80FFFFFFFFFFFFFFFFFFFFFFFFFF000080FF800080FF000080FF800080FFFFFF
    FFFF800080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0C0FF000000FF8000
    80FFFFFFFFFFFFFFFFFFFFFFFFFF800080FF000080FFFFFFFFFF000080FFFFFF
    FFFF000080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0C0FF000000FF0000
    80FFFFFFFFFFFFFFFFFFFFFFFFFF808080FF800080FF000080FFC0C0C0FFFFFF
    FFFF800080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0C0FF000000FF8000
    80FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
    FFFF000080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0C0FF000000FF0000
    80FF80008081000080FF80008081000080FF800080FF000080FF800080FF0000
    80FF800080FF008080FF008080FF008080FF008080FFC0C0C0FF000000FFFFFF
    FF00FFFFFF00FFFFFF00008080FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF
    FFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF008080FF000000FFFFFF
    FF00FFFFFF00FFFFFF00FFFFFF00008080FF008080FF008080FF008080FF0080
    80FF008080FF008080FF008080FF008080FF008080FF008080FF000000FFF001
    0000E0000000C0000000C0000000000000000000000000000000000000000000
    00000000000000000000000000000000000000000000E0000000F0000000}
  KeyPreview = True
  Position = poDefault
  Visible = True
  OnActivate = FormActivate
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDeactivate = FormDeactivate
  OnKeyDown = FormKeyDown
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 100
    Height = 340
    Align = alLeft
    BevelOuter = bvLowered
    Color = 5263440
    ParentBackground = False
    TabOrder = 0
    Visible = False
    object HsGradientPanel1: THsGradientPanel
      Left = 1
      Top = 1
      Width = 98
      Height = 338
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
    Width = 652
    Height = 340
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object Notebook1: TNotebook
      Left = 0
      Top = 0
      Width = 652
      Height = 340
      Align = alClient
      TabOrder = 0
      object TPage
        Left = 0
        Top = 0
        Caption = 'Tabelle'
        object Splitter1: TSplitter
          Left = 0
          Top = 82
          Width = 652
          Height = 3
          Cursor = crVSplit
          Align = alTop
          ExplicitWidth = 258
        end
        object Panel3: TPanel
          Left = 0
          Top = 0
          Width = 652
          Height = 24
          Align = alTop
          Alignment = taLeftJustify
          BevelOuter = bvNone
          Caption = ' Abfrage'
          Color = 5263440
          Font.Charset = ANSI_CHARSET
          Font.Color = clWhite
          Font.Height = -16
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentBackground = False
          ParentFont = False
          TabOrder = 1
          object btnNewQueryWindow: TSpeedButton
            Left = 487
            Top = 0
            Width = 90
            Height = 24
            Align = alRight
            Caption = 'Neues Fenster'
            Flat = True
            Font.Charset = ANSI_CHARSET
            Font.Color = clWhite
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            OnClick = btnNewQueryWindowClick
            ExplicitLeft = 480
          end
          object SpeedButton2: TSpeedButton
            Left = 577
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
            OnClick = SpeedButton1Click
            ExplicitLeft = 445
          end
          object btnSaveQuery: TSpeedButton
            Left = 397
            Top = 0
            Width = 90
            Height = 24
            Align = alRight
            Caption = 'Speichern'
            Flat = True
            Font.Charset = ANSI_CHARSET
            Font.Color = clWhite
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            OnClick = btnSaveQueryClick
            ExplicitLeft = 480
          end
          object btnLoadQuery: TSpeedButton
            Left = 307
            Top = 0
            Width = 90
            Height = 24
            Align = alRight
            Caption = 'Laden'
            Flat = True
            Font.Charset = ANSI_CHARSET
            Font.Color = clWhite
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            OnClick = btnLoadQueryClick
            ExplicitLeft = 480
          end
          object btnCSVExport: TSpeedButton
            Left = 217
            Top = 0
            Width = 90
            Height = 24
            Align = alRight
            Caption = 'CSV Export'
            Flat = True
            Font.Charset = ANSI_CHARSET
            Font.Color = clWhite
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            OnClick = btnCSVExportClick
            ExplicitLeft = 480
          end
        end
        object dbgTable: TwwDBGrid
          Left = 0
          Top = 85
          Width = 652
          Height = 230
          TabStop = False
          MemoAttributes = [mSizeable, mWordWrap, mGridShow]
          IniAttributes.Delimiter = ';;'
          IniAttributes.UnicodeIniFile = False
          TitleColor = clBtnFace
          FixedCols = 0
          ShowHorzScrollBar = True
          EditControlOptions = [ecoCheckboxSingleClick, ecoSearchOwnerForm]
          Align = alClient
          BorderStyle = bsNone
          DataSource = DataSource1
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          KeyOptions = [dgEnterToTab, dgAllowDelete, dgAllowInsert]
          Options = [dgEditing, dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgConfirmDelete, dgCancelOnExit, dgWordWrap, dgTrailingEllipsis, dgShowCellHint, dgFixedResizable, dgRowResize]
          ParentFont = False
          TabOrder = 2
          TitleAlignment = taLeftJustify
          TitleFont.Charset = ANSI_CHARSET
          TitleFont.Color = clWindowText
          TitleFont.Height = -11
          TitleFont.Name = 'Tahoma'
          TitleFont.Style = []
          TitleLines = 1
          TitleButtons = False
          Visible = False
          LoadAllRTF = True
          OnCalcCellColors = dbgTableCalcCellColors
          OnKeyUp = dbgTableKeyUp
          PaintOptions.AlternatingRowRegions = [arrDataColumns, arrActiveDataColumn]
        end
        object Panel6: TPanel
          Left = 0
          Top = 24
          Width = 652
          Height = 58
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          object Memo1: TMemo
            Left = 46
            Top = 0
            Width = 606
            Height = 58
            Align = alClient
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Courier'
            Font.Style = []
            ParentFont = False
            ScrollBars = ssVertical
            TabOrder = 0
            OnKeyDown = Memo1KeyDown
          end
          object Panel7: TPanel
            Left = 0
            Top = 0
            Width = 46
            Height = 58
            Align = alLeft
            BevelOuter = bvNone
            BorderWidth = 8
            TabOrder = 1
            object SpeedButton1: TSpeedButton
              Left = 8
              Top = 8
              Width = 30
              Height = 30
              Hint = 'SQL-Befehl ausf'#252'hren'
              Align = alTop
              Glyph.Data = {
                F6000000424DF600000000000000760000002800000010000000100000000100
                04000000000080000000880B0000880B00001000000000000000000000000000
                8000008000000080800080000000800080008080000080808000C0C0C0000000
                FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDD77777777
                770DDDD7878787878770DD78888888888870DD7FFFFFFFFFFF805151515151FF
                FF801FFFFFFFF5FFFF805F517FFFF1FFFF801F1517FFF5FFFF805FF15157F1FF
                FF801FFF1515F5FFFF805FFF51F1F1FFFF801FFF7518F5FFFF805FFFFFFFF1FF
                FF801515151515333380DDD3BBBBBBBBBB30DDDD333333333330}
              ParentShowHint = False
              ShowHint = True
              OnClick = SpeedButton1Click
            end
          end
        end
        object QueryStatusPanel: TPanel
          Left = 0
          Top = 315
          Width = 652
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
          TabOrder = 3
        end
      end
    end
  end
  object DataSource1: TDataSource
    OnDataChange = DataSource1DataChange
    Left = 200
    Top = 216
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'sql'
    Filter = 'SQL-Skripts (*.sql)|*.sql|Alle Dateien (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Title = 'SQL speichern'
    Left = 72
    Top = 188
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'sql'
    Filter = 'SQL-Skripts (*.sql)|*.sql|Alle Dateien (*.*)|*.*'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Title = 'SQL '#246'ffnen'
    Left = 44
    Top = 188
  end
  object FindDialog1: TFindDialog
    OnClose = FindDialog1Close
    Options = [frDown, frHideWholeWord, frHideUpDown]
    OnFind = FindDialog1Find
    Left = 104
    Top = 184
  end
end
