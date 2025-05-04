unit Export;

// TODO: Problem: "Memo" Felder werden im Export überhaupt nicht angezeigt und also auch nicht exportiert
// TODO: fragen ob man datei ersetzen möchte

// TODO: Folgende Feldtypen sind derzeit nicht implementiert:
(*
  ftUnknown
  ftBytes
  ftVarBytes
  ftFmtMemo
  ftParadoxOle
  ftDBaseOle
  ftTypedBinary
  ftCursor
  ftADT
  ftArray
  ftReference
  ftDataSet
  ftOraBlob
  ftOraClob
  ftVariant
  ftInterface
  ftIDispatch
  ftTimeStamp
  ftOraTimeStamp
  ftOraInterval
  ftConnection
  ftParams
  ftStream
  ftTimeStampOffset
  ftObject
*)

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, {$IFNDEF WIN64}DBTables, {$ENDIF}StdCtrls, ExtCtrls, Buttons, Gauges,
  ShellAPI;

type
  TDLG_Export = class(TForm)
    btnFertig: TButton;
    btnWeiter: TButton;
    btnZurueck: TButton;
    btnCancel: TButton;
    Bevel1: TBevel;
    SaveDialog1: TSaveDialog;
    Notebook1: TNotebook;
    Shape1: TShape;
    Label2: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Panel8: TPanel;
    Image8: TImage;
    mvDateiformate: TLabel;
    rgDateiformate: TRadioGroup;
    Panel2: TPanel;
    Bevel2: TBevel;
    Label11: TLabel;
    Label1: TLabel;
    Image1: TImage;
    SrcLabel: TLabel;
    DstLabel: TLabel;
    IncludeBtn: TSpeedButton;
    IncAllBtn: TSpeedButton;
    ExcludeBtn: TSpeedButton;
    ExAllBtn: TSpeedButton;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    lbSrc: TListBox;
    lbDst: TListBox;
    Panel3: TPanel;
    Bevel3: TBevel;
    Image2: TImage;
    Label12: TLabel;
    Label13: TLabel;
    Panel4: TPanel;
    Bevel4: TBevel;
    Image3: TImage;
    Label3: TLabel;
    Label6: TLabel;
    GroupBox1: TGroupBox;
    Label4: TLabel;
    Label18: TLabel;
    Bevel8: TBevel;
    eTexterkennung: TEdit;
    cbFeldnamenInErsteZeile: TCheckBox;
    rgTrennzeichen: TRadioGroup;
    eTrennzeichen: TEdit;
    Label5: TLabel;
    LbSpeedButton1: TSpeedButton;
    eDateiname: TEdit;
    Panel6: TPanel;
    Bevel6: TBevel;
    Image5: TImage;
    Label15: TLabel;
    Label16: TLabel;
    lExportFormat: TLabel;
    Panel1: TPanel;
    LbGauge1: TGauge;
    Panel7: TPanel;
    Bevel7: TBevel;
    Image6: TImage;
    Label8: TLabel;
    Label17: TLabel;
    Label7: TLabel;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure btnFertigClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnWeiterClick(Sender: TObject);
    procedure btnZurueckClick(Sender: TObject);
    procedure lbDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure lbDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure IncludeBtnClick(Sender: TObject);
    procedure ExcludeBtnClick(Sender: TObject);
    procedure lbMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure IncAllBtnClick(Sender: TObject);
    procedure ExAllBtnClick(Sender: TObject);
    procedure rgDateiformateClick(Sender: TObject);
    procedure LbSpeedButton1Click(Sender: TObject);
    procedure UpDownCLick(Sender: TObject);
    procedure eDateinameKeyPress(Sender: TObject; var Key: Char);
  private
    FDateinameBasis: string;
    FTable1: TDataSet;
    FIsWorking: Boolean;
    function GetDateinameBasis: string;
    procedure IncludeExcludeButtonsRefresh;
  protected
    function DefaultExportDir: string;
  public
    property Table1: TDataSet read FTable1 write FTable1;
    property DateinameBasis: string read GetDateinameBasis
      write FDateinameBasis;
  end;

const
  RG_DATEIFORMAT_CSV = 0;
  RG_DATEIFORMAT_XML = 1;
  RG_DATEIFORMAT_HTML = 2;
  RG_DATEIFORMAT_DBASE = 3;
  RG_DATEIFORMAT_PARADOX = 4;
  RG_DATEIFORMAT_SQL = 5;

var
  DLG_Export: TDLG_Export;

implementation

{$R *.DFM}

uses
  hl.System.Types, hl.Utils, System.UITypes;

procedure TDLG_Export.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    Key := 0;
    Close;
  end;
end;

procedure TDLG_Export.FormShow(Sender: TObject);
var
  iCounter: Integer;

begin
  btnCancel.Enabled := true;
  btnZurueck.Enabled := false;
  btnWeiter.Enabled := true;
  btnFertig.Enabled := false;

  Notebook1.ActivePage := '0';
  FIsWorking := false;
  for iCounter := 0 to ComponentCount - 1 do
  begin
    if (Components[iCounter] is TImage) and
      (TImage(Components[iCounter]).Picture.Bitmap.Empty) then
    begin
      TImage(Components[iCounter]).Picture := Image1.Picture;
    end;
  end;

  rgDateiformateClick(rgDateiformate);
end;

function TDLG_Export.GetDateinameBasis: string;
begin
  if FDateinameBasis = '' then
    result := 'export'
  else
    result := FDateinameBasis;
end;

procedure TDLG_Export.btnCancelClick(Sender: TObject);
resourcestring
  SAreYouSureExportCancel =
    'Sind Sie sicher, dass Sie den Datenexport abbrechen möchten?';
begin
  if FIsWorking then
    if Application.MessageBox(PChar(SAreYouSureExportCancel),
      PChar(Application.Title), MB_ICONQUESTION + MB_YESNOCANCEL) <> IDYES then
      exit;
  FIsWorking := false;
  Close;
end;

procedure TDLG_Export.btnWeiterClick(Sender: TObject);
var
  lCounter: LongInt;
  supportedDataType: Boolean;
resourcestring
  SFieldTypeNotSupported =
    'Achtung: Feld %s hat nicht unterstützten Feldtyp %d!';
begin
  if Notebook1.ActivePage = '0' then
  begin
    Notebook1.ActivePage := '1';
    btnZurueck.Enabled := true;
  end
  else if Notebook1.ActivePage = '1' then
  begin
    Notebook1.ActivePage := '2';
    btnZurueck.Enabled := true;
    btnWeiter.Enabled := false;
    lbSrc.Items.Clear;
    lbDst.Items.Clear;

    for lCounter := 0 to FTable1.FieldDefs.Count - 1 do
    begin
      supportedDataType := false;
      case FTable1.FieldDefs.Items[lCounter].DataType of
        ftString, ftWidestring, ftFixedWideChar, ftFixedChar, ftSmallint,
          ftLargeInt, ftInteger, ftWord, ftBoolean, ftFloat, ftSingle,
          ftExtended, ftByte, ftLongWord, ftShortint, ftFMTBcd, ftCurrency,
          ftBCD, ftDate, ftTime, ftDateTime, ftGUID, ftMemo, ftWideMemo,
          ftAutoInc:
          begin
            lbSrc.Items.Add(FTable1.FieldDefs.Items[lCounter].Name);
            supportedDataType := true;
          end;
        (*
          ftAutoInc:
          begin
          if rgDateiformate.ItemIndex in [RG_DATEIFORMAT_HTML,RG_DATEIFORMAT_PARADOX,RG_DATEIFORMAT_CSV,RG_DATEIFORMAT_XML,RG_DATEIFORMAT_SQL] then
          begin
          lbSrc.Items.Add(FTable1.FieldDefs.Items[lCounter].Name);
          supportedDataType := true;
          end;
          end;
        *)
        ftBlob, ftGraphic:
          begin
            if rgDateiformate.ItemIndex = RG_DATEIFORMAT_PARADOX then
            begin
              lbSrc.Items.Add(FTable1.FieldDefs.Items[lCounter].Name);
              supportedDataType := true;
            end;
          end;
        (*
          ftWideMemo,
          ftMemo:
          begin
          if rgDateiformate.ItemIndex in [RG_DATEIFORMAT_DBASE,RG_DATEIFORMAT_PARADOX] then
          begin
          lbSrc.Items.Add(FTable1.FieldDefs.Items[lCounter].Name);
          supportedDataType := true;
          end;
          end
        *)
      end;

      if not supportedDataType then
      begin
        ShowMessageFmt(SFieldTypeNotSupported,
          [FTable1.FieldDefs.Items[lCounter].Name,
          Integer(FTable1.FieldDefs.Items[lCounter].DataType)]);
        lbSrc.Items.Add(FTable1.FieldDefs.Items[lCounter].Name);
        // trotzdem probieren
      end;
    end;
  end
  else if Notebook1.ActivePage = '2' then
  begin
    if rgDateiformate.ItemIndex = RG_DATEIFORMAT_CSV then
    begin
      Notebook1.ActivePage := '3';
    end
    else
    begin
      Notebook1.ActivePage := '5';
      btnWeiter.Enabled := false;
      btnFertig.Enabled := true;
      case rgDateiformate.ItemIndex of
        RG_DATEIFORMAT_DBASE:
          eDateiname.Text := IncludeTrailingPathDelimiter(DefaultExportDir) +
            DateinameBasis + '.dbf'; // do not localize
        RG_DATEIFORMAT_HTML:
          eDateiname.Text := IncludeTrailingPathDelimiter(DefaultExportDir) +
            DateinameBasis + '.htm'; // do not localize
        RG_DATEIFORMAT_PARADOX:
          eDateiname.Text := IncludeTrailingPathDelimiter(DefaultExportDir) +
            DateinameBasis + '.db'; // do not localize
        RG_DATEIFORMAT_XML:
          eDateiname.Text := IncludeTrailingPathDelimiter(DefaultExportDir) +
            DateinameBasis + '.xml'; // do not localize
        RG_DATEIFORMAT_SQL:
          eDateiname.Text := IncludeTrailingPathDelimiter(DefaultExportDir) +
            DateinameBasis + '.sql'; // do not localize
      end;
    end;
  end
  else if Notebook1.ActivePage = '3' then
  begin
    Notebook1.ActivePage := '5';
    btnWeiter.Enabled := false;
    btnFertig.Enabled := true;
    eDateiname.Text := IncludeTrailingPathDelimiter(DefaultExportDir) +
      DateinameBasis + '.csv'; // do not localize
  end;
  IncludeExcludeButtonsRefresh;
end;

procedure TDLG_Export.btnZurueckClick(Sender: TObject);
begin
  if Notebook1.ActivePage = '1' then
  begin
    Notebook1.ActivePage := '0';
    btnZurueck.Enabled := false;
  end
  else if Notebook1.ActivePage = '2' then
  begin
    Notebook1.ActivePage := '1';
    btnWeiter.Enabled := true;
  end
  else if Notebook1.ActivePage = '3' then
  begin
    Notebook1.ActivePage := '2';
    btnWeiter.Enabled := true;
    btnFertig.Enabled := false;
  end
  else if Notebook1.ActivePage = '5' then
  begin
    case rgDateiformate.ItemIndex of
      RG_DATEIFORMAT_DBASE, RG_DATEIFORMAT_HTML, RG_DATEIFORMAT_PARADOX,
        RG_DATEIFORMAT_SQL, RG_DATEIFORMAT_XML:
        Notebook1.ActivePage := '2';
      RG_DATEIFORMAT_CSV:
        Notebook1.ActivePage := '3';
    end;
    btnWeiter.Enabled := true;
    btnFertig.Enabled := false;
  end;
end;

function TDLG_Export.DefaultExportDir: string;
begin
  result := GetDesktopFolder;
end;

procedure TDLG_Export.eDateinameKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    Key := #0;
    btnFertig.Click;
  end;
end;

procedure TDLG_Export.lbDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := (Source is TListBox) and not(Source = Sender);
end;

procedure TDLG_Export.lbDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  lCounter: LongInt;

begin
  lCounter := 0;
  while lCounter < (Source as TListBox).Items.Count do
  begin
    if (Source as TListBox).Selected[lCounter] then
    begin
      (Sender as TListBox).Items.Add((Source as TListBox).Items[lCounter]);
      (Source as TListBox).Items.Delete(lCounter);
    end
    else
      inc(lCounter);
  end;
  IncludeExcludeButtonsRefresh;
  btnWeiter.Enabled := ExAllBtn.Enabled;
end;

procedure TDLG_Export.IncludeExcludeButtonsRefresh;
begin
  IncludeBtn.Enabled := (lbSrc.SelCount > 0);
  ExcludeBtn.Enabled := (lbDst.SelCount > 0);
  IncAllBtn.Enabled := (lbSrc.Items.Count > 0);
  ExAllBtn.Enabled := (lbDst.Items.Count > 0);
end;

procedure TDLG_Export.IncludeBtnClick(Sender: TObject);
begin
  lbDragDrop(lbDst, lbSrc, 0, 0);
end;

procedure TDLG_Export.ExcludeBtnClick(Sender: TObject);
begin
  lbDragDrop(lbSrc, lbDst, 0, 0);
end;

procedure TDLG_Export.lbMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  IncludeBtn.Enabled := (lbSrc.SelCount > 0);
  ExcludeBtn.Enabled := (lbDst.SelCount > 0);
end;

procedure TDLG_Export.IncAllBtnClick(Sender: TObject);
var
  iCounter: Integer;

begin
  for iCounter := 0 to lbSrc.Items.Count - 1 do
    lbSrc.Selected[iCounter] := true;
  lbDragDrop(lbDst, lbSrc, 0, 0);
end;

procedure TDLG_Export.ExAllBtnClick(Sender: TObject);
var
  iCounter: Integer;

begin
  for iCounter := 0 to lbDst.Items.Count - 1 do
    lbDst.Selected[iCounter] := true;
  lbDragDrop(lbSrc, lbDst, 0, 0);
end;

procedure TDLG_Export.UpDownCLick(Sender: TObject);
var
  iBuffer: Integer;
resourcestring
  SOnlyMoveOneElement =
    'Es kann immer nur ein einzelnes Element auf einmal bewegt werden!';
begin
  if lbDst.SelCount = 0 then
  begin
    // nix machen
  end
  else if lbDst.SelCount > 1 then
  begin
    Application.MessageBox(PChar(SOnlyMoveOneElement), PChar(Application.Title),
      MB_ICONEXCLAMATION + MB_OK)
  end
  else if lbDst.SelCount = 1 then
  begin
    iBuffer := lbDst.ItemIndex - (Sender as TSpeedButton).Tag;
    if iBuffer < 0 then
      exit
    else if iBuffer > lbDst.Items.Count - 1 then
      exit;

    lbDst.Items.Move(lbDst.ItemIndex, iBuffer);
    lbDst.ItemIndex := iBuffer;
    lbDst.Selected[iBuffer] := true;
  end;
end;

procedure TDLG_Export.rgDateiformateClick(Sender: TObject);
var
  labelText: string;
resourcestring
  SDBaseInfoText =
    'dBase ist das Format der Wahl, wenn es darauf ankommt, dass möglichst ' +
    'viele Programme die exportierten Daten lesen können.<BR><BR>' +
    'DBTool exportiert in das dBase für Windows-Format, das als eines ' +
    'der Standardformate für den Datenaustausch gilt. Dafür sind speziellere ' +
    'Feldtypen, z.B. Auto-Inkrement-Felder, nicht möglich, und ' +
    'werden beim Export nicht berücksichtigt.';
  SHtmlInfoText =
    'HTML ist ideal für den Export von Daten, wenn verschiedene Computersysteme '
    + 'darauf zugreifen sollen.<BR><BR>Das Format ist durch ' +
    'seine Plattformunabhängigkeit besonders für eine Veröffentlichung ' +
    'im Internet geeignet. Allerdings gibt es nicht viele Programme,' +
    'die HTML-Dateien als Datenquelle nutzen können.';
  SParadoxInfoText =
    'Paradox ist ein sehr komplexes Datenbankformat, das viele verschiedene ' +
    'Datentypen speichern kann. Es ist die erste Wahl, wenn möglichst ' +
    'alle Datenbankfelder exportiert werden sollen.<BR><BR>' +
    'DBTool exportiert in das Paradox5-Format, das von vielen Programmen ' +
    'eingelesen werden kann, und gleichzeitig eine große Menge an Feldtypen ' +
    'zur Verfügung stellt.';
  SCsvInfoText =
    'Durch Trennzeichen (Komma, Semikolon usw.) getrennter Text kann ' +
    'von sehr vielen Programmen eingelesen werden, und stellt durch seine ' +
    'Einfachheit eines der Standardformate für den Datenaustausch ' +
    'dar. Größere oder binäre Feldtypen, wie Memo- oder Grafikfelder,' +
    'können allerdings nicht exportiert werden.<BR><BR>TIPP:<BR>' +
    'Mit den Standardeinstellungen exportierte Daten können direkt ' +
    'in Microsoft Excel eingelesen werden!';
  SXmlInfoText =
    'XML ist der neue Standard für den Datenaustausch im Internet. Immer ' +
    'mehr Programme unterstützen dieses Format.<BR><BR>Allerdings ' +
    'können keine Grafiken oder Memos exportiert werden.';
  SSqldumpInfoText =
    'Mit einem SQL-Dump können Sie INSERT-Anweisungen erfolgen, ' +
    'um Daten per Abfrage zu übermitteln.<BR><BR>' +
    'Achtung: Dieses Feature ist experimentell und kann noch Fehler aufweisen.';
begin
  if rgDateiformate.ItemIndex = RG_DATEIFORMAT_DBASE then
    labelText := SDBaseInfoText;

  if rgDateiformate.ItemIndex = RG_DATEIFORMAT_HTML then
    labelText := SHtmlInfoText;

  if rgDateiformate.ItemIndex = RG_DATEIFORMAT_PARADOX then
    labelText := SParadoxInfoText;

  if rgDateiformate.ItemIndex = RG_DATEIFORMAT_CSV then
    labelText := SCsvInfoText;

  if rgDateiformate.ItemIndex = RG_DATEIFORMAT_XML then
    labelText := SXmlInfoText;

  if rgDateiformate.ItemIndex = RG_DATEIFORMAT_SQL then
    labelText := SSqldumpInfoText;

  labelText := StringReplace(labelText, '<BR>', #13#10,
    [rfReplaceAll, rfIgnoreCase]); // do not localize
  mvDateiformate.Caption := labelText;
end;

procedure TDLG_Export.LbSpeedButton1Click(Sender: TObject);
begin
  SaveDialog1.FileName := eDateiname.Text;
  if SaveDialog1.Execute then
  begin
    case rgDateiformate.ItemIndex of
      RG_DATEIFORMAT_DBASE:
        eDateiname.Text := changefileext(SaveDialog1.FileName, '.dbf');
        // do not localize
      RG_DATEIFORMAT_HTML:
        eDateiname.Text := changefileext(SaveDialog1.FileName, '.htm');
        // do not localize
      RG_DATEIFORMAT_PARADOX:
        eDateiname.Text := changefileext(SaveDialog1.FileName, '.db');
        // do not localize
      RG_DATEIFORMAT_CSV:
        eDateiname.Text := changefileext(SaveDialog1.FileName, '.csv');
        // do not localize
      RG_DATEIFORMAT_XML:
        eDateiname.Text := changefileext(SaveDialog1.FileName, '.xml');
        // do not localize
      RG_DATEIFORMAT_SQL:
        eDateiname.Text := changefileext(SaveDialog1.FileName, '.sql');
        // do not localize
    end;
  end;
end;

procedure TDLG_Export.btnFertigClick(Sender: TObject);
var
  iCounter: Integer;
  F: TextFile;
  sZeilenBuffer: string;
  sBuffer: string;
  sTrennzeichen: string;
  sTexterkennung: string;
  sTableName: string;
  felder: string;
  values: string;
  sFeldInhalt: string;
  isString: Boolean;
{$IFNDEF WIN64}
  ttNeu: TTable;
{$ENDIF}
  function ToHtml(sText: string): string;
  begin
    sText := StringReplace(sText, 'Ä', '&Auml;', [rfReplaceAll]);
    // do not localize
    sText := StringReplace(sText, 'Ö', '&Ouml;', [rfReplaceAll]);
    // do not localize
    sText := StringReplace(sText, 'Ü', '&Uuml;', [rfReplaceAll]);
    // do not localize
    sText := StringReplace(sText, 'ä', '&auml;', [rfReplaceAll]);
    // do not localize
    sText := StringReplace(sText, 'ö', '&ouml;', [rfReplaceAll]);
    // do not localize
    sText := StringReplace(sText, 'ü', '&uuml;', [rfReplaceAll]);
    // do not localize
    sText := StringReplace(sText, 'ß', '&szlig;', [rfReplaceAll]);
    // do not localize
    sText := StringReplace(sText, '<', '&lt;', [rfReplaceAll]);
    // do not localize
    sText := StringReplace(sText, '>', '&gt;', [rfReplaceAll]);
    // do not localize
    sText := StringReplace(sText, #13, '<br>' + #13, [rfReplaceAll]);
    // do not localize
    result := sText;
  end;

  function FieldStr(F: TField; hf: Boolean = false): string;
  begin
    if not hf and (F.DataType = ftBoolean) then
    begin
      if F.AsBoolean then
        result := '1'
      else
        result := '0';
    end
    else
      result := F.AsString;
  end;

resourcestring
  SFileAlreadyExistsOverwrite = 'Datei %s existiert bereits. Überschreiben?';
  SDbQuery = 'Datenbankabfrage';
  SExportDBaseRunning = 'Export ins dBase-Format läuft...';
  SExportHtmlRunning = 'Export ins HTML-Format läuft...';
  SExportParadoxRunning = 'Export ins Paradox-Format läuft...';
  SExportCsvRunning = 'Export ins CSV-Format läuft...';
  SExportXmlRunning = 'Export ins XML-Format läuft...';
  SExportSqlDumpRunning = 'SQL-Dump-Export läuft...';
  SBde64NotSupported = 'BDE wird von 64-Bit Build von DBTool nicht unterstützt';
  SDBToolHtmlComment = 'Created by HickelSOFT DBTool';
  SExportDone = 'Export abgeschlossen.';
begin
  if FileExists(eDateiname.Text) then
  begin
    if MessageDlg(Format(SFileAlreadyExistsOverwrite, [eDateiname.Text]),
      mtWarning, mbYesNoCancel, 0) <> mrYes then
      Abort;
  end;

{$IFNDEF WIN64}
  if FTable1 is TTable then
    sTableName := TTable(FTable1).TableName
  else
{$ENDIF}
    if FDateinameBasis <> '' then
      sTableName := FDateinameBasis
    else
      sTableName := SDbQuery;

  LbGauge1.Progress := 0;
  FIsWorking := true;
  Notebook1.ActivePage := '6';
  btnFertig.Enabled := false;
  btnWeiter.Enabled := false;
  btnZurueck.Enabled := false;
  case rgDateiformate.ItemIndex of
    RG_DATEIFORMAT_DBASE:
      lExportFormat.Caption := SExportDBaseRunning;
    RG_DATEIFORMAT_HTML:
      lExportFormat.Caption := SExportHtmlRunning;
    RG_DATEIFORMAT_PARADOX:
      lExportFormat.Caption := SExportParadoxRunning;
    RG_DATEIFORMAT_CSV:
      lExportFormat.Caption := SExportCsvRunning;
    RG_DATEIFORMAT_XML:
      lExportFormat.Caption := SExportXmlRunning;
    RG_DATEIFORMAT_SQL:
      lExportFormat.Caption := SExportSqlDumpRunning;
  end;

  if rgDateiformate.ItemIndex in [RG_DATEIFORMAT_DBASE, RG_DATEIFORMAT_PARADOX]
  then // dBase- oder Paradox-Export ----
  begin
{$IFNDEF WIN64}
    // Erst mal die neue(n) Datei(en) anlegen
    ttNeu := TTable.Create(Self);
    try
      FTable1.Active := true;
      ttNeu.Active := false;
      ttNeu.Databasename := extractfilepath(eDateiname.Text);
      ttNeu.TableName := extractfilename(eDateiname.Text);

      try
        ttNeu.DeleteTable;
      except
        on E: EAbort do
        begin
          Abort;
        end;
        on E: Exception do
        begin
          // ignore
        end;
      end;

      ttNeu.FieldDefs.Clear;
      for iCounter := 0 to lbDst.Items.Count - 1 do
      begin
        with ttNeu.FieldDefs.AddFieldDef do
        begin
          try
            FieldNo := ttNeu.FieldDefs.Count - 1;
            Name := lbDst.Items[iCounter];
            DataType := FTable1.FieldByName(lbDst.Items[iCounter]).DataType;
            if FTable1.FieldByName(lbDst.Items[iCounter]).DataType
              in [ftString, ftBCD, ftFMTBcd, ftBlob, ftMemo, ftGraphic] then
              Size := FTable1.FieldByName(lbDst.Items[iCounter]).DataSize - 1;
          except
            on E: EAbort do
            begin
              Abort;
            end;
            on E: Exception do
            begin
              // ignore
            end;
          end;
        end;
      end;

      case rgDateiformate.ItemIndex of
        RG_DATEIFORMAT_DBASE:
          begin
            ttNeu.TableType := ttDBase;
            ttNeu.tablelevel := 0;
          end;
        RG_DATEIFORMAT_PARADOX:
          begin
            ttNeu.TableType := ttParadox;
            ttNeu.tablelevel := 5;
          end;
      end;

      ttNeu.CreateTable;
      ttNeu.Active := true;

      // Jetzt die Daten exportieren
      FTable1.DisableControls;
      try
        FTable1.First;
        LbGauge1.MaxValue := FTable1.RecordCount;
        while (not FTable1.Eof) and FIsWorking do
        begin
          ttNeu.Insert;

          // Feldinhalte kopieren
          for iCounter := 0 to ttNeu.Fields.Count - 1 do
            ttNeu.Fields[iCounter].Value :=
              FTable1.FieldByName(ttNeu.Fields[iCounter].FieldName).Value;

          try
            ttNeu.Post;
          except
            on E: EAbort do
            begin
              Abort;
            end;
            on E: Exception do
            begin
              ttNeu.Cancel;
            end;
          end;

          FTable1.Next;
          LbGauge1.Progress := LbGauge1.Progress + 1;
          Application.ProcessMessages;
        end;
        FTable1.First; // TODO: Better navigate to last selected row
      finally
        FTable1.EnableControls;
      end;
      ttNeu.Active := false;
    finally
      FreeAndNil(ttNeu);
    end;
{$ELSE}
    raise Exception.Create(SBde64NotSupported);
{$ENDIF}
  end
  else if rgDateiformate.ItemIndex = RG_DATEIFORMAT_SQL then
  // SQL Dump ------------------------
  begin
    // Erst mal die neue Datei anlegen
    AssignFile(F, eDateiname.Text);
    Rewrite(F);

    // Feldnamen sammeln
    felder := '';
    for iCounter := 0 to lbDst.Items.Count - 1 do
    begin
      // TODO: escape
      felder := felder + lbDst.Items[iCounter] + ', ';
    end;
    felder := Copy(felder, 1, Length(felder) - 2);

    // Jetzt die Daten exportieren
    FTable1.DisableControls;
    try
      FTable1.First;
      LbGauge1.MaxValue := FTable1.RecordCount;
      while (not FTable1.Eof) and FIsWorking do
      begin
        // Feldinhalte kopieren
        values := '';
        for iCounter := 0 to lbDst.Items.Count - 1 do
        begin
          // TODO: je nach datentyp string-zeichen geben oder NULL geben
          case FTable1.FieldDefs.Items[iCounter].DataType of
            ftString, ftWidestring, ftFixedWideChar, ftFixedChar, ftGUID,
              ftMemo, ftWideMemo, ftDate, ftTime, ftDateTime:
              begin
                isString := true;
              end;

            ftSmallint, ftLargeInt, ftInteger, ftWord, ftFloat, ftSingle,
              ftExtended, ftByte, ftLongWord, ftShortint, ftFMTBcd, ftCurrency,
              ftBCD, ftAutoInc, ftBoolean:
              begin
                isString := false;
              end;

            ftBlob, ftGraphic:
              begin
                // TODO
              end;
          end;

          sFeldInhalt :=
            FieldStr(FTable1.FieldByName(lbDst.Items[iCounter]), false);
          if not FTable1.FieldDefs.Items[iCounter].Required and
            (sFeldInhalt = '') then
          begin
            sFeldInhalt := 'NULL'; // do not localize
          end
          else
          begin
            sFeldInhalt := hlString(sFeldInhalt).toSQLString;
          end;

          values := values + sFeldInhalt + ', ';
        end;
        values := Copy(values, 1, Length(values) - 2);

        // ...und rein in die Datei!
        // TODO: escape
        sZeilenBuffer := 'insert into ' + sTableName + ' (' + felder +
          ') values (' + values + ');'; // do not localize
        writeln(F, sZeilenBuffer);

        FTable1.Next;
        LbGauge1.Progress := LbGauge1.Progress + 1;
        Application.ProcessMessages;
      end;
      FTable1.First; // TODO: Better navigate to last selected row
    finally
      FTable1.EnableControls;
    end;

    CloseFile(F);
  end
  else if rgDateiformate.ItemIndex = RG_DATEIFORMAT_HTML then
  // HTML-Export ------------------------
  begin
    // Erst mal die neue Datei anlegen
    AssignFile(F, eDateiname.Text);
    Rewrite(F);
    writeln(F,
      '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">');
    // do not localize
    writeln(F, '<!-- ' + SDBToolHtmlComment + ' -->'); // do not localize
    writeln(F, '');
    writeln(F, '<HTML><HEAD><TITLE>' + ToHtml(sTableName) + '</TITLE></HEAD>');
    // do not localize
    writeln(F, '<BODY bgColor="#ffffff"><CENTER><H1>' + ToHtml(sTableName) +
      '</H1><BR><TABLE BORDER="1">'); // do not localize

    // Feldnamen in die erste Zeile
    sZeilenBuffer := '<TR>'; // do not localize
    for iCounter := 0 to lbDst.Items.Count - 1 do
    begin
      sZeilenBuffer := sZeilenBuffer + '<TD>' + ToHtml(lbDst.Items[iCounter]) +
        '</TD>'; // do not localize
    end;
    writeln(F, sZeilenBuffer + '</TR>'); // do not localize

    // Jetzt die Daten exportieren
    FTable1.DisableControls;
    try
      FTable1.First;
      LbGauge1.MaxValue := FTable1.RecordCount;
      while (not FTable1.Eof) and FIsWorking do
      begin
        // Feldinhalte kopieren
        sZeilenBuffer := '<TR>'; // do not localize
        for iCounter := 0 to lbDst.Items.Count - 1 do
        begin
          // TODO: escape
          sZeilenBuffer := sZeilenBuffer + '<TD>' +
            ToHtml(FieldStr(FTable1.FieldByName(lbDst.Items[iCounter]), true)) +
            '</TD>'; // do not localize
        end;
        sZeilenBuffer := sZeilenBuffer + '</TR>'; // do not localize

        // ...und rein in die Datei!
        writeln(F, sZeilenBuffer);
        FTable1.Next;
        LbGauge1.Progress := LbGauge1.Progress + 1;
        Application.ProcessMessages;
      end;
      FTable1.First; // TODO: Better navigate to last selected row
    finally
      FTable1.EnableControls;
    end;

    writeln(F, '</TABLE></CENTER></BODY></HTML>'); // do not localize
    CloseFile(F);
  end
  else if rgDateiformate.ItemIndex = RG_DATEIFORMAT_CSV then
  // Text-Export -----------------------
  begin
    // Erst mal die neue Datei anlegen
    AssignFile(F, eDateiname.Text);
    Rewrite(F);

    case rgTrennzeichen.ItemIndex of
      0:
        sTrennzeichen := #9;
      1:
        sTrennzeichen := ',';
      2:
        sTrennzeichen := ';';
      3:
        sTrennzeichen := #32;
      4:
        sTrennzeichen := '/';
      5:
        sTrennzeichen := eTrennzeichen.Text;
    end;
    sTexterkennung := eTexterkennung.Text;

    if cbFeldnamenInErsteZeile.Checked then
    begin
      sZeilenBuffer := lbDst.Items[0];
      for iCounter := 1 to lbDst.Items.Count - 1 do
        sZeilenBuffer := sZeilenBuffer + sTrennzeichen + lbDst.Items[iCounter];
      writeln(F, sZeilenBuffer);
    end;

    // Jetzt die Daten exportieren
    FTable1.DisableControls;
    try
      FTable1.First;
      LbGauge1.MaxValue := FTable1.RecordCount;
      while (not FTable1.Eof) and FIsWorking do
      begin
        // Feldinhalte kopieren
        sZeilenBuffer := '';
        for iCounter := 0 to lbDst.Items.Count - 1 do
        begin
          // TODO: escape!
          sBuffer := StringReplace
            (FieldStr(FTable1.FieldByName(lbDst.Items[iCounter]), false),
            sTexterkennung, sTexterkennung + sTexterkennung,
            [rfReplaceAll, rfIgnoreCase]);
          if (pos(sTrennzeichen, sBuffer) > 0) or
            (pos(sTexterkennung, sBuffer) > 0) then
            sBuffer := sTexterkennung + sBuffer + sTexterkennung;
          sBuffer := StringReplace(sBuffer, #13, ' ', [rfReplaceAll]);
          sBuffer := StringReplace(sBuffer, #10, '', [rfReplaceAll]);
          sZeilenBuffer := sZeilenBuffer + sBuffer + sTrennzeichen;
        end;
        sZeilenBuffer := Copy(sZeilenBuffer, 1, Length(sZeilenBuffer) -
          Length(sTrennzeichen));

        // ...und rein in die Datei!
        writeln(F, sZeilenBuffer);
        FTable1.Next;
        LbGauge1.Progress := LbGauge1.Progress + 1;
        Application.ProcessMessages;
      end;
      FTable1.First; // TODO: Better navigate to last selected row
    finally
      FTable1.EnableControls;
    end;

    CloseFile(F);
  end
  else if rgDateiformate.ItemIndex = RG_DATEIFORMAT_XML then
  // XML-Export ------------------------
  begin
    // Erst mal die neue Datei anlegen
    AssignFile(F, eDateiname.Text);
    Rewrite(F);
    writeln(F, '<?xml version="1.0" encoding="ISO-8859-1"?>');
    // do not localize
    writeln(F, '<!-- ' + SDBToolHtmlComment + ' -->'); // do not localize
    writeln(F, '');
    writeln(F, '<DATA>'); // TODO: Tablename? // do not localize

    // Daten exportieren
    FTable1.DisableControls;
    try
      FTable1.First;
      LbGauge1.MaxValue := FTable1.RecordCount;
      while (not FTable1.Eof) and FIsWorking do
      begin
        // Ein einzelner Record!
        writeln(F, '  <RECORD>'); // do not localize
        for iCounter := 0 to lbDst.Items.Count - 1 do
        begin
          // TODO: Escape
          writeln(F, '    <' + lbDst.Items[iCounter] + '>' +
            FieldStr(FTable1.FieldByName(lbDst.Items[iCounter]), false) + '</' +
            lbDst.Items[iCounter] + '>');
        end;
        writeln(F, '  </RECORD>'); // do not localize

        FTable1.Next;
        LbGauge1.Progress := LbGauge1.Progress + 1;
        Application.ProcessMessages;
      end;
      FTable1.First; // TODO: Better navigate to last selected row
    finally
      FTable1.EnableControls;
    end;

    writeln(F, '</DATA>'); // do not localize
    CloseFile(F);
  end;
  FIsWorking := false;
  Application.MessageBox(PChar(SExportDone), PChar(Application.Title),
    MB_ICONINFORMATION + MB_OK);
  if not(rgDateiformate.ItemIndex in [RG_DATEIFORMAT_DBASE,
    RG_DATEIFORMAT_PARADOX]) then
  begin
    ShellExecute64(Handle, nil, 'explorer.exe',
      PChar('/select,' + eDateiname.Text), nil, SW_SHOWNORMAL);
    // do not localize
  end;
  Close;
end;

end.
