unit Import;

{$INCLUDE 'Globals.inc'}

// TODO: Bei den Verknüpfungen (lbSrc, lbDst) wird beim hinzufügen und löschen
//       die Original-Reihenfolge nicht mehr beibehalten.

interface

uses
  Windows, SysUtils, Classes, Controls, StdCtrls, Forms, ExtCtrls, ComCtrls, Graphics,
  C_Database, Db, Dialogs, Buttons, Gauges;

type
  TDLG_Import = class(TForm)
    btnFertig: TButton;
    btnWeiter: TButton;
    btnZurueck: TButton;
    btnCancel: TButton;
    Bevel1: TBevel;
    Notebook1: TNotebook;
    Shape1: TShape;
    Label2: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Panel8: TPanel;
    Image8: TImage;
    Label19: TLabel;
    SpeedButton2: TSpeedButton;
    Label21: TLabel;
    Panel2: TPanel;
    Bevel2: TBevel;
    Label1: TLabel;
    Image1: TImage;
    Label11: TLabel;
    eQuelldatei: TEdit;
    Memo1: TMemo;
    Label18: TLabel;
    Panel4: TPanel;
    Bevel4: TBevel;
    Image2: TImage;
    Label3: TLabel;
    Label6: TLabel;
    GroupBox1: TGroupBox;
    Label4: TLabel;
    eTexterkennung: TEdit;
    cbFeldnamenInErsterZeile: TCheckBox;
    ListView1: TListView;
    GroupBox2: TGroupBox;
    rbTabStop: TRadioButton;
    rbSemikolon: TRadioButton;
    rbKomma: TRadioButton;
    rbLeerzeichen: TRadioButton;
    RadioButton5: TRadioButton;
    eTrennzeichen: TEdit;
    SrcLabel: TLabel;
    DstLabel: TLabel;
    Label22: TLabel;
    lbSrc: TListBox;
    lbDst: TListBox;
    Panel3: TPanel;
    Bevel3: TBevel;
    Image3: TImage;
    Label12: TLabel;
    Label13: TLabel;
    btnAdd: TButton;
    btnLoeschen: TButton;
    lbVerknuepfungen: TListBox;
    btnAlleLoeschen: TButton;
    lExportFormat: TLabel;
    Panel1: TPanel;
    LbGauge1: TGauge;
    Panel7: TPanel;
    Bevel7: TBevel;
    Image4: TImage;
    Label8: TLabel;
    Label17: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Panel5: TPanel;
    Bevel5: TBevel;
    Image5: TImage;
    Label7: TLabel;
    Label14: TLabel;
    Memo2: TMemo;
    OpenDialog1: TOpenDialog;
    Label5: TLabel;
    cbFormat: TComboBox;
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure SpeedButton2Click(Sender: TObject);
    procedure btnWeiterClick(Sender: TObject);
    procedure btnZurueckClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnFertigClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TrennzeichenOptionClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure ListBoxClick(Sender: TObject);
    procedure btnAlleLoeschenClick(Sender: TObject);
    procedure btnLoeschenClick(Sender: TObject);
    procedure lbVerknuepfungenClick(Sender: TObject);
    procedure cbFormatChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cbFeldnamenInErsterZeileClick(Sender: TObject);
  private
    FRunning: boolean;
    FTableName: string;
    FDatabase: TDbToolDatabase;
    FDataSet: TDataSet;
    FDatenGeaendert: boolean;
    procedure Csv2Sl(CSV: string; List: TStringList; Sep, Text: Char);
    procedure VorschauAktualisieren;
    procedure LoadFile(sFileName: string);
  protected
    FCSVLines: TStringList;
  public
    property DatenGeaendert: boolean read FDatenGeaendert;
    constructor Create(Owner: TComponent; aDB: TDbToolDatabase; aDS: TDataSet; sTable: String); reintroduce;
  end;

var
  DLG_Import: TDLG_Import;

implementation

{$R *.DFM}

uses
  Globals;

resourcestring
  SClose = 'Schließen';

constructor TDLG_Import.Create(Owner: TComponent; aDB: TDbToolDatabase; aDS: TDataSet; sTable: String);
begin
  inherited Create(Owner);

  FDatabase := aDB;
  FDataSet := aDS;
  FTableName := sTable;
  FRunning := false;
  Notebook1.PageIndex := 0;
end;

procedure TDLG_Import.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) and btnCancel.Enabled then
  begin
    Key := 0;
    btnCancel.Click;
  end;
end;

procedure TDLG_Import.SpeedButton2Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    LoadFile(OpenDialog1.FileName);
    eQuelldatei.Text := OpenDialog1.FileName;
  end;
end;

procedure TDLG_Import.cbFeldnamenInErsterZeileClick(Sender: TObject);
begin
  VorschauAktualisieren;
end;

procedure TDLG_Import.cbFormatChange(Sender: TObject);
begin
  LoadFile(eQuelldatei.Text);
end;

procedure TDLG_Import.LoadFile(sFileName: string);
var
  i: integer;
resourcestring
  SCannotOpenFile = 'Die Datei konnte nicht geöffnet werden. (%s)';
begin
   try
     // TODO: Aus irgendeinem Grund kann ich keine Dateien mit Sonderzeichen importieren
     //       => wenn datei aus david kommt (holz mail 09.12.2019), dann ist "ä" nicht im richtigen Unicode, sondern composed (zusammengesetzt aus a und den Punkten oben)
     FCSVLines.LoadFromFile(sFileName);
   except
     on E: EAbort do
     begin
       Abort;
     end;
     on E: Exception do
     begin
       raise Exception.CreateFmt(SCannotOpenFile, [E.Message]);
     end;
   end;

   // Achtung: Die Anzeige hat Limitierungen. So wird jede Zeile z.B. nach max 1024 Zeichen umgebrochen.
   // Das ist der Grund, wieso wir für die Datenverarbeitung eine reine StringList (FCSVLines) verwenden.
   Memo1.Lines.BeginUpdate;
   Memo1.Text := FCSVLines.Text;
   Memo1.Lines.EndUpdate;

   if cbFormat.ItemIndex = 1 then // OEM-Format? Datei konvertieren!
   begin
     for i := 0 to FCSVLines.Count-1 do
     begin
       FCSVLines.Strings[i] := OEM2Ansi(FCSVLines.Strings[i]);
     end;
   end;
end;

procedure TDLG_Import.Csv2Sl(CSV: String; List: TStringList; Sep, Text: char);
var
  bQuoting: boolean;
  cCurrent: char;
  sBuffer: string;
  iCounter: integer;
  iLength: integer;
begin
  List.Clear;
  CSV := CSV + Sep;
  bQuoting := false;
  iCounter := 1;
  iLength := Length(CSV);

   while(iCounter <= iLength) do
   begin
      cCurrent := CSV[iCounter];

      if cCurrent = Text then
      begin
         if (CSV[iCounter+1] = Text) and bQuoting then
         begin
            sBuffer := sBuffer + Text;
            Inc(iCounter);
         end
         else
           bQuoting := not bQuoting;
      end
      else
      begin
         if (cCurrent = Sep) and not bQuoting then
         begin
            List.Add(sBuffer);
            sBuffer := '';
         end
         else
           sBuffer := sBuffer + cCurrent;
      end;
      Inc(iCounter);
   end;
   if sBuffer <> '' then List.Add(sBuffer);   
end;

procedure TDLG_Import.VorschauAktualisieren;
var
  slBuffer: TStringList;
  anItem: TListItem;
  sLine: string;
  sFeldname: string;
  iLine: integer;
  cSep, cText: char;
  iMaxLines: integer;
  i: integer;
resourcestring
  SSourceFileEmpty = 'Die Quelldatei ist leer.';
  STextCharMissing = 'Sie müssen ein Texterkennungszeichen angeben!';
  SDelimitermissing = 'Sie müssen ein Trennzeichen angeben!';
  SFieldNr = 'Feld %d';
begin
   // Quelldatei überprüfen. Texterkennungs- und Trennzeichen ermitteln
   if(FCSVLines.Count < 1) then raise Exception.Create(SSourceFileEmpty);
   if Length(eTexterkennung.Text) = 1 then
     cText := eTexterkennung.Text[1]
   else
     raise Exception.Create(STextCharMissing);
   if(rbTabStop.Checked) then
     cSep := #9
   else if(rbSemikolon.Checked) then
     cSep := ';'
   else if(rbKomma.Checked) then
     cSep := ','
   else if(rbLeerzeichen.Checked) then
     cSep := ' '
   else if Length(eTrennzeichen.Text) = 1 then
     cSep := eTrennzeichen.Text[1]
   else raise Exception.Create(SDelimitermissing);

   slBuffer := TStringList.Create;
   try
     ListView1.Enabled := false;
      ListView1.Items.Clear;
      ListView1.Columns.Clear;
      lbSrc.Items.Clear;
      if cbFeldnamenInErsterZeile.Checked then iLine := 1 else iLine := 0;  // Erste Datenzeile

      // Feldnamen aus erster Zeile auslesen bzw. manuell erzeugen
      sLine := FCSVLines.Strings[0];
      Csv2Sl(sLine, slBuffer, cSep, cText);
      for i := 0 to slBuffer.Count-1 do
      begin
         if cbFeldnamenInErsterZeile.Checked then
           sFeldname := Trim(slBuffer.Strings[i])
         else
           sFeldname := format(SFieldNr, [i+1]);

         ListView1.Columns.Add.Caption := sFeldname;
         lbSrc.Items.Add(sFeldname);
      end;

      // Maximal die ersten 50 Datensätze einlesen
      iMaxLines := 50+iLine;
      if(FCSVLines.Count < 50) then iMaxLines := FCSVLines.Count;
      while iLine < iMaxLines do
      begin
         sLine := FCSVLines.Strings[iLine];
         Csv2Sl(sLine, slBuffer, cSep, cText);
         anItem := ListView1.Items.Add;
         anItem.Caption := slBuffer.Strings[0];
         for i := 1 to slBuffer.Count-1 do
         begin
           anItem.SubItems.Add(slBuffer.Strings[i]);
         end;
         Inc(iLine);
       end;
   finally
     FreeAndNil(slBuffer);
   end;

   // Jetzt noch die Spaltengrößen anpassen...
   for i := 0 to ListView1.Columns.Count-1 do
   begin
     ListView1.Columns.Items[i].Width := -1;
   end;
   ListView1.Enabled := true;
end;

procedure TDLG_Import.btnWeiterClick(Sender: TObject);
var
  slBuffer: TStringList;
  i: integer;
  j: Integer;
begin
   // WARUM darf ich hier kein switch benutzen?!? Grummel...
   if Notebook1.PageIndex = 0 then
   begin
      Notebook1.PageIndex := 1;
      btnZurueck.Enabled := true;
      exit;
   end;

   if Notebook1.PageIndex = 1 then
   begin
      // Datei einlesen
      LoadFile(eQuelldatei.Text);

      // Versuchen, das Trennzeichen zu erkennen: Mit den gebräuchlichsten
      // Trennzeichen durchprobieren, wenn's mehr als 1 Feld gibt . annehmen
      // Ansonsten das Komma als Standard belassen
      slBuffer := TStringList.Create;
      try
        // Komma?
        Csv2Sl(FCSVLines.Strings[0], slBuffer, ',', '"');
        if(slBuffer.Count <= 1) then
        begin
           // Semikolon?
           Csv2Sl(FCSVLines.Strings[0], slBuffer, ';', '"');
           if(slBuffer.Count > 1) then
             rbSemikolon.Checked := true
           else
           begin
              // Tabstop?
              Csv2Sl(FCSVLines.Strings[0], slBuffer, #9, '"');
              if(slBuffer.Count > 1) then
                rbTabStop.Checked := true
              else
              begin
                 // Leerzeichen?
                 Csv2Sl(FCSVLines.Strings[0], slBuffer, ' ', '"');
                 if(slBuffer.Count > 1) then rbLeerzeichen.Checked := true;
              end;
           end;
        end;
      finally
        FreeAndNil(slBuffer);
      end;

      // Vorschau anpassen und ab ins nächste Fenster!
      VorschauAktualisieren;
      Notebook1.PageIndex := 2;
      exit;
   end;
   if Notebook1.PageIndex = 2 then
   begin
     // Felder der Zieltabelle durchgehen und in die ListBoxen einfügen
     lbDst.Items.Clear;
     for i := 0 to FDataSet.Fields.Count-1 do
     begin
       lbDst.Items.Add(FDataSet.Fields.Fields[i].FieldName);
     end;

      lbVerknuepfungen.Items.Clear;

      // Versuchen, einfache Feldverknüpfungen selbst herzustellen
      // Dazu werden Felder mit gleichem Namen verknüpft
      if not cbFeldnamenInErsterZeile.Checked then
      begin
        if lbSrc.Count = lbDst.Count then
        begin
          for i := 0 to lbSrc.Count - 1 do
          begin
            lbSrc.ItemIndex := 0; // muss 0 sein, denn bei btnAddClick werden die Felder nach oben verschoben
            lbDst.ItemIndex := 0;
            btnAddClick(Sender);
          end;
        end;
      end
      else
      begin
        i := 0;
        while i < lbDst.Items.Count do
        begin
           for j := 0 to lbSrc.Items.Count-1 do
           begin
             if SameText(Trim(lbDst.Items.Strings[i]), Trim(lbSrc.Items.Strings[j])) then
             begin
              lbSrc.ItemIndex := j;
              lbDst.ItemIndex := i;
              btnAddClick(Sender);
              Dec(i);
              break;
             end;
           end;
           Inc(i);
        end;
      end;

      Notebook1.PageIndex := 3;
      btnWeiter.Enabled := false;
      btnFertig.Enabled := true;
      exit;
   end;
end;

procedure TDLG_Import.btnZurueckClick(Sender: TObject);
resourcestring
  SFinish = 'Fertigstellen';
begin
  btnFertig.Caption := SFinish;

   case Notebook1.PageIndex of
      1:
      begin
         Notebook1.PageIndex := 0;
         btnZurueck.Enabled := false;
      end;

      2:
      begin
         Notebook1.PageIndex := 1;
      end;

      3:
      begin
         Notebook1.PageIndex := 2;
         btnWeiter.Enabled := true;
         btnFertig.Enabled := false;
         btnAlleLoeschen.Click;
      end;

      5:
      begin
         Notebook1.PageIndex := 3;
         btnWeiter.Enabled := false;
         btnFertig.Enabled := true;
      end;
   end;
end;

procedure TDLG_Import.btnCancelClick(Sender: TObject);
resourcestring
  SAreYouSureCancelImport = 'Die Daten wurden noch nicht vollständig eingelesen. Sind Sie sicher, dass Sie den Import abbrechen wollen?';
begin
  if(FRunning) then
  begin
    if Application.MessageBox(PChar(SAreYouSureCancelImport), PChar(Application.Title), MB_ICONQUESTION + MB_YESNOCANCEL) <> IDYES then exit;
    FRunning := false;
    exit;
  end;

  Close;
end;

procedure TDLG_Import.FormShow(Sender: TObject);
begin
   // Assistenten-Symbol auf alle TImage-Komponenten verteilen
   Image2.Picture.Assign(Image1.Picture);
   Image3.Picture.Assign(Image1.Picture);
   Image4.Picture.Assign(Image1.Picture);
   Image5.Picture.Assign(Image1.Picture);
end;

procedure TDLG_Import.TrennzeichenOptionClick(Sender: TObject);
begin
  VorschauAktualisieren;
end;

procedure TDLG_Import.btnAddClick(Sender: TObject);
begin
  // Benutzer hat im Fenster "Feldzuordnung" auf den Hinzufügen-Button geklickt
  btnAdd.Enabled := false;
  // Vorsicht: Diese Zeichenfolge [Tab].[Tab] muss darf nicht verändert werden.
  // Wird sie verändert (z.B. den Punkt durch einen Pfeil ersetzen), dann muss
  // z.B. an 2 Stellen die Angabe "+3" zu "+4" gemacht werden.
  // (An diesen Stellen wird der Dest-Wert ausgelesen)
  lbVerknuepfungen.Items.Add(Trim(lbSrc.Items.Strings[lbSrc.ItemIndex]) + #9 + '.' + #9 + Trim(lbDst.Items.Strings[lbDst.ItemIndex]));
  lbSrc.Items.Delete(lbSrc.ItemIndex);
  lbDst.Items.Delete(lbDst.ItemIndex);
  btnAlleLoeschen.Enabled := lbVerknuepfungen.Count > 0;
end;

procedure TDLG_Import.ListBoxClick(Sender: TObject);
begin
  if(lbSrc.ItemIndex <> -1) and (lbDst.ItemIndex <> -1) then btnAdd.Enabled := true;
end;

procedure TDLG_Import.btnAlleLoeschenClick(Sender: TObject);
begin
  while lbVerknuepfungen.Items.Count > 0 do
  begin
    lbVerknuepfungen.ItemIndex := 0;
    btnLoeschenClick(Sender);
  end;
end;

procedure TDLG_Import.btnLoeschenClick(Sender: TObject);
var
  sBuffer: string;
begin
  btnLoeschen.Enabled := false;
  sBuffer := lbVerknuepfungen.Items.Strings[lbVerknuepfungen.ItemIndex];
  lbSrc.Items.Add(Copy(sBuffer, 1, Pos(#9, sBuffer)-1));
  lbDst.Items.Add(Copy(sBuffer, Pos(#9, sBuffer)+3));
  lbVerknuepfungen.Items.Delete(lbVerknuepfungen.ItemIndex);
  btnAlleLoeschen.Enabled := lbVerknuepfungen.Count > 0;
end;

procedure TDLG_Import.lbVerknuepfungenClick(Sender: TObject);
begin
   if (lbVerknuepfungen.ItemIndex <> -1) then btnLoeschen.Enabled := true;
end;

procedure TDLG_Import.btnFertigClick(Sender: TObject);
var
  sBuffer, sLine: String;
  cText, cSep: char;
  saFelder: array[0..255] of String;
  slBuffer: TStringList;
  sInsertClause: string;
  i: integer;
  iLine: Integer;
  aFieldType: TFieldType;
  aTable: TDataSet;
  cntDS, cntErr: integer;
resourcestring
  SNoFieldsAssociated = 'Keine Felder zugeordnet.';
  SImportStarted = 'Import beginnt';
  SEmptyLineSkipped = 'Leere Datenzeile wird übersprungen';
  SError = 'Fehler';
  SSQL = 'SQL';
  SImportFinishedDDatasetsDErrors = 'Import abgeschlossen (%d Datensätze erzeugt, %d Fehler)';
  SImportFinished = 'Der Import wurde abgebrochen.';
  FieldD = 'Feld %d';
begin
  if TButton(Sender).Caption = SClose then
  begin
    Close;
    exit;
  end;

  cntDS := 0;
  cntErr := 0;

  // Vorbereitungen...
  if lbVerknuepfungen.Items.Count = 0 then raise Exception.Create(SNoFieldsAssociated);
  btnZurueck.Enabled := false;
  btnFertig.Enabled := false;
  Notebook1.PageIndex := 4;
  LbGauge1.Progress := 0;
  LbGauge1.MaxValue := FCSVLines.Count-1;
  Memo2.Lines.Clear;
  Memo2.Lines.Add(FormatDateTime('dd.mm.yyyy hh:nn:ss', Now) + ' ' + SImportStarted); // do not localize
  // Memo2.Lines.Add('');
  cText := eTexterkennung.Text[1];
  if(rbTabStop.Checked) then
    cSep := #9
  else if(rbSemikolon.Checked) then
    cSep := ';'
  else if(rbKomma.Checked) then
    cSep := ','
  else if(rbLeerzeichen.Checked) then
    cSep := ' '
  else
    cSep := eTrennzeichen.Text[1];
  Application.ProcessMessages;

  aTable := FDatabase.GetTable(FTableName);
  slBuffer := TStringList.Create;
  try
    // Feldzuordnungen in ein int-Array einlesen
    // Index = Nummer des Quellfeldes, Inhalt = Nummer des Zielfeldes
    Csv2Sl(FCSVLines.Strings[0], slBuffer, cSep, cText);  // In slBuffer stehen jetzt die Feldnamen der Quelldatei in der Original-Reihenfolge
    for i := 0 to slBuffer.Count - 1 do
    begin
      if cbFeldnamenInErsterZeile.Checked then
        slBuffer.Strings[i] := Trim(slBuffer.Strings[i])
      else
        slBuffer.Strings[i] := Format(FieldD, [i+1]);
    end;
    for i := 0 to lbVerknuepfungen.Items.Count-1 do
    begin
      sBuffer := lbVerknuepfungen.Items.Strings[i];
      saFelder[slBuffer.IndexOf(Copy(sBuffer, 1, Pos(#9, sBuffer)-1))] := Copy(sBuffer, Pos(#9, sBuffer)+3);
    end;

    // Insert-Anweisung vorbereiten
    sInsertClause := 'INSERT INTO ' + FDatabase.SQL_Escape_TableName(FTableName) + '('; // do not localize
    for i := 0 to slBuffer.Count-1 do
    begin
      if saFelder[i] <> '' then sInsertClause := sInsertClause + FDatabase.SQL_Escape_FieldName(saFelder[i]) + ',';
    end;
    sInsertClause := Copy(sInsertClause, 1, Length(sInsertClause)-1) + ') VALUES ('; // do not localize

    // OK, dann importieren wir die Daten eben...
    FRunning := true;
    for iLine := 1 to FCSVLines.Count-1 do
    begin
      if not FRunning then break;
      sLine := FCSVLines.Strings[iLine];
      if Trim(sLine) = '' then
      begin
         Memo2.Lines.Add(FormatDateTime('dd.mm.yyyy hh:nn:ss', Now) + ' ' + SEmptyLineSkipped); // do not localize
      end
      else
      begin
        Csv2Sl(sLine, slBuffer, cSep, cText);

        sBuffer := sInsertClause;
        for i := 0 to slBuffer.Count-1 do
        begin
          if(saFelder[i] <> '') then
          begin
            aFieldType := aTable.FieldByName(saFelder[i]).DataType;

            if aFieldType in [ftSmallint, ftInteger, ftWord, ftAutoInc, ftLargeint] then
            begin
              if slBuffer.Strings[i] = '' then
                sBuffer := sBuffer + 'null,' // do not localize
              else
                sBuffer := sBuffer + slBuffer.Strings[i] + ',';
            end
            else if aFieldType in [ftBoolean] then
            begin
              if slBuffer.Strings[i] = '' then
                sBuffer := sBuffer + 'null,' // do not localize
              else if slBuffer.Strings[i] = '1' then
                sBuffer := sBuffer + '1,'
              else if slBuffer.Strings[i] = '0' then
                sBuffer := sBuffer + '0,'
              else if SameText(slBuffer.Strings[i],'wahr') then // do not localize
                sBuffer := sBuffer + '1,'
              else if SameText(slBuffer.Strings[i],'falsch') then // do not localize
                sBuffer := sBuffer + '0,'
              else if SameText(slBuffer.Strings[i],'true') then // do not localize
                sBuffer := sBuffer + '1,'
              else if SameText(slBuffer.Strings[i],'false') then // do not localize
                sBuffer := sBuffer + '0,'
              else if SameText(slBuffer.Strings[i],'yes') then // do not localize
                sBuffer := sBuffer + '1,'
              else if SameText(slBuffer.Strings[i],'no') then // do not localize
                sBuffer := sBuffer + '0,'
              else if SameText(slBuffer.Strings[i],'ja') then // do not localize
                sBuffer := sBuffer + '1,'
              else if SameText(slBuffer.Strings[i],'nein') then // do not localize
                sBuffer := sBuffer + '0,'
              else
                sBuffer := sBuffer + 'null,'; // TODO: sollte nicht passieren! // do not localize
            end
            else if aFieldType in [ftFloat, ftCurrency, ftBCD, ftFMTBcd] then
            begin
              if slBuffer.Strings[i] = '' then
                sBuffer := sBuffer + 'null,' // do not localize
              else
                sBuffer := sBuffer + StringReplace(slBuffer.Strings[i], ',', '.', []) + ',';
            end
            else if aFieldType in [ftString, ftMemo, ftFmtMemo, ftFixedChar, ftWideString, ftFixedWideChar, ftWideMemo] then
            begin
              sBuffer := sBuffer + '''' + StringReplace(slBuffer.Strings[i], '''', '''''', [rfReplaceAll]) + ''',';
            end
            else
            begin
              // TODO: Derzeit nicht behandelt:
              // ftUnknown, ftDate, ftTime, ftDateTime, ftBytes, ftVarBytes, ftBlob
              // ftGraphic, ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor,
              // ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob,
              // ftVariant, ftInterface, ftIDispatch, ftGuid, ftTimeStamp,
              // ftOraTimeStamp, ftOraInterval
              if slBuffer.Strings[i] = '' then
                sBuffer := sBuffer + 'null,' // do not localize
              else
                sBuffer := sBuffer + '''' + StringReplace(slBuffer.Strings[i], '''', '''''', [rfReplaceAll]) + ''',';
            end;
          end;
        end;

        try
          FDatenGeaendert := true;
          FDatabase.ExecSql(Copy(sBuffer, 1, Length(sBuffer)-1) + ');');
          Inc(cntDS);
        except
          on E: EAbort do
          begin
            Abort;
          end;
          on E: Exception do
          begin
            Inc(cntErr);
            Memo2.Lines.Add(FormatDateTime('dd.mm.yyyy hh:nn:ss', Now) + ' '+SError+': "' + E.Message + '", '+SSQL+': "' + Copy(sBuffer, 1, Length(sBuffer)-1) + ');"'); // do not localize
          end;
        end;

        // Alle 100 Zeilen den Status aktualisieren
        if (iLine mod 100) = 0 then
        begin
          LbGauge1.Progress := iLine;
          Application.ProcessMessages;
        end;
      end;
    end;
  finally
    FreeAndNil(slBuffer);
  end;

  // Fertig: Raus hier!
  // Memo2.Lines.Add('');
  Memo2.Lines.Add(FormatDateTime('dd.mm.yyyy hh:nn:ss', Now) + ' ' + Format(SImportFinishedDDatasetsDErrors, [cntDs, cntErr])); // do not localize
  btnCancel.Enabled := false;
  btnFertig.Enabled := true;
  TButton(Sender).Caption := SClose;
  Notebook1.PageIndex := 5;
  if not FRunning then Label7.Caption := SImportFinished;

  btnZurueck.Enabled := true; // Man will bei einem Fehler ggf. nochmal probieren.
end;

procedure TDLG_Import.FormCreate(Sender: TObject);
begin
  FCSVLines := TStringList.Create;
  FDatenGeaendert := false;
end;

procedure TDLG_Import.FormDestroy(Sender: TObject);
begin
  if Assigned(FCSVLines) then FreeAndNil(FCSVLines);
end;

end.
