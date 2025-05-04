unit hl.Datenbank.CSVExporter;

interface

uses
  DB, Classes, SysUtils, ComCtrls, hl.Utils.StreamEx;

type
  ThlCSVStringQuotes = (sqAlle, sqMitLeerzeichen, sqKeine);

type
  TQuoteEscapingRule = (qerDouble, qerDelete, qerFake);

type
  TVirtualFieldDef = record
    PhysicalName: string;
    TableName: string;

    DataType: TFieldType;
    Required: boolean;
    PrimaryKey: boolean;
    Size: integer;
    Precision: integer;

    VirtualName: string;
    DateTimeFormat: string;
  end;

  TVirtualFieldDefArray = array of TVirtualFieldDef;

type
  ThlCSVExporter = record
  strict private
    function EscapeAndQuote(const s: string; var doQuotes: boolean): string;
  public
    floatNachkommastellen: integer;
    stringQuotes: ThlCSVStringQuotes;
    QuoteEscapeRule: TQuoteEscapingRule;
    // autoCorrectQuoteProblems: boolean;   // z.B. wenn Anführungszeichen nicht übereinstimmen; TODO
    richTextUmwandeln: boolean;
    Trimmen: boolean;
    boolean10AnstelleJaNein: boolean;

    vfd: TVirtualFieldDefArray;
    useVFD: boolean;

    // Hinweis: excludeSL bezieht sich bei VFD immer auf den Virtuellen Namen, nicht auf den physikalischen

    // Diese Funktionen wären eigentlich protected, weil sie von GDPDUExport verwendet würden,
    // aber leider gibt es in D2007 keine abgeleiteten Records. Daher Public.
    procedure CSVKopfzeileEinfügen(ds: TDataSet; sl: TObject;
      excludeSL: TStrings = nil; renameSL: TStrings = nil);
    function JaNein(b: boolean): string;
    function Boolean10(b: boolean): string;
    function FeldAlsString(feld: TField; DateTimeFormat: string = '';
      nkStellen: integer = -1): string;
    procedure ExportDatensatz(ds: TDataSet; sl: TObject;
      excludeSL: TStrings = nil);

    /// <summary>Exportiert alle Einträge des Dataset in eine Stringlist im CSV-Format</summary>
    /// <param name="ds">Das Dataset mit den Daten</param>
    /// <param name="outSL">In diese Stringliste werden die Datensätze hinzugefügt.</param>
    /// <param name="pd">Ist entweder eine TGauge, TProgressBar oder ein TProgressDlg</param>
    /// <param name="mitKopfzeile">Wenn true, dann wird eine Kopfzeile mit den Spaltennamen hinzugefügt.</param>
    /// <returns>Anzahl der exportierten Datensätze</returns>
    function SimpleExport(ds: TDataSet; outSL: TObject; pd: TObject;
      mitKopfzeile: boolean = true): integer;

{$IF CompilerVersion > 20.0} // Version geraten
    class operator Initialize(out Dest: ThlCSVExporter);
    class operator Finalize(var Dest: ThlCSVExporter);
{$IFEND}
  end;

implementation

uses
  Math, Gauges, ProgrDlg, Forms, hl.Utils.StringStreamEx;

// TODO: CODE DUPLIKATE
// hl.Utils.pas (RichTextToPlainText)
// hl.Datenbank.CSVExporter.pas (RichTextToPlainText)
// hcl.Utils.Rtf.pas (ThclUtilsRtf.RtfToPlainText)
function RichTextToPlainText(richText: string): string;
var
  RichEdit1: TRichEdit;
begin
  if Copy(richText, 1, 5) <> '{\rtf' then
  begin
    result := richText;
    exit;
  end;
  RichEdit1 := TRichEdit.Create(Application.MainForm);
  try
    // Wenn Visible=true oder Parent=nil, dann geht es nicht...
    RichEdit1.Width := 0;
    RichEdit1.Height := 0;
    RichEdit1.Parent := Application.MainForm;
    RichEdit1.Text := richText;
    RichEdit1.PlainText := true;
    result := Trim(RichEdit1.Text);
  finally
    FreeAndNil(RichEdit1);
  end;
end;

function ThlCSVExporter.Boolean10(b: boolean): string;
begin
  if b then
    result := '1'
  else
    result := '0';
end;

procedure ThlCSVExporter.CSVKopfzeileEinfügen(ds: TDataSet; sl: TObject;
  excludeSL: TStrings = nil; renameSL: TStrings = nil);
var
  line: string;
  i: integer;
  colName: string;
begin
  line := '';

  if useVFD then
  begin
    for i := 0 to Length(vfd) - 1 do
    begin
      colName := vfd[i].VirtualName;
      if Assigned(renameSL) and (renameSL.IndexOfName(colName) <> -1) then
      begin
        colName := renameSL.Values[colName];
      end;
      if Assigned(excludeSL) and (excludeSL.IndexOf(colName) >= 0) then
        Continue;
      line := line + colName + ';';
    end;
  end
  else
  begin
    for i := 0 to ds.FieldDefs.Count - 1 do
    begin
      colName := ds.FieldDefs.Items[i].Name;
      if Assigned(renameSL) and (renameSL.IndexOfName(colName) <> -1) then
      begin
        colName := renameSL.Values[colName];
      end;
      if Assigned(excludeSL) and (excludeSL.IndexOf(colName) >= 0) then
        Continue;
      line := line + colName + ';';
    end;
  end;

  if line <> '' then
    line := Copy(line, 1, Length(line) - 1); // Letztes ';' entfernen

  if sl is TStrings then
    TStrings(sl).Add(line)
  else if sl is TStringStream then
    TStringStream(sl).Add(line)
  else if sl is TFileStream then
    TStream(sl).Add(line)
  else
    Assert(False);
end;

function ThlCSVExporter.JaNein(b: boolean): string;
begin
  if b then
    result := 'Ja'
  else
    result := 'Nein';
end;

function ThlCSVExporter.FeldAlsString(feld: TField; DateTimeFormat: string = '';
  nkStellen: integer = -1): string;
var
  doQuotes: boolean;
begin
  if feld.IsNull then
  begin
    // Audicon Fragenkatalog 1, Frage 1 bzgl. Datums-Werten: Feld muss bei NULL entweder leer sein oder mit Nullen aufgefüllt werden (00.00.0000)
    // Ich gehe davon aus, dass das bei Integer-Werten auch so gehandhabt werden soll.
    result := '';
    exit;
  end;

  case feld.DataType of
    ftFloat, ftBCD, ftCurrency, ftFMTBcd, ftExtended:
      begin
        if nkStellen = -1 then
          nkStellen := floatNachkommastellen;
        if nkStellen = -1 then
        begin
          // So viele Nachkommastellen wie die Zahl hat
          result := FloatToStr(feld.AsFloat);
        end
        else
        begin
          // Feste Anzahl an Nachkommastellen, gemäß Beschreibungsstandard
          result := format('%.*f', [nkStellen, feld.AsFloat])
        end;
      end;

    ftBoolean:
      begin
        if boolean10AnstelleJaNein then
        begin
          result := Boolean10(feld.AsBoolean);
        end
        else
        begin
          result := JaNein(feld.AsBoolean);
        end;
        if stringQuotes = sqAlle then
        begin
          result := '"' + result + '"';
        end;
      end;

    ftDateTime, ftDate, ftTime:
      begin
        if DateTimeFormat = '' then
        begin
          result := feld.AsString;
        end
        else
        begin
          result := FormatDateTime(DateTimeFormat, feld.AsDateTime);
        end;
      end;

    ftString, ftWideString, ftFixedWideChar, ftMemo, ftWideMemo:
      begin
        result := feld.AsString;
        if richTextUmwandeln then
        begin
          result := RichTextToPlainText(result);
        end;
        if Trimmen then
        begin
          result := Trim(result);
        end;
        case stringQuotes of
          sqAlle:
            doQuotes := true;
          sqMitLeerzeichen:
            doQuotes := Pos(' ', result) > 0;
          sqKeine:
            doQuotes := False;
        else
          doQuotes := False; // dieser Fall sollte nicht eintreten
        end;

        result := EscapeAndQuote(result, doQuotes);
      end
  else
    begin
      result := feld.AsString;
    end;
  end;
end;

{$IF CompilerVersion > 20.0}
// Version geraten
class operator ThlCSVExporter.Finalize(var Dest: ThlCSVExporter);
begin

end;
{$IFEND}
{$IF CompilerVersion > 20.0}
// Version geraten
class operator ThlCSVExporter.Initialize(out Dest: ThlCSVExporter);
begin
  Dest.floatNachkommastellen := -1;
  Dest.stringQuotes := sqAlle;
  Dest.QuoteEscapeRule := qerDouble;
  // Dest.autoCorrectQuoteProblems := false;
  Dest.richTextUmwandeln := False;
  Dest.boolean10AnstelleJaNein := False;
  Dest.Trimmen := False;
  SetLength(Dest.vfd, 0);
  Dest.useVFD := False;
end;
{$IFEND}

function ThlCSVExporter.EscapeAndQuote(const s: string;
  var doQuotes: boolean): string;
begin
  if doQuotes then
  begin
    case QuoteEscapeRule of
      qerDouble:
        begin
          // Gemäß RFC 4180 wird mit einem verdoppelten Zeichen escaped (so wie bei Delphi)
          result := StringReplace(s, '"', '""', [rfReplaceAll]);
        end;

      qerDelete:
        begin
          result := StringReplace(s, '"', '', [rfReplaceAll]);
        end;

      qerFake:
        begin
          result := StringReplace(s, '"', '''''', [rfReplaceAll]);
        end;
    end;
  end
  else
  begin
    if (Pos(';', s) > 0) or (Pos(' ', s) > 0) or (Pos('"', s) > 0) then
    begin
      // Es geht nicht anders... Wir müssen Quotes verwenden
      doQuotes := true;
      result := EscapeAndQuote(s, doQuotes);
      exit;
    end;
  end;
  if doQuotes then
    result := '"' + s + '"'
  else
    result := s;
end;

procedure ThlCSVExporter.ExportDatensatz(ds: TDataSet; sl: TObject;
  excludeSL: TStrings = nil);
var
  line: string;
  i: integer;
  colName: string;

  procedure ProcessVal(value: string);
  begin
    value := Trim(value);

    // RechnungenBelegKopf2015 hat ein Feld, das mehrere Zeilen lang ist, und macht damit die CSV kaputt
    value := StringReplace(value, #10, '', [rfReplaceAll]);
    // Achtung: Statt #13#10 gibt es in der Datenbank an einigen Stellen auch #10#13 (ungültig)!
    value := StringReplace(value, #13, '  [Zeilenumbruch]  ', [rfReplaceAll]);

    line := line + value + ';';
  end;

begin
  line := '';

  if useVFD then
  begin
    for i := 0 to Length(vfd) - 1 do
    begin
      colName := vfd[i].VirtualName;
      if Assigned(excludeSL) and (excludeSL.IndexOf(colName) >= 0) then
        Continue;
      ProcessVal(FeldAlsString(ds.FieldByName(vfd[i].PhysicalName),
        vfd[i].DateTimeFormat, vfd[i].Size));
    end;
  end
  else
  begin
    for i := 0 to ds.FieldCount - 1 do
    begin
      colName := ds.FieldDefs.Items[i].Name;
      if Assigned(excludeSL) and (excludeSL.IndexOf(colName) >= 0) then
        Continue;
      ProcessVal(FeldAlsString(ds.Fields[i], '', floatNachkommastellen));
    end;
  end;

  if line <> '' then
    line := Copy(line, 1, Length(line) - 1); // Letztes ';' entfernen

  if sl is TStrings then
    TStrings(sl).Add(line)
  else if sl is TStringStream then
    TStringStream(sl).Add(line)
  else if sl is TFileStream then
    TStream(sl).Add(line)
  else
    Assert(False);
end;

function ThlCSVExporter.SimpleExport(ds: TDataSet; outSL: TObject; pd: TObject;
  mitKopfzeile: boolean = true): integer;
var
  excludeKopfZeilen: TStringList;
begin
  result := 0; // exported lines

  excludeKopfZeilen := TStringList.Create;
  try
    // Kopfzeile
    if mitKopfzeile then
    begin
      CSVKopfzeileEinfügen(ds, outSL, excludeKopfZeilen);
    end;

    // Datenzeilen
    if ds.RecordCount = 0 then
    begin
      if Assigned(pd) then
      begin
        if pd is TGauge then
        begin
          TGauge(pd).MinValue := 0;
          TGauge(pd).MaxValue := 1;
          TGauge(pd).Progress := TGauge(pd).MaxValue;
        end
        else if pd is TProgressBar then
        begin
          TProgressBar(pd).Min := 0;
          TProgressBar(pd).Max := 1;
          TProgressBar(pd).Position := TProgressBar(pd).Max;
        end
        else if pd is TProgressDlg then
        begin
          TProgressDlg(pd).MaxValue := 1;
          TProgressDlg(pd).Position := TProgressDlg(pd).MaxValue;
        end;
      end;
      exit;
    end;

    if Assigned(pd) then
    begin
      if pd is TGauge then
      begin
        TGauge(pd).MinValue := 0;
        TGauge(pd).MaxValue := ds.RecordCount - 1;
      end
      else if pd is TProgressBar then
      begin
        TProgressBar(pd).Min := 0;
        TProgressBar(pd).Max := ds.RecordCount - 1;
      end
      else if pd is TProgressDlg then
      begin
        TProgressDlg(pd).MaxValue := ds.RecordCount - 1;
      end;
    end;

    ds.First;
    while not ds.Eof do
    begin
      if Assigned(pd) then
      begin
        if pd is TGauge then
        begin
          TGauge(pd).Progress := Min(TGauge(pd).MaxValue,
            TGauge(pd).Progress + 1);
        end
        else if pd is TProgressBar then
        begin
          TProgressBar(pd).Position := Min(TProgressBar(pd).Max,
            TProgressBar(pd).Position + 1);
        end
        else if pd is TProgressDlg then
        begin
          TProgressDlg(pd).IncPos;
          if TProgressDlg(pd).StopButtonSignal then
            Abort;
        end;
      end;

      ExportDatensatz(ds, outSL, excludeKopfZeilen);
      Inc(result);
      ds.Next;
    end;
  finally
    FreeAndNil(excludeKopfZeilen);
  end;
end;

end.
