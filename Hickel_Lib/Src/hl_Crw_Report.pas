unit hl_Crw_Report;

interface

uses Windows, Controls, MessaBox, Printers,
     hl_Report, Classes, Drucker, hl_IPC_Crw_LowLevel_Client,
     hl_IPC_LowLevel_Common;

type
  TDateiNachBearbeitungProcedure = function(const ADateiname: string): string of object;

  ThlCrwReport = class(ThlReport)
  private
    crpe2: TCrwLowLevelClient;

    FParamPreislistenr: integer;
    FParamPreislistename: string;
    FParamPreisAbfragedatum: string;
    FParamFilialNr: integer;

    FKeinProgressDialog: boolean;

    procedure SetReport(KopieNummer: integer);

    procedure SetzeSeitenraender(FPrinterData: TPrinterData);
    procedure SetzeCRWExportOptionen(zielDateiname: string);
    procedure SetzeFelder(FPrinterData: TPrinterData; KopieNummer: integer);
    procedure DoExecute;
    procedure SetzeBildschirmDruckFenstereinstellungen;
    procedure AuswahlFormelFestlegen;
    function IstBonFormular: boolean;

  protected
    procedure ErzeugeReportMitPassendemConnectionString; virtual;
  public
    DateiNachBearbeitung: TDateiNachBearbeitungProcedure;

    constructor Create(ARequireCrpe: boolean=true);
    destructor Destroy; override;

    property ParamPreislistenr: integer read FParamPreislistenr write FParamPreislistenr;
    property ParamPreislistename: string read FParamPreislistename write FParamPreislistename;
    property ParamPreisAbfragedatum: string read FParamPreisAbfragedatum write FParamPreisAbfragedatum;
    property ParamFilialNr: integer read FParamFilialNr write FParamFilialNr;

    property KeinProgressDialog: boolean read FKeinProgressDialog write FKeinProgressDialog;

    // TODO: Es handelt sich hierbei um unabhängige Implementierungen. Bitte vergleichen und ggf. Gemeinsamkeiten auslagern in Methoden.
    procedure Druck(APrinterName: string; AAnzahl: integer; ADuplexNr, ASchachtNr: integer; KopieNummer: integer=0); overload; override;
    procedure Druck(APrinterData, APrinterDataKopien: TPrinterData; ModalBildschirmDruck: boolean); overload; override;

    procedure Anzeige(Modal: boolean); override;
    procedure DateiExport(AExportDateiname: string); override;
    procedure SendEMail(ExportFilename: string=''; betreff: string=''; text: string=''; toEMail: string=''; toEMailCC: string=''; toEMailBCC: string=''); override;

    class function CRWExportDateifilter: string;

    function OptimiereReport: boolean; virtual;

    procedure ll(a1: string; a2: string=''; a3: string=''; a4: string=''; a5: string=''; a6: string='');
    procedure llnw(a1: string; a2: string=''; a3: string=''; a4: string=''; a5: string=''; a6: string=''); // "No wait"
    function lls(a1: string; a2: string=''; a3: string=''; a4: string=''; a5: string=''; a6: string=''): string;
    function lli(a1: string; a2: string=''; a3: string=''; a4: string=''; a5: string=''; a6: string=''): integer;
    function llb(a1: string; a2: string=''; a3: string=''; a4: string=''; a5: string=''; a6: string=''): boolean;
  end;

implementation

uses hl_Exceptions, SysUtils, forms, StrUtils, hl.System.ExceptionHandler, HsSqlServerProvider,
     hl.Utils, hl_Datenbank, Registry, hl.Utils.Mapi, HS_Auth;

constructor ThlCrwReport.Create(ARequireCrpe: boolean=true);
begin
  inherited Create;

  RandOben := -1;
  RandUnten := -1;
  ParamPreislistenr := -1;

  if ARequireCrpe then
  begin
    crpe2 := TCrwLowLevelClient.Create;
    ll('crpe.Create()');
  end
  else
  begin
    crpe2 := nil; // e.g. if you just want to run OptimiereReport (CRW13) alone
  end;
end;

destructor ThlCrwReport.Destroy;
begin
  if Assigned(crpe2) then
  begin
    try
      ll('crpe.FreeAndNil()');
    except
      on E: EAbort do
      begin
        Abort;
      end;
    end;
    try
      ll('Exit');
    except
      on E: EAbort do
      begin
        Abort;
      end;
    end;
    FreeAndNil(crpe2);
  end;

  inherited Destroy;
end;

procedure ThlCrwReport.SetzeFelder(FPrinterData: TPrinterData; KopieNummer: integer);
var
  i: integer;
  outPfValue: string;
  formulaName: string;
begin
  for i := 0 to lli('crpe.Formulas.Count:Get') - 1 do
  begin
    ll('crpe.Formulas.Number:Set', IntToStr(i)); // einstellen der entsprechenden Formel

    formulaName := lls('crpe.Formulas.Name:Get');

    if Assigned(pfFeldCallback) then
    begin
      if pfFeldCallback(formulaName, outPfValue) then
      begin
        ll('crpe.Formulas.Item.Formula.Clear()');
        ll('crpe.Formulas.Item.Formula.Add()', outPfValue);
        continue;
      end;
    end;

    // einstellen der entsprechenden Formel
    if SameText(formulaName, 'pfKopie') then
    begin
      // Die Formel pfKopie wird derzeit nur von HickelSOFT + Beate Huth verwendet
      ll('crpe.Formulas.Item.Formula.Clear()');
      ll('crpe.Formulas.Item.Formula.Add()', IntToStr(KopieNummer));
    end
    else if SameText(formulaName, 'pfKopfdrucken') then
    begin
      ll('crpe.Formulas.Item.Formula.Clear()');
      if FPrinterData <> nil then
      begin
        if FPrinterData.DruckenKopf then
          ll('crpe.Formulas.Item.Formula.Add()', 'False')
        else
          ll('crpe.Formulas.Item.Formula.Add()', 'True');
      end
      else
      begin
        if KopfDrucken then
          ll('crpe.Formulas.Item.Formula.Add()', 'False')
        else
          ll('crpe.Formulas.Item.Formula.Add()', 'True');
      end;
    end
    else if SameText(formulaName, 'pfFussdrucken') then
    begin
      ll('crpe.Formulas.Item.Formula.Clear()');
      if FPrinterData <> nil then
      begin
        if FPrinterData.DruckenFuss then
          ll('crpe.Formulas.Item.Formula.Add()', 'False')
        else
          ll('crpe.Formulas.Item.Formula.Add()', 'True');
      end
      else
      begin
        if FussDrucken then
          ll('crpe.Formulas.Item.Formula.Add()', 'False')
        else
          ll('crpe.Formulas.Item.Formula.Add()', 'True');
      end;
    end
    else if SameText(formulaName, 'pfAdressueberschrift') then
    begin
      ll('crpe.Formulas.Item.Formula.Clear()');
      if FPrinterData <> nil then
      begin
        if FPrinterData.DruckenAdressueberschrift then
          ll('crpe.Formulas.Item.Formula.Add()', 'False')
        else
          ll('crpe.Formulas.Item.Formula.Add()', 'True');
      end
      else
      begin
        if AdressUeberschriftDrucken then
          ll('crpe.Formulas.Item.Formula.Add()', 'False')
        else
          ll('crpe.Formulas.Item.Formula.Add()', 'True');
      end;
    end
    else if SameText(formulaName, 'pfKopf2anders') then
    begin
      ll('crpe.Formulas.Item.Formula.Clear()');
      if FPrinterData <> nil then
      begin
        if FPrinterData.Kopfseite2Anders then
          ll('crpe.Formulas.Item.Formula.Add()', 'False')
        else
          ll('crpe.Formulas.Item.Formula.Add()', 'True');
      end
      else
      begin
        if KopfSeite2Anders then
          ll('crpe.Formulas.Item.Formula.Add()', 'False')
        else
          ll('crpe.Formulas.Item.Formula.Add()', 'True');
      end;
    end
    else if SameText(formulaName, 'pfVon1') then
    begin
      ll('crpe.Formulas.Item.Formula.Clear()');
      ll('crpe.Formulas.Item.Formula.Add()', ParamVon1);
    end
    else if SameText(formulaName, 'pfVon2') then
    begin
      ll('crpe.Formulas.Item.Formula.Clear()');
      ll('crpe.Formulas.Item.Formula.Add()', ParamVon2);
    end
    else if SameText(formulaName, 'pfVon3') then
    begin
      ll('crpe.Formulas.Item.Formula.Clear()');
      ll('crpe.Formulas.Item.Formula.Add()', ParamVon3);
    end
    else if SameText(formulaName, 'pfVon4') then
    begin
      ll('crpe.Formulas.Item.Formula.Clear()');
      ll('crpe.Formulas.Item.Formula.Add()', ParamVon4);
    end
    else if SameText(formulaName, 'pfVon5') then
    begin
      ll('crpe.Formulas.Item.Formula.Clear()');
      ll('crpe.Formulas.Item.Formula.Add()', ParamVon5);
    end
    else if SameText(formulaName, 'pfVon6') then
    begin
      ll('crpe.Formulas.Item.Formula.Clear()');
      ll('crpe.Formulas.Item.Formula.Add()', ParamVon6);
    end
    else if SameText(formulaName, 'pfVon7') then
    begin
      ll('crpe.Formulas.Item.Formula.Clear()');
      ll('crpe.Formulas.Item.Formula.Add()', ParamVon7);
    end
    else if SameText(formulaName, 'pfVon8') then
    begin
      ll('crpe.Formulas.Item.Formula.Clear()');
      ll('crpe.Formulas.Item.Formula.Add()', ParamVon8);
    end
    else if SameText(formulaName, 'pfVon9') then
    begin
      ll('crpe.Formulas.Item.Formula.Clear()');
      ll('crpe.Formulas.Item.Formula.Add()', ParamVon9);
    end
    else if SameText(formulaName, 'pfVon10') then
    begin
      ll('crpe.Formulas.Item.Formula.Clear()');
      ll('crpe.Formulas.Item.Formula.Add()', ParamVon10);
    end
    else if SameText(formulaName, 'pfVon11') then
    begin
      ll('crpe.Formulas.Item.Formula.Clear()');
      ll('crpe.Formulas.Item.Formula.Add()', ParamVon11);
    end
    else if SameText(formulaName, 'pfVon12') then
    begin
      ll('crpe.Formulas.Item.Formula.Clear()');
      ll('crpe.Formulas.Item.Formula.Add()', ParamVon12);
    end
    else if SameText(formulaName, 'pfVon13') then
    begin
      ll('crpe.Formulas.Item.Formula.Clear()');
      ll('crpe.Formulas.Item.Formula.Add()', ParamVon13);
    end
    else if SameText(formulaName, 'pfVon14') then
    begin
      ll('crpe.Formulas.Item.Formula.Clear()');
      ll('crpe.Formulas.Item.Formula.Add()', ParamVon14);
    end
    else if SameText(formulaName, 'pfVon15') then
    begin
      ll('crpe.Formulas.Item.Formula.Clear()');
      ll('crpe.Formulas.Item.Formula.Add()', ParamVon15);
    end
    else if SameText(formulaName, 'pfBis1') then
    begin
      ll('crpe.Formulas.Item.Formula.Clear()');
      ll('crpe.Formulas.Item.Formula.Add()', ParamBis1);
    end
    else if SameText(formulaName, 'pfBis2') then
    begin
      ll('crpe.Formulas.Item.Formula.Clear()');
      ll('crpe.Formulas.Item.Formula.Add()', ParamBis2);
    end
    else if SameText(formulaName, 'pfBis3') then
    begin
      ll('crpe.Formulas.Item.Formula.Clear()');
      ll('crpe.Formulas.Item.Formula.Add()', ParamBis3);
    end
    else if SameText(formulaName, 'pfBis4') then
    begin
      ll('crpe.Formulas.Item.Formula.Clear()');
      ll('crpe.Formulas.Item.Formula.Add()', ParamBis4);
    end
    else if SameText(formulaName, 'pfBis5') then
    begin
      ll('crpe.Formulas.Item.Formula.Clear()');
      ll('crpe.Formulas.Item.Formula.Add()', ParamBis5);
    end
    else if SameText(formulaName, 'pfBis6') then
    begin
      ll('crpe.Formulas.Item.Formula.Clear()');
      ll('crpe.Formulas.Item.Formula.Add()', ParamBis6);
    end
    else if SameText(formulaName, 'pfBis7') then
    begin
      ll('crpe.Formulas.Item.Formula.Clear()');
      ll('crpe.Formulas.Item.Formula.Add()', ParamBis7);
    end
    else if SameText(formulaName, 'pfBis8') then
    begin
      ll('crpe.Formulas.Item.Formula.Clear()');
      ll('crpe.Formulas.Item.Formula.Add()', ParamBis8);
    end
    else if SameText(formulaName, 'pfBis9') then
    begin
      ll('crpe.Formulas.Item.Formula.Clear()');
      ll('crpe.Formulas.Item.Formula.Add()', ParamBis9);
    end
    else if SameText(formulaName, 'pfBis10') then
    begin
      ll('crpe.Formulas.Item.Formula.Clear()');
      ll('crpe.Formulas.Item.Formula.Add()', ParamBis10);
    end
    else if SameText(formulaName, 'pfBis11') then
    begin
      ll('crpe.Formulas.Item.Formula.Clear()');
      ll('crpe.Formulas.Item.Formula.Add()', ParamBis11);
    end
    else if SameText(formulaName, 'pfBis12') then
    begin
      ll('crpe.Formulas.Item.Formula.Clear()');
      ll('crpe.Formulas.Item.Formula.Add()', ParamBis12);
    end
    else if SameText(formulaName, 'pfBis13') then
    begin
      ll('crpe.Formulas.Item.Formula.Clear()');
      ll('crpe.Formulas.Item.Formula.Add()', ParamBis13);
    end
    else if SameText(formulaName, 'pfBis14') then
    begin
      ll('crpe.Formulas.Item.Formula.Clear()');
      ll('crpe.Formulas.Item.Formula.Add()', ParamBis14);
    end
    else if SameText(formulaName, 'pfBis15') then
    begin
      ll('crpe.Formulas.Item.Formula.Clear()');
      ll('crpe.Formulas.Item.Formula.Add()', ParamBis15);
    end
    else if SameText(formulaName, 'pfPreisabfragedatum') then
    begin
      ll('crpe.Formulas.Item.Formula.Clear()');
      ll('crpe.Formulas.Item.Formula.Add()', ParamPreisabfrageDatum);
    end
    else if SameText(formulaName, 'pfPreislistenr') then
    begin
      // Bei einigen Formularen wird pfPreislistenr und pfPreislistename
      // als Konstante verwendet. Wir dürfen daher die Formel nicht leeren! (Ticket 54508)
      //ll('crpe.Formulas.Item.Formula.Clear()');
      if ParamPreislistenr > 0 then
      begin
        ll('crpe.Formulas.Item.Formula.Clear()');
        ll('crpe.Formulas.Item.Formula.Add()', IntToStr(ParamPreislistenr));
      end;
    end
    else if SameText(formulaName, 'pfPreislistename') then
    begin
      // Bei einigen Formularen wird pfPreislistenr und pfPreislistename
      // als Konstante verwendet. Wir dürfen daher die Formel nicht leeren! (Ticket 54508)
      //ll('crpe.Formulas.Item.Formula.Clear()');
      if ParamPreislistenr > 0 then
      begin
        ll('crpe.Formulas.Item.Formula.Clear()');
        ll('crpe.Formulas.Item.Formula.Add()', ParamPreislistename);
      end;
    end
    else if SameText(formulaName, 'pfFilialnr') then
    begin
      ll('crpe.Formulas.Item.Formula.Clear()');
      ll('crpe.Formulas.Item.Formula.Add()', IntToStr(ParamFilialNr));
    end;
  end;
end;

procedure ThlCrwReport.SetzeCRWExportOptionen(zielDateiname: string);
begin
  if LowerCase(ExtractFileExt(zielDateiname)) = '.pdf' then
  begin
    ll('crpe.ExportOptions.FileType:Set:AdobeAcrobatPDF');
  end
  else if LowerCase(ExtractFileExt(zielDateiname)) = '.xls' then
  begin
    ll('crpe.ExportOptions.FileType:Set:MSExcel');
    // Data Only ist in vielen Fällen besser für den Anwender.
    // CSV ist keine alternative, da CSV aus irgendeinem Grund die Spaltenüberschriften
    // auf jeder Zeile wiederholt (Formular HL050214)
    ll('crpe.ExportOptions.Excel.XlsType:Set:ExcelDataOnly');
    //ll('crpe.ExportOptions.Excel.XlsType:Set:ExcelStandard');
  end
  else if LowerCase(ExtractFileExt(zielDateiname)) = '.rtf' then
  begin
    ll('crpe.ExportOptions.FileType:Set:EditableWord'); // Alternativ: MSWord oder RichText, aber ist schwieriger zu editieren
  end
  else if (LowerCase(ExtractFileExt(zielDateiname)) = '.htm') or
          (LowerCase(ExtractFileExt(zielDateiname)) = '.html') then
  begin
    ll('crpe.ExportOptions.FileType:Set:HTML40');
  end
  else if LowerCase(ExtractFileExt(zielDateiname)) = '.csv' then
  begin
    ll('crpe.ExportOptions.FileType:Set:SeparatedValues');
    ll('crpe.ExportOptions.Text.FieldSeparator:Set', ';'); // Das will Excel
  end
  else if LowerCase(ExtractFileExt(zielDateiname)) = '.txt' then
  begin
    ll('crpe.ExportOptions.FileType:Set:TextFormat');
  end
  else
  begin
    raise Exception.Create('Unbekanntes Dateiformat.');
  end;
end;

class function ThlCrwReport.CRWExportDateifilter: string;
begin
  result := 'PDF-Dateien (*.pdf)|*.pdf|' +
            'Microsoft Excel, nur unformatierte Daten (*.xls)|*.xls|' +
            'Microsoft Word / RTF (*.rtf)|*.rtf|'+
            'HTML (*.htm;*.html)|*.htm;*.html|'+
            'CSV-Datei, nur unformatierte Daten (*.csv)|*.csv|'+
            'Textdatei (*.txt)|*.txt|'+
            'Alle unterstützten Dateien|*.pdf;*.xls;*.rtf;*.htm;*.html;*.csv;*.txt';
end;

procedure ThlCrwReport.SetzeSeitenraender(FPrinterData: TPrinterData);
begin
  if FPrinterData <> nil then
  begin
    if (FPrinterData.RandUnten >= 0) then ll('crpe.Margins.Bottom:Set', IntToStr(Integer(Round(FPrinterData.RandUnten * 567))));
    if (FPrinterData.RandOben  >= 0) then ll('crpe.Margins.Top:Set',    IntToStr(integer(Round(FPrinterData.RandOben  * 567))));
  end
  else
  begin
    if (RandUnten >= 0) then ll('crpe.Margins.Bottom:Set', IntToStr(Integer(Round(RandUnten * 567))));
    if (RandOben  >= 0) then ll('crpe.Margins.Top:Set',    IntToStr(integer(Round(RandOben  * 567))));
  end;
end;

function ThlCrwReport.OptimiereReport: boolean;
begin
  // not available
  result := false;
end;

procedure ThlCrwReport.ErzeugeReportMitPassendemConnectionString;

  function UnnoetigesEntfernen(const connStr: string): string;
  var
    i: integer;
    tmp: string;
  begin
    result := connStr;

    // "Get" könnte etwas abgeschnitten haben, wenn der ConnStr > 511 war.
    // Wir entfernen daher unvollständige Attribute, und hoffen, dass diese nicht relevant sind
    for i := Length(result) downto 1 do
    begin
      if result[i] = '=' then break;
      if result[i] = ';' then
      begin
        result := Copy(result, 1, i);
        break;
      end;
    end;

    // Diese Funktion ist da, weil unser ConnStr unbedingt unter 511 Bytes sein muss, egal wie!!!
    result := StringReplace(result, 'Current Language=;;', '', [rfReplaceAll]);
    result := StringReplace(result, 'Current Language=;',  '', [rfReplaceAll]);
    result := StringReplace(result, 'Initial File Name=;;', '', [rfReplaceAll]);
    result := StringReplace(result, 'Initial File Name=;',  '', [rfReplaceAll]);
    result := StringReplace(result, 'Replication server name connect option=;;', '', [rfReplaceAll]);
    result := StringReplace(result, 'Replication server name connect option=;',  '', [rfReplaceAll]);

    // Verkürze: ABC\DEF,49010 => ABC,49010
    tmp := ConnStrReadAttr('Data Source', result);
    if (Pos('\', tmp) <> 0) and (Pos(',', tmp) <> 0) and (Pos(',', tmp) > Pos('\', tmp)) then
    begin
      tmp := StringReplace(tmp, Copy(tmp, Pos('\', tmp), Pos(',', tmp) - Pos('\', tmp)), '', []);
      result := ConnStrWriteAttr('Data Source', tmp, result);
    end;
  end;

  function VerbindungsZeugErsetzen(rptConnStr, coraConnStr: string): string;
  var
    tmp: string;
  begin
    // Diese Funktion baut einen ConnectionString für CrystalReports um.
    // Es gibt folgendes Problem:
    // Vor dem Druck muss "ConnectionBuffer" für jede Tabelle ersetzt werden.
    // Wenn der SQL-Provider zwischen dem RPT-File und CORA unterschiedlich ist,
    // dann passieren 2 Dinge:
    // 1. Das Zuweisen des ConnectionBuffers dauert extrem lange
    // 2. Der ConnectionString wird komplett umgebaut und um Defaultwerte erweitert.
    //    Bei einem etwaigen .Save() oder anderen Operationen wird
    //    der ConnectionBuffer auf 512 Zeichen abgeschnitten, und dann ist alles im
    //    Eimer! (siehe Notgedrungenem Patch "HickelSOFTWorkaround" in UCrpe32.pas,
    //    damals eingebaut, da die Ursache für das automatische Erweitern des
    //    ConnectionStrings nicht feststand)

    // Wir übertragen nun die relevanten Anmeldeinformationen vom CORA-ConnStr in den ConnStr aus dem Report!
    result := rptConnStr;
    result := ConnStrWriteAttr('User ID', ConnStrReadAttr('User ID', coraConnStr), result);
    result := ConnStrWriteAttr('Password', ConnStrReadAttr('Password', coraConnStr), result);
    result := ConnStrWriteAttr('Data Source', ConnStrReadAttr('Data Source', coraConnStr), result);
    result := ConnStrWriteAttr('Initial Catalog', ConnStrReadAttr('Initial Catalog', coraConnStr), result);
    result := ConnStrWriteAttr('Integrated Security', ConnStrReadAttr('Integrated Security', coraConnStr), result);

    tmp := ConnStrReadAttr('Provider', result);
    if not ProviderIsInstalled(tmp) then
    begin
      // Wenn der rpt Provider beim Kunden nicht installiert ist, dann den NIEDRIGSTEN Provider verwenden
      result := ConnStrWriteAttr('Provider', SqlServerProviderCompat, result);
    end;
  end;

  procedure ConnectionBufferSetzen(connstr: string);
  var
    i1, i: integer;
    stmp: string;
  begin
    // Subreport "0" ist unser Haupt-Report
    for i1 := 0 to lli('crpe.Subreports.Count:Get') -1 do
    begin
      ll('crpe.Subreports.Number:Set', IntToStr(i1));

      for i := 0 to lli('crpe.Tables.Count:Get') -1 do
      begin
        ll('crpe.Tables.Number:Set', IntToStr(i));

        stmp := lls('crpe.Tables[].ConnectBuffer:Get', IntToStr(i));
        try
          ll('crpe.Tables[].ConnectBuffer:Set', IntToStr(i),
            UnnoetigesEntfernen(VerbindungsZeugErsetzen(lls('crpe.Tables[].ConnectBuffer:Get', IntToStr(i)), connStr)));
        except
          // GMRI 06.05.2021 Mandant 1, TOURENLISTE01.rpt
          // Hat ConnectionString von 511 Bytes, und es kracht dann genau beim Setzen von ConnectBuffer
          // Daher machen wir dann das Verhalten aus CORA 7.0
          on E: EAbort do
          begin
            Abort;
          end;
          on E: Exception do
          begin
            ll('crpe.Tables[].ConnectBuffer:Set', IntToStr(i), UnnoetigesEntfernen(connStr));
          end;
        end;
      end;
    end;
    ll('crpe.Subreports.Number:Set', IntToStr(0)); // Wichtig!
  end;

var
  tmpFile: string;
  crpeCreated: boolean;
  OldConnStrMaxLength: integer;
  connstr: string;
  reg: TRegistry;
  NoReportOptimization: Boolean;
begin
  connstr := AdoConnectionString;

  if not llb('crpe:Assigned') then
  begin
    ll('crpe.Create()');
    crpeCreated := true;
  end
  else crpeCreated := false;

  ll('crpe.ReportName:Set', ''); // Closes the PrintJob
  ll('crpe.ReportName:Set', ReportFileName);

  // Die Datenbankquellen auf Mandant und Server umstellen, inkl. aller Subreporte
  ConnectionBufferSetzen(connStr);

  if crpeCreated then ll('crpe.FreeAndNil()');
end;

procedure ThlCrwReport.ll(a1, a2, a3, a4, a5, a6: string);
begin
  // Die Befehle sind in hl_IPC_Crw_LowLevel_Server.pas implementiert
  crpe2.send_lowlevel_command(a1, a2, a3, a4, a5, a6, false);
end;

procedure ThlCrwReport.llnw(a1, a2, a3, a4, a5, a6: string);
begin
  // Die Befehle sind in hl_IPC_Crw_LowLevel_Server.pas implementiert
  crpe2.send_lowlevel_command(a1, a2, a3, a4, a5, a6, true);
end;

function ThlCrwReport.lls(a1, a2, a3, a4, a5, a6: string): string;
begin
  // Die Befehle sind in hl_IPC_Crw_LowLevel_Server.pas implementiert
  result := crpe2.send_lowlevel_command(a1, a2, a3, a4, a5, a6, false);
end;

function ThlCrwReport.lli(a1, a2, a3, a4, a5, a6: string): integer;
begin
  result := StrToInt(lls(a1, a2, a3, a4, a5, a6));
end;

function ThlCrwReport.llb(a1, a2, a3, a4, a5, a6: string): boolean;
begin
  result := StrToBool(lls(a1, a2, a3, a4, a5, a6));
end;

procedure ThlCrwReport.DoExecute;
var
  nachricht: string;
begin
  (*
  try
     ll('crpe.VerifyDataBase()');
  except
     on E: EAbort do
     begin
       Abort;
     end;
     On E: Exception do
     begin
        ThlExceptionHandler.ErstelleStacktrace(E);
     end;
  end;
  *)

  try
    ll('crpe.Execute()');
  except
    on E: EAbort do
    begin
      Abort;
    end;
    on E: Exception do
    begin
      if Pos('Feldname ist unbekannt', E.Message) > 0 then
      begin
        nachricht := 'Dieses Formular kann nicht gedruckt werden, da Filter gesetzt wurden, ' +
                     'die im Kontext des Formulars ungültig sind (z.B. das Filtern nach ' +
                     'Inventurdatum in einer Preisliste oder das Filtern nach Preisgruppe ' +
                     'in einem Artikelsortiment ohne Preise). Bitte prüfen Sie die Filter ' +
                     'und versuchen Sie es erneut.'+#13#10#13#10 + E.Message;
        raise Exception.Create(nachricht);
      end
      else
      begin
        raise;
      end;
    end;
  end;

  // Workaround für seltsamen Fehler... Print to PDF, schließen, dann kommt
  // eine andere Anwendung in den Vordergrund (Modality Bug)
  SetForegroundWindow(Application.Handle);
end;

procedure ThlCrwReport.SetReport(KopieNummer: integer);
begin
  ErzeugeReportMitPassendemConnectionString;
  ll('crpe.ReportTitle:Set', ReportTitle);
  ll('crpe.SummaryInfo.Author:Set', 'HickelSOFT Huth GmbH');
  ll('crpe.SummaryInfo.Title:Set', ReportTitle);

  try
    // Ränder einstellen
    if (RandUnten >= 0) then ll('crpe.Margins.Bottom:Set', IntToStr(Integer(Round(RandUnten * 567))));
    if (RandOben  >= 0) then ll('crpe.Margins.Top:Set',    IntToStr(integer(Round(RandOben  * 567))));
  except
    on E: EAbort do
    begin
      Abort;
    end;
    on E: Exception do
    begin
      // Kann fehlschlagen: Ticket 36064, nach Update auf Windows 10
      //TMessageBox.ZeigeException(E);
      ThlExceptionHandler.ErstelleStacktrace(E);
    end;
  end;

  // Parameterfelder befüllen (soweit vorhanden/benötigt)
  SetzeFelder(nil, KopieNummer);

  // Filter setzen
  AuswahlFormelFestlegen;
end;

procedure ThlCrwReport.AuswahlFormelFestlegen;
var
  i: integer;
  alteAuswahlFormel: string;
begin
   {$REGION 'Auswahlformel festlegen'}

     {$REGION 'Auswahlformel für Hauptreport'}
      alteAuswahlFormel := lls('crpe.Selection.Formula.Text:Get');

      // Ticket 51546
      if Pos('// $$$ AN DELPHI: FORMEL NICHT ERSETZEN $$$', alteAuswahlFormel) <= 0 then
      begin
        ll('crpe.Selection.Clear()');
        ll('crpe.Selection.Formula.Add()', Filter);
      end;
     {$ENDREGION}

     {$REGION 'Auswahlformel für Subreports'}
      for i := 0 to lli('crpe.Subreports.Count:Get') - 1 do
      begin
        ll('crpe.Subreports.Number:Set', IntToStr(i));

        alteAuswahlFormel := lls('crpe.Selection.Formula.Text:Get');

        if Pos('// $$$ AN DELPHI: FORMEL NICHT ERSETZEN $$$', alteAuswahlFormel) > 0 then
        begin
          // Ticket 51546
          continue;
        end;

        if Pos('<@@@USE_PARENT_SELECTION_FORMULA@@@>', alteAuswahlFormel) > 0 then
        begin
          ll('crpe.Selection.Clear()');
          ll('crpe.Selection.Formula.Add()', Filter);
        end;
      end;
      ll('crpe.Subreports.Number:Set', '0');
     {$ENDREGION}

   {$ENDREGION}
end;

procedure ThlCrwReport.Druck(APrinterData, APrinterDataKopien: TPrinterData; ModalBildschirmDruck: boolean);

  procedure _DruckMitObjekt(FPrinterData: TPrinterData; AKopieDrucker: boolean);
  var
    LKopienModus: TKopienModus;
    i1: integer;
    tmpSchachtNr: integer;
  begin
    if Assigned(FPrinterData) and
       FPrinterData.Active and
       (FPrinterData.Printerindex > -1) and
       (FPrinterData.SpinKopien.Value > 0) then
    begin
      {$REGION 'Seitenränder setzen'}
      try
        SetzeSeitenraender(FPrinterData);
      except
        on E: EAbort do
        begin
          Abort;
        end;
        On E: Exception do
        begin
          // Kann fehlschlagen: Ticket 36064, nach Update auf Windows 10
          ThlExceptionHandler.ErstelleStacktrace(E);
        end;
      end;
      {$ENDREGION}

      {$REGION 'Einstellen des Druckernamens'}
        ll('crpe.Printer.Retrieve()');
        ll('crpe.Printer.Name:Set', TPrinterData.CachedPrinterList[FPrinterData.Printerindex]);
        // ShowDialog := False;
        if Pos('_papersize', LowerCase(ReportFileName)) > 0 then
          ll('crpe.Printer.PreserveRptSettings:Set:OrPs') // Or=prOrientation, Ps=prPaperSize
        else
          ll('crpe.Printer.PreserveRptSettings:Set:Or'); // Or=prOrientation

        // PROBLEM: Pointer kann nicht zwischen Prozessen getauscht werden. Aber müssen wir den PMode überhaupt überschreiben?! Wer sagt das?
        // ll('crpe.Printer.PMode:Set', FPrinterData.FDevMode);

        ll('crpe.Printer.Send()');
      {$ENDREGION}

      {$REGION 'Einstellen des Einzugschachts und des Duplexes'}

      //ll('Debug:CheckPMode');
      if llb('crpe.Printer.pMode:Assigned') then
      begin
        tmpSchachtNr := FPrinterData.Schachtnr;
        if tmpSchachtNr = -1 then
          // DM 04.06.2023 : Geändert von 0 auf DMBIN_AUTO . Nicht 100% sicher, ob das richtig ist.
          ll('crpe.Printer.pMode^.dmDefaultSource:Set', IntToStr(DMBIN_AUTO))
        else
          ll('crpe.Printer.pMode^.dmDefaultSource:Set', IntToStr(tmpSchachtNr));
        (*
        if (FPrinterData.Zufuhrliste <> nil) and (FPrinterData.Zufuhrliste.Items.Count > 0) and
           FPrinterData.Zufuhrliste.Enabled then
        begin
          ll('crpe.Printer.pMode^.dmDefaultSource:Set', FPrinterData.FBinNumber[FPrinterData.Zufuhrliste.ItemIndex])
        end
        else
        begin
          // DM 04.06.2023 : Geändert von 0 auf DMBIN_AUTO . Nicht 100% sicher, ob das richtig ist.
          ll('crpe.Printer.pMode^.dmDefaultSource:Set', IntToStr(DMBIN_AUTO))
        end;
        *)

          // Hinweis: TPrinterData.SetDuplexListe definiert   0=Simplex, 1=Horizontal, 2=Vertical,
          //          während die WinAPI (wingdi.h) definiert 1=Simplex, 2=Vertical,   3=Horizontal
          if FPrinterData.DuplexIndex = 0 then ll('crpe.Printer.pMode^.dmDuplex:Set:DMDUP_SIMPLEX');
          if FPrinterData.DuplexIndex = 1 then ll('crpe.Printer.pMode^.dmDuplex:Set:DMDUP_HORIZONTAL');
          if FPrinterData.DuplexIndex = 2 then ll('crpe.Printer.pMode^.dmDuplex:Set:DMDUP_VERTICAL');
      end;

      ll('crpe.Printer.Send()');
      {$ENDREGION}

      {$REGION 'Nun drucken, abhängig von der Sortierreihenfolge'}

      LKopienModus := FPrinterData.KopienModus;
      if LKopienModus = kmDefault then
      begin
        if AKopieDrucker then
          LKopienModus := km123123
        else
          LKopienModus := kmPfKopie;
      end;

      case LKopienModus of
        kmPfKopie:
        begin
          if AKopieDrucker then
          begin
            ll('crpe.Printoptions.Copies:Set', '1'); // muss "1" sein, denn wir iterieren ja in der Forschleife (weil wir pfKopie individuell machen müssen)
            // ll('crpe.PrintOptions.Collation:Set', BoolToStr(DefaultCollation));
            for i1 := 1 to FPrinterData.SpinKopien.Value do
            begin
              {$REGION 'pf* Felder setzen, insbesondere pfKopie=i1'}
              SetzeFelder(FPrinterData, i1);
              {$ENDREGION}
              DoExecute;
            end;
          end
          else
          begin
            ll('crpe.Printoptions.Copies:Set', IntToStr(FPrinterData.SpinKopien.Value));
            // ll('crpe.PrintOptions.Collation:Set', BoolToStr(DefaultCollation));
            {$REGION 'pf* Felder setzen, insbesondere pfKopie=0'}
            SetzeFelder(FPrinterData, 0{Standarddrucker hat immer pfKopie=0});
            {$ENDREGION}
            DoExecute;
          end;
        end;

        km123123:
        begin
          ll('crpe.Printoptions.Copies:Set', IntToStr(FPrinterData.SpinKopien.Value));
          ll('crpe.PrintOptions.Collation:Set', BoolToStr(true));
          {$REGION 'pf* Felder setzen'}
          SetzeFelder(FPrinterData, 0{pfKopie nicht möglich oder implementiert});
          {$ENDREGION}
          DoExecute;
        end;

        km112233:
        begin
          ll('crpe.Printoptions.Copies:Set', IntToStr(FPrinterData.SpinKopien.Value));
          ll('crpe.PrintOptions.Collation:Set', BoolToStr(false));
          {$REGION 'pf* Felder setzen'}
          SetzeFelder(FPrinterData, 0{pfKopie nicht möglich oder implementiert});
          {$ENDREGION}
          DoExecute;
        end;
      end;
      {$ENDREGION}
    end;
  end;

var
  IstBildschirmDruck: Boolean;

begin
  // TODO: Sollten wir hier auch mehrfach versuchen, wenn wir eine EAccessViolation bekommen,
  //       so wie bei Anzeigen/DateiExport/SendEMail? Aber es besteht die Gefahr, dass
  //       eventuell der Ausdruck dann mehrfach erfolgen könnte (Müller 58601)
   {$REGION 'Connection setzen, sowie einige andere Parameter'}
  ErzeugeReportMitPassendemConnectionString;

  // ll('crpe.Reportoptions.VerifyOnEveryPrint:Set', BoolToStr(True));

  ll('crpe.ReportTitle:Set', ReportTitle);

  ll('crpe.ProgressDialog:Set', BoolToStr(not KeinProgressDialog));

  ll('crpe.SummaryInfo.Author:Set', 'HickelSOFT Huth GmbH');
  ll('crpe.SummaryInfo.Title:Set', ReportTitle);
   {$ENDREGION}

   AuswahlFormelFestlegen;

   IstbildschirmDruck := true;
   if Assigned(APrinterData) then
     IstBildschirmDruck := APrinterData.PrinterIndex <= -1
   else if Assigned(APrinterDataKopien) then
     IstBildschirmDruck := APrinterDataKopien.PrinterIndex <= -1
   else
     Assert(False);

   if not IstbildschirmDruck then
   begin
     ll('crpe.Output:Set:toPrinter');
     _DruckMitObjekt(APrinterData, false);
     _DruckMitObjekt(APrinterDataKopien, true);
   end
   else
   begin
     Anzeige(ModalBildschirmDruck);
   end;
end;

function ThlCrwReport.IstBonFormular: boolean;
var
  fn: string;
begin
  fn := ExtractFilename(ReportFileName);
  result := StartsText('LM_BELEG_BON', fn)
         or StartsText('KETT_', fn)
         or StartsText('__KASSE_NG_', fn);
end;

procedure ThlCrwReport.Druck(APrinterName: string; AAnzahl: integer; ADuplexNr, ASchachtNr: integer; KopieNummer: integer=0);
var
  aReportFileNameOriginal: string;
  aReportFileNameBixolon: string;
begin
  // TODO: Sollten wir hier auch mehrfach versuchen, wenn wir eine EAccessViolation bekommen,
  //       so wie bei Anzeigen/DateiExport/SendEMail? Aber es besteht die Gefahr, dass
  //       eventuell der Ausdruck dann mehrfach erfolgen könnte (Müller 58601)
  try
    aReportFileNameOriginal := ReportFileName;
    try
      if ContainsText(APrinterName, 'Bixolon') then
      begin
        // Sonderanpassungen für Bixolon: __KASSE_NG_LEERGUT.RPT'
        // Änderungen: 1. Leergut-Code Formel muss "LGBxxxx" heißen und nicht "*LGBxxxx*" (Fehler gemeldet 04.05.2022 an den Hersteller)
        //                (Damit "LGBxxxx" überhaupt funktioniert, wird der Treiber vom 04.05.2022 benötigt)
        //             2. Barcode Font Name heißt "Barcode5". Es ist ganz wichtig, dass zuerst die
        //                Schriftart verkleinert wird z.B. 20 und dann "Barcode5" gewählt wird.
        //                Ansonsten wird "Barcode5" zurückgesetzt. Die Barcodegröße wird im Druckertriber gesetzt.
        aReportFileNameBixolon := ChangeFileExt(ReportFileName, '_BIXOLON.rpt');
        if FileExists(aReportFileNameBixolon) then ReportFileName := aReportFileNameBixolon;

        // Ebenfalls an Hersteller gemeldet am 04.05.2022 und mit einem Firmware-Patch behoben:
        // QR-Codes drucken mit Bixolon nicht richtig
      end;

      SetReport(KopieNummer);

      // Die Existenz von EpsStmApi.dll wird geprüft für den Fall dass Kunden den
      // Bondrucker in der Systemsteuerung umbenannt haben
      // TODO: Wenn es kein Bondrucker ist (sondern ein A4 Drucker), dann darf dieser Code nicht greifen! Denn A4-Drucker kommen mit Rand=0 nicht aus! (Ticket 56268)
      if IstBonFormular and
         (ContainsText(APrinterName, 'Bixolon') or (not ContainsText(APrinterName, 'Epson') and not FileExists(GetSysDir+'\EpsStmApi.dll'))) then
      begin
        // EPSON braucht einen linken Rand von 0,395cm; deswegen sind alle unsere
        // Reports so angepasst. BIXOLON und der Rest der Welt
        // braucht aber 0cm, ansonsten ist rechts etwas abgeschnitten.
        ll('crpe.Margins.Left:Set', '0');
        // Ticket 51601
        ll('crpe.Margins.Right:Set', '0');
      end;

      {$REGION 'Druckereinstellungen'}
      if APrinterName = '' then
      begin
        // da dies definitiv niemals ein Bildschirmdruck ist, Fehler auslösen, wenn Druckername leer ist
        raise ThlException.Create('Druck kann nicht gestartet werden, kein Drucker definiert!');
      end;

      ll('crpe.Output:Set:toPrinter');
      ll('crpe.ProgressDialog:Set', BoolToStr(not KeinProgressDialog));
      ll('crpe.Printer.Retrieve()');
      ll('crpe.Printer.Name:Set', APrinterName);
      if Pos('_papersize', LowerCase(ReportFileName)) > 0 then
        ll('crpe.Printer.PreserveRptSettings:Set:OrPs') // Or=prOrientation, Ps=prPaperSize
      else
        ll('crpe.Printer.PreserveRptSettings:Set:Or'); // Or=prOrientation
      ll('crpe.Printoptions.Copies:Set', IntToStr(AAnzahl));
      ll('crpe.Printer.Send()'); // Damit wird auch der PMode und FDEVMode gesetzt.

      //ll('Debug:CheckPMode');
      if llb('crpe.Printer.pMode:Assigned') then
      begin
        // Definiert in wingdi.h: 1 = DMDUP_SIMPLEX; 2 = DMDUP_VERTICAL; 3 = DMDUP_HORIZONTAL
        if ADuplexNr = 1 then ll('crpe.Printer.pMode^.dmDuplex:Set:DMDUP_SIMPLEX');
        if ADuplexNr = 2 then ll('crpe.Printer.pMode^.dmDuplex:Set:DMDUP_VERTICAL');
        if ADuplexNr = 3 then ll('crpe.Printer.pMode^.dmDuplex:Set:DMDUP_HORIZONTAL');

        if ASchachtNr >= 0 then ll('crpe.Printer.pMode^.dmDefaultSource:Set', IntToStr(ASchachtNr)); // StrToInt (FPrinterData.FBinNumber[FPrinterData.Zufuhrliste.ItemIndex])

        ll('crpe.Printer.Send()'); // Nochmals übertragen, wegen des neuen PMode
      end;
      {$ENDREGION}

      DoExecute;
    finally
      ReportFileName := aReportFileNameOriginal;
    end;
  except
    on E: EAbort do
    begin
      Abort;
    end;
    On E: Exception do
    begin
      ThlExceptionHandler.ErstelleStackTrace(E);
      TMessageBox.ZeigeException(e);
    end;
  end;
end;

procedure ThlCrwReport.Anzeige(Modal: boolean);

  procedure _Anzeige;
  var
    aReportFileNameBixolon: string;
    aReportFileNameOriginal: string;
    aPrinterName: string;
  begin
    aReportFileNameOriginal := ReportFileName;
    try
      aPrinterName := '';

      try
        Printers.Printer.PrinterIndex := -1; // default printer
        aPrinterName := Printers.Printer.Printers.Strings[Printer.PrinterIndex];
      except
        on E: EAbort do
        begin
          Abort;
        end;
      end;

      if ContainsText(aPrinterName, 'Bixolon') then
      begin
        // Sonderanpassungen für Bixolon: __KASSE_NG_LEERGUT.RPT'
        // Änderungen: 1. Leergut-Code Formel muss "LGBxxxx" heißen und nicht "*LGBxxxx*" (Fehler gemeldet 04.05.2022 an den Hersteller)
        //                (Damit "LGBxxxx" überhaupt funktioniert, wird der Treiber vom 04.05.2022 benötigt)
        //             2. Barcode Font Name heißt "Barcode5". Es ist ganz wichtig, dass zuerst die
        //                Schriftart verkleinert wird z.B. 20 und dann "Barcode5" gewählt wird.
        //                Ansonsten wird "Barcode5" zurückgesetzt. Die Barcodegröße wird im Druckertriber gesetzt.
        aReportFileNameBixolon := ChangeFileExt(ReportFileName, '_BIXOLON.rpt');
        if FileExists(aReportFileNameBixolon) then ReportFileName := aReportFileNameBixolon;

        // Ebenfalls an Hersteller gemeldet am 04.05.2022 und mit einem Firmware-Patch behoben:
        // QR-Codes drucken mit Bixolon nicht richtig
      end;

      SetReport(0);

      // Die Existenz von EpsStmApi.dll wird geprüft für den Fall dass Kunden den
      // Bondrucker in der Systemsteuerung umbenannt haben
      if IstBonFormular and
         (ContainsText(APrinterName, 'Bixolon') or (not ContainsText(APrinterName, 'Epson') and not FileExists(GetSysDir+'\EpsStmApi.dll'))) then
      begin
        // EPSON braucht einen linken Rand von 0,395cm; deswegen sind alle unsere
        // Reports so angepasst. BIXOLON und der Rest der Welt
        // braucht aber 0cm, ansonsten ist rechts etwas abgeschnitten.
        ll('crpe.Margins.Left:Set', '0');
        // Ticket 51601
        ll('crpe.Margins.Right:Set', '0');
      end;

      ll('crpe.Output:Set:toWindow');
      ll('crpe.ProgressDialog:Set', BoolToStr(not KeinProgressDialog));

      ll('crpe.Printer.Retrieve()');
      ll('crpe.Printer.SetCurrent()');

      // Dieser Part kommt von einer anderen Implementierung
      Printer.PrinterIndex := -1;
      ll('crpe.Printer.Name:Set', TPrinterData.CachedPrinterList[Printer.PrinterIndex]);
      (*
      if Pos('_papersize', LowerCase(ReportFileName)) > 0 then
          ll('crpe.Printer.PreserveRptSettings:Set:OrPs') // Or=prOrientation, Ps=prPaperSize
      else
          ll('crpe.Printer.PreserveRptSettings:Set:Or'); // Or=prOrientation
      // (Ende)
      *)
      // DM 07.07.2023: Bildschirmanzeige zeigt nun das Originalformat an, und nicht mehr das format des Windows-Standard-Druckers
      ll('crpe.Printer.PreserveRptSettings:Set:OrPs'); // Or=prOrientation, Ps=prPaperSize

      ll('crpe.Printer.Send()');

      ll('crpe.ReportTitle:Set', ReportTitle);

      SetzeBildschirmDruckFenstereinstellungen;

      ll('crpe.SummaryInfo.Author:Set', 'HickelSOFT Huth GmbH');
      ll('crpe.SummaryInfo.Title:Set', ReportTitle);

      // DoExecute;
      ll('crpe.Show()');

      if Modal then
      begin
        ll('crpe.ReportWindowHandle:Wait');
      end
      else
      begin
        llnw('crpe.ReportWindowHandle:WaitAndExit');
        FreeAndNil(crpe2); // Verhindern, dass ein FreeAndNil()+Exit im Destructor von ThlCrwReport erfolgt
      end;
    finally
      ReportFileName := aReportFileNameOriginal;
    end;
  end;

var
  errCount: integer;
  BitteNochmalVersuchen: boolean;
begin
  errCount := 0;
  repeat
    BitteNochmalVersuchen := false;
    try
      _Anzeige;
    except
      on E: EAbort do
      begin
        Abort;
      end;
      on E: EHsIpcForwardedException do
      begin
        if EHsIpcForwardedException(E).OriginalClass = 'EAccessViolation' then
        begin
          BitteNochmalVersuchen := errCount < 3;
          Inc(errCount);
          if BitteNochmalVersuchen then
          begin
            // Ticket 52325 und Ticket 53031, und vielleicht sogar ein paar mehr Kunden...
            // ... da kommt es bei ll('crpe.Printer.Send()') sporadisch zu einem Fehler in MWSetupPrinter() bei CRPE32.DLL
            // Bei mir (mit Delphi 11) kam sogar mal das an einer anderen Stelle im Dateiexport:
            // "Zugriffsverletzung bei Adresse 75F9DC0B in Modul 'RPCRT4.dll'. Lesen von Adresse 00000438"
            // und unmittelbar danach ging wieder alles (im selben Prozess!)
            // Deswegen machen wir hier einen Quick'n'Dirty fix:
            // Wenn der Dateiexport fehlgeschlagen ist, dann warten wir 1 Sekunde und
            // probieren es einfach nochmal!
            Sleep(1000);
          end
          else
          begin
            ThlExceptionHandler.ErstelleStackTrace(E);
            TMessageBox.ZeigeException(e);
          end;
        end
        else
        begin
          BitteNochmalVersuchen := false;
          ThlExceptionHandler.ErstelleStackTrace(E);
          TMessageBox.ZeigeException(e);
        end;
      end;
      on E: Exception do
      begin
        BitteNochmalVersuchen := false;
        ThlExceptionHandler.ErstelleStackTrace(E);
        TMessageBox.ZeigeException(e);
      end;
    end;
  until not BitteNochmalVersuchen;
end;

procedure ThlCrwReport.DateiExport(AExportDateiname: string);

  procedure _DateiExport(AExportDateiname: string);
  begin
    SetReport(0);

    ll('crpe.Output:Set:toExport');
    ll('crpe.ProgressDialog:Set', BoolToStr(not KeinProgressDialog));
    ll('crpe.ExportOptions.PromptForOptions:Set', BoolToStr(False));

    SetzeCRWExportOptionen(AExportDateiname);
    ll('crpe.ExportOptions.FileName:Set', AExportDateiname);

    ll('crpe.Printer.Retrieve()');
    ll('crpe.Printer.SetCurrent()');
    // TODO: Geht nicht bei einem Kunden (Ticket 55851), aber bei uns geht es?!
    ll('crpe.Printer.PreserveRptSettings:Set:OrPs'); // Or=prOrientation, Ps=prPaperSize  (DM 07.07.2023 hinzugefügt)
    ll('crpe.Printer.Send()');

    ll('crpe.ReportTitle:Set', ReportTitle);

    ll('crpe.SummaryInfo.Author:Set', 'HickelSOFT Huth GmbH');
    ll('crpe.SummaryInfo.Title:Set', ReportTitle);

    DoExecute;
  end;

var
  errCount: integer;
  BitteNochmalVersuchen: boolean;
begin
  errCount := 0;
  repeat
    BitteNochmalVersuchen := false;
    try
      _DateiExport(AExportDateiname);
    except
      on E: EAbort do
      begin
        Abort;
      end;
      on E: EHsIpcForwardedException do
      begin
        if EHsIpcForwardedException(E).OriginalClass = 'EAccessViolation' then
        begin
          BitteNochmalVersuchen := errCount < 3;
          Inc(errCount);
          if BitteNochmalVersuchen then
          begin
            // Ticket 52325 und Ticket 53031, und vielleicht sogar ein paar mehr Kunden...
            // ... da kommt es bei ll('crpe.Printer.Send()') sporadisch zu einem Fehler in MWSetupPrinter() bei CRPE32.DLL
            // Bei mir (mit Delphi 11) kam sogar mal das an einer anderen Stelle im Dateiexport:
            // "Zugriffsverletzung bei Adresse 75F9DC0B in Modul 'RPCRT4.dll'. Lesen von Adresse 00000438"
            // und unmittelbar danach ging wieder alles (im selben Prozess!)
            // Deswegen machen wir hier einen Quick'n'Dirty fix:
            // Wenn der Dateiexport fehlgeschlagen ist, dann warten wir 1 Sekunde und
            // probieren es einfach nochmal!
            Sleep(1000);
          end
          else
          begin
            ThlExceptionHandler.ErstelleStackTrace(E);
            raise; // TMessageBox.ZeigeException(e);
          end;
        end
        else
        begin
          BitteNochmalVersuchen := false;
          ThlExceptionHandler.ErstelleStackTrace(E);
          raise; // TMessageBox.ZeigeException(e);
        end;
      end;
      on E: Exception do
      begin
        BitteNochmalVersuchen := false;
        ThlExceptionHandler.ErstelleStackTrace(E);
        raise; // TMessageBox.ZeigeException(e);
      end;
    end;
  until not BitteNochmalVersuchen;

  if Assigned(DateiNachBearbeitung) then DateiNachBearbeitung(AExportDateiname);
end;

procedure ThlCrwReport.SetzeBildschirmDruckFenstereinstellungen;
begin
  ll('crpe.WindowState:Set', IntToStr(Ord(wsMaximized)));
  ll('crpe.WindowStyle.Title:Set', ReportTitle);
  ll('crpe.WindowStyle.BorderStyle:Set', IntToStr(Ord(bsSingle)));
  ll('crpe.WindowStyle.MaxButton:Set', BoolToStr(True));
  ll('crpe.WindowStyle.MinButton:Set', BoolToStr(False));
  ll('crpe.WindowButtonBar.CancelBtn:Set', BoolToStr(True));
  ll('crpe.WindowButtonBar.RefreshBtn:Set', BoolToStr(True));
  ll('crpe.WindowButtonBar.SearchBtn:Set', BoolToStr(True));
  ll('crpe.WindowButtonBar.CloseBtn:Set', BoolToStr(True));
  ll('crpe.WindowButtonBar.PrintSetupBtn:Set', BoolToStr(True));
  ll('crpe.WindowZoom.Preview:Set:pwPageWidth');
end;

procedure ThlCrwReport.SendEMail(ExportFilename: string=''; betreff: string=''; text: string=''; toEMail: string=''; toEMailCC: string=''; toEMailBCC: string='');

  procedure _SendEMail(ExportFilename: string='');
  var
    DateinameBasis: string;
  begin
    SetReport(0);

    // Ticket 49198
    //DateinameBasis := 'EMail_Anhang_' + RandomString(10);
    DateinameBasis := CleanFileName(ReportTitle);
    if DateinameBasis = '' then DateinameBasis := 'Anhang';

    ll('crpe.Output:Set:toExport');
    ll('crpe.ProgressDialog:Set', BoolToStr(not KeinProgressDialog));
    ll('crpe.ExportOptions.PromptForOptions:Set', BoolToStr(False));
    ll('crpe.ExportOptions.FileType:Set:AdobeAcrobatPDF');

    if ExportFilename = '' then
    begin
      ExportFilename := IncludeTrailingPathDelimiter(GetTempDir) + '\'+DateinameBasis+'.pdf';
    end;

    SetzeCRWExportOptionen(ExportFilename);
    ll('crpe.ExportOptions.FileName:Set', ExportFileName);

    ll('crpe.Printer.Retrieve()');
    ll('crpe.Printer.SetCurrent()');
    ll('crpe.Printer.PreserveRptSettings:Set:OrPs'); // Or=prOrientation, Ps=prPaperSize  (DM 07.07.2023 hinzugefügt)
    ll('crpe.Printer.Send()');

    ll('crpe.ReportTitle:Set', ReportTitle);

    ll('crpe.SummaryInfo.Author:Set', 'HickelSOFT Huth GmbH');
    ll('crpe.SummaryInfo.Title:Set', ReportTitle);

    DoExecute;
  end;

var
  errCount: integer;
  BitteNochmalVersuchen: boolean;
  dateiZuSenden: string;
begin
  errCount := 0;
  repeat
    BitteNochmalVersuchen := false;
    try
      _SendEMail(ExportFilename);
    except
      on E: EAbort do
      begin
        Abort;
      end;
      on E: EHsIpcForwardedException do
      begin
        if EHsIpcForwardedException(E).OriginalClass = 'EAccessViolation' then
        begin
          BitteNochmalVersuchen := errCount < 3;
          Inc(errCount);
          if BitteNochmalVersuchen then
          begin
            // Ticket 52325 und Ticket 53031, und vielleicht sogar ein paar mehr Kunden...
            // ... da kommt es bei ll('crpe.Printer.Send()') sporadisch zu einem Fehler in MWSetupPrinter() bei CRPE32.DLL
            // Bei mir (mit Delphi 11) kam sogar mal das an einer anderen Stelle im Dateiexport:
            // "Zugriffsverletzung bei Adresse 75F9DC0B in Modul 'RPCRT4.dll'. Lesen von Adresse 00000438"
            // und unmittelbar danach ging wieder alles (im selben Prozess!)
            // Deswegen machen wir hier einen Quick'n'Dirty fix:
            // Wenn der Dateiexport fehlgeschlagen ist, dann warten wir 1 Sekunde und
            // probieren es einfach nochmal!
            Sleep(1000);
          end
          else
          begin
            ThlExceptionHandler.ErstelleStackTrace(E);
            raise; // TMessageBox.ZeigeException(e);
          end;
        end
        else
        begin
          BitteNochmalVersuchen := false;
          ThlExceptionHandler.ErstelleStackTrace(E);
          raise; // TMessageBox.ZeigeException(e);
        end;
      end;
      on E: Exception do
      begin
        BitteNochmalVersuchen := false;
        ThlExceptionHandler.ErstelleStackTrace(E);
        raise; // TMessageBox.ZeigeException(e);
      end;
    end;
  until not BitteNochmalVersuchen;

  if Assigned(DateiNachBearbeitung) then
    dateiZuSenden := DateiNachBearbeitung(ExportFileName)
  else
    dateiZuSenden := ExportFileName;

  try
    if FileExists(ExportFilename) then
    begin
      ThlEMail.SendEMailWithAttachment(
        betreff,  // Betreff
        text,  // Nachrichtentext
        dateiZuSenden, // pdfDatei
        '', '', // Sender (kommt vom E-Mail-Programm)
        toEMailBCC, //emailAdresseBcc
        toEMailCC, //emailAdresseCC
        '', //empfaengerName
        toEMail); // emailAdresse
    end;
  except
    on E: EAbort do
    begin
      Abort;
    end;
    on E: Exception do
    begin
      ThlExceptionHandler.ErstelleStackTrace(E);
      TMessageBox.ZeigeException(e);
    end;
  end;
end;

end.
