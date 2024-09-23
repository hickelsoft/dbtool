unit hl_Report;

interface

uses
  DB, Drucker;

type
  TpfFeldCallback = function(pfFeldName: string; var outPfValue: string): boolean of object;

  ThlReport = class(TObject)
  strict private
    FRandOben: Double;
    FRandUnten: Double;
    FKopfDrucken: boolean;
    FFussDrucken: boolean;
    FKopfSeite2Anders: boolean;
    FAdressUeberschriftDrucken: boolean;

    FParamVon1: string;
    FParamBis1: string;
    FParamVon2: string;
    FParamBis2: string;
    FParamVon3: string;
    FParamBis3: string;
    FParamVon4: string;
    FParamBis4: string;
    FParamVon5: string;
    FParamBis5: string;
    FParamVon6: string;
    FParamBis6: string;
    FParamVon7: string;
    FParamBis7: string;
    FParamVon8: string;
    FParamBis8: string;
    FParamVon9: string;
    FParamBis9: string;
    FParamVon10: string;
    FParamBis10: string;
    FParamVon11: string;
    FParamBis11: string;
    FParamVon12: string;
    FParamBis12: string;
    FParamVon13: string;
    FParamBis13: string;
    FParamVon14: string;
    FParamBis14: string;
    FParamVon15: string;
    FParamBis15: string;
    FpfFeldCB: TpfFeldCallback;

    FReportTitle: string;
    FReportFileName: string;
    FFilter: string;

    FAdoConnectionString: string;

    procedure SetReportFileName(const Value: string); protected
  public
    property RandOben: Double read FRandOben write FRandOben;
    property RandUnten: Double read FRandUnten write FRandUnten;
    property KopfDrucken: boolean read FKopfDrucken write FKopfDrucken;
    property FussDrucken: boolean read FFussDrucken write FFussDrucken;
    property KopfSeite2Anders: boolean read FKopfSeite2Anders write FKopfSeite2Anders;
    property AdressUeberschriftDrucken: boolean read FAdressUeberschriftDrucken write FAdressUeberschriftDrucken;

    property ParamVon1: string read FParamVon1 write FParamVon1;
    property ParamBis1: string read FParamBis1 write FParamBis1;
    property ParamVon2: string read FParamVon2 write FParamVon2;
    property ParamBis2: string read FParamBis2 write FParamBis2;
    property ParamVon3: string read FParamVon3 write FParamVon3;
    property ParamBis3: string read FParamBis3 write FParamBis3;
    property ParamVon4: string read FParamVon4 write FParamVon4;
    property ParamBis4: string read FParamBis4 write FParamBis4;
    property ParamVon5: string read FParamVon5 write FParamVon5;
    property ParamBis5: string read FParamBis5 write FParamBis5;
    property ParamVon6: string read FParamVon6 write FParamVon6;
    property ParamBis6: string read FParamBis6 write FParamBis6;
    property ParamVon7: string read FParamVon7 write FParamVon7;
    property ParamBis7: string read FParamBis7 write FParamBis7;
    property ParamVon8: string read FParamVon8 write FParamVon8;
    property ParamBis8: string read FParamBis8 write FParamBis8;
    property ParamVon9: string read FParamVon9 write FParamVon9;
    property ParamBis9: string read FParamBis9 write FParamBis9;
    property ParamVon10: string read FParamVon10 write FParamVon10;
    property ParamBis10: string read FParamBis10 write FParamBis10;
    property ParamVon11: string read FParamVon11 write FParamVon11;
    property ParamBis11: string read FParamBis11 write FParamBis11;
    property ParamVon12: string read FParamVon12 write FParamVon12;
    property ParamBis12: string read FParamBis12 write FParamBis12;
    property ParamVon13: string read FParamVon13 write FParamVon13;
    property ParamBis13: string read FParamBis13 write FParamBis13;
    property ParamVon14: string read FParamVon14 write FParamVon14;
    property ParamBis14: string read FParamBis14 write FParamBis14;
    property ParamVon15: string read FParamVon15 write FParamVon15;
    property ParamBis15: string read FParamBis15 write FParamBis15;
    property pfFeldCallback: TpfFeldCallback read FpfFeldCB write FpfFeldCB;

    property ReportTitle: string read FReportTitle write FReportTitle;
    property ReportFileName: string read FReportFileName write SetReportFileName;
    property Filter: string read FFilter write FFilter;

    property AdoConnectionString: string read FAdoConnectionString;

    procedure SetDbConnection(DatenbankServer, DatenbankName: string; NtAuth: boolean; BenutzerName, Passwort: string); virtual;

    procedure Druck(APrinterName: string; AAnzahl: integer; ADuplexNr, ASchachtNr: integer; KopieNummer: integer=0); overload; virtual; abstract;
    procedure Druck(FPrinterData, FPrinterDataKopien: TPrinterData; ModalBildschirmDruck: boolean); overload; virtual; abstract;
    procedure Anzeige(Modal: boolean); virtual; abstract;
    procedure DateiExport(AExportDateiname: string); virtual; abstract;
    procedure SendEMail(ExportFilename: string=''; betreff: string=''; text: string=''; toEMail: string=''; toEMailCC: string=''; toEMailBCC: string=''); virtual; abstract;
  end;

implementation

uses
  HsSqlServerProvider, SysUtils;

{ ThlReport }

procedure ThlReport.SetDbConnection(DatenbankServer, DatenbankName: string; NtAuth: boolean; BenutzerName, Passwort: string);
begin
  if NtAuth then
  begin
    FAdoConnectionString :=
             'Provider='+SqlServerProvider+';;' +
             'Data Source=' + DatenbankServer + ';;' +
             'Initial Catalog=' + DatenbankName  + ';;' +

             'User ID=;;' +
             'Password=;;' +
             'Application Name='+ExtractFileName(ParamStr(0))+' hl_Report;;' +
             'Integrated Security=SSPI;;' +
             'Persist Security Info=False;;' +

           //'Use DSN Default Properties=0;; ' +
             'Locale Identifier=1031;;' +
             'Connect Timeout=15;;' +
             'General Timeout=0;;' +
             'OLE DB Services=-5;;' +
             'Current Language=;;' +
             'Initial File Name=;;' +
             'Use Encryption for Data=0;;' +
             'Replication server name connect option=;;' +
             'Tag with column collation when possible=0';
  end
  else
  begin
    FAdoConnectionString :=
             'Provider='+SqlServerProvider+';;' +
             'Data Source=' + DatenbankServer + ';;' +
             'Initial Catalog=' + DatenbankName  + ';;' +

             'User ID=sa;;' +
             'Password='+Passwort+';;' +
             'Application Name='+ExtractFileName(ParamStr(0))+' hl_Report;;' +
             'Integrated Security=0;;' +
             'Persist Security Info=True;;' + // Achtung: CRW ignoriert das aus Sicherheitsgründen!

             'Use DSN Default Properties=0;; ' +
             'Locale Identifier=1031;;' +
             'Connect Timeout=15;;' +
             'General Timeout=0;;' +
             'OLE DB Services=-5;;' +
             'Current Language=;;' +
             'Initial File Name=;;' +
             'Use Encryption for Data=0;;' +
             'Replication server name connect option=;;' +
             'Tag with column collation when possible=0';
  end;

  if SqlServerProvider = 'MSOLEDBSQL19' then
    FAdoConnectionString := FAdoConnectionString + ';;Use Encryption for Data=Optional';

end;

procedure ThlReport.SetReportFileName(const Value: string);
begin
  if FReportFileName <> Value then
  begin
    if not FileExists(Value) then
    begin
      raise Exception.CreateFmt('Reportfile "%s" nicht vorhanden, Druck abgebrochen!', [Value]);
    end;
    FReportFileName := Value;
  end;
end;

end.
