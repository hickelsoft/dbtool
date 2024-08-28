unit HsDruckerConfig;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Drucker, ADODB;

type
  {$IF CompilerVersion > 20.0} // Version geraten
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$IFEND}
  THsDruckerConfig = class(TComponent)
  private
    FPrinterData: TPrinterData;
    FPrinterDataKopien: TPrinterData;
    FFormularArt: string;
    FFormularNr: integer;
    FConnection: TADOConnection;
    FDruckerArt: string;
  protected
    procedure WarnConnectionMissing;
  public
    function WritePrinter: boolean;
    function ReadPrinter: boolean;
  published
    property FormularArt: string read FFormularArt write FFormularArt;
    property FormularNr: integer read FFormularNr write FFormularNr;
    property DruckerArt: string read FDruckerArt write FDruckerArt;
    property Connection: TADOConnection read FConnection write FConnection;
    Property PrinterData: TPrinterData read FPrinterData write FPrinterData;
    Property PrinterDataKopien: TPrinterData read FPrinterDataKopien write FPrinterDataKopien;
  end;


procedure Register;

implementation

uses  IniFiles, printers, MessaBox;

procedure Register;
begin
   RegisterComponents( 'HS', [THsDruckerConfig] );
end;

function GetComputerName: string; // siehe auch ThlUtils.hsGetComputerName
                                  // Ist hier nochmal separat, da Hickel_Components.bpl nicht von HickelLib abhängt
var
   ComputerName: String;
   nsize: Dword;
begin
  nsize := 25;
  SetLength(ComputerName,nsize);
  if Windows.GetComputerName(PChar(ComputerName),nsize) then
  begin
     SetLength(ComputerName,nsize);
     result := ComputerName;
  end
  else result := '';
end;
procedure THsDruckerConfig.WarnConnectionMissing;
begin
  // TODO: Loggen
  HsShowMessage(Format('Drucker-Einstellungen konnten nicht geladen bzw. gespeichert werden, da keine Datenbankverbindung besteht. (%s-%d%s)', [FFormularArt, FFormularNr, FDruckerArt]), 'Drucker-Konfiguration', mbsExclamation, mbbOk);
end;

function THsDruckerConfig.WritePrinter: boolean;

  procedure _Go(DruckerNr: integer; PrinterData: TPrinterData);
  var
    ds: TADODataSet;
  begin
    if PrinterData = nil then exit;
    if FFormularArt = '' then exit;

    FConnection.BeginTrans;
    try
      FConnection.Execute('delete from DRUCKFORMEINST_POS where FORMULARART = '''+FFormularArt+''' and FORMULARNR = '+IntToStr(FFormularNr)+' and DRUCKERART = '''+FDruckerArt+''' and LFD_DRUCKNR = '+IntToStr(DruckerNr)+' and BAP = '''+GetComputerName+''';'); // TODO: escaping

      ds := TADODataSet.Create(nil);
      try
        ds.EnableBCD := false;
        ds.Connection := FConnection;
        ds.ParamCheck := false;
        ds.CommandText := 'select * from DRUCKFORMEINST_POS where 1=0';
        ds.Active := true;
        ds.DisableControls;

        ds.Insert;
        ds.FieldByName('FORMULARART').AsString  := FFormularArt;
        ds.FieldByName('FORMULARNR').AsInteger  := FFormularNr;
        ds.FieldByName('DRUCKERART').AsString   := FDruckerArt;
        ds.FieldByName('LFD_DRUCKNR').AsInteger := DruckerNr;
        ds.FieldByName('BAP').AsString          := GetComputerName;

        if PrinterData.PrinterIndex >= 0 then
          ds.FieldByName('DRUCKERNAME').AsString := PrinterData.PrinterName
        else
          ds.FieldByName('DRUCKERNAME').AsString := 'Bildschirm';

        if PrinterData.Zufuhrliste <> nil then
          ds.FieldByName('ZUFUHRINDEX').AsInteger     := PrinterData.Zufuhrliste.Itemindex
        else
          ds.FieldByName('ZUFUHRINDEX').AsInteger     := -1;

        if PrinterData.DuplexListe <> nil then
          ds.FieldByName('DUPLEXINDEX').AsInteger     := PrinterData.DuplexListe.Itemindex
        else
          ds.FieldByName('DUPLEXINDEX').AsInteger     := -1;

        if PrinterData.SpinKopien  <> nil then
          ds.FieldByName('ANZAHLAUSDRUCKE').AsInteger := PrinterData.SpinKopien.Value
        else
          ds.FieldByName('ANZAHLAUSDRUCKE').AsInteger := -1;

        ds.FieldByName('DRUCKEN_KOPF').AsBoolean := PrinterData.DruckenKopf;
        ds.FieldByName('DRUCKEN_ADRESSÜBERSCHRIFT').AsBoolean := PrinterData.DruckenAdressueberschrift;
        ds.FieldByName('DRUCKEN_FUSS').AsBoolean := PrinterData.DruckenFuss;
        ds.FieldByName('RANDOBEN').AsFloat := PrinterData.RandOben;
        ds.FieldByName('RANDUNTEN').AsFloat := PrinterData.RandUnten;
        ds.FieldByName('KOPFSEITE2ANDERS').AsBoolean := PrinterData.Kopfseite2Anders;

        ds.FieldByName('AKTIV').AsBoolean := PrinterData.Active;

        ds.Post;
      finally
        FreeAndNil(ds);
      end;
      FConnection.CommitTrans;
    except
      FConnection.RollbackTrans;
      raise;
    end;
  end;

begin
  result := false;
  if FConnection = nil then
  begin
    WarnConnectionMissing;
    exit;
  end;
  _Go(1, FPrinterData);
  _Go(2, FPrinterDataKopien);
  result := true;
end;
function THsDruckerConfig.ReadPrinter: boolean;

  procedure _Go(DruckerNr: integer; PrinterData: TPrinterData);
  var
    iBuffer: integer;
    ds: TADODataSet;
  begin
    if PrinterData = nil then exit;

    PrinterData.Kopiendrucker := DruckerNr > 1;
    PrinterData.SetDefaults; // Alle Index, Schächte, usw. leeren

    // Werte auf DRUCKFORMEINST_POS laden und in PrinterData-Komponenten laden
    ds := TADODataSet.Create(nil);
    try
      ds.EnableBCD := false;
      ds.Connection := FConnection;
      ds.ParamCheck := false;
      ds.CommandText := 'select * from DRUCKFORMEINST_POS where FORMULARART = '''+FFormularArt+''' and FORMULARNR = '+IntToStr(FFormularNr)+' and DRUCKERART = '''+FDruckerArt+''' and LFD_DRUCKNR = '+IntToStr(DruckerNr)+' and BAP = '''+GetComputerName+''';'; // TODO: escaping (hl.System.Types gibt es hier noch nicht)
      ds.Active := true;
      ds.DisableControls;

      if ds.RecordCount > 0 then
      begin
        PrinterData.PrinterIndex := TPrinterData.CachedPrinterList.IndexOf(ds.FieldByName('DRUCKERNAME').AsString);

        if (PrinterData.PrinterIndex > -1) then   // DM 20.03.2024 : Das war auskommentiert. Warum? Hab die Zeile jetzt wieder aktiviert...
        begin
          if ds.FieldByName('ZUFUHRINDEX').IsNull then
            iBuffer := -1
          else
            iBuffer := ds.FieldByName('ZUFUHRINDEX').AsInteger;
          PrinterData.SchachtIndex := iBuffer;
          if PrinterData.Zufuhrliste <> nil then PrinterData.Zufuhrliste.Itemindex := iBuffer;
          if ds.FieldByName('DUPLEXINDEX').IsNull then
            iBuffer := -1
          else
            iBuffer := ds.FieldByName('DUPLEXINDEX').AsInteger;
          PrinterData.DuplexIndex := iBuffer;
          if PrinterData.DuplexListe <> nil then PrinterData.DuplexListe.Itemindex := iBuffer;
          if PrinterData.SpinKopien  <> nil then PrinterData.SpinKopien.Value      := ds.FieldByName('ANZAHLAUSDRUCKE').AsInteger;
        end;

        PrinterData.DruckenKopf := ds.FieldByName('DRUCKEN_KOPF').AsBoolean;
        PrinterData.DruckenAdressueberschrift := ds.FieldByName('DRUCKEN_ADRESSÜBERSCHRIFT').AsBoolean;
        PrinterData.DruckenFuss := ds.FieldByName('DRUCKEN_FUSS').AsBoolean;
        PrinterData.RandOben := ds.FieldByName('RANDOBEN').AsFloat;
        PrinterData.RandUnten := ds.FieldByName('RANDUNTEN').AsFloat;
        PrinterData.Kopfseite2Anders := ds.FieldByName('KOPFSEITE2ANDERS').AsBoolean;

        PrinterData.Active := ds.FieldByName('AKTIV').AsBoolean;
      end;
    finally
      FreeAndNil(ds);
    end;
  end;

begin
  result := false;
  if FConnection = nil then
  begin
    WarnConnectionMissing;
    exit;
  end;
  _Go(1, FPrinterData);
  _Go(2, FPrinterDataKopien);
  result := true;
end;

end.
