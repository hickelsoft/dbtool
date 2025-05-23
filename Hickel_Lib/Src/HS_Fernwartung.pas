unit HS_Fernwartung;

interface

procedure HS_FernwartungStarten;

implementation

uses
  Windows, SysUtils, ProgrDlg, hl.Utils, hl.Utils.Web, MessaBox, Classes,
  ShellAPI, StrUtils, Zip;

resourcestring
  StrFehlerBeimHerunter = 'Fehler beim Herunterladen der %s.';
  StrOnlinePräsentations = 'Online-Präsentations-Software';
  StrFernwartungsSoftwar = 'Fernwartungs-Software';
  StrSoftwareAktualisier = 'Software-Aktualisierung';
  StrSWirdAktualisiert = '%s wird aktualisiert. Bitte warten.';
  StrSWirdHeruntergela = '%s wird heruntergeladen. Bitte warten.';
  StrDateigrößeIstNicht = 'Dateigröße ist nicht plausibel.';
  StrBitteNochEinmalVe = 'Bitte noch einmal versuchen.';
  StrFehlerBeimEntpacke = 'Fehler beim Entpacken der Datei %s.';
  StrBitteInternetVerbi =
    'Bitte Internet-Verbindung prüfen und Programm nochmal neu starten.';
  StrGenaueFehlermeldung = 'Genaue Fehlermeldung: %s';

  // HickelSOFT customized RustDesk Download+Launch (without portable EXE)
procedure HS_FernwartungStarten;
const
  zipMaxAge = 30; // days
  // AdminModeDefault 0 = Run normally without UAC
  // AdminModeDefault 1 = Try to run as admin, otherwise run normally if UAC is denied
  // AdminModeDefault 2 = Require admin UAC (fail if UAC is denied)
  AdminModeDefault = 1;
var
  zipUrlSigned, zipUrlUnsigned: string;
  pgd: TProgressDlg;
  downloadedZip, downloadedExe: string;
  zipWasChanged: boolean;
  fehlerMeldung: string;
  AnwendungsName: string;
  a: AnsiChar;
  downloadOrdner: string;
  AdminMode: integer;
  tmp: string;
  Zip: TZipFile;
begin
  AdminMode := AdminModeDefault;

  if ParamStr(1) = '/setup' then
  begin
    // Backupwards compatibility. Old versions of CORA_Verwaltung.exe might call this.
    // Newer versions of CORA will repair the *.lnk file in hcl_Core.pas
    exit;
  end;

{$REGION 'Download-Ordner bestimmen'}
  if FileExists(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) +
    '..\config\Hickel.config.xml') then
  begin
    // Ins CORAplus\Zusatz Verzeichnis rein
    downloadOrdner := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))
      + '..\..\Zusatz');
  end
  else
  begin
    downloadOrdner := '';
    // Selbst wenn wir nicht von CORA ausgeführt wurden, dann versuchen wir trotzdem in den CORA-Ordner abzuspeichern,
    // denn C:\CORA2012 bzw. C:\CORAplus wird bei Neukunden im Defender ausgeschlossen. Daher ist das ein guter Ort!
    for a := 'C' to 'Z' do
    begin
      if DirectoryExists(a + ':\CORAplus\Zusatz') then
      begin
        downloadOrdner := a + ':\CORAplus\Zusatz';
        break;
      end
      else if DirectoryExists(a + ':\CORA2012\Zusatz') then
      begin
        downloadOrdner := a + ':\CORA2012\Zusatz';
        break;
      end;
    end;
  end;
  if downloadOrdner = '' then
  begin
    tmp := SysUtils.GetEnvironmentVariable('APPDATA');
    if tmp <> '' then
    begin
      tmp := IncludeTrailingPathDelimiter(tmp) + '..\Local\HickelSOFT';
      tmp := StringReplace(tmp, 'Roaming\..\', '', [rfIgnoreCase]);
      downloadOrdner := tmp;
    end
    else
    begin
      downloadOrdner := GetTempDir; // ansonsten das Temp-Verzeichnis
    end;
  end;
  downloadOrdner := IncludeTrailingPathDelimiter(downloadOrdner) +
    'Fernwartung';
  ForceDirectories(downloadOrdner);
{$ENDREGION}
  downloadedZip := downloadOrdner + '\' + 'rustdesk-hickelsoft-win' +
    IntToStr(WindowsBits) + '.zip';
  fehlerMeldung := '';

  if ContainsText(ExtractFileName(ParamStr(0)), 'Fernwartung') or
    ContainsText(ExtractFileName(ParamStr(0)), 'CORA_') or
    ContainsText(ExtractFileName(ParamStr(0)), 'HsInfo') or
    ContainsText(ExtractFileName(ParamStr(0)), 'DbTool') then
  begin
    AnwendungsName := StrFernwartungsSoftwar;
    // Achtung: Substantiv muss weiblichen Artikel haben, damit es zur Meldung unten passt
  end
  else if ContainsText(ExtractFileName(ParamStr(0)), 'Demo') or
    ContainsText(ExtractFileName(ParamStr(0)), 'Presentation') or
    ContainsText(ExtractFileName(ParamStr(0)), 'Präsentation') then
  begin
    AnwendungsName := StrOnlinePräsentations;
    // Achtung: Substantiv muss weiblichen Artikel haben, damit es zur Meldung unten passt
    AdminMode := 0;
    // Interessenten auf keinen Fall eine "Diese Anwendung möchte Änderungen an Ihrem PC vornehmen" Meldung zeigen!!!
  end
  else
  begin
    AnwendungsName := StrSoftwareAktualisier;
    // Achtung: Substantiv muss weiblichen Artikel haben, damit es zur Meldung unten passt
  end;

  zipWasChanged := false;

  try
    if not FileExists(downloadedZip) or (zipMaxAge <= 0) or
      (Now - GetFileModDate(downloadedZip) > zipMaxAge) then
    begin
      if WindowsBits = 32 then
      begin
        zipUrlSigned :=
          'https://www.hickelsoft.de/fernwartung/v3/rustdesk-hickelsoft-win32.zip';
        zipUrlUnsigned :=
          'https://github.com/hickelsoft/rustdesk/releases/download/nightly/rustdesk-hickelsoft-win32.zip';
      end
      else
      begin
        zipUrlSigned :=
          'https://www.hickelsoft.de/fernwartung/v3/rustdesk-hickelsoft-win64.zip';;
        zipUrlUnsigned :=
          'https://github.com/hickelsoft/rustdesk/releases/download/nightly/rustdesk-hickelsoft-win64.zip';
      end;

      pgd := TProgressDlg.Create(nil);
      try
        if FileExists(downloadedZip) then
          pgd.Text := Format(StrSWirdAktualisiert, [AnwendungsName])
        else
          pgd.Text := Format(StrSWirdHeruntergela, [AnwendungsName]);
        pgd.ShowStopButton := true;
        pgd.Open;
        try
          DeleteFile(PChar(downloadedZip + '.tmp'));
          try
            DownloadFile(zipUrlSigned, downloadedZip + '.tmp', pgd);
          except
            on E: EAbort do
            begin
              Abort;
            end;
            on E: Exception do
            begin
              DownloadFile(zipUrlUnsigned, downloadedZip + '.tmp', pgd);
            end;
          end;
          if ThlUtils.GetFileSize(downloadedZip + '.tmp') >= 1024 then
          begin
            DeleteFile(PChar(downloadedZip));
            MoveFile(PChar(downloadedZip + '.tmp'), PChar(downloadedZip));
            zipWasChanged := true;
          end
          else
          begin
            fehlerMeldung := Format(StrFehlerBeimHerunter, [AnwendungsName]) +
              ' ' + StrBitteNochEinmalVe + #13#10#13#10 + StrDateigrößeIstNicht;
          end;
        except
          on E: EAbort do
          begin
            Abort;
          end;
          on E: Exception do
          begin
            fehlerMeldung := Format(StrFehlerBeimHerunter, [AnwendungsName]) +
              ' ' + StrBitteNochEinmalVe + #13#10#13#10 + E.Message;
          end;
        end;
      finally
        FreeAndNil(pgd);
      end;
    end;
  finally
    // FileExists ist wichtig wegen dem Abort
    if FileExists(downloadedZip) then
    begin
      downloadedExe := ExtractFilePath(downloadedZip) + 'rustdesk\rustdesk.exe';

      if zipWasChanged or not FileExists(downloadedExe) then
      begin
        try
          Zip := TZipFile.Create;
          try
            Zip.Open(downloadedZip, zmRead);
            Zip.ExtractAll(ExtractFilePath(downloadedZip));
            Zip.Close;
          finally
            FreeAndNil(Zip);
          end;
        except
          on E: EAbort do
          begin
            Abort;
          end;
          on E: Exception do
          begin
            if FileExists(downloadedExe) then
            begin
              // Maybe a file is locked, because it is already running.
              // Don't throw an Exception and just hope that RustDesk starts!
            end
            else
            begin
              try
                DeleteFile(downloadedZip);
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
              raise Exception.Create(Format(StrFehlerBeimEntpacke,
                [downloadedZip]) + ' ' + StrBitteInternetVerbi + ' ' +
                Format(StrGenaueFehlermeldung, [E.Message]));
            end;
          end;
        end;
      end;

      if AdminMode = 0 then
      begin
        // AdminMode 0 = Run normally without UAC
        ShellExecute64(0, 'open', PChar(downloadedExe), '', '', SW_NORMAL);
      end
      else
      begin
        // AdminMode 1 = Try to run as admin, otherwise run normally if UAC is denied
        // AdminMode 2 = Require admin UAC (fail if UAC is denied)
        if ShellExecute64(0, 'runas', PChar(downloadedExe), '', '', SW_NORMAL) = SE_ERR_ACCESSDENIED
        then
        begin
          if AdminMode = 1 then
          begin
            ShellExecute64(0, 'open', PChar(downloadedExe), '', '', SW_NORMAL);
          end;
        end;
      end;
    end
    else if fehlerMeldung <> '' then
    begin
      raise Exception.Create(fehlerMeldung);
    end;
  end;
end;

end.
