unit HS_Fernwartung;

interface

// HickelSOFT customized RustDesk Download+Launch (without portable EXE)
procedure HS_FernwartungStarten;

implementation

uses
  Windows, SysUtils, ProgrDlg, hl.Utils, hl.Utils.Web, MessaBox, Classes,
  ShellAPI, StrUtils, Zip, HS_Auth;

{$REGION 'Lokalisierung'}
resourcestring
  StrFehlerBeimHerunter = 'Fehler beim Herunterladen von %s.';
  StrOnlinePräsentations = 'Online-Präsentations-Software';
  StrFernwartungsSoftwar = 'Fernwartungs-Software';
  StrRustDesk = 'RustDesk';
  StrSWirdAktualisiert = '%s wird aktualisiert. Bitte warten.';
  StrSWirdHeruntergela = '%s wird heruntergeladen. Bitte warten.';
  StrDateigrößeIstNicht = 'Dateigröße ist nicht plausibel.';
  StrBitteNochEinmalVe = 'Bitte noch einmal versuchen.';
  StrFehlerBeimEntpacke = 'Fehler beim Entpacken der Datei %s.';
  StrBitteInternetVerbi = 'Bitte Internet-Verbindung prüfen und Programm nochmal neu starten.';
  StrGenaueFehlermeldung = 'Genaue Fehlermeldung: %s';
  StrHMacFehler = 'HMAC stimmt nicht überein. Bitte Download nochmal versuchen.';
{$ENDREGION}

procedure HS_FernwartungStarten;
const
  // AdminModeDefault 0 = Run normally without UAC
  // AdminModeDefault 1 = Try to run as admin, otherwise run normally if UAC is denied
  // AdminModeDefault 2 = Require admin UAC (fail if UAC is denied)
  AdminModeDefault = 1;
var
  zipUrlSigned, zipUrlSignedHmac: string;
  pgd: TProgressDlg;
  downloadedZip, downloadedExe: string;
  zipWasChanged, istRustDeskUpdate, needsDownload: boolean;
  fehlerMeldung: string;
  AnwendungsName: string;
  a: AnsiChar;
  downloadOrdner: string;
  AdminMode: integer;
  tmp: string;
  Zip: TZipFile;
  ParamStrs: string;
  i: integer;
begin
  fehlerMeldung := '';
  zipWasChanged := false;
  AdminMode := AdminModeDefault;

  {$REGION 'Parameter prüfen'}
  istRustDeskUpdate := false;
  for i := 1 to ParamCount do
  begin
    if SameText(ParamStr(i), '/setup') then
    begin
      // Abwärtskompatibilität für sehr alte CORA-Versionen.
      // Da hat nämlich CORA_Verwaltung.exe die "Fernwartung.exe /Setup"
      // aufrufen, um die TeamViewer-Desktop-Verknüpfungen zu reparieren.
      // Späteren wurden die durch hcl_Core.pas direkt korrigiert.
      exit;
    end;
    if SameText(ParamStr(i), '--update') then
    begin
      // Aufgerufen von RustDesk beim Auto-Update-Button-Klick.
      // Oder durch das Auto-Update im System-Service, dann sind die Pfade wie folgt:
      // C:\WINDOWS\SystemTemp\rustdesk-1.4.3-x86_64.exe --update
      // C:\Windows\System32\config\systemprofile\AppData\Local\HickelSOFT\Fernwartung\Update-2025-10-22-21-29-01\rustdesk-hickelsoft-win64.zip.tmp
      istRustDeskUpdate := true;
      break;
    end;
  end;
  {$ENDREGION}

  {$REGION 'Download-Ordner, ZIP-Name und EXE-Name bestimmen'}
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
      // Local App Data für Nicht-CORA-Kunden verwenden
      tmp := IncludeTrailingPathDelimiter(tmp) + '..\Local\HickelSOFT';
      tmp := StringReplace(tmp, 'Roaming\..\', '', [rfIgnoreCase]);
      downloadOrdner := tmp;
    end
    else
    begin
      // Ansonsten das Temp-Verzeichnis, falls %AppData% nicht existiert
      downloadOrdner := IncludeTrailingPathDelimiter(GetTempDir) + 'HickelSOFT';
    end;
  end;

  downloadOrdner := IncludeTrailingPathDelimiter(downloadOrdner) +
    'Fernwartung';

  if istRustDeskUpdate then
  begin
    // Dadurch wird sichergestellt, dass der Unzip-Vorgang NIEMALS scheitert,
    // d.h. die RustDesk.exe, die installiert wird ist 100% neu und unversehrt
    downloadOrdner := IncludeTrailingPathDelimiter(downloadOrdner) + 'Update-' +
      FormatDateTime('yyyy-mm-dd-hh-nn-ss', Now);
  end;

  ForceDirectories(downloadOrdner);

  downloadedZip := IncludeTrailingPathDelimiter(downloadOrdner) +
    'rustdesk-hickelsoft-win' + IntToStr(WindowsBits) + '.zip';

  // Das Verzeichnis "rustdesk" kommt aus der ZIP-Datei!
  downloadedExe := IncludeTrailingPathDelimiter(downloadOrdner) + 'rustdesk' +
    PathDelim + 'rustdesk.exe';
  {$ENDREGION}

  {$REGION 'Anwendungsname und Admin-Modus bestimmen'}
  if ContainsText(ExtractFileName(ParamStr(0)), 'Fernwartung') or
    ContainsText(ExtractFileName(ParamStr(0)), 'CORA_') or
    ContainsText(ExtractFileName(ParamStr(0)), 'HsInfo') or
    ContainsText(ExtractFileName(ParamStr(0)), 'DbTool') then
  begin
    // Achtung: Substantiv muss weiblichen Artikel haben, damit es zur Meldung unten passt
    AnwendungsName := StrFernwartungsSoftwar;
  end
  else if ContainsText(ExtractFileName(ParamStr(0)), 'Demo') or
    ContainsText(ExtractFileName(ParamStr(0)), 'Presentation') or
    ContainsText(ExtractFileName(ParamStr(0)), 'Präsentation') then
  begin
    // Achtung: Substantiv muss weiblichen Artikel haben, damit es zur Meldung unten passt
    AnwendungsName := StrOnlinePräsentations;
    // Interessenten auf keinen Fall eine "Diese Anwendung möchte Änderungen an Ihrem PC vornehmen" Meldung zeigen!!!
    if not istRustDeskUpdate then
      AdminMode := 0;
  end
  else
  begin
    // Achtung: Substantiv muss weiblichen Artikel haben, damit es zur Meldung unten passt
    AnwendungsName := StrRustDesk;
  end;

  if IstHickelSoftTestPC and not istRustDeskUpdate then
  begin
    // Wenn wir RustDesk als Admin gestartet haben, kann HsInfo den rustdesk:// Link nicht öffnen
    // (da eine Nicht-Admin-Anwendung einer Admin-Anwendung kein WM_USER+2 Signal senden kann).
    // Deshalb normal starten, ohne UAC.
    AdminMode := 0;
  end;
  {$ENDREGION}

  try
    {$REGION 'Online-Datei bestimmen'}
    if WindowsBits = 32 then
    begin
      zipUrlSigned :=
        'https://www.hickelsoft.de/fernwartung/v3/rustdesk-hickelsoft-win32.zip';
      zipUrlSignedHmac :=
        'https://www.hickelsoft.de/fernwartung/v3/rustdesk-hickelsoft-win32.hmac';
    end
    else
    begin
      zipUrlSigned :=
        'https://www.hickelsoft.de/fernwartung/v3/rustdesk-hickelsoft-win64.zip';
      zipUrlSignedHmac :=
        'https://www.hickelsoft.de/fernwartung/v3/rustdesk-hickelsoft-win64.hmac';
    end;
    {$ENDREGION}

    {$REGION 'Prüfen, ob Herunterladen erforderlich ist'}
    try
      needsDownload := not FileExists(downloadedZip) or
                       (Trim(DoGet(zipUrlSignedHmac)) <> HashHmacFileSHA256Hex(downloadedZip, HS_ALUV4_HMAC_SECRET)); // in this case, use the HMAC to check "has the online file changed?"
    except
      needsDownload := true;
    end;
    {$ENDREGION}

    if needsDownload then
    begin
      {$REGION 'Herunterladen versuchen'}
      pgd := TProgressDlg.Create(nil);
      try
        if FileExists(downloadedZip) or istRustDeskUpdate then
          pgd.Text := Format(StrSWirdAktualisiert, [AnwendungsName])
        else
          pgd.Text := Format(StrSWirdHeruntergela, [AnwendungsName]);
        pgd.ShowStopButton := true;
        pgd.Open;
        try
          DeleteFile(PChar(downloadedZip + '.tmp'));
          DownloadFile(zipUrlSigned, downloadedZip + '.tmp', pgd);
          if HS_ALUV4_HMAC_SECRET <> '' then
          begin
            if Trim(DoGet(zipUrlSignedHmac)) <> HashHmacFileSHA256Hex(downloadedZip + '.tmp', HS_ALUV4_HMAC_SECRET) then
            begin
              DeleteFile(downloadedZip + '.tmp');
              raise Exception.Create(StrHMacFehler);
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
{$ENDREGION}
    end;
  finally
    // FileExists ist wichtig wegen dem Abort
    if FileExists(downloadedZip) then
    begin
      {$REGION 'Entpacken versuchen'}
      if istRustDeskUpdate or
         zipWasChanged or
         not FileExists(downloadedExe) then
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
            try
              // Let it download and extract again next time
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
            if FileExists(downloadedExe) then
            begin
              // Maybe a file is locked, because it is already running.
              // Don't throw an Exception and just hope that RustDesk starts!
            end
            else
            begin
              raise Exception.Create(Format(StrFehlerBeimEntpacke,
                [downloadedZip]) + ' ' + StrBitteInternetVerbi + ' ' +
                Format(StrGenaueFehlermeldung, [E.Message]));
            end;
          end;
        end;
      end;
      {$ENDREGION}

      {$REGION 'Aufrufen'}
      // Rustdesk Auto-Update downloads Fernwartung.exe and runs it as --update
      // we will pass through the paramter
      ParamStrs := '';
      for i := 1 to ParamCount do
      begin
        ParamStrs := ParamStrs + ' ' + ParamStr(i);
      end;

      if AdminMode = 0 then
      begin
        // AdminMode 0 = Run normally without UAC
        ShellExecute64(0, 'open', PChar(downloadedExe), PChar(Trim(ParamStrs)),
          '', SW_NORMAL);
      end
      else
      begin
        // AdminMode 1 = Try to run as admin, otherwise run normally if UAC is denied
        // AdminMode 2 = Require admin UAC (fail if UAC is denied)
        if ShellExecute64(0, 'runas', PChar(downloadedExe),
          PChar(Trim(ParamStrs)), '', SW_NORMAL) = SE_ERR_ACCESSDENIED then
        begin
          if AdminMode = 1 then
          begin
            ShellExecute64(0, 'open', PChar(downloadedExe),
              PChar(Trim(ParamStrs)), '', SW_NORMAL);
          end;
        end;
      end;
      {$ENDREGION}
    end
    else if fehlerMeldung <> '' then
    begin
      raise Exception.Create(fehlerMeldung);
    end;
  end;
end;

end.
