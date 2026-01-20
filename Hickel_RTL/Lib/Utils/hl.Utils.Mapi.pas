unit hl.Utils.Mapi;

interface

type
  ThlEMail = class(TObject)
  private
    class function IsOutlook: boolean;
  protected
    class function SendEMailUsingOutlook(Subject, Body, FileName, SenderName,
      SenderEMail, Bcc, Cc, RecipientName, RecipientEMail: string)
      : Integer; virtual;
    class function SendEMailUsingMAPI(Subject, Body, FileName, SenderName,
      SenderEMail, Bcc, Cc, RecipientName, RecipientEMail: Ansistring)
      : Integer; virtual;
  public
    class function SendEMailWithAttachment(Subject, Body, FileName, SenderName,
      SenderEMail, Bcc, Cc, RecipientName, RecipientEMail: string): Integer;
  end;

implementation

uses
  SysUtils, Windows, Forms, Mapi, Registry, ComObj, hl.Utils.Web, Classes,
  MessaBox, hl.Utils, hl_Log;

resourcestring
  StrFehlerBeimSendenD = 'Fehler beim Senden der E-Mail (%d)';

class function ThlEMail.IsOutlook: boolean;
var
  reg: TRegistry;
  sDefaultMail: string;
begin
  result := false;

  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if reg.OpenKeyReadOnly('Software\HickelSOFT\CORAplus') then
    begin
      if reg.ReadString('NutzeOutlookSchnittstelle') = '1' then
      begin
        result := true;
        Exit;
      end;
      if reg.ReadString('NutzeOutlookSchnittstelle') = '0' then
      begin
        result := false;
        Exit;
      end;
      reg.CloseKey;
    end;

    reg.RootKey := HKEY_LOCAL_MACHINE;
    if reg.OpenKeyReadOnly('Software\HickelSOFT\CORAplus') then
    begin
      if reg.ReadString('NutzeOutlookSchnittstelle') = '1' then
      begin
        result := true;
        Exit;
      end;
      if reg.ReadString('NutzeOutlookSchnittstelle') = '0' then
      begin
        result := false;
        Exit;
      end;
    end;

    // Auskommentiert DM 20.03.2021: Das wird nicht aktualisiert von Windows! Ist ein Trugschluss!
    // Zurückgesetzt 04.06.2021, da es häufiger vorkommt, dass HKCU richtig(er) ist
    reg.RootKey := HKEY_CURRENT_USER;
    if reg.OpenKeyReadOnly('Software\Clients\Mail') then
    begin
      sDefaultMail := reg.ReadString('');
      if sDefaultMail = 'Microsoft Outlook' then
      begin
        result := true;
      end;
      reg.CloseKey;
      Exit;
    end;

    reg.RootKey := HKEY_LOCAL_MACHINE;
    if reg.OpenKeyReadOnly('Software\Clients\Mail') then
    begin
      sDefaultMail := reg.ReadString('');
      if sDefaultMail = 'Microsoft Outlook' then
      begin
        result := true;
      end;
      reg.CloseKey;
      Exit;
    end;

    result := false; // Standard = MAPI
  finally
    FreeAndNil(reg);
  end;
end;

class function ThlEMail.SendEMailUsingOutlook(Subject, Body, FileName,
  SenderName, SenderEMail, Bcc, Cc, RecipientName, RecipientEMail
  : string): Integer;
var
  // objWShell: OLEVariant;
  ol, newMail: OLEVariant;
  debug: TStringList;
const
  olFormatUnspecified = 0;
  olFormatPlain = 1;
  olFormatHTML = 2;
  olFormatRichText = 3;
  olMailItem = 0;
begin
  debug := TStringList.Create;
  ol := CreateOleObject('Outlook.Application');
  try
    newMail := ol.CreateItem(olMailItem);

    newMail.BodyFormat := olFormatHTML;
    // newMail.ItemProperties.Add('isCopy', 6).Value := 0;
    // newMail.PropertyAccessor.SetProperty('MailItem.PropertyAccessor.SetProperty', false);

    try
      newMail.Display;
    except
      // Ticket 36469: Outlook 2007 unterstützt den Befehl "Display" nicht. Die Mail geht automatisch auf.
      on E: EAbort do
      begin
        Abort;
      end;
      on E: Exception do
      begin
        // ignore
      end;
    end;

    newMail.Subject := Subject;

    newMail.To := RecipientEMail; // newMail.Recipents.Add(ToAddress);
    if Cc <> '' then
      newMail.Cc := Cc;
    if Bcc <> '' then
      newMail.Bcc := Bcc;

    newMail.Attachments.Add(FileName).Displayname :=
      ChangeFileExt(ExtractFileName(FileName), '');
    if not IsHTML(Body) then
    begin
      (*
        if ThclUtilsRtf.IsRtf(Body) then
        begin
        debug.Add('***BODY (IS RTF)***');
        debug.Add(body);

        try
        body := ThclUtilsRtf.RtfToHtml(body, true);
        except
        on E: EAbort do
        begin
        Abort;
        end;
        on E: Exception do
        begin
        if HsShowMessage('E-Mail-Textkörper ist beschädigt. Die E-Mail wird ohne Text erstellt. Fortfahren?', 'CORAplus', mbsExclamation, mbbYesNoCancel) <> idYes then
        Abort;
        end;
        end;
        end
        else
        begin
      *)
      debug.Add('***BODY (IS TEXT)***');
      debug.Add(Body);

      Body := TextToHtml(Body);
      (*
        end;
      *)

      debug.Add('***BODY CONVERTED TO HTML***');
      debug.Add(Body);
    end
    else
    begin
      debug.Add('***BODY (ALREADY HTML)***');
      debug.Add(Body);
    end;

    debug.Add('***EMAIL TEMPLATE HTML***');
    debug.Add(newMail.htmlbody);

    newMail.htmlbody := InsertHTMLAfterBody(Body, newMail.htmlbody);

    debug.Add('***EMAIL HTML INTEGRATION***');
    debug.Add(newMail.htmlbody);

    try
      newMail.Display;
    except
      // Ticket 36469: Outlook 2007 unterstützt den Befehl "Display" nicht. Die Mail geht automatisch auf.
      on E: EAbort do
      begin
        Abort;
      end;
      on E: Exception do
      begin
        // ignore
      end;
    end;

    debug.SaveToFile(ThlLog.DefaultLogDir + 'HTMLMail.log');

    result := 0;
  finally
    FreeAndNil(debug);
    VarClear(ol);
  end;
end;

// TODO: Auch Unicode anbieten : https://stackoverflow.com/questions/9943314/windows-mapi-unicode-issue
class function ThlEMail.SendEMailUsingMAPI(Subject, Body, FileName, SenderName,
  SenderEMail, Bcc, Cc, RecipientName, RecipientEMail: Ansistring): Integer;
var
  Message: TMapiMessage; // Das ist Ansi!
  lpSender, lpRecipient: TMapiRecipDesc;
  lpRecipients: array of TMapiRecipDesc;
  FileAttach: TMapiFileDesc;
  SM: TFNMapiSendMail;
  MAPIModule: HModule;
  FileType: TMapiFileTagExt;
  slAddr: TStringList;
begin
  // Funktion hier: http://www.stackoverflow.com/questions/1234623/how-to-send-a-mapi-email-with-an-attachment-to-a-fax-recipient
  // modifiziert

  // TODO: Bei David kommt gar keine Formatierung an!

  FillChar(Message, SizeOf(Message), 0);

  if (Subject <> '') then
  begin
    Message.lpszSubject := PAnsiChar(Subject);
  end;

  if (Body <> '') then
  begin
    if IsHTML(Body) then
    begin
      Body := HTMLToText(Body);
    end
    (*
      else if ThclUtilsRtf.IsRtf(Body) then
      begin
      body := ThclUtilsRtf.RtfToPlainText(body);
      end
    *)
    else
    begin
      // Otherwise assume it is Text => OK
    end;
    if (Body <> '') then
    begin
      Message.lpszNoteText := PAnsiChar(Body);
    end;
  end;

  if (SenderEMail <> '') then
  begin
    lpSender.ulRecipClass := MAPI_ORIG;
    if (SenderName = '') then
    begin
      lpSender.lpszName := PAnsiChar(SenderEMail);
    end
    else
    begin
      lpSender.lpszName := PAnsiChar(SenderName);
    end;
    lpSender.lpszAddress := PAnsiChar('smtp:' + SenderEMail);
    lpSender.ulReserved := 0;
    lpSender.ulEIDSize := 0;
    lpSender.lpEntryID := nil;
    Message.lpOriginator := @lpSender;
  end;

  if (RecipientEMail <> '') then
  begin
    SetLength(lpRecipients, Length(lpRecipients) + 1);
    lpRecipient.ulRecipClass := MAPI_TO;
    if (RecipientName = '') then
    begin
      lpRecipient.lpszName := PAnsiChar(RecipientEMail);
    end
    else
    begin
      lpRecipient.lpszName := PAnsiChar(RecipientName);
    end;
    lpRecipient.lpszAddress := PAnsiChar('smtp:' + RecipientEMail);
    lpRecipient.ulReserved := 0;
    lpRecipient.ulEIDSize := 0;
    lpRecipient.lpEntryID := nil;
    lpRecipients[Length(lpRecipients) - 1] := lpRecipient;
  end;

  slAddr := Split(Cc, ':');
  try
    for Cc in slAddr do
    begin
      SetLength(lpRecipients, Length(lpRecipients) + 1);
      lpRecipient.ulRecipClass := MAPI_CC;
      lpRecipient.lpszName := PAnsiChar(Cc);
      lpRecipient.lpszAddress := PAnsiChar('smtp:' + Cc);
      lpRecipient.ulReserved := 0;
      lpRecipient.ulEIDSize := 0;
      lpRecipient.lpEntryID := nil;
      lpRecipients[Length(lpRecipients) - 1] := lpRecipient;
    end;
  finally
    FreeAndNil(slAddr);
  end;

  slAddr := Split(Bcc, ':');
  try
    for Bcc in slAddr do
    begin
      SetLength(lpRecipients, Length(lpRecipients) + 1);
      lpRecipient.ulRecipClass := MAPI_BCC;
      lpRecipient.lpszName := PAnsiChar(Bcc);
      lpRecipient.lpszAddress := PAnsiChar('smtp:' + Bcc);
      lpRecipient.ulReserved := 0;
      lpRecipient.ulEIDSize := 0;
      lpRecipient.lpEntryID := nil;
      lpRecipients[Length(lpRecipients) - 1] := lpRecipient;
    end;
  finally
    FreeAndNil(slAddr);
  end;

  Message.nRecipCount := Length(lpRecipients);
  if Length(lpRecipients) = 0 then
    Message.lpRecips := nil
  else
    Message.lpRecips := @lpRecipients[0];

  if (FileName = '') then
  begin
    Message.nFileCount := 0;
    Message.lpFiles := nil;
  end
  else
  begin
    FillChar(FileAttach, SizeOf(FileAttach), 0);
    FileAttach.nPosition := Cardinal($FFFFFFFF);
    FileAttach.lpszPathName := PAnsiChar(FileName);

    FileType.ulReserved := 0;
    FileType.cbEncoding := 0;
    FileType.cbTag := 0;
    FileType.lpTag := nil;
    FileType.lpEncoding := nil;

    FileAttach.lpFileType := @FileType;
    Message.nFileCount := 1;
    Message.lpFiles := @FileAttach;
  end;

  MAPIModule := LoadLibrary(PChar(MAPIDLL));

  if MAPIModule = 0 then
  begin
    result := -1;
  end
  else
  begin
    try
      @SM := GetProcAddress(MAPIModule, 'MAPISendMail'); // Das ist Ansi!
      if @SM <> nil then
      begin
        result := SM(0, Application.Handle, Message, MAPI_DIALOG or
          MAPI_LOGON_UI, 0);
      end
      else
      begin
        result := 1;
      end;
    finally
      FreeLibrary(MAPIModule);
    end;
  end;

  if result <> 0 then
  begin
    raise Exception.CreateFmt(StrFehlerBeimSendenD, [result]);
  end;
end;

class function ThlEMail.SendEMailWithAttachment(Subject, Body, FileName,
  SenderName, SenderEMail, Bcc, Cc, RecipientName, RecipientEMail
  : string): Integer;
begin
  if IsOutlook then
    result := SendEMailUsingOutlook(Subject, Body, FileName, SenderName,
      SenderEMail, Bcc, Cc, RecipientName, RecipientEMail)
  else
    result := SendEMailUsingMAPI(Subject, Body, FileName, SenderName,
      SenderEMail, Bcc, Cc, RecipientName, RecipientEMail);
end;

end.
