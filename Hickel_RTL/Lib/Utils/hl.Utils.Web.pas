unit hl.Utils.Web;

{$IFNDEF CONSOLE}
  {$IFNDEF NO_GUI}
    {$DEFINE CAN_USE_GUI_CODE}
  {$ENDIF}
{$ENDIF}

interface

uses
  Windows, Classes, IdSSLOpenSSL, IdComponent{$IFDEF CAN_USE_GUI_CODE}, ProgrDlg, Forms{$ENDIF};

function secure_email(email, linktext: string; crypt_linktext: boolean): string;
function htmlEntities(s: string): string;
function nl2br(s: string): string;
function InsertHTMLAfterBody(insert, htmlTemplate: string): string;
function IsHTML(s: string): boolean;
function HTMLToText(html: string): string;
function TextToHtml(text: string): string;
function DoPost(const URL: string; Params: TStringList): string;
function DoGet(const URL: string): string;
function EncodeURIComponent(const ASrc: string): string;
function EncodeURL(const URL: string): string;
procedure DownloadFile(const URL, filename: string; pgd: {$IFDEF CAN_USE_GUI_CODE}TProgressDlg{$ELSE}TObject{$ENDIF} = nil);

var
  hl_Web_UseIndy: boolean;

implementation

uses
  SysUtils, StrUtils, SHDocVW, MsHTML, Variants, ActiveX,
  IdSSLOpenSSLHeaders, IdHTTP, Math,
  WinInet, System.Net.URLClient, System.NetEncoding,
  hl.Utils;

resourcestring
  StrBitteJavaScriptEin =
    'Bitte JavaScript einschalten, um die E-Mail-Adresse anzuzeigen.';
  StrErrorTooManyRedi = 'Fehler: Zu viele Umleitungen';
  StrHTTPErrorDWithG = 'HTTP-Fehler %d bei GET-Anfrage %s';
  StrErrorOpeningReques = 'Fehler beim Öffnen der HTTP-Anfrage: %s';
  StrErrorInitializingW = 'Fehler beim Initialisieren von WinInet: %s';
  StrErrorConnectingTo = 'Fehler beim Verbinden zum Server: %s';
  StrErrorSendingReques = 'Fehler beim Senden der HTTP-Anfrage: %s';

function DummyUserAgent: string;

  function ChromeMajor: Integer;
  var
    BaseVersion: Integer;
    BaseDate: TDateTime;
    Days: Double;
  begin
    // https://chromestatus.com/roadmap
    BaseVersion := 149;
    BaseDate := EncodeDate(2026, 5, 20);

    Days := (Now - BaseDate);

    Result := BaseVersion + Floor(Days / 28);
  end;

begin
  result := 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/'+IntToStr(chromeMajor)+'.0.0.0 Safari/537.36';
end;

function secure_email(email, linktext: string; crypt_linktext: boolean): string;

  function alas_js_crypt(text: string): string;
  var
    i: integer;
  begin
    result := '';
    for i := 1 to Length(text) do
    begin
      result := result + 'document.write("&#' + IntToStr(ord(text[i])) + ';");';
    end;
  end;

  function alas_js_write(text: string): string;
  begin
    text := StringReplace(text, '\', '\\', [rfReplaceAll]);
    text := StringReplace(text, '"', '\"', [rfReplaceAll]);
    text := StringReplace(text, '/', '\/', [rfReplaceAll]);
    // W3C Validation </a> -> <\/a>
    result := 'document.write("' + text + '");';
  end;

var
  aus: string;
begin
  // siehe http://www.viathinksoft.de/tools/antispam/antispam_v3.inc.phps

  // No new lines to avoid a JavaScript error!
  linktext := StringReplace(linktext, #13, ' ', [rfReplaceAll]);
  linktext := StringReplace(linktext, #10, ' ', [rfReplaceAll]);

  aus := '';
  if email <> '' then
  begin
    aus := aus + '<script><!--' + #13;
    aus := aus + alas_js_write('<a href="');
    aus := aus + alas_js_crypt('mailto:' + email);
    aus := aus + alas_js_write('">');
    if crypt_linktext then
      aus := aus + alas_js_crypt(linktext)
    else
      aus := aus + alas_js_write(linktext);
    aus := aus + alas_js_write('</a>') + '// --></script>';
  end;

  result := aus + '<noscript>' + StrBitteJavaScriptEin + '</noscript>';
end;

function htmlEntities(s: string): string;
begin
  result := s;
  result := StringReplace(result, '&', '&amp;', [rfReplaceAll]);
  result := StringReplace(result, '<', '&lt;', [rfReplaceAll]);
  result := StringReplace(result, '>', '&gt;', [rfReplaceAll]);
  result := StringReplace(result, '"', '&quot;', [rfReplaceAll]);
end;

function nl2br(s: string): string;
begin
  result := s;
  result := StringReplace(result, #13, '<br>' + #13, [rfReplaceAll]);
end;

function InsertHTMLAfterBody(insert, htmlTemplate: string): string;
var
  p: integer;
begin
  p := Pos('<body', lowercase(htmlTemplate));
  if p > 0 then
  begin
    result := insert + htmlTemplate; // kein HTML
    Exit;
  end;

  p := PosEx('>', htmlTemplate, p + 1);
  if p > 0 then
  begin
    result := insert + htmlTemplate; // kein HTML
    Exit;
  end;

  result := Copy(htmlTemplate, 1, p) + insert + Copy(htmlTemplate, p + 1,
    Length(htmlTemplate) - p);
end;

function IsHTML(s: string): boolean;
begin
  s := lowercase(s);
  result := (Pos('<b>', s) >= 1) or (Pos('<i>', s) >= 1) or (Pos('<u>', s) >= 1)
    or (Pos('<img', s) >= 1) or (Pos('<font', s) >= 1) or (Pos('<div', s) >= 1)
    or (Pos('<span', s) >= 1) or (Pos('<table', s) >= 1) or (Pos('<a', s) >= 1)
    or (Pos('<p', s) >= 1);
end;

function HTMLToText(html: string): string;
var
  WebBrowser: TWebBrowser;
  Document: IHtmlDocument2;
  Doc: OleVariant;
  v: Variant;
  Body: IHTMLBodyElement;
  TextRange: IHTMLTxtRange;
begin
  // Quelle: http://www.prestwoodboards.com/ASPSuite/KB/document_view.asp?qid=100413

  result := '';
  WebBrowser := TWebBrowser.Create(nil);
  try
    Doc := 'about:blank';
    WebBrowser.Navigate2(Doc);
    Document := WebBrowser.Document as IHtmlDocument2;

    if (Assigned(Document)) then
    begin
      v := VarArrayCreate([0, 0], varVariant);
      v[0] := html;
      Document.Write(PSafeArray(TVarData(v).VArray));
      Document.Close;
      Body := Document.Body as IHTMLBodyElement;
      TextRange := Body.createTextRange;
      result := TextRange.text;
    end;
  finally
    FreeAndNil(WebBrowser);
  end;
end;

(*
  function HTMLToText(html: string): string;
  var
  P: PChar;
  InTag: Boolean;
  i, intResultLength: Integer;
  begin
  // Quelle: http://www.scalabium.com/faq/dct0162.htm
  // Problem: Zeilenumbrüche gehen verloren

  P := PChar(html);
  Result := '';

  InTag := False;
  repeat
  case P^ of
  '<': InTag := True;
  '>': InTag := False;
  #13, #10: ; {do nothing}
  else
  if not InTag then
  begin
  if (P^ in [#9, #32]) and ((P+1)^ in [#10, #13, #32, #9, '<']) then
  else
  Result := Result + P^;
  end;
  end;
  Inc(P);
  until (P^ = #0);

  {convert system characters}
  Result := StringReplace(Result, '&quot;', '"',  [rfReplaceAll]);
  Result := StringReplace(Result, '&apos;', '''', [rfReplaceAll]);
  Result := StringReplace(Result, '&gt;',   '>',  [rfReplaceAll]);
  Result := StringReplace(Result, '&lt;',   '<',  [rfReplaceAll]);
  Result := StringReplace(Result, '&amp;',  '&',  [rfReplaceAll]);
  {here you may add another symbols from RFC if you need}
  end;
*)

function TextToHtml(text: string): string;
begin
  result := nl2br(text);

  { convert system characters }
  result := StringReplace(result, '"', '&quot;', [rfReplaceAll]);
  result := StringReplace(result, '''', '&apos;', [rfReplaceAll]);
  result := StringReplace(result, '>', '&gt;', [rfReplaceAll]);
  result := StringReplace(result, '<', '&lt;', [rfReplaceAll]);
  result := StringReplace(result, '&', '&amp;', [rfReplaceAll]);
  { here you may add another symbols from RFC if you need }
end;

{$REGION 'Indy HTTP Get/Post/Download'}

function Indy_DoPost(const URL: string; Params: TStringList): string;
var
  Stream: TStringStream;
  lHTTP: TIdHTTP;
begin
  lHTTP := TIdHTTP.Create;
  try
    lHTTP.HandleRedirects := True;
    lHTTP.IOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(lHTTP);
    TIdSSLIOHandlerSocketOpenSSL(lHTTP.IOHandler).SSLOptions.SSLVersions :=
      [sslvTLSv1_2];
    TIdSSLIOHandlerSocketOpenSSL(lHTTP.IOHandler).SSLOptions.Method :=
      sslvTLSv1_2;

    Stream := TStringStream.Create('', TEncoding.UTF8);
    try
      begin
      var s: string := '';
      var i: Integer;
      for i := 0 to Params.Count - 1 do
      begin
        if i > 0 then
          s := s + '&';

        s := s +
          string(EncodeURIComponent(Params.Names[i])) + '=' +
          string(EncodeURIComponent(Params.ValueFromIndex[i]));
      end;

      var PostData := TStringStream.Create(s, TEncoding.UTF8);
      try
        lHTTP.Request.ContentType :=
          'application/x-www-form-urlencoded; charset=UTF-8';

        lHTTP.Request.UserAgent := DummyUserAgent;

        lHTTP.Post(EncodeURL(URL), PostData, Stream);
      finally
        PostData.Free;
      end;
    end;
      Stream.Position := 0;
      result := Stream.DataString;
    finally
      FreeAndNil(Stream);
    end;
  finally
    FreeAndNil(lHTTP);
  end;
end;

function Indy_DoGet(const URL: string): string;
var
  Stream: TStringStream;
  lHTTP: TIdHTTP;
begin
  lHTTP := TIdHTTP.Create;
  try
    lHTTP.HandleRedirects := True;
    lHTTP.IOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(lHTTP);
    TIdSSLIOHandlerSocketOpenSSL(lHTTP.IOHandler).SSLOptions.SSLVersions :=
      [sslvTLSv1_2];
    TIdSSLIOHandlerSocketOpenSSL(lHTTP.IOHandler).SSLOptions.Method :=
      sslvTLSv1_2;

    Stream := TStringStream.Create('', TEncoding.UTF8);
    try

      // https://stackoverflow.com/questions/53261747/indy10-connecttimeout-minimal-value
      lHTTP.ConnectTimeout := 60000; // 60s
      lHTTP.ReadTimeout := 60000; // 60s

      lHTTP.Request.UserAgent := DummyUserAgent;

      lHTTP.Get(EncodeURL(URL), Stream);
      Stream.Position := 0;
      result := Stream.DataString;
    finally
      FreeAndNil(Stream);
    end;
  finally
    FreeAndNil(lHTTP);
  end;
end;

type
  TTempDownloadProgress = class(TObject)
  public
    {$IFDEF CAN_USE_GUI_CODE}
    pgd: TProgressDlg;
    {$ENDIF}
    procedure DoWorkEnd(ASender: TObject; AWorkMode: TWorkMode);
    procedure DoWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
    procedure DoWorkBegin(ASender: TObject; AWorkMode: TWorkMode;
      AWorkCountMax: Int64);
  end;

procedure TTempDownloadProgress.DoWork(ASender: TObject; AWorkMode: TWorkMode;
  AWorkCount: Int64);
begin
  {$IFDEF CAN_USE_GUI_CODE}
  if pgd.StopButtonSignal then
  begin
    Abort;
  end;

  // In 50KB-Schritten vorangehen, weil die ProgressBar-Komponente nicht Int64 kann!
  pgd.Position := AWorkCount div (1024 * 50);
  {$ENDIF}
end;

procedure TTempDownloadProgress.DoWorkBegin(ASender: TObject;
  AWorkMode: TWorkMode; AWorkCountMax: Int64);
begin
  {$IFDEF CAN_USE_GUI_CODE}
  // In 50KB-Schritten vorangehen, weil die ProgressBar-Komponente nicht Int64 kann!
  pgd.MaxValue := AWorkCountMax div (1024 * 50);
  {$ENDIF}
end;

procedure TTempDownloadProgress.DoWorkEnd(ASender: TObject;
  AWorkMode: TWorkMode);
begin
  // Nichts hier
end;

procedure Indy_DownloadFile(const URL, filename: string;
  pgd: {$IFDEF CAN_USE_GUI_CODE}TProgressDlg{$ELSE}TObject{$ENDIF} = nil);
var
  IdHTTP1: TIdHTTP;
  Stream: TMemoryStream;
  tmpDownload: TTempDownloadProgress;
begin
  IdHTTP1 := TIdHTTP.Create(nil);
  Stream := TMemoryStream.Create;
  try
    IdHTTP1.HandleRedirects := True;
    IdHTTP1.IOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(IdHTTP1);
    TIdSSLIOHandlerSocketOpenSSL(IdHTTP1.IOHandler).SSLOptions.SSLVersions :=
      [sslvTLSv1_2];
    TIdSSLIOHandlerSocketOpenSSL(IdHTTP1.IOHandler).SSLOptions.Method :=
      sslvTLSv1_2;

    if Assigned(pgd) then
    begin
      tmpDownload := TTempDownloadProgress.Create;
      {$IFDEF CAN_USE_GUI_CODE}
      tmpDownload.pgd := pgd;
      {$ENDIF}

      IdHTTP1.OnWork := tmpDownload.DoWork;
      IdHTTP1.OnWorkBegin := tmpDownload.DoWorkBegin;
      IdHTTP1.OnWorkEnd := tmpDownload.DoWorkEnd;
    end
    else
      tmpDownload := nil;

    IdHTTP1.Request.UserAgent := DummyUserAgent;

    IdHTTP1.Get(EncodeURL(URL), Stream);

    if Assigned(tmpDownload) then
    begin
      FreeAndNil(tmpDownload);
    end;

    Stream.SaveToFile(filename);
  finally
    FreeAndNil(Stream);
    FreeAndNil(IdHTTP1);
  end;
end;

{$ENDREGION}
{$REGION 'WinInet HTTP Get/Post/Download'}

const
  MaxRedirects = 5;

function GetRedirectLocation(hRequest: HINTERNET): string;
var
  Buffer: array [0 .. 1023] of Char;
  BufferLength, HeaderIndex: DWORD;
begin
  result := '';
  BufferLength := SizeOf(Buffer);
  HeaderIndex := 0;

  // Query the "Location" header to get the new URL for redirection
  if HttpQueryInfo(hRequest, HTTP_QUERY_LOCATION, @Buffer, BufferLength,
    HeaderIndex) then
    result := string(Buffer);
end;

function GetStatusCode(hRequest: HINTERNET): DWORD;
var
  StatusCode: DWORD;
  StatusCodeLen: DWORD;
  HeaderIndex: DWORD;
begin
  StatusCode := 0;
  StatusCodeLen := SizeOf(StatusCode);
  HeaderIndex := 0;

  // Query the status code from the HTTP response
  if HttpQueryInfo(hRequest, HTTP_QUERY_STATUS_CODE or HTTP_QUERY_FLAG_NUMBER,
    @StatusCode, StatusCodeLen, HeaderIndex) then
    result := StatusCode
  else
    result := 0;
end;

function WinInet_DoPost(const URL: string; Params: TStringList): string;

  function StringListToPostData(Params: TStringList): UTF8String;
  var
    i: Integer;
    s: string;
  begin
    s := '';

    for i := 0 to Params.Count - 1 do
    begin
      if i > 0 then
        s := s + '&';

      s := s +
        string(EncodeURIComponent(Params.Names[i])) + '=' +
        string(EncodeURIComponent(Params.ValueFromIndex[i]));
    end;

    Result := UTF8Encode(s);
  end;

  procedure ExtractHostAndPath(
    const URL: string;
    var Host, Path: string;
    var Port: Integer
  );
  var
    URI: TURI;
  begin
    URI := TURI.Create(URL);

    Host := URI.Host;
    Path := URI.Path;

    if URI.Query <> '' then
      Path := Path + '?' + URI.Query;

    Port := URI.Port;
  end;

var
  AURL: string;

  hSession : HINTERNET;
  hConnect : HINTERNET;
  hRequest : HINTERNET;

  PostData : UTF8String;
  Headers  : string;
  Host     : string;
  Path     : string;
  Port     : Integer;

  PostDataLength : DWORD;

  Buffer    : array[0..1023] of Byte;
  BytesRead : DWORD;

  ResponseStream : TMemoryStream;
  UTF8Response   : UTF8String;

  StatusCode   : DWORD;
  RedirectCount: Integer;

begin
  Result := '';

  AURL := URL;

  hSession := InternetOpen(
    PChar(DummyUserAgent),
    INTERNET_OPEN_TYPE_PRECONFIG,
    nil,
    nil,
    0
  );

  if not Assigned(hSession) then
    raise Exception.CreateFmt(
      StrErrorInitializingW,
      [SysErrorMessage(GetLastError)]
    );

  try

    RedirectCount := 0;

    while True do
    begin

      {$IFDEF CAN_USE_GUI_CODE}
      if Assigned(Application) and Application.Terminated then
        Abort;
      {$ENDIF}

      ExtractHostAndPath(
        EncodeURL(AURL),
        Host,
        Path,
        Port
      );

      hConnect := InternetConnect(
        hSession,
        PChar(Host),
        Port,
        nil,
        nil,
        INTERNET_SERVICE_HTTP,
        0,
        0
      );

      if not Assigned(hConnect) then
        raise Exception.CreateFmt(
          StrErrorConnectingTo,
          [SysErrorMessage(GetLastError)]
        );

      try

        hRequest := HttpOpenRequest(
          hConnect,
          'POST',
          PChar(Path),
          nil,
          nil,
          nil,
          INTERNET_FLAG_SECURE,
          0
        );

        if not Assigned(hRequest) then
          raise Exception.CreateFmt(
            StrErrorOpeningReques,
            [SysErrorMessage(GetLastError)]
          );

        try

          Headers :=
            'Content-Type: application/x-www-form-urlencoded; charset=UTF-8'
            + #13#10;

          PostData := StringListToPostData(Params);

          PostDataLength := Length(PostData);

          if not HttpSendRequest(
            hRequest,
            PChar(Headers),
            Length(Headers),
            PAnsiChar(PostData),
            PostDataLength
          ) then
          begin
            raise Exception.CreateFmt(
              StrErrorSendingReques,
              [SysErrorMessage(GetLastError)]
            );
          end;

          StatusCode := GetStatusCode(hRequest);

          if (StatusCode >= 300) and (StatusCode < 400) then
          begin

            Inc(RedirectCount);

            if RedirectCount > MaxRedirects then
              raise Exception.Create(StrErrorTooManyRedi);

            AURL := GetRedirectLocation(hRequest);
          end
          else
          if StatusCode = 200 then
          begin

            ResponseStream := TMemoryStream.Create;
            try

              repeat

                BytesRead := 0;

                InternetReadFile(
                  hRequest,
                  @Buffer,
                  SizeOf(Buffer),
                  BytesRead
                );

                if BytesRead > 0 then
                  ResponseStream.WriteBuffer(Buffer, BytesRead);

                {$IFDEF CAN_USE_GUI_CODE}
                if Assigned(Application) and Application.Terminated then
                  Abort;
                {$ENDIF}

              until BytesRead = 0;

              SetString(
                UTF8Response,
                PAnsiChar(ResponseStream.Memory),
                ResponseStream.Size
              );

              Result := UTF8ToString(UTF8Response);

            finally
              ResponseStream.Free;
            end;

            Break;
          end
          else
          begin
            raise Exception.CreateFmt(
              StrHTTPErrorDWithG,
              [StatusCode, AURL]
            );
          end;

        finally
          InternetCloseHandle(hRequest);
        end;

      finally
        InternetCloseHandle(hConnect);
      end;
    end;

  finally
    InternetCloseHandle(hSession);
  end;
end;

function WinInet_DoGet(const AURL: string): string;
var
  hSession : HINTERNET;
  hURL     : HINTERNET;
  dwread   : DWORD;
  databuffer : array[0..1023] of Byte;

  ResponseStream : TMemoryStream;
  UTF8Response   : UTF8String;
begin
  Result := '';

  hSession := InternetOpen(
    PChar(DummyUserAgent),
    INTERNET_OPEN_TYPE_PRECONFIG,
    nil,
    nil,
    0
  );

  if hSession = nil then
    Exit;

  try

    hURL := InternetOpenUrl(
      hSession,
      PChar(EncodeURL(AURL)),
      nil,
      0,
      INTERNET_FLAG_RELOAD,
      0
    );

    if hURL = nil then
      Exit;

    try

      ResponseStream := TMemoryStream.Create;
      try

        repeat

          dwread := 0;

          InternetReadFile(
            hURL,
            @databuffer,
            SizeOf(databuffer),
            dwread
          );

          if dwread > 0 then
            ResponseStream.WriteBuffer(databuffer, dwread);

        until dwread = 0;

        SetString(
          UTF8Response,
          PAnsiChar(ResponseStream.Memory),
          ResponseStream.Size
        );

        Result := UTF8ToString(UTF8Response);

      finally
        ResponseStream.Free;
      end;

    finally
      InternetCloseHandle(hURL);
    end;

  finally
    InternetCloseHandle(hSession);
  end;
end;

procedure WinInet_DownloadFile(const URL, filename: string;
  pgd: {$IFDEF CAN_USE_GUI_CODE}TProgressDlg{$ELSE}TObject{$ENDIF} = nil);
var
  AURL: string;
  hSession, hRequest: HINTERNET;
  Buffer: array [0 .. 1023] of Byte;
  BufferLen: DWORD;
  FileStream: TFileStream;
  FileSize, TotalRead: DWORD;
  dwSize: DWORD;
  reserved: DWORD;
  StatusCode: DWORD;
  RedirectCount: integer;
begin
  AURL := URL;

  hSession := InternetOpen(PChar(DummyUserAgent), INTERNET_OPEN_TYPE_PRECONFIG,
    nil, nil, 0);
  if not Assigned(hSession) then
    raise Exception.CreateFmt(StrErrorInitializingW,
      [SysErrorMessage(GetLastError)]);
  try
    RedirectCount := 0;
    while True do
    begin
      {$IFDEF CAN_USE_GUI_CODE}
      if Assigned(Application) and Application.Terminated then
        Abort;
      {$ENDIF}
      hRequest := InternetOpenUrl(hSession, PChar(EncodeURL(AURL)), nil, 0,
        INTERNET_FLAG_RELOAD or INTERNET_FLAG_NO_CACHE_WRITE, 0);
      if not Assigned(hRequest) then
        raise Exception.CreateFmt(StrErrorOpeningReques,
          [SysErrorMessage(GetLastError)]);
      try
        StatusCode := GetStatusCode(hRequest);
        if (StatusCode >= 300) and (StatusCode < 400) then
        begin
          Inc(RedirectCount);

          // Stop following redirects if we exceed the maximum number of allowed redirects.
          if RedirectCount > MaxRedirects then
          begin
            raise Exception.Create(StrErrorTooManyRedi);
          end;

          // Get the "Location" header for the new URL
          AURL := GetRedirectLocation(hRequest);
        end
        else if (StatusCode = 200) then
        begin
          dwSize := SizeOf(FileSize);
          reserved := 0;
          if pgd <> nil then
          begin
            {$IFDEF CAN_USE_GUI_CODE}
            if HttpQueryInfo(hRequest, HTTP_QUERY_CONTENT_LENGTH or
              HTTP_QUERY_FLAG_NUMBER, @FileSize, dwSize, reserved) then
              pgd.MaxValue := FileSize div 1024 // Number of KiB
            else
              pgd.MaxValue := 0;
            {$ENDIF}
          end;

          FileStream := TFileStream.Create(filename, fmCreate);
          try
            TotalRead := 0;
            repeat
              // Lese Daten von der URL
              InternetReadFile(hRequest, @Buffer, SizeOf(Buffer), BufferLen);
              if BufferLen > 0 then
              begin
                FileStream.Write(Buffer, BufferLen);
                TotalRead := TotalRead + BufferLen;
                {$IFDEF CAN_USE_GUI_CODE}
                if pgd <> nil then
                begin
                  pgd.Position := TotalRead div 1024;
                  if pgd.StopButtonSignal then
                    Abort;
                end;
                {$ENDIF}
              end;
              {$IFDEF CAN_USE_GUI_CODE}
              if Assigned(Application) and Application.Terminated then
                Abort;
              {$ENDIF}
            until BufferLen = 0;
          finally
            FreeAndNil(FileStream);
          end;

          break;
        end
        else
          raise Exception.CreateFmt(StrHTTPErrorDWithG, [StatusCode, AURL]);
      finally
        InternetCloseHandle(hRequest);
      end;
    end;
  finally
    InternetCloseHandle(hSession);
  end;
end;

{$ENDREGION}

procedure CopyOpenSslLibs;
var
  // ResStream: TResourceStream;
  bits: integer;
  outfil: string;
  outdir: string;
  counter: integer;
begin
  hl_Web_UseIndy := false;

  // Neuste Version der Indy SSL Libraries hier herunterladen: https://github.com/IndySockets/OpenSSL-Binaries

{$IFDEF WIN64}
  bits := 64;
{$ELSE}
  bits := 32;
{$ENDIF}

  counter := 0;

  outdir := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'Win' + IntToStr(bits);

  outfil := IncludeTrailingPathDelimiter(outdir) + 'libeay32.dll';
  if FileExists(outfil) then
  begin
    Inc(counter);
  end
  else if FileExists(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) +
      'libeay32.' + IntToStr(bits) + '.dll') then
  begin
    ForceDirectories(outdir);
    if CopyFile(PChar(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) +
      'libeay32.' + IntToStr(bits) + '.dll'), PChar(outfil), false) then
    begin
      Inc(counter);
    end;
  end;

  outfil := IncludeTrailingPathDelimiter(outdir) + 'ssleay32.dll';
  if FileExists(outfil) then
  begin
    Inc(counter);
  end
  else if FileExists(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) +
      'ssleay32.' + IntToStr(bits) + '.dll') then
  begin
    ForceDirectories(outdir);
    if CopyFile(PChar(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) +
      'ssleay32.' + IntToStr(bits) + '.dll'), PChar(outfil), false) then
    begin
      Inc(counter);
    end;
  end;

  if counter = 2 then
  begin
    IdOpenSSLSetLibPath(outdir);
    hl_Web_UseIndy := True;
  end;
end;

function DoPost(const URL: string; Params: TStringList): string;
begin
  if hl_Web_UseIndy then
    result := Indy_DoPost(URL, Params)
  else
    result := WinInet_DoPost(URL, Params);
end;

function DoGet(const URL: string): string;
begin
  if hl_Web_UseIndy then
    result := Indy_DoGet(URL)
  else
    result := WinInet_DoGet(URL);
end;

procedure DownloadFile(const URL, filename: string; pgd: {$IFDEF CAN_USE_GUI_CODE}TProgressDlg{$ELSE}TObject{$ENDIF} = nil);
begin
  if hl_Web_UseIndy then
    Indy_DownloadFile(URL, filename, pgd)
  else
    WinInet_DownloadFile(URL, filename, pgd);
end;

// https://marc.durdin.net/2012/07/indy-tiduri-pathencode-urlencode-and-paramsencode-and-more/
function EncodeURIComponent(const ASrc: string): string;
const
  HexMap: string = '0123456789ABCDEF';

  function IsSafeChar(ch: integer): boolean;
  begin
    if (ch >= 48) and (ch <= 57) then
      result := True // 0-9
    else if (ch >= 65) and (ch <= 90) then
      result := True // A-Z
    else if (ch >= 97) and (ch <= 122) then
      result := True // a-z
    else if (ch = 33) then
      result := True // !
    else if (ch >= 39) and (ch <= 42) then
      result := True // '()*
    else if (ch >= 45) and (ch <= 46) then
      result := True // -.
    else if (ch = 95) then
      result := True // _
    else if (ch = 126) then
      result := True // ~
    else
      result := false;
  end;

var
  i, J: integer;
  ASrcUTF8: Utf8String;
begin
  result := ''; { Do not Localize }

  ASrcUTF8 := UTF8Encode(ASrc);
  // UTF8Encode call not strictly necessary but
  // prevents implicit conversion warning

  i := 1;
  J := 1;
  SetLength(result, Length(ASrcUTF8) * 3); // space to %xx encode every byte
  while i <= Length(ASrcUTF8) do
  begin
    if IsSafeChar(ord(ASrcUTF8[i])) then
    begin
      result[J] := Char(ASrcUTF8[i]);
      Inc(J);
    end
    else
    begin
      result[J] := '%';
      result[J + 1] := HexMap[(ord(ASrcUTF8[i]) shr 4) + 1];
      result[J + 2] := HexMap[(ord(ASrcUTF8[i]) and 15) + 1];
      Inc(J, 3);
    end;
    Inc(i);
  end;

  SetLength(result, J - 1);
end;

function EncodeURL(const URL: string): string;
var
  p: Integer;
  BaseURL, Query, Pair, EncodedQuery: string;
  SL: TStringList;
begin
  p := Pos('?', URL);

  if p = 0 then
    Exit(URL);

  BaseURL := Copy(URL, 1, p - 1);
  Query := Copy(URL, p + 1, MaxInt);

  SL := TStringList.Create;
  try
    SL.StrictDelimiter := True;
    SL.Delimiter := '&';
    SL.DelimitedText := Query;

    EncodedQuery := '';

    for p := 0 to SL.Count - 1 do
    begin
      Pair := SL[p];

      if EncodedQuery <> '' then
        EncodedQuery := EncodedQuery + '&';

      if Pos('=', Pair) > 0 then
      begin
        EncodedQuery := EncodedQuery +
          EncodeURIComponent(Copy(Pair, 1, Pos('=', Pair)-1)) + '=' +
          EncodeURIComponent(Copy(Pair, Pos('=', Pair)+1, MaxInt));
      end
      else
      begin
        EncodedQuery := EncodedQuery + EncodeURIComponent(Pair);
      end;
    end;

    Result := BaseURL + '?' + EncodedQuery;
  finally
    SL.Free;
  end;
end;

initialization

CopyOpenSslLibs;

end.