unit hl.Utils.Web;

// TODO: GET/POST Unicode implementations seem to be off
//       GET Indy     : Response as UTF8, GET Parameters are sent as '?'
//       GET WinInet  : Response as UTF8, GET Parameters are sent as ANSI
//       POST Indy    : Response as UTF8, GET Parameters are sent as '?',  POST parameter as sent as UTF8
//       POST WinInet : Response as UTF8, GET Parameters are sent as UTF8, POST parameter as sent as UTF8

interface

uses
  Windows, Classes, IdSSLOpenSSL, IdComponent, ProgrDlg;

function secure_email(email, linktext: string; crypt_linktext: boolean): string;
function htmlEntities(s: string): string;
function nl2br(s: string): string;
function InsertHTMLAfterBody(insert, htmlTemplate: string): string;
function IsHTML(s: string): boolean;
function HTMLToText(html: string): string;
function TextToHtml(text: string): string;
function DoPost(const URL: string; Params: TStringList): string;
function DoGet(const URL: string): string;
function EncodeURIComponent(const ASrc: string): UTF8String;
procedure DownloadFile(const url, filename: string; pgd: TProgressDlg=nil);

var
  hl_Web_UseIndy: boolean;

implementation

uses
  SysUtils, StrUtils, SHDocVW, MsHTML, Variants, ActiveX,
  IdSSLOpenSSLHeaders, IdHTTP,
  WinInet, System.Net.URLClient, System.NetEncoding,
  hl.Utils;

function secure_email(email, linktext: string; crypt_linktext: boolean): string;
  function alas_js_crypt(text: string): string;
  var
    i: integer;
  begin
    result := '';
    for i := 1 to Length(text) do
    begin
      result := result + 'document.write("&#'+IntToStr(ord(text[i]))+';");';
    end;
  end;
  function alas_js_write(text: string): string;
  begin
    text := StringReplace(text, '\', '\\', [rfReplaceAll]);
    text := StringReplace(text, '"', '\"', [rfReplaceAll]);
    text := StringReplace(text, '/', '\/', [rfReplaceAll]); // W3C Validation </a> -> <\/a>
    result := 'document.write("'+text+'");';
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
		aus := aus + '<script><!--'+#13;
		aus := aus + alas_js_write('<a href="');
		aus := aus + alas_js_crypt('mailto:'+email);
		aus := aus + alas_js_write('">');
    if crypt_linktext then
      aus := aus + alas_js_crypt(linktext)
    else
      aus := aus + alas_js_write(linktext);
		aus := aus + alas_js_write('</a>')+'// --></script>';
	end;

	result := aus+'<noscript>Bitte JavaScript einschalten, um die E-Mail-Adresse anzuzeigen.</noscript>';
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
  result := StringReplace(result, #13, '<br>'+#13, [rfReplaceAll]);
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

  p := PosEx('>', htmlTemplate, p+1);
  if p > 0 then
  begin
    result := insert + htmlTemplate; // kein HTML
    Exit;
  end;

  result := Copy(htmlTemplate, 1, p) + insert + Copy(htmlTemplate, p+1, Length(htmlTemplate)-p);
end;

function IsHTML(s: string): boolean;
begin
  s := LowerCase(s);
  result := (Pos('<b>', s) >= 1) or
            (Pos('<i>', s) >= 1) or
            (Pos('<u>', s) >= 1) or
            (Pos('<img', s) >= 1) or
            (Pos('<font', s) >= 1) or
            (Pos('<div', s) >= 1) or
            (Pos('<span', s) >= 1) or
            (Pos('<table', s) >= 1) or
            (Pos('<a', s) >= 1) or
            (Pos('<p', s) >= 1);
end;

function HtmlToText(html: string): string;
var
  WebBrowser: TWebBrowser;
  Document: IHtmlDocument2;
  Doc: OleVariant;
  v: Variant;
  Body: IHTMLBodyElement;
  TextRange: IHTMLTxtRange;
begin
  // Quelle: http://www.prestwoodboards.com/ASPSuite/KB/document_view.asp?qid=100413

  Result := '';
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
      Body := Document.body as IHTMLBodyElement;
      TextRange := Body.createTextRange;
      Result := TextRange.text;
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

  {convert system characters}
  Result := StringReplace(Result, '"', '&quot;',  [rfReplaceAll]);
  Result := StringReplace(Result, '''', '&apos;', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '&gt;',    [rfReplaceAll]);
  Result := StringReplace(Result, '<', '&lt;',    [rfReplaceAll]);
  Result := StringReplace(Result, '&', '&amp;',   [rfReplaceAll]);
  {here you may add another symbols from RFC if you need}
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
    TIdSSLIOHandlerSocketOpenSSL(lHTTP.IOHandler).SSLOptions.SSLVersions := [sslvTLSv1_2];
    TIdSSLIOHandlerSocketOpenSSL(lHTTP.IOHandler).SSLOptions.Method := sslvTLSv1_2;

    // Ich bekomme es nicht hin mit TIdEncoding, es kommt immer "h?f" anstelle "häf" raus
    // Mit Stream gehts...
    Stream:= TStringStream.Create('');
    try
      lHTTP.Post(URL, Params, Stream);
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
    TIdSSLIOHandlerSocketOpenSSL(lHTTP.IOHandler).SSLOptions.SSLVersions := [sslvTLSv1_2];
    TIdSSLIOHandlerSocketOpenSSL(lHTTP.IOHandler).SSLOptions.Method := sslvTLSv1_2;

    // Ich bekomme es nicht hin mit TIdEncoding, es kommt immer "h?f" anstelle "häf" raus
    // Mit Stream gehts...
    Stream:= TStringStream.Create('');
    try

      // https://stackoverflow.com/questions/53261747/indy10-connecttimeout-minimal-value
      lHTTP.ConnectTimeout := 60000; // 60s
      lHTTP.ReadTimeout := 60000; // 60s

      lHTTP.Get(URL, Stream);
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
    pgd: TProgressDlg;
    procedure DoWorkEnd(ASender: TObject; AWorkMode: TWorkMode);
    procedure DoWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
    procedure DoWorkBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
  end;

procedure TTempDownloadProgress.DoWork(ASender: TObject; AWorkMode: TWorkMode;
  AWorkCount: Int64);
begin
  if pgd.StopButtonSignal then
  begin
    Abort;
  end;

  // In 50KB-Schritten vorangehen, weil die ProgressBar-Komponente nicht Int64 kann!
  pgd.Position := aworkcount div (1024*50);
end;

procedure TTempDownloadProgress.DoWorkBegin(ASender: TObject; AWorkMode: TWorkMode;
  AWorkCountMax: Int64);
begin
  // In 50KB-Schritten vorangehen, weil die ProgressBar-Komponente nicht Int64 kann!
  pgd.MaxValue := AWorkCountMax div (1024*50);
end;

procedure TTempDownloadProgress.DoWorkEnd(ASender: TObject; AWorkMode: TWorkMode);
begin
  // Nichts hier
end;

procedure Indy_DownloadFile(const url, filename: string; pgd: TProgressDlg=nil);
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
    TIdSSLIOHandlerSocketOpenSSL(IdHTTP1.IOHandler).SSLOptions.SSLVersions := [sslvTLSv1_2];
    TIdSSLIOHandlerSocketOpenSSL(IdHTTP1.IOHandler).SSLOptions.Method := sslvTLSv1_2;

    if Assigned(pgd) then
    begin
      tmpDownload := TTempDownloadProgress.Create;
      tmpDownload.pgd := pgd;

      IdHTTP1.OnWork := tmpDownload.DoWork;
      IdHTTP1.OnWorkBegin := tmpDownload.DoWorkBegin;
      IdHTTP1.OnWorkEnd := tmpDownload.DoWorkEnd;
    end;

    IdHTTP1.Get(Url, Stream);

    if Assigned(tmpDownload) then
    begin
      FreeAndNil(tmpDownload);
    end;

    Stream.SaveToFile(FileName);
  finally
    FreeAndNil(Stream);
    FreeAndNil(IdHTTP1);
  end;
end;

{$ENDREGION}

{$REGION 'WinInet HTTP Get/Post/Download'}

const
  USER_AGENT = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/128.0.0.0 Safari/537.36';
  MaxRedirects = 5;

function GetRedirectLocation(hRequest: HINTERNET): string;
var
  Buffer: array[0..1023] of Char;
  BufferLength, HeaderIndex: DWORD;
begin
  Result := '';
  BufferLength := SizeOf(Buffer);
  HeaderIndex := 0;

  // Query the "Location" header to get the new URL for redirection
  if HttpQueryInfo(hRequest, HTTP_QUERY_LOCATION, @Buffer, BufferLength, HeaderIndex) then
    Result := string(Buffer);
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
  if HttpQueryInfo(hRequest, HTTP_QUERY_STATUS_CODE or HTTP_QUERY_FLAG_NUMBER, @StatusCode, StatusCodeLen, HeaderIndex) then
    Result := StatusCode
  else
    Result := 0;
end;

function WinInet_DoPost(const URL: string; Params: TStringList): string;

  function StringListToPostData(Params: TStringList): RawByteString;
  var
    i: Integer;
  begin
    Result := '';
    for i := 0 to Params.Count - 1 do
    begin
      if i > 0 then Result := Result + '&';
      Result := Result + TNetEncoding.URL.Encode(Params.KeyNames[i]) + '=' + TNetEncoding.URL.Encode(Params.ValueFromIndex[i]);
    end;
  end;

  procedure ExtractHostAndPath(const URL: string; var Host, Path: string; var Port: integer);
  var
    URI: TURI;
  begin
    URI := TURI.Create(URL);
    Host := URI.Host;
    Path := URI.Path;
    if URI.Query <> '' then Path := Path + '?' + URI.Query;
    Port := URI.Port;
  end;

var
  AURL: string;
  hSession, hConnect, hRequest: HINTERNET;
  PostData: RawByteString; // sic!!!
  Headers, Host, Path: string;
  Port: integer;
  PostDataLength: DWORD;
  Buffer: array[0..1024] of AnsiChar;
  BytesRead: DWORD;
  Response: RawByteString;
  StatusCode: DWORD;
  RedirectCount: integer;
const
  ContentType = 'application/x-www-form-urlencoded';
begin
  AUrl := URL;

  hSession := InternetOpen(USER_AGENT, INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
  if not Assigned(hSession) then
    raise Exception.Create('Error initializing WinInet: ' + SysErrorMessage(GetLastError));
  try
    RedirectCount := 0;
    while true do
    begin
      ExtractHostAndPath(AURL, Host, Path, Port);
      hConnect := InternetConnect(hSession, PChar(Host), Port, nil, nil, INTERNET_SERVICE_HTTP, 0, 0);
      if not Assigned(hConnect) then
        raise Exception.Create('Error connecting to server: ' + SysErrorMessage(GetLastError));
      try
        hRequest := HttpOpenRequest(hConnect, 'POST', PChar(Path), nil, nil, nil, INTERNET_FLAG_SECURE, 0);
        if not Assigned(hRequest) then
          raise Exception.Create('Error opening request: ' + SysErrorMessage(GetLastError));
        try
          // Set headers.
          Headers := 'Content-Type: ' + ContentType + #13#10;

          // Convert the StringList parameters to a URL-encoded string.
          PostData := StringListToPostData(Params);
          PostDataLength := Length(PostData);

          // Send the request with POST data.
          if not HttpSendRequest(hRequest, PChar(Headers), Length(Headers), PAnsiChar(PostData), PostDataLength) then
            raise Exception.Create('Error sending request: ' + SysErrorMessage(GetLastError));

          StatusCode := GetStatusCode(hRequest);

          if (StatusCode >= 300) and (StatusCode < 400) then
          begin
            Inc(RedirectCount);

            // Stop following redirects if we exceed the maximum number of allowed redirects.
            if RedirectCount > MaxRedirects then
            begin
              raise Exception.Create('Error: Too many redirects');
            end;

            // Get the "Location" header for the new URL
            AURL := GetRedirectLocation(hRequest);
          end
          else if (StatusCode = 200) then // do not localize
          begin
            // Read the response.
            Response := '';
            repeat
              InternetReadFile(hRequest, @Buffer, SizeOf(Buffer), BytesRead);
              if BytesRead > 0 then
                Response := Response + Copy(Buffer, 1, BytesRead);
            until BytesRead = 0;

            // Output the server response.
            Result := Response;

            break;
          end
          else
            raise Exception.CreateFmt('HTTP Error %d with GET request %s', [StatusCode, aurl]);
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

function WinInet_DoGet(const Url: string): string;
var
  AUrl: string;
  databuffer : array[0..4095] of ansichar; // SIC! ansichar!
  Response : ansistring; // SIC! ansistring
  hSession, hRequest: hInternet;
  dwread,dwNumber: cardinal;
  Str    : pansichar; // SIC! pansichar
  StatusCode: DWORD;
  RedirectCount: integer;
begin
  Response:='';
  AUrl := Url;

  hSession:=InternetOpen(USER_AGENT, INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
  if not Assigned(hSession) then
    raise Exception.Create('Error initializing WinInet: ' + SysErrorMessage(GetLastError));
  try
    RedirectCount := 0;
    while true do
    begin
      hRequest:=InternetOpenUrl(hsession, pchar(AUrl), nil, 0, INTERNET_FLAG_RELOAD, 0);
      if not Assigned(hRequest) then
        raise Exception.Create('Error opening request: ' + SysErrorMessage(GetLastError));
      try
        StatusCode := GetStatusCode(hRequest);
        if (StatusCode >= 300) and (StatusCode < 400) then
        begin
          Inc(RedirectCount);

          // Stop following redirects if we exceed the maximum number of allowed redirects.
          if RedirectCount > MaxRedirects then
          begin
            raise Exception.Create('Error: Too many redirects');
          end;

          // Get the "Location" header for the new URL
          AURL := GetRedirectLocation(hRequest);
        end
        else if (StatusCode = 200) then // do not localize
        begin
          dwNumber := 1024;
          while (InternetReadfile(hRequest, @databuffer, dwNumber, DwRead)) do
          begin
            if dwRead =0 then
              break;
            databuffer[dwread]:=#0;
            Str := pansichar(@databuffer);
            Response := Response + Str;
          end;

          // Output the server response.
          Result := Response;

          break;
        end
        else
          raise Exception.CreateFmt('HTTP Error %d with GET request %s', [StatusCode, aurl]);
      finally
        InternetCloseHandle(hRequest);
      end;
    end;
  finally
    InternetCloseHandle(hsession);
  end;
end;

procedure WinInet_DownloadFile(const URL, FileName: string; pgd: TProgressDlg=nil);
var
  AUrl: string;
  hSession, hRequest: HINTERNET;
  Buffer: array[0..1023] of Byte;
  BufferLen: DWORD;
  FileStream: TFileStream;
  FileSize, TotalRead: DWORD;
  dwSize: DWORD;
  reserved: DWORD;
  StatusCode: DWORD;
  RedirectCount: integer;
begin
  AUrl := Url;

  hSession := InternetOpen(USER_AGENT, INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
  if not Assigned(hSession) then
    raise Exception.Create('Error initializing WinInet: ' + SysErrorMessage(GetLastError));
  try
    RedirectCount := 0;
    while true do
    begin
      hRequest := InternetOpenUrl(hSession, PChar(AURL), nil, 0, INTERNET_FLAG_RELOAD or INTERNET_FLAG_NO_CACHE_WRITE, 0);
      if not Assigned(hRequest) then
        raise Exception.Create('Error opening request: ' + SysErrorMessage(GetLastError));
      try
        StatusCode := GetStatusCode(hRequest);
        if (StatusCode >= 300) and (StatusCode < 400) then
        begin
          Inc(RedirectCount);

          // Stop following redirects if we exceed the maximum number of allowed redirects.
          if RedirectCount > MaxRedirects then
          begin
            raise Exception.Create('Error: Too many redirects');
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
            if HttpQueryInfo(hRequest, HTTP_QUERY_CONTENT_LENGTH or HTTP_QUERY_FLAG_NUMBER, @FileSize, dwSize, reserved) then
              pgd.MaxValue := FileSize div 1024 // Number of KiB
            else
              pgd.MaxValue := 0;
          end;

          FileStream := TFileStream.Create(FileName, fmCreate);
          try
            TotalRead := 0;
            repeat
              // Lese Daten von der URL
              InternetReadFile(hRequest, @Buffer, SizeOf(Buffer), BufferLen);
              if BufferLen > 0 then
              begin
                FileStream.Write(Buffer, BufferLen);
                TotalRead := TotalRead + BufferLen;
                if pgd <> nil then pgd.Position := TotalRead div 1024;
              end;
            until BufferLen = 0;
          finally
            FileStream.Free;
          end;

          break;
        end
        else
          raise Exception.CreateFmt('HTTP Error %d with GET request %s', [StatusCode, aurl]);
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
  //ResStream: TResourceStream;
  bits: integer;
  outfil: string;
  outdir: string;
begin
  hl_Web_UseIndy := false;

  // Neuste Version der Indy SSL Libraries hier herunterladen: https://github.com/IndySockets/OpenSSL-Binaries

  {$IFDEF WIN64}
  bits := 64;
  {$ELSE}
  bits := 32;
  {$ENDIF}

  {$REGION 'Ordner erzeugen'}
  outdir := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))+'Win'+IntToStr(bits);
  if not DirectoryExists(outdir) then
  begin
    outdir := IncludeTrailingPathDelimiter(GetTempDir) + 'HS_OpenSSL'+IntToStr(bits);
  end;
  if not DirectoryExists(outdir) then
  begin
    ForceDirectories(outdir);
  end;
  if not DirectoryExists(outdir) then exit;
  {$ENDREGION}

  {$REGION 'LibEay32'}
  outfil := IncludeTrailingPathDelimiter(outdir)+'libeay32.dll';
  if not FileExists(outfil) then
  begin
    CopyFile(PChar(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))+'libeay32.'+IntToStr(bits)+'.dll'), PChar(outfil), false);
  end;
  (*
  if not FileExists(outfil) then
  begin
    try
      ResStream := TResourceStream.Create(HInstance, 'LIBEAY32', 'DLL');
      try
        ResStream.Position := 0;
        ResStream.SaveToFile(outfil);
      finally
        FreeAndNil(ResStream);
      end;
    except
    end;
  end;
  *)
  if not FileExists(outfil) then exit;
  {$ENDREGION}

  {$REGION 'SslEay32'}
  outfil := IncludeTrailingPathDelimiter(outdir)+'ssleay32.dll';
  if not FileExists(outfil) then
  begin
    CopyFile(PChar(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))+'ssleay32.'+IntToStr(bits)+'.dll'), PChar(outfil), false);
  end;
  (*
  if not FileExists(outfil) then
  begin
    try
      ResStream := TResourceStream.Create(HInstance, 'SSLEAY32', 'DLL');
      try
        ResStream.Position := 0;
        ResStream.SaveToFile(outfil);
      finally
        FreeAndNil(ResStream);
      end;
    except
    end;
  end;
  *)
  if not FileExists(outfil) then exit;
  {$ENDREGION}

  IdOpenSSLSetLibPath(outdir);
  hl_Web_UseIndy := true;
end;

function DoPost(const URL: string; Params: TStringList): string;
begin
  if hl_Web_UseIndy then
    result := Indy_DoPost(Url, Params)
  else
    result := WinInet_DoPost(Url, Params);
end;

function DoGet(const Url: string): string;
begin
  if hl_Web_UseIndy then
    result := Indy_DoGet(Url)
  else
    result := WinInet_DoGet(Url);
end;

procedure DownloadFile(const URL, FileName: string; pgd: TProgressDlg=nil);
begin
  if hl_Web_UseIndy then
    Indy_DownloadFile(URL, FileName, pgd)
  else
    WinInet_DownloadFile(URL, FileName, pgd);
end;

// https://marc.durdin.net/2012/07/indy-tiduri-pathencode-urlencode-and-paramsencode-and-more/
function EncodeURIComponent(const ASrc: string): UTF8String;
const
  HexMap: UTF8String = '0123456789ABCDEF';

  function IsSafeChar(ch: Integer): Boolean;
  begin
    if (ch >= 48) and (ch <= 57) then Result := True // 0-9
    else if (ch >= 65) and (ch <= 90) then Result := True // A-Z
    else if (ch >= 97) and (ch <= 122) then Result := True // a-z
    else if (ch = 33) then Result := True // !
    else if (ch >= 39) and (ch <= 42) then Result := True // '()*
    else if (ch >= 45) and (ch <= 46) then Result := True // -.
    else if (ch = 95) then Result := True // _
    else if (ch = 126) then Result := True // ~
    else Result := False;
  end;
var
  I, J: Integer;
  ASrcUTF8: UTF8String;
begin
  Result := '';    {Do not Localize}

  ASrcUTF8 := UTF8Encode(ASrc);
  // UTF8Encode call not strictly necessary but
  // prevents implicit conversion warning

  I := 1; J := 1;
  SetLength(Result, Length(ASrcUTF8) * 3); // space to %xx encode every byte
  while I <= Length(ASrcUTF8) do
  begin
    if IsSafeChar(Ord(ASrcUTF8[I])) then
    begin
      Result[J] := ASrcUTF8[I];
      Inc(J);
    end
    else
    begin
      Result[J] := '%';
      Result[J+1] := HexMap[(Ord(ASrcUTF8[I]) shr 4) + 1];
      Result[J+2] := HexMap[(Ord(ASrcUTF8[I]) and 15) + 1];
      Inc(J,3);
    end;
    Inc(I);
  end;

  SetLength(Result, J-1);
end;

initialization
  CopyOpenSslLibs;
end.
