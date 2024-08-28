unit hl.Utils.Web;

interface

uses
  Windows, Classes, IdHTTP, IdSSLOpenSSL, IdComponent, ProgrDlg;

function secure_email(email, linktext: string; crypt_linktext: boolean): string;
function htmlEntities(s: string): string;
function nl2br(s: string): string;
function InsertHTMLAfterBody(insert, htmlTemplate: string): string;
function IsHTML(s: string): boolean;
function HTMLToText(html: string): string;
function TextToHtml(text: string): string;
function DoPost(URL: string; lParamList: TStringList): string;
function DoGet(URL: string): string;
function EncodeURIComponent(const ASrc: string): UTF8String;
procedure DownloadFile(url, filename: string; pgd: TProgressDlg=nil);

implementation

uses
  SysUtils, StrUtils, SHDocVW, MsHTML, Variants, ActiveX,
  IdSSLOpenSSLHeaders, hl.Utils;

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

function DoPost(URL: string; lParamList: TStringList): string;
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
      lHTTP.Post(URL, lParamList, Stream);
      Stream.Position := 0;
      result := Stream.DataString;
    finally
      FreeAndNil(Stream);
    end;
  finally
    FreeAndNil(lHTTP);
  end;
end;

function DoGet(URL: string): string;
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

procedure DownloadFile(url, filename: string; pgd: TProgressDlg=nil);
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

procedure CopyOpenSslLibs;
var
  ResStream: TResourceStream;
  bits: integer;
  outfil: string;
  outdir: string;
begin
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
  if not FileExists(outfil) then exit;
  {$ENDREGION}

  {$REGION 'SslEay32'}
  outfil := IncludeTrailingPathDelimiter(outdir)+'ssleay32.dll';
  if not FileExists(outfil) then
  begin
    CopyFile(PChar(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))+'ssleay32.'+IntToStr(bits)+'.dll'), PChar(outfil), false);
  end;
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
  if not FileExists(outfil) then exit;
  {$ENDREGION}

  IdOpenSSLSetLibPath(outdir);
end;

initialization
  CopyOpenSslLibs;
end.
