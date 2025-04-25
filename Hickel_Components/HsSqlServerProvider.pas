unit HsSqlServerProvider;

interface

function SqlServerProvider(NoCache: boolean=false): string; // gibt den NEUSTEN SQL-Provider zurück oder den Debug-Hartkodierten Provider
function SqlServerProviderCompat(NoCache: boolean=false): string; // gibt den ÄLTESTEN SQL-Provider zurück
function ProviderIsInstalled(Provider: string): boolean;

implementation

uses
  Registry, Windows, SysUtils;

threadvar
  _SqlServerProvider_Cache: string;

function SqlServerProvider(NoCache: boolean=false): string;
var
  reg: TRegistry;
begin
  if not NoCache and (_SqlServerProvider_Cache <> '') then
  begin
    result := _SqlServerProvider_Cache;
    exit;
  end;

  // Achtung: Wenn hier was neues hinzukommt, dann den Provider testen
  // mit RECHNUNG drucken, mit und ohne "sa" Anmeldung

  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CLASSES_ROOT;

    if reg.KeyExists('CLSID\{EE5DE99A-4453-4C96-861C-F8832A7F59FE}') then
    begin
      result := 'MSOLEDBSQL19'; // Generation 3, Version 19+
      _SqlServerProvider_Cache := result;
      exit;
    end;
    if reg.KeyExists('CLSID\{5A23DE84-1D7B-4A16-8DED-B29C09CB648D}') then
    begin
      result := 'MSOLEDBSQL'; // Generation 3
      _SqlServerProvider_Cache := result;
      exit;
    end;
    if reg.KeyExists('CLSID\{397C2819-8272-4532-AD3A-FB5E43BEAA39}') then
    begin
      result := 'SQLNCLI11'; // Generation 2, Version 11
      _SqlServerProvider_Cache := result;
      exit;
    end;
    if reg.KeyExists('CLSID\{8F4A6B68-4F36-4e3c-BE81-BC7CA4E9C45C}') then
    begin
      result := 'SQLNCLI10'; // Generation 2, Version 10
      _SqlServerProvider_Cache := result;
      exit;
    end;
    if reg.KeyExists('CLSID\{0C7FF16C-38E3-11d0-97AB-00C04FC2AD98}') then
    begin
      result := 'SQLOLEDB'; // Generation 1
      _SqlServerProvider_Cache := result;
      exit;
    end;
  finally
    FreeAndNil(reg);
  end;

  result := 'SQLOLEDB'; // Fallback (sollte nie passieren)
  _SqlServerProvider_Cache := result;
end;

var
  _SqlServerProviderCompat_Cache: string = '';

function SqlServerProviderCompat(NoCache: boolean=false): string;
var
  reg: TRegistry;
begin
  if not NoCache and (_SqlServerProviderCompat_Cache <> '') then
  begin
    result := _SqlServerProviderCompat_Cache;
    exit;
  end;

  // Achtung: Wenn hier was neues hinzukommt, dann den Provider testen
  // mit RECHNUNG drucken, mit und ohne "sa" Anmeldung

  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CLASSES_ROOT;

    if reg.KeyExists('CLSID\{0C7FF16C-38E3-11d0-97AB-00C04FC2AD98}') then
    begin
      result := 'SQLOLEDB'; // Generation 1
      _SqlServerProviderCompat_Cache := result;
      exit;
    end;
    if reg.KeyExists('CLSID\{8F4A6B68-4F36-4e3c-BE81-BC7CA4E9C45C}') then
    begin
      result := 'SQLNCLI10'; // Generation 2, Version 10
      _SqlServerProviderCompat_Cache := result;
      exit;
    end;
    if reg.KeyExists('CLSID\{397C2819-8272-4532-AD3A-FB5E43BEAA39}') then
    begin
      result := 'SQLNCLI11'; // Generation 2, Version 11
      _SqlServerProviderCompat_Cache := result;
      exit;
    end;
    if reg.KeyExists('CLSID\{5A23DE84-1D7B-4A16-8DED-B29C09CB648D}') then
    begin
      result := 'MSOLEDBSQL'; // Generation 3
      _SqlServerProviderCompat_Cache := result;
      exit;
    end;
    if reg.KeyExists('CLSID\{EE5DE99A-4453-4C96-861C-F8832A7F59FE}') then
    begin
      result := 'MSOLEDBSQL19'; // Generation 3, Version 19+
      _SqlServerProviderCompat_Cache := result;
      exit;
    end;
  finally
    FreeAndNil(reg);
  end;

  result := 'SQLOLEDB'; // Fallback (sollte nie passieren)
  _SqlServerProviderCompat_Cache := result;
end;

function ProviderIsInstalled(Provider: string): boolean;
var
  reg: TRegistry;
begin
  // Achtung: Wenn hier was neues hinzukommt, dann den Provider testen
  // mit RECHNUNG drucken, mit und ohne "sa" Anmeldung
  result := false;
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CLASSES_ROOT;
    if (Provider='SQLOLEDB') and reg.KeyExists('CLSID\{0C7FF16C-38E3-11d0-97AB-00C04FC2AD98}') then
    begin
      result := true; // Generation 1
      exit;
    end;
    if (Provider='SQLNCLI10') and reg.KeyExists('CLSID\{8F4A6B68-4F36-4e3c-BE81-BC7CA4E9C45C}') then
    begin
      result := true; // Generation 2, Version 10
      exit;
    end;
    if (Provider='SQLNCLI11') and reg.KeyExists('CLSID\{397C2819-8272-4532-AD3A-FB5E43BEAA39}') then
    begin
      result := true; // Generation 2, Version 11
      exit;
    end;
    if (Provider='MSOLEDBSQL') and reg.KeyExists('CLSID\{5A23DE84-1D7B-4A16-8DED-B29C09CB648D}') then
    begin
      result := true; // Generation 3
      exit;
    end;
    if (Provider='MSOLEDBSQL19') and reg.KeyExists('CLSID\{EE5DE99A-4453-4C96-861C-F8832A7F59FE}') then
    begin
      result := true; // Generation 3, Version 19+
      exit;
    end;
  finally
    FreeAndNil(reg);
  end;
end;

end.
