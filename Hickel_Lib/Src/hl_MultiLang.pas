unit hl_MultiLang;

interface

procedure HS_SwitchLanguage;

implementation

uses
  Windows, SysUtils;

// Taken from https://github.com/danielmarschall/spacemission/blob/master/Global.pas

procedure SwitchLanguage(newLang: string);
var
  oldHInst: hInst;
  newHInst: hInst;
  bakOverride: string;
  FileName: array [0 .. MAX_PATH] of Char;
  Module: PLibModule;
begin
  Module := LibModuleList;
  GetModuleFileName(Module.Instance, FileName, Length(FileName));

  bakOverride := GetLocaleOverride('');
  try
    SetLocaleOverride(newLang);

    // Note: SetLocaleOverride() alone does not work, because LibModuleList.ResInstance
    // is already set and won't be re-set by the FindResourceHInstance()!
    newHInst := LoadResourceModule(FileName);
    if newHInst = 0 then
      newHInst := Module.Instance;

    oldHInst := Module.ResInstance;
    Module.ResInstance := newHInst;
    ResStringCleanupCache;
    FreeLibrary(oldHInst);
  except
    on E: EAbort do
    begin
      Abort;
    end;
    on E: Exception do
    begin
      SetLocaleOverride(bakOverride);
    end;
  end;
end;

function GetUserDefaultUILanguage: LANGID; stdcall; external 'kernel32';

procedure HS_SwitchLanguage;
const
  BaseLanguage = LANG_GERMAN;
  DesiredFallbackLanguage = 'ENU'; // English USA
begin
  // We need this because of a tricky problem...
  // Our base language is German (DE), and we have a translation for English USA (ENU)
  // If the system locale is not exactly ENU (even ENG is not accepted), then the base language (German) will be used.
  // But much more people are speaking English than German. So we need to force the system to use ENU instead of DE,
  // if the system is not DE.
  if (GetLocaleOverride('') = '') and
    ((GetUserDefaultUILanguage and $FF) <> BaseLanguage) then
  begin
    SwitchLanguage(DesiredFallbackLanguage);
  end;
end;

end.
