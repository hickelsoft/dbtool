unit hl.Utils.DbGridUtils;

interface

uses
  Classes, wwdbgrid, wwdblook, SysUtils;

type
  TwwdbGridHelper = class helper for TwwdbGrid
  private
    procedure FeldAendern(sucheFeld, neuerName: string; ausblenden: boolean);
  public
    // W�chtig IniAttributes.Enabled muss false sein, da sonst LoadFromIniFile (das Unsichere) im FormCreate aufgerufen wird
    // Eigentlich w�re es bessern, man w�rde TwwDbGrid ABLEITEN und LoadFromIniFile �berschreiben. Aber dann m�sste man die Deklaration in JEDEM Delphi Form �ndern...
    procedure SafeLoadFromIniFile;
    procedure FeldAusblenden(feldname: string; keinFehler: boolean = false);
    procedure FeldUmbenennen(feldname, neueBezeichnung: string;
      keinFehler: boolean = false);
    function FeldVorhanden(sucheFeld: string): boolean;
    procedure ApplyRowSelectionAndFree(sl: TStringList);
    function RememberRowSelection: TStringList;
  end;

  TwwDBCustomLookupComboHelper = class helper for TwwDBCustomLookupCombo
  private
    procedure FeldAendern(sucheFeld, neuerName: string; ausblenden: boolean);
  public
    procedure FeldAusblenden(feldname: string; keinFehler: boolean = false);
    procedure FeldUmbenennen(feldname, neueBezeichnung: string;
      keinFehler: boolean = false);
    function FeldVorhanden(sucheFeld: string): boolean;
  end;

implementation

uses
  IniFiles;

resourcestring
  StrFeldSNichtGefund = 'Feld %s nicht gefunden';

procedure Explode(Delimiter: Char; Str: string; ListOfStrings: TStrings);
begin
  ListOfStrings.Clear;
  ListOfStrings.Delimiter := Delimiter;
  ListOfStrings.StrictDelimiter := True; // Requires D2006 or newer.
  ListOfStrings.DelimitedText := Str;
end;

function Implode(const cSeparator: String; const sl: TStrings): String;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to sl.Count - 1 do
  begin
    Result := Result + cSeparator + sl[i];
  end;
  System.Delete(Result, 1, Length(cSeparator));
end;

{ TwwdbGridHelper }

procedure TwwdbGridHelper.FeldAendern(sucheFeld, neuerName: string;
  ausblenden: boolean);
var
  i: Integer;
  outSL: TStrings;
begin
  outSL := TStringList.Create;
  try
    for i := Selected.Count - 1 downto 0 do
    begin
      Explode(#9, Selected.Strings[i], outSL);
      if (outSL.Count > 0) and (outSL.Strings[0] = sucheFeld) then
      begin
        if ausblenden then
        begin
          Selected.Delete(i);
        end
        else
        begin
          Assert(outSL.Count >= 3);
          if neuerName <> '' then
            outSL.Strings[2] := neuerName;
          Selected.Strings[i] := Implode(#9, outSL);
        end;
        Exit;
      end;
    end;
    raise Exception.CreateFmt(StrFeldSNichtGefund, [sucheFeld]);
  finally
    FreeAndNil(outSL);
  end;
end;

procedure TwwdbGridHelper.FeldAusblenden(feldname: string; keinFehler: boolean);
begin
  if keinFehler and not FeldVorhanden(feldname) then
    Exit;
  FeldAendern(feldname, '', True);

  // TODO: Hier kommt "Listenindex �berschreitet maximum (-1)" wenn keine tabelle ge�ffnet ist
  ApplySelected;
end;

procedure TwwdbGridHelper.FeldUmbenennen(feldname, neueBezeichnung: string;
  keinFehler: boolean);
begin
  if keinFehler and not FeldVorhanden(feldname) then
    Exit;
  FeldAendern(feldname, neueBezeichnung, false);
  ApplySelected;
end;

function TwwdbGridHelper.FeldVorhanden(sucheFeld: string): boolean;
var
  i: Integer;
  outSL: TStrings;
begin
  outSL := TStringList.Create;
  try
    Result := false;
    for i := 0 to Selected.Count - 1 do
    begin
      Explode(#9, Selected.Strings[i], outSL);
      if (outSL.Count > 0) and (outSL.Strings[0] = sucheFeld) then
      begin
        Result := True;
      end;
    end;
  finally
    FreeAndNil(outSL);
  end;
end;

procedure TwwdbGridHelper.SafeLoadFromIniFile;

  function _sltos(sl: TStrings): string;
  var
    slx: TStringList;
    i, p: Integer;
    tmp: string;
  begin
    slx := TStringList.Create;
    try
      slx.Assign(sl);
      for i := 0 to slx.Count - 1 do
      begin
        tmp := slx.Strings[i];
        if trim(tmp) = '' then
          continue;
        p := Pos('=', tmp);
        if p = 0 then
          p := Pos(';', tmp);
        if p = 0 then
          p := Pos(#9, tmp);
        tmp := Copy(tmp, 1, p - 1);
        slx.Strings[i] := tmp;
      end;
      slx.Sort;
      Result := trim(slx.Text);
    finally
      FreeAndNil(slx);
    end;
  end;

var
  ini: TMemIniFile;
  iniFields: TStringList;
  gridFields: TStringList;
  sIniFields, sGridFields: string;
begin
  ini := TMemIniFile.Create(IniAttributes.FileName);
  iniFields := TStringList.Create;
  gridFields := TStringList.Create;
  try
    ini.ReadSectionValues(IniAttributes.SectionName, iniFields);

    sIniFields := _sltos(iniFields);

    sGridFields := _sltos(Selected);

    // showmessage(IniAttributes.SectionName + #13#10#13#10 + 'Ini Fields: ' + #13#10 + sIniFields + #13#10#13#10 + 'Grid Fields: ' + #13#10 + sGridFields);

    if sIniFields = sGridFields then
    begin
      // Es sind keine Felder hinzugekommen oder weggefallen. Wir k�nnen sicher weitermachen
      LoadFromIniFile;
    end;

  finally
    FreeAndNil(ini);
    FreeAndNil(iniFields);
    FreeAndNil(gridFields);
  end;
end;

function TwwdbGridHelper.RememberRowSelection: TStringList;
var
  s: string;
  i: Integer;
  grd: TwwdbGrid;
begin
  grd := self;
  Result := nil;
  if grd.DataSource = nil then
    Exit;
  if grd.DataSource.DataSet = nil then
    Exit;
  if not grd.DataSource.DataSet.Active then
    Exit;
  Result := TStringList.Create;

  // Remember row with cursor
  s := '';
  for i := 0 to grd.DataSource.DataSet.FieldCount - 1 do
  begin
    s := s + grd.DataSource.DataSet.Fields[i].AsWideString + ';';
  end;
  Result.Add('CurRow=' + s);

  // Remember other selected rows
  grd.DataSource.DataSet.First;
  while not grd.DataSource.DataSet.Eof do
  begin
    if grd.IsSelected then
    begin
      s := '';
      for i := 0 to grd.DataSource.DataSet.FieldCount - 1 do
      begin
        s := s + grd.DataSource.DataSet.Fields[i].AsWideString + ';';
      end;
      Result.Add(s);
    end;
    grd.DataSource.DataSet.Next;
  end;

  // Go back to cursor
  grd.DataSource.DataSet.First;
  while not grd.DataSource.DataSet.Eof do
  begin
    s := '';
    for i := 0 to grd.DataSource.DataSet.FieldCount - 1 do
    begin
      s := s + grd.DataSource.DataSet.Fields[i].AsWideString + ';';
    end;
    if Result.IndexOf('CurRow=' + s) > -1 then
      break;
    grd.DataSource.DataSet.Next;
  end;
end;

procedure TwwdbGridHelper.ApplyRowSelectionAndFree(sl: TStringList);
var
  s: string;
  i: Integer;
  grd: TwwdbGrid;
begin
  grd := self;
  if sl = nil then
    Exit;
  if grd.DataSource = nil then
    Exit;
  if grd.DataSource.DataSet = nil then
    Exit;
  if not grd.DataSource.DataSet.Active then
    Exit;

  // Recover selected rows
  grd.UnselectAll;
  grd.DataSource.DataSet.First;
  while not grd.DataSource.DataSet.Eof do
  begin
    s := '';
    for i := 0 to grd.DataSource.DataSet.FieldCount - 1 do
    begin
      s := s + grd.DataSource.DataSet.Fields[i].AsWideString + ';';
    end;
    if sl.IndexOf(s) = -1 then
      grd.UnselectRecord
    else
      grd.SelectRecord;
    grd.DataSource.DataSet.Next;
  end;

  // Go back to cursor
  grd.DataSource.DataSet.First;
  while not grd.DataSource.DataSet.Eof do
  begin
    s := '';
    for i := 0 to grd.DataSource.DataSet.FieldCount - 1 do
    begin
      s := s + grd.DataSource.DataSet.Fields[i].AsWideString + ';';
    end;
    if sl.IndexOf('CurRow=' + s) > -1 then
      break;
    grd.DataSource.DataSet.Next;
  end;
end;

{ TwwDBCustomLookupComboHelper }

procedure TwwDBCustomLookupComboHelper.FeldAendern(sucheFeld, neuerName: string;
  ausblenden: boolean);
var
  i: Integer;
  outSL: TStrings;
begin
  outSL := TStringList.Create;
  try
    for i := Selected.Count - 1 downto 0 do
    begin
      Explode(#9, Selected.Strings[i], outSL);
      if (outSL.Count > 0) and (outSL.Strings[0] = sucheFeld) then
      begin
        if ausblenden then
        begin
          Selected.Delete(i);
        end
        else
        begin
          Assert(outSL.Count >= 3);
          if neuerName <> '' then
            outSL.Strings[2] := neuerName;
          Selected.Strings[i] := Implode(#9, outSL);
        end;
        Exit;
      end;
    end;
    raise Exception.CreateFmt(StrFeldSNichtGefund, [sucheFeld]);
  finally
    FreeAndNil(outSL);
  end;
end;

procedure TwwDBCustomLookupComboHelper.FeldAusblenden(feldname: string;
  keinFehler: boolean);
begin
  if keinFehler and not FeldVorhanden(feldname) then
    Exit;
  FeldAendern(feldname, '', True);

  // TODO: Hier kommt "Listenindex �berschreitet maximum (-1)" wenn keine tabelle ge�ffnet ist
  // ApplySelected;
end;

procedure TwwDBCustomLookupComboHelper.FeldUmbenennen(feldname, neueBezeichnung
  : string; keinFehler: boolean);
begin
  if keinFehler and not FeldVorhanden(feldname) then
    Exit;
  FeldAendern(feldname, neueBezeichnung, false);
  // ApplySelected;
end;

function TwwDBCustomLookupComboHelper.FeldVorhanden(sucheFeld: string): boolean;
var
  i: Integer;
  outSL: TStrings;
begin
  outSL := TStringList.Create;
  try
    Result := false;
    for i := 0 to Selected.Count - 1 do
    begin
      Explode(#9, Selected.Strings[i], outSL);
      if (outSL.Count > 0) and (outSL.Strings[0] = sucheFeld) then
      begin
        Result := True;
      end;
    end;
  finally
    FreeAndNil(outSL);
  end;
end;

end.
