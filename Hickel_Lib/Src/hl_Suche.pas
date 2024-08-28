unit hl_Suche;

interface

uses Classes, Db, DbClient, SysUtils, Dialogs, StrUtils;

type
  ThlSuche = class(TObject)
  private
    wortListe: TStringList;

    /// <summary>Ermittelt, ob der aktuelle Datensatz eines TDataSet zu einer Menge von Suchbegriffen passt (im Sinne von: Mindestens eines der Suchbegriffe enthält)</summary>
    function Passt(suche: string; woerter: TStringList): boolean; overload;
    function Passt(dataRow: TDataSet; suchbegriffListe: TStringList): boolean; overload;
  public
    function Suchen(daten: TDataSet; suchbegriffe: string): TDataSet; overload;
  end;

var
  hlSuche: ThlSuche;

implementation

function ThlSuche.Passt(suche: string; woerter: TStringList): boolean;
var
  i: integer;
begin
  result := false;
  for i := 0 to woerter.Count -1 do if (pos (suche, woerter [i]) > 0) then result := true;

end;
function ThlSuche.Passt(dataRow: TDataSet; suchbegriffListe: TStringList): boolean;
var
  i: integer;
  buffer: string;
  woerter2: TStringList;
begin
  wortListe.Clear;

  // Die dataRow in einen String umwandeln, diesen in einzelne Wörter
  // aufspalten, deren phonetische Codes berechnen und merken
  buffer := '';
  for i := 0 to dataRow.FieldCount-1 do buffer := buffer + dataRow.Fields[i].AsString + ' ';

  buffer := AnsiReplaceStr (buffer, '''', '');

  while buffer <> '' do
  begin
    i := pos(' ', buffer);
    if i = 0 then break;
    wortListe.Add (AnsiLowerCase (copy(buffer, 1, i-1)));
    buffer := copy(buffer, i+1, length(buffer));
  end;

  woerter2 := TStringList.Create;
  for i := 0 to wortliste.Count -1 do woerter2.Add (wortliste[i]);

  // Wenn alle der Suchbegriffe in der wortListe enthalten ist, Result := true und raus!
  result := true;
  for i := 0 to suchbegriffListe.count-1 do result := (result) and (((Passt (suchbegriffliste[i], wortliste)) or (Passt (suchbegriffliste[i], woerter2))));
end;

function ThlSuche.Suchen(daten: TDataSet; suchbegriffe: string): TDataSet;
var
  ret: TClientDataSet;
  suchbegriffListe: TStringList;
  i: integer;
  alle: boolean;

begin
  wortliste := TStringList.Create;
  try
    alle := (suchbegriffe = '');

    ret := TClientDataSet.Create(nil);
    ret.FieldDefs.Assign(daten.FieldDefs);
    ret.CreateDataSet;
    for i := 0 to ret.Fields.Count -1 do
      ret.Fields[i].ReadOnly := false;
    suchbegriffe := StringReplace(AnsiLowerCase(suchbegriffe) + ' ', '  ', ' ', [rfReplaceAll]);
    suchbegriffListe := TStringList.Create;
    try
      // Suchbegriffe in Liste aufspalten und deren phonetische Codes merken  /  TStringList.DelimitedText wäre nett, funktioniert mit Leerzeichen aber nicht zuverlässig
      while suchbegriffe <> '' do
      begin
        i := pos(' ', suchbegriffe);
        if i = 0 then break;
        suchbegriffListe.Add(copy(suchbegriffe, 1, i-1));
        suchbegriffe := copy(suchbegriffe, i+1, length(suchbegriffe));
      end;

      // Datensätze durchgehen, alle zu den Suchbegriffen passenden in die Ergebnismenge einfügen
      daten.First;
      while not daten.EOF do
      begin
        if alle or passt(daten, suchbegriffListe) then
        begin
          ret.Append;
          for i := 0 to daten.FieldCount-1 do ret.Fields[i].Value := daten.Fields[i].Value;
          ret.Post;
        end;
        daten.Next;
      end;

      ret.First;

    finally
      FreeAndNil(suchbegriffListe);
    end;
    Result := ret;
  finally
    FreeAndNil(wortListe);
  end;
end;

(*

initialization
  hlSuche := ThlSuche.Create;

f_inalization
  FreeAndNil(hlSuche);

*)

end.
