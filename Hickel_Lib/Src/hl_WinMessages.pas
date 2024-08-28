unit hl_WinMessages;

interface

uses
  Windows, Messages;

type
  THsWindowsMessage = class(TObject)
  strict protected
    class function GetMessageID: UINT; virtual; abstract;
    class procedure Send(awParam: WPARAM; alParam: LPARAM); virtual;
  end;

  ThswmOeffneBelegEvent = procedure (BelegTyp, BelegArt: string; BelegNr: integer) of object;
  ThswmOeffneBeleg = class(THsWindowsMessage)
  strict private
    class var FMsg: integer;
  strict protected
    class function GetMessageID: UINT; override;
  public
    class procedure Send(BelegArt: string; BelegNr: integer); reintroduce;
    class function Check(m: TMessage; Event: ThswmOeffneBelegEvent): boolean;
  end;

  ThswmAktualisiereBelegListeEvent = procedure (BelegTyp, BelegArt: string; BelegNr: integer) of object;
  ThswmAktualisiereBelegListe = class(THsWindowsMessage)
  strict private
    class var FMsg: integer;
  strict protected
    class function GetMessageID: UINT; override;
  public
    class procedure Send(BelegArt: string; BelegNr: integer); reintroduce;
    class function Check(m: TMessage; Event: ThswmAktualisiereBelegListeEvent): boolean;
  end;

  ThswmOpenAppEvent = procedure (AppCrc32: Cardinal) of object;
  ThswmOpenApp = class(THsWindowsMessage)
  strict private
    class var FMsg: integer;
  strict protected
    class function GetMessageID: UINT; override;
  public
    class procedure Send(AppCrc32: Cardinal); reintroduce;
    class function Check(m: TMessage; Event: ThswmOpenAppEvent): boolean;
  end;

implementation

function _BelegArtToInteger(BelegArt: string): integer;
begin
  // Wird verwendet von ThswmOeffneBeleg und ThswmAktualisiereBelegListe
  // Bitte synchron halten mit HickelSoftWindowsMessages.cs und hcl_Consts.pas!

  { A-Belegtyp }
  if BelegArt = 'ANG' then result := 1              // A	ANG		WA-Angebot
  else if BelegArt = 'AUF' then result := 2         // A	AUF		WA-Auftrag
  else if BelegArt = 'DAU' then result := 3         // A	DAU		WA-Dauerauftrag
  else if BelegArt = 'LIE' then result := 4         // A	LIE		WA-Lieferschein
  else if BelegArt = 'iLI' then result := 5         // A	iLI		Interner Lieferschein
  else if BelegArt = 'RUE' then result := 6         // A	RUE		WA-Rückgabeschein
  else if BelegArt = 'REC' then result := 7         // A	REC		WA-Rechnung
  else if BelegArt = 'WEB' then result := 8         // A	WEB		Webshop-Bestellung
  else if BelegArt = 'ABS' then result := 9         // A	ABS		Anfangsbestand für Leergutmeldungen für Kunden (Derzeit keine Eingabemöglichkeit für den Endkunden)
  else if BelegArt = 'BON' then result := 10        // A	BON		WA-Bon
  else if BelegArt = 'LGB' then result := 11        // A	LGB		Leergut-Annahme-Bon
  else if BelegArt = 'AUR' then result := 12        // A	AUR		Rückhol-Auftrag

  { L-Belegtyp }
  else if BelegArt = 'INV' then result := 20        // L	INV		Inventuren
  else if BelegArt = 'LAG' then result := 21        // L	LAG		Lagerbuchungen (Mengenbuchung ohne Preis)
  else if BelegArt = 'UMB' then result := 22        // L	UMB		Lager-Umbuchungen

  { E-Belegtyp }
  else if BelegArt = 'anf' then result := 40        // E	anf		WE-Anfrage (Bei Bestellungen 2.0 : WE-Anforderung)
  else if BelegArt = 'bes' then result := 41        // E	bes		WE-Bestellung
  else if BelegArt = 'lie' then result := 42        // E	lie		WE-Lieferschein
  else if BelegArt = 'rec' then result := 43        // E	rec		WE-Rechnung
  else if BelegArt = 'abs' then result := 44        // E	abs		Anfangsbestand für Leergutmeldungen von Lieferanten (Derzeit keine Eingabemöglichkeit für den Endkunden)

  else result := 0; // nicht -1, da sonst cast nach WPARAM oder LPARAM nicht möglich ist
end;

function _IntegerToBelegArt(i: integer): string;
begin
  // Wird verwendet von ThswmOeffneBeleg und ThswmAktualisiereBelegListe
  // Bitte synchron halten mit HickelSoftWindowsMessages.cs und hcl_Consts.pas!

  { A-Belegtyp }
  if i = 1 then result := 'ANG'                     // A	ANG		WA-Angebot
  else if i = 2 then result := 'AUF'                // A	AUF		WA-Auftrag
  else if i = 3 then result := 'DAU'                // A	DAU		WA-Dauerauftrag
  else if i = 4 then result := 'LIE'                // A	LIE		WA-Lieferschein
  else if i = 5 then result := 'iLI'                // A	iLI		Interner Lieferschein
  else if i = 6 then result := 'RUE'                // A	RUE		WA-Rückgabeschein
  else if i = 7 then result := 'REC'                // A	REC		WA-Rechnung
  else if i = 8 then result := 'WEB'                // A	WEB		Webshop-Bestellung
  else if i = 9 then result := 'ABS'                // A	ABS		Anfangsbestand für Leergutmeldungen für Kunden (Derzeit keine Eingabemöglichkeit für den Endkunden)
  else if i = 10 then result := 'BON'               // A	BON		WA-Bon
  else if i = 11 then result := 'LGB'               // A	LGB		Leergut-Annahme-Bon
  else if i = 12 then result := 'AUR'               // A	AUR		Rückhol-Auftrag

  { L-Belegtyp }
  else if i = 20 then result := 'INV'               // L	INV		Inventuren
  else if i = 21 then result := 'LAG'               // L	LAG		Lagerbuchungen (Mengenbuchung ohne Preis)
  else if i = 22 then result := 'UMB'               // L	UMB		Lager-Umbuchungen

  { E-Belegtyp }
  else if i = 40 then result := 'anf'               // E	anf		WE-Anfrage (Bei Bestellungen 2.0 : WE-Anforderung)
  else if i = 41 then result := 'bes'               // E	bes		WE-Bestellung
  else if i = 42 then result := 'lie'               // E	lie		WE-Lieferschein
  else if i = 43 then result := 'rec'               // E	rec		WE-Rechnung
  else if i = 44 then result := 'abs'               // E	abs		Anfangsbestand für Leergutmeldungen von Lieferanten (Derzeit keine Eingabemöglichkeit für den Endkunden)

  else result := '???';
end;

function _IntegerToBelegTyp(i: integer): string;
begin
  // Wird verwendet von ThswmOeffneBeleg und ThswmAktualisiereBelegListe
  if      (i >=  1) and (i <= 19) then result := 'A'
  else if (i >= 20) and (i <= 39) then result := 'L'
  else if (i >= 40) and (i <= 59) then result := 'E'
  else result := '?';
end;

{ THsWindowsMessage }

class procedure THsWindowsMessage.Send(awParam: WPARAM; alParam: LPARAM);
var
  Msg: UINT;
begin
  Msg := GetMessageID;
  SendNotifyMessage(wnd_Broadcast, Msg, awParam, alParam);
end;

{ ThswmOeffneBeleg }

class function ThswmOeffneBeleg.GetMessageID: UINT;
var
  oid: string;
begin
  if FMsg = 0 then
  begin
    // { iso(1) identified-organization(3) dod(6) internet(1) private(4) enterprise(1) 56776 windows-messages(150) oeffne-beleg(1) }
    oid := '1.3.6.1.4.1.56776.150.1';
    FMsg := RegisterWindowMessage(PChar(oid));
  end;
  result := FMsg;
end;

class procedure ThswmOeffneBeleg.Send(BelegArt: string; BelegNr: integer);
var
  lwParam: WPARAM;
  llParam: LPARAM;
begin
  lwParam := _BelegArtToInteger(Belegart);
  llParam := BelegNr;
  inherited Send(lwParam, llParam);
end;

class function ThswmOeffneBeleg.Check(m: TMessage; Event: ThswmOeffneBelegEvent): boolean;
var
  BelegArt: string;
  BelegNr: Integer;
  BelegTyp: string;
begin
  if m.Msg = GetMessageID then
  begin
    if Assigned(Event) then
    begin
      BelegTyp := _IntegerToBelegTyp(m.WParam);
      BelegArt := _IntegerToBelegArt(m.WParam);
      BelegNr := m.LParam;
      Event(BelegTyp, BelegArt, BelegNr);
    end;
    result := true;
  end
  else result := false;
end;

{ ThswmAktualisiereBelegListe }

class function ThswmAktualisiereBelegListe.GetMessageID: UINT;
var
  oid: string;
begin
  if FMsg = 0 then
  begin
    // { iso(1) identified-organization(3) dod(6) internet(1) private(4) enterprise(1) 56776 windows-messages(150) aktualisiere-belegliste(2) }
    oid := '1.3.6.1.4.1.56776.150.2';
    FMsg := RegisterWindowMessage(PChar(oid));
  end;
  result := FMsg;
end;

class procedure ThswmAktualisiereBelegListe.Send(BelegArt: string; BelegNr: integer);
var
  lwParam: WPARAM;
  llParam: LPARAM;
begin
  lwParam := _BelegArtToInteger(Belegart);
  llParam := BelegNr;
  inherited Send(lwParam, llParam);
end;

class function ThswmAktualisiereBelegListe.Check(m: TMessage; Event: ThswmAktualisiereBelegListeEvent): boolean;
var
  BelegArt: string;
  BelegNr: Integer;
  BelegTyp: string;
begin
  if m.Msg = GetMessageID then
  begin
    if Assigned(Event) then
    begin
      BelegTyp := _IntegerToBelegTyp(m.WParam);
      BelegArt := _IntegerToBelegArt(m.WParam);
      BelegNr := m.LParam;
      Event(BelegTyp, BelegArt, BelegNr);
    end;
    result := true;
  end
  else result := false;
end;

{ ThswmOpenApp }

class function ThswmOpenApp.GetMessageID: UINT;
var
  oid: string;
begin
  if FMsg = 0 then
  begin
    // { iso(1) identified-organization(3) dod(6) internet(1) private(4) enterprise(1) 56776 windows-messages(150) open-app(3) }
    oid := '1.3.6.1.4.1.56776.150.3';
    FMsg := RegisterWindowMessage(PChar(oid));
  end;
  result := FMsg;
end;

type
  pWPARAM = ^WPARAM;

class function ThswmOpenApp.Check(m: TMessage; Event: ThswmOpenAppEvent): boolean;
var
  AppCrc32: Cardinal;
  tmp1: pWPARAM;
  tmp2: PCardinal;
begin
  if m.Msg = GetMessageID then
  begin
    if Assigned(Event) then
    begin
      tmp1 := @m.WParam;
      tmp2 := pCardinal(tmp1);

      AppCrc32 := tmp2^;
      Event(AppCrc32);
    end;
    result := true;
  end
  else result := false;
end;

class procedure ThswmOpenApp.Send(AppCrc32: Cardinal);
var
  lwParam: WPARAM;
  llParam: LPARAM;
  tmp1: pWPARAM;
  tmp2: PCardinal;
begin
  tmp2 := @AppCrc32;
  tmp1 := pWPARAM(tmp2);

  lwParam := tmp1^;
  llParam := 0;
  inherited Send(lwParam, llParam);
end;

end.
