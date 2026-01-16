# DBTool ToDo

## Important features

- SQL Server: Also allow entering of username and password instead of only allowing NT Auth

## Known bugs

If you close the database window, all query windows also close, but the OnCloseQuery is not executed there!

Error when copying an entire CORA_1 database via drag'n'drop (client DB. System DB is OK):
- BelegeWaBasis and BelegeWaPositionen: "Statistics already exist" for the index that is 128 characters long in the name
- Problem with views: There may be dependency conflicts. How can we create a view without it checking that the tables described in it exist?

Table.pas: Left buttons highlight is light blue with white font, which is unreadable!

Query.pas: /* */ comments are not handled correctly

## Ideas

Table.pas: It would be nice if the left side menu would also have "Show Stored Procedure Definition" or "Show View Definition", etc.

Query.pas: Search inside data set or search inside SQL Query. Both things are important!

## DBTool ToDo Old List (might already be solved or invalid)

Weggefallene Funktionen durch Umstellung C++ Builder → Delphi auf Standardkomponenten, wiederherstellen. (Im Quellcode mit XXX markiert)

- ~~Strukturdruck~~ => CRW Implementierung in Erwägung ziehen, da QuickReport nicht mehr existiert
- Farbauswahl-Optionen
- Farbverläufe und andere visuelle Dinge

CSV Im/Export: Puffer erhöhen

DirectSQL:

- F5 = F9 ? (SQL Studio Kompatibilität)
- Knopf bleibt hängen bei SQL-Fehler
- Mehrzeilige SQL-Befehle sollen mehrere Ergebnis-Tabellen aufmachen, wie bei SQL Studio (ist aber wohl zu kompliziert)
- ~~Strg+S/O zum Laden/Speichern einer \*.sql Datei~~ (Kollidiert mit den Shotcuts des MainForms)
- Tab erlauben?
- Man soll Zeilen auch höher machen können, wie bei der Tabellenansicht
- Auch Suchen erlauben, so wie bei Table.cpp und Query.cpp
- Query.cpp soll mehr Features von Table.cpp übernehmen

Problem mit BCDField... Alle TQuery auf form behandeln.... damit das nicht mehr vorkommt

Struktur-Ansicht:

- Welche Felder dürfen NULL haben? => Diese Anzeige ist fehlerhaft, da hier die DEFAULT-Eigenschaft nicht berücksichtigt wird!
- BCD wird angezeigt statt Decimal

Decimal-Felder werden immer auf 2 Nachkommastellen abgeschnitten

(Ticket 29924/3) alle Tabellennamen mit [] ansprechen, damit Sonderzeichen in Tabellennamen erlaubt! Auch [dbo] Prefix beachten.

`exec xxx` stored procedure wird nicht als Tabelle erkannt.

Anmelden-Dialog (MySQL + SQLServer):

- OpenMySQLDb.dfm und OpenSqlDb.dfm haben extreme Ähnlichkeiten – kann der DFM Code irgendwie vererbt werden?
- Beim ersten Start nicht Localhost abprüfen (da sonst unnötiger Timeout auf BAPs), sondern nach Server fragen.
- Weitere Parameter wie z.B. User+Passwort anstelle NT Authentifizierung
- Probleme bei unterschiedlicher DPI-Einstellung
- ~~Wenn man runterscrollt und was klickt, scrollt er hoch und somit klickt man was falsches
  Grund: Reload der Tabellen-Liste beim EditExit~~
  => OnExit Event entfernt. Man muss nun "Aktualisieren" klicken.
- ~~Falschen Servernamen eingeben => Abbrechen klicken => Es wird trotzdem ein Login-Versuch gestartet.~~ => OnExit Event entfernt. Man muss nun "Aktualisieren" klicken.

Tabellen => Felder

- Die Spalten werden kleiner gemacht. Hier muss wohl Width besser gesetzt werden.

Optionen-dialog

- Farben funktionieren nicht (haben nie funktioniert)

- Admin Elevation bei MakeStandard()

Wenn man auf "Weitersuchen" das erste Mal klickt (es kommt der Suchen-Dialog) und dann sucht, dann kommt die Such-Abschlussmeldung (nix gefunden) zweimal.

**Achtung**: So Dinge wie die Eck-Klammer-Syntax [] oder sp\_rename existieren in anderen Datenbanken nicht. Das muss berücksichtigt werden.

Future: SQLite

Eine Konfigurationsklasse einführen

Rechtsklick zeigt nur "RTF Bearbeiten", aber nicht die normale Plaintext bearbeitung (doppelklick)

Auch TempDB anzeigen

Verschieben einer Tabelle in eine andere Datenbank:

- Man darf nicht zu sich selbst kopieren
- "Feldtyp LongInt als gefährlich eingestuft" Meldung prüfen/entfernen
- Wenn existiert, dann folgende Optionen
  - Abbrechen
  - Löschen und Neu anlegen
  - Anhängen
    - Wenn PK existiert, überschreiben oder nicht

Auswahlbasierter Filter bei DateTime funktioniert nicht

(RH sagte etwas von einer Verschlechterung? Backspace Taste?)

"Spalte ausblenden" entf.

"Längster Wert" => Dauert zu lange => schneller mit SQL Befehl?

SQL Log anlegen

CORA-Mode soll keine NT Authentifizierung mehr machen, sondern ein Passwort

Idee: Möglichkeit bieten, Datensätze von 1 Tabelle in die andere zu kopieren. Z.B. Daten aus einer CORA-Benutzertabelle in die CORA-Basistabelle

Connection-Timeout ist mittlerweile zu kurz… bei manchen langsamen PCs kommt die Verbindung nicht zustande.

    `	`SELECT
    
    `		`DATUM,
    
    `		`CAST(FLOOR(CAST(DATUM AS FLOAT)) AS DATETIME)
    
    `	`FROM
    
    `		`dbo.BELEGEWABASIS bas

`	`--> bei dem zweiten (nur Datum) kann ich die Datepicker dropdownbox nicht aufmachen

Man kann einen deadlock erzeugen...
- 1. Tabelleneditor: Datensatz mit ungültigen Daten in den Editiermodus versetzen (z.B. PK-Konflikt)
- 2. (ohne zu posten) : in das filter-fenster
- 3. Filter setzen
- 4. zurück zur Tabellenansicht: es wird ein Post versucht, der fehlschlägt (z.B. PK Konflikt), aber man kommt aus dem filter-fenster nicht mehr raus

"text" Felder (z.B. BEDIENER.ZUGANG) sind nicht wie im SQL-Studio dargestellt. Es wird nur die erste Zeile gezeigt. SQL-Studio blendet die Linebreaks aus.

ADO-Table ignoriert Groß und Kleinschreibung beim Filter. Lösung:

```
ADOTable1.Filtered := False;
ADOTable1.FilterOptions := [foCaseInsensitive];
ADOTable1.Filter   := '(BELEG = ''rec'')';
ADOTable1.Filtered := True;
```

---

beim directsql (query) soll man auch viel machen können im kontextmenü

\+ wenn man auf exportieren klickt, dann verschwindet die tabelle kurz

---

man darf immer noch keine `/* TODO: */` in directsql schreiben

---

lbdbd: kann usertabellen nicht löschen wegen foreign key constraint... kann man da was machen?

lbdbd: inplace edit für varchar(max)

lbdbd/dbtool: wenn alle fenster geschlossen, dann sql connection dialog erneut öffnen

lbdbd: im neuen abfragefenster geht f9 nicht

lbdbd: im neuen sql query fenster ahbe ich ganz viele meldungen bekommen '' ist kein gültiger integerwert

lbdbd: wenn man enter drückt im "name des datenbankservers" und keine db ausgewählt, dann soll er aktualisieren und fokus auf die db

lbdbd esc soll fenster schließen (in cora auch)

lbdbd: er setzt die default-werte nicht richtig. man muss nach insert erst reloaden

lbdbd : fragen, ob text im direktsql gespeichert werden soll oder nicht
