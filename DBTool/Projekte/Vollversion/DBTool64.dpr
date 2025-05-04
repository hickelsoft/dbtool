program DBTool64;

uses
  Forms,
  hl_MultiLang,
  {$IF CompilerVersion < 20.0}
  VCL_Hotfix_AltGr,
  {$IFEND }
  Main in '..\..\Main.pas' {DLG_Main},
  Database in '..\..\Database.pas' {MDI_Database},
  Table in '..\..\Table.pas' {MDI_Table},
  Info in '..\..\Info.pas' {DLG_Info},
  OpenMySqlDb in '..\..\OpenMySqlDb.pas' {DLG_OpenMySqlDb},
  IbDatabaseName in '..\..\IbDatabaseName.pas' {DLG_IbDatabaseName},
  Optionen in '..\..\Optionen.pas' {DLG_Optionen},
  Export in '..\..\Export.pas' {DLG_Export},
  Import in '..\..\Import.pas' {DLG_Import},
  DateiNeu in '..\..\DateiNeu.pas' {DLG_DateiNeu},
  OpenSqlDb in '..\..\OpenSqlDb.pas' {DLG_OpenSqlDb},
  EditRTF in '..\..\EditRTF.pas' {DLG_EditRTF},
  Query in '..\..\Query.pas' {MDI_Query},
  C_Database in '..\..\C_Database.pas',
  Globals in '..\..\Globals.pas';

{$R *.res}

resourcestring
  SDBToolTitle = 'Datenbankoberfläche';

begin
  HS_SwitchLanguage;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;

  Application.Title := SDBToolTitle;
  Application.CreateForm(TDLG_Main, DLG_Main);
  //Application.CreateForm(TMDI_Database, MDI_Database);
//Application.CreateForm(TMDI_Table, MDI_Table);
  Application.CreateForm(TDLG_Info, DLG_Info);
  Application.CreateForm(TDLG_OpenMySqlDb, DLG_OpenMySqlDb);
  Application.CreateForm(TDLG_IbDatabaseName, DLG_IbDatabaseName);
  Application.CreateForm(TDLG_Optionen, DLG_Optionen);
  Application.CreateForm(TDLG_Export, DLG_Export);
  Application.CreateForm(TDLG_Import, DLG_Import);
  Application.CreateForm(TDLG_DateiNeu, DLG_DateiNeu);
  Application.CreateForm(TDLG_OpenSqlDb, DLG_OpenSqlDb);
  Application.CreateForm(TDLG_EditRTF, DLG_EditRTF);
//Application.CreateForm(TMDI_Query, MDI_Query);
  Application.Run;
end.

