# Database Desktop / DBTool

**HickelSOFT Database Desktop** (also known as **DBTool**) is a straightforward and fast database management application for Windows, supporting both 32-bit and 64-bit systems. It handles a variety of DBMS including SQL Server, MySQL, Access, and more. The tool allows for easy table export, import from CSV files, and drag-and-drop table copying across different database connections. With multilingual support (English and German) and quick access to common commands, DBTool is free for commercial use under the Apache 2.0 license.

## Download

**[Setup for Win32 and Win64](https://github.com/hickelsoft/dbtool/raw/main/Distribution/DBTool_Setup.exe)** (approx. 10 MB)

## Features

- Easy to use

- Multilingual (English and German)

- Free, even for commercial use. Licensed under the terms of the Apache 2.0 license

- 32-bit and 64-bit Windows Application

- DBMS supported:

	* SQL Server
	* MySQL
 	* Access97/Jet (.mdb)
	* Access (.accdb)
	* Interbase
	* Paradox (.db), only 32 bit
	* dBase (.dbf), only 32 bit

- Export a table as:

	* CSV
	* XML
	* HTML
	* dBase
	* Paradox
	* SQL-Dump

- Import a table from a CSV file

- Copy tables from one database window to another database window using Drag'n'Drop, even if there are different connections/DBMS!

- Open a database(s) using command line (`DBTool64.exe "_SQLSRV:...ConnectionString...` or `DBTool64.exe "_MYSQL:...ConnectionString...` or `DBTool32.exe C:\Paradox.db`) or Drag'n'Drop a DB/MDB/ACCDB/... file into the MDI window.

- Program opens very fast, in contrast to tools like SQL Server Management Studio

- Show Table Structure

- Fast commands like Filter by value, max value, min value, count distinct values, etc. are available in the Table windows via right-clicking a column.

- View Tables or Execute SQL Queries

- Show and Edit Views and Stored Procedures

## Technical Restrictions

- Only available for the Microsoft Windows Operating System (7, 8, 8.1, 10, 11, or newer)

- Login via GUI currently only via NT Authentication, not via Username/Password. If you need Username/Password authentication, you can use the command-line to start DBTool, e.g. for SQL Server `DBTool64.exe "_SQLSRV:Provider=SQLOLEDB.1;User ID=John Doe;Password=MyPass123;Persist Security Info=True;Initial Catalog=DATABASENAME1234;Data Source=SERVERNAMEXYZ,49011;Use Encryption for Data=False"`

- Mostly used/tested with Microsoft SQL Server. Other DBMS might not be as stable.

- CANNOT be used to view or modify databases of HickelSOFT CORA*plus* and HickelSOFT HS-Info 2.0

## Screenshots

![Database Desktop](https://raw.githubusercontent.com/hickelsoft/dbtool/main/DBTool/Private/DBTool%20Screenshot.png)

## Source Code

To use and compile the source code, you need:

- Embarcadero Delphi 12

- BDE for Delphi (to support Paradox and dBase)

- For translations in addition: GetIt Package "VCL Translation Support", and a PHP interpreter

- Woll2Woll InfoPower

## License and Restrictions

Free, even for commercial use. Licensed under the terms of the Apache 2.0 license.

The manufacturer of this software is under no circumstances liable for damages resulting from the use of this software!


(C) 1998-2002 Leif Bruder, 2016-2024 HickelSOFT Huth GmbH
