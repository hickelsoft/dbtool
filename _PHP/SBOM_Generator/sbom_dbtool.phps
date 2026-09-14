<?php

/*
 * HickelSOFT SBOM-Generator for DBTool
 * (C) 2026 HickelSOFT Huth GmbH
 */

include __DIR__.'/sbom.inc.php';

// --- Base config ---

define("VERBOSE", false);

$cfg_output_file = "C:\\HS-Service\\DBTool\\sbom.xml";
//$cfg_output_file = __DIR__ . '\\test_sbom.xml';

$cfg_csharp_dirs = [
];

$cfg_delphi_dir = "C:\\SVN\\Delphi\\";

$cfg_bin_dirs = [
	"bin" => ["C:\\HS-Service\\DBTool\\", "C:\\Program Files\\HickelSOFT\\DBTool 1.0\\"]
];

$cfg_doc_dirs = [
];

$cfg_report_dirs = [
];

$main_versioninfo = getVersionInfo($cfg_bin_dirs["bin"][0]."DBTool64.exe");
$cfg_name = "DBTool ".$main_versioninfo["ProductVersion"];
$cfg_version = $main_versioninfo["FileVersion"]."+svn".getSvnRev($cfg_delphi_dir."\\DBTool");
$cfg_desc = "Database Desktop for Windows";
$cfg_company = $main_versioninfo["CompanyName"];
$cfg_copyright = $main_versioninfo["LegalCopyright"];
$cfg_domain = "https://www.hickelsoft.de";
$cfg_person = "Daniel Marschall";
$cfg_email = "dmarschall@hickelsoft.de";
$cfg_license = "Apache-2.0";
unset($main_versioninfo);

$cfg_generator_name = "HickelSOFT SBOM Generator";

$cfg_own_extensions = [ ]; // Eigenes Copyright angeben
$cfg_ignore = [ "*.drc", "*.rsm", "*.map", "*.dcu", "sbom.json", "sbom.xml", "sbom.pem", "Setup.ini", "unins000.dat", "unins000.msg" ];

// --- VCL ---

$cfg_manual_vcl = []; // VCLs not in BPL detected

$cfg_delphi_external_units_exclude = function(string $pathname): bool {
	if (hickel_pathname_is_forbidden($pathname)) return false;
	if (hickel_pathname_is_foreign($pathname)) return false;

	// CORA Zeug hat in DBTool nichts zu suchen
	if (hickel_pathname_is_cora($pathname)) return false;

	// HS-Info Zeug hat in CORA nichts zu suchen
	if (hickel_pathname_is_hsinfo2($pathname)) return false;

	return true;
};

$cfg_dcc_prefixes = explode(';', 'Vcl;Vcl.Imaging;Vcl.Touch;Vcl.Samples;Vcl.Shell;System;Xml;Data;Datasnap;Web;Soap;Winapi;Data.Win;System.Win;Xml.Win;FMX;VCLTee;IBX;BDE');

// Pascal Dateien in HICKEL_VCL.bpl zählen zu unserem eigenen Code
// und nicht als Fremdcode. Folglich müssen wir die Komponente hier definieren.
// Hinweis: Würden wir HICKEL_VCL in findPascalFiles() ausschließen,
// dann würden die Units korrekt der BPL zugeordnet werden, und dann müssten
// wir sie hier nicht beschreiben. *ABER* das hätte den großen Nachteil,
// dass Abhängigkeiten die wir in der HICKEL_VCL verwenden nicht mehr erfasst
// werden können. Und wir wollen für unser VCL/RTL nicht extra ein eigenes SBOM machen.
$cfg_delphi_extra_bpl = [
	"HICKEL_RTL.bpl",
	"HICKEL_VCL.bpl"
];

// --- Runtimes ---

$ver_bde = getVersionInfo($cfg_delphi_dir."\\DBTool\\Setup\\BDE_Setup.exe");
$fb3032_ver = explode("-",trim(file($cfg_bin_dirs["bin"][0]."\\FB30_32\\_VERSION.TXT")[1]))[1];
$fb3064_ver = explode("-",trim(file($cfg_bin_dirs["bin"][0]."\\FB30_64\\_VERSION.TXT")[1]))[1];
$fb4032_ver = explode("-",trim(file($cfg_bin_dirs["bin"][0]."\\FB40_32\\_VERSION.TXT")[1]))[1];
$fb4064_ver = explode("-",trim(file($cfg_bin_dirs["bin"][0]."\\FB40_64\\_VERSION.TXT")[1]))[1];
$fb5032_ver = explode("-",trim(file($cfg_bin_dirs["bin"][0]."\\FB50_32\\_VERSION.TXT")[1]))[1];
$fb5064_ver = explode("-",trim(file($cfg_bin_dirs["bin"][0]."\\FB50_64\\_VERSION.TXT")[1]))[1];
$cfg_runtimes = [
	[
		"type" => "library",
		"name" => "MSDASQL through ADO (for MySQL Support)",
		"version" => "",
		"company" => "Microsoft Corporation",
		"copyright" => "(C) Microsoft Corporation.  All rights reserved.",
		"license" => "Proprietary",
		"scope" => "optional",
		"vts:disposition" => "external",
		"vts:discovery" => "manual",
		"vts:discovered_by" => $cfg_generator_name
	],
	[
		"type" => "library",
		"name" => "SQLOLEDB through ADO (for SQL Server Support)",
		"version" => "",
		"company" => "Microsoft Corporation",
		"copyright" => "(C) Microsoft Corporation.  All rights reserved.",
		"license" => "Proprietary",
		"scope" => "optional",
		"vts:disposition" => "external",
		"vts:discovery" => "manual",
		"vts:discovered_by" => $cfg_generator_name
	],
	[
		"type" => "library",
		"name" => "SQLNCLI10 through ADO (for SQL Server Support)",
		"version" => "",
		"company" => "Microsoft Corporation",
		"copyright" => "(C) Microsoft Corporation.  All rights reserved.",
		"license" => "Proprietary",
		"scope" => "optional",
		"vts:disposition" => "external",
		"vts:discovery" => "manual",
		"vts:discovered_by" => $cfg_generator_name
	],
	[
		"type" => "library",
		"name" => "SQLNCLI11 through ADO (for SQL Server Support)",
		"version" => "",
		"company" => "Microsoft Corporation",
		"copyright" => "(C) Microsoft Corporation.  All rights reserved.",
		"license" => "Proprietary",
		"scope" => "optional",
		"vts:disposition" => "external",
		"vts:discovery" => "manual",
		"vts:discovered_by" => $cfg_generator_name
	],
	[
		"type" => "library",
		"name" => "MSOLEDBSQL through ADO (for SQL Server Support)",
		"version" => "",
		"company" => "Microsoft Corporation",
		"copyright" => "(C) Microsoft Corporation.  All rights reserved.",
		"license" => "Proprietary",
		"scope" => "optional",
		"vts:disposition" => "external",
		"vts:discovery" => "manual",
		"vts:discovered_by" => $cfg_generator_name
	],
	[
		"type" => "library",
		"name" => "MSOLEDBSQL19 through ADO (for SQL Server Support)",
		"version" => "19",
		"company" => "Microsoft Corporation",
		"copyright" => "(C) Microsoft Corporation.  All rights reserved.",
		"license" => "Proprietary",
		"scope" => "optional",
		"vts:disposition" => "external",
		"vts:discovery" => "manual",
		"vts:discovered_by" => $cfg_generator_name
	],
	[
		"type" => "library",
		"name" => "BDE (for Paradox Support)",
		"version" => $ver_bde["ProductVersion"],
		"company" => "Borland Software Corporation",
		"copyright" => "(C) 1994-2001 Borland Software Corporation",
		"license" => "Proprietary",
		"scope" => "optional",
		"vts:disposition" => "external",
		"vts:discovery" => "manual",
		"vts:discovered_by" => $cfg_generator_name
	],
	[
		"type" => "library",
		"name" => "Interbase through Interbase VCL (for Interbase Support)",
		"version" => "",
		"company" => "Embarcadero Technologies, Inc.",
		"copyright" => "",
		"license" => "Proprietary",
		"scope" => "optional",
		"vts:disposition" => "external",
		"vts:discovery" => "manual",
		"vts:discovered_by" => $cfg_generator_name
	],
	[
		"type" => "library",
		"name" => "Firebird through FireDAC with FireBird Client Driver (for Firebird Support)",
		"version" => "",
		"company" => "Embarcadero Technologies, Inc.",
		"copyright" => "",
		"license" => "Proprietary",
		"scope" => "optional",
		"vts:disposition" => "external",
		"vts:discovery" => "manual",
		"vts:discovered_by" => $cfg_generator_name
	],
	[
		"type" => "library",
		"name" => "Firebird Client 3.0, 32 Bits",
		"version" => $fb3032_ver,
		"company" => "Firebird Foundation",
		"copyright" => "",
		"license" => "Initial Developer's Public License",
		"scope" => "optional",
		"vts:disposition" => "external",
		"vts:discovery" => "manual",
		"vts:discovered_by" => $cfg_generator_name
	],
	[
		"type" => "library",
		"name" => "Firebird Client 3.0, 64 Bits",
		"version" => $fb3064_ver,
		"company" => "Firebird Foundation",
		"copyright" => "",
		"license" => "Initial Developer's Public License",
		"scope" => "optional",
		"vts:disposition" => "external",
		"vts:discovery" => "manual",
		"vts:discovered_by" => $cfg_generator_name
	],
	[
		"type" => "library",
		"name" => "Firebird Client 4.0, 32 Bits",
		"version" => $fb4032_ver,
		"company" => "Firebird Foundation",
		"copyright" => "",
		"license" => "Initial Developer's Public License",
		"scope" => "optional",
		"vts:disposition" => "external",
		"vts:discovery" => "manual",
		"vts:discovered_by" => $cfg_generator_name
	],
	[
		"type" => "library",
		"name" => "Firebird Client 4.0, 64 Bits",
		"version" => $fb4064_ver,
		"company" => "Firebird Foundation",
		"copyright" => "",
		"license" => "Initial Developer's Public License",
		"scope" => "optional",
		"vts:disposition" => "external",
		"vts:discovery" => "manual",
		"vts:discovered_by" => $cfg_generator_name
	],
	[
		"type" => "library",
		"name" => "Firebird Client 5.0, 32 Bits",
		"version" => $fb5032_ver,
		"company" => "Firebird Foundation",
		"copyright" => "",
		"license" => "Initial Developer's Public License",
		"scope" => "optional",
		"vts:disposition" => "external",
		"vts:discovery" => "manual",
		"vts:discovered_by" => $cfg_generator_name
	],
	[
		"type" => "library",
		"name" => "Firebird Client 5.0, 64 Bits",
		"version" => $fb5064_ver,
		"company" => "Firebird Foundation",
		"copyright" => "",
		"license" => "Initial Developer's Public License",
		"scope" => "optional",
		"vts:disposition" => "external",
		"vts:discovery" => "manual",
		"vts:discovered_by" => $cfg_generator_name
	],
	[
		"type" => "library",
		"name" => "Mirosoft.Jet.OLEDB through ADO (for Access 97 Support)",
		"version" => "",
		"company" => "Microsoft Corporation",
		"copyright" => "(C) Microsoft Corporation.  All rights reserved.",
		"license" => "Proprietary",
		"scope" => "optional",
		"vts:disposition" => "external",
		"vts:discovery" => "manual",
		"vts:discovered_by" => $cfg_generator_name
	],
	[
		"type" => "library",
		"name" => "Microsoft.ACE.OLEDB through ADO (for Access Support)",
		"version" => "",
		"company" => "Microsoft Corporation",
		"copyright" => "(C) Microsoft Corporation.  All rights reserved.",
		"license" => "Proprietary",
		"scope" => "optional",
		"vts:disposition" => "external",
		"vts:discovery" => "manual",
		"vts:discovered_by" => $cfg_generator_name
	],
];

// --- nuGet ---

$cfg_nuget_supplierMap = [
];

// --- License/Company/Copyright override ---

$cfg_license_company_copyright_override = [
	"INDY" => [
		"license" => "MPL-1.1"
	],
	"KJCLDEBUGONLY.BPL" => [ // JEDI JCL, but only JclDebug in a DPK
		"license" => "MPL-1.1"
	],
	"HICKEL_RTL.BPL" => [
		"license" => "Proprietary, partially Apache-2.0"
	],
	"HICKEL_VCL.BPL" => [
		"license" => "Proprietary, partially Apache-2.0"
	]
];

// --- SBOM erzeugen ---

$cfg_merge_cb = function(&$sbom) {
};

$cfg_json_final_cb = function(string &$sbom_cont) {
};

sbom_generate();
