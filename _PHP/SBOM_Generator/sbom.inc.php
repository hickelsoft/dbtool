<?php

/*
 * HickelSOFT SBOM-Generator Utilities
 * (C) 2026 HickelSOFT Huth GmbH
 */

function getVersionInfo(string $file): array {
	if (!file_exists($file)) {
		return [];
	}

	$file = realpath($file);
	$sha256 = hash_file("sha256", $file);

	static $version_cache_file = __DIR__."/versioncache.ser";
	static $version_cache = file_exists($version_cache_file) ? unserialize(file_get_contents($version_cache_file)) : [];
	if (isset($version_cache["v1_".$sha256])) {
		return $version_cache["v1_".$sha256];
	}

	// https://learn.microsoft.com/en-us/dotnet/api/system.diagnostics.fileversioninfo?view=netframework-4.8.1
	$cmd =
		'powershell.exe -NoProfile -NonInteractive -ExecutionPolicy Bypass ' .
		'"$v=(Get-Item \'' . $file . '\').VersionInfo;' .
		'Write-Host \'CompanyName=\'$v.CompanyName;' .
		'Write-Host \'ProductName=\'$v.ProductName;' .
		'Write-Host \'FileDescription=\'$v.FileDescription;' .
		'Write-Host \'LegalCopyright=\'$v.LegalCopyright;' .
		'Write-Host \'ProductVersion=\'$v.ProductVersion;' .
		'Write-Host \'FileVersion=\'$v.FileVersion;' .
		'"';
	$output = shell_exec($cmd);
	if (!$output) {
		return [];
	}
	$result = [];
	foreach (explode("\n", $output) as $line) {
		$line = trim($line);
		if ($line === "") {
			continue;
		}
		[$key, $value] = array_pad(
			explode("=", $line, 2),
			2,
			""
		);
		$result[$key] = trim($value);
	}

	$version_cache["v1_".$sha256] = $result;
	file_put_contents($version_cache_file, serialize($version_cache));

	return $result;
}

function gen_uuid(): string {
	return sprintf( "%04x%04x-%04x-%04x-%04x-%04x%04x%04x",
		random_int( 0, 0xffff ), random_int( 0, 0xffff ),
		random_int( 0, 0xffff ),
		random_int( 0, 0x0fff ) | 0x4000,
		random_int( 0, 0x3fff ) | 0x8000,
		random_int( 0, 0xffff ), random_int( 0, 0xffff ), random_int( 0, 0xffff )
	);
}

function makeBomRef(string $filename): string {
	$name = basename($filename);
	$name = mb_strtolower($name);
	$replace = [
		"ä" => "ae",
		"ö" => "oe",
		"ü" => "ue",
		"ß" => "ss",
	];
	$name = strtr($name, $replace);
	$name = preg_replace("/[^a-z0-9]+/u", "-", $name);
	$name = preg_replace("/-+/", "-", $name);
	$name = trim($name, "-");
	return $name;
}

function isMzExecutable(string $file): bool {
	if (!is_readable($file)) {
		return false;
	}
	$fh = fopen($file, "rb");
	if (!$fh) {
		return false;
	}
	$magic = fread($fh, 2);
	fclose($fh);
	return $magic === "MZ";
}

function getLicenses(string $license): array {
	static $spdx_license_ids = null;
	if ($spdx_license_ids == null) {
		$spdx_license_ids = [];
		$licenses_lookup_file = __DIR__."/licenses.json";
		if (!file_exists($licenses_lookup_file)) {
			$tmp = file_get_contents($url = 'https://raw.githubusercontent.com/spdx/license-list-data/refs/heads/main/json/licenses.json');
			if ($tmp !== false) file_put_contents($licenses_lookup_file, $tmp);
			if (!file_exists($licenses_lookup_file)) {
				throw new Exception("Could not download licenses.json from GitHub ($url)");
			}
		}
		$tmp = json_decode(file_get_contents($licenses_lookup_file),true);
		foreach ($tmp["licenses"] as $tmp2) {
			$spdx_license_ids[] = $tmp2["licenseId"];
		}
	}
	if (in_array($license,$spdx_license_ids)) {
		return [ [ "license" => [ "id" => $license ] ] ];
	} else {
		return [ [ "license" => [ "name" => $license ] ] ];
	}
}

function getSvnRev($dir) {
	$cmd = "\"C:\\Program Files\\TortoiseSVN\\bin\\svn.exe\" info " . escapeshellarg($dir);
	exec($cmd, $output, $returnCode);
	if ($returnCode !== 0) {
		$revision = null;
	} else {
		$revision = null;
		foreach ($output as $line) {
			if (preg_match("/^Revision:\s*(\d+)/i", $line, $matches)) {
				$revision = (int)$matches[1];
				break;
			}
		}
	}
	if (!$revision) echo "Warning: Cannot determine SVN revision of dir $dir\n";
	return $revision ?? "Unknown";
}

# ------------------------------------------------------------------------------

function _verarbeite_bin_dirs(array $metadata, array &$components, array &$unterkomponenten, array $dirs): void {
	foreach ($dirs as $prefix => $dir_info) {
		$dir = $dir_info[0];
		$install_location = $dir_info[1];
		_verarbeite_bin_dir($metadata, $components, $unterkomponenten, $prefix, $dir, $install_location);
	}
}

function _verarbeite_bin_dir(array $metadata, array &$components, array &$unterkomponenten, string $ref_prefix, string $dir, string $install_location=""): void {
	foreach (glob($dir . "\\*.*") as $file) {
		$path_info = pathinfo($file);
		global $cfg_ignore;
		foreach ($cfg_ignore as $ignore) {
			if (fnmatch($ignore, $path_info["basename"])) {
				if (VERBOSE) echo "Ignore: $file ...\n";
				continue 2;
			}
		}
		if (VERBOSE) echo "Processing: $file ...\n";
		$md5 = hash_file("md5", $file);
		$sha1 = hash_file("sha1", $file);
		$sha256 = hash_file("sha256", $file);
		$pe_file = $file;
		if (in_array($path_info["extension"], ["pdb", "xml"], true)) {
			$dll_file = $path_info["dirname"] . DIRECTORY_SEPARATOR . $path_info["filename"] . ".dll"; // z.B. test.pdb -> test.dll
			if (file_exists($dll_file)) {
				$pe_file = $dll_file;
			}
			$exe_file = $path_info["dirname"] . DIRECTORY_SEPARATOR . $path_info["filename"] . ".exe"; // z.B. test.pdb -> test.exe
			if (file_exists($exe_file)) {
				$pe_file = $exe_file;
			}
		} elseif ($path_info["extension"] === "config") {
			if (str_ends_with(strtolower($path_info["basename"]), ".dll.config")) { // z.B. test.dll.config -> test.dll
				$dll_filename = preg_replace("/\.dll$/i", "", $path_info["filename"]);
				$dll_file = $path_info["dirname"] . DIRECTORY_SEPARATOR . $dll_filename . ".dll";
				if (file_exists($dll_file)) {
					$pe_file = $dll_file;
				}
			} else if (str_ends_with(strtolower($path_info["basename"]), ".exe.config")) { // z.B. test.exe.config -> test.exe
				$exe_filename = preg_replace("/\.exe$/i", "", $path_info["filename"]);
				$exe_file = $path_info["dirname"] . DIRECTORY_SEPARATOR . $exe_filename . ".exe";
				if (file_exists($exe_file)) {
					$pe_file = $exe_file;
				}
			}
		}

		if (isMzExecutable($pe_file)) {
			$versioninfo = getVersionInfo($pe_file);
		} else {
			$versioninfo = [];
		}

		$company = $versioninfo["CompanyName"] ?? "";
		$license = "";
		$copyright = $versioninfo["LegalCopyright"] ?? "";
		global $cfg_own_extensions, $cfg_company, $cfg_license, $cfg_copyright;
		if (in_array(strtolower($path_info["extension"]),$cfg_own_extensions)) {
			if (!$company) $company = $cfg_company;
			if (!$license) $license = $cfg_license;
			if (!$copyright) $copyright = $cfg_copyright;
		} else if (($company != "") && ($company == $cfg_company)) {
			if (!$company) $company = $cfg_company;
			if (!$license) $license = $cfg_license;
			if (!$copyright) $copyright = $cfg_copyright;
		} else {
			if (str_ends_with(strtolower($pe_file), ".dll")) {
				global $cfg_nuget_supplierMap;
				foreach ($cfg_nuget_supplierMap as $prefix => $nuget) {
					if (str_starts_with($path_info["basename"], $prefix)) {
						if ($nuget["company"] && !$company)
							$company = $nuget["company"];
						if ($nuget["license"] && !$license)
							$license = $nuget["license"];
						if ($nuget["copyright"] && !$copyright)
							$copyright = $nuget["copyright"];
						break;
					}
				}
			}
		}
		global $cfg_license_company_copyright_override;
		foreach ($cfg_license_company_copyright_override as $override_file => $overrides) {
			if (str_starts_with(strtolower($path_info["basename"]), strtolower($override_file))) {
				if (($overrides["company"] ?? "") != "") $company = $overrides["company"];
				if (($overrides["license"] ?? "") != "") $license = $overrides["license"];
				if (($overrides["copyright"] ?? "") != "") $copyright = $overrides["copyright"];
			}
		}
		if ($license == "" && $company == "") echo "Warning: License and Company unknown: ".$path_info["basename"]."\n";
		else if ($license == "") echo "Warning: License unknown: ".$path_info["basename"]."\n";
		else if ($company == "") echo "Warning: Company unknown: ".$path_info["basename"]."\n";

		if (!$company) $company = "Unknown";
		if (!$license) $license = "Unknown";
		//if (!$copyright) $license = "$copyright";

		$component = [];
		if ((strtolower($path_info["extension"]) == "exe") || (strtolower($path_info["extension"]) == "com")) {
			$component["type"] = "application";
		} else if ((strtolower($path_info["extension"]) == "dll") || (strtolower($path_info["extension"]) == "bpl")) {
			$component["type"] = "library";
		} else {
			$component["type"] = "file";
		}
		$component["name"] = $path_info["basename"];
		$component["version"] = $versioninfo["FileVersion"] ?? "";
		$component["bom-ref"] = $ref_prefix . "-" . makeBomRef($path_info["basename"]);
		$component["supplier"] = [ "name" => $company ];
		$component["manufacturer"] = [ "name" => $company ];
		$component["publisher"] = $company;
		if (strtolower($path_info["extension"]) == "phpx") {
			$component["description"] = "Encrypted PHP Script (runs via Mini-PHP)";
		} else if (strtolower($path_info["extension"]) == "dbc") {
			$component["description"] = "Database Checksum File";
		} else {
			$component["description"] = $versioninfo["FileDescription"] ?? "";
		}
		$component["scope"] = "required";
		$component["licenses"] = getLicenses($license);
		$component["copyright"] = $copyright;
		$component["hashes"] = [
			[ "alg" => "MD5", "content" => $md5 ],
			[ "alg" => "SHA-1", "content" => $sha1 ],
			[ "alg" => "SHA-256", "content" => $sha256 ]
		];
		global $cfg_generator_name;
		$component["properties"] = [
			[
				"name" => "bsi:component:executable",
				"value" => _type_executable($path_info["basename"]) ? "executable" : "non-executable"
			],
			[
				"name" => "bsi:component:archive",
				"value" => _type_archive($path_info["basename"]) ? "archive" : "no archive"
			],
			[
				"name" => "bsi:component:structured",
				"value" => _type_structured($path_info["basename"]) ? "structured" : "unstructured"
			],
			[
				"name" => "bsi:component:filename",
				"value" => $path_info["basename"]
			],
			[
				"name" => "vts:disposition",
				"value" => "artifact"
			],
			[
				"name" => "vts:discovery",
				"value" => "filesystem"
			],
			[
				"name" => "vts:discovered_by",
				"value" => $cfg_generator_name
			],
			[
				"name" => "vts:discovery:confidence",
				"value" => "confirmed"
			]
		];
		if (strtolower($path_info["extension"]) == "phpx") {
			$component["properties"][] = [
				"name" => "vts:encrypted",
				"value" => "yes"
			];
		}
		if (strtolower($path_info["extension"]) == "dbc") {
			$component["properties"][] = [
				"name" => "vts:obfuscated",
				"value" => "yes"
			];
		}
		if (_type_archive($path_info["basename"]) && strtolower($path_info["extension"]) != "tar") {
			$component["properties"][] = [
				"name" => "vts:compressed",
				"value" => "yes"
			];
		}
		if ($install_location != "") {
			$component["properties"][] = [
				"name" => "vts:install_location",
				"value" => $install_location.$path_info["basename"]
			];
		}
		if (in_array($path_info["extension"], ["exe", "dll"], true)) {
			if (file_exists(substr($file,0,strlen($file)-4).'.pdb')) {
				$component["properties"][] = [
					"name" => "vts:debuginfo",
					"value" => "external"
				];
			}
		}
		$tmp = $versioninfo["CompanyName"] ?? "";
		if ($tmp != "") $component["properties"][] = [ "name" => "vts:3p:msft:pe:versioninfo:companyName", "value" => $tmp ];
		$tmp = $versioninfo["ProductName"] ?? "";
		if ($tmp != "") $component["properties"][] = [ "name" => "vts:3p:msft:pe:versioninfo:productName", "value" => $tmp ];
		$tmp = $versioninfo["FileDescription"] ?? "";
		if ($tmp != "") $component["properties"][] = [ "name" => "vts:3p:msft:pe:versioninfo:fileDescription", "value" => $tmp ];
		$tmp = $versioninfo["LegalCopyright"] ?? "";
		if ($tmp != "") $component["properties"][] = [ "name" => "vts:3p:msft:pe:versioninfo:legalCopyright", "value" => $tmp ];
		$tmp = $versioninfo["ProductVersion"] ?? "";
		if ($tmp != "") $component["properties"][] = [ "name" => "vts:3p:msft:pe:versioninfo:productVersion ", "value" => $tmp ];
		$tmp = $versioninfo["FileVersion"] ?? "";
		if ($tmp != "") $component["properties"][] = [ "name" => "vts:3p:msft:pe:versioninfo:fileVersion ", "value" => $tmp ];
		global $cfg_delphi_dir;
		if ($path_info["basename"] == "mini_php.exe") {
			$component["components"] = [];
			_verarbeite_bin_dir($metadata, $component["components"], $unterkomponenten, "php32", $cfg_delphi_dir."\\MiniPHP\\php_files\\");
		}
		if ($path_info["basename"] == "mini_php64.exe") {
			$component["components"] = [];
			_verarbeite_bin_dir($metadata, $component["components"], $unterkomponenten, "php64", $cfg_delphi_dir."\\MiniPHP\\php_files64\\");
		}
		$components[] = $component;
		if ($ref_prefix == "php32") {
			if (!isset($unterkomponenten["bin-mini-php-exe"])) $unterkomponenten["bin-mini-php-exe"] = [];
			$unterkomponenten["bin-mini-php-exe"][] = $component["bom-ref"];
		} else if ($ref_prefix == "php64") {
			if (!isset($unterkomponenten["bin-mini-php64-exe"])) $unterkomponenten["bin-mini-php64-exe"] = [];
			$unterkomponenten["bin-mini-php64-exe"][] = $component["bom-ref"];
		} else {
			if (!isset($unterkomponenten[$metadata["component"]["bom-ref"]])) $unterkomponenten[$metadata["component"]["bom-ref"]] = [];
			$unterkomponenten[$metadata["component"]["bom-ref"]][] = $component["bom-ref"];
		}
	}
}

function _verarbeite_report_dirs(array $metadata, array &$components, array &$unterkomponenten, array $dirs): void {
	foreach ($dirs as $prefix => $dir_info) {
		$dir = $dir_info[0];
		$install_location = $dir_info[1];
		_verarbeite_report_dir($metadata, $components, $unterkomponenten, $prefix, $dir, $install_location);
	}
}

function _verarbeite_report_dir(array $metadata, array &$components, array &$unterkomponenten, string $ref_prefix, string $dir, string $install_location=""): void {
	foreach (glob($dir . "\\*.rpt") as $file) {
		$path_info = pathinfo($file);
		global $cfg_ignore;
		foreach ($cfg_ignore as $ignore) {
			if (fnmatch($ignore, $path_info["basename"])) {
				if (VERBOSE) echo "Ignore: $file ...\n";
				continue 2;
			}
		}
		if (VERBOSE) echo "Processing: $file ...\n";
		global $cfg_company, $cfg_license;
		$component = [];
		$component["type"] = "file";
		$component["name"] = $path_info["basename"];
		$component["version"] = date("Y.m.d.His", filemtime($file));
		$component["bom-ref"] = $ref_prefix . "-" . makeBomRef($path_info["basename"]);
		$component["supplier"] = [ "name" => $cfg_company ];
		$component["manufacturer"] = [ "name" => $cfg_company ];
		$component["publisher"] = $cfg_company;
		$component["description"] = "Crystal Reports Report File";
		$component["scope"] = "optional";
		$component["licenses"] = getLicenses($cfg_license);
		$component["copyright"] = "(C) ".date("Y", filemtime($file))." ".$cfg_company;
		$component["hashes"] = [
			[ "alg" => "MD5", "content" => hash_file("md5", $file) ],
			[ "alg" => "SHA-1", "content" => hash_file("sha1", $file) ],
			[ "alg" => "SHA-256", "content" => hash_file("sha256", $file) ]
		];
		global $cfg_generator_name;
		$component["properties"] = [
			[
				"name" => "bsi:component:executable",
				"value" => _type_executable($path_info["basename"]) ? "executable" : "non-executable"
			],
			[
				"name" => "bsi:component:archive",
				"value" => _type_archive($path_info["basename"]) ? "archive" : "no archive"
			],
			[
				"name" => "bsi:component:structured",
				"value" => _type_structured($path_info["basename"]) ? "structured" : "unstructured"
			],
			[
				"name" => "bsi:component:filename",
				"value" => $path_info["basename"]
			],
			[
				"name" => "vts:disposition",
				"value" => "artifact"
			],
			[
				"name" => "vts:discovery",
				"value" => "filesystem"
			],
			[
				"name" => "vts:discovered_by",
				"value" => $cfg_generator_name
			],
			[
				"name" => "vts:discovery:confidence",
				"value" => "confirmed"
			]
		];
		if (strtolower($path_info["extension"]) == "phpx") {
			$component["properties"][] = [
				"name" => "vts:encrypted",
				"value" => "yes"
			];
		}
		if (strtolower($path_info["extension"]) == "dbc") {
			$component["properties"][] = [
				"name" => "vts:obfuscated",
				"value" => "yes"
			];
		}
		if (_type_archive($path_info["basename"]) && strtolower($path_info["extension"]) != "tar") {
			$component["properties"][] = [
				"name" => "vts:compressed",
				"value" => "yes"
			];
		}
		if ($install_location != "") {
			$component["properties"][] = [
				"name" => "vts:install_location",
				"value" => $install_location.$path_info["basename"]
			];
		}
		$components[] = $component;
		if (!isset($unterkomponenten[$metadata["component"]["bom-ref"]])) $unterkomponenten[$metadata["component"]["bom-ref"]] = [];
		$unterkomponenten[$metadata["component"]["bom-ref"]][] = $component["bom-ref"];
	}
}

function _verarbeite_doc_dirs(array $metadata, array &$components, array &$unterkomponenten, array $dirs): void {
	foreach ($dirs as $prefix => $dir_info) {
		$dir = $dir_info[0];
		$install_location = $dir_info[1];
		_verarbeite_doc_dir($metadata, $components, $unterkomponenten, $prefix, $dir, $install_location);
	}
}

function _verarbeite_doc_dir(array $metadata, array &$components, array &$unterkomponenten, string $ref_prefix, string $dir, string $install_location=""): void {
	foreach (glob($dir . "\\*.pdf") as $file) {
		$path_info = pathinfo($file);
		global $cfg_ignore;
		foreach ($cfg_ignore as $ignore) {
			if (fnmatch($ignore, $path_info["basename"])) {
				if (VERBOSE) echo "Ignore: $file ...\n";
				continue 2;
			}
		}
		if (VERBOSE) echo "Processing: $file ...\n";
		global $cfg_company, $cfg_license;
		$component = [];
		$component["type"] = "file";
		$component["name"] = $path_info["basename"];
		$component["version"] = date("Y.m.d.His", filemtime($file));
		$component["bom-ref"] = $ref_prefix . "-" . makeBomRef($path_info["basename"]);
		$component["supplier"] = [ "name" => $cfg_company ];
		$component["manufacturer"] = [ "name" => $cfg_company ];
		$component["publisher"] = $cfg_company;
		$component["description"] = "Software Manual / Documentation";
		$component["scope"] = "optional";
		$component["licenses"] = getLicenses($cfg_license);
		$component["copyright"] = "(C) ".date("Y", filemtime($file))." ".$cfg_company;
		$component["hashes"] = [
			[ "alg" => "MD5", "content" => hash_file("md5", $file) ],
			[ "alg" => "SHA-1", "content" => hash_file("sha1", $file) ],
			[ "alg" => "SHA-256", "content" => hash_file("sha256", $file) ]
		];
		global $cfg_generator_name;
		$component["properties"] = [
			[
				"name" => "bsi:component:executable",
				"value" => _type_executable($path_info["basename"]) ? "executable" : "non-executable"
			],
			[
				"name" => "bsi:component:archive",
				"value" => _type_archive($path_info["basename"]) ? "archive" : "no archive"
			],
			[
				"name" => "bsi:component:structured",
				"value" => _type_structured($path_info["basename"]) ? "structured" : "unstructured"
			],
			[
				"name" => "bsi:component:filename",
				"value" => $path_info["basename"]
			],
			[
				"name" => "vts:disposition",
				"value" => "artifact"
			],
			[
				"name" => "vts:discovery",
				"value" => "filesystem"
			],
			[
				"name" => "vts:discovered_by",
				"value" => $cfg_generator_name
			],
			[
				"name" => "vts:discovery:confidence",
				"value" => "confirmed"
			]
		];
		if (strtolower($path_info["extension"]) == "phpx") {
			$component["properties"][] = [
				"name" => "vts:encrypted",
				"value" => "yes"
			];
		}
		if (strtolower($path_info["extension"]) == "dbc") {
			$component["properties"][] = [
				"name" => "vts:obfuscated",
				"value" => "yes"
			];
		}
		if (_type_archive($path_info["basename"]) && strtolower($path_info["extension"]) != "tar") {
			$component["properties"][] = [
				"name" => "vts:compressed",
				"value" => "yes"
			];
		}
		if ($install_location != "") {
			$component["properties"][] = [
				"name" => "vts:install_location",
				"value" => $install_location.$path_info["basename"]
			];
		}
		$components[] = $component;
		if (!isset($unterkomponenten[$metadata["component"]["bom-ref"]])) $unterkomponenten[$metadata["component"]["bom-ref"]] = [];
		$unterkomponenten[$metadata["component"]["bom-ref"]][] = $component["bom-ref"];
	}
}

function _verarbeite_runtimes(array $metadata, array &$components, array &$unterkomponenten, array $runtimes): void {
	foreach ($runtimes as $runtime) {
		$component = [];
		$component["type"] = $runtime["type"];
		$component["name"] = $runtime["name"];
		$component["version"] = $runtime["version"];
		$component["bom-ref"] = "runtime-" . makeBomRef($runtime["name"]);
		$component["supplier"] = [ "name" => $runtime["company"] ];
		$component["manufacturer"] = [ "name" => $runtime["company"] ];
		$component["publisher"] = $runtime["company"];
		$component["description"] = "Runtime must be installed on customer system.";
		$component["scope"] = $runtime["scope"];
		$component["licenses"] = getLicenses($runtime["license"]);
		$component["copyright"] = $runtime["copyright"];
		$component["isExternal"] = true;
		$component["hashes"] = [];

		$component["properties"] = [];
		foreach ($runtimes as $name => $value) {
			if (str_starts_with($name, 'vts:')) {
				$component["properties"][] =
					[
						"name" => $name,
						"value" => $value
					];
			}
		}

		$components[] = $component;
		if (!isset($unterkomponenten[$metadata["component"]["bom-ref"]])) $unterkomponenten[$metadata["component"]["bom-ref"]] = [];
		$unterkomponenten[$metadata["component"]["bom-ref"]][] = $component["bom-ref"];
	}
}

function _verarbeite_vcls(array $metadata, array &$components, array &$unterkomponenten, array $vcls): void {
	foreach ($vcls as $vcl) {
		$component = [];
		$component["type"] = "library"; // In CycloneDX bedeutet library allgemein: "wiederverwendbare Software-Komponente"
		$component["name"] = $vcl["name"];
		$component["version"] = $vcl["version"];
		$component["bom-ref"] = "vcl-" . makeBomRef($vcl["name"]);
		$component["supplier"] = [ "name" => $vcl["company"] ];
		$component["manufacturer"] = [ "name" => $vcl["company"] ];
		$component["publisher"] = $vcl["company"];
		$component["description"] = "Delphi Runtime Library (RTL) or Visual Component Library (VCL)";
		$component["scope"] = "required";
		$component["licenses"] = getLicenses($vcl["license"]);
		$component["copyright"] = $vcl["copyright"];
		$component["hashes"] = [];
		if (isset($vcl["hash:md5"])) $component["hashes"][] = [ "alg" => "MD5", "content" => $vcl["hash:md5"] ];
		if (isset($vcl["hash:sha1"])) $component["hashes"][] = [ "alg" => "SHA-1", "content" => $vcl["hash:sha1"] ];
		if (isset($vcl["hash:sha256"])) $component["hashes"][] = [ "alg" => "SHA-256", "content" => $vcl["hash:sha256"] ];

		$component["properties"] = [];
		foreach ($vcl as $name => $value) {
			if (str_starts_with($name, 'vts:')) {
				$component["properties"][] =
					[
						"name" => $name,
						"value" => $value
					];
			}
		}

		$components[] = $component;
		if (!isset($unterkomponenten[$metadata["component"]["bom-ref"]])) $unterkomponenten[$metadata["component"]["bom-ref"]] = [];
		$unterkomponenten[$metadata["component"]["bom-ref"]][] = $component["bom-ref"];
	}
}

function _verarbeite_csharp_dirs(array $metadata, array &$components, array &$unterkomponenten, array $dirs): void {
	foreach ($dirs as $prefix => $dir) {
		_verarbeite_csharp_dir($metadata, $components, $unterkomponenten, $prefix, $dir);
	}
}

function _verarbeite_csharp_dir(array $metadata, array &$components, array &$unterkomponenten, string $ref_prefix, string $dir): void {
	foreach (glob($dir . "*\\app.config") as $file) {
		if (VERBOSE) echo "Processing: $file ...\n";
		$xml = simplexml_load_file($file);
		if ($xml === false) {
			continue;
		}
		if (isset($xml->runtime->assemblyBinding))
		foreach ($xml->runtime->assemblyBinding->dependentAssembly as $binding) {
			$id = (string)$binding->assemblyIdentity->attributes()['name'];
			$publicKeyToken = (string)$binding->assemblyIdentity->attributes()['publicKeyToken'];
			$culture = (string)$binding->assemblyIdentity->attributes()['culture'];
			$version = (string)$binding->bindingRedirect->attributes()['newVersion'];

			$key = strtolower($id . "@" . $version);

			$supplier = [
				"company" => "Unknown",
				"copyright" => "",
				"license" => "Unknown"
			];
			global $cfg_nuget_supplierMap;
			foreach ($cfg_nuget_supplierMap as $test_name => $test_supplier) {
				if (str_starts_with($id,$test_name)) $supplier = $test_supplier;
			}

			if ($supplier["company"] == "Unknown") echo "Warning: Unknown nuGet Supplier: $id\n";

			$component = [];
			$component["type"] = "library";
			$component["name"] = $id;
			$component["version"] = $version;
			$component["bom-ref"] = $ref_prefix . "-" . makeBomRef($id . "-" . $version);
			$component["supplier"] = [ "name" => $supplier["company"] ];
			$component["manufacturer"] = [ "name" => $supplier["company"] ];
			$component["publisher"] = $supplier["company"];
			$component["description"] = "Package from app.config";
			$component["scope"] = "required";
			$component["licenses"] = getLicenses($supplier["license"]);
			$component["copyright"] = $supplier["copyright"];
			$component["hashes"] = [];
			$component["purl"] = "pkg:nuget/" . rawurlencode($id) . "@" . rawurlencode($version);
			global $cfg_generator_name;
			$component["properties"] = [
				//[
				//	"name" => "bsi:component:executable",
				//	"value" => "non-executable"
				//],
				//[
				//	"name" => "bsi:component:archive",
				//	"value" => "no archive"
				//],
				//[
				//	"name" => "bsi:component:structured",
				//	"value" => "unstructured"
				//],
				//[
				//	"name" => "bsi:component:filename",
				//	"value" => $id.".dll"
				//],
				[
					"name" => "vts:3p:msft:dotnet:asm:assemblyName",
					"value" => $id
				],
				[
					"name" => "vts:3p:msft:dotnet:asm:version",
					"value" => $version
				],
				[
					"name" => "vts:3p:msft:dotnet:asm:publicKeyToken",
					"value" => $publicKeyToken
				],
				[
					"name" => "vts:3p:msft:dotnet:asm:culture",
					"value" => $culture
				],
				[
					"name" => "vts:disposition",
					"value" => "embedded"
				],
				[
					"name" => "vts:discovery",
					"value" => "project"
				],
				[
					"name" => "vts:discovered_by",
					"value" => $cfg_generator_name
				],
				[
					"name" => "vts:discovery:confidence",
					"value" => "uncertain"
				]
			];
			if (!isset($unterkomponenten[$metadata["component"]["bom-ref"]])) $unterkomponenten[$metadata["component"]["bom-ref"]] = [];
			if (!in_array($component["bom-ref"],$unterkomponenten[$metadata["component"]["bom-ref"]])) {
				$components[] = $component;
				$unterkomponenten[$metadata["component"]["bom-ref"]][] = $component["bom-ref"];
			}
		}
	}
}

function _make_metadata(): array {
	global $cfg_name, $cfg_version, $cfg_desc, $cfg_license, $cfg_email, $cfg_person, $cfg_company, $cfg_generator_name, $cfg_domain;
	return [
		"timestamp" => gmdate("c"),
		"component" => [
			"type" => "application",
			"name" => $cfg_name,
			"version" => $cfg_version,
			"bom-ref" => "main",
			"description" => $cfg_desc,
			"licenses"=> getLicenses($cfg_license)
		],
		"authors" => [
			[
				"name" => $cfg_person,
				"email" => $cfg_email
			]
		],
		"manufacturer" => [
			"name" => $cfg_company,
			"url" => [
				$cfg_domain
			],
			"contact" => [
				[
					"name" => $cfg_person,
					"email" => $cfg_email
				]
			]
		],
		"supplier" => [
			"name" => $cfg_company,
			"url" => [
				$cfg_domain
			],
			"contact" => [
				[
					"name" => $cfg_person,
					"email" => $cfg_email
				]
			]
		],
		"tools" => [
			"components" => [
				[
					"type" => "application",
					"manufacturer" => [
						"name" => $cfg_company,
						"url" => [
							$cfg_domain
						],
						"contact" => [
							[
								"name" => $cfg_person,
								"email" => $cfg_email
							]
						]
					],
					"name" => $cfg_generator_name,
					"version" => date("Y.m.d.His", filemtime(__FILE__))
				]
			]
		]
	];
}

function _make_dependencies(array $unterkomponenten): array {
	$dependencies = [];
	foreach ($unterkomponenten as $ref_father => $ref_children) {
		$dependencies[] = [
			"ref" => $ref_father,
			"dependsOn" => $ref_children
		];
	}
	return $dependencies;
}

function _type_executable(string $filename): bool {
	$filename = strtolower($filename);

	return
		str_ends_with($filename, '.exe') ||
		str_ends_with($filename, '.msi') ||
		str_ends_with($filename, '.bat') ||
		str_ends_with($filename, '.cmd') ||
		str_ends_with($filename, '.com') ||
		str_ends_with($filename, '.sh') ||
		str_ends_with($filename, '.run') ||
		str_ends_with($filename, '.appimage') ||
		str_ends_with($filename, '.jar') ||
		str_ends_with($filename, '.ps1') ||
		str_ends_with($filename, '.apk') ||
		str_ends_with($filename, '.ipa');
}

function _type_archive(string $filename): bool {
	$filename = strtolower($filename);

	return
		str_ends_with($filename, '.7z') ||
		str_ends_with($filename, '.zip') ||
		str_ends_with($filename, '.rar') ||
		str_ends_with($filename, '.tar') ||
		str_ends_with($filename, '.gz') ||
		str_ends_with($filename, '.tgz') ||
		str_ends_with($filename, '.bz2') ||
		str_ends_with($filename, '.xz') ||
		str_ends_with($filename, '.zst') ||
		str_ends_with($filename, '.tar.gz') ||
		str_ends_with($filename, '.tar.bz2') ||
		str_ends_with($filename, '.tar.xz') ||
		str_ends_with($filename, '.cab') ||
		str_ends_with($filename, '.iso');
}

function _type_structured(string $filename): bool {
	// "structured" heißt laut BSI, dass man die Einzelkomponenten erhalten kann
	// also z.b. ein Archiv, oder eine EXE wo SBOM integriert ist,
	// aber kein Firmware-Image beispielsweise
	return _type_archive($filename); // wir halten es erstmal ganz simpel...
}

function _merge_sbom(array &$main, string $php_sbom, array $archiveNames, string $toolname, ?string $wanted_root): void {
	$php  = json_decode(file_get_contents($php_sbom), true, 512, JSON_THROW_ON_ERROR);

	$main['components'] ??= [];
	$main['dependencies'] ??= [];

	$php['components'] ??= [];
	$php['dependencies'] ??= [];

	//
	// ------------------------------------------------------------
	// metadata.component (oder eigene stammkomponente) übernehmen
	//

	$phpRoot = null;
	if ($wanted_root != null) {
		foreach ($php['components'] as $c) {
			if (str_starts_with($c['bom-ref'], $wanted_root)) {
				$phpRoot = $c;
				break;
			}
		}
	} else {
		$phpRoot = $php['metadata']['component'] ?? null;
		if ($phpRoot !== null) {
			$exists = false;
			foreach ($main['components'] as $c) {
				if (($c['bom-ref'] ?? '') === ($phpRoot['bom-ref'] ?? '')) {
					$exists = true;
					break;
				}
			}
			if (!$exists)
				$main['components'][] = $phpRoot;
		}
	}

	//
	// ------------------------------------------------------------
	// components mergen
	//

	$known = [];

	foreach ($main['components'] as $c)
		if (isset($c['bom-ref']))
			$known[$c['bom-ref']] = true;

	foreach ($php['components'] as $c) {

		$ref = $c['bom-ref'] ?? null;

		if ($ref === null)
			continue;

		if (!isset($known[$ref])) {
			if (!isset($c['properties'])) $c['properties'] = [];
			$c['properties'][] = [
				"name" => "vts:disposition",
				"value" => "artifact"
			];
			$c['properties'][] = [
				"name" => "vts:discovery",
				"value" => "project" // found by composer, cargo, etc.
			];
			$c['properties'][] = [
				"name" => "vts:discovered_by",
				"value" => $toolname
			];
			$c['properties'][] = [
				"name" => "vts:discovery:confidence",
				"value" => "confirmed"
			];
			$main['components'][] = $c;
			$known[$ref] = true;
		}
	}

	//
	// ------------------------------------------------------------
	// dependencies mergen
	//

	$deps = [];

	foreach ($main['dependencies'] as $d)
		$deps[$d['ref']] = $d;

	foreach ($php['dependencies'] as $d) {

		if (!isset($deps[$d['ref']])) {

			$deps[$d['ref']] = $d;

		} else {

			$deps[$d['ref']]['dependsOn'] ??= [];
			$d['dependsOn'] ??= [];

			$deps[$d['ref']]['dependsOn'] = array_values(array_unique(array_merge(
				$deps[$d['ref']]['dependsOn'],
				$d['dependsOn']
			)));
		}
	}

	$deps = array_filter(
		$deps,
		static fn($d) => isset($d['dependsOn'])
	);

	//
	// ------------------------------------------------------------
	// phplib_xx.7z finden
	//

	$archiveRefs = [];
	foreach ($archiveNames as $archiveName) {
		foreach ($main['components'] as $c) {
			if (strtolower(($c['name'] ?? '')) === strtolower($archiveName)) {
				$archiveRefs[] = $c['bom-ref'] ?? $archiveName;
				break;
			}
		}
	}

	//
	// ------------------------------------------------------------
	// metadata.component als Dependency eintragen
	//

	if ($phpRoot !== null) {

		$rootRef = $phpRoot['bom-ref'] ?? null;

		if ($rootRef !== null) {

			if (!is_array($archiveRefs)) $archiveRefs = [ $archiveRefs ];
			foreach ($archiveRefs as $archiveRef) {
				if (!isset($deps[$archiveRef])) {
					$deps[$archiveRef] = [
						'ref' => $archiveRef,
						'dependsOn' => []
					];
				}

				if (!in_array($rootRef, $deps[$archiveRef]['dependsOn'], true))
					$deps[$archiveRef]['dependsOn'][] = $rootRef;
			}
		}
	} else {
		fwrite(STDERR, "Warning: Root of SBOM file to be merged ($php_sbom) not found\n");
	}

	//
	// ------------------------------------------------------------
	// tools (1.5 und 1.7)
	//

	if (isset($php['metadata']['tools'])) {

		if (array_is_list($php['metadata']['tools'])) {

			// Add 1.5 to 1.6+

			$main['metadata']['tools']['components'] ??= [];

			$known = [];

			foreach ($main['metadata']['tools']['components'] as $t) {
				$known[($t['vendor'] ?? $t['manufacturer']['name'] ?? '').'|'.($t['name'] ?? '').'|'.($t['version'] ?? '')] = true;
			}

			foreach ($php['metadata']['tools'] as $t) {

				$k = ($t['vendor'] ?? $t['manufacturer']['name'] ?? '').'|'.($t['name'] ?? '').'|'.($t['version'] ?? '');

				if (!isset($known[$k])) {

					if (isset($t['vendor'])) {
						if (!isset($t['manufacturer']))
							$t['manufacturer'] = [ "name" => $t['vendor'] ];
						unset($t['vendor']);
					}

					if (!isset($t['type']))
						$t['type'] = (str_contains($t['name'], 'library'))  ? 'library' : 'application';

					$main['metadata']['tools']['components'][] = $t;
					$known[$k] = true;
				}
			}

		} elseif (isset($php['metadata']['tools']['components'])) {

			// Add 1.6+ to 1.6+

			$main['metadata']['tools']['components'] ??= [];

			$known = [];

			foreach ($main['metadata']['tools']['components'] as $t)
				$known[$t['bom-ref']??$t['name']] = true;

			foreach ($php['metadata']['tools']['components'] as $t) {

				if (!isset($known[$t['bom-ref']??$t['name']])) {

					if (isset($t['vendor'])) {
						if (!isset($t['manufacturer']))
							$t['manufacturer'] = [ "name" => $t['vendor'] ];
						unset($t['vendor']);
					}

					$main['metadata']['tools']['components'][] = $t;
					$known[$t['bom-ref']??$t['name']] = true;
				}
			}
		}
	}

	//
	// ------------------------------------------------------------
	// schreiben
	//

	$main['dependencies'] = array_values($deps);

	if (VERBOSE) echo "Added $php_sbom\n";
}


function normalizeUnitName(string $name): string {
	$name = trim($name);

	// Delphi Unit-Namen sind case-insensitive
	return strtolower($name);
}

function findBplForUnit(
	string $unit,
	array $unit_mapping,
	array $dcc_prefixes
): ?string {
	// 1. Exakter Match
	if (isset($unit_mapping[$unit])) {
		return $unit_mapping[$unit];
	}

	// Normalisierte Mapping-Tabelle aufbauen
	$normalizedMapping = [];

	foreach ($unit_mapping as $name => $bpl) {
		$normalizedMapping[normalizeUnitName($name)] = $bpl;
	}

	// 2. Normalisierter Match
	$normalizedUnit = normalizeUnitName($unit);

	if (isset($normalizedMapping[$normalizedUnit])) {
		return $normalizedMapping[$normalizedUnit];
	}

	foreach ($dcc_prefixes as $dcc_prefix) {
		// 3. "System." davorsetzen und exakt versuchen
		$systemUnit = $dcc_prefix. '.' . $unit;

		if (isset($unit_mapping[$systemUnit])) {
			return $unit_mapping[$systemUnit];
		}

		// 4. "System." + normalisierter Match
		$normalizedSystemUnit = normalizeUnitName($systemUnit);

		if (isset($normalizedMapping[$normalizedSystemUnit])) {
			return $normalizedMapping[$normalizedSystemUnit];
		}
	}

	return null;
}

function getDelphiProjectExternalUnits(string $projectDir, callable $exclude_cb): array {

	$files = findPascalFiles($projectDir, $exclude_cb);

	$projectUnits = [];
	$allUsedUnits = [];

	$unitsByFile = [];

	foreach ($files as $file) {

		$code = file_get_contents($file);

		if ($code === false) {
			continue;
		}

		/*
		 * --------------------------------------------------------
		 * Eigene Unit
		 * --------------------------------------------------------
		 */

		$unitName = extractUnitName(
			prepareCode($code)
		);

		if ($unitName !== null) {

			$projectUnits[
				normalizeUnitName($unitName)
			] = $unitName;
		}


		/*
		 * --------------------------------------------------------
		 * Uses
		 * --------------------------------------------------------
		 */

		$units = extractUsesUnits($code);

		$units = array_values(
			array_unique($units)
		);

		$unitsByFile[$file] = $units;

		foreach ($units as $unit) {

			$key = normalizeUnitName($unit);

			$allUsedUnits[$key] = $unit;
		}
	}

	$externalUnits = [];

	foreach ($allUsedUnits as $normalized => $unit) {

		/*
		 * Wenn die Unit selbst Bestandteil des Projektes ist,
		 * interessiert sie uns für das VCL/Library-Mapping nicht.
		 */
		if (isset($projectUnits[$normalized])) {
			continue;
		}

		$externalUnits[$normalized] = $unit;
	}


	$externalUnits = array_unique($externalUnits);
	return array_values($externalUnits);

}

function getDelphiEmbtBplUnitMapping(string $install_dir): array {
	$out = array();
	exec(__DIR__.'/utils/ListPackageInfoUnits.exe "'.$install_dir.'"', $out, $ec);
	if ($ec != 0) throw new Exception("ListPackageInfoUnits failed with Error Code $ec");
	$out = implode("\n", $out);
	$delphi_packages = json_decode($out, true);
	if (!$delphi_packages) throw new Exception("ListPackageInfoUnits has output an internal error (invalid JSON)");
	$unit_mapping = array();
	foreach ($delphi_packages as $bpl_filename => $unit_names) {
		foreach ($unit_names as $unit_name) {
			if ($unit_name == 'SysInit') continue; // this is never included directly
			if (isset($unit_mapping[$unit_name])) {
				//echo "Warning: $unit_name is in multiple packages! At least in $unit_mapping[$unit_name] and $bpl_filename\n";
				$unit_mapping[$unit_name] = '';
			}
			$unit_mapping[$unit_name] = $bpl_filename;
		}
	}
	foreach ($unit_mapping as $unit_name => &$bpl_filename) {
		if ($bpl_filename == '') {
			$bpl_filename = null; // remove
		}
	}
	return $unit_mapping;
}

function prepareCode(string $code): string {
	// ============================================================
	// KOMMENTARE ENTFERNEN
	// ============================================================

	/*
	 * Wichtig:
	 *
	 * Delphi Conditional Directives wie
	 *
	 *   {$IFDEF FOO}
	 *
	 * werden NICHT entfernt.
	 *
	 * Wir entfernen ausschließlich Kommentare.
	 */

	// { ... }
	$code = preg_replace(
		'/\{(?!\$).*?\}/s',
		' ',
		$code
	);

	// (* ... *)
	$code = preg_replace(
		'/\(\*.*?\*\)/s',
		' ',
		$code
	);

	// // ...
	$code = preg_replace(
		'/\/\/.*$/m',
		' ',
		$code
	);

	// ============================================================
	// CONDITIONAL DIRECTIVES NEUTRALISIEREN
	// ============================================================
	/*
	 * Conditional Compilation:
	 *
	 * Alle Branches bleiben erhalten.
	 */
	/*
	 * Wir werten Conditional Compilation absichtlich NICHT aus.
	 *
	 * Beispiel:
	 *
	 * {$IFDEF DEBUG}
	 *   uses DebugUnit;
	 * {$ELSE}
	 *   uses ReleaseUnit;
	 * {$ENDIF}
	 *
	 * Beide Units sollen erkannt werden:
	 *
	 *   DebugUnit
	 *   ReleaseUnit
	 *
	 * Deshalb entfernen wir nur die Direktiven selbst,
	 * nicht deren Inhalt.
	 */

	$code = preg_replace(
		'/\{\$(?:IFDEF|IFNDEF|IF|ELSEIF|ELSE|ENDIF|DEFINE|UNDEF|IFOPT|IFEND|IFDECLARED|IFNDEF)\b[^}]*\}/i',
		' ',
		$code
	);

	return $code;
}


// ============================================================
// EIGENE UNIT ERMITTELN
// ============================================================

function extractUnitName(string $code): ?string {
	/*
	 * Sucht beispielsweise:
	 *
	 * unit MainForm;
	 *
	 * oder:
	 *
	 * unit My.Namespace.UnitName;
	 */

	if (preg_match(
		'/\bunit\s+([A-Za-z_][A-Za-z0-9_.]*)\s*;/i',
		$code,
		$match
	)) {
		return trim($match[1]);
	}

	return null;
}


// ============================================================
// USES-KLAUSELN EXTRAHIEREN
// ============================================================

function extractUsesUnits(string $code): array {
	$code = prepareCode($code);

	$units = [];

	/*
	 * Findet alle uses-Klauseln.
	 *
	 * Beispiel:
	 *
	 * uses
	 *   Windows,
	 *   SysUtils,
	 *   Classes;
	 *
	 * Durch das Nicht-Auswerten der IFDEFs werden auch Units
	 * aus allen Conditional Branches gefunden.
	 */

	preg_match_all(
		'/\buses\b(.*?);/is',
		$code,
		$matches
	);

	foreach ($matches[1] as $usesBlock) {

		/*
		 * Entfernt:
		 *
		 *   Foo in 'Foo.pas'
		 *
		 * aus Delphi uses-Klauseln.
		 */
		$usesBlock = preg_replace(
			"/\b[A-Za-z_][A-Za-z0-9_.]*\s+in\s+'[^']*'/i",
			'',
			$usesBlock
		);

		/*
		 * Auf einzelne Einträge aufteilen.
		 */
		$parts = preg_split(
			'/,/',
			$usesBlock
		);

		foreach ($parts as $part) {

			$part = trim($part);

			if ($part === '') {
				continue;
			}

			/*
			 * Nur gültige Unit-Namen akzeptieren.
			 *
			 * Beispiele:
			 *
			 *   Forms
			 *   SysUtils
			 *   System.SysUtils
			 *   Vcl.Forms
			 */
			if (preg_match(
				'/^([A-Za-z_][A-Za-z0-9_.]*)$/',
				$part,
				$match
			)) {
				$units[] = $match[1];
			}
		}
	}

	return $units;
}


// ============================================================
// PASCAL DATEIEN FINDEN
// ============================================================

function findPascalFiles(string $directory, callable $exclude_cb): array {
	$files = [];

	$iterator = new RecursiveIteratorIterator(
		new RecursiveDirectoryIterator(
			$directory,
			FilesystemIterator::SKIP_DOTS
		)
	);

	foreach ($iterator as $file) {

		if (!$file->isFile()) {
			continue;
		}

		if ((strtolower($file->getExtension()) !== 'pas') && (strtolower($file->getExtension()) !== 'dpr')) {
			continue;
		}

		if (!$exclude_cb($file->getPathname())) continue;

		$files[] = $file->getPathname();
	}

	sort(
		$files,
		SORT_NATURAL | SORT_FLAG_CASE
	);

	return $files;
}

function findLatestEmbarcaderoUserDir(): ?string {
	$baseDir = "C:\\Users\\Public\\Documents\\Embarcadero\\Studio";
	return _findLatestDir($baseDir);
}

function findLatestEmbarcaderoStudioDir(): ?string {
	$baseDir = 'C:\\Program Files (x86)\\Embarcadero\\Studio';
	return _findLatestDir($baseDir);
}

function _findLatestDir($baseDir): ?string {
	if (!is_dir($baseDir)) {
		return null;
	}

	$dirs = [];

	foreach (scandir($baseDir) as $dir) {
		// Nur Verzeichnisse berücksichtigen
		if ($dir === '.' || $dir === '..') {
			continue;
		}

		$fullPath = $baseDir . DIRECTORY_SEPARATOR . $dir;

		if (is_dir($fullPath)) {
			// Nur Verzeichnisse mit einer Versionsnummer wie 37.0
			if (preg_match('/^\d+(?:\.\d+)*$/', $dir)) {
				$dirs[] = $dir;
			}
		}
	}

	if (empty($dirs)) {
		return null;
	}

	// Numerisch nach Version sortieren
	usort($dirs, 'version_compare');

	// Höchste Version
	$latestVersion = end($dirs);

	return $baseDir . DIRECTORY_SEPARATOR . $latestVersion;
}

function hickel_pathname_is_forbidden(string $pathname): bool {
	if (str_contains(strtoupper($pathname), 'ZZ___ABGESCHALTETE')) return true;
	if (str_contains(strtoupper($pathname), '__HISTORY')) return true;
	if (str_contains(strtoupper($pathname), '__RECOVERY')) return true;
	return false;
}

function hickel_pathname_is_foreign(string $pathname): bool {
	if (str_contains(strtoupper($pathname), '\\VCL_') && !str_contains($pathname, 'VCL_Hotfix_AltGr')) return true;
	return false;
}

function hickel_pathname_is_hsinfo2(string $pathname): bool {
	if (str_contains(strtoupper($pathname), '\\HS_INFO2')) return true;
	return false;
}

function hickel_pathname_is_dbtool(string $pathname): bool {
	if (str_contains(strtoupper($pathname), '\\DBTOOL')) return true;
	return false;
}

function hickel_pathname_is_cora(string $pathname): bool {
	if (str_contains(strtoupper($pathname), '\\HICKEL_CORA_')) return true;
	if (str_contains(strtoupper($pathname), '\\_PHP')) return true;
	if (str_contains(strtoupper($pathname), '\\CORA')) return true;
	if (str_contains(strtoupper($pathname), '\\ANDROID MDE APP')) return true;
	if (str_contains(strtoupper($pathname), '\\EDITION DLL')) return true;
	if (str_contains(strtoupper($pathname), '\\HSBATCHSQL')) return true;
	if (str_contains(strtoupper($pathname), '\\MINIPHP')) return true;
	if (str_contains(strtoupper($pathname), '\\SYSTEM_DBUPDATES')) return true;
	if (str_contains(strtoupper($pathname), '\\WEBSHOPINTERN')) return true;
	if (str_contains(strtoupper($pathname), '\\WRV_AUSWERTUNGEN')) return true;
	if (str_contains(strtoupper($pathname), '\\ZZ_KUNDEN_SONDERPROGRAMME')) return true;
	return false;
}

function sbom_generate(): void {
	global $cfg_csharp_dirs;
	foreach ($cfg_csharp_dirs as $testdir) if (!is_dir("$testdir")) throw new Exception("Not found: $testdir");

	global $cfg_delphi_dir;
	if (!is_dir("$cfg_delphi_dir")) throw new Exception("Not found: $cfg_delphi_dir");

	global $cfg_bin_dirs;
	foreach ($cfg_bin_dirs as [$testdir,$test_installdir]) if (!is_dir("$testdir")) throw new Exception("Not found: $testdir");

	global $cfg_doc_dirs;
	foreach ($cfg_doc_dirs as [$testdir,$test_installdir]) if (!is_dir("$testdir")) throw new Exception("Not found: $testdir");

	global $cfg_report_dirs;
	foreach ($cfg_report_dirs as [$testdir,$test_installdir]) if (!is_dir("$testdir")) throw new Exception("Not found: $testdir");

	$embt_dir = findLatestEmbarcaderoStudioDir();
	if ($embt_dir === null) {
		throw new Exception('No Embarcadero Studio installation found.');
	}

	$embt_userdir = findLatestEmbarcaderoUserDir();
	if ($embt_userdir === null) {
		throw new Exception('No Embarcadero Studio user dir found.');
	}

	$delphi_bpl = [];

	global $cfg_delphi_extra_bpl;
	foreach ($cfg_delphi_extra_bpl as $extra_bpl) {
		if (file_exists($fn = "$embt_dir\\bin\\$extra_bpl"))
			$delphi_bpl[$fn] = [];
		else if (file_exists($fn = "$embt_userdir\\Bpl\\$extra_bpl"))
			$delphi_bpl[$fn] = [];
		else
			throw new Exception("Extra BPL $extra_bpl not found");
	}

	$unit_mapping = array_merge(
		getDelphiEmbtBplUnitMapping("$embt_dir\\bin"),
		getDelphiEmbtBplUnitMapping("$embt_userdir\\Bpl")
	);

	global $cfg_delphi_external_units_exclude;
	$externalUnits = getDelphiProjectExternalUnits($cfg_delphi_dir, $cfg_delphi_external_units_exclude);
	$used_bpls = [];
	foreach ($externalUnits as $externalUnit) {
		global $cfg_dcc_prefixes;
		$bpl = findBplForUnit($externalUnit, $unit_mapping, $cfg_dcc_prefixes);

		if ($bpl !== null) {
			if (!in_array($bpl, $used_bpls, true)) {
				if (!isset($used_bpls[$bpl])) $used_bpls[$bpl] = array();
				$used_bpls[$bpl][] = $externalUnit;
			}
		} else {
			if ($externalUnit == 'MidasLib') continue; // this is am Embt PAS file, not in any BPL (will be ignored in SBOM)
			if (str_starts_with($externalUnit, 'Androidapi')) continue; // these are Embt PAS files, not in any BPL (will be ignored in SBOM)

			if ($externalUnit == 'lcltype') continue; // Uses via FPC compiler switch (so, not used)
			if ($externalUnit == 'lresources') continue; // Uses via FPC compiler switch (so, not used)
			if ($externalUnit == 'FastMM4') continue; // Uses via disabled "define"-switch (so, not used)

			echo "WARNING: UNIT/VCL MAPPING NOT FOUND FOR UNIT: $externalUnit\n";
		}
	}

	foreach ($used_bpls as $used_bpl => $units) {
		if (file_exists("$embt_dir\\bin\\$used_bpl"))
			$delphi_bpl["$embt_dir\\bin\\$used_bpl"] = $units;
		else if (file_exists("$embt_userdir\\Bpl\\$used_bpl"))
			$delphi_bpl["$embt_userdir\\Bpl\\$used_bpl"] = $units;
	}

	uksort($delphi_bpl, function ($a, $b) {
		return strnatcasecmp(basename($a), basename($b));
	});
	global $cfg_manual_vcl;
	$vcls_detected = $cfg_manual_vcl;
	foreach ($delphi_bpl as $fil => $units) {
		sort($units, SORT_NATURAL | SORT_FLAG_CASE);
		$ver_vcl = getVersionInfo($fil);

		$company = $ver_vcl["CompanyName"];
		$copyright = $ver_vcl["LegalCopyright"];
		$license = "Proprietary";

		global $cfg_license_company_copyright_override;
		foreach ($cfg_license_company_copyright_override as $override_file => $overrides) {
			if (str_starts_with(strtolower(basename($fil)), strtolower($override_file))) {
				if (($overrides["company"] ?? "") != "") $company = $overrides["company"];
				if (($overrides["license"] ?? "") != "") $license = $overrides["license"];
				if (($overrides["copyright"] ?? "") != "") $copyright = $overrides["copyright"];
			}
		}

		if (preg_match("@(HICKEL.+)\\.BPL@isU", basename($fil), $m))
			$version = "1.0.0.0+svn".getSvnRev($cfg_delphi_dir."\\".$m[1]);
		else
			$version = $ver_vcl["FileVersion"];

		global $cfg_generator_name;
		$vcl =
			[
				"name" => $ver_vcl["FileDescription"]." (".basename($fil).")",
				"version" => $version,
				"company" => $ver_vcl["CompanyName"],
				"copyright" => $ver_vcl["LegalCopyright"],
				"license" => $license,
				"vts:disposition" => "embedded",
				"vts:discovery" => "project",
				"vts:discovered_by" => $cfg_generator_name,
				"vts:discovery:confidence" => "confirmed",
				"hash:md5" => hash_file("md5", $fil),
				"hash:sha1" => hash_file("sha1", $fil),
				"hash:sha256" => hash_file("sha256", $fil)
			];
		if (count($units) > 0) {
			$vcl["vts:delphi_units"] = implode(', ', $units);
		}
		$vcls_detected[] = $vcl;
	}

	$metadata = _make_metadata();
	$unterkomponenten = [];
	$components = [];
	_verarbeite_bin_dirs($metadata, $components, $unterkomponenten, $cfg_bin_dirs);
	_verarbeite_report_dirs($metadata, $components, $unterkomponenten, $cfg_report_dirs);
	_verarbeite_doc_dirs($metadata, $components, $unterkomponenten, $cfg_doc_dirs);
	global $cfg_runtimes;
	_verarbeite_runtimes($metadata, $components, $unterkomponenten, $cfg_runtimes);
	_verarbeite_vcls($metadata, $components, $unterkomponenten, $vcls_detected);
	_verarbeite_csharp_dirs($metadata, $components, $unterkomponenten, $cfg_csharp_dirs);
	$dependencies = _make_dependencies($unterkomponenten);
	$sbom = [
		"\$schema" => "http://cyclonedx.org/schema/bom-1.7.schema.json", // https://www.jsonschemavalidator.net/
		"bomFormat" => "CycloneDX",
		"specVersion" => "1.7",
		"version" => 1,
		"serialNumber" => "urn:uuid:" . gen_uuid(),
		"metadata" => $metadata,
		"components" => $components,
		"dependencies" => $dependencies
	];

	global $cfg_merge_cb;
	$cfg_merge_cb($sbom);

	// --- Zunächst als JSON abspeichern ---

	global $cfg_output_file;
	$output_file_xml  = $cfg_output_file;
	$output_file_json = substr($output_file_xml, 0, strlen($output_file_xml)-4).'.json';

	$sbom_cont = json_encode($sbom, JSON_PRETTY_PRINT | JSON_UNESCAPED_UNICODE | JSON_UNESCAPED_SLASHES);

	global $cfg_json_final_cb;
	$cfg_json_final_cb($sbom_cont);

	file_put_contents($output_file_json, $sbom_cont);

	// --- Als XML abspeichern und signieren ---

	if (!file_exists(__DIR__.'\\private.key')) {
		system(escapeshellcmd(__DIR__."\\utils\\cyclonedx-win-x64.exe")." keygen");
		// cyclonedx-win-x64.exe erzeugt leider nur RSA-2048.
		// Verwende openssl um RSA-4096 zu erstellen, sofern openssl installiert ist
		system("openssl genrsa -aes256 -out private.key 4096");
		system("openssl rsa -in private.key -pubout -out public.key");
	}
	system(escapeshellcmd(__DIR__."\\utils\\cyclonedx-win-x64.exe")." convert --input-file ".escapeshellarg($output_file_json)." --output-file ".escapeshellarg($output_file_xml));
	system(escapeshellcmd(__DIR__."\\utils\\cyclonedx-win-x64.exe")." sign bom ".escapeshellarg($output_file_xml));
	system(escapeshellcmd(__DIR__."\\utils\\cyclonedx-win-x64.exe")." verify all ".escapeshellarg($output_file_xml));
	unlink($output_file_json);

	copy(__DIR__.'\\public.key', dirname($cfg_output_file).'\\sbom.pem');

	if (VERBOSE) echo "Created: $output_file_xml\n";
}
