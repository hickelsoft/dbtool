<?php

// This script copies all translated strings from
// DBTool64_DRC.rcn to DBTool32_DRC.rcn
// So, you only need to translate DBTool64, and then run this script.

// How to:
// 1. Open Delphi
// 2. Activate Project DBTool64.exe
// 3. Compile it
// 4. Projekt => Language => Update Localized Project
// 5. Activate Project DBTool64.enu and edit DBTool64_DRC.rc
// 6. Close that file and save (the diskette icon does not work)
// 7. Run this PHP script
// 8. Activate Project DBTool32.exe
// 9. Compile it
// 10. Projekt => Language => Update Localized Project
// 11. Activate Project DBTool32.enu
// 12. Compile
// 13. To test, adjust HKEY_CURRENT_USER\Software\Embarcadero\Locales
// 14. Run DBTool32.exe to test

$rcnSource = __DIR__ . '\Projekte\Vollversion\ENU\C\HS-Service\DBTool64_DRC.rcn';
$rcnTarget = __DIR__ . '\Projekte\Vollversion\ENU\C\HS-Service\DBTool32_DRC.rcn';

$translation_ary = [];

// Load translations
$x = simplexml_load_file($rcnSource);
foreach ($x->file->body->{"trans-unit"} as $a) {
	$source = "".$a->source;
	$target = "".$a->target;
	if ($target != '') {
		$translation_ary[$source] = $target;
	}
}
$x = null;

// Replace translations
// TODO: Also Copy Status and Timestamps?
$x = simplexml_load_file($rcnTarget);
foreach ($x->file->body->{"trans-unit"} as $a) {
	$source = "".$a->source;
	$target = "".$a->target;
	//if ($target == '') {
		if (isset($translation_ary[$source])) {
			$a->target = $translation_ary[$source];
		}
	//}
}
$x->saveXml($rcnTarget);
echo "DONE.\n";
