<?php

// This script copies all translated strings from
// DBTool64_DRC.rcn to DBTool32_DRC.rcn
// So, you only need to translate DBTool64, and then run this script.

// How to:
// 1. Open Delphi
// 2. Activate Project DBTool64.exe
// 3. Compile it
// 4. Projekt => Languages => Update Localized Projects
// 5. Activate Project DBTool64.enu and edit DBTool64_DRC.rc
// 6. Close that file and save (the diskette icon does not work)
// 7. Compile it
// 8. Run this PHP script
// 9. Activate Project DBTool32.exe
// 10. Compile it
// 11. Projekt => Language => Update Localized Project
// 12. Activate Project DBTool32.enu
// 13. Compile
// 14. To test, adjust HKEY_CURRENT_USER\Software\Embarcadero\Locales
// 15. Run DBTool32.exe to test
// 16. Compile FixResourceDll.exe (the post-build-process changes the ENU files by adding missing Forms)

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
