<?php

$files = array_merge(
  //glob('c:/SVN/Delphi/Cora_Verwaltung/Src/*.pas'),
  //glob('c:/SVN/Delphi/Cora_Verwaltung/Src/*/*.pas'),
  //glob('c:/SVN/Delphi/Cora_Verwaltung/Src/*/*/*.pas'),
  //glob('c:/SVN/Delphi/Cora_Verwaltung/Src/*/*/*/*.pas')


  glob('c:/SVN/VTS_CMDB2/trunk/*.pas'),
  glob('c:/SVN/VTS_CMDB2/trunk/*.dpr'),
  glob('c:/SVN/VTS_Decoder/Decoder50/*.pas'),
  glob('c:/SVN/VTS_Decoder/Decoder50/*.dpr'),


  glob('c:/SVN/Delphi/*.pas'),
  glob('c:/SVN/Delphi/*/*.pas'),
  glob('c:/SVN/Delphi/*/*/*.pas'),
  glob('c:/SVN/Delphi/*/*/*/*.pas'),
  glob('c:/SVN/Delphi/*/*/*/*/*.pas'),
  glob('c:/SVN/Delphi/*/*/*/*/*/*.pas'),
  glob('c:/SVN/Delphi/*/*/*/*/*/*/*.pas'),
  glob('c:/SVN/Delphi/*/*/*/*/*/*/*/*.pas'),
  glob('c:/SVN/Delphi/*.dpr'),
  glob('c:/SVN/Delphi/*/*.dpr'),
  glob('c:/SVN/Delphi/*/*/*.dpr'),
  glob('c:/SVN/Delphi/*/*/*/*.dpr'),
  glob('c:/SVN/Delphi/*/*/*/*/*.dpr'),
  glob('c:/SVN/Delphi/*/*/*/*/*/*.dpr'),
  glob('c:/SVN/Delphi/*/*/*/*/*/*/*.dpr'),
  glob('c:/SVN/Delphi/*/*/*/*/*/*/*/*.dpr'),


);


function checkPascalFile($filePath) {
    $content = file_get_contents($filePath);

	if (strpos($filePath, 'ABGESCHALTETE') !== false) return;
	if (strpos($filePath, 'BetterADODataSet.pas') !== false) return;
	if (strpos($filePath, 'PerlRegEx.pas') !== false) return;
	if (strpos($filePath, '/pcre.pas') !== false) return;
	if (strpos($filePath, 'schnellere Localization (F7, F8)') !== false) return;



    if ($content === false) {
        die("Fehler beim Lesen der Datei: $filePath\n");
    }

	$content = str_replace('try-except-end', '', $content);

    // Unit-Name extrahieren (aus `unit <Name>;`)
    if (preg_match('/\bunit\s+(\w+)\s*;/i', $content, $unitMatch)) {
        $unitName = $unitMatch[1];
    } else {
        $unitName = "Unbekannte Unit";
    }

    // Alle except-Blöcke suchen
    preg_match_all('/except\s+(.*?)\bend\b/si', $content, $exceptBlocks, PREG_SET_ORDER);

    $missingEAbort = false;


//print_r($exceptBlocks);

    foreach ($exceptBlocks as $block) {
        if (!preg_match('/\bEAbort\b/i', $block[1])) {
            if (!$missingEAbort) echo "****** Fehlendes EAbort in Unit: $filePath ******\n";
			echo $block[0]."\n\n\n\n";
            $missingEAbort = true;
            //break; // Falls nur eine Meldung pro Datei gewünscht ist
        }
    }

    if (!$missingEAbort) {
        //echo "Alle except-Blöcke enthalten EAbort in Unit: $filePath\n";
    }
}


foreach ($files as $file) {

	if (strpos($file, '/VCL_') !== false) continue;
	if (strpos($file, '/_CRW11') !== false) continue;

    checkPascalFile($file);




}

echo "Fertig.";
