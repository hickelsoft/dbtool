<?php

$files = array_merge(


  glob('c:/SVN/VTS_CMDB2/trunk/*.pas'),
  glob('c:/SVN/VTS_CMDB2/trunk/*.dpr'),
  glob('c:/SVN/VTS_Decoder/Decoder5x/*.pas'),
  glob('c:/SVN/VTS_Decoder/Decoder5x/*.dpr'),


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

	if (strpos($filePath, '/VCL_') !== false) return;
	if (strpos($filePath, '/_CRW11') !== false) return;
	if (strpos($filePath, 'ABGESCHALTETE') !== false) return;
	if (strpos($filePath, 'BetterADODataSet.pas') !== false) return;
	if (strpos($filePath, 'PerlRegEx.pas') !== false) return;
	if (strpos($filePath, '/pcre.pas') !== false) return;
	if (strpos($filePath, 'schnellere Localization (F7, F8)') !== false) return;

    $content = file_get_contents($filePath);

	if (strpos($content, 'on E: EAbort do{60208}') !== false) {
		$content = str_ireplace('on E: EAbort do{60208}', 'on E: EAbort do', $content);
		file_put_contents($filePath, $content);
	}

    if ($content === false) {
        die("Fehler beim Lesen der Datei: $filePath\n");
    }








	if (substri_count($content, 'on E: EAbort do') != substri_count($content, 'on E: Exception do')) {
	  echo $filePath."\n";

	  $content = str_ireplace('on E: EAbort do', 'on E: EAbort do{60208}', $content);
	  file_put_contents($filePath, $content);

	}
	return;



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


    checkPascalFile($file);




}

echo "Fertig.";

function substri_count($haystack, $needle)
{
    return substr_count(strtoupper($haystack), strtoupper($needle));
}
