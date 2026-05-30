<?php

// Diese PHP-Datei muss als UTF-8 abgespeichert sein, wegen dem RegEx weiter unten

$directory = 'C:\\SVN\\'; // Ersetze dies durch dein Verzeichnis

# ---

function extractComponentsFromDfm($dfmPath) {
    $components = [];
    $file = fopen($dfmPath, 'r');

    if ($file) {
        while (($line = fgets($file)) !== false) {
            $line = trim($line);

			if (mb_detect_encoding($line, ['ASCII', 'UTF-8', 'ISO-8859-1'], false) != 'UTF-8') {
				$line = mb_convert_encoding($line, 'UTF-8', 'ISO-8859-1');
			}

            if (stripos($line, "object ") === 0) {
                $parts = explode(':', $line);
				if (count($parts) < 2) continue;
                $componentName = trim(explode(' ', $parts[0])[1]);
               	$components[] = $componentName;
            }
        }
        fclose($file);
    } else {
        echo "Unable to open DFM file: $dfmPath";
    }

    return $components;
}

function extractComponentsFromPas($pasPath) {
    $components = [];
    $file = fopen($pasPath, 'r');
    $insideClass = false;
	$inPublished = true;

    if ($file) {
        while (($line = fgets($file)) !== false) {
            $line = trim($line);

			if (mb_detect_encoding($line, ['ASCII', 'UTF-8', 'ISO-8859-1'], false) != 'UTF-8') {
				$line = mb_convert_encoding($line, 'UTF-8', 'ISO-8859-1');
			}

			if ($line == 'private') $inPublished = false;
			if ($line == 'protected') $inPublished = false;
			if ($line == 'public') $inPublished = false;
			if ($line == 'strict private') $inPublished = false;
			if ($line == 'strict protected') $inPublished = false;

			$aa = false;
			if (strpos($line, "class(TFrame)") !== false) $aa = true;
			if (strpos($line, "class(TForm)") !== false) $aa = true;
			if (strpos($line, "class(TDXForm)") !== false) $aa = true;
			if (strpos($line, "class(TDataModule") !== false) $aa = true;
            if (!$insideClass && $aa) {
                $insideClass = true;
				$inPublished = true;
            }

            if ($insideClass) {
                // Skip methods and functions
                if (strpos($line, 'procedure') === 0 || strpos($line, 'function') === 0) {
                    continue;
                }

                // Only capture actual component declarations
                if ($inPublished && strpos($line, ":") !== false && preg_match('/^[a-zA-Z0-9_äöüÄÖÜß]+: T[a-zA-Z0-9_äöüÄÖÜß]+;$/', $line)) {
                    $componentName = trim(explode(':', $line)[0]);
                    $components[] = $componentName;
                }

                if (strpos($line, "end;") === 0) {
                    $insideClass = false;
                }
            }
        }
        fclose($file);
    } else {
        echo "Unable to open PAS file: $pasPath";
    }

    return $components;
}

function compareComponents($dfmComponents, $pasComponents) {

	foreach ($dfmComponents as &$x) $x = mb_strtolower($x);
	foreach ($pasComponents as &$x) $x = mb_strtolower($x);

    $dfmOnly = array_diff($dfmComponents, $pasComponents);
    $pasOnly = array_diff($pasComponents, $dfmComponents);

    return [$dfmOnly, $pasOnly];
}

$dfmFiles = findDFMFiles($directory);
foreach ($dfmFiles as $xx) {
	if (strpos($xx, 'VCL_WOLL2WOLL') !== false) continue;
	if (strpos($xx, 'VCL_DELPHIZIP') !== false) continue;
	if (strpos($xx, '_CRW11') !== false) continue;

	$dfmPath = $xx;
	$pasPath = str_replace('.dfm', '.pas', $xx);

	if (!file_exists($pasPath)) continue;

	if (strpos(file_get_contents($dfmPath), chr(255)) !== false) continue;

	$dfmComponents = extractComponentsFromDfm($dfmPath);
	$pasComponents = extractComponentsFromPas($pasPath);

	list($dfmOnly, $pasOnly) = compareComponents($dfmComponents, $pasComponents);

	array_shift($dfmOnly); // form name itself

	if (count($dfmOnly)+count($pasOnly) != 0) {
		echo "====== $xx\n";
		echo "Components only in DFM:\n";
		print_r($dfmOnly);
		echo "\nComponents only in PAS:\n";
		print_r($pasOnly);
	}
}

function findDFMFiles($dir) {
    $rii = new RecursiveIteratorIterator(new RecursiveDirectoryIterator($dir));

    $files = [];
    foreach ($rii as $file) {
        if ($file->isDir()) {
            continue;
        }

        if (strtolower(pathinfo($file->getFilename(), PATHINFO_EXTENSION)) === 'dfm') {
            $files[] = $file->getPathname();
        }
    }

    return $files;
}

echo "Done.\n";
