<?php

$datei = 'DBTool_Setup.exe';

$local_file = __DIR__.'/../../kunden/DBTool/'.$datei;

if (!file_exists($local_file)) {
	header('HTTP/1.1 404 Not Found');
	die('Datei nicht gefunden');
}

header('Content-Disposition: inline; filename="'.$datei.'"');
header('Content-Type: application/octet-stream');
header('Content-Length: '.filesize($local_file));

readfile($local_file);
