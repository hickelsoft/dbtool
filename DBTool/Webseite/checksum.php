<?php

$datei = 'DBTool_Setup.exe';

$local_file = __DIR__.'/../../kunden/Programmdateien Sonstiges/'.$datei;

echo strtolower(md5_file($local_file));
