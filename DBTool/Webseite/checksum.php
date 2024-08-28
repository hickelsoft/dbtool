<?php

$datei = 'DBTool_Setup.exe';

$local_file = __DIR__.'/../../kunden/DBTool/'.$datei;

echo strtolower(md5_file($local_file));
