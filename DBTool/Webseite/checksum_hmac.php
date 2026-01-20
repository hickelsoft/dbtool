<?php

$datei = 'DBTool_Setup.hmac';

$local_file = __DIR__.'/../../kunden/Programmdateien Sonstiges/'.$datei;

echo file_get_contents($local_file);
