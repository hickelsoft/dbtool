<?php

$files = [];

/*
$files = array_merge($files,glob('C:\\SVN\\Delphi\\HS_Info2\\*.pas'));
$files = array_merge($files,glob('C:\\SVN\\Delphi\\HS_Info2\\*\\*.pas'));
$files = array_merge($files,glob('C:\\SVN\\Delphi\\HS_Info2\\*\\*\\*.pas'));
$files = array_merge($files,glob('C:\\SVN\\Delphi\\HS_Info2\\*\\*\\*\\*.pas'));
*/

$files = array_merge($files,glob('C:\\SVN\\Delphi\\DBTool\\*.pas'));
$files = array_merge($files,glob('C:\\SVN\\Delphi\\DBTool\\*\\*.pas'));
$files = array_merge($files,glob('C:\\SVN\\Delphi\\DBTool\\*\\*\\*.pas'));
$files = array_merge($files,glob('C:\\SVN\\Delphi\\DBTool\\*\\*\\*\\*.pas'));

$files = array_merge($files,glob('C:\\SVN\\Delphi\\Hickel_RTL\\*.pas'));
$files = array_merge($files,glob('C:\\SVN\\Delphi\\Hickel_RTL\\*\\*.pas'));
$files = array_merge($files,glob('C:\\SVN\\Delphi\\Hickel_RTL\\*\\*\\*.pas'));
$files = array_merge($files,glob('C:\\SVN\\Delphi\\Hickel_RTL\\*\\*\\*\\*.pas'));

$files = array_merge($files,glob('C:\\SVN\\Delphi\\Hickel_VCL\\*.pas'));
$files = array_merge($files,glob('C:\\SVN\\Delphi\\Hickel_VCL\\*\\*.pas'));
$files = array_merge($files,glob('C:\\SVN\\Delphi\\Hickel_VCL\\*\\*\\*.pas'));
$files = array_merge($files,glob('C:\\SVN\\Delphi\\Hickel_VCL\\*\\*\\*\\*.pas'));

$cnt_komplett = 0;

foreach ($files as $file) {

	$cont = file_get_contents($file);




	if (stripos($file,'HsDlgLizenzErstellen.pas') !== false) continue; // HickelSOFT spezifisch. Muss nicht übersetzt werden


	//if (stripos($file,'Haupt.pas') !== false) continue; // TEST
	//if (stripos($file,'TicketStat.pas') !== false) continue; // TEST
	//if (stripos($file,'HsInfo2_Funcs.pas') === false) continue; // TEST



	$cont = str_replace("''", "", $cont);

	do {
		$cont = preg_replace("@resourcestring\s*(//){0,1}\s*S_(.+)\s*=\s*'(.+)';\s*(//([^\\n]+)){0,1}\n@imU", 'resourcestring', $cont, -1, $cnt);
	} while ($cnt > 0);

	$cont = preg_replace("@\{\\\$REGION\s*'(.*)'\}@ismU", '', $cont);

	preg_match_all("@'([^']*)'(.+)\$@ismU", $cont, $m, PREG_SET_ORDER);

	$cnt_do_not_localize = 0;
	$strings = [];
	foreach ($m as $data) {
		if (!preg_match('@[a-zäöüßÄÖÜ]@i', $data[1])) continue;
		if (stripos($data[2],'do not localize') === false) {
			$strings[] = $data[1];
		} else {
			$cnt_do_not_localize++;
		}
	}

	if (count($strings) > 0) {
		echo "$file (".count($strings).")\n";
		foreach ($strings as $s) {
			//echo "- $s\n";
			$cnt_komplett++;
		}
		//echo "\n";
	}

	if ($cnt_do_not_localize != substr_count($cont, 'do not localize')) {
		// TODO echo "! $file do not localize found ".substr_count($cont, 'do not localize')." but should be $cnt_do_not_localize\n";
	}

}

echo "Done. ($cnt_komplett Strings)\n";
