<?php

$files = array_merge(
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

foreach ($files as $pas_file) {

	$pas = file_get_contents($pas_file);
	$dfm_file = str_ireplace('.pas', '.dfm', $pas_file);
	if (!file_exists($dfm_file)) continue;

	if (strpos($dfm_file,'_CRW11')) continue;
	if (strpos($dfm_file,'VCL_WOLL2WOLL')) continue;
	if (strpos($dfm_file,'ZZ___ABGESCHALTETE')) continue;

	$dfm = file_get_contents($dfm_file);

	$pas = preg_replace('@interface\((.+)end;@ismU','',$pas); // hcg_Suche.pas

	preg_match_all('@ ([^ \(]+)\s*\(Sender\s*:@ismU', $pas, $m);
	foreach ($m[1] as &$a) {
		if (stripos($a, '.') !== false) {
			$a = explode('.', $a)[1];
		}
	}
	$m[1] = array_unique($m[1]);
	foreach ($m[1] as $a) {
		if (stripos($dfm, $a) === false) {
			if (substr_count(strtolower($pas), strtolower($a)) <= 2) {
				// hcg_Utils.pas
				if ((basename($pas_file) == 'hcg_Utils.pas') && ($a == 'Def_VordergrundFarbe')) continue;
				if ((basename($pas_file) == 'hcg_Utils.pas') && ($a == 'Def_HintergrundFarbe')) continue;
				if ((basename($pas_file) == 'hcg_Utils.pas') && ($a == 'Def_VordergrundFarbeHighlight')) continue;
				if ((basename($pas_file) == 'hcg_Utils.pas') && ($a == 'Def_HintergrundFarbeHighlight')) continue;
				if ((basename($pas_file) == 'hcg_Utils.pas') && ($a == 'Def_EditCheck')) continue;
				if ((basename($pas_file) == 'hcg_Utils.pas') && ($a == 'Def_EditEnter')) continue;
				if ((basename($pas_file) == 'hcg_Utils.pas') && ($a == 'Def_EditExit')) continue;
				if ((basename($pas_file) == 'hcg_Utils.pas') && ($a == 'Def_NumericKeyPress')) continue;

				// CORA_Verwaltung/src/Hauptfenster.pas
				if ((basename($pas_file) == 'Hauptfenster.pas') && ($a == 'Def_KeyPress')) continue;

				// HS_Info2/Src/Haupt.pas
				if ((basename($pas_file) == 'Haupt.pas') && ($a == 'F6KeyEvent')) continue;

				echo "$pas_file : $a\n";
			}
		}
	}

}

echo "Fertig.";

# ---

function file_replace_contents($filename, $search, $replace) {
	$cont = file_get_contents($filename);
	if ($cont === false) return;
	$cont = str_ireplace($search, $replace, $cont);
	file_put_contents($filename, $cont);
}

function file_contains($filename, $search) {
	$cont = file_get_contents($filename);
	return stripos($cont, $search) !== false;
}
