<?php

// This script checks Delphi Pascal files (*.pas) and tries to find out
// if there are missing .Free or FreeAndNil
// (C) 2017 Daniel Marschall, ViaThinkSoft

$dir = 'C:\\SVN\\Delphi\\';
if (isset($argv[1])) $dir = $argv[1];

$exclude_dir = array();
$exclude_dir[] = $dir . '/Cora_Verwaltung/Src/GoBD/Res/GoBD_DocToINI/TLB/';
$exclude_dir[] = $dir . '/_CRW11/';
$exclude_dir[] = $dir . '/VCL_WOLL2WOLL/';
$exclude_dir[] = $dir . '/VCL_ZXING/';
$exclude_dir[] = $dir . '/ZZ___ABGESCHALTETE/';

_start($dir);

/*

procedure Bad1;
var
  x: TObject;
begin
  x := TFoo.Create; // Memory Leak
end;

procedure Good1;
var
  x: TObject;
begin
  x := TFoo.Create;
  FreeAndNil(x);
end;

---

function Bad2: TFoo;
var
  x: TObject;
begin
  x := TFoo.Create;
  result := x;
end;

function Good2: TFoo;
var
  x: TObject;
begin
  x := {LEAK_OK}TFoo.Create;
  result := x;
end;

---

procedure Bad3;
var
  x: TObject;
begin
  x := TFoo.Create;
  if something = 3 then
  begin
    x.Free; // will cause a mismatch between Free and Create. Use try..finally instead!
    exit;
  end;
  x.Free;
end;

procedure Good3;
var
  x: TObject;
begin
  x := TFoo.Create;
  try
    if something = 3 then exit;
  finally
    x.Free;
  end;
end;

---

procedure Bad4;
var
  x, y: TObject;
begin
  x := TFoo.Create;
  y := x;
  y.Free;
end;

procedure Good4;
var
  x, y: TObject;
begin
  x := TFoo.Create;
  y := x;
  x.Free;
end;

---

procedure Bad5;
begin
  with TFoo.Create do
  begin
    ShowMessage(XYZ);
  end;
end;

procedure Good5;
begin
  with TFoo.Create do
  begin
    ShowMessage(XYZ);
    Free;
  end;
end;

---

procedure Bad6;
begin
  with{LEAK_OK} ListView1.Items.Add do
  begin
    Caption := 'XYZ';
  end;
end;

procedure Good6;
begin
  with{LEAK_OK} ListView1.Items.Add do // TListItem will be automatically freed.
  begin
    Caption := 'XYZ';
  end;
end;

---

procedure Bad7;
begin
  with XYZ.Create do
  begin
    if a then
    begin
      abc;
      Free;
      exit;
    end;
    def;
    Free;
  end;
end;

procedure Good7;
begin
  with XYZ.Create do
  begin
    try
      if a then
      begin
        abc;
        exit;
      end;
      def;
    finally
      Free;
    end;
  end;
end;


ATTENTION!
- 2 superfluous frees and 2 additional created will cause a balance and therefore no error!
- Only the count is enumerated. The correct nesting of Create/Free is currently not checked!

*/

// ---

function _start($verzeichnis) {
     $verzeichnis .= (substr($verzeichnis, -1) == '/' ? '' : '/');
     $handle =  opendir($verzeichnis);
     while ($datei = readdir($handle)) {
          if (($datei != '.') && ($datei != '..')) {
               $file = $verzeichnis.$datei;
               if (is_dir($file)) {
                    _start($file.'/');
               } else if (fileExt($file) == 'pas') {
                    checkFile($file);
               }
          }
     }
     closedir($handle);
}

function fileExt($file) {
	$arx = pathinfo($file);
	if (!isset($arx['extension'])) return '';
	return strtolower($arx['extension']);
}

function startsWith($haystack, $needle)
{
     $length = strlen($needle);
     return (substr($haystack, 0, $length) === $needle);
}

function checkFile($file) {
	$cont = file_get_contents($file);

	global $exclude_dir;
	foreach ($exclude_dir as $exclude) if (startsWith($file, $exclude)) return;

	$cont = preg_replace('@//.+[\r\n]@ismU', '', $cont);
	$cont = str_replace('{LEAK_OK}', '**LEAK_OK**', $cont);
	$cont = preg_replace('@\{.+\}@ismU', '', $cont);
	$cont = str_replace('**LEAK_OK**', '{LEAK_OK}', $cont);
	$cont = preg_replace('@\(\s*\*\s*\)@ismU', '', $cont); // count(*) is not a comment
	$cont = preg_replace('@\(\*.+\*\)@ismU', '', $cont);

	$cont = str_ireplace('with format', '', $cont); // this is part of a SQL command

	$cont = preg_replace('@Memo2.Lines.Add\(\'.+\'\)@ismU', '', $cont); // for code generators

	echo $file . PHP_EOL;

	preg_match_all('@\b([^ \(\)\t\r\n]+)\s*:=\s*[^\r\n]+\.Create@ismU', $cont, $mAssign); // TODO: hier kommt dann aber nicht "GetTable" zum Einsatz
	preg_match_all('@\bFreeAndNil\s*\(\s*([^\)]+)\s*\)@ismU', $cont, $mFree1);
	preg_match_all('@\b([^ \(\)\t\r\n]+)\.Free\b@ismU', $cont, $mFree2);

	$vars = array_unique(array_merge($mAssign[1], $mFree1[1], $mFree2[1]));
	foreach ($vars as $varname) {
		preg_match_all('|\b'.$varname.'\s*:=|ismU', $cont, $m2);
		preg_match_all('|\b'.$varname.'\s*:=\s*\{LEAK_OK\}|ismU', $cont, $m4);
		$cnt_assign = count($m2[0]) - count($m4[0]);

		preg_match_all('@\bFreeAndNil\s*\(\s*'.$varname.'\s*\)|\b'.$varname.'\.Free\b@ismU', $cont, $m3);
		$cnt_free = count($m3[0]);

		if ($cnt_assign != $cnt_free) echo "\t$varname : $cnt_assign assign vs. $cnt_free free!" . PHP_EOL;
	}

        preg_match_all('@\bwith\b.+\bdo\b@ismU', $cont, $mAssign);
	preg_match_all('@\sFree\b@ismU', $cont, $mFree);
	$cnt_free = count($mFree[0]);
	$cnt_assign = 0;
        foreach ($mAssign[0] as $dAssign) {
		$cnt_free += strpos($dAssign, '{LEAK_OK}') !== false ? 1 : 0;
		$cnt_assign += stripos($dAssign, 'Create') !== false ? 1 : 0; // TODO: hier kommt dann aber nicht "GetTable" zum Einsatz
	}

	// TODO: Interfaces are freed with :=nil
	// TODO: problem: variable declared in 2 Create override, and freed in Destroy
	// TODO: Records brauchen kein Free

	if ($cnt_assign != $cnt_free) echo "\tWITH BLOCK : $cnt_assign assign vs. $cnt_free free!" . PHP_EOL;
}
