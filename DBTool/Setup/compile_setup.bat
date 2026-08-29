@echo off

call "C:\SVN\Delphi\DevTools\AuthentiCode Sign\sign_single.bat" "C:\HS-Service\DBTool\DBTool32.exe"
call "C:\SVN\Delphi\DevTools\AuthentiCode Sign\sign_single.bat" "C:\HS-Service\DBTool\DBTool64.exe"
call "C:\SVN\Delphi\DevTools\AuthentiCode Sign\sign_single.bat" "C:\HS-Service\DBTool\DBTool32.enu"
call "C:\SVN\Delphi\DevTools\AuthentiCode Sign\sign_single.bat" "C:\HS-Service\DBTool\DBTool64.enu"

"C:\SVN\Delphi\MiniPHP\php_files64\php.exe" "C:\SVN\Delphi\_PHP\SBOM_Generator\sbom_dbtool.phps"

"C:\Program Files (x86)\Inno Setup 6\ISCC.exe" dbtool_setup.iss

pause.
