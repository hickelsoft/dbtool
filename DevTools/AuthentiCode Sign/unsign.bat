@echo off

set SDKVER=10.0.26100.0

if "%~1"=="" goto :done

echo Unsign %1 ...

"C:\Program Files (x86)\Windows Kits\10\bin\%SDKVER%\x64\signtool.exe" remove /s %1

pause.
