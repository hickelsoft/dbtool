@echo off

cd /d "K:\Programmdateien Getr„nkehandel"

for /r %%a in (*.exe) do (
call "%~dp0sign_single.bat" "%%a"
)

for /r %%a in (*.dll) do (
call "%~dp0sign_single.bat" "%%a"
)


cd /d "K:\Programmdateien Handel"

for /r %%a in (*.exe) do (
call "%~dp0sign_single.bat" "%%a"
)

for /r %%a in (*.dll) do (
call "%~dp0sign_single.bat" "%%a"
)


pause.
