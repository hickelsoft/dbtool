@echo off

cd /d "D:\CORAplus\BAP\bin"

for /r %%a in (*.exe) do (
call "%~dp0sign_single.bat" "%%a"
)

for /r %%a in (*.dll) do (
call "%~dp0sign_single.bat" "%%a"
)


cd /d "D:\CORA2012\BAP\bin"

for /r %%a in (*.exe) do (
call "%~dp0sign_single.bat" "%%a"
)

for /r %%a in (*.dll) do (
call "%~dp0sign_single.bat" "%%a"
)


pause.
