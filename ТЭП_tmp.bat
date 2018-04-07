@echo off

set myfile=tep.exe
set path=G:\
if not exist %path% set msg=НЕ
echo %path% %msg% существует

for /f "delims=" %%i in ('dir /od /b *.prg') do (
rem for /f "delims=" %%i in (%myfile%) do (
echo %%~nxi-%%~zi
set size=%%~zi
echo %size%

rem pause
)

pause