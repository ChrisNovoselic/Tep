@echo off

set name_file_log=���_��-%1.log

set path_db_tep=W:\KTC
set path_distr_tep=%path_db_tep%\A\TEPW_RT
rem set path_local_tep=D:\TEPW_RT

rem echo %path_db_tep%
rem echo %path_distr_tep%
rem echo %path_local_tep%

rem date /t
rem time /t

rem if exist %name_file_log% (
echo. >> %name_file_log%
) else (
)

set msg_log=%DATE%
set msg_log=%msg_log% %TIME%
@echo %msg_log% >> %name_file_log%

set msg_log=�믮������ ����⭮�� 䠩�� ��� ����᪠ �ணࠬ�� ����� ���
@echo %msg_log% >> %name_file_log%

if not exist %path_db_tep% (
SET msg_unfortunately=�������� �� �������� �⥢�� ��� W:\=//KASKAD/User
GOTO UNFORTUNATELY
)

rem ��।������ ࠧ��� 䠩�� ???
rem for /f "delims=" %%i in (%name_file_log%) do (
rem set sz=%%~zi
rem echo %%~nxi
rem )
rem echo %sz% >> %name_file_log%
rem pause

set msg_log=�������� ⥪�饩 ���ᨨ �ணࠬ�� ����� ���
@echo %msg_log% >> %name_file_log%

erase tep.exe
IF %ERRORLEVEL% == 0 GOTO ERASE_INBLOCK
SET msg_unfortunately=�� 㤠���� 㤠���� ⥪���� ����� �ணࠬ�� ����� ���
GOTO UNFORTUNATELY

:ERASE_INBLOCK
set msg_log=�������� ⥪�饩 ���ᨨ �室��� ⠡���� ��� ����� ���
@echo %msg_log% >> %name_file_log%

erase inblok.dbf
IF %ERRORLEVEL% == 0 GOTO COPY_TEP
SET msg_unfortunately=�� 㤠���� 㤠���� ⥪���� �����  �室��� ⠡���� ��� ����� ���
GOTO UNFORTUNATELY

:COPY_TEP
set msg_log=����஢���� ���㠫쭮� ���ᨨ �ணࠬ�� ����� ���
@echo %msg_log% >> %name_file_log%

@copy %path_distr_tep%\tep.exe *.* /Y
IF %ERRORLEVEL%==0 GOTO COPY_INBLOCK
SET msg_unfortunately=�� 㤠���� ᪮��஢���� ���㠫��� ����� �ணࠬ�� ����� ���
GOTO UNFORTUNATELY

:COPY_INBLOCK
set msg_log=����஢���� ���㠫쭮� ���ᨨ  �室��� ⠡���� ��� ����� ���
@echo %msg_log% >> %name_file_log%

@copy %path_distr_tep%\inblok.dbf *.* /Y
IF %ERRORLEVEL%==0 GOTO RUN_TEP
SET msg_unfortunately=�� 㤠���� ᪮��஢���� ���㠫��� ����� �室��� ⠡���� ��� ����� ���
GOTO UNFORTUNATELY

:UNFORTUNATELY
@echo %msg_unfortunately% >> %name_file_log%
@echo %msg_unfortunately% (34-70)
pause
exit

:RUN_TEP
set msg_log=����� �ணࠬ�� ����� ���
@echo %msg_log% >> %name_file_log%

rem pause

rem echo %0
rem echo %1
rem echo %path_local_tep%\tep.exe /%1 %path_db_tep% /RT 1100
rem pause

rem start %path_local_tep%\tep.exe /%1 %path_db_tep% /RT 1100
start tep.exe /%1 %path_db_tep% /RT 1100
IF %ERRORLEVEL%==0 GOTO SUCCESS
SET msg_unfortunately=�� 㤠���� �������� �ணࠬ�� ����� ���
GOTO UNFORTUNATELY

:SUCCESS
set msg_log=�ᯥ�
@echo %msg_log% >> %name_file_log%
exit

