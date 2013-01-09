@ECHO OFF
REM time synchronisation with another pc by its name
:LOOP
IF %1!==! GOTO END
net time \\%1 /set /yes
REM timeout /T 1 /nobreak
GOTO LOOP
:END