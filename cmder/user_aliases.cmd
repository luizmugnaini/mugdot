;= @echo off
;= rem Call DOSKEY and use this file as the macrofile
;= %SystemRoot%\system32\doskey /listsize=1000 /macrofile=%0%
;= rem In batch mode, jump to the end of the file
;= goto:eof
;= Add aliases below here
e.=explorer .
pwd=cd
clear=cls
pwsh=%SystemRoot%/System32/WindowsPowerShell/v1.0/powershell.exe -ExecutionPolicy Bypass -NoLogo -NoProfile -NoExit -Command "Invoke-Expression '. ''%CMDER_ROOT%/vendor/profile.ps1'''"

ls=ls -g --color=always --group-directories-first --human-readable -X --almost-all $*

ctags=C:/Users/luizm/scoop/app/universal-ctags/current/ctags.exe
python=python3 $*

home=cd /d "%USERPROFILE%"
mugdot=cd /d "%USERPROFILE%/.config/mugdot"
dev=cd /d D:/$*
