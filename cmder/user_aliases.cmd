;= @echo off
;= rem Call DOSKEY and use this file as the macrofile
;= %SystemRoot%\system32\doskey /listsize=1000 /macrofile=%0%
;= rem In batch mode, jump to the end of the file
;= goto:eof
;= Add aliases below here
e.=explorer .
pwd=cd
clear=cls
vim=nvim $*
cmderr=cd /d "%CMDER_ROOT%"
pwsh=%SystemRoot%/System32/WindowsPowerShell/v1.0/powershell.exe -ExecutionPolicy Bypass -NoLogo -NoProfile -NoExit -Command "Invoke-Expression '. ''%CMDER_ROOT%/vendor/profile.ps1'''"
l=eza --icons --group-directories-first
la=eza --icons --group-directories-first -a
ll=eza --long --icons --git --group-directories-first
lla=eza --long --icons --git --group-directories-first -a
raddbg=D:\app\raddebugger\build\raddbg.exe
remedy=D:\app\remedybg\remedybg.exe
python=python3
p=python3
mugdot=cd /d "%HOME%/.config/mugdot"
dev=cd /d "D:/"
