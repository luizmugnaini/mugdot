:: use this file to run your own startup commands
:: use in front of the command to prevent printing the command

:: uncomment this to have the ssh agent load when cmder starts
:: call "%GIT_INSTALL_ROOT%/cmd/start-ssh-agent.cmd" /k exit

:: uncomment the next two lines to use pageant as the ssh authentication agent
:: SET SSH_AUTH_SOCK=/tmp/.ssh-pageant-auth-sock
:: call "%GIT_INSTALL_ROOT%/cmd/start-ssh-pageant.cmd"

:: you can add your plugins to the cmder path like so
:: set "PATH=%CMDER_ROOT%\vendor\whatever;%PATH%"

:: arguments in this batch are passed from init.bat, you can quickly parse them like so:
:: more usage can be seen by typing "cexec /?"

:: %ccall% "/customOption" "command/program"

@echo off

:: Set env variables for the MSVC toolchain.
call "C:/Program Files/Microsoft Visual Studio/2022/Community/VC/Auxiliary/Build/vcvarsall.bat" x64 > NUL

:: Latex-related binaries.
set "PATH=%HOME%/AppData/Local/Programs/MiKTeX/miktex/bin/x64;%PATH%"
