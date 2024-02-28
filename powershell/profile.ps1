$env:starship_config = "$HOME\.config\starship\starship.toml"
Invoke-Expression (&starship init powershell)
Invoke-Expression (& { (zoxide init powershell | Out-String) })

Import-Module -Name Terminal-Icons

Set-Alias -Name vim -Value nvim
Set-Alias -Name v -Value nvim

function l {
    eza --icons --group-directories-first
}

function la {
    eza --icons --group-directories-first -a
}

function ll {
    eza --long --icons --git --group-directories-first
}

function lla {
    eza --long --icons --git --group-directories-first -a
}

function mklink($name, $val) {
    New-Item -Name $name -Type SymbolicLink -Value $val
}

function raddbg {
    Invoke-Expression $ExecutionContext.InvokeCommand.ExpandString("$HOME\Programs\raddebugger\build\raddbg.exe")
}
