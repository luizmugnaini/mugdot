$env:starship_config = "$HOME\.config\starship\starship.toml"
Invoke-Expression (&starship init powershell)
Invoke-Expression (& { (zoxide init powershell | Out-String) })

Import-Module -Name Terminal-Icons

Set-Alias -Name vim -Value nvim
Set-Alias -Name v -Value nvim

function l($dir) {
    eza --icons --group-directories-first $dir
}

function la($dir) {
    eza --icons --group-directories-first -a $dir
}

function ll($dir) {
    eza --long --icons --git --group-directories-first $dir
}

function lla($dir) {
    eza --long --icons --git --group-directories-first -a $dir
}

function mklink($name, $val) {
    New-Item -Name $name -Type SymbolicLink -Value $val
}

function raddbg {
    Invoke-Expression $ExecutionContext.InvokeCommand.ExpandString("$HOME\Programs\raddebugger\build\raddbg.exe")
}

function allenv {
    gci env:* | sort-object name
}

function remove-env($name) {
    [Environment]::SetEnvironmentVariable("$name", $null, "Machine")
}
