$env:starship_config = "$HOME\.config\starship\starship.toml"
Invoke-Expression (&starship init powershell)
Invoke-Expression (& { (zoxide init powershell | Out-String) })

Import-Module -Name Terminal-Icons

Set-Alias -Name vim -Value nvim
Set-Alias -Name v -Value nvim

Function l() {
    eza --icons --group-directories-first
}

Function la() {
    eza --icons --group-directories-first -a
}

Function ll() {
    eza --long --icons --git --group-directories-first
}

Function lla() {
    eza --long --icons --git --group-directories-first -a
}
