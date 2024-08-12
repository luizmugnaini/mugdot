-- =============================================================================
-- My Wezterm configuration.
--
-- Author: Luiz G. Mugnaini A. <luizmugnaini@gmail.com>
-- =============================================================================

local wezterm = require("wezterm")
local config = wezterm.config_builder()

if wezterm.target_triple == "x86_64-pc-windows-msvc" then
    config.font = wezterm.font("Terminus (TTF) for Windows")

    config.default_prog = {
        "cmd.exe",
        "/k",
        "%CMDER_ROOT%\\vendor\\init.bat",
        "&&",
        "'C:\\Program Files\\Microsoft Visual Studio\\2022\\Community\\VC\\Auxiliary\\Build\\vcvarsall.bat'",
        "x64",
        "> NUL",
    }
    config.default_cwd = "D:\\"
else
    config.font = wezterm.font("Terminus (TTF)")

    config.default_prog = { "zsh" }
    config.default_cwd = "~/"
end

config.font_size = 12
config.window_frame = { font = config.font, font_size = 9 }
config.colors = {
    background = "#0d0e1c",
    foreground = "#ffffff",
}

config.leader = { key = "Space", mods = "CTRL" }
config.keys = {
    {
        key = "f",
        mods = "LEADER",
        action = wezterm.action.ToggleFullScreen,
    },
    {
        key = "r",
        mods = "LEADER",
        action = wezterm.action.ReloadConfiguration,
    },
    {
        key = "o",
        mods = "LEADER",
        action = wezterm.action.ActivateLastTab,
    },
    {
        key = "v",
        mods = "LEADER",
        action = wezterm.action.SplitVertical({ domain = "CurrentPaneDomain" }),
    },
}
for tab_idx = 1, 9 do
    table.insert(config.keys, {
        key = tostring(tab_idx),
        mods = "LEADER",
        action = wezterm.action.ActivateTab(tab_idx - 1),
    })
end

return config
