-- =============================================================================
-- My Wezterm configuration.
--
-- Author: Luiz G. Mugnaini A. <luizmugnaini@gmail.com>
-- =============================================================================

local wezterm = require("wezterm")
local config = wezterm.config_builder()
local mux = wezterm.mux

-- Start with a maximized window.
wezterm.on("gui-startup", function()
    local tab, pane, window = mux.spawn_window({})
    window:gui_window():maximize()
end)

local font = {
    terminus  = { name = "Terminus (TTF) for Windows", size = 12 },
    fira_code = { name = "FiraCode Nerd Font Mono",    size = 10  },
}

local default_font = font.terminus

if wezterm.target_triple == "x86_64-pc-windows-msvc" then
    config.font = wezterm.font(default_font.name)

    config.default_prog = { "cmd.exe", "/k", "%CMDER_ROOT%\\vendor\\init.bat" }
    config.default_cwd = "D:\\"
else
    config.font = wezterm.font("Terminus")

    config.default_prog = { "zsh" }
    config.default_cwd  = "~/"
end

config.font_size    = default_font.size
config.window_frame = { font = config.font, font_size = 9 }

local colors = {
    grey    = "#2b3045",
    blue    = "#79a8ff",
    green   = "#6ae4b9",
    magenta = "#b6a0ff",
    red     = "#ff7f9f",
    white   = "#C8C093",
    yellow  = "#f1b090",
}
config.colors = {
    background = "#0d0e1c",
    foreground = "#ffffff",
    ansi = {
        colors.grey,
        colors.red,
        colors.green,
        colors.yellow,
        colors.blue,
        colors.magenta,
        colors.white,
        colors.magenta,
    },
    brights = {
        colors.grey,
        colors.red,
        colors.green,
        colors.yellow,
        colors.blue,
        colors.magenta,
        colors.white,
        colors.magenta,
    },
}

config.audible_bell = "Disabled"

config.leader = { key = "Space", mods = "CTRL" }
config.keys = {
    {
        key = "t",
        mods = "LEADER",
        action = wezterm.action.SpawnTab("CurrentPaneDomain"),
    },
    -- Pane creation and movement.
    {
        key = "v",
        mods = "LEADER",
        action = wezterm.action.SplitHorizontal({ domain = "CurrentPaneDomain" }),
    },
    {
        key = "h",
        mods = "LEADER",
        action = wezterm.action.ActivatePaneDirection("Left"),
    },
    {
        key = "j",
        mods = "LEADER",
        action = wezterm.action.ActivatePaneDirection("Down"),
    },
    {
        key = "k",
        mods = "LEADER",
        action = wezterm.action.ActivatePaneDirection("Up"),
    },
    {
        key = "l",
        mods = "LEADER",
        action = wezterm.action.ActivatePaneDirection("Right"),
    },
    -- Misc.
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
}

-- Activate tab by its index (starts at 1).
for tab_idx = 1, 9 do
    table.insert(config.keys, {
        key = tostring(tab_idx),
        mods = "LEADER",
        action = wezterm.action.ActivateTab(tab_idx - 1),
    })
end

return config
