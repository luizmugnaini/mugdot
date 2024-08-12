-- =============================================================================
-- My Wezterm configuration.
--
-- Author: Luiz G. Mugnaini A. <luizmugnaini@gmail.com>
-- =============================================================================

local wezterm = require("wezterm")
local config = wezterm.config_builder()
local mux = wezterm.mux

-- Start in fullscreen.
wezterm.on("gui-startup", function(cmd)
    local tab, pane, window = mux.spawn_window({ cmd or {} })
    window:gui_window():maximize()
end)

config.font = wezterm.font("Terminus (TTF) for Windows")
config.font_size = 12
config.window_frame = { font = config.font, font_size = 9 }
config.colors = {
    background = "#0d0e1c",
    foreground = "#ffffff",
}

config.default_prog = {
    "cmd.exe",
    "/k",
    "%CMDER_ROOT%\\vendor\\init.bat",
    "&&",
    '"C:\\Program Files\\Microsoft Visual Studio\\2022\\Community\\VC\\Auxiliary\\Build\\vcvarsall.bat" x64 > NUL',
}
config.default_cwd = "D:/"

return config
