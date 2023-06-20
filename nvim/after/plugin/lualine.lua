require("lualine").setup {
    options = {
        icons_enabled = true,
        theme = "onedark",
        component_separators = { left = "", right = "|" },
        section_separators = { left = "", right = "" }
    },
    sections = {
        lualine_a = { "mode" },
        lualine_b = { "branch", "diagnostics" },
        lualine_c = { "filename" },
        lualine_x = { "fileformat", "filetype" },
        lualine_y = { "progress" },
        lualine_z = { "location" }
    }
}
