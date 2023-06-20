require("mug")

require("tokyonight").setup {
    style = "night",
}
vim.cmd.colorscheme("tokyonight")

require("lualine").setup {
    options = {
        icons_enabled = true,
        theme = "tokyonight",
        component_separators = { left = "", right = " |" },
        section_separators = { left = "", right = "" }
    },
    sections = {
        lualine_a = { "mode" },
        lualine_b = { "branch", "diagnostics" },
        lualine_c = { "filename" },
        lualine_x = { "fileformat", "filetype" },
        lualine_z = { "location" }
    }
}
