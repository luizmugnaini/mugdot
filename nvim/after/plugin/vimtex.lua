vim.g.vimtex_view_method = "zathura"
vim.g.vimtex_compiler_method = "latexmk -pvc"
vim.g.vimtex_quickfix_mode = 0 -- The quickfix window is never opened/closed automatically.
vim.g.vimtex_syntax_conceal = {
    ["fancy"] = 1,             -- items
    ["greek"] = 1,             -- math mode greek letters
    ["spacing"] = 1,           -- \quad
    ["math_bounds"] = 1,       -- \(...\)
    ["math_super_sub"] = 1,    -- x^2
}
