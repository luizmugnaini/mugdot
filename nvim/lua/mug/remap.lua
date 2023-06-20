vim.keymap.set("n", "<leader>b", vim.cmd.Ex, {desc = "[B]rowse files"})

-- Escaping
local modes = {"n", "i", "x", "v", "s", "c", "o", "l", "t"}
for i = 1, #modes do
    vim.keymap.set(modes[i], "<C-k>", "<Esc>", {desc = "Escape to normal mode"})
end

-- Saving files
vim.keymap.set("n", "<leader>w", ":w<CR>", {desc = "[W]rite file"})

-- Copying to external clipboard
vim.keymap.set("v", "<C-y>", "\"+y")

-- Moving between split panes
local directions = {"h", "j", "k", "l"}
for i = 1, #directions do
    local key = "<leader>p" .. directions[i]
    local command = ":wincmd " .. directions[i] .. "<CR>"
    vim.keymap.set("n", key, command)
end
