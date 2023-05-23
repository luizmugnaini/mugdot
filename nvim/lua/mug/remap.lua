vim.keymap.set("n", "<leader>pv", vim.cmd.Ex)

-- Escaping
vim.keymap.set("n", "<C-k>", "<Esc>")
vim.keymap.set("i", "<C-k>", "<Esc>")
vim.keymap.set("x", "<C-k>", "<Esc>")
vim.keymap.set("v", "<C-k>", "<Esc>")

-- Saving files
vim.keymap.set("i", "<C-x><C-s>", ":w")
vim.keymap.set("n", "<C-x><C-s>", ":w")
