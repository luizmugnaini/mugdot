vim.keymap.set("n", "<leader>b", vim.cmd.Ex, { desc = "[B]rowse files" })

-- Escaping
vim.keymap.set("n", "<C-k>", "<Esc>")
vim.keymap.set("i", "<C-k>", "<Esc>")
vim.keymap.set("x", "<C-k>", "<Esc>")
vim.keymap.set("v", "<C-k>", "<Esc>")

-- Saving files
vim.keymap.set("n", "<leader>w", ":w<CR>")

-- Copying to external clipboard
vim.keymap.set("v", "<C-y>", '"+y')
