local all_modes = { "n", "i", "x", "v", "s", "c", "o", "l", "t" }

vim.keymap.set(all_modes, "<C-k>", "<Esc>", { desc = "Escape to normal mode", silent = true })

local non_insert_modes = { "n", "x", "o" }

vim.keymap.set(non_insert_modes, "<leader>w", vim.cmd.write, { desc = "[W]rite file" })

vim.keymap.set(non_insert_modes, "<leader>qq", vim.cmd.quit, { desc = "Kill current instance" })

vim.keymap.set("v", "<C-y>", '"+y', { desc = "Copy to external clipboard" })

vim.keymap.set(non_insert_modes, "<leader>bf", vim.cmd.Ex, { desc = "Native [B]rowse {F}iles" })

-- Splits
vim.keymap.set(non_insert_modes, "<leader>sv", vim.cmd.vsplit, { silent = true, desc = "[S]plit [V]ertically" })
vim.keymap.set(non_insert_modes, "<leader>sh", vim.cmd.split, { silent = true, desc = "[S]plit [H]orizontally" })

-- Move between splits
vim.keymap.set(non_insert_modes, "<leader>vh", "<cmd>wincmd h<CR>", { silent = true, desc = "Move to left split" })
vim.keymap.set(non_insert_modes, "<leader>vj", "<cmd>wincmd j<CR>", { silent = true, desc = "Move to down split" })
vim.keymap.set(non_insert_modes, "<leader>vk", "<cmd>wincmd k<CR>", { silent = true, desc = "Move to up split" })
vim.keymap.set(non_insert_modes, "<leader>vl", "<cmd>wincmd l<CR>", { silent = true, desc = "Move to right split" })

-- ctags
vim.keymap.set({ "n", "x", "o" }, "<leader>gt", "<C-]>", { desc = "Go to definition using ctags" })
vim.keymap.set({ "n", "x", "o" }, "<leader>pt", vim.cmd.pop, { desc = "Go to [P]revious [T]ag" })
vim.keymap.set({ "n", "x", "o" }, "<leader>ut", "<cmd>!ctags -R<CR>", { desc = "Update the tag cache" })
