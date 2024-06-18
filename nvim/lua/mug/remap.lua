local all_modes = { "n", "i", "x", "v", "s", "c", "o", "l", "t" }
local non_insert_modes = { "n", "x", "o" }

vim.keymap.set(all_modes, "<C-k>", "<Esc>", { desc = "Escape to normal mode", silent = true })

vim.keymap.set(non_insert_modes, "<leader>w", vim.cmd.write, { desc = "[W]rite file" })

vim.keymap.set(non_insert_modes, "<leader>qq", vim.cmd.quit, { desc = "Kill current window" })

vim.keymap.set("v", "<C-y>", '"+y', { desc = "Copy to external clipboard" })

vim.keymap.set(non_insert_modes, "<leader>bf", vim.cmd.Ex, { desc = "Native [B]rowse {F}iles" })

-- Splits
vim.keymap.set(non_insert_modes, "<leader>ss", vim.cmd.vsplit, { silent = true, desc = "[S]plit [V]ertically" })
vim.keymap.set(non_insert_modes, "<leader>sh", vim.cmd.split, { silent = true, desc = "[S]plit [H]orizontally" })

-- Move between splits
vim.keymap.set(
	non_insert_modes,
	"<leader>o",
	"<cmd>wincmd w<CR>",
	{ silent = true, desc = "Move to the next window split" }
)

-- ctags
vim.keymap.set({ "n", "x", "o" }, "<leader>gt", "<C-]>", { desc = "Go to definition using ctags" })
vim.keymap.set({ "n", "x", "o" }, "<leader>gb", vim.cmd.pop, { desc = "Go to [P]revious [T]ag" })
vim.keymap.set({ "n", "x", "o" }, "<leader>ut", "<cmd>!ctags -R<CR>", { desc = "Update the tag cache" })
