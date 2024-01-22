vim.keymap.set("n", "<leader>bf", vim.cmd.Ex, { desc = "Native [B]rowse {F}iles" })

vim.keymap.set("n", "<leader>nn", vim.cmd.bnext, { desc = "[N]ext buffer" })
vim.keymap.set("n", "<leader>pp", vim.cmd.bprevious, { desc = "[P]revious buffer" })

vim.keymap.set(
	{ "n", "i", "x", "v", "s", "c", "o", "l", "t" },
	"<C-k>",
	"<Esc>",
	{ desc = "Escape to normal mode", silent = true }
)

vim.keymap.set("n", "<leader>w", "<cmd>w<CR>", { desc = "[W]rite file" })

vim.keymap.set("v", "<C-y>", '"+y', { desc = "Copy to external clipboard" })

vim.keymap.set("n", "<leader>hh", "<cmd>wincmd h<CR>", { silent = true, desc = "Move to left split pane" })
vim.keymap.set("n", "<leader>jj", "<cmd>wincmd j<CR>", { silent = true, desc = "Move to down split pane" })
vim.keymap.set("n", "<leader>kk", "<cmd>wincmd k<CR>", { silent = true, desc = "Move to up split pane" })
vim.keymap.set("n", "<leader>ll", "<cmd>wincmd l<CR>", { silent = true, desc = "Move to right split pane" })
