require("todo-comments").setup({
	keywords = {
		TODO = { color = "warning" },
	},
})

vim.keymap.set("n", "<leader>td", "<cmd>TodoTelescope<CR>", { desc = "Open [T]o[D]o list in Telescope" })
