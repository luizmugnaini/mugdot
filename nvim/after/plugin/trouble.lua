require("trouble").setup({
	height = 3,
	auto_open = false,
	auto_close = true,
	auto_preview = false,
	icons = false,
})

vim.keymap.set("n", "<leader>tt", function()
	require("trouble").open()
end, { desc = "[T]trouble [T]oggle" })
