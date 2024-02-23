vim.keymap.set("n", "<leader>tt", function()
	require("trouble").open()
end, { desc = "[T]trouble [T]oggle" })
