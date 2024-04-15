local builtin = require("telescope.builtin")

vim.keymap.set("n", "<leader>ff", function()
	builtin.find_files({
		hidden = true,
		file_ignore_patterns = { "node_modules", ".git", "target", "__pycache__", ".cache", "assets" },
	})
end, { desc = "[F]ind [F]ile" })
vim.keymap.set("n", "<leader>fz", function()
	builtin.grep_string({ search = vim.fn.input("grep > ") })
end, { desc = "Fuzzy finder" })
vim.keymap.set("n", "<leader>bb", builtin.buffers, { desc = "[F]ind [B]uffers" })
