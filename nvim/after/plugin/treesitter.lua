if vim.g.mug_enable_treesitter then
	require("nvim-treesitter.configs").setup({
		ensure_installed = { "c", "cpp", "lua" },
		indent = { enable = false },
		sync_install = false,
		auto_install = false,
		highlight = {
			enable = true,
			disable = { "latex" },
			additional_vim_regex_highlighting = false,
		},
	})
end
