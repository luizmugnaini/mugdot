require("nvim-treesitter.configs").setup({
	-- A list of parser names, or "all" (the five listed parsers should always be installed)
	ensure_installed = {
		"c",
		"cpp",
		"lua",
		"rust",
		"python",
		"latex",
		"markdown",
	},
	sync_install = false,
	auto_install = false,

	indent = {
		enable = false,
	},

	highlight = {
		enable = true,
		disable = { "latex" },
		additional_vim_regex_highlighting = false,
	},
})
