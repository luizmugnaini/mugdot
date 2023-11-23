-- Strangely, if we have treesitter indent disabled in a python file we get all sorts of weird
-- indentation errors.
require("nvim-treesitter.configs").setup({
	indent = {
		enable = true,
	},
})
