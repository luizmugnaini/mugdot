local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"

if not vim.loop.fs_stat(lazypath) then
	vim.fn.system({
		"git",
		"clone",
		"--filter=blob:none",
		"https://github.com/folke/lazy.nvim.git",
		"--branch=stable",
		lazypath,
	})
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
	-- Themes
	{ "sainnhe/gruvbox-material" },

	-- -------------------------------------------------------------------------
	-- Utilities for better development.
	-- * Terminal mode enhancement.
	-- * File navigation with telescope.
	-- * Comment utilities.
	-- * TODO comment highlighting.
	-- * Writting snippets.
	-- -------------------------------------------------------------------------

	-- * Forward search: "s"
	-- * Backwards search: "S"
	{ "ggandor/leap.nvim", dependencies = { "tpope/vim-repeat" } },

	{ "akinsho/toggleterm.nvim", version = "*", event = "VeryLazy" },

	-- Utility for undoing actions and navigating file history.
	-- * Undo: "u" in normal mode.
	-- * Redo: "r" in normal mode.
	-- * Open history: "<leader>-u".
	{ "mbbill/undotree", event = "VeryLazy" },

	-- Telescope file navigation with "<leader>ff".
	{
		"nvim-telescope/telescope.nvim",
		version = "0.1.4",
		dependencies = { "nvim-lua/plenary.nvim" },
		event = "VeryLazy",
	},

	-- Utility for line and block comments.
	-- * Add line comment with "<leader>lc".
	-- * Add block comment with "<leader>bc".
	{
		"numToStr/Comment.nvim",
		config = function()
			require("Comment").setup()
		end,
		event = "VeryLazy",
	},

	-- Highlighting for TODO/NOTE/HACK/BUG comments.
	{ "folke/todo-comments.nvim", event = "VeryLazy", dependencies = { { "nvim-lua/plenary.nvim", optional = true } } },

	-- -------------------------------------------------------------------------
	-- LSP support, code parsing, linting, and formatting
	-- -------------------------------------------------------------------------
	{
		"L3MON4D3/LuaSnip",
		run = "make install_jsregexp",
	},

	-- LSP support.
	{
		"VonHeikemen/lsp-zero.nvim",
		branch = "v2.x",
		dependencies = {
			"neovim/nvim-lspconfig",
			{
				"williamboman/mason.nvim",
				build = function()
					pcall(vim.cmd, "MasonUpdate")
				end,
			},
			"williamboman/mason-lspconfig.nvim",
			"L3MON4D3/LuaSnip",
			"hrsh7th/nvim-cmp",
			"hrsh7th/cmp-nvim-lsp",
			"hrsh7th/cmp-path",
		},
		event = "VeryLazy",
	},

	-- Code parser.
	{
		"nvim-treesitter/nvim-treesitter",
		event = "VeryLazy",
		build = ":TSUpdate",
	},

	-- Automatic code formatting.
	{ "stevearc/conform.nvim", event = "VeryLazy" },

	-- View errors and warnings from the LSP in a separate buffer with "<leader>tt".
	{
		"folke/trouble.nvim",
		config = function()
			require("trouble").setup({
				height = 3,
				auto_open = false,
				auto_close = true,
				auto_preview = false,
			})
		end,
	},

	-- -------------------------------------------------------------------------
	-- Language specific plugins
	-- -------------------------------------------------------------------------

	{ "lervag/vimtex", event = "VeryLazy", ft = "tex" },
	{ "tikhomirov/vim-glsl", event = "VeryLazy", ft = "glsl" },
})
