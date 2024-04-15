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

	-- * Forward search: "<leader>l"
	-- * Backwards search: "<leader>L"
	{ "ggandor/leap.nvim", dependencies = { "tpope/vim-repeat" } },

	{ "akinsho/toggleterm.nvim", version = "*", event = "VeryLazy" },

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

	-- Snippets and completion
	{ "L3MON4D3/LuaSnip", build = "make install_jsregexp", event = "VeryLazy" },
	{
		"hrsh7th/nvim-cmp",
		dependencies = {
			"hrsh7th/cmp-path",
		},
		event = "VeryLazy",
	},

	-- -------------------------------------------------------------------------
	-- LSP support, code parsing, linting, and formatting
	-- -------------------------------------------------------------------------

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
	{ "folke/trouble.nvim", event = "VeryLazy" },

	-- -------------------------------------------------------------------------
	-- Language specific plugins
	-- -------------------------------------------------------------------------

	{ "lervag/vimtex", event = "VeryLazy", ft = "tex" },
	{ "tikhomirov/vim-glsl", event = "VeryLazy", ft = "glsl" },
})
