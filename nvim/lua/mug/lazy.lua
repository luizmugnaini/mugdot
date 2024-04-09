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
	-- Theme
	{ "sainnhe/gruvbox-material" },

	-- -------------------------------------------------------------------------
	-- Utilities for better development.
	-- * Terminal mode enhancement.
	-- * File navigation with telescope.
	-- * Comment utilities.
	-- * Snippets + completions.
	-- -------------------------------------------------------------------------

	{ "ggandor/leap.nvim", dependencies = { "tpope/vim-repeat" } },

	{ "akinsho/toggleterm.nvim", event = "VeryLazy", version = "*", opts = { open_mapping = [[<c-t>]] } },

	-- Telescope file navigation with "<leader>ff".
	{
		"nvim-telescope/telescope.nvim",
		event = "VeryLazy",
		version = "0.1.4",
		dependencies = { "nvim-lua/plenary.nvim" },
	},

	-- Utility for line and block comments.
	-- * Add line comment with "<leader>lc".
	-- * Add block comment with "<leader>bc".
	{
		"numToStr/Comment.nvim",
		event = "VeryLazy",
		config = function()
			require("Comment").setup()
		end,
	},

	{ "L3MON4D3/LuaSnip", run = "make install_jsregexp" },
	{
		"hrsh7th/nvim-cmp",
		event = "VeryLazy",
		dependencies = {
			"hrsh7th/cmp-path",
		},
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
			"hrsh7th/nvim-cmp",
			"hrsh7th/cmp-nvim-lsp",
			"hrsh7th/cmp-path",
			"L3MON4D3/LuaSnip",
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
		event = "VeryLazy",
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

	{ "tikhomirov/vim-glsl", event = "VeryLazy", ft = "glsl" },
	{ "lervag/vimtex", event = "VeryLazy" },
})
