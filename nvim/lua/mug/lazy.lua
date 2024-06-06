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
	-- -------------------------------------------------------------------------
	-- Visuals.
	-- -------------------------------------------------------------------------

	-- Code parser.
	{ "nvim-treesitter/nvim-treesitter", build = ":TSUpdate" },

	-- Themes
	{ "miikanissi/modus-themes.nvim" },
	{ "rebelot/kanagawa.nvim" },

	-- -------------------------------------------------------------------------
	-- Utilities for better development.
	-- -------------------------------------------------------------------------

	{ "akinsho/toggleterm.nvim", version = "*", event = "VeryLazy" },

	-- Telescope file navigation with "<leader>ff".
	{
		"nvim-telescope/telescope.nvim",
		tag = "0.1.6",
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

	-- Automatic code formatting.
	{ "stevearc/conform.nvim", event = "VeryLazy" },

	-- View errors and warnings from the LSP in a separate buffer with "<leader>tt".
	{ "folke/trouble.nvim", event = "VeryLazy" },

	-- -------------------------------------------------------------------------
	-- LSP support
	-- -------------------------------------------------------------------------

	-- LSP management within Neovim.
	{
		"williamboman/mason.nvim",
		build = function()
			pcall(vim.cmd, "MasonUpdate")
		end,
		event = "VeryLazy",
	},
	{ "williamboman/mason-lspconfig.nvim", event = "VeryLazy" },

	-- LSP support.
	{ "VonHeikemen/lsp-zero.nvim", branch = "v3.x", event = "VeryLazy" },
	{ "neovim/nvim-lspconfig", event = "VeryLazy" },

	-- -------------------------------------------------------------------------
	-- Snippets and completion support
	-- -------------------------------------------------------------------------

	-- Snippet engine
	{
		"L3MON4D3/LuaSnip",
		build = "make install_jsregexp",
	},

	-- Snippets and completion
	{
		"hrsh7th/nvim-cmp",
		dependencies = {
			"hrsh7th/cmp-path",
			"hrsh7th/cmp-nvim-lsp",
			"saadparwaiz1/cmp_luasnip",
		},
	},
})
