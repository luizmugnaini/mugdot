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
	-- User interface stuff: themes, icons, transparency, and status line.
	-- -------------------------------------------------------------------------

	-- Themes
	{ "sainnhe/gruvbox-material", event = "VeryLazy" },

	-- NerdFont icons.
	"nvim-tree/nvim-web-devicons",

	-- Status line.
	{ "nvim-lualine/lualine.nvim", dependencies = { { "nvim-tree/nvim-web-devicons", optional = true } } },

	-- -------------------------------------------------------------------------
	-- Utilities for better development.
	-- * Undo-redo functionality enhancement.
	-- * File navigation with telescope and harpoon.
	-- * Comment utilities.
	-- * TODO comment highlighting.
	-- * Writting snippets.
	-- -------------------------------------------------------------------------

	-- Utility for undoing actions and navigating file history.
	-- * Undo: "u" in normal mode.
	-- * Redo: "r" in normal mode.
	-- * Open history: "<leader>-u".
	"mbbill/undotree",

	-- Telescope file navigation with "<leader>ff".
	{
		"nvim-telescope/telescope.nvim",
		version = "0.1.4",
		dependencies = { "nvim-lua/plenary.nvim" },
	},

	-- Utility for line and block comments.
	-- * Add line comment with "<leader>lc".
	-- * Add block comment with "<leader>bc".
	{
		"numToStr/Comment.nvim",
		config = function()
			require("Comment").setup()
		end,
	},

	-- Highlighting for TODO/NOTE/HACK/BUG comments.
	{ "folke/todo-comments.nvim", dependencies = { { "nvim-lua/plenary.nvim", optional = true } } },

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
	},

	-- Code parser.
	{ "nvim-treesitter/nvim-treesitter", build = ":TSUpdate" },

	-- Automatic code formatting.
	"stevearc/conform.nvim",

	-- View errors and warnings from the LSP in a separate buffer with "<leader>tt".
	{
		"folke/trouble.nvim",
		dependencies = { { "nvim-tree/nvim-web-devicons", optional = true } },
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
	{
		"iamcco/markdown-preview.nvim",
		cmd = { "MarkdownPreviewToggle" },
		build = function()
			vim.fn["mkdp#util#install"]()
		end,
		event = "VeryLazy",
		ft = { "markdown" },
	},

	-- Latex stuff
	-- { "lervag/vimtex", event = "VeryLazy", ft = "tex" },
	-- {
	-- 	"L3MON4D3/LuaSnip",
	-- 	dependencies = { "hrsh7th/nvim-cmp", "saadparwaiz1/cmp_luasnip" },
	-- 	event = "VeryLazy",
	-- 	ft = { "tex" },
	-- },
})
