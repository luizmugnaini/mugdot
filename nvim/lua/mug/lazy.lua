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
	{
		"sainnhe/gruvbox-material",
		config = function()
			vim.g.gruvbox_material_better_performance = 1
			vim.g.gruvbox_material_foreground = "material" -- choices are "material", "mix", "original"
			vim.g.gruvbox_material_background = "hard"
			vim.g.gruvbox_material_disable_italic_comment = 1
			vim.cmd.colorscheme("gruvbox-material")
		end,
	},

	-- NerdFont icons.
	{ "nvim-tree/nvim-web-devicons", event = "VeryLazy" },

	-- Status line.
	{
		"nvim-lualine/lualine.nvim",
		event = "VeryLazy",
		dependencies = { { "nvim-tree/nvim-web-devicons", optional = true } },
	},

	-- -------------------------------------------------------------------------
	-- Utilities for better development.
	-- * Terminal mode enhancement.
	-- * Undo-redo functionality enhancement.
	-- * File navigation with telescope.
	-- * Comment utilities.
	-- * TODO comment highlighting.
	-- * Writting snippets.
	-- -------------------------------------------------------------------------

	{ "akinsho/toggleterm.nvim", event = "VeryLazy", version = "*", opts = { open_mapping = [[<c-t>]] } },

	-- Utility for undoing actions and navigating file history.
	-- * Undo: "u" in normal mode.
	-- * Redo: "r" in normal mode.
	-- * Open history: "<leader>-u".
	{ "mbbill/undotree", event = "VeryLazy" },

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

	-- Highlighting for TODO/NOTE/HACK/BUG comments.
	{
		"folke/todo-comments.nvim",
		event = "VeryLazy",
		dependencies = { { "nvim-lua/plenary.nvim", optional = true } },
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
})
