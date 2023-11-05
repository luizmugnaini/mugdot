local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"

if not vim.loop.fs_stat(lazypath) then
	vim.fn.system({
		"git",
		"clone",
		"--filter=blob:none",
		"https://github.com/folke/lazy.nvim.git",
		"--branch=stable", -- latest stable release
		lazypath,
	})
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
	-- UI components
	"folke/tokyonight.nvim",
	"sainnhe/gruvbox-material",
	"rebelot/kanagawa.nvim",

	{
		-- Icons for other plugins
		"nvim-tree/nvim-web-devicons",
	},

	{
		-- Transparency management
		"xiyaowong/transparent.nvim",
	},

	{
		-- UI line for nvim
		"nvim-lualine/lualine.nvim",
		dependencies = { { "nvim-tree/nvim-web-devicons", optional = true } },
	},

	{
		-- Tree structure for navigating files
		"nvim-neo-tree/neo-tree.nvim",

		branch = "v3.x",
		dependencies = {
			"nvim-lua/plenary.nvim",
			"nvim-tree/nvim-web-devicons", -- not strictly required, but recommended
			"MunifTanjim/nui.nvim",
		},
		keys = {
			{
				"<leader>fe",
				function()
					require("neo-tree.command").execute({ toggle = true, dir = vim.loop.cwd() })
				end,
				desc = "Explorer NeoTree (cwd)",
			},
		},
	},

	{
		-- Utility for undoing actions and navigating file history
		"mbbill/undotree",
	},

	{
		-- Better navigation
		"nvim-telescope/telescope.nvim",
		version = "0.1.4",
		dependencies = {
			"nvim-lua/plenary.nvim",
		},
	},

	{
		-- Better navigation
		"theprimeagen/harpoon",
		dependencies = { "nvim-telescope/telescope.nvim" },
	},

	{
		-- Utility for line and block comments
		"numToStr/Comment.nvim",
		config = function()
			require("Comment").setup()
		end,
	},

	{
		-- Highlighting for TODO/NOTE/HACK/BUG comments
		"folke/todo-comments.nvim",
		dependencies = { { "nvim-lua/plenary.nvim", optional = true } },
		config = function()
			require("todo-comments").setup()
		end,
	},

	{
		-- Snippets
		"L3MON4D3/LuaSnip",
		dependencies = {
			"hrsh7th/nvim-cmp",
			"saadparwaiz1/cmp_luasnip",
		},
	},

	{
		-- Code parsing
		"nvim-treesitter/nvim-treesitter",
		build = ":TSUpdate",
	},

	{
		-- LSP support
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

	{
		-- Code linting
		"mfussenegger/nvim-lint",
	},

	{
		-- Code formatting
		"stevearc/conform.nvim",
		opts = {},
	},

	{
		-- View errors and warnings from the LSP
		"folke/trouble.nvim",
		dependencies = { { "nvim-tree/nvim-web-devicons", optional = true } },
		config = function()
			require("trouble").setup({
				height = 5,
				auto_close = true,
			})
		end,
	},

	-- Language specific plugins
	{ "rust-lang/rust.vim", ft = "rust" },
	{ "tikhomirov/vim-glsl", ft = "glsl" },
	{ "lervag/vimtex", ft = "tex" },
})
