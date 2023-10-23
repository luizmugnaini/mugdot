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
	{
		"nvim-telescope/telescope.nvim",
		version = "0.1.4",
		dependencies = {
			"nvim-lua/plenary.nvim",
		},
	},

	{ "theprimeagen/harpoon", dependencies = { "nvim-telescope/telescope.nvim" } },

	"nvim-tree/nvim-web-devicons",
	"folke/tokyonight.nvim",
	{ "nvim-lualine/lualine.nvim", dependencies = { { "nvim-tree/nvim-web-devicons", optional = true } } },
	"xiyaowong/transparent.nvim",

	"mbbill/undotree",

	{
		"numToStr/Comment.nvim",
		config = function()
			require("Comment").setup()
		end,
	},

	{
		"folke/todo-comments.nvim",
		dependencies = { { "nvim-lua/plenary.nvim", optional = true } },
		config = function()
			require("todo-comments").setup()
		end,
	},

	{
		"folke/trouble.nvim",
		dependencies = { { "nvim-tree/nvim-web-devicons", optional = true } },
		config = function()
			require("trouble").setup({
				height = 5,
				auto_close = true,
			})
		end,
	},

	{
		"nvim-treesitter/nvim-treesitter",
		build = ":TSUpdate",
	},

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
			"L3MON4D3/LuaSnip",
		},
	},

	{
		"jose-elias-alvarez/null-ls.nvim",
		dependencies = { "nvim-lua/plenary.nvim" },
	},

	{ "rust-lang/rust.vim", ft = "rust" },
	{ "tikhomirov/vim-glsl", ft = "glsl" },
	{ "lervag/vimtex", ft = "tex" },
})
