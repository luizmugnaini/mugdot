return require("packer").startup(function(use)
	-- Packer itself
	use("wbthomason/packer.nvim")

	-- Keybinding lookup
	use({
		"folke/which-key.nvim",
		config = function()
			vim.o.timeout = true
			vim.o.timeoutlen = 300
			require("which-key").setup()
		end,
	})

	-- File browsing
	use("theprimeagen/harpoon")
	use({
		"nvim-telescope/telescope.nvim",
		tag = "0.1.1",
		requires = { { "nvim-lua/plenary.nvim" } },
	})

	-- Theme
	use("folke/tokyonight.nvim")

	-- Line mode
	use({
		"nvim-lualine/lualine.nvim",
		requires = { "kyazdani42/nvim-web-devicons", opt = true },
	})

	-- Transparency
	use("xiyaowong/transparent.nvim")

	-- Undo history
	use("mbbill/undotree")

	-- Git integration
	use("tpope/vim-fugitive")

	-- Commenting
	use({
		"numToStr/Comment.nvim",
		config = function()
			require("Comment").setup()
		end,
	})

	-- Trimming whitespaces
	use({
		"cappyzawa/trim.nvim",
		config = function()
			require("trim").setup({})
		end,
	})

	-- Dealing with todos
	use({
		"folke/todo-comments.nvim",
		requires = { "nvim-lua/plenary.nvim", opt = true },
		config = function()
			require("todo-comments").setup()
		end,
	})

	-- Diagnostics list
	use("nvim-tree/nvim-web-devicons")
	use({
		"folke/trouble.nvim",
		requires = { "nvim-tree/nvim-web-devicons", opt = true },
		config = function()
			require("trouble").setup({
				height = 5,
				auto_close = true,
			})
		end,
	})

	-- Treesitter
	use({ "nvim-treesitter/nvim-treesitter", run = ":TSUpdate" })

	-- LSP integration
	use({
		"VonHeikemen/lsp-zero.nvim",
		branch = "v2.x",
		requires = {
			{ "neovim/nvim-lspconfig" },
			{
				"williamboman/mason.nvim",
				run = function()
					pcall(vim.cmd, "MasonUpdate")
				end,
			},
			{ "williamboman/mason-lspconfig.nvim" },
			{ "hrsh7th/nvim-cmp" },
			{ "hrsh7th/cmp-nvim-lsp" },
			{ "L3MON4D3/LuaSnip" },
		},
	})

	use({
		"jose-elias-alvarez/null-ls.nvim",
		requires = { "nvim-lua/plenary.nvim" },
	})

	-- Rust
	use("rust-lang/rust.vim")

	-- Graphics
	use("DingDean/wgsl.vim")
	use("tikhomirov/vim-glsl")

	-- Latex stuff
	use("lervag/vimtex")
end)
