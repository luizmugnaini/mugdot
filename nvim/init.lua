-- -----------------------------------------------------------------------------
-- General settings
-- -----------------------------------------------------------------------------

-- Find python and set its provider path.
local python_path_proc = io.popen('python -c "import sys;print(sys.executable)"')
if python_path_proc ~= nil then
	local python_path = python_path_proc:read("l")
	if python_path ~= nil and python_path ~= "" then
		vim.g.python3_host_prog = python_path
	end
	python_path_proc:close()
end

-- Disable useless language providers.
vim.g.loaded_ruby_provider = 0
vim.g.loaded_node_provider = 0
vim.g.loaded_perl_provider = 0

vim.g.mapleader = " " -- Use SPACE as the leader key.

local opt = vim.opt

opt.guicursor = "" -- Use a block as the cursor.
opt.showmode = false -- Don't show the current mode in the minibuffer.
opt.number = false -- Don't show line numbers

-- Indentation stuff
opt.tabstop = 4
opt.shiftwidth = 4
opt.expandtab = true
opt.smartindent = true

opt.wrap = false -- Text wrapping
opt.scrolloff = 4 -- Minimal number of lines below cursor line

opt.virtualedit = "block" -- Allow cursor to move where there is no text in visual block mode

-- Backup
opt.swapfile = false
opt.backup = false
opt.undodir = vim.fn.stdpath("data") .. "/undo"
opt.undofile = true

-- Searching functionality
opt.grepprg = "rg --vimgrep"
opt.hlsearch = false
opt.incsearch = true
opt.smartcase = true
opt.ignorecase = true

opt.termguicolors = true

opt.isfname:append("@-@")

opt.spelllang = { "en" }
opt.spell = true

opt.updatetime = 50

-- -----------------------------------------------------------------------------
-- Keybindings
-- -----------------------------------------------------------------------------

local all_modes = { "n", "i", "x", "v", "s", "c", "o", "l", "t" }
local nins_modes = { "n", "x", "o" } -- non-insert modes.

vim.keymap.set("v", "<C-y>", '"+y', { desc = "Copy to external clipboard" })
vim.keymap.set(all_modes, "<C-k>", "<Esc>", { desc = "Escape to normal mode", silent = true })
vim.keymap.set(nins_modes, "<leader>w", vim.cmd.write, { desc = "[W]rite file" })
vim.keymap.set(nins_modes, "<leader>qq", vim.cmd.quit, { desc = "Kill the current buffer" })
vim.keymap.set(nins_modes, "<leader>qa", vim.cmd.wqall, { desc = "Save and kill all buffers" })
vim.keymap.set(nins_modes, "<leader>qa", vim.cmd.wq, { desc = "Save and kill the current buffer" })
vim.keymap.set(nins_modes, "<leader>bf", vim.cmd.Ex, { desc = "Native file browsing" })
vim.keymap.set(nins_modes, "<leader>ss", vim.cmd.vsplit, { silent = true, desc = "Split Vertically" })
vim.keymap.set(nins_modes, "<leader>sh", vim.cmd.split, { silent = true, desc = "Split horizontally" })
vim.keymap.set(nins_modes, "<leader>o", vim.cmd("wincmd w"), { silent = true, desc = "Move to next window" })
vim.keymap.set(nins_modes, "<leader>gt", "<C-]>", { desc = "Go to definition using ctags" })
vim.keymap.set(nins_modes, "<leader>gb", vim.cmd.pop, { desc = "Go to [P]revious [T]ag" })
vim.keymap.set(nins_modes, "<leader>ut", vim.cmd("!ctags -R"), { desc = "Update the tag cache" })

-- -----------------------------------------------------------------------------
-- File type configuration.
-- -----------------------------------------------------------------------------

-- GLSL
vim.api.nvim_create_autocmd("BufEnter", {
	desc = "Detect GLSL files",
	pattern = { "*.glsl", "*.vert", "*.tesc", "*.tese", "*.geom", "*.frag", "*.comp" },
	command = "set filetype=glsl",
})

-- -----------------------------------------------------------------------------
-- Packages
-- -----------------------------------------------------------------------------

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
	-- { "nvim-treesitter/nvim-treesitter", build = ":TSUpdate" },

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
	{
		"numToStr/Comment.nvim",
		config = function()
			require("Comment").setup()
		end,
		event = "VeryLazy",
	},

	-- Automatic code formatting.
	{ "stevearc/conform.nvim", event = "VeryLazy" },

	-- -------------------------------------------------------------------------
	-- Snippets and completion support
	-- -------------------------------------------------------------------------

	{ "L3MON4D3/LuaSnip", build = "make install_jsregexp" },
	{ "hrsh7th/nvim-cmp", dependencies = { "saadparwaiz1/cmp_luasnip" } },

	-- -------------------------------------------------------------------------
	-- LSP support
	-- -------------------------------------------------------------------------

	-- LSP support.
	{ "VonHeikemen/lsp-zero.nvim", branch = "v3.x", event = "VeryLazy" },
	{ "hrsh7th/cmp-nvim-lsp" },
	{ "neovim/nvim-lspconfig", event = "VeryLazy" },
	-- View errors and warnings from the LSP in a separate buffer with "<leader>tt".
	{ "folke/trouble.nvim", event = "VeryLazy" },
})
