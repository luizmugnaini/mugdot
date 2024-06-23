-- -----------------------------------------------------------------------------
-- General settings
-- -----------------------------------------------------------------------------

vim.g.mug_enable_lsp = false
vim.g.mug_enable_treesitter = true

-- Use SPACE as the leader key.
vim.g.mapleader = " "

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

-- Visuals
vim.opt.guicursor = "" -- Use a block as the cursor.
vim.opt.showmode = false -- Don't show the current mode in the minibuffer.
vim.opt.number = false -- Don't show line numbers
vim.opt.termguicolors = true
vim.opt.signcolumn = "no"
vim.opt.showtabline = 0
vim.opt.statusline = " %f %h%m%r%=%-14.(%l,%c%)"

-- Indentation
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true
vim.opt.smartindent = true

-- Text rendering behaviour
vim.opt.wrap = false
vim.opt.scrolloff = 4 -- Minimal number of lines below cursor line

-- Visual mode
vim.opt.virtualedit = "block" -- Allow cursor to move where there is no text in visual block mode

-- Backup
vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.undodir = vim.fn.stdpath("data") .. "/undo"
vim.opt.undofile = true

-- Searching functionality
vim.opt.grepprg = "rg --vimgrep"
vim.opt.hlsearch = false
vim.opt.incsearch = true
vim.opt.smartcase = true
vim.opt.ignorecase = true

-- Spelling
vim.opt.spelllang = { "en" }
vim.opt.spell = false

vim.opt.updatetime = 50

-- -----------------------------------------------------------------------------
-- Auto-commands.
-- -----------------------------------------------------------------------------

local mug_group = vim.api.nvim_create_augroup("mug", { clear = true })

local c_like =
	{ "*.c", "*.h", "*.cc", ".cpp", ".hpp", "*.glsl", "*.vert", "*.tesc", "*.tese", "*.geom", "*.frag", "*.comp" }

function trim_whitespaces()
	local view = vim.fn.winsaveview()
	vim.api.nvim_exec([[keepjumps keeppatterns silent! %s/\s\+$//e]], { output = false })
	vim.fn.winrestview(view)
end

function fmt_buf(formatter)
	local view = vim.fn.winsaveview()
	vim.api.nvim_exec([[keepjumps keeppatterns silent %!]] .. formatter, { output = false })
	vim.fn.winrestview(view)
end

vim.api.nvim_create_autocmd("BufEnter", {
	desc = "Detect GLSL files",
	group = mug_group,
	pattern = { "*.glsl", "*.vert", "*.tesc", "*.tese", "*.geom", "*.frag", "*.comp" },
	command = "set filetype=glsl",
})

vim.api.nvim_create_autocmd("BufWritePre", {
	desc = "Remove trailing whitespaces",
	group = mug_group,
	pattern = "*",
	callback = function()
		trim_whitespaces()
	end,
})

vim.api.nvim_create_autocmd("BufWritePre", {
	desc = "Format C-like files",
	group = mug_group,
	pattern = c_like,
	callback = function()
		fmt_buf("clang-format")
	end,
})

vim.api.nvim_create_autocmd("BufWritePre", {
	desc = "Format Python files",
	group = mug_group,
	pattern = "*.py",
	callback = function()
		fmt_buf("black -q -")
	end,
})

vim.api.nvim_create_autocmd("BufWritePre", {
	desc = "Format Lua files",
	group = mug_group,
	pattern = "*.lua",
	callback = function()
		fmt_buf("stylua -")
	end,
})

-- -----------------------------------------------------------------------------
-- Keybindings
-- -----------------------------------------------------------------------------

local all_modes = { "n", "i", "x", "v", "s", "c", "o", "l", "t" }
local nins_modes = { "n", "x", "o" }

vim.keymap.set(all_modes, "<C-k>", "<Esc>", { desc = "Escape to normal mode", silent = true })
vim.keymap.set("v", "<C-y>", '"+y', { desc = "Copy to external clipboard" })
vim.keymap.set(nins_modes, "<leader>w", vim.cmd.write, { desc = "[W]rite file" })
vim.keymap.set(nins_modes, "<leader>qq", vim.cmd.quit, { desc = "Kill the current buffer" })
vim.keymap.set(nins_modes, "<leader>bf", vim.cmd.Ex, { desc = "Native file browsing" })

-- Window splits
vim.keymap.set(nins_modes, "<leader>ss", vim.cmd.vsplit, { desc = "Split Vertically", silent = true })
vim.keymap.set(nins_modes, "<leader>sh", vim.cmd.split, { desc = "Split horizontally", silent = true })

-- Window movement
vim.keymap.set(nins_modes, "<leader>o", function()
	vim.cmd.wincmd("w")
end, { desc = "Move to next window", silent = true })

-- Tags
vim.keymap.set(nins_modes, "gt", "<C-]>", { desc = "Go to definition using ctags" })
vim.keymap.set(nins_modes, "gb", vim.cmd.pop, { desc = "Go to [P]revious [T]ag" })
vim.keymap.set(nins_modes, "<leader>ut", function()
	vim.cmd("!ctags -R")
end, { desc = "Update the tag cache" })

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
	{ "nvim-treesitter/nvim-treesitter", build = ":TSUpdate" },

	-- Theme building helper.
	{ "tjdevries/colorbuddy.nvim" },

	-- -------------------------------------------------------------------------
	-- Utilities for better development.
	-- -------------------------------------------------------------------------

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
