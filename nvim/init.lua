-- =============================================================================
-- My custom NeoVim experience.
--
-- Author: Luiz G. Mugnaini A. <luizmugnaini@gmail.com>
-- =============================================================================

function tern(cond, opt_true, opt_false)
	if cond then
		return opt_true
	else
		return opt_false
	end
end

vim.g.mug_os_windows = package.config:sub(1, 1) == "\\"
vim.g.mug_home = tern(vim.g.mug_os_windows, os.getenv("USERPROFILE"), os.getenv("HOME"))

-- -----------------------------------------------------------------------------
-- General settings
-- -----------------------------------------------------------------------------

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
vim.opt.showmode = false -- Don't show the current mode in the minibuffer
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

-- Tags
vim.opt.tags = "tags"

-- Misc
vim.opt.encoding = "utf8"
vim.opt.clipboard = "unnamedplus" -- Copy to and from vim using the system clipboard register

-- -----------------------------------------------------------------------------
-- Auto-commands.
-- -----------------------------------------------------------------------------

local mug_group = vim.api.nvim_create_augroup("mug", { clear = true })

local c_like =
	{ "*.c", "*.h", "*.cc", "*.cpp", "*.hpp", "*.glsl", "*.vert", "*.tesc", "*.tese", "*.geom", "*.frag", "*.comp" }

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

-- vim.api.nvim_create_autocmd("BufWritePre", {
-- 	desc = "Remove trailing whitespaces",
-- 	group = mug_group,
-- 	pattern = "*",
-- 	callback = function()
-- 		trim_whitespaces()
-- 	end,
-- })

-- vim.api.nvim_create_autocmd("BufWritePre", {
-- 	desc = "Format C-like files",
-- 	group = mug_group,
-- 	pattern = c_like,
-- 	callback = function()
-- 		fmt_buf("clang-format")
-- 	end,
-- })
--
-- vim.api.nvim_create_autocmd("BufWritePre", {
-- 	desc = "Format Python files",
-- 	group = mug_group,
-- 	pattern = "*.py",
-- 	callback = function()
-- 		fmt_buf("black -q -")
-- 	end,
-- })
--
-- vim.api.nvim_create_autocmd("BufWritePre", {
-- 	desc = "Format Lua files",
-- 	group = mug_group,
-- 	pattern = "*.lua",
-- 	callback = function()
-- 		fmt_buf("stylua -")
-- 	end,
-- })

-- -----------------------------------------------------------------------------
-- Keybindings
-- -----------------------------------------------------------------------------

local all_modes = { "n", "i", "x", "v", "s", "c", "o", "l", "t" }
local nins_modes = { "n", "x", "o" }

vim.keymap.set(all_modes, "<C-k>", "<Esc>", { desc = "Escape to normal mode", silent = true })
vim.keymap.set(nins_modes, "<leader>w", vim.cmd.write, { desc = "[W]rite file" })
vim.keymap.set(nins_modes, "<leader>q", vim.cmd.quit, { desc = "Kill the current buffer" })
vim.keymap.set(nins_modes, "<leader>e", vim.cmd.Ex, { desc = "Explore files" })

-- Window splits
vim.keymap.set(nins_modes, "<leader>s", vim.cmd.vsplit, { desc = "Split Vertically", silent = true })
vim.keymap.set(nins_modes, "<leader>h", vim.cmd.split, { desc = "Split horizontally", silent = true })

-- Window movement
vim.keymap.set(nins_modes, "<leader>o", function()
	vim.cmd.wincmd("w")
end, { desc = "Move to next window", silent = true })

-- Tags
local ctags_exe =
	tern(vim.g.mug_os_windows, vim.g.mug_home .. "/scoop/apps/universal-ctags/current/ctags.exe", "/usr/bin/ctags")
local ctags_args =
	"-o tags --languages=c,c++ --kinds-all=* --extras=* --fields=NPESZaimnorts --exclude=.git --exclude=build --recurse"
vim.keymap.set(nins_modes, "gd", "<C-]>", { desc = "Go to definition" })
vim.keymap.set(nins_modes, "gt", vim.cmd.tselect, { desc = "Get all tags under this identifier" })
vim.keymap.set(nins_modes, "gb", vim.cmd.pop, { desc = "Go to [P]revious [T]ag" })
vim.keymap.set(nins_modes, "<leader>ut", function()
	vim.cmd("!" .. ctags_exe .. " " .. ctags_args)
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
	defaults = { lazy = true },

	-- -------------------------------------------------------------------------
	-- Visuals.
	-- -------------------------------------------------------------------------

	-- Code parser.
	{
		"nvim-treesitter/nvim-treesitter",
		enabled = true,
		build = ":TSUpdate",
		config = function()
			require("nvim-treesitter.configs").setup({
				ensure_installed = { "c", "cpp", "lua" },
				indent = { enable = { "python" } },
				sync_install = false,
				auto_install = false,
				highlight = {
					enable = true,
					disable = { "latex" },
					additional_vim_regex_highlighting = false,
				},
			})
		end,
	},

	-- Custom colorscheme.
	{
		"tjdevries/colorbuddy.nvim",
		init = function()
			vim.cmd.colorscheme("muggy")
		end,
	},

	-- -------------------------------------------------------------------------
	-- Utilities for better development.
	-- -------------------------------------------------------------------------

	-- Telescope file navigation with "<leader>ff".
	{
		"nvim-telescope/telescope.nvim",
		tag = "0.1.6",
		dependencies = { "nvim-lua/plenary.nvim" },
		event = "VeryLazy",
		config = function()
			local builtin = require("telescope.builtin")

			vim.keymap.set("n", "<leader>ff", function()
				builtin.find_files({
					hidden = true,
					file_ignore_patterns = { ".git" },
				})
			end, { desc = "[F]ind [F]ile" })
			vim.keymap.set("n", "<leader>bb", builtin.buffers, { desc = "[F]ind [B]uffers" })
			vim.keymap.set("n", "<leader>fz", function()
				builtin.grep_string({ search = vim.fn.input("> ") })
			end, { desc = "Fuzzy finder" })
		end,
	},

	-- Utility for line and block comments.
	{
		"numToStr/Comment.nvim",
		event = "VeryLazy",
		config = function()
			require("Comment").setup({
				toggler = {
					line = "<A-;>",
				},
				opleader = {
					line = "<A-;>",
				},
			})
		end,
	},

	-- Automatic formatting at save.
	{
		"nvimdev/guard.nvim",
		dependencies = { "nvimdev/guard-collection" },
		event = "VeryLazy",
		ft = { "c", "cpp", "glsl", "lua", "python" },
		config = function()
			local ft = require("guard.filetype")

			ft("c,cpp,glsl"):fmt("clang-format")
			ft("lua"):fmt("stylua")
			ft("python"):fmt("black")

			require("guard").setup({
				fmt_on_save = true,
			})
		end,
	},

	-- -------------------------------------------------------------------------
	-- Snippets
	-- -------------------------------------------------------------------------

	{
		"L3MON4D3/LuaSnip",
		build = "make install_jsregexp",
		ft = { "tex", "c", "cpp" },
		config = function()
			local ls = require("luasnip")

			require("luasnip.loaders.from_lua").lazy_load({ paths = vim.g.mug_home .. "/.config/mugdot/nvim/snippets/" })

			ls.config.setup({
				-- Enable autotriggered snippets
				enable_autosnippets = true,
			})

			vim.keymap.set({ "i" }, "<C-e>", function()
				ls.expand()
			end, { silent = true, desc = "Expand snippet" })
		end,
	},

	-- -------------------------------------------------------------------------
	-- LSP support
	-- -------------------------------------------------------------------------

	{
		"VonHeikemen/lsp-zero.nvim",
		cond = (os.getenv("LSP") ~= nil),
		branch = "v3.x",
		-- event = "VeryLazy",
		dependencies = {
			"neovim/nvim-lspconfig",
			"saadparwaiz1/cmp_luasnip",
			"hrsh7th/cmp-buffer",
			"hrsh7th/cmp-nvim-lsp",
			"hrsh7th/nvim-cmp",
			"folke/trouble.nvim",
		},
		config = function()
			-- ----------------------------------------------------------------------------
			-- Completions
			-- ----------------------------------------------------------------------------

			local cmp = require("cmp")

			cmp.setup({
				snippet = {
					expand = function(args)
						vim.snippet.expand(args.body)
					end,
				},
				mapping = cmp.mapping.preset.insert({
					["<C-y>"] = cmp.mapping.confirm({ select = true }),
					["<C-j>"] = cmp.mapping.select_prev_item(),
					["<C-n>"] = cmp.mapping.select_next_item(),
				}),
				sources = {
					{ name = "luasnip" },
					{ name = "nvim_lsp" },
					{ name = "buffer" },
				},
			})

			-- ----------------------------------------------------------------------------
			-- LSP config
			-- ----------------------------------------------------------------------------

			local lsp = require("lsp-zero").preset({ name = "recommended" })
			local lspconfig = require("lspconfig")
			local cmp_capabilities = require("cmp_nvim_lsp").default_capabilities()

			lspconfig.clangd.setup({
				cmd = { "clangd", "--log=verbose", "--compile-commands-dir=./build" },
				filetypes = { "c", "cpp" },
				capabilities = cmp_capabilities,
				root_dir = function()
					lsp.dir.find_first({ ".git", ".clang-format", ".clangd", ".clang-tidy" })
				end,
			})
			lspconfig.pyright.setup({ capabilities = cmp_capabilities })

			lsp.extend_cmp()
			lsp.set_preferences({
				suggest_lsp_servers = false,
				sign_icons = { error = "E", warn = "W", hint = "H", info = "I" },
			})
			lsp.on_attach(function(client, bufnr)
				local opts = { buffer = bufnr, remap = false }
				vim.keymap.set("n", "gd", function()
					vim.lsp.buf.definition()
				end, opts)
				vim.keymap.set("n", "gr", function()
					vim.lsp.buf.references()
				end, opts)
				vim.keymap.set("n", "K", function()
					vim.lsp.buf.hover()
				end, opts)
				vim.keymap.set("n", "<leader>ca", function()
					vim.lsp.buf.code_action()
				end, opts)
				vim.keymap.set("n", "<leader>rn", function()
					vim.lsp.buf.rename()
				end, opts)
			end)

			lsp.setup()

			require("trouble").setup({
				height = 3,
				auto_open = false,
				auto_close = true,
				auto_preview = false,
				icons = false,
			})

			vim.keymap.set("n", "<leader>tt", function()
				require("trouble").open()
			end, { desc = "[T]trouble [T]oggle" })
		end,
	},
})
