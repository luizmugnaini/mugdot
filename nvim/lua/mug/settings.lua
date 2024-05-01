local python_path_proc = io.popen('python -c "import sys;print(sys.executable)"')
if python_path_proc ~= nil then
	local python_path = python_path_proc:read("l")
	if python_path ~= nil and python_path ~= "" then
		vim.g.python3_host_prog = python_path
	end
	python_path_proc:close()
end

vim.g.loaded_ruby_provider = 0
vim.g.loaded_node_provider = 0
vim.g.loaded_perl_provider = 0

vim.g.mapleader = " "

local opt = vim.opt

opt.guicursor = "" -- Use a block as the cursor.
opt.showmode = false -- Don't show the current mode in the minibuffer.

opt.number = false -- Don't show line numbers

opt.tabstop = 4
opt.shiftwidth = 4
opt.expandtab = true
opt.smartindent = true

opt.virtualedit = "block" -- Allow cursor to move where there is no text in visual block mode

opt.wrap = false

opt.swapfile = false
opt.backup = false
opt.undodir = vim.fn.stdpath("data") .. "/undo"
opt.undofile = true

opt.grepprg = "rg --vimgrep"
opt.hlsearch = false
opt.incsearch = true
opt.smartcase = true
opt.ignorecase = true

opt.termguicolors = true

opt.scrolloff = 4 -- Minimal number of lines below cursor line

opt.signcolumn = "yes" -- Always have space for the sign column (used by LSP)

opt.isfname:append("@-@")

opt.spelllang = { "en" }
opt.spell = true

opt.updatetime = 50
