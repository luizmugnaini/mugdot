local python_path_proc = io.popen('python -c "import sys;print(sys.executable)"')
if python_path_proc ~= nil then
	local python_path = python_path_proc:read("l")
	if python_path ~= nil and python_path ~= "" then
		vim.g.python3_host_prog = python_path
	end
	python_path_proc:close()
end

vim.g.mapleader = " "

local opt = vim.opt

opt.guicursor = ""

opt.number = true
opt.relativenumber = true

opt.textwidth = 100
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
opt.ignorecase = true
opt.smartcase = true

opt.termguicolors = true

opt.scrolloff = 4 -- Minimal number of lines below cursor line
opt.signcolumn = "yes"
opt.isfname:append("@-@")

opt.spelllang = { "en" }
opt.spell = true

opt.updatetime = 50
