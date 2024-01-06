vim.g.python3_host_prog = "~/.pyenv/versions/3.11.5/bin/python3"
vim.g.mapleader = " "

local opt = vim.opt

opt.guicursor = ""

opt.number = true
opt.relativenumber = true

opt.textwidth = 130
opt.tabstop = 4
opt.shiftwidth = 4
opt.expandtab = true
opt.smartindent = true

opt.virtualedit = "block" -- Allow cursor to move where there is no text in visual block mode

opt.wrap = false

opt.swapfile = false
opt.backup = false
opt.undodir = os.getenv("HOME") .. "/.cache/nvim/undodir"
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
