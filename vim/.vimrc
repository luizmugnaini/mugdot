" Use space as the leader key.
nnoremap <SPACE> <Nop>
let mapleader="\<Space>"

" Other alternatives for using escape
nnoremap <C-k> <Esc>
inoremap <C-k> <Esc>
vnoremap <C-k> <Esc>
snoremap <C-k> <Esc>
xnoremap <C-k> <Esc>
cnoremap <C-k> <C-c>
onoremap <C-k> <Esc>
lnoremap <C-k> <Esc>
tnoremap <C-k> <Esc>
" Create splits
nnoremap <leader>s :vsplit<CR>
nnoremap <leader>h :split<CR>
" Switch between splits
nnoremap <leader>o :wincmd w<CR>
" Quick save
nnoremap <leader>w :w<CR>
" Browse files
nnoremap <leader>e :Ex<CR>
" Tag searching
nnoremap gd <C-]>
nnoremap gt :tselect<CR>
nnoremap gp :pop<CR>
nnoremap <leader>ut :!ctags -o .tags --languages=c,c++ --kinds-all=* --extras=* --fields=NPESZaimnorts --exclude=.git --exclude=build --recurse<CR>

" Colorscheme
syntax enable
if has('termguicolors')
  set termguicolors
endif
colorscheme mug

" Visuals
set guicursor=i:block
set t_Co=256
set belloff=all
set conceallevel=0
set lazyredraw

" Utilities
set virtualedit=block
set clipboard=unnamedplus
set smartcase
set tags=.tags

" Buffers and stuff
set hidden               " Required to keep multiple buffers open multiple buffers
set history=500          " Sets how many lines of history VIM has to remember
set autoread             " Set to auto read when a file is changed from the outside
set mouse=a              " Enable your mouse
set splitbelow
set splitright

" Spacings
set tabstop=4            " Insert 4 spaces for a tab
set shiftwidth=4         " Change the number of space characters inserted for indentation
set smarttab             " Makes tabbing smarter will realize you have 2 vs 4
set expandtab            " Converts tabs to spaces
set smartindent          " Makes indenting smart
set autoindent           " Good auto indent

" Status line and mini-buffer
set laststatus=0
set noshowmode
set cmdheight=1
set scrolloff=4
set pumheight=10

set iskeyword+=-         " treat dash separated words as a word text object
set encoding=utf-8       " The encoding displayed
set fileencoding=utf-8   " The encoding written to file

" Littering with backups
set noswapfile
set nobackup
set nowritebackup

" Timings
set updatetime=300
set timeoutlen=500
