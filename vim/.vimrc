" Use space as the leader key.
nnoremap <SPACE> <Nop>
let mapleader="\<Space>"

nnoremap <C-k> <Esc>
inoremap <C-k> <Esc>
vnoremap <C-k> <Esc>
snoremap <C-k> <Esc>
xnoremap <C-k> <Esc>
cnoremap <C-k> <C-c>
onoremap <C-k> <Esc>
lnoremap <C-k> <Esc>
tnoremap <C-k> <Esc>

nnoremap <leader>s :vsplit<CR>
nnoremap <leader>h :split<CR>
nnoremap <leader>o :wincmd w<CR>

nnoremap <leader>q :wq<CR>
nnoremap <leader>w :w<CR>
nnoremap <leader>e :Ex<CR>

nnoremap gd <C-]>
nnoremap gt :tselect<CR>
nnoremap gp :pop<CR>
if has("win32") || has("win64")
    nnoremap <leader>ut :!~/scoop/apps/universal-ctags/current/ctags.exe -o .tags --languages=c,c++ --kinds-all=* --extras=* --fields=NPESZaimnorts --exclude=.git --exclude=build --recurse<CR>
elseif has("unix")
    nnoremap <leader>ut :!/user/bin/ctags -o .tags --languages=c,c++ --kinds-all=* --extras=* --fields=NPESZaimnorts --exclude=.git --exclude=build --recurse<CR>
endif

" Query highlight group
nnoremap <leader>qhg :echo synIDattr(synID(line('.'), col('.'), 1), 'name')<CR>

" Colorscheme
colorscheme mug

" Visuals
set guicursor=i:block      " Block rocks in every mode.
set guicursor+=a:blinkon0  " Disable blinking.
set belloff=all
set conceallevel=0
set lazyredraw

" Utilities
set virtualedit=block
set clipboard^=unnamed    " Jeez... just use the system clipboard.
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

set noshowmode           " I'm not stupid
set laststatus=2         " Always show the statusline
set statusline=\ %f\ %h%m%r%=%-14.(%l,%c%)

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

call plug#begin()

Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

call plug#end()

nnoremap <leader>ff :FZF<CR>
