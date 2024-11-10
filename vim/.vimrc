" Use space as the leader key.
nnoremap <Space> <Nop>
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

" Auto-recenter search results
nnoremap n nzz
nnoremap N Nzz

nnoremap <leader>s :vsplit<CR>
nnoremap <leader>h :split<CR>
nnoremap <leader>o :wincmd w<CR>

nnoremap <leader>q :q<CR>
nnoremap <leader>w :w<CR>
nnoremap <leader>e :Ex<CR>

nnoremap gd <C-]>
nnoremap gt :tselect<CR>
nnoremap gp :pop<CR>
if has("win32") || has("win64")
    nnoremap <leader>ut :!~/scoop/apps/universal-ctags/current/ctags.exe -o .tags --languages=c,c++ --fields=NPESZaimnorts --exclude=.git --exclude=build --recurse<CR><CR>
elseif has("unix")
    nnoremap <leader>ut :!/usr/bin/ctags -o .tags --languages=c,c++ --fields=NPESZaimnorts --exclude=.git --exclude=build --recurse<CR><CR>
endif

" Query highlight group
nnoremap <leader>qhg :echo synIDattr(synID(line('.'), col('.'), 1), 'name')<CR>

" Colorscheme
" colorscheme mug

" Settings ---------------------------------------------------------------------

" Visuals
set guicursor=i:block      " Block rocks in every mode.
set guicursor+=a:blinkon0  " Disable blinking.
set belloff=all
set conceallevel=0

set noshowmode           " I'm not stupid
set laststatus=2         " Always show the statusline
set statusline=\ %f\ %h%m%r%=%-14.(%l,%c%)

set nowrap

" Utilities
set virtualedit=block
set clipboard=unnamedplus    " Jeez... just use the system clipboard.
set smartcase
set tags=.tags

" Buffers and stuff
set hidden               " Required to keep multiple buffers open
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
set timeout
set timeoutlen=500

" Formatting
set formatoptions-=t     " Disable automatic text wrapping while typing

" Utilities --------------------------------------------------------------------

" Code divisor header in C and C++ files.
autocmd FileType c,cpp abbr cmt // -----------------------------------------------------------------------------<CR> <ESC>a<CR>-----------------------------------------------------------------------------<Esc>

" Plugins
call plug#begin()
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'vim-autoformat/vim-autoformat'
call plug#end()

augroup autoformat_on_save
    autocmd!
    autocmd BufWrite *.c,*.cc,*.cpp,*.h,*.hh,*.hpp,*.lua :Autoformat
augroup END

nnoremap <leader>ff :FZF<CR>
nnoremap <leader>bb :Buffers<CR>
nnoremap <leader>fz :Rg<Space>

