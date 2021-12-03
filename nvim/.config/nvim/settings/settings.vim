" Python provider
let g:python_host_prog = '/usr/bin/python2'
let g:python3_host_skip_check=1
let g:python3_host_prog = '/usr/bin/python3'

nnoremap <SPACE> <Nop>
let mapleader="\<Space>"

" Change cwd
nnoremap <leader>cd :lcd %:p:h<CR>:pwd<CR>

" Quick save
nnoremap <leader>w :w<CR>

" Stop annoying highlighting
nnoremap <leader>hh :noh<CR>

" Cycling through buffers
nnoremap <silent> <C-h> :bp<CR>
nnoremap <silent> <C-l> :bn<CR>

" Strip trailing white-spaces
autocmd FileType c,cpp,java,python,rust,vim,lua,tex autocmd BufWritePre <buffer> %s/\s\+$//e

" If you want to have beam in insert mode, just comment this line out
set guicursor=i:block

set colorcolumn=80

syntax enable                           " Enables syntax highlighing
set lazyredraw                          " Don't redraw while executing macros (good performance config)
set hidden                              " Required to keep multiple buffers open multiple buffers
set history=500                         " Sets how many lines of history VIM has to remember
set autoread                            " Set to auto read when a file is changed from the outside
set wrap                                " Display long lines as just one line
set textwidth=80                        " The line will tend to be wrapped at 80 characters
set encoding=utf-8                      " The encoding displayed
set pumheight=10                        " Makes popup menu smaller
set fileencoding=utf-8                  " The encoding written to file
set ruler              			            " Show the cursor position all the time
set number                              " Line numbers
set relativenumber                      " Shows number relative to the line you are in
"set cursorline                          " Enable highlighting of the current line
set cmdheight=2                         " More space for displaying messages
set iskeyword+=-                      	" treat dash separated words as a word text object
set mouse=a                             " Enable your mouse
set splitbelow                          " Horizontal splits will automatically be below
set splitright                          " Vertical splits will automatically be to the right
set t_Co=256                            " Support 256 colors
set conceallevel=0                      " So that I can see `` in markdown files
set tabstop=2                           " Insert 2 spaces for a tab
set shiftwidth=2                        " Change the number of space characters inserted for indentation
set smarttab                            " Makes tabbing smarter will realize you have 2 vs 4
set expandtab                           " Converts tabs to spaces
set smartindent                         " Makes indenting smart
set autoindent                          " Good auto indent
set laststatus=0                        " Always display the status line
set showtabline=2                       " Always show tabs
set noshowmode                          " We don't need to see things like -- INSERT -- anymore
set nobackup                            " This is recommended by coc
set nowritebackup                       " This is recommended by coc
set noswapfile                          " Turn swaps off
set updatetime=300                      " Faster completion
set timeoutlen=500                      " By default timeoutlen is 1000 ms
set formatoptions-=cro                  " Stop newline continuation of comments
set smartcase                           " Smart searching on cases
set hlsearch                            " highlighted search as default
set scrolloff=1                         " show at least 1 lines above/below cursor

set undodir=~/.cache/nvim/.undovim
set undofile

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

" More on clipboard: requires xclip or tmux
"set clipboard+=unnamedplus              " Copy paste between everything else to vim
vnoremap <C-y> "+y
map <C-p> "+P
""""""""

au! BufWritePost $MYVIMRC source %      " auto source when writing to init.vm alternatively you can run :source $MYVIMRC

setlocal spell
set spelllang=en
inoremap <C-l> <c-g>u<Esc>[s1z=`]a<c-g>u

" You can't stop me
cmap w!! w !sudo tee %
