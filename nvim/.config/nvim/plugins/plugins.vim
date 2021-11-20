"----------------------------------------------------------------
" Plugin manager - vim-plug
call plug#begin('~/.config/plugged-nvim')

" Snippets
Plug 'SirVer/ultisnips'
    let g:UltiSnipsExpandTrigger = '<tab>'
    let g:UltiSnipsJumpForwardTrigger = '<tab>'
    let g:UltiSnipsJumpBackwardTrigger = '<s-tab>'

" Tex support
Plug 'lervag/vimtex'
    let g:tex_flavor='latex'
    let g:vimtex_view_method='zathura'
    let g:vimtex_quickfix_mode=0
    let g:vimtex_compiler_progname = '/home/luiz/.local/bin/nvr'

Plug 'KeitaNakamura/tex-conceal.vim'
    set conceallevel=0
    let g:tex_conceal='abdmg'
    hi Conceal ctermbg=none

" Themes
Plug 'morhetz/gruvbox'
Plug 'liuchengxu/space-vim-theme'

Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

Plug 'lifepillar/vim-colortemplate'
Plug 'glepnir/dashboard-nvim'

" <leader> + cc comments out the marked lines
Plug 'preservim/nerdcommenter'

Plug 'ryanoasis/vim-devicons'

" Finders
Plug 'mcchrish/nnn.vim'

Plug 'nvim-lua/popup.nvim'
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'
Plug 'nvim-telescope/telescope-fzf-native.nvim', { 'do': 'make' }

Plug 'airblade/vim-rooter'

" Git integration
Plug 'tpope/vim-fugitive'

" Language stuff
Plug 'nvim-treesitter/nvim-treesitter', { 'do': ':TSUpdate' }
Plug 'neovim/nvim-lspconfig'

Plug 'neovimhaskell/haskell-vim'
Plug 'rust-lang/rust.vim'
Plug 'iamcco/markdown-preview.nvim', { 'do': 'cd app && yarn install'  }

" Completions
Plug 'hrsh7th/cmp-nvim-lsp'
Plug 'hrsh7th/cmp-buffer'
Plug 'hrsh7th/nvim-cmp'
Plug 'quangnguyen30192/cmp-nvim-ultisnips'
Plug 'hrsh7th/cmp-path'

call plug#end()
