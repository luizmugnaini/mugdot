" My init.vim
"
" Luiz Mugnaini
""""""""""""""""""""""""""""""""""""""""""""""""""
" General settings
source $HOME/.config/nvim/settings/settings.vim

" Plugin management
source $HOME/.config/nvim/plugins/plugins.vim

" Themes
source $HOME/.config/nvim/themes/gruvbox.vim
source $HOME/.config/nvim/themes/airline.vim

" cwd
source $HOME/.config/nvim/plugins/rooter.vim

" Language support
source $HOME/.config/nvim/plugins/treesitter.vim
source $HOME/.config/nvim/plugins/lsp_conf.lua
source $HOME/.config/nvim/plugins/rust.vim
source $HOME/.config/nvim/plugins/haskell-vim.vim

" File searching
source $HOME/.config/nvim/plugins/telescope.vim
source $HOME/.config/nvim/plugins/rnvimr.vim

" Git integration
source $HOME/.config/nvim/plugins/fugitive.vim
