let g:space_vim_italic = 0
let g:space_vim_transp_bg = 1
colorscheme space_vim_theme

" Alacritty color suport
if exists('+termguicolors')
  let &t_8f="\<Esc>[38;2;%lu;%lu;%lum"
  let &t_8b="\<Esc>[48;2;%lu;%lu;%lum"
  set termguicolors
endif

" Grey comments
hi Comment guifg=#5C6370 ctermfg=59
