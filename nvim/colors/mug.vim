" Name: mug.vim
" Author: Luiz G. Mugnaini A. <luizmugnaini@gmail.com>
"
" Querying the highlight group of the word under the cursor:
" ```
" :echo synIDattr(synID(line('.'), col('.'), 1), 'name')
" ```

if exists('syntax on')
    syntax reset
endif

set background=dark
hi clear

let g:colors_name='mug'
set t_Co=256

" fg #ffffff
" bg #0d0e1c
hi Normal       guifg=#ffffff guibg=#0d0e1c
hi netrwList    guifg=#ffffff guibg=NONE
hi Delimeter    guifg=#ffffff guibg=NONE
hi Identifier   guifg=#ffffff guibg=NONE
hi Constant     guifg=#ffffff guibg=NONE
hi Number       guifg=#ffffff guibg=NONE
hi Float        guifg=#ffffff guibg=NONE
hi Operator     guifg=#ffffff guibg=NONE
hi luaTable     guifg=#ffffff guibg=NONE

" rust #db7b5f
hi Todo         guifg=#ffffff guibg=#db7b5f

" clay #f1b090
hi Comment      guifg=#f1b090 guibg=NONE
hi netrwComment guifg=#f1b090 guibg=NONE

" magenta light #caa6df
hi Function     guifg=#caa6df guibg=NONE
hi luaFunction  guifg=#caa6df guibg=NONE

" magenta #b6a0ff
hi StorageClass    guifg=#b6a0ff guibg=NONE
hi Structure       guifg=#b6a0ff guibg=NONE
hi Typedef         guifg=#b6a0ff guibg=NONE
hi Special         guifg=#b6a0ff guibg=NONE
hi Conditional     guifg=#b6a0ff guibg=NONE
hi Repeat          guifg=#b6a0ff guibg=NONE
hi Label           guifg=#b6a0ff guibg=NONE
hi Keyword         guifg=#b6a0ff guibg=NONE
hi cType           guifg=#b6a0ff guibg=NONE
hi cOperator       guifg=#b6a0ff guibg=NONE
hi cStatement      guifg=#b6a0ff guibg=NONE
hi cppCast         guifg=#b6a0ff guibg=NONE
hi cppBuiltinNames guifg=#b6a0ff guibg=NONE
hi vimHighlight    guifg=#b6a0ff guibg=NONE
hi vimCommand      guifg=#b6a0ff guibg=NONE
hi vimNotFunc      guifg=#b6a0ff guibg=NONE
hi pythonStatement guifg=#b6a0ff guibg=NONE
hi pythonException guifg=#b6a0ff guibg=NONE
hi tomlTable       guifg=#b6a0ff guibg=NONE
hi luaStatement    guifg=#b6a0ff guibg=NONE
hi luaOperator     guifg=#b6a0ff guibg=NONE

" magenta #b6a0ff
hi MatchParen   guifg=NONE guibg=#b6a0ff

" blue #79a8ff
hi String       guifg=#79a8ff guibg=NONE
hi Character    guifg=#79a8ff guibg=NONE
hi Directory    guifg=#79a8ff guibg=NONE

" green #6ae4b9
hi Boolean      guifg=#6ae4b9 guibg=NONE
hi Type         guifg=#6ae4b9 guibg=NONE

" red #ff7f9f
hi PreProc      guifg=#ff7f9f guibg=NONE
hi Include      guifg=#ff7f9f guibg=NONE
hi Define       guifg=#ff7f9f guibg=NONE
hi PreCondit    guifg=#ff7f9f guibg=NONE

" grey active   #4a4f69
hi StatusLine   guifg=#ffffff guibg=#4a4f69

" grey inactive #2b3045
hi StatusLineNC guifg=#ffffff guibg=#2b3045
hi Visual       guifg=NONE guibg=#2b3045
