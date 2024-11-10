" Name: mug.vim
" Author: Luiz G. Mugnaini A. <luizmugnaini@gmail.com>
" Notes: In order to query the highlight group of the word under the cursor
" you can run
" ```
" :echo synIDattr(synID(line('.'), col('.'), 1), 'name')
" ```
" Copyright:
"           DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
"                    Version 2, December 2004
"
" Copyright (C) 2024 Luiz G. Mugnaini A. <luizmugnaini@gmail.com>
"
" Everyone is permitted to copy and distribute verbatim or modified
" copies of this license document, and changing it is allowed as long
" as the name is changed.
"
"            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
"   TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
"
"  0. You just DO WHAT THE FUCK YOU WANT TO.

if exists('syntax on')
    syntax reset
endif

hi clear
set background=dark
set termguicolors

let g:colors_name='mug'

" fg #ffffff
" bg #0d0e1c
hi Normal       guifg=#ffffff guibg=#0d0e1c
hi netrwList    guifg=#ffffff
hi Delimeter    guifg=#ffffff
hi Identifier   guifg=#ffffff
hi Constant     guifg=#ffffff
hi Number       guifg=#ffffff
hi Float        guifg=#ffffff
hi Operator     guifg=#ffffff
hi Todo         guifg=#ffffff guibg=#db7b5f
hi luaTable     guifg=#ffffff
hi vimHiGroup   guifg=#ffffff
hi vimGroup     guifg=#ffffff

" clay #f1b090
hi Comment      guifg=#f1b090
hi netrwComment guifg=#f1b090

" magenta light #caa6df
hi Function     guifg=#caa6df
hi luaFunction  guifg=#caa6df

" magenta #b6a0ff
hi StorageClass    guifg=#b6a0ff
hi Structure       guifg=#b6a0ff
hi Typedef         guifg=#b6a0ff
hi Special         guifg=#b6a0ff
hi Conditional     guifg=#b6a0ff
hi Repeat          guifg=#b6a0ff
hi Label           guifg=#b6a0ff
hi Keyword         guifg=#b6a0ff
hi cType           guifg=#b6a0ff
hi cOperator       guifg=#b6a0ff
hi cStatement      guifg=#b6a0ff
hi cppCast         guifg=#b6a0ff
hi cppBuiltinNames guifg=#b6a0ff
hi vimHighlight    guifg=#b6a0ff
hi vimCommand      guifg=#b6a0ff
hi vimNotFunc      guifg=#b6a0ff
hi pythonStatement guifg=#b6a0ff
hi pythonException guifg=#b6a0ff
hi tomlTable       guifg=#b6a0ff
hi luaStatement    guifg=#b6a0ff
hi luaOperator     guifg=#b6a0ff

" magenta #b6a0ff
hi MatchParen      guibg=#b6a0ff

" blue #79a8ff
hi String       guifg=#79a8ff
hi Character    guifg=#79a8ff
hi Directory    guifg=#79a8ff

" green #6ae4b9
hi Boolean      guifg=#6ae4b9
hi Type         guifg=#6ae4b9

" red #ff7f9f
hi PreProc      guifg=#ff7f9f
hi Include      guifg=#ff7f9f
hi Define       guifg=#ff7f9f
hi PreCondit    guifg=#ff7f9f

" grey active   #4a4f69
" grey inactive #2b3045
if has('nvim')
  hi StatusLine   guifg=#ffffff guibg=#4a4f69
  hi StatusLineNC guifg=#ffffff guibg=#2b3045
else
  hi StatusLine   guifg=#4a4f69 guibg=#ffffff
  hi StatusLineNC guifg=#2b3045 guibg=#ffffff
endif

hi VertSplit    guifg=#2b3045 guibg=#2b3045
hi Visual       guibg=#2b3045
