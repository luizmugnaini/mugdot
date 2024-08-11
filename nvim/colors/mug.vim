" Name: mug.vim
" Author: Luiz G. Mugnaini A. <luizmugnaini@gmail.com>
"
" Colors:
" - fg           "#ffffff"
" - bg           "#0d0e1c"
" - greyActive   "#4a4f69"
" - greyInactive "#2b3045"
" - clay         "#f1b090"
" - rust         "#db7b5f"
" - green        "#6ae4b9"
" - blue         "#79a8ff"
" - pink         "#ff66ff"
" - magenta      "#b6a0ff"
" - magentaLight "#caa6df"
" - red          "#ff7f9f"

if exists('syntax on')
    syntax reset
endif

set background=dark
hi clear

let g:colors_name='mug'
set t_Co=256


" misc

hi ColorColumn      guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi Conceal          guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi Cursor           guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi lCursor          guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi CursorIM         guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi CursorColumn     guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi CursorLine       guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi Directory        guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi DiffAdd          guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi DiffChange       guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi DiffDelete       guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi DiffText         guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi EndOfBuffer      guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi ErrorMsg         guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi VertSplit        guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi Folded           guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi FoldColumn       guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi SignColumn       guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi IncSearch        guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi LineNr           guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi LineNrAbove      guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi LineNrBelow      guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi CursorLineNr     guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi MatchParen       guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi ModeMsg          guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi MoreMsg          guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi NonText          guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi Pmenu            guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi PmenuSel         guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi PmenuSbar        guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi PmenuThumb       guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi Question         guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi QuickFixLine     guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi Search           guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi SpecialKey       guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi SpellBad         guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi SpellCap         guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi SpellLocal       guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi SpellRare        guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi StatusLine       guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi StatusLineNC     guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi StatusLineTerm   guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi StatusLineTermNC guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi TabLine          guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi TabLineFill      guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi TabLineSel       guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi Terminal         guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi Title            guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi Visual           guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi VisualNOS        guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi WarningMsg       guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi WildMenu         guifg=NONE guibg=NONE ctermfg=231 ctermbg=234

" major

hi Normal     guifg=#ffffff guibg=NONE ctermfg=231 ctermbg=NONE
hi Comment    guifg=#f1b090 guibg=NONE ctermfg=#f1b090 ctermbg=234
hi Constant   guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi Identifier guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi Statement  guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi PreProc    guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi Type       guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi Special    guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi Underlined guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi Ignore     guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi Error      guifg=NONE guibg=NONE ctermfg=231 ctermbg=234
hi Todo       guifg=NONE guibg=NONE ctermfg=231 ctermbg=234

" minor

hi String         guifg=#ffffff guibg=#1c1c1c ctermfg=231 ctermbg=234
hi Character      guifg=#ffffff guibg=#1c1c1c ctermfg=231 ctermbg=234
hi Number         guifg=#ffffff guibg=#1c1c1c ctermfg=231 ctermbg=234
hi Boolean        guifg=#ffffff guibg=#1c1c1c ctermfg=231 ctermbg=234
hi Float          guifg=#ffffff guibg=#1c1c1c ctermfg=231 ctermbg=234
hi Function       guifg=#ffffff guibg=#1c1c1c ctermfg=231 ctermbg=234
hi Conditional    guifg=#ffffff guibg=#1c1c1c ctermfg=231 ctermbg=234
hi Repeat         guifg=#ffffff guibg=#1c1c1c ctermfg=231 ctermbg=234
hi Label          guifg=#ffffff guibg=#1c1c1c ctermfg=231 ctermbg=234
hi Operator       guifg=#ffffff guibg=#1c1c1c ctermfg=231 ctermbg=234
hi Keyword        guifg=#ffffff guibg=#1c1c1c ctermfg=231 ctermbg=234
hi Exception      guifg=#ffffff guibg=#1c1c1c ctermfg=231 ctermbg=234
hi Include        guifg=#ffffff guibg=#1c1c1c ctermfg=231 ctermbg=234
hi Define         guifg=#ffffff guibg=#1c1c1c ctermfg=231 ctermbg=234
hi Macro          guifg=#ffffff guibg=#1c1c1c ctermfg=231 ctermbg=234
hi PreCondit      guifg=#ffffff guibg=#1c1c1c ctermfg=231 ctermbg=234
hi StorageClass   guifg=#ffffff guibg=#1c1c1c ctermfg=231 ctermbg=234
hi Structure      guifg=#ffffff guibg=#1c1c1c ctermfg=231 ctermbg=234
hi Typedef        guifg=#ffffff guibg=#1c1c1c ctermfg=231 ctermbg=234
hi SpecialChar    guifg=#ffffff guibg=#1c1c1c ctermfg=231 ctermbg=234
hi Tag            guifg=#ffffff guibg=#1c1c1c ctermfg=231 ctermbg=234
hi Delimiter      guifg=#ffffff guibg=#1c1c1c ctermfg=231 ctermbg=234
hi SpecialComment guifg=#ffffff guibg=#1c1c1c ctermfg=231 ctermbg=234
hi Debug          guifg=#ffffff guibg=#1c1c1c ctermfg=231 ctermbg=234
