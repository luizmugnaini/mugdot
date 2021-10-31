" Disable default mappings
let g:nnn#set_default_mappings = 0
let g:nnn#replace_netrw = 1

" Start nnn with the following command
let g:nnn#command = 'nnn -H'

" Start nnn in the current file's directory
nnoremap <silent> <leader>n :NnnPicker %:p:h<CR>
