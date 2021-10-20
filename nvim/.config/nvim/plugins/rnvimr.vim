" Make Ranger replace netrw and be the file explorer
let g:rnvimr_ex_enable = 1

" Make Ranger to be hidden after picking a file
let g:rnvimr_enable_picker = 1

" Change the border's color
let g:rnvimr_border_attr = {'fg': 14, 'bg': -1}

" Hide the files included in gitignore
let g:rnvimr_hide_gitignore = 1

" Make nvim wipe the buffers corresponding to the files deleted by Ranger
let g:rnvimr_enable_bw = 1

nmap <space>r :RnvimrToggle<CR>
