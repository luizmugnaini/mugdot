local non_insert_modes = { "n", "x", "o" }

vim.keymap.set(non_insert_modes, "<leader>l", "<Plug>(leap-forward)")
vim.keymap.set(non_insert_modes, "<leader>L", "<Plug>(leap-backward)")
vim.keymap.set(non_insert_modes, "<leader>gl", "<Plug>(leap-from-window)")
