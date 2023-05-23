local mark = require("harpoon.mark")
local ui = require("harpoon.ui")

vim.keymap.set("n", "<leader>ha", mark.add_file, { desc = "[H]arpoon [A]dd file" })
vim.keymap.set("n", "<leader>ho", ui.toggle_quick_menu, { desc = "[H]arpoon [O]pen" })
