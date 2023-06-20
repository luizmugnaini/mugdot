local builtin = require("telescope.builtin")
vim.keymap.set("n", "<leader>ff", builtin.find_files, {desc = "[F]ind [F]ile"})
vim.keymap.set("n", "<leader>fg", builtin.git_files,
               {desc = "[F]ind in [G]it repo"})
vim.keymap.set("n", "<leader>fz", function()
    builtin.grep_string({search = vim.fn.input("Grep > ")});
end, {desc = "Fuzzy finder"})
