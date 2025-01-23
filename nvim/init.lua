-- =============================================================================
-- My custom NeoVim experience.
--
-- Author: Luiz G. Mugnaini A. <luizmugnaini@gmail.com>
-- =============================================================================

-- -----------------------------------------------------------------------------
-- System information
-- -----------------------------------------------------------------------------

local os_windows = package.config:sub(1, 1) == "\\"
local path_sep    = os_windows and "\\" or "/"

local function make_path(components)
    if #components < 1 then
        return ""
    end

    local path = components[1]
    for i = 2, #components do
        path = path .. path_sep .. components[i]
    end

    return path
end

local work_dir   = vim.fn.getcwd()
local home_dir   = os_windows and os.getenv("USERPROFILE") or os.getenv("HOME")
local config_dir = make_path({ home_dir, ".config", "mugdot" } )
local nvim_dir   = make_path({ config_dir, "nvim" })
local lua_exe    = (type(jit) == "table") and "luajit" or "lua"

-- -----------------------------------------------------------------------------
-- General settings
-- -----------------------------------------------------------------------------

-- Use SPACE as the leader key.
vim.g.mapleader = " "

-- Disable useless language providers.
vim.g.loaded_python3_provider = 0
vim.g.loaded_ruby_provider    = 0
vim.g.loaded_node_provider    = 0
vim.g.loaded_perl_provider    = 0

-- Visuals
vim.opt.guicursor     = ""    -- Use a block as the cursor.
vim.opt.showmode      = false -- Don't show the current mode in the minibuffer
vim.opt.number        = false -- Don't show line numbers
vim.opt.termguicolors = true
vim.opt.signcolumn    = "no"
vim.opt.showtabline   = 0
vim.opt.statusline    = " %f %h%m%r%=%-14.(%l,%c%)"

-- Indentation
vim.opt.tabstop     = 4
vim.opt.shiftwidth  = 4
vim.opt.expandtab   = true
vim.opt.smartindent = true

-- Text rendering behaviour
vim.opt.wrap      = false
vim.opt.scrolloff = 4 -- Minimal number of lines below cursor line

-- Visual mode
vim.opt.virtualedit = "block" -- Allow cursor to move where there is no text in visual block mode

-- Backup
vim.opt.swapfile = false
vim.opt.backup   = false
vim.opt.undodir  = vim.fn.stdpath("data") .. "/undo"
vim.opt.undofile = true

-- Searching functionality
vim.opt.grepprg    = "rg --vimgrep"
vim.opt.hlsearch   = false
vim.opt.incsearch  = false
vim.opt.smartcase  = true
vim.opt.ignorecase = true

-- Spelling
vim.opt.spelllang = { "en" }
vim.opt.spell     = false

vim.opt.updatetime = 50

-- Tags
vim.opt.tags = ".tags"

-- Misc
vim.opt.wildignore        = { "*.o", "*.obj", "*.lib", "*.a", "*.exe", "*.pdb", "*.ilk", ".git", "*.pdf", "*.png", "*.jpeg", "*.aseprite" }
vim.g.netrw_sort_sequence = "[\\/],*"
vim.opt.encoding          = "utf8"
vim.opt.clipboard         = "unnamedplus" -- Copy to and from vim using the system clipboard register

-- Buffer reloading
--
-- Automatically reload buffers which changed outside of the editor.
vim.opt.autoread = true
vim.api.nvim_create_autocmd(
    { "FocusGained", "BufEnter" },
    { callback = function() vim.cmd("checktime") end }
)

-- Key mapping responsiveness
vim.o.timeout    = true
vim.o.timeoutlen = 500

local mug_group = vim.api.nvim_create_augroup("mug", { clear = true })

local all_modes        = { "n", "i", "x", "v", "s", "c", "o", "l", "t" }
local non_insert_modes = { "n", "v", "x", "o" }

-- -----------------------------------------------------------------------------
-- File-types
-- -----------------------------------------------------------------------------

vim.api.nvim_create_autocmd("BufEnter", {
    desc    = "Treat miscelaneous files as C for syntax highlighting",
    group   = mug_group,
    pattern = { "*.cu", "*.glsl", "*.vert", "*.tesc", "*.tese", "*.geom", "*.frag", "*.comp" },
    command = "set filetype=c",
})

vim.api.nvim_create_autocmd("BufEnter", {
    desc    = "Use LaTeX mode for .tex...",
    group   = mug_group,
    pattern = { "*.tex" },
    command = "set filetype=tex",
})

-- -----------------------------------------------------------------------------
-- Formatting
-- -----------------------------------------------------------------------------

local function trim_whitespaces()
    local view = vim.fn.winsaveview()
    vim.api.nvim_exec([[keepjumps keeppatterns silent! %s/\s\+$//e]], { output = false })
    vim.fn.winrestview(view)
end

local function fmt_buf(formatter)
    local view = vim.fn.winsaveview()
    vim.api.nvim_exec([[keepjumps keeppatterns silent %!]] .. formatter, { output = false })
    vim.fn.winrestview(view)
end

vim.api.nvim_create_autocmd("BufWritePre", {
    desc     = "Trim whitespaces in text files",
    group    = mug_group,
    pattern  = { "*" },
    callback = trim_whitespaces,
})

-- -----------------------------------------------------------------------------
-- Auto-commands.
-- -----------------------------------------------------------------------------

local c_like = { "*.c", "*.h", "*.cc", "*.cpp", "*.hpp", "*.glsl", "*.vert", "*.tesc", "*.tese", "*.geom", "*.frag", "*.comp" }

local function insert_code_section_header()
    local comment_header_separator = "// " .. string.rep("-", 97)
    vim.api.nvim_put({ comment_header_separator, "// " }, "l", true, true)
    vim.api.nvim_put({ comment_header_separator }, "l", true, false)
end

vim.api.nvim_create_user_command(
    "CodeSection",
    insert_code_section_header,
    { desc = "Write a code section header for C-like languages." }
)

local function comment_visual_selection()
    local start_line, _, end_line = unpack(vim.fn.getpos("'<")), unpack(vim.fn.getpos("'>"))

    for line = start_line, end_line do
        local line_content = vim.fn.getline(line)
        if line_content ~= "" then
            vim.fn.setline(line, "// " .. line_content)
        end
    end
end

vim.api.nvim_create_user_command(
    "CommentSelection",
    comment_visual_selection,
    { desc = "Comment-out the selected lines" }
)
vim.keymap.set("v", "<A-;>", function() comment_visual_selection() end, { noremap = true, silent = true })

-- -----------------------------------------------------------------------------
-- Terminal integration
-- -----------------------------------------------------------------------------

local terminal         = { buf = -1, win = -1 }
local last_marker_line = 0

local function terminal_get_last_marker_line()
    local value      = previous_marker_line
    last_marker_line = vim.api.nvim_buf_line_count(terminal.buf)
    return value
end

local function make_floating_terminal(terminal_buf)
    local instance_columns = vim.o.columns
    local instance_lines   = vim.o.lines

    local win_config    = {}
    win_config.relative = "editor"
    win_config.width    = math.floor(instance_columns * 0.8)
    win_config.height   = math.floor(instance_lines * 0.8)
    win_config.col      = math.floor((instance_columns - win_config.width) / 2.0)
    win_config.row      = math.floor((instance_lines - win_config.height) / 2.0)
    win_config.style    = "minimal"
    win_config.border   = "rounded"

    local buf = vim.api.nvim_buf_is_valid(terminal_buf) and terminal_buf or vim.api.nvim_create_buf(false, true)
    local win = vim.api.nvim_open_win(buf, true, win_config)

    if vim.bo[buf].buftype ~= "terminal" then
        vim.cmd.terminal()
    end

    return { buf = buf, win = win }
end

local function toggle_floating_terminal_visibility()
    if not vim.api.nvim_win_is_valid(terminal.win) then
        terminal = make_floating_terminal(terminal.buf)
    else
        vim.api.nvim_win_hide(terminal.win)
    end
end

vim.api.nvim_create_user_command(
    "FloatingTerminalToggle",
    toggle_floating_terminal_visibility,
    { desc = "Toggle the visibility floating terminal buffer." }
)

-- Terminal mode.
vim.keymap.set("t", "<esc><esc>", "<c-\\><c-n>")
vim.keymap.set({ "n", "t" }, "<leader>tt", vim.cmd.FloatingTerminalToggle, { desc = "Toggle the floating terminal window" })

-- -----------------------------------------------------------------------------
-- Build system integration
-- -----------------------------------------------------------------------------

local build_file_name  = "build.lua"
local build_file_path  = make_path({ work_dir, build_file_name })
local has_build_file   = (vim.fn.filereadable(build_file_path) == 1)
local cached_build_cmd = ""

local function parse_last_build_command_errors_in_terminal()
    if not vim.api.nvim_buf_is_valid(terminal.buf) then
        vim.api.nvim_notify("No terminal buffer found - can't parse non-existing build command.", vim.log.levels.ERROR, {})
        return
    end

    vim.fn.setqflist({})

    local errors = {}
    local lines = vim.api.nvim_buf_get_lines(terminal.buf, terminal_get_last_marker_line(), -1, false)
    for _, line in ipairs(lines) do
        -- Parse Clang compiler errors.
        local file, line_number, msg = line:match("([%.\\/%w%._\\-]+)%((%d+),%d+%)%:%s*(.*)")
        if not file then
            -- Parse MSVC compiler errors.
            file, line_number, msg = line:match("([A-Za-z]:[%w%._\\/-]+)%((%d+)%):%s*(.*)")
        end

        if file and line_number then
          table.insert(errors, { filename = file, lnum = tonumber(line_number), text = msg })
        end
    end

    if #errors > 0 then
        vim.api.nvim_notify("Compiler output parsed: errors found, adding to the quickfix list.", vim.log.levels.WARN, {})
        vim.fn.setqflist(errors)

        local current_win = vim.api.nvim_get_current_win()
        vim.cmd("botright copen") -- Open the quickfix window at the bottom without changing focus
        vim.api.nvim_set_current_win(current_win)
    else
        vim.api.nvim_notify("Compiler output parsed: no errors found.", vim.log.levels.INFO, {})
        vim.cmd("cclose")
    end
end

local function send_build_command_to_terminal(args)
    if terminal.buf == vim.api.nvim_get_current_buf() then
        vim.api.nvim_notify("Don't be silly, just write the damn command directly...", vim.log.levels.WARN, {})
        return
    end

    if not vim.api.nvim_win_is_valid(terminal.win) then
        terminal = make_floating_terminal(terminal.buf)
    end

    cached_build_cmd = table.concat(args, " ")
    vim.api.nvim_notify(string.format("Sending the command '%s'...", cached_build_cmd), vim.log.levels.INFO, {})
    vim.api.nvim_chan_send(terminal.buf, cached_build_cmd .. "\r")
end

vim.api.nvim_create_user_command(
    "Build",
    function(opts)
        send_build_command_to_terminal(opts.fargs)
    end,
    { nargs = "*", desc  = "Run a build command in a terminal buffer." }
)

vim.api.nvim_create_user_command(
    "ParseCompilerOutput",
    parse_last_build_command_errors_in_terminal,
    { desc  = "Parse the compiler output of the last build command ran in the floating terminal." }
)

vim.keymap.set(
    non_insert_modes,
    "<leader>cc",
    ":Build " .. cached_build_cmd,
    { noremap = true, silent = false }
)
vim.keymap.set(
    non_insert_modes,
    "<leader>pc",
    vim.cmd.ParseCompilerOutput,
    { noremap = true, silent = true }
)

-- -----------------------------------------------------------------------------
-- Keybindings
-- -----------------------------------------------------------------------------

vim.keymap.set(all_modes, "<C-k>", "<Esc>", { silent = true })

vim.keymap.set(non_insert_modes, "<leader>w", vim.cmd.write, { desc = "[W]rite file" })
vim.keymap.set(non_insert_modes, "<leader>q", function() vim.cmd("q") end, { desc = "Kill the current buffer" })

-- vim.keymap.set(non_insert_modes, "<leader>e", vim.cmd.Ex, { desc = "Explore files" })

-- Window splits
vim.keymap.set(non_insert_modes, "<leader>s", vim.cmd.vsplit, { desc = "Split Vertically", silent = true })
vim.keymap.set(non_insert_modes, "<leader>h", vim.cmd.split, { desc = "Split horizontally", silent = true })

-- Window movement
vim.keymap.set(non_insert_modes, "<leader>o", function() vim.cmd.wincmd("w") end, { desc = "Move to next window", silent = true })

-- Tags
vim.keymap.set(non_insert_modes, "gd", "<C-]>", { desc = "Go to definition" })
vim.keymap.set(non_insert_modes, "gt", vim.cmd.tselect, { desc = "Get all tags under this identifier" })
vim.keymap.set(non_insert_modes, "gp", vim.cmd.pop, { desc = "Go to [P]revious [T]ag" })

-- CTags
local ctags_exe = os_windows and (home_dir .. "/scoop/apps/universal-ctags/current/ctags.exe") or "/usr/bin/ctags"
local ctags_args = "-o .tags --languages=c,c++,lua,python --fields=NPESZaimnorts --exclude=.git --exclude=build --recurse"
vim.keymap.set(
    non_insert_modes,
    "<leader>ut",
    "<cmd>!" .. ctags_exe .. " " .. ctags_args .. "<cr><cr>",
    { desc = "Update the tag cache" }
)

-- -----------------------------------------------------------------------------
-- Colors
-- -----------------------------------------------------------------------------

vim.cmd.colorscheme("mug")

-- -----------------------------------------------------------------------------
-- Bootstrap lazy.nvim
-- -----------------------------------------------------------------------------

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"

if not vim.loop.fs_stat(lazypath) then
    vim.fn.system({
        "git",
        "clone",
        "--filter=blob:none",
        "https://github.com/folke/lazy.nvim.git",
        "--branch=stable",
        lazypath,
    })
end
vim.opt.rtp:prepend(lazypath)

-- -----------------------------------------------------------------------------
-- Packages
-- -----------------------------------------------------------------------------

require("lazy").setup({
    {
        "nvim-telescope/telescope.nvim",
        tag = "0.1.6",
        dependencies = {
            { "nvim-lua/plenary.nvim" },
            {
                "nvim-telescope/telescope-fzf-native.nvim",
                build = "cmake -S . -B build -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release"
                    .. ( os_windows and " && mv build/Release/libfzf.dll build/libfzf.dll && mv build/Release/fzf.lib build/fzf.lib" or ""),
            },
        },
        event  = "VeryLazy",
        config = function()
            local telescope = require("telescope")
            local builtin   = require("telescope.builtin")

            telescope.setup({
                defaults = {
                    file_ignore_patterns = {
                        -- Extension based.
                        "%.exe$", "%.dll$", "%.so$", "%.obj", "%.pdb$", "%.ilk$", "%.lib$", "%.a$", "%.ttf$",
                        "%.xml$", "%.in$", "%.ini$", "%.lock$", "%.png$", "%.aseprite$", "%.gpl$", "%.blend$",
                        "%.kra$", "%.jpg$", "%.jpeg$", "%.rdbg",
                        -- Directory based.
                        "%.git", "build" .. path_sep, "%.ignore" .. path_sep,
                        -- Directory + extension.
                        "thirdparty.*%.cmake$", "deps.*%.cmake$",
                        -- Name based.
                        ".*LICENSE.*", "tags", "%.tags",
                    },
                }
            })

            telescope.load_extension("fzf")

            vim.keymap.set("n", "<leader>ff", function() builtin.find_files({ hidden = true }) end)
            vim.keymap.set("n", "<leader>bb", builtin.buffers)
            vim.keymap.set("n", "<leader>fz", function() builtin.grep_string({ search = vim.fn.input("grep> ") }) end)
        end,
    },

    -- File explorer.
    {
        "stevearc/oil.nvim",
        config = function()
            require("oil").setup({
                default_file_explorer = true,
                columns = {
                    "permissions",
                    "mtime",
                    "size",
                },
                view_options = {
                    show_hidden = true,
                },
                use_default_keymaps = true,
                keymap = {
                    ["<CR>"]  = { "actions.select" },
                    ["-"]     = { "actions.parent",    mode = "n" },
                    ["_"]     = { "actions.open_cwd",  mode = "n" },
                    ["cd"]    = { "actions.cd",        mode = "n" },
                    ["gr"]    = { "actions.refresh",   mode = "n" },
                },
            })
            vim.keymap.set(non_insert_modes, "<leader>e", "<cmd>Oil<cr>")
        end
    },

    {
        "hrsh7th/nvim-cmp",
        dependencies = { "hrsh7th/cmp-buffer" },
        config       = function()
            local cmp = require("cmp")

            cmp.setup({
                snippet = {
                    expand = function(args)
                        vim.snippet.expand(args.body)
                    end,
                },
                mapping = cmp.mapping.preset.insert({
                    ["<C-y>"] = cmp.mapping.confirm({ select = true }),
                    ["<C-j>"] = cmp.mapping.select_prev_item(),
                    ["<C-n>"] = cmp.mapping.select_next_item(),
                }),
                sources = { { name = "buffer" } },
            })
        end,
    },
})
