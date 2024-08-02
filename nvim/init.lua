-- =============================================================================
-- My custom NeoVim experience.
--
-- Author: Luiz G. Mugnaini A. <luizmugnaini@gmail.com>
-- =============================================================================

function tern(cond, opt_true, opt_false)
    if cond then
        return opt_true
    else
        return opt_false
    end
end

vim.g.mug_os_windows = package.config:sub(1, 1) == "\\"
vim.g.mug_home = tern(vim.g.mug_os_windows, os.getenv("USERPROFILE"), os.getenv("HOME"))
vim.g.mug_nvim_dir = vim.g.mug_home .. "/.config/mugdot/nvim"

vim.g.lsp_enabled = (os.getenv("LSP") ~= nil)
vim.g.treesitter_enabled = true

-- -----------------------------------------------------------------------------
-- General settings
-- -----------------------------------------------------------------------------

-- Use SPACE as the leader key.
vim.g.mapleader = " "

-- Find python and set its provider path.
local python_path_proc = io.popen('python3 -c "import sys;print(sys.executable)"')
if python_path_proc ~= nil then
    local python_path = python_path_proc:read("l")
    if python_path ~= nil and python_path ~= "" then
        vim.g.python3_host_prog = python_path
    end
    python_path_proc:close()
end

-- Disable useless language providers.
vim.g.loaded_ruby_provider = 0
vim.g.loaded_node_provider = 0
vim.g.loaded_perl_provider = 0

-- Visuals
vim.opt.guicursor = "" -- Use a block as the cursor.
vim.opt.showmode = false -- Don't show the current mode in the minibuffer
vim.opt.number = false -- Don't show line numbers
vim.opt.termguicolors = true
vim.opt.signcolumn = "no"
vim.opt.showtabline = 0
vim.opt.statusline = " %f %h%m%r%=%-14.(%l,%c%)"

-- Indentation
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true
vim.opt.smartindent = true

-- Text rendering behaviour
vim.opt.wrap = false
vim.opt.scrolloff = 4 -- Minimal number of lines below cursor line

-- Visual mode
vim.opt.virtualedit = "block" -- Allow cursor to move where there is no text in visual block mode

-- Backup
vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.undodir = vim.fn.stdpath("data") .. "/undo"
vim.opt.undofile = true

-- Searching functionality
vim.opt.grepprg = "rg --vimgrep"
vim.opt.hlsearch = false
vim.opt.incsearch = true
vim.opt.smartcase = true
vim.opt.ignorecase = true

-- Spelling
vim.opt.spelllang = { "en" }
vim.opt.spell = false

vim.opt.updatetime = 50

-- Tags
vim.opt.tags = ".tags"

-- Misc
vim.opt.wildignore = { "*.o", "*.obj", "*.lib", "*.a", "*.exe", "*.pdb", "*.ilk", ".git" }
vim.g.netrw_sort_sequence = "[\\/],*"
vim.opt.encoding = "utf8"
vim.opt.clipboard = "unnamedplus" -- Copy to and from vim using the system clipboard register

-- -----------------------------------------------------------------------------
-- Auto-commands.
-- -----------------------------------------------------------------------------

local mug_group = vim.api.nvim_create_augroup("mug", { clear = true })

local c_like =
    { "*.c", "*.h", "*.cc", "*.cpp", "*.hpp", "*.glsl", "*.vert", "*.tesc", "*.tese", "*.geom", "*.frag", "*.comp" }

function trim_whitespaces()
    local view = vim.fn.winsaveview()
    vim.api.nvim_exec([[keepjumps keeppatterns silent! %s/\s\+$//e]], { output = false })
    vim.fn.winrestview(view)
end

function fmt_buf(formatter)
    local view = vim.fn.winsaveview()
    vim.api.nvim_exec([[keepjumps keeppatterns silent %!]] .. formatter, { output = false })
    vim.fn.winrestview(view)
end

vim.api.nvim_create_autocmd("BufEnter", {
    desc = "Detect GLSL files",
    group = mug_group,
    pattern = { "*.glsl", "*.vert", "*.tesc", "*.tese", "*.geom", "*.frag", "*.comp" },
    command = "set filetype=glsl",
})

-- -----------------------------------------------------------------------------
-- Keybindings
-- -----------------------------------------------------------------------------

local all_modes = { "n", "i", "x", "v", "s", "c", "o", "l", "t" }
local non_insert_modes = { "n", "v", "x", "o" }

vim.keymap.set(all_modes, "<C-k>", "<Esc>", { silent = true })

vim.keymap.set(non_insert_modes, "<leader>w", vim.cmd.write, { desc = "[W]rite file" })
vim.keymap.set(non_insert_modes, "<leader>q", vim.cmd.quit, { desc = "Kill the current buffer" })
vim.keymap.set(non_insert_modes, "<leader>e", vim.cmd.Ex, { desc = "Explore files" })

-- Window splits
vim.keymap.set(non_insert_modes, "<leader>s", vim.cmd.vsplit, { desc = "Split Vertically", silent = true })
vim.keymap.set(non_insert_modes, "<leader>h", vim.cmd.split, { desc = "Split horizontally", silent = true })

-- Window movement
vim.keymap.set(non_insert_modes, "<leader>o", function()
    vim.cmd.wincmd("w")
end, { desc = "Move to next window", silent = true })

-- Tags
vim.keymap.set(non_insert_modes, "gd", "<C-]>", { desc = "Go to definition" })
vim.keymap.set(non_insert_modes, "gt", vim.cmd.tselect, { desc = "Get all tags under this identifier" })
vim.keymap.set(non_insert_modes, "gp", vim.cmd.pop, { desc = "Go to [P]revious [T]ag" })

local ctags_exe =
    tern(vim.g.mug_os_windows, vim.g.mug_home .. "/scoop/apps/universal-ctags/current/ctags.exe", "/usr/bin/ctags")
local ctags_args =
    "-o .tags --languages=c,c++ --kinds-all=* --extras=* --fields=NPESZaimnorts --exclude=.git --exclude=build --recurse"
vim.keymap.set(
    non_insert_modes,
    "<leader>ut",
    "<cmd>!" .. ctags_exe .. " " .. ctags_args .. "<cr>",
    { desc = "Update the tag cache" }
)

-- -----------------------------------------------------------------------------
-- Packages
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

require("lazy").setup({
    -- -------------------------------------------------------------------------
    -- Visuals.
    -- -------------------------------------------------------------------------

    -- Custom colorscheme.
    {
        "tjdevries/colorbuddy.nvim",
        init = function()
            vim.cmd.colorscheme("muggy")
        end,
    },

    -- -------------------------------------------------------------------------
    -- Utilities for better development.
    -- -------------------------------------------------------------------------

    {
        "nvim-telescope/telescope.nvim",
        tag = "0.1.6",
        dependencies = { "nvim-lua/plenary.nvim" },
        event = "VeryLazy",
        config = function()
            local builtin = require("telescope.builtin")

            vim.keymap.set("n", "<leader>ff", function()
                builtin.find_files({ hidden = true, file_ignore_patterns = { ".git" } })
            end, { desc = "Find file" })
            vim.keymap.set("n", "<leader>bb", builtin.buffers, { desc = "Find open buffer" })
            vim.keymap.set("n", "<leader>fz", function()
                builtin.grep_string({ search = vim.fn.input("> ") })
            end, { desc = "Fuzzy find string" })
        end,
    },

    -- Utility for line and block comments.
    {
        "numToStr/Comment.nvim",
        event = "VeryLazy",
        config = function()
            require("Comment").setup({
                toggler = {
                    line = "<A-;>",
                },
                opleader = {
                    line = "<A-;>",
                },
            })
        end,
    },

    -- Automatic formatting at save.
    {
        "stevearc/conform.nvim",
        event = "VeryLazy",
        ft = { "c", "cpp", "glsl", "lua", "python" },
        config = function()
            require("conform").setup({
                formatters = {
                    stylua = { append_args = { "--indent-type=Spaces" } },
                },
                formatters_by_ft = {
                    c = { "clang-format" },
                    cpp = { "clang-format" },
                    glsl = { "clang-format" },
                    lua = { "stylua" },
                    python = { "black" },
                },
            })

            vim.api.nvim_create_autocmd("BufWritePre", {
                pattern = "*",
                callback = function(args)
                    require("conform").format({ bufnr = args.buf })
                end,
            })
        end,
    },

    -- -------------------------------------------------------------------------
    -- Snippets
    -- -------------------------------------------------------------------------

    {
        "L3MON4D3/LuaSnip",
        build = "make install_jsregexp",
        ft = { "tex", "c", "cpp", "glsl" },
        config = function()
            local ls = require("luasnip")

            require("luasnip.loaders.from_lua").lazy_load({ paths = vim.g.mug_nvim_dir .. "/snippets" })
            ls.config.setup({ enable_autosnippets = true })

            vim.keymap.set({ "i" }, "<C-e>", function()
                ls.expand()
            end, { silent = true, desc = "Expand snippet" })
        end,
    },

    {
        "hrsh7th/nvim-cmp",
        cond = not vim.g.lsp_enabled,
        dependencies = { "hrsh7th/cmp-buffer" },
        config = function()
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
                sources = { { name = "luasnip" }, { name = "buffer" } },
            })
        end,
    },

    -- -------------------------------------------------------------------------
    -- LSP support
    -- -------------------------------------------------------------------------

    {
        "VonHeikemen/lsp-zero.nvim",
        cond = vim.g.lsp_enabled,
        branch = "v3.x",
        -- event = "VeryLazy",
        dependencies = {
            "neovim/nvim-lspconfig",
            "L3MON4D3/LuaSnip",
            "saadparwaiz1/cmp_luasnip",
            "hrsh7th/cmp-buffer",
            "hrsh7th/cmp-nvim-lsp",
            "hrsh7th/nvim-cmp",
            "folke/trouble.nvim",
        },
        config = function()
            -- ----------------------------------------------------------------------------
            -- Completions
            -- ----------------------------------------------------------------------------

            local cmp = require("cmp")
            local ls = require("luasnip")

            cmp.setup({
                snippet = {
                    expand = function(args)
                        require("luasnip").lsp_expand(args.body)
                    end,
                },
                mapping = cmp.mapping.preset.insert({
                    ["<C-y>"] = cmp.mapping.confirm({ select = true }),
                    ["<C-j>"] = cmp.mapping.select_prev_item(),
                    ["<C-n>"] = cmp.mapping.select_next_item(),
                }),
                sources = { { name = "luasnip" }, { name = "nvim_lsp" }, { name = "buffer" } },
            })

            -- ----------------------------------------------------------------------------
            -- LSP config
            -- ----------------------------------------------------------------------------

            local lsp = require("lsp-zero").preset({ name = "recommended" })
            local lspconfig = require("lspconfig")
            local cmp_capabilities = require("cmp_nvim_lsp").default_capabilities()

            lspconfig.clangd.setup({
                cmd = { "clangd", "--log=verbose", "--compile-commands-dir=./build" },
                filetypes = { "c", "cpp" },
                capabilities = cmp_capabilities,
                root_dir = function()
                    lsp.dir.find_first({ ".git", ".clang-format", ".clangd", ".clang-tidy" })
                end,
            })
            lspconfig.pyright.setup({ capabilities = cmp_capabilities })

            lsp.extend_cmp()
            lsp.set_preferences({
                suggest_lsp_servers = false,
                sign_icons = { error = "E", warn = "W", hint = "H", info = "I" },
            })
            lsp.on_attach(function(client, bufnr)
                local opts = { buffer = bufnr, remap = false }
                vim.keymap.set("n", "gd", function()
                    vim.lsp.buf.definition()
                end, opts)
                vim.keymap.set("n", "gr", function()
                    vim.lsp.buf.references()
                end, opts)
                vim.keymap.set("n", "K", function()
                    vim.lsp.buf.hover()
                end, opts)
                vim.keymap.set("n", "<leader>ca", function()
                    vim.lsp.buf.code_action()
                end, opts)
                vim.keymap.set("n", "<leader>rn", function()
                    vim.lsp.buf.rename()
                end, opts)
            end)

            lsp.setup()

            require("trouble").setup({
                height = 3,
                auto_open = false,
                auto_close = true,
                auto_preview = false,
                icons = false,
            })

            vim.keymap.set("n", "<leader>tt", function()
                require("trouble").open()
            end, { desc = "Toggle Trouble window" })
        end,
    },
})
