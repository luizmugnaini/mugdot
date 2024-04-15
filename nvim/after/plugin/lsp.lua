
-- ----------------------------------------------------------------------------
-- Mason
-- ----------------------------------------------------------------------------

require("mason").setup({
	ensure_installed = {
		-- Python
		"black",
		"pyright",
		"ruff",

		-- C/C++
		"clang-format",
		"clangd",

		-- Rust
		"rust_analyzer",

		-- Lua
		"luaformatter",
		"lua_ls",
	},
})

-- ----------------------------------------------------------------------------
-- LSP
-- ----------------------------------------------------------------------------

local lsp = require("lsp-zero").preset({
	name = "recommended",
})
local lspconfig = require("lspconfig")

lspconfig.clangd.setup({
	cmd = { "clangd", "--log=verbose", "--compile-commands-dir=./build" },
	filetypes = { "c", "cpp" },
	root_dir = function()
		lsp.dir.find_first({ ".git", ".clang-format", ".clangd", ".clang-tidy" })
	end,
})

-- Completions
local cmp = require("cmp")
local cmp_select = { behavior = cmp.SelectBehavior.Select }
local cmp_mappings = lsp.defaults.cmp_mappings({
	["<C-p>"] = cmp.mapping.select_prev_item(cmp_select),
	["<C-n>"] = cmp.mapping.select_next_item(cmp_select),
	["<C-y>"] = cmp.mapping.confirm({ select = true }),
	["<Tab>"] = nil,
	["<S-Tab>"] = nil,
})

lsp.setup_nvim_cmp({ mapping = cmp_mappings })

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

-- Whether or not to display text messages on the screen.
vim.diagnostic.config({ virtual_text = false })
