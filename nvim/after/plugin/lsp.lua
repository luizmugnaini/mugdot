-- ----------------------------------------------------------------------------
-- LSP config.
-- ----------------------------------------------------------------------------

local lsp = require("lsp-zero").preset({
	name = "recommended",
})
local lspconfig = require("lspconfig")

lsp.ensure_installed({ "rust_analyzer", "clangd", "pyright" })

lspconfig.clangd.setup({
	cmd = { "clangd", "--log=verbose", "--compile-commands-dir=./build" },
	filetypes = { "c", "cpp" },
	root_dir = function()
		lsp.dir.find_first({ ".git", ".clang-format", ".clangd", ".clang-tidy" })
	end,
})

-- Completion mappings
local cmp = require("cmp")
local cmp_select = { behavior = cmp.SelectBehavior.Select }
local cmp_mappings = lsp.defaults.cmp_mappings({
	["<C-p>"] = cmp.mapping.select_prev_item(cmp_select),
	["<C-n>"] = cmp.mapping.select_next_item(cmp_select),
	["<C-y>"] = cmp.mapping.confirm({ select = true }),
	["<C-Space>"] = cmp.mapping.complete(),
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
	vim.keymap.set("n", "<leader>vd", function()
		vim.diagnostic.open_float()
	end, opts)
	vim.keymap.set("n", "<leader>vca", function()
		vim.lsp.buf.code_action()
	end, opts)
	vim.keymap.set("n", "<leader>vrr", function()
		vim.lsp.buf.references()
	end, opts)
	vim.keymap.set("n", "<leader>vrn", function()
		vim.lsp.buf.rename()
	end, opts)
end)

lsp.setup()

-- DON'T display text messages on the screen.
vim.diagnostic.config({ virtual_text = false })

-- ----------------------------------------------------------------------------
-- Mason
-- ----------------------------------------------------------------------------

require("mason").setup({
	ensure_installed = {
		-- Python
		"pyright",
		"black",
		"ruff",

		-- C/C++
		"clang-format",
		"clangd",

		-- TS/JS
		"eslint_d",
		"tsserver",

		-- Rust
		"rust_analyzer",

		-- Lua
		"lua_ls",
		"luaformatter",
	},
})

-- ----------------------------------------------------------------------------
-- Trouble
-- ----------------------------------------------------------------------------

vim.keymap.set("n", "<leader>tt", function()
	require("trouble").open()
end, { desc = "[T]trouble [T]oggle" })
