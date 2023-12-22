local lsp = require("lsp-zero").preset({
	name = "recommended",
})
local lspconfig = require("lspconfig")

lsp.ensure_installed({ "tsserver", "rust_analyzer", "pyright", "lua_ls", "clangd", "zls" })

-- Fix Undefined global 'vim' in Lua server.
lspconfig.lua_ls.setup({ settings = { Lua = { diagnostics = { globals = { "vim" } } } } })

lspconfig.clangd.setup({
	cmd = { "clangd", "--log=verbose", "--compile-commands-dir=./build" },
	filetypes = { "c", "cpp" },
	root_dir = function()
		lsp.dir.find_first({ ".git", ".clang-format", ".clangd", ".clang-tidy" })
	end,
})

-- Typst lsp - only experimenting with it for the time being
vim.filetype.add({ extension = { typst = "typ" } })
lspconfig.typst_lsp.setup({
	filetypes = { "typst" },
})

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
	vim.keymap.set("n", "K", function()
		vim.lsp.buf.hover()
	end, opts)
	vim.keymap.set("n", "<leader>vws", function()
		vim.lsp.buf.workspace_symbol()
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
	vim.keymap.set("i", "<C-h>", function()
		vim.lsp.buf.signature_help()
	end, opts)
end)

lsp.setup()

-- Whether or not to display text messages on the screen.
vim.diagnostic.config({ virtual_text = false })
