-- ----------------------------------------------------------------------------
-- Mason
-- ----------------------------------------------------------------------------

require("mason").setup({
	ensure_installed = {
		"pyright",
		"clangd",
		"rust_analyzer",
		"lua_ls",
	},
})

-- ----------------------------------------------------------------------------
-- lspconfig
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
lspconfig.lua_ls.setup({
	capabilities = cmp_capabilities,
	settings = {
		-- Fix "undefined global 'vim'"
		Lua = { diagnostics = { globals = { "vim" } } },
	},
})
lspconfig.pyright.setup({ capabilities = cmp_capabilities })

-- ----------------------------------------------------------------------------
-- lsp-zero
-- ----------------------------------------------------------------------------

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

-- Whether or not to display text messages on the screen.
vim.diagnostic.config({ virtual_text = false })
