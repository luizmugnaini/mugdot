local lsp = require("lsp-zero").preset({
	name = "recommended",
})
local lspconfig = require("lspconfig")

lsp.ensure_installed({ "gopls", "tsserver", "rust_analyzer", "pyright", "pyre", "lua_ls", "clangd" })

-- Fix Undefined global 'vim' in Lua server.
lspconfig.lua_ls.setup({ settings = { Lua = { diagnostics = { globals = { "vim" } } } } })

lsp.format_on_save({
	["rust_analyzer"] = { "rust" },
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
	vim.keymap.set("n", "[d", function()
		vim.diagnostic.goto_next()
	end, opts)
	vim.keymap.set("n", "]d", function()
		vim.diagnostic.goto_prev()
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

local lsp_configurations = require("lspconfig.configs")

if not lsp_configurations.pyre then
	lsp_configurations.pyre = {
		default_config = {
			name = "pyre",
			cmd = { "pyre", "persistent" },
			filetypes = { "python" },
			root_dir = require("lspconfig.util").root_pattern(".pyre_configuration"),
		},
	}
end

-- Whether or not to display text messages on the screen.
vim.diagnostic.config({ virtual_text = false })
