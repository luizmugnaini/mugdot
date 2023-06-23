local null_ls = require("null-ls")
local augroup = vim.api.nvim_create_augroup("LspFormatting", {})

null_ls.setup({
	sources = {
		-- Lua
		null_ls.builtins.formatting.stylua,

		-- Python
		-- null_ls.builtins.diagnostics.mypy, -- Type analysis
		null_ls.builtins.formatting.black, -- Formatter
		-- null_ls.builtins.diagnostics.pydocstyle, -- Docstring
		null_ls.builtins.diagnostics.ruff, -- Linter

		-- Markdown
		null_ls.builtins.formatting.mdformat,
	},

	-- you can reuse a shared lspconfig on_attach callback here
	on_attach = function(client, bufnr)
		if client.supports_method("textDocument/formatting") then
			vim.api.nvim_clear_autocmds({ group = augroup, buffer = bufnr })
			vim.api.nvim_create_autocmd("BufWritePre", {
				group = augroup,
				buffer = bufnr,
				callback = function()
					vim.lsp.buf.format({ async = false })
				end,
			})
		end
	end,
})
