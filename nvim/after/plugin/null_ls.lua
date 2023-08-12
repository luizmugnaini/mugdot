local null_ls = require("null-ls")
local augroup = vim.api.nvim_create_augroup("LspFormatting", {})

null_ls.setup({
	sources = {
		-- Lua
		null_ls.builtins.formatting.stylua,

		-- Python
		-- null_ls.builtins.diagnostics.mypy, -- Type analysis (I'm trying pyre-check right now)
		null_ls.builtins.formatting.black, -- Formatter
		null_ls.builtins.diagnostics.ruff, -- Linter

		-- Markdown
		null_ls.builtins.formatting.mdformat,

		-- JS & TS
		null_ls.builtins.formatting.prettier,
		null_ls.builtins.code_actions.eslint_d,

		-- C++
		null_ls.builtins.diagnostics.clang_check,
		null_ls.builtins.formatting.clang_format,
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
