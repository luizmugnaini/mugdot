require("conform").setup({
	formatters_by_ft = {
		lua = { "stylua" },
		python = { "black" },
		rust = { "rustfmt" },
		cpp = { "clang_format" },
		c = { "clang_format" },
		-- TODO: html, htmldjango, and css are temporary, remove this later
		html = { "djlint" },
		htmldjango = { "djlint" },
		["_"] = { "trim_whitespace" },
	},

	format_on_save = {
		timeout_ms = 500,
		lsp_fallback = true,
	},
})

vim.api.nvim_create_autocmd("BufWritePre", {
	pattern = "*",
	callback = function(args)
		require("conform").format({ bufnr = args.buf })
	end,
})
