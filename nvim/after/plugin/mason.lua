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
		"luaformatter",
		"lua_ls",
	},
})
