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

		-- Go
		"gopls",

		-- Rust
		"rust_analyzer",

		-- Lua
		"luaformatter",
		"lua_ls",

		-- WGSL (WebGL)
		"wgsl_analyzer",
	},
})
