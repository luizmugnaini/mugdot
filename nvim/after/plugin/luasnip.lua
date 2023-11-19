local ls = require("luasnip")

require("luasnip.loaders.from_lua").lazy_load({ paths = "~/.config/nvim/LuaSnip/" })

ls.config.setup({
	-- Enable autotriggered snippets
	enable_autosnippets = true,
})
