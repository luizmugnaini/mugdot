-- ----------------------------------------------------------------------------
-- LuaSnip
-- ----------------------------------------------------------------------------

local ls = require("luasnip")

require("luasnip.loaders.from_lua").lazy_load({ paths = "~/.config/mugdot/nvim/LuaSnip/" })

ls.config.setup({
	-- Enable autotriggered snippets
	enable_autosnippets = true,
})

vim.keymap.set({ "i" }, "<C-e>", function()
	ls.expand()
end, { silent = true, desc = "Expand snippet" })

-- ----------------------------------------------------------------------------
-- nvim-cmp
-- ----------------------------------------------------------------------------

local cmp = require("cmp")

cmp.setup({
	snippet = {
		expand = function(args)
			vim.snippet.expand(args.body)
		end,
	},
	mapping = cmp.mapping.preset.insert({
		["<C-y>"] = cmp.mapping.confirm({ select = true }),
		["<C-n>"] = cmp.mapping.select_next_item(),
		["<C-p>"] = cmp.mapping.select_prev_item(),
	}),
	sources = {
		{ name = "luasnip" },
		{ name = "nvim_lsp" },
		{ name = "buffer" },
	},
})

-- Latex completion
cmp.setup.filetype("tex", {
	sources = {
		{ name = "luasnip" },
		{ name = "buffer" },
	},
})
