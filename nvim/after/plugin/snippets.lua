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
			require("luasnip").lsp_expand(args.body)
		end,
	},
	mapping = cmp.mapping.preset.insert({
		["<C-y>"] = cmp.mapping.confirm({ select = true }),
		["<C-n>"] = cmp.mapping.select_next_item(),
		["<C-p>"] = cmp.mapping.select_prev_item(),
	}),
	sources = {
		{ name = "luasnip", priority = 20 },
		{ name = "nvim_lsp", priority = 10 },
	},
})

-- Latex completion
cmp.setup.filetype("tex", {
	sources = cmp.config.sources({
		{ name = "luasnip" },
	}, {
		{ name = "buffer" },
	}),
})
