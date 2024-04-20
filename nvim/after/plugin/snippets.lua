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
	sources = {
		{ name = "luasnip", priority = 40 },
		{ name = "nvim_lsp", priority = 30 },
		{ name = "buffer", priority = 20 },
		{ name = "path", priority = 10 },
	},

	mapping = {
		-- Confirm completion
		["<C-y>"] = cmp.mapping.confirm({ select = true }),

		-- Next
		["<C-n>"] = cmp.mapping(function(fallback)
			if cmp.visible() then
				cmp.select_next_item()
			elseif ls.expand_or_jumpable() then
				ls.expand_or_jump()
			elseif cmp.has_words_before() then
				cmp.complete()
			else
				fallback()
			end
		end, { "i", "s" }),

		-- Previous
		["<C-p>"] = cmp.mapping(function(fallback)
			if cmp.visible() then
				cmp.select_prev_item()
			elseif ls.jumpable(-1) then
				ls.jump(-1)
			else
				fallback()
			end
		end, { "i", "s" }),
	},
	snippet = {
		expand = function(args)
			require("luasnip").lsp_expand(args.body)
		end,
	},
})
