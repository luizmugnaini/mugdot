local function set_colorscheme(color)
	if color == "kanagawa" then
		require("kanagawa").setup({
			compile = true,
			undercurl = false,
			commentStyle = { italic = false },
			keywordStyle = { italic = false },
			statementStyle = { bold = true },
			theme = "dragon",
			background = {
				dark = "dragon",
				light = "lotus",
			},
			-- Disable italic for `self`, `this`, etc.
			overrides = function()
				return {
					["@variable.builtin"] = { italic = false },
				}
			end,
		})
	elseif color == "modus" then
		require("modus-themes").setup({
			style = "modus_vivendi",
			variant = "deuteranopia",
		})
	elseif color == "gruvbox-material" then
		vim.g.gruvbox_material_better_performance = 1
		vim.g.gruvbox_material_foreground = "material"
		vim.g.gruvbox_material_background = "hard"
		vim.g.gruvbox_material_disable_italic_comment = 1
	end
	vim.cmd.colorscheme(color)
end

set_colorscheme("modus")
vim.cmd.highlight("clear SignColumn")
