local function set_colorscheme(color)
	if color == "kanagawa" then
		require("kanagawa").setup({
			undercurl = false,
			transparent = true,
			theme = "wave",
		})
	elseif color == "tokyonight" then
		require("tokyonight").setup({
			style = "night",
			transparent = vim.g.transparent_enable,
		})
	elseif color == "gruvbox-material" then
		vim.g.gruvbox_material_better_performance = 1
		vim.g.gruvbox_material_background = "hard"
	end
	vim.cmd.colorscheme(color)
end

set_colorscheme("gruvbox-material")
