function Theme(color)
	color = color or "gruvbox-material"
	vim.cmd.colorscheme(color)
end

Theme()
