vim.api.nvim_create_autocmd("BufEnter", {
	desc = "Detect GLSL files",
	pattern = { "*.glsl", "*.vert", "*.tesc", "*.tese", "*.geom", "*.frag", "*.comp" },
	command = "set filetype=glsl",
})
