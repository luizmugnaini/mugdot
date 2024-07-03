local ft = require("guard.filetype")

ft("c,cpp,glsl"):fmt("clang-format")
ft("lua"):fmt("stylua")
ft("python"):fmt("black")

require("guard").setup({
	fmt_on_save = true,
})
