-- Muggy colorscheme
--
-- Theme based on the modus-themes vivendi-tinted colorscheme. Built using colorbuddy.nvim
--
-- Author: Luiz G. Mugnaini A. <luizmugnaini@gmail.com>

vim.opt.bg = "dark"
vim.cmd.highlight("clear SignColumn")

local colorbuddy = require("colorbuddy")

colorbuddy.colorscheme("muggy")

local Color = colorbuddy.Color
local Group = colorbuddy.Group
local colors = colorbuddy.colors
local groups = colorbuddy.groups
local styles = colorbuddy.styles

-- -----------------------------------------------------------------------------
-- Colors
-- -----------------------------------------------------------------------------

Color.new("fg", "#ffffff")
Color.new("bg", "#0d0e1c")
Color.new("greyActive", "#4a4f69")
Color.new("greyInactive", "#2b3045")
Color.new("clay", "#f1b090")
Color.new("rust", "#db7b5f")
Color.new("green", "#6ae4b9")
Color.new("blue", "#79a8ff")
Color.new("pink", "#ff66ff")
Color.new("magenta", "#b6a0ff")
Color.new("magentaLight", "#caa6df")
Color.new("red", "#ff7f9f")

-- -----------------------------------------------------------------------------
-- Standard groups
-- -----------------------------------------------------------------------------

Group.new("Normal", colors.fg, colors.bg)

Group.new("Comment", colors.clay)
Group.new("Todo", colors.fg, colors.rust, styles.none)

Group.new("Identifier", colors.fg)
Group.new("Function", colors.fg)

Group.new("Constant", colors.fg)
Group.new("String", colors.blue)
Group.new("Character", colors.blue)
Group.new("Number", colors.fg)
Group.new("Float", colors.fg)
Group.new("Boolean", colors.green)

Group.new("Type", colors.green)
Group.new("StorageClass", colors.magenta)
Group.new("Structure", colors.magenta)
Group.new("Typedef", colors.magenta)

Group.new("Special", colors.magenta)

Group.new("Statement", colors.magenta)
Group.new("Conditional", colors.magenta)
Group.new("Repeat", colors.magenta)
Group.new("Label", colors.magenta)
Group.new("Operator", colors.fg)
Group.new("Keyword", colors.magenta)

Group.new("MatchParen", colors.none, colors.pink)
Group.new("Visual", colors.none, colors.greyActive)

Group.new("Delimeter", colors.fg)

Group.new("PreProc", colors.red)
Group.new("Include", colors.red)
Group.new("Define", colors.red)
Group.new("PreCondit", colors.red)

Group.new("StatusLine", colors.fg, colors.greyActive)
Group.new("StatusLineNC", colors.fg, colors.greyInactive)

Group.new("Directory", colors.blue)

-- -----------------------------------------------------------------------------
-- Treesitter groups
-- -----------------------------------------------------------------------------

Group.new("@constant", colors.fg)
Group.new("@constant.builtin", colors.green)
Group.new("@constant.macro", colors.green)
Group.new("@macro", colors.red)
Group.new("@define", colors.red)
Group.new("@number", colors.fg)
Group.new("@float", colors.fg)
Group.new("@boolean", colors.green)
Group.new("@string", colors.blue)
Group.new("@string.escape", colors.blue)
Group.new("@string.special", colors.blue)
Group.new("@character", colors.blue)
Group.new("@character.special", colors.blue)

Group.new("@property", colors.fg)
Group.new("@field", colors.fg)

Group.new("@function", colors.magentaLight)
Group.new("@function.builtin", colors.magenta)
Group.new("@parameter", colors.fg)
Group.new("@method", colors.fg)
Group.new("@constructor", colors.fg)

Group.new("@statement", colors.magenta)
Group.new("@conditional", colors.magenta)
Group.new("@repeat", colors.magenta)
Group.new("@label", colors.magenta)
Group.new("@operator", colors.fg)
Group.new("@keyword", colors.magenta)
Group.new("@keyword.import", colors.red)
Group.new("@keyword.directive", colors.red)

Group.new("@variable", colors.fg)
Group.new("@variable.builtin", colors.magenta)
Group.new("@type", colors.green)
Group.new("@type.definition", colors.magenta)
Group.new("@type.builtin", colors.green)
Group.new("@typedef", colors.magenta)
Group.new("@structure", colors.magenta)
Group.new("@storageclass", colors.magenta)
Group.new("@namespace", colors.fg)
Group.new("@module", colors.fg)
Group.new("@include", colors.red)
Group.new("@preproc", colors.red)

Group.new("@punctuation", colors.fg)
Group.new("@punctuation.bracket", colors.fg)
Group.new("@punctuation.delimiter", colors.fg)
Group.new("@punctuation.special", colors.fg)

Group.new("@comment", colors.clay)
Group.new("@text.todo", colors.fg, colors.rust)
