local ls = require("luasnip")
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node

return {
	s({ trig = "doc" }, t({ "/**", " * @brief " }), i(1), t(" */")),
}
