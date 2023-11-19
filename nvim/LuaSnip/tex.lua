local ls = require("luasnip")
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node
local f = ls.function_node
local extras = require("luasnip.extras")
local postfix = require("luasnip.extras.postfix").postfix

local tex_utils = {}
tex_utils.in_mathzone = function() -- math context detection
	return vim.fn["vimtex#syntax#in_mathzone"]() == 1
end
tex_utils.in_text = function()
	return not tex_utils.in_mathzone()
end
tex_utils.in_comment = function() -- comment detection
	return vim.fn["vimtex#syntax#in_comment"]() == 1
end
tex_utils.in_env = function(name) -- generic environment detection
	local is_inside = vim.fn["vimtex#env#is_inside"](name)
	return (is_inside[1] > 0 and is_inside[2] > 0)
end
-- A few concrete environments---adapt as needed
tex_utils.in_equation = function() -- equation environment detection
	return tex_utils.in_env("equation")
end
tex_utils.in_itemize = function() -- itemize environment detection
	return tex_utils.in_env("itemize")
end
tex_utils.in_tikz = function() -- TikZ picture environment detection
	return tex_utils.in_env("tikzpicture")
end

return {
	-- Portuguese accents
	s({ trig = ".a", condition = tex_utils.in_text, snippetType = "autosnippet" }, t("ã")),
	s({ trig = ";a", condition = tex_utils.in_text, snippetType = "autosnippet" }, t("á")),
	s({ trig = "/a", condition = tex_utils.in_text, snippetType = "autosnippet" }, t("â")),
	s({ trig = ",a", condition = tex_utils.in_text, snippetType = "autosnippet" }, t("à")),
	s({ trig = ".A", condition = tex_utils.in_text, snippetType = "autosnippet" }, t("Ã")),
	s({ trig = ";A", condition = tex_utils.in_text, snippetType = "autosnippet" }, t("Á")),
	s({ trig = "/A", condition = tex_utils.in_text, snippetType = "autosnippet" }, t("Â")),
	s({ trig = ",A", condition = tex_utils.in_text, snippetType = "autosnippet" }, t("À")),
	s({ trig = ".o", condition = tex_utils.in_text, snippetType = "autosnippet" }, t("õ")),
	s({ trig = ";o", condition = tex_utils.in_text, snippetType = "autosnippet" }, t("ó")),
	s({ trig = "/o", condition = tex_utils.in_text, snippetType = "autosnippet" }, t("ô")),
	s({ trig = ".O", condition = tex_utils.in_text, snippetType = "autosnippet" }, t("Õ")),
	s({ trig = ";O", condition = tex_utils.in_text, snippetType = "autosnippet" }, t("Ó")),
	s({ trig = "/O", condition = tex_utils.in_text, snippetType = "autosnippet" }, t("Ô")),
	s({ trig = ";e", condition = tex_utils.in_text, snippetType = "autosnippet" }, t("é")),
	s({ trig = "/e", condition = tex_utils.in_text, snippetType = "autosnippet" }, t("ê")),
	s({ trig = ";E", condition = tex_utils.in_text, snippetType = "autosnippet" }, t("É")),
	s({ trig = "/E", condition = tex_utils.in_text, snippetType = "autosnippet" }, t("Ê")),
	s({ trig = ";i", condition = tex_utils.in_text, snippetType = "autosnippet" }, t("í")),
	s({ trig = ";I", condition = tex_utils.in_text, snippetType = "autosnippet" }, t("Í")),
	s({ trig = ";u", condition = tex_utils.in_text, snippetType = "autosnippet" }, t("ú")),
	s({ trig = ";U", condition = tex_utils.in_text, snippetType = "autosnippet" }, t("Ú")),
	s({ trig = ";c", condition = tex_utils.in_text, snippetType = "autosnippet" }, t("ç")),

	-- Math
	s({
		trig = "mk",
		desc = "inline math",
		snippetType = "autosnippet",
		condition = tex_utils.in_text,
	}, { t("\\("), i(1), t("\\)") }),

	s({
		trig = "math",
		desc = "inline math",
		condition = tex_utils.in_text,
	}, { t({ "\\[", "\t" }), i(1), t({ "", "\\]" }) }),

	postfix(
		{
			trig = ".inv",
			condition = tex_utils.in_mathzone,
			snippetType = "autosnippet",
			desc = "inverse",
		},
		f(function(_, parent)
			return parent.env.POSTFIX_MATCH .. "^{-1}"
		end)
	),

	s({
		trig = "set",
		condition = tex_utils.in_mathzone,
		snippetType = "autosnippet",
	}, { t("\\{"), i(1), t("\\}") }, {}),

	s({
		trig = "tt",
		condition = tex_utils.in_mathzone,
		desc = "in-math text",
	}, { t("\\text{"), i(1), t("}") }, {}),

	s({
		trig = "->",
		condition = tex_utils.in_mathzone,
		snippetType = "autosnippet",
		desc = "arrow",
	}, t("\\to"), {}),

	s({
		trig = "-->",
		condition = tex_utils.in_mathzone,
		snippetType = "autosnippet",
		desc = "long arrow",
	}, t("\\longrightarrow"), {}),

	s({ trig = "<=", condition = tex_utils.in_mathzone, snippetType = "autosnippet" }, t("\\leq"), {}),

	s({ trig = ">=", condition = tex_utils.in_mathzone, snippetType = "autosnippet" }, t("\\geq"), {}),

	s({ trig = ":=", condition = tex_utils.in_mathzone, snippetType = "autosnippet" }, t("\\coloneq"), {}),

	s({ trig = "**", condition = tex_utils.in_mathzone }, t("\\times"), {}),

	s(
		{ trig = "//", condition = tex_utils.in_mathzone, snippetType = "autosnippet", desc = "fraction" },
		{ t("\\frac{"), i(1), t("}{"), i(2), t("}") },
		{}
	),
	s({ trig = "cases", condition = tex_utils.in_mathzone }, {
		t({ "\\begin{cases}", "\t" }),
		i(1),
		t("\\end{cases}"),
	}, {}),

	-- Environments
	s({ trig = "enum", condition = tex_utils.in_text, desc = "enumerate env" }, {
		t({ "\\begin{enumerate}[(a)]\\setlength\\itemsep{0em}", "\t\\item" }),
		i(1),
		t({ "", "\\end{enumerate}" }),
	}),

	s({ trig = "item", condition = tex_utils.in_text, desc = "itemize env" }, {
		t({ "\\begin{itemize}", "\t\\item" }),
		i(1),
		t({ "", "\\end{itemize}" }),
	}),

	s({ trig = "beg", condition = tex_utils.in_text, dev = "generic begin env" }, {
		t({ "\\begin{" }),
		i(1),
		t({ "}", "\t" }),
		i(2),
		t({ "", "\\end{" }),
		extras.rep(1),
		t("}"),
	}),

	s({ trig = "diag", condition = tex_utils.in_mathzone, dev = "tikzcd diagram env" }, {
		t({ "\\begin{tikzcd}", "\t" }),
		i(1),
		t({ "", "\\end{tikzcd}" }),
	}),
}
