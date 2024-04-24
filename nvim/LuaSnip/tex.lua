local ls = require("luasnip")
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node
local f = ls.function_node
local extras = require("luasnip.extras")
local postfix = require("luasnip.extras.postfix").postfix

local tex_utils = {}

return {
	-- Portuguese accents
	s({ trig = ".a", snippetType = "autosnippet" }, t("ã")),
	s({ trig = ";a", snippetType = "autosnippet" }, t("á")),
	s({ trig = "/a", snippetType = "autosnippet" }, t("â")),
	s({ trig = ",a", snippetType = "autosnippet" }, t("à")),
	s({ trig = ".A", snippetType = "autosnippet" }, t("Ã")),
	s({ trig = ";A", snippetType = "autosnippet" }, t("Á")),
	s({ trig = "/A", snippetType = "autosnippet" }, t("Â")),
	s({ trig = ",A", snippetType = "autosnippet" }, t("À")),
	s({ trig = ".o", snippetType = "autosnippet" }, t("õ")),
	s({ trig = ";o", snippetType = "autosnippet" }, t("ó")),
	s({ trig = "/o", snippetType = "autosnippet" }, t("ô")),
	s({ trig = ".O", snippetType = "autosnippet" }, t("Õ")),
	s({ trig = ";O", snippetType = "autosnippet" }, t("Ó")),
	s({ trig = "/O", snippetType = "autosnippet" }, t("Ô")),
	s({ trig = ";e", snippetType = "autosnippet" }, t("é")),
	s({ trig = "/e", snippetType = "autosnippet" }, t("ê")),
	s({ trig = ";E", snippetType = "autosnippet" }, t("É")),
	s({ trig = "/E", snippetType = "autosnippet" }, t("Ê")),
	s({ trig = ";i", snippetType = "autosnippet" }, t("í")),
	s({ trig = ";I", snippetType = "autosnippet" }, t("Í")),
	s({ trig = ";u", snippetType = "autosnippet" }, t("ú")),
	s({ trig = ";U", snippetType = "autosnippet" }, t("Ú")),
	s({ trig = ";c", snippetType = "autosnippet" }, t("ç")),

	-- Math
	s({
		trig = "mk",
		desc = "inline math",
		snippetType = "autosnippet",
	}, { t("\\("), i(1), t("\\)") }),
	s({
		trig = ";mk",
		desc = "inline math",
		snippetType = "autosnippet",
	}, { t({ "\\[", "\t" }), i(1), t({ "", "\\]" }) }),

	-- Symbols
	s({
		trig = "->",
		snippetType = "autosnippet",
		desc = "arrow",
	}, t("\\to"), {}),
	s({
		trig = "-->",
		snippetType = "autosnippet",
		desc = "long arrow",
	}, t("\\longrightarrow"), {}),
	s({ trig = "<=", snippetType = "autosnippet" }, t("\\leq"), {}),
	s({ trig = ">=", snippetType = "autosnippet" }, t("\\geq"), {}),
	s({ trig = ":=", snippetType = "autosnippet" }, t("\\coloneq"), {}),
	s({ trig = "**" }, t("\\times"), {}),
	s({
		trig = "//",
		snippetType = "autosnippet",
		desc = "fraction",
	}, { t("\\frac{"), i(1), t("}{"), i(2), t("}") }, {}),

	postfix(
		{
			trig = ".inv",
			snippetType = "autosnippet",
			desc = "inverse",
		},
		f(function(_, parent)
			return parent.env.POSTFIX_MATCH .. "^{-1}"
		end)
	),

	s({
		trig = ".set",
		snippetType = "autosnippet",
	}, { t("\\{"), i(1), t("\\}") }, {}),

	s({
		trig = ".tt",
		desc = "in-math text",
	}, { t("\\text{"), i(1), t("}") }, {}),

	s({ trig = "cases" }, {
		t({ "\\begin{cases}", "\t" }),
		i(1),
		t("\\end{cases}"),
	}, {}),

	-- Environments
	s({
		trig = ".beg",
		desc = "generic begin env",
		snippetType = "autosnippet",
	}, {
		t({ "\\begin{" }),
		i(1),
		t({ "}", "\t" }),
		i(2),
		t({ "", "\\end{" }),
		extras.rep(1),
		t("}"),
	}),
	s({
		trig = ".ali",
		desc = "enumerate env",
		snippetType = "autosnippet",
	}, {
		t({ "\\begin{enumerate}[(a)]\\setlength\\itemsep{0em}", "\t\\item" }),
		i(1),
		t({ "", "\\end{enumerate}" }),
	}),
	s({
		trig = ".enum",
		desc = "enumerate env",
		snippetType = "autosnippet",
	}, {
		t({ "\\begin{enumerate}[(a)]\\setlength\\itemsep{0em}", "\t\\item" }),
		i(1),
		t({ "", "\\end{enumerate}" }),
	}),
	s({
		trig = ".item",
		desc = "itemize env",
		snippetType = "autosnippet",
	}, {
		t({ "\\begin{itemize}", "\t\\item" }),
		i(1),
		t({ "", "\\end{itemize}" }),
	}),
	s({
		trig = ".diag",
		dev = "tikzcd diagram env",
		snippetType = "autosnippet",
	}, {
		t({ "\\begin{tikzcd}", "\t" }),
		i(1),
		t({ "", "\\end{tikzcd}" }),
	}),
}
