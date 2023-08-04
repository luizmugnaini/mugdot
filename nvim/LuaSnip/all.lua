local ls = require("luasnip")
local s = ls.snippet
local t = ls.text_node
--[[
".a"
".A" "Ã"
"/a" "â"
"/A" "Â"
";a" "á"
";A" "Á"
",a" "à"
",A" "À"

".o" "õ"
".O" "Õ"
"/o" "ô"
"/O" "Ô"
";o" "ó"
";O" "Ó"

";e" "é"
";E" "É"
"/e" "ê"
"/E" "Ê"

";c" "ç"
";i" "í"
";I" "Í"
";u" "ú"
";U" "Ú"
]]
return {
	--
	s({ trig = ";a" }, { t("ã"), t("á"), t("â"), t("à") }),
	s({ trig = ";A" }, { t("Ã"), t("Á"), t("Â"), t("À") }),
	s({ trig = ";o" }, { t("õ"), t("ó"), t("ô") }),
	s({ trig = ";O" }, { t("Õ"), t("Ó"), t("Ô") }),
	s({ trig = ";e" }, { t("é"), t("ê") }),
	s({ trig = ";E" }, { t("É"), t("Ê") }),
	s({ trig = ";i" }, { t("í") }),
	s({ trig = ";I" }, { t("Í") }),
	s({ trig = ";u" }, { t("ú") }),
	s({ trig = ";U" }, { t("Ú") }),
	s({ trig = ";c" }, { t("ç") }),

	-- To return multiple snippets, use one `return` statement per snippet file
	-- and return a table of Lua snippets.
	s({ trig = "foo" }, { t("Another snippet.") }),
}
