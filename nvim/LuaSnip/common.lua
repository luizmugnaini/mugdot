-- -----------------------------------------------------------------------------
-- Snippets for both C and C++.
-- -----------------------------------------------------------------------------

local ls = require("luasnip")
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node

return {
	s({
		trig = "cmt",
		desc = "Comment separator",
	}, {
		t({ "// -----------------------------------------------------------------------------", "// - " }),
		i(1),
		t({ " -", "// -----------------------------------------------------------------------------" }),
	}),
	s({ trig = "once", desc = "Pragma once preprocessor" }, { t("#pragma once"), i(1) }),
	s({ trig = "inc", desc = "Include preprocessor" }, { t("#include "), i(1) }),
	s({ trig = "main", desc = "Standard main function" }, {
		t({ "int main() {", "\treturn 0;", "}" }),
	}),
	s({ trig = "header", desc = "Header with GPL v2.0" }, {
		t("///                            "),
		i(1), -- Library name here.
		t({
			"",
			"///    Copyright (C) 2024 Luiz Gustavo Mugnaini Anselmo",
			"///",
			"///    This program is free software; you can redistribute it and/or modify",
			"///    it under the terms of the GNU General Public License as published by",
			"///    the Free Software Foundation; either version 2 of the License, or",
			"///    (at your option) any later version.",
			"///",
			"///    This program is distributed in the hope that it will be useful,",
			"///    but WITHOUT ANY WARRANTY; without even the implied warranty of",
			"///    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the",
			"///    GNU General Public License for more details.",
			"///",
			"///    You should have received a copy of the GNU General Public License along",
			"///    with this program; if not, write to the Free Software Foundation, Inc.,",
			"///    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.",
			"///",
			"/// Description:",
			"/// Author: Luiz G. Mugnaini A. <luizmuganini@gmail.com>",
			"",
			"#pragma once",
		}),
	}),
}
