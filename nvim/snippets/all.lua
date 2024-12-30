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
        t({ "// -------------------------------------------------------------------------------------------------", "// " }),
        i(1),
        t({ "", "// -------------------------------------------------------------------------------------------------" }),
    }),
    s({ trig = "inc", desc = "Include preprocessor" }, { t("#include "), i(1) }),
}
