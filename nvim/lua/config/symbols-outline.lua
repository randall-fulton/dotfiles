local outline = require("symbols-outline")
local wk = require("which-key")

wk.register({
	["<leader>"] = {
		i = {
			name = "Interface",
			o = { ":SymbolsOutline<CR>", "Outline" },
		},
	},
}, {
	silent = true,
	noremap = true,
})

outline.setup()
