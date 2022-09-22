local neogit = require("neogit")
local wk = require("which-key")

local maps = {
	["<leader>"] = {
		g = {
			name = "Git",
			g = { neogit.open, "Open Neogit" },
		},
	},
}
wk.register(maps, { silent = true, noremap = true })

neogit.setup({
	integrations = {
		diffview = true,
	},
})
