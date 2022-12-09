-- https://github.com/numToStr/Comment.nvim
local comment = require("Comment")
local wk = require("which-key")

comment.setup({
	mappings = {
		basic = false,
		extra = false,
		extended = false,
	},
})

local nmaps = {
	["<C-/>"] = { "<Plug>(comment_toggle_linewise_current)<CR>", "Toggle comment" },
}
local vmaps = {
	["<C-/>"] = { "<Plug>(comment_toggle_linewise_visual)<CR>", "Toggle comment" },
}
wk.register(nmaps, { silent = true, noremap = true })
wk.register(vmaps, { mode = "v", silent = true, noremap = true })
