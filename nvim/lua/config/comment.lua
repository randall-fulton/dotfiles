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
	["<C-/>"] = { "<Plug>(comment_toggle_linewise)<CR>", "Toggle comment" },
	-- ["<C-_>"] = { "<Plug>(comment_toggle_linewise)<CR>", "Toggle comment" }, -- this is actually <C-/>
}
local vmaps = {
	["<C-/>"] = { "<Plug>(comment_toggle_linewise_visual)<CR>", "Toggle comment" },
	-- ["<C-_>"] = { "<Plug>(comment_toggle_linewise_visual)<CR>", "Toggle comment" }, -- this is actually <C-/>
}
wk.register(nmaps, { silent = true, noremap = true })
wk.register(vmaps, { mode = "v", silent = true, noremap = true })
