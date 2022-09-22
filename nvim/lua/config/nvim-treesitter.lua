local treesitter = require("nvim-treesitter.configs")

treesitter.setup({
	ident = {
		enable = true,
	},
})

vim.api.nvim_set_option("foldmethod", "expr")
vim.api.nvim_set_option("foldexpr", "nvim_treesitter#foldexpr()")
