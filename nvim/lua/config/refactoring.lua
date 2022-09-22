local refactor = require("refactoring")
local wk = require("which-key")

refactor.setup({
	prompt_func_return_type = {
		go = false,
		java = false,

		cpp = false,
		c = false,
		h = false,
		hpp = false,
		cxx = false,
	},
	prompt_func_param_type = {
		go = false,
		java = false,

		cpp = false,
		c = false,
		h = false,
		hpp = false,
		cxx = false,
	},
	printf_statements = {},
	print_var_statements = {},
})

wk.register({
	["<leader>"] = {
		r = {
			name = "Refactor",
			e = {
				name = "Extract",
				f = { [[ <Esc><Cmd>lua require('refactoring').refactor('Extract Function')<CR>]], "Function" },
				F = {
					[[ <Esc><Cmd>lua require('refactoring').refactor('Extract Function To File')<CR>]],
					"Function to file",
				},
				v = { [[ <Esc><Cmd>lua require('refactoring').refactor('Extract Variable')<CR>]], "Variable" },
			},
			i = {
				name = "Inline",
				v = { [[ <Esc><Cmd>lua require('refactoring').refactor('Inline Variable')<CR>]], "Variable" },
			},
		},
	},
}, {
	mode = "v",
	silent = true,
	noremap = true,
})

wk.register({
	["<leader>"] = {
		r = {
			name = "Refactor",
			e = {
				name = "Extract",
				b = { [[ <Cmd>lua require('refactoring').refactor('Extract Block')<CR>]], "Block" },
				B = { [[ <Cmd>lua require('refactoring').refactor('Extract Block To File')<CR>]], "Block To File" },
			},
			i = {
				name = "Inline",
				v = { [[ <Cmd>lua require('refactoring').refactor('Inline Variable')<CR>]], "Variable" },
			},
		},
	},
}, {
	mode = "n",
	silent = true,
	noremap = true,
})
