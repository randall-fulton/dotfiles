local neotest = require('neotest')
local wk = require('which-key')

neotest.setup({
	adapters = {
		require('neotest-go')({
			args = { '-count=1' },
		}),
		require('neotest-python'),
		require('neotest-rust'),
	}
})

local maps = {
	['<leader>'] = {
		i = {
			t = { '<cmd>lua require("neotest").summary.toggle()<CR>', 'Tests' },
		},
		t = {
			name = 'Tests',
			d = { '<cmd>lua require("neotest").run.run({ strategy = "dap" })<CR>', 'Debug nearest' },
			f = { '<cmd>lua require("neotest").run.run(vim.fn.expand("%"))<CR>', 'File' },
			s = { '<cmd>lua require("neotest").run.stop()<CR>', 'Stop nearest' },
			t = { '<cmd>lua require("neotest").run.run()<CR>', 'Nearest' },
		}
	}
}
wk.register(maps, { silent = true, noremap = true })
