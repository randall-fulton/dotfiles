-- https://github.com/folke/which-key.nvim

local wk = require('which-key')

local nmaps = {
	['<leader>'] = {
		c = {
			name = 'Config',
			e = { ':e ~/.config/nvim/init.lua<CR>', 'Edit' },
			s = { ':PackerSync<CR>', 'Packer Sync' },
		}
	},
	['<C-h>'] = { '<C-w>h', 'Window left' },
	['<C-l>'] = { '<C-w>l', 'Window right' },
	['<C-j>'] = { '<C-w>j', 'Window down' },
	['<C-k>'] = { '<C-w>k', 'Window up' },
}
local vmaps = {}

wk.register(nmaps, { mode = 'n', silent = true, noremap = true })
wk.register(vmaps, { mode = 'v', silent = true, noremap = true })
