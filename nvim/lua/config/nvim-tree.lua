local tree = require('nvim-tree')
local wk = require('which-key')

tree.setup({
	sort_by = 'case_insensitive',
	filters = {
		dotfiles = false,
	},
})

wk.register({
	['<leader>if'] = { ':NvimTreeToggle .<CR>', 'Files' }
}, { silent = true, noremap = true })
