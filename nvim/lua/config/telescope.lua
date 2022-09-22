local wk = require('which-key')
local telescope = require('telescope')
local actions = require('telescope.actions')
local builtin = require('telescope.builtin')

telescope.setup {
	defaults = {
		mappings = {
			i = {
				['<Esc>'] = actions.close,
				['<C-j>'] = actions.move_selection_next,
				['<C-k>'] = actions.move_selection_previous,
			}
		}
	}
}

local maps = {
	['<C-p'] = { builtin.find_files, 'Files' },
	['<leader>'] = {
		f = {
			name = 'Telescope',
			b = { builtin.buffers, 'Buffers' },
			f = { builtin.find_files, 'Files' },
			g = { builtin.live_grep, 'Grep' },
		}
	}
}
wk.register(maps, { mode = 'n', noremap = true, silent = true })
