local lint = require('lint')

lint.linters.black = {
	cmd = 'black',
	stdin = false,
}

lint.linters_by_ft = {
	python = { 'black' },
}

vim.api.nvim_create_autocmd({ "BufWritePost" }, {
	callback = function()
		require('lint').try_lint()
	end
})
