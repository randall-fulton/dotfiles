local null_ls = require("null-ls")

local diagnostics = null_ls.builtins.diagnostics
local formatting = null_ls.builtins.formatting

null_ls.setup({
	sources = {
		-- go
		formatting.gofmt,
		formatting.goimports,
		diagnostics.golangci_lint,

		-- python
		formatting.black.with({
			extra_args = { "--fast" },
		}),

		-- rust
		formatting.rustfmt,

		-- lua
		formatting.stylua,
	},
})
