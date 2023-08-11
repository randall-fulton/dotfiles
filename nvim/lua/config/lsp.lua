local lsp = require("lspconfig")
local rt = require("rust-tools")
local wk = require("which-key")

local function key_maps(bufnr)
	local maps = {
		["<leader>"] = {
			d = {
				name = "Diagnostics",
				d = { vim.diagnostic.open_float, "Open float" },
				l = { vim.diagnostic.setloclist, "List" },
				n = { vim.diagnostic.goto_next, "Next" },
				p = { vim.diagnostic.goto_prev, "Previous" },
			},
			l = {
				name = "LSP",
				["<C-k>"] = { vim.lsp.buf.signature_help, "Signature help" },
				a = { vim.lsp.buf.code_action, "Code action" },
				f = { vim.lsp.buf.format, "Format buffer" },
				g = {
					name = "Go to",
					d = { vim.lsp.buf.definition, "Definition" },
					D = { vim.lsp.buf.declaration, "Declaration" },
					i = { vim.lsp.buf.implementation, "Implementation" },
					r = { vim.lsp.buf.references, "References" },
					t = { vim.lsp.buf.type_definition, "Type definition" },
				},
				k = { vim.lsp.buf.hover, "Documentation" },
				r = { vim.lsp.buf.rename, "Rename" },
				w = {
					name = "Workspace",
					a = { vim.lsp.buf.add_workspace_folder, "Add folder" },
					r = { vim.lsp.buf.remove_workspace_folder, "Remove folder" },
				},
			},
		},
		K = { vim.lsp.buf.hover, "Documentation" },
	}
	wk.register(maps, { noremap = true, silent = true, buffer = bufnr })
end

local function on_attach(_, bufnr)
	vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")

	local augroup = vim.api.nvim_create_augroup("LspFormatting", {})
	vim.api.nvim_clear_autocmds({ group = augroup, buffer = bufnr })
	vim.api.nvim_create_autocmd("BufWritePre", {
		group = augroup,
		buffer = bufnr,
		callback = function()
			vim.lsp.buf.format()
		end,
	})

	key_maps(bufnr)
end

local capabilities = require("cmp_nvim_lsp").default_capabilities(vim.lsp.protocol.make_client_capabilities())

local lsp_flags = {
	-- This is the default in Nvim 0.7+
	debounce_text_changes = 150,
}
lsp["bashls"].setup({
	on_attach = on_attach,
	flags = lsp_flags,
	capabilities = capabilities,
})
lsp["fennel_language_server"].setup({
	on_attach = on_attach,
	flags = lsp_flags,
	capabilities = capabilities,
	root_dir = lsp.util.root_pattern("fnl"),
	settings = {
		fennel = {
			workspace = {
				-- If you are using hotpot.nvim or aniseed,
				-- make the server aware of neovim runtime files.
				library = vim.api.nvim_get_runtime_file("", true),
				-- library = vim.api.nvim_list_runtime_paths(),
			},
			diagnostics = {
				globals = { "vim" },
			},
		},
	},
})
lsp["gopls"].setup({
	on_attach = on_attach,
	flags = lsp_flags,
	capabilities = capabilities,
})
lsp["hls"].setup({
	on_attach = on_attach,
	flags = lsp_flags,
	capabilities = capabilities,
})
lsp["ocamllsp"].setup({
	on_attach = on_attach,
	flags = lsp_flags,
	capabilities = capabilities,
})
lsp["pyright"].setup({
	on_attach = on_attach,
	flags = lsp_flags,
	capabilities = capabilities,
	rootPatterns = {
		"setup.py",
		"setup.cfg",
		"pyproject.toml",
		"requirements.txt",
	},
})
rt.setup({
	server = {
		on_attach = on_attach,
		flags = lsp_flags,
		capabilities = capabilities,
		settings = {
			["rust-analyzer"] = {
				checkOnSave = {
					-- enable = true,
					command = "clippy",
					-- allTargets = true,
				},
			},
		},
	},
	dap = {
		adapter = {
			type = "executable",
			command = "/usr/local/Cellar/llvm/15.0.0/bin/lldb-vscode",
			name = "rt_lldb",
		},
	},
})
lsp["lua_ls"].setup({
	on_attach = on_attach,
	flags = lsp_flags,
	capabilities = capabilities,
	settings = {
		Lua = {
			diagnostics = {
				-- don't warn that "vim" is unknown symbol
				globals = { "vim" },
			},
			telemetry = {
				enable = false,
			},
			workspace = {
				library = vim.api.nvim_get_runtime_file("", true),
			},
		},
	},
})
