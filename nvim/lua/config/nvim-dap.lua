local wk = require("which-key")
local dap = require("dap")

local maps = {
	["<leader>"] = {
		D = {
			name = "Debug",
			b = { dap.toggle_breakpoint, "Toggle breakpoint" },
			c = { dap.continue, "Continue" },
			i = { dap.step_into, "Step into" },
			l = { dap.list_breakpoints, "List breakpoints" },
			o = { dap.step_over, "Step over" },
			O = { dap.step_out, "Step out" },
			r = { dap.repl.toggle, "Toggle REPL" },
			x = { dap.terminate, "Stop" },
		},
	},
}
wk.register(maps, { silent = true, noremap = true })

dap.adapters.go = {
	type = "executable",
	command = "node",
	args = { os.getenv("HOME") .. "/dev/vscode-go/dist/debugAdapter.js" },
}
dap.configurations.go = {
	{
		type = "go",
		name = "Debug",
		request = "launch",
		showLog = false,
		program = "${file}",
		dlvToolPath = vim.fn.exepath("dlv"),
	},
}
