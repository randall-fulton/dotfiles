local install_path = vim.fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
	print("Downloading Packer ...")
	vim.fn.system({
		"git",
		"clone",
		"https://github.com/wbthomason/packer.nvim",
		install_path,
	})
	vim.api.nvim_command("packadd packer.nvim")
	require("config.packer")
	require("packer").sync()
end

require("config.packer")

-- Colorscheme
-- vim.cmd("colorscheme gruvbox-flat")
vim.cmd("colorscheme dayfox")

-- Leader keys
vim.g.mapleader = ","

-- Basic UI tweaks
vim.wo.relativenumber = true
vim.o.cursorline = true
vim.o.termguicolors = true
vim.o.signcolumn = "yes:1"
vim.o.scrolloff = 10

-- Controls when which-key triggers
vim.o.timeoutlen = 500

-- GUI
vim.o.guifont = "Hack_Nerd_Font_Mono:h12"

-- Mouse support
vim.o.mouse = "a"

-- Default formatting
vim.o.tabstop = 4
vim.o.softtabstop = 4
vim.o.shiftwidth = 4

-- Better completion experience
vim.o.completeopt = "menuone,noinsert,noselect"
vim.o.foldmethod = "expr"
vim.o.foldexpr = "nvim_treesitter#foldexpr()"
