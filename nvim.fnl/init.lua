local hotpot_path = vim.fn.stdpath('data') .. '/site/pack/paqs/start/hotpot.nvim'
if vim.fn.empty(vim.fn.glob(hotpot_path)) > 0 then
  print("Could not find hotpot.nvim, cloning new copy to", hotpot_path)
  vim.fn.system({'git', 'clone',
                 'https://github.com/rktjmp/hotpot.nvim', hotpot_path})
  vim.cmd("helptags " .. hotpot_path .. "/doc")
end

-- Enable fnl/ support
require("hotpot")

-- Now you can load fennel code, so you could put the rest of your
-- config in a separate `~/.config/nvim/fnl/my_config.fnl` or
-- `~/.config/nvim/fnl/plugins.fnl`, etc.
local config = require("config")
config()


-- local install_path = vim.fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
-- if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
-- 	print("Downloading Packer ...")
-- 	vim.fn.system({
-- 		"git",
-- 		"clone",
-- 		"https://github.com/wbthomason/packer.nvim",
-- 		install_path,
-- 	})
-- 	vim.api.nvim_command("packadd packer.nvim")
-- 	require("packer").sync()
-- end

require("packer").startup({
	function(use)
		use "folke/which-key.nvim"
	end,
})

-- Colorscheme
-- vim.cmd("colorscheme gruvbox-flat")
-- vim.cmd("colorscheme dayfox")

-- Leader keys
vim.api.nvim_set_keymap("n", "<SPC>", "", {})
vim.g.mapleader = " "

-- Basic UI tweaks
vim.wo.relativenumber = true
vim.o.cursorline = true
vim.o.termguicolors = true
vim.o.signcolumn = "yes:1"
vim.o.scrolloff = 10

-- Controls when which-key triggers
vim.o.timeoutlen = 500

-- GUI
vim.o.guifont = "Hack_Nerd_Font_Mono:h16"

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

vim.api.nvim_create_user_command("Config", "exe 'e '.stdpath('config').'/init.lua'", {})
