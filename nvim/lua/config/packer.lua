local packer = require("packer")

local plugins = packer.startup({
	function(use)
		-- Packer
		use("wbthomason/packer.nvim")

		-- Lisp
		use 'Olical/aniseed'
		use 'Olical/conjure'
		use 'PaterJason/cmp-conjure'

		-- Completion
		use({
			"hrsh7th/nvim-cmp",
			requires = {
				"hrsh7th/cmp-nvim-lsp",
				"hrsh7th/cmp-buffer",
				"hrsh7th/cmp-path",
				"hrsh7th/cmp-cmdline",
				"hrsh7th/cmp-vsnip",
				"hrsh7th/vim-vsnip",
			},
			config = [[require('config.nvim-cmp')]],
		})

		-- Colorscheme
		-- use("eddyekofo94/gruvbox-flat.nvim")
		use({ "EdenEast/nightfox.nvim", commit = "bb70a64" })

		use({
			"feline-nvim/feline.nvim",
			commit = "6d4e3f9",
			config = [[require('config.feline')]],
		})

		-- Comments
		use({
			"numToStr/Comment.nvim",
			config = [[require('config.comment')]],
		})

		-- DAP
		use({ "mfussenegger/nvim-dap", config = [[require('config.nvim-dap')]] })

		-- File tree
		use({
			"kyazdani42/nvim-tree.lua",
			requires = {
				"kyazdani42/nvim-web-devicons", -- optional, for file icons
			},
			tag = "nightly", -- optional, updated every week. (see issue #1193)
			config = [[require('config.nvim-tree')]],
		})

		-- Git
		use({
			"TimUntersberger/neogit",
			commit = "1843330",
			requires = "nvim-lua/plenary.nvim",
			config = [[require('config.neogit')]],
		})
		use({
			"sindrets/diffview.nvim",
			requires = "nvim-lua/plenary.nvim",
			commit = "b31fafb",
		})
		use({
			"lewis6991/gitsigns.nvim",
			tag = "v0.5",
			config = [[require('config.gitsigns')]],
		})

		-- Keybinds
		use({ "folke/which-key.nvim", config = [[require('config.which-key')]] })

		-- LSP
		use({
			"neovim/nvim-lspconfig",
			requires = {
				{ "simrat39/rust-tools.nvim", commit = "86a2b4e" },
			},
			config = [[require('config.lsp')]],
		})

		-- Null-ls
		use({
			"jose-elias-alvarez/null-ls.nvim",
			config = [[require('config.null-ls')]],
		})

		-- Outline
		use({ "simrat39/symbols-outline.nvim", config = [[require('config.symbols-outline')]] })

		-- Refactoring
		use({
			"ThePrimeagen/refactoring.nvim",
			requires = {
				{ "nvim-lua/plenary.nvim" },
				{ "nvim-treesitter/nvim-treesitter" },
			},
			commit = "c9ca8e3",
			config = [[require('config.refactoring')]],
		})

		-- Telescope
		use({
			"nvim-telescope/telescope.nvim",
			branch = "0.1.x",
			requires = { "nvim-lua/plenary.nvim" },
			config = [[require('config.telescope')]],
		})

		-- Tests
		use({
			"nvim-neotest/neotest",
			requires = {
				"nvim-lua/plenary.nvim",
				"nvim-treesitter/nvim-treesitter",
				"antoinemadec/FixCursorHold.nvim",
				"nvim-neotest/neotest-go",
				"nvim-neotest/neotest-python",
				{ "rouge8/neotest-rust", commit = "84171da" },
			},
			config = [[require('config.neotest')]],
			tag = "v1.37.4",
		})

		-- Treesitter
		use({
			"nvim-treesitter/nvim-treesitter",
			commit = "5891e2e",
			config = [[require('config.nvim-treesitter')]],
		})
		use({
			'nvim-treesitter/playground',
			commit = "1290fdf",
		})

		-- dependencies outside of Packer
		--   brew install lua-language-server
	end,
})

return plugins
