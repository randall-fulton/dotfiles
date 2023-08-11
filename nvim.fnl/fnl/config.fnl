(fn bootstrap-packer []
  (let [path (.. (vim.fn.stdpath "data")
                 "/site/pack/packer/start/packer.nvim")]
    (when (> (vim.fn.empty (vim.fn.glob path)) 0)
      (print "Downloading Packer...")
      (vim.fn.system ["git"
                      "clone"
                      "https://github.com/wbthomason/packer.nvim"
                      path])
      (vim.api.nvim_command "packadd packer.nvim")
      (let [packer (require :packer)]
        (table.view packer)))))

(fn install-packages [use]
  (use {1 "nvim-neotest/neotest"
        :requires ["nvim-lua/plenary.nvim"
                   "nvim-treesitter/nvim-treesitter"
                   "antoinemadec/FixCursorHold.nvim"
                   "nvim-neotest/neotest-go"
                   "nvim-neotest/neotest-python"
                   {1 "rouge8/neotest-rust" :commit "84171da" }]
        :config (fn [] (require :custom.neotest))
        :tag "v1.37.4"})
  (use {1 "numToStr/Comment.nvim"
        :config (fn [] (require :custom.comments))})
  (use "wbthomason/packer.nvim")
  (use {1 "nvim-treesitter/nvim-treesitter" :commit "5891e2e"})
  (use {1 "nvim-treesitter/playground"
        :commit "1290fdf"})
  (use {1 "neovim/nvim-lspconfig"
        :config (fn [] (require :custom.lsp))})
  (use {1 "hrsh7th/nvim-cmp"
 		:requires ["hrsh7th/cmp-nvim-lsp"
                   "hrsh7th/cmp-buffer"
                   "hrsh7th/cmp-path"
                   "hrsh7th/cmp-cmdline"
                   "hrsh7th/cmp-vsnip"
                   "hrsh7th/vim-vsnip"]
 		:config (fn [] (require :custom.cmp))})
  (use "folke/which-key.nvim")
  (use "atweiden/vim-fennel")
  (use {1 "EdenEast/nightfox.nvim" :commit "bb70a64"})
  (use "Olical/conjure")
  (use {1 "nvim-telescope/telescope.nvim"
        :branch "0.1.x"
        :requires "nvim-lua/plenary.nvim"
        :config (fn [] (require :custom.telescope))})
  (use {1 "TimUntersberger/neogit"
        :commit "1843330"
        :requires "nvim-lua/plenary.nvim"
        :config (fn [] (require :custom.git))})
  (use {1 "sindrets/diffview.nvim"
        :requires "nvim-lua/plenary.nvim"
        :commit "b31fafb"})
  (use {1 "lewis6991/gitsigns.nvim"
        :tag "v0.5"})
  (use {1 "kyazdani42/nvim-tree.lua"
        :requires ["kyazdani42/nvim-web-devicons"]
        :tag "nightly"
        :config (fn [] (require :custom.nvim-tree))}))

(bootstrap-packer)

(vim.cmd "colorscheme nightfox")
(vim.api.nvim_set_keymap "n" "<SPC>" "" {})
(set vim.g.mapleader " ")
(set vim.g.maplocalleader ",")
(set vim.wo.relativenumber true)
(set vim.o.clipboard "unnamedplus")
(set vim.o.cursorline false)
(set vim.o.signcolumn "yes:1")
(set vim.o.scrolloff 5)
(set vim.o.timeoutlen 500) ; time until which-key triggers
(set vim.o.guifont "Hack_Nerd_Font_Mono:h16")
(set vim.o.mouse "a")
(set vim.o.tabstop 4)
(set vim.o.softtabstop 4)
(set vim.o.shiftwidth 4)
;; Better completion experience
(set vim.o.completeopt "menuone,noinsert,noselect")
(set vim.o.foldmethod "expr")
(set vim.o.foldexpr "nvim_treesitter#foldexpr()")

(vim.api.nvim_create_user_command
  "Config" "exe 'e '.stdpath('config').'/init.lua'" {})

(let [packer (require :packer)
      wk (require :which-key)]
  (packer.startup install-packages)
  (wk.register {"<C-h>" ["<C-w>h" "Window left"]
                "<C-l>" ["<C-w>l" "Window right"]
                "<C-j>" ["<C-w>j" "Window down"]
                "<C-k>" ["<C-w>k" "Window up"]}))
