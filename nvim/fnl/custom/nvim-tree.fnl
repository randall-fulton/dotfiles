(let [tree (require "nvim-tree")
      wk (require "which-key")]
  (tree.setup {:sort_by "case_insensitive"
	           :filters {:dotfiles false}})
  (wk.register {"<leader>if" [ ":NvimTreeToggle .<CR>" "Files" ]}
               { :silent true :noremap true }))
