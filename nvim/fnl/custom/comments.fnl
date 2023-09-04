(let [wk (require :which-key)]
  (wk.register {"<C-/>" ["<Plug>(comment_toggle_linewise_current)<CR>" "Toggle comment"]}
               { :noremap true :silent true })
  (wk.register {"<C-/>" ["<Plug>(comment_toggle_linewise_visual)<CR>" "Toggle comment"]}
               { :mode "v" :noremap true :silent true }))
