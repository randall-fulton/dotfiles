(module config.comments
  {autoload {com Comment
             wk which-key}})

(defn setup []
  (com.setup {:mappings {:basic false
                         :extra false
                         :extended false}})
  (wk.register
    {"<C-/>" ["<Plug>(comment_toggle_linewise_current)<CR>"
              "Toggle comment" ]}
    {:silent true
     :noremap true})
  (wk.register
    {"<C-/>" ["<Plug>(comment_toggle_linewise_visual)<CR>"
              "Toggle comment" ]}
    {:mode "v"
     :silent true
     :noremap true}))
