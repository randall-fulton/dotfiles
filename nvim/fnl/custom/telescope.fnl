(let [wk (require :which-key)
      telescope (require :telescope)
      actions (require :telescope.actions)
      builtin (require :telescope.builtin)]
  (telescope.setup
    {:defaults
     {:mappings
      {:i {"<Esc>" actions.close
           "<C-j>" actions.move_selection_next
           "<C-k>" actions.move_selection_previous}}}})
  (wk.register
    {"<C-p>" [builtin.find_files "Files"]
     "<leader>" {:f {:name "Telescope"
                     :b [builtin.buffers "Buffers"]
                     :f [builtin.find_files "Files"]
                     :g [builtin.live_grep "Grep"]}}}
    {:mode "n" :noremap true :silent true}))
