(local maps
  {"<leader>"
   {:i {:t [ "<cmd>lua require('neotest').summary.toggle()<CR>" "Tests" ]}
    :t {:name "Tests"
        :d [ "<cmd>lua require('neotest').run.run({ strategy = 'dap' })<CR>" "Debug nearest" ]
        :f [ "<cmd>lua require('neotest').run.run(vim.fn.expand('%'))<CR>" "File" ]
        :o [ "<cmd>lua require('neotest').output.open()<CR>" "Show output" ]
        :s [ "<cmd>lua require('neotest').run.stop()<CR>" "Stop nearest" ]
        :t [ "<cmd>lua require('neotest').run.run()<CR>" "Nearest" ]}}})

(let [neotest (require :neotest)
      wk (require :which-key)]
  (neotest.setup {
      :adapters [(require :neotest-go)
                 (require :neotest-python)
                 (require :neotest-rust)]
      :quickfix {:open (fn [] (vim.cmd "Trouble quickfix"))}})
  (wk.register maps { :silent true :noremap true }))
