(let [cmp (require :cmp)]
  (cmp.setup
    {:snippet
     {:expand (fn [args] ((. vim.fn "vsnip#anonymous") args.body))}
     ; :window {:completion cmp.config.window.bordered
     ;          :documentation cmp.config.window.bordered}
     :mapping (cmp.mapping.preset.insert
                 {"<C-b>" (fn [] (cmp.mapping.scroll_docs -4))
		          "<C-f>" (fn [] (cmp.mapping.scroll_docs 4))
		          "<C-Space>" cmp.mapping.complete
		          "<C-e>" cmp.mapping.abort
		          "<CR>" (fn [] (cmp.mapping.confirm { :select true }))})
     :sources (cmp.config.sources [{ :name "nvim_lsp" }
		                           { :name "vsnip" }
		                           { :name "path" }
		                           { :name "buffer" }
		                           { :name "conjure" }])})
  (cmp.setup.cmdline "/" {:mapping cmp.mapping.preset.cmdline
	                      :sources [{ :name "buffer" }]})
  (cmp.setup.cmdline ":" {
	  :mapping cmp.mapping.preset.cmdline
	  :sources (cmp.config.sources [{ :name "path" }]
                                   [{ :name "cmdline" }])}))
